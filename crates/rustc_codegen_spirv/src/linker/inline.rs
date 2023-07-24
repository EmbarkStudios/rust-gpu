//! This algorithm is not intended to be an optimization, it is rather for legalization.
//! Specifically, spir-v disallows things like a `StorageClass::Function` pointer to a
//! `StorageClass::Input` pointer. Our frontend definitely allows it, though, this is like taking a
//! `&Input<T>` in a function! So, we inline all functions that take these "illegal" pointers, then
//! run mem2reg (see mem2reg.rs) on the result to "unwrap" the Function pointer.

use super::apply_rewrite_rules;
use super::ipo::CallGraph;
use super::simple_passes::outgoing_edges;
use super::{get_name, get_names};
use crate::custom_insts::{self, CustomInst, CustomOp};
use rspirv::dr::{Block, Function, Instruction, Module, ModuleHeader, Operand};
use rspirv::spirv::{FunctionControl, Op, StorageClass, Word};
use rustc_data_structures::fx::{FxHashMap, FxHashSet};
use rustc_errors::ErrorGuaranteed;
use rustc_session::Session;
use smallvec::SmallVec;
use std::convert::TryInto;
use std::mem::{self, take};

type FunctionMap = FxHashMap<Word, Function>;

// FIXME(eddyb) this is a bit silly, but this keeps being repeated everywhere.
fn next_id(header: &mut ModuleHeader) -> Word {
    let result = header.bound;
    header.bound += 1;
    result
}

pub fn inline(sess: &Session, module: &mut Module) -> super::Result<()> {
    // This algorithm gets real sad if there's recursion - but, good news, SPIR-V bans recursion
    deny_recursion_in_module(sess, module)?;

    let custom_ext_inst_set_import = module
        .ext_inst_imports
        .iter()
        .find(|inst| {
            assert_eq!(inst.class.opcode, Op::ExtInstImport);
            inst.operands[0].unwrap_literal_string() == &custom_insts::CUSTOM_EXT_INST_SET[..]
        })
        .map(|inst| inst.result_id.unwrap());

    // HACK(eddyb) compute the set of functions that may `Abort` *transitively*,
    // which is only needed because of how we inline (sometimes it's outside-in,
    // aka top-down, instead of always being inside-out, aka bottom-up).
    //
    // (inlining is needed in the first place because our custom `Abort`
    // instructions get lowered to a simple `OpReturn` in entry-points, but
    // that requires that they get inlined all the way up to the entry-points)
    let functions_that_may_abort = custom_ext_inst_set_import
        .map(|custom_ext_inst_set_import| {
            let mut may_abort_by_id = FxHashSet::default();

            // FIXME(eddyb) use this `CallGraph` abstraction more during inlining.
            let call_graph = CallGraph::collect(module);
            for func_idx in call_graph.post_order() {
                let func_id = module.functions[func_idx].def_id().unwrap();

                let any_callee_may_abort = call_graph.callees[func_idx].iter().any(|&callee_idx| {
                    may_abort_by_id.contains(&module.functions[callee_idx].def_id().unwrap())
                });
                if any_callee_may_abort {
                    may_abort_by_id.insert(func_id);
                    continue;
                }

                let may_abort_directly = module.functions[func_idx].blocks.iter().any(|block| {
                    match &block.instructions[..] {
                        [.., last_normal_inst, terminator_inst]
                            if last_normal_inst.class.opcode == Op::ExtInst
                                && last_normal_inst.operands[0].unwrap_id_ref()
                                    == custom_ext_inst_set_import
                                && CustomOp::decode_from_ext_inst(last_normal_inst)
                                    == CustomOp::Abort =>
                        {
                            assert_eq!(terminator_inst.class.opcode, Op::Unreachable);
                            true
                        }

                        _ => false,
                    }
                });
                if may_abort_directly {
                    may_abort_by_id.insert(func_id);
                }
            }

            may_abort_by_id
        })
        .unwrap_or_default();

    let functions = module
        .functions
        .iter()
        .map(|f| (f.def_id().unwrap(), f.clone()))
        .collect();
    let legal_globals = LegalGlobal::gather_from_module(module);

    // Drop all the functions we'll be inlining. (This also means we won't waste time processing
    // inlines in functions that will get inlined)
    let mut dropped_ids = FxHashSet::default();
    let mut inlined_to_legalize_dont_inlines = Vec::new();
    module.functions.retain(|f| {
        let should_inline_f = should_inline(&legal_globals, &functions_that_may_abort, f, None);
        if should_inline_f != Ok(false) {
            if should_inline_f == Err(MustInlineToLegalize) && has_dont_inline(f) {
                inlined_to_legalize_dont_inlines.push(f.def_id().unwrap());
            }
            // TODO: We should insert all defined IDs in this function.
            dropped_ids.insert(f.def_id().unwrap());
            false
        } else {
            true
        }
    });

    if !inlined_to_legalize_dont_inlines.is_empty() {
        let names = get_names(module);
        for f in inlined_to_legalize_dont_inlines {
            sess.warn(format!(
                "`#[inline(never)]` function `{}` needs to be inlined \
                 because it has illegal argument or return types",
                get_name(&names, f)
            ));
        }
    }

    let header = module.header.as_mut().unwrap();
    // FIXME(eddyb) clippy false positive (seperate `map` required for borrowck).
    #[allow(clippy::map_unwrap_or)]
    let mut inliner = Inliner {
        op_type_void_id: module
            .types_global_values
            .iter()
            .find(|inst| inst.class.opcode == Op::TypeVoid)
            .map(|inst| inst.result_id.unwrap())
            .unwrap_or_else(|| {
                let id = next_id(header);
                let inst = Instruction::new(Op::TypeVoid, None, Some(id), vec![]);
                module.types_global_values.push(inst);
                id
            }),

        custom_ext_inst_set_import: custom_ext_inst_set_import.unwrap_or_else(|| {
            let id = next_id(header);
            let inst = Instruction::new(
                Op::ExtInstImport,
                None,
                Some(id),
                vec![Operand::LiteralString(
                    custom_insts::CUSTOM_EXT_INST_SET.to_string(),
                )],
            );
            module.ext_inst_imports.push(inst);
            id
        }),

        id_to_name: module
            .debug_names
            .iter()
            .filter(|inst| inst.class.opcode == Op::Name)
            .map(|inst| {
                (
                    inst.operands[0].unwrap_id_ref(),
                    inst.operands[1].unwrap_literal_string(),
                )
            })
            .collect(),

        cached_op_strings: FxHashMap::default(),

        header,
        debug_string_source: &mut module.debug_string_source,
        annotations: &mut module.annotations,
        types_global_values: &mut module.types_global_values,

        functions: &functions,
        legal_globals: &legal_globals,
        functions_that_may_abort: &functions_that_may_abort,
    };
    for function in &mut module.functions {
        inliner.inline_fn(function);
        fuse_trivial_branches(function);
    }

    // Drop OpName etc. for inlined functions
    module.debug_names.retain(|inst| {
        !inst.operands.iter().any(|op| {
            op.id_ref_any()
                .map_or(false, |id| dropped_ids.contains(&id))
        })
    });

    Ok(())
}

// https://stackoverflow.com/a/53995651
fn deny_recursion_in_module(sess: &Session, module: &Module) -> super::Result<()> {
    let func_to_index: FxHashMap<Word, usize> = module
        .functions
        .iter()
        .enumerate()
        .map(|(index, func)| (func.def_id().unwrap(), index))
        .collect();
    let mut discovered = vec![false; module.functions.len()];
    let mut finished = vec![false; module.functions.len()];
    let mut has_recursion = None;
    for index in 0..module.functions.len() {
        if !discovered[index] && !finished[index] {
            visit(
                sess,
                module,
                index,
                &mut discovered,
                &mut finished,
                &mut has_recursion,
                &func_to_index,
            );
        }
    }

    fn visit(
        sess: &Session,
        module: &Module,
        current: usize,
        discovered: &mut Vec<bool>,
        finished: &mut Vec<bool>,
        has_recursion: &mut Option<ErrorGuaranteed>,
        func_to_index: &FxHashMap<Word, usize>,
    ) {
        discovered[current] = true;

        for next in calls(&module.functions[current], func_to_index) {
            if discovered[next] {
                let names = get_names(module);
                let current_name = get_name(&names, module.functions[current].def_id().unwrap());
                let next_name = get_name(&names, module.functions[next].def_id().unwrap());
                *has_recursion = Some(sess.err(format!(
                    "module has recursion, which is not allowed: `{current_name}` calls `{next_name}`"
                )));
                break;
            }

            if !finished[next] {
                visit(
                    sess,
                    module,
                    next,
                    discovered,
                    finished,
                    has_recursion,
                    func_to_index,
                );
            }
        }

        discovered[current] = false;
        finished[current] = true;
    }

    fn calls<'a>(
        func: &'a Function,
        func_to_index: &'a FxHashMap<Word, usize>,
    ) -> impl Iterator<Item = usize> + 'a {
        func.all_inst_iter()
            .filter(|inst| inst.class.opcode == Op::FunctionCall)
            .map(move |inst| {
                *func_to_index
                    .get(&inst.operands[0].id_ref_any().unwrap())
                    .unwrap()
            })
    }

    match has_recursion {
        Some(err) => Err(err),
        None => Ok(()),
    }
}

/// Any type/const/global variable, which is "legal" (i.e. can be kept in SPIR-V).
///
/// For the purposes of the inliner, a legal global cannot:
/// - refer to any illegal globals
/// - (if a type) refer to any pointer types
///   - this rules out both pointers in composites, and pointers to pointers
///     (the latter itself *also* rules out variables containing pointers)
enum LegalGlobal {
    TypePointer(StorageClass),
    TypeNonPointer,
    Const,
    Variable,
}

impl LegalGlobal {
    fn gather_from_module(module: &Module) -> FxHashMap<Word, Self> {
        let mut legal_globals = FxHashMap::<_, Self>::default();
        for inst in &module.types_global_values {
            let global = match inst.class.opcode {
                Op::TypePointer => Self::TypePointer(inst.operands[0].unwrap_storage_class()),
                Op::Variable => Self::Variable,
                op if rspirv::grammar::reflect::is_type(op) => Self::TypeNonPointer,
                op if rspirv::grammar::reflect::is_constant(op) => Self::Const,

                // FIXME(eddyb) should this be `unreachable!()`?
                _ => continue,
            };
            let legal_result_type = match inst.result_type {
                Some(result_type_id) => matches!(
                    (&global, legal_globals.get(&result_type_id)),
                    (Self::Variable, Some(Self::TypePointer(_)))
                        | (Self::Const, Some(Self::TypeNonPointer))
                ),
                None => matches!(global, Self::TypePointer(_) | Self::TypeNonPointer),
            };
            let legal_operands = inst.operands.iter().all(|operand| match operand {
                Operand::IdRef(id) => matches!(
                    legal_globals.get(id),
                    Some(Self::TypeNonPointer | Self::Const)
                ),

                // NOTE(eddyb) this assumes non-ID operands are always legal.
                _ => operand.id_ref_any().is_none(),
            });
            if legal_result_type && legal_operands {
                legal_globals.insert(inst.result_id.unwrap(), global);
            }
        }
        legal_globals
    }

    fn legal_as_fn_param_ty(&self) -> bool {
        match *self {
            Self::TypePointer(storage_class) => matches!(
                storage_class,
                StorageClass::UniformConstant
                    | StorageClass::Function
                    | StorageClass::Private
                    | StorageClass::Workgroup
                    | StorageClass::AtomicCounter
            ),
            Self::TypeNonPointer => true,

            // FIXME(eddyb) should this be an `unreachable!()`?
            Self::Const | Self::Variable => false,
        }
    }

    fn legal_as_fn_ret_ty(&self) -> bool {
        #[allow(clippy::match_same_arms)]
        match *self {
            Self::TypePointer(_) => false,
            Self::TypeNonPointer => true,

            // FIXME(eddyb) should this be an `unreachable!()`?
            Self::Const | Self::Variable => false,
        }
    }
}

/// Helper type which encapsulates all the information about one specific call.
#[derive(Copy, Clone)]
struct CallSite<'a> {
    caller: &'a Function,
    call_inst: &'a Instruction,
}

fn has_dont_inline(function: &Function) -> bool {
    let def = function.def.as_ref().unwrap();
    let control = def.operands[0].unwrap_function_control();
    control.contains(FunctionControl::DONT_INLINE)
}

/// Helper error type for `should_inline` (see its doc comment).
#[derive(Copy, Clone, PartialEq, Eq)]
struct MustInlineToLegalize;

/// Returns `Ok(true)`/`Err(MustInlineToLegalize)` if `callee` should/must be
/// inlined (either in general, or specifically from `call_site`, if provided).
///
/// The distinction made is that `Err(MustInlineToLegalize)` is not a heuristic,
/// and inlining is *mandatory* due to an illegal signature/arguments.
fn should_inline(
    legal_globals: &FxHashMap<Word, LegalGlobal>,
    functions_that_may_abort: &FxHashSet<Word>,
    callee: &Function,
    call_site: Option<CallSite<'_>>,
) -> Result<bool, MustInlineToLegalize> {
    let callee_def = callee.def.as_ref().unwrap();
    let callee_control = callee_def.operands[0].unwrap_function_control();

    // HACK(eddyb) this "has a call-site" check ensures entry-points don't get
    // accidentally removed as "must inline to legalize" function, but can still
    // be inlined into other entry-points (if such an unusual situation arises).
    if call_site.is_some() && functions_that_may_abort.contains(&callee.def_id().unwrap()) {
        return Err(MustInlineToLegalize);
    }

    let ret_ty = legal_globals
        .get(&callee_def.result_type.unwrap())
        .ok_or(MustInlineToLegalize)?;
    if !ret_ty.legal_as_fn_ret_ty() {
        return Err(MustInlineToLegalize);
    }

    for (i, param) in callee.parameters.iter().enumerate() {
        let param_ty = legal_globals
            .get(param.result_type.as_ref().unwrap())
            .ok_or(MustInlineToLegalize)?;
        if !param_ty.legal_as_fn_param_ty() {
            return Err(MustInlineToLegalize);
        }

        // If the call isn't passing a legal pointer argument (a "memory object",
        // i.e. an `OpVariable` or one of the caller's `OpFunctionParameters),
        // then inlining is required to have a chance at producing legal SPIR-V.
        //
        // FIXME(eddyb) rewriting away the pointer could be another alternative.
        if let (LegalGlobal::TypePointer(_), Some(call_site)) = (param_ty, call_site) {
            let ptr_arg = call_site.call_inst.operands[i + 1].unwrap_id_ref();
            match legal_globals.get(&ptr_arg) {
                Some(LegalGlobal::Variable) => {}

                // FIXME(eddyb) should some constants (undef/null) be allowed?
                Some(_) => return Err(MustInlineToLegalize),

                None => {
                    let mut caller_param_and_var_ids = call_site
                        .caller
                        .parameters
                        .iter()
                        .chain(
                            call_site.caller.blocks[0]
                                .instructions
                                .iter()
                                .filter(|caller_inst| {
                                    // HACK(eddyb) this only avoids scanning the
                                    // whole entry block for `OpVariable`s, so
                                    // it can overapproximate debuginfo insts.
                                    let may_be_debuginfo = matches!(
                                        caller_inst.class.opcode,
                                        Op::Line | Op::NoLine | Op::ExtInst
                                    );
                                    !may_be_debuginfo
                                })
                                .take_while(|caller_inst| caller_inst.class.opcode == Op::Variable),
                        )
                        .map(|caller_inst| caller_inst.result_id.unwrap());

                    if !caller_param_and_var_ids.any(|id| ptr_arg == id) {
                        return Err(MustInlineToLegalize);
                    }
                }
            }
        }
    }

    Ok(callee_control.contains(FunctionControl::INLINE))
}

// Steps:
// Move OpVariable decls
// Rewrite return
// Renumber IDs
// Insert blocks

struct Inliner<'m, 'map> {
    /// ID of `OpExtInstImport` for our custom "extended instruction set"
    /// (see `crate::custom_insts` for more details).
    custom_ext_inst_set_import: Word,

    op_type_void_id: Word,

    /// Pre-collected `OpName`s, that can be used to find any function's name
    /// during inlining (to be able to generate debuginfo that uses names).
    id_to_name: FxHashMap<Word, &'m str>,

    /// `OpString` cache (for deduplicating `OpString`s for the same string).
    //
    // FIXME(eddyb) currently this doesn't reuse existing `OpString`s, but since
    // this is mostly for inlined callee names, it's expected almost no overlap
    // exists between existing `OpString`s and new ones, anyway.
    cached_op_strings: FxHashMap<&'m str, Word>,

    header: &'m mut ModuleHeader,
    debug_string_source: &'m mut Vec<Instruction>,
    annotations: &'m mut Vec<Instruction>,
    types_global_values: &'m mut Vec<Instruction>,

    functions: &'map FunctionMap,
    legal_globals: &'map FxHashMap<Word, LegalGlobal>,
    functions_that_may_abort: &'map FxHashSet<Word>,
    // rewrite_rules: FxHashMap<Word, Word>,
}

impl Inliner<'_, '_> {
    fn id(&mut self) -> Word {
        next_id(self.header)
    }

    /// Applies all rewrite rules to the decorations in the header.
    fn apply_rewrite_for_decorations(&mut self, rewrite_rules: &FxHashMap<Word, Word>) {
        // NOTE(siebencorgie): We don't care *what* decoration we rewrite atm.
        // AFAIK there is no case where keeping decorations on inline wouldn't be valid.
        for annotation_idx in 0..self.annotations.len() {
            let inst = &self.annotations[annotation_idx];
            if let [Operand::IdRef(target), ..] = inst.operands[..] {
                if let Some(&rewritten_target) = rewrite_rules.get(&target) {
                    // Copy decoration instruction and push it.
                    let mut cloned_inst = inst.clone();
                    cloned_inst.operands[0] = Operand::IdRef(rewritten_target);
                    self.annotations.push(cloned_inst);
                }
            }
        }
    }

    fn ptr_ty(&mut self, pointee: Word) -> Word {
        // TODO: This is horribly slow, fix this
        let existing = self.types_global_values.iter().find(|inst| {
            inst.class.opcode == Op::TypePointer
                && inst.operands[0].unwrap_storage_class() == StorageClass::Function
                && inst.operands[1].unwrap_id_ref() == pointee
        });
        if let Some(existing) = existing {
            return existing.result_id.unwrap();
        }
        let inst_id = self.id();
        self.types_global_values.push(Instruction::new(
            Op::TypePointer,
            None,
            Some(inst_id),
            vec![
                Operand::StorageClass(StorageClass::Function),
                Operand::IdRef(pointee),
            ],
        ));
        inst_id
    }

    fn inline_fn(&mut self, function: &mut Function) {
        let mut block_idx = 0;
        while block_idx < function.blocks.len() {
            // If we successfully inlined a block, then repeat processing on the same block, in
            // case the newly inlined block has more inlined calls.
            // TODO: This is quadratic
            if !self.inline_block(function, block_idx) {
                block_idx += 1;
            }
        }
    }

    fn inline_block(&mut self, caller: &mut Function, block_idx: usize) -> bool {
        // Find the first inlined OpFunctionCall
        let call = caller.blocks[block_idx]
            .instructions
            .iter()
            .enumerate()
            .filter(|(_, inst)| inst.class.opcode == Op::FunctionCall)
            .map(|(index, inst)| {
                (
                    index,
                    inst,
                    self.functions
                        .get(&inst.operands[0].id_ref_any().unwrap())
                        .unwrap(),
                )
            })
            .find(|(_, inst, f)| {
                let call_site = CallSite {
                    caller,
                    call_inst: inst,
                };
                match should_inline(
                    self.legal_globals,
                    self.functions_that_may_abort,
                    f,
                    Some(call_site),
                ) {
                    Ok(inline) => inline,
                    Err(MustInlineToLegalize) => true,
                }
            });
        let (call_index, call_inst, callee) = match call {
            None => return false,
            Some(call) => call,
        };
        let call_result_type = {
            let ty = call_inst.result_type.unwrap();
            if ty == self.op_type_void_id {
                None
            } else {
                Some(ty)
            }
        };
        let call_result_id = call_inst.result_id.unwrap();

        // Get the debuginfo instructions that apply to the call.
        let custom_ext_inst_set_import = self.custom_ext_inst_set_import;
        let call_debug_insts = caller.blocks[block_idx].instructions[..call_index]
            .iter()
            .filter(|inst| match inst.class.opcode {
                Op::Line | Op::NoLine => true,
                Op::ExtInst if inst.operands[0].unwrap_id_ref() == custom_ext_inst_set_import => {
                    CustomOp::decode_from_ext_inst(inst).is_debuginfo()
                }
                _ => false,
            });

        // Rewrite parameters to arguments
        let call_arguments = call_inst
            .operands
            .iter()
            .skip(1)
            .map(|op| op.id_ref_any().unwrap());
        let callee_parameters = callee.parameters.iter().map(|inst| {
            assert!(inst.class.opcode == Op::FunctionParameter);
            inst.result_id.unwrap()
        });
        let mut rewrite_rules = callee_parameters.zip(call_arguments).collect();

        let return_variable = if call_result_type.is_some() {
            Some(self.id())
        } else {
            None
        };
        let return_jump = self.id();
        // Rewrite OpReturns of the callee.
        let (mut inlined_callee_blocks, extra_debug_insts_pre_call, extra_debug_insts_post_call) =
            self.get_inlined_blocks(callee, call_debug_insts, return_variable, return_jump);
        // Clone the IDs of the callee, because otherwise they'd be defined multiple times if the
        // fn is inlined multiple times.
        self.add_clone_id_rules(&mut rewrite_rules, &inlined_callee_blocks);
        apply_rewrite_rules(&rewrite_rules, &mut inlined_callee_blocks);
        self.apply_rewrite_for_decorations(&rewrite_rules);

        // Split the block containing the `OpFunctionCall` into pre-call vs post-call.
        let pre_call_block_idx = block_idx;
        #[expect(unused)]
        let block_idx: usize; // HACK(eddyb) disallowing using the unrenamed variable.
        let mut post_call_block_insts = caller.blocks[pre_call_block_idx]
            .instructions
            .split_off(call_index + 1);

        // pop off OpFunctionCall
        let call = caller.blocks[pre_call_block_idx]
            .instructions
            .pop()
            .unwrap();
        assert!(call.class.opcode == Op::FunctionCall);

        // HACK(eddyb) inject the additional debuginfo instructions generated by
        // `get_inlined_blocks`, so the inlined call frame "stack" isn't corrupted.
        caller.blocks[pre_call_block_idx]
            .instructions
            .extend(extra_debug_insts_pre_call);
        post_call_block_insts.splice(0..0, extra_debug_insts_post_call);

        if let Some(call_result_type) = call_result_type {
            // Generate the storage space for the return value: Do this *after* the split above,
            // because if block_idx=0, inserting a variable here shifts call_index.
            insert_opvariables(
                &mut caller.blocks[0],
                [Instruction::new(
                    Op::Variable,
                    Some(self.ptr_ty(call_result_type)),
                    Some(return_variable.unwrap()),
                    vec![Operand::StorageClass(StorageClass::Function)],
                )],
            );
        }

        // Insert non-entry inlined callee blocks just after the pre-call block.
        let non_entry_inlined_callee_blocks = inlined_callee_blocks.drain(1..);
        let num_non_entry_inlined_callee_blocks = non_entry_inlined_callee_blocks.len();
        caller.blocks.splice(
            (pre_call_block_idx + 1)..(pre_call_block_idx + 1),
            non_entry_inlined_callee_blocks,
        );

        if let Some(call_result_type) = call_result_type {
            // Add the load of the result value after the inlined function. Note there's guaranteed no
            // OpPhi instructions since we just split this block.
            post_call_block_insts.insert(
                0,
                Instruction::new(
                    Op::Load,
                    Some(call_result_type),
                    Some(call_result_id),
                    vec![Operand::IdRef(return_variable.unwrap())],
                ),
            );
        }

        // Insert the post-call block, after all the inlined callee blocks.
        {
            let post_call_block_idx = pre_call_block_idx + num_non_entry_inlined_callee_blocks + 1;
            let post_call_block = Block {
                label: Some(Instruction::new(Op::Label, None, Some(return_jump), vec![])),
                instructions: post_call_block_insts,
            };
            caller.blocks.insert(post_call_block_idx, post_call_block);

            // Adjust any `OpPhi`s in the (caller) targets of the original call block,
            // to refer to post-call block (the new source of those CFG edges).
            rewrite_phi_sources(
                caller.blocks[pre_call_block_idx].label_id().unwrap(),
                &mut caller.blocks,
                post_call_block_idx,
            );
        }

        // Fuse the inlined callee entry block into the pre-call block.
        // This is okay because it's illegal to branch to the first BB in a function.
        {
            // Return the subsequence of `insts` made from `OpVariable`s, and any
            // debuginfo instructions (which may apply to them), while removing
            // *only* `OpVariable`s from `insts` (and keeping debuginfo in both).
            let mut steal_vars = |insts: &mut Vec<Instruction>| {
                let mut vars_and_debuginfo = vec![];
                insts.retain_mut(|inst| {
                    let is_debuginfo = match inst.class.opcode {
                        Op::Line | Op::NoLine => true,
                        Op::ExtInst => {
                            inst.operands[0].unwrap_id_ref() == self.custom_ext_inst_set_import
                                && CustomOp::decode_from_ext_inst(inst).is_debuginfo()
                        }
                        _ => false,
                    };
                    if is_debuginfo {
                        // NOTE(eddyb) `OpExtInst`s have a result ID,
                        // even if unused, and it has to be unique.
                        let mut inst = inst.clone();
                        if let Some(id) = &mut inst.result_id {
                            *id = self.id();
                        }
                        vars_and_debuginfo.push(inst);
                        true
                    } else if inst.class.opcode == Op::Variable {
                        // HACK(eddyb) we're removing this `Instruction` from
                        // `inst`, so it doesn't really matter what we use here.
                        vars_and_debuginfo.push(mem::replace(
                            inst,
                            Instruction::new(Op::Nop, None, None, vec![]),
                        ));
                        false
                    } else {
                        true
                    }
                });
                vars_and_debuginfo
            };

            let [mut inlined_callee_entry_block]: [_; 1] =
                inlined_callee_blocks.try_into().unwrap();

            // Move the `OpVariable`s of the callee to the caller.
            insert_opvariables(
                &mut caller.blocks[0],
                steal_vars(&mut inlined_callee_entry_block.instructions),
            );

            caller.blocks[pre_call_block_idx]
                .instructions
                .append(&mut inlined_callee_entry_block.instructions);

            // Adjust any `OpPhi`s in the (inlined callee) targets of the
            // inlined callee entry block, to refer to the pre-call block
            // (the new source of those CFG edges).
            rewrite_phi_sources(
                inlined_callee_entry_block.label_id().unwrap(),
                &mut caller.blocks,
                pre_call_block_idx,
            );
        }

        true
    }

    fn add_clone_id_rules(&mut self, rewrite_rules: &mut FxHashMap<Word, Word>, blocks: &[Block]) {
        for block in blocks {
            for inst in block.label.iter().chain(&block.instructions) {
                if let Some(result_id) = inst.result_id {
                    let new_id = self.id();
                    let old = rewrite_rules.insert(result_id, new_id);
                    assert!(old.is_none());
                }
            }
        }
    }

    // HACK(eddyb) the second and third return values are additional debuginfo
    // instructions that need to be inserted just before/after the callsite.
    fn get_inlined_blocks<'a>(
        &mut self,
        callee: &Function,
        call_debug_insts: impl Iterator<Item = &'a Instruction>,
        return_variable: Option<Word>,
        return_jump: Word,
    ) -> (
        Vec<Block>,
        SmallVec<[Instruction; 8]>,
        SmallVec<[Instruction; 8]>,
    ) {
        let Self {
            custom_ext_inst_set_import,
            op_type_void_id,
            ..
        } = *self;

        // HACK(eddyb) this is terrible, but we have to deal with it becasue of
        // how this inliner is outside-in, instead of inside-out, meaning that
        // context builds up "outside" of the callee blocks, inside the caller.
        let mut enclosing_inlined_frames = SmallVec::<[_; 8]>::new();
        let mut current_debug_src_loc_inst = None;
        for inst in call_debug_insts {
            match inst.class.opcode {
                Op::Line => current_debug_src_loc_inst = Some(inst),
                Op::NoLine => current_debug_src_loc_inst = None,
                Op::ExtInst
                    if inst.operands[0].unwrap_id_ref() == self.custom_ext_inst_set_import =>
                {
                    match CustomOp::decode_from_ext_inst(inst) {
                        CustomOp::SetDebugSrcLoc => current_debug_src_loc_inst = Some(inst),
                        CustomOp::ClearDebugSrcLoc => current_debug_src_loc_inst = None,
                        CustomOp::PushInlinedCallFrame => {
                            enclosing_inlined_frames
                                .push((current_debug_src_loc_inst.take(), inst));
                        }
                        CustomOp::PopInlinedCallFrame => {
                            if let Some((callsite_debug_src_loc_inst, _)) =
                                enclosing_inlined_frames.pop()
                            {
                                current_debug_src_loc_inst = callsite_debug_src_loc_inst;
                            }
                        }
                        CustomOp::Abort => {}
                    }
                }
                _ => {}
            }
        }

        // Prepare the debuginfo insts to prepend/append to every block.
        // FIXME(eddyb) this could be more efficient if we only used one pair of
        // `{Push,Pop}InlinedCallFrame` for the whole inlined callee, but there
        // is no way to hint the SPIR-T CFG (re)structurizer that it should keep
        // the entire callee in one region - a SPIR-T inliner wouldn't have this
        // issue, as it would require a fully structured callee.
        let callee_name = self
            .id_to_name
            .get(&callee.def_id().unwrap())
            .copied()
            .unwrap_or("");
        let callee_name_id = *self
            .cached_op_strings
            .entry(callee_name)
            .or_insert_with(|| {
                let id = next_id(self.header);
                self.debug_string_source.push(Instruction::new(
                    Op::String,
                    None,
                    Some(id),
                    vec![Operand::LiteralString(callee_name.to_string())],
                ));
                id
            });
        let mut mk_debuginfo_prefix_and_suffix = |include_callee_frame| {
            // NOTE(eddyb) `OpExtInst`s have a result ID, even if unused, and
            // it has to be unique (same goes for the other instructions below).
            let instantiate_debuginfo = |this: &mut Self, inst: &Instruction| {
                let mut inst = inst.clone();
                if let Some(id) = &mut inst.result_id {
                    *id = this.id();
                }
                inst
            };
            let custom_inst_to_inst = |this: &mut Self, inst: CustomInst<_>| {
                Instruction::new(
                    Op::ExtInst,
                    Some(op_type_void_id),
                    Some(this.id()),
                    [
                        Operand::IdRef(custom_ext_inst_set_import),
                        Operand::LiteralExtInstInteger(inst.op() as u32),
                    ]
                    .into_iter()
                    .chain(inst.into_operands())
                    .collect(),
                )
            };
            // FIXME(eddyb) this only allocates to avoid borrow conflicts.
            let mut prefix = SmallVec::<[_; 8]>::new();
            let mut suffix = SmallVec::<[_; 8]>::new();
            for &(callsite_debug_src_loc_inst, push_inlined_call_frame_inst) in
                &enclosing_inlined_frames
            {
                prefix.extend(
                    callsite_debug_src_loc_inst
                        .into_iter()
                        .chain([push_inlined_call_frame_inst])
                        .map(|inst| instantiate_debuginfo(self, inst)),
                );
                suffix.push(custom_inst_to_inst(self, CustomInst::PopInlinedCallFrame));
            }
            prefix.extend(current_debug_src_loc_inst.map(|inst| instantiate_debuginfo(self, inst)));

            if include_callee_frame {
                prefix.push(custom_inst_to_inst(
                    self,
                    CustomInst::PushInlinedCallFrame {
                        callee_name: Operand::IdRef(callee_name_id),
                    },
                ));
                suffix.push(custom_inst_to_inst(self, CustomInst::PopInlinedCallFrame));
            }

            (prefix, suffix)
        };

        let mut blocks = callee.blocks.clone();
        for block in &mut blocks {
            let mut terminator = block.instructions.pop().unwrap();

            // HACK(eddyb) strip trailing debuginfo (as it can't impact terminators).
            while let Some(last) = block.instructions.last() {
                let can_remove = match last.class.opcode {
                    Op::Line | Op::NoLine => true,
                    Op::ExtInst => {
                        last.operands[0].unwrap_id_ref() == custom_ext_inst_set_import
                            && matches!(
                                CustomOp::decode_from_ext_inst(last),
                                CustomOp::SetDebugSrcLoc | CustomOp::ClearDebugSrcLoc
                            )
                    }
                    _ => false,
                };
                if can_remove {
                    block.instructions.pop();
                } else {
                    break;
                }
            }

            if let Op::Return | Op::ReturnValue = terminator.class.opcode {
                if Op::ReturnValue == terminator.class.opcode {
                    let return_value = terminator.operands[0].id_ref_any().unwrap();
                    block.instructions.push(Instruction::new(
                        Op::Store,
                        None,
                        None,
                        vec![
                            Operand::IdRef(return_variable.unwrap()),
                            Operand::IdRef(return_value),
                        ],
                    ));
                } else {
                    assert!(return_variable.is_none());
                }
                terminator =
                    Instruction::new(Op::Branch, None, None, vec![Operand::IdRef(return_jump)]);
            }

            let num_phis = block
                .instructions
                .iter()
                .take_while(|inst| inst.class.opcode == Op::Phi)
                .count();

            // HACK(eddyb) avoid adding debuginfo to otherwise-empty blocks.
            if block.instructions.len() > num_phis {
                let (debuginfo_prefix, debuginfo_suffix) = mk_debuginfo_prefix_and_suffix(true);
                // Insert the prefix debuginfo instructions after `OpPhi`s,
                // which sadly can't be covered by them.
                block
                    .instructions
                    .splice(num_phis..num_phis, debuginfo_prefix);
                // Insert the suffix debuginfo instructions before the terminator,
                // which sadly can't be covered by them.
                block.instructions.extend(debuginfo_suffix);
            }

            block.instructions.push(terminator);
        }

        let (caller_restore_debuginfo_after_call, calleer_reset_debuginfo_before_call) =
            mk_debuginfo_prefix_and_suffix(false);
        (
            blocks,
            calleer_reset_debuginfo_before_call,
            caller_restore_debuginfo_after_call,
        )
    }
}

fn insert_opvariables(block: &mut Block, insts: impl IntoIterator<Item = Instruction>) {
    let first_non_variable = block
        .instructions
        .iter()
        .position(|inst| inst.class.opcode != Op::Variable);
    let i = first_non_variable.unwrap_or(block.instructions.len());
    block.instructions.splice(i..i, insts);
}

fn fuse_trivial_branches(function: &mut Function) {
    let all_preds = compute_preds(&function.blocks);
    'outer: for (dest_block, mut preds) in all_preds.iter().enumerate() {
        // Don't fuse branches into blocks with `OpPhi`s.
        let any_phis = function.blocks[dest_block]
            .instructions
            .iter()
            .filter(|inst| {
                // These are the only instructions that are allowed before `OpPhi`.
                !matches!(inst.class.opcode, Op::Line | Op::NoLine)
            })
            .take_while(|inst| inst.class.opcode == Op::Phi)
            .next()
            .is_some();
        if any_phis {
            continue;
        }

        // if there's two trivial branches in a row, the middle one might get inlined before the
        // last one, so when processing the last one, skip through to the first one.
        let pred = loop {
            if preds.len() != 1 || preds[0] == dest_block {
                continue 'outer;
            }
            let pred = preds[0];
            if !function.blocks[pred].instructions.is_empty() {
                break pred;
            }
            preds = &all_preds[pred];
        };
        let pred_insts = &function.blocks[pred].instructions;
        if pred_insts.last().unwrap().class.opcode == Op::Branch {
            let mut dest_insts = take(&mut function.blocks[dest_block].instructions);
            let pred_insts = &mut function.blocks[pred].instructions;
            pred_insts.pop(); // pop the branch
            pred_insts.append(&mut dest_insts);

            // Adjust any `OpPhi`s in the targets of the original block, to refer
            // to the sole predecessor (the new source of those CFG edges).
            rewrite_phi_sources(
                function.blocks[dest_block].label_id().unwrap(),
                &mut function.blocks,
                pred,
            );
        }
    }
    function.blocks.retain(|b| !b.instructions.is_empty());
}

fn compute_preds(blocks: &[Block]) -> Vec<Vec<usize>> {
    let mut result = vec![vec![]; blocks.len()];
    for (source_idx, source) in blocks.iter().enumerate() {
        for dest_id in outgoing_edges(source) {
            let dest_idx = blocks
                .iter()
                .position(|b| b.label_id().unwrap() == dest_id)
                .unwrap();
            result[dest_idx].push(source_idx);
        }
    }
    result
}

/// Helper for adjusting `OpPhi` source label IDs, when the terminator of the
/// `original_label_id`-labeled block got moved to `blocks[original_block_idx]`.
fn rewrite_phi_sources(original_label_id: Word, blocks: &mut [Block], new_block_idx: usize) {
    let new_label_id = blocks[new_block_idx].label_id().unwrap();

    // HACK(eddyb) can't keep `blocks` borrowed, the loop needs mutable access.
    let target_ids: SmallVec<[_; 4]> = outgoing_edges(&blocks[new_block_idx]).collect();

    for target_id in target_ids {
        let target_block = blocks
            .iter_mut()
            .find(|b| b.label_id().unwrap() == target_id)
            .unwrap();
        let phis = target_block
            .instructions
            .iter_mut()
            .filter(|inst| {
                // These are the only instructions that are allowed before `OpPhi`.
                !matches!(inst.class.opcode, Op::Line | Op::NoLine)
            })
            .take_while(|inst| inst.class.opcode == Op::Phi);
        for phi in phis {
            for value_and_source_id in phi.operands.chunks_mut(2) {
                let source_id = value_and_source_id[1].id_ref_any_mut().unwrap();
                if *source_id == original_label_id {
                    *source_id = new_label_id;
                    break;
                }
            }
        }
    }
}
