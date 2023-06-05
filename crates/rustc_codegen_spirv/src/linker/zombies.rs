//! See documentation on `CodegenCx::zombie` for a description of the zombie system.

use super::{get_name, get_names};
use crate::custom_decorations::{CustomDecoration, SpanRegenerator, ZombieDecoration};
use crate::custom_insts::{self, CustomOp};
use rspirv::dr::{Instruction, Module, Operand};
use rspirv::spirv::{Op, Word};
use rustc_data_structures::fx::{FxHashMap, FxHashSet, FxIndexMap};
use rustc_errors::{DiagnosticBuilder, ErrorGuaranteed};
use rustc_session::Session;
use rustc_span::{Span, DUMMY_SP};
use smallvec::SmallVec;

#[derive(Copy, Clone)]
struct Zombie<'a> {
    id: Word,
    kind: &'a ZombieKind<'a>,
}

enum ZombieKind<'a> {
    /// Definition annotated with `ZombieDecoration`.
    Leaf,

    /// Transitively zombie'd by using other zombies, from an instruction.
    Uses(Vec<ZombieUse<'a>>),
}

struct ZombieUse<'a> {
    used_zombie_id: Word,

    /// Active debug "source location" instruction at the time of the use, if any
    /// (both `OpLine` and `CustomInst::SetDebugSrcLoc` are supported).
    use_debug_src_loc_inst: Option<&'a Instruction>,

    origin: UseOrigin,
}

enum UseOrigin {
    GlobalOperandOrResultType,
    IntraFuncOperandOrResultType { parent_func_id: Word },
    CallCalleeOperand { caller_func_id: Word },
}

struct Zombies<'a> {
    /// ID of `OpExtInstImport` for our custom "extended instruction set",
    /// if present (see `crate::custom_insts` for more details).
    custom_ext_inst_set_import: Option<Word>,

    id_to_zombie_kind: FxIndexMap<Word, ZombieKind<'a>>,
}

impl<'a> Zombies<'a> {
    // FIXME(eddyb) rename all the other methods to say `_inst` explicitly.
    fn get_zombie_by_id(&self, id: Word) -> Option<Zombie<'_>> {
        self.id_to_zombie_kind
            .get(&id)
            .map(|kind| Zombie { id, kind })
    }

    fn zombies_used_from_inst<'b>(
        &'b self,
        inst: &'b Instruction,
    ) -> impl Iterator<Item = Zombie<'b>> + 'b {
        inst.result_type
            .into_iter()
            .chain(inst.operands.iter().filter_map(|op| op.id_ref_any()))
            .filter_map(move |id| self.get_zombie_by_id(id))
    }

    fn spread(&mut self, module: &'a Module) -> bool {
        let mut any = false;
        // globals are easy
        {
            let mut debug_src_loc_inst = None;
            for inst in module.global_inst_iter() {
                match inst.class.opcode {
                    Op::Line => debug_src_loc_inst = Some(inst),
                    Op::NoLine => debug_src_loc_inst = None,
                    Op::ExtInst
                        if Some(inst.operands[0].unwrap_id_ref())
                            == self.custom_ext_inst_set_import =>
                    {
                        match CustomOp::decode_from_ext_inst(inst) {
                            CustomOp::SetDebugSrcLoc => debug_src_loc_inst = Some(inst),
                            CustomOp::ClearDebugSrcLoc => debug_src_loc_inst = None,
                            _ => {}
                        }
                    }
                    _ => {}
                }

                if let Some(result_id) = inst.result_id {
                    if self.id_to_zombie_kind.contains_key(&result_id) {
                        continue;
                    }
                    let zombie_uses: Vec<_> = self
                        .zombies_used_from_inst(inst)
                        .map(|zombie| ZombieUse {
                            used_zombie_id: zombie.id,
                            use_debug_src_loc_inst: debug_src_loc_inst,
                            origin: UseOrigin::GlobalOperandOrResultType,
                        })
                        .collect();
                    if !zombie_uses.is_empty() {
                        self.id_to_zombie_kind
                            .insert(result_id, ZombieKind::Uses(zombie_uses));
                        any = true;
                    }
                }
            }
        }
        // No need to zombie defs within a function: If any def within a function is zombied, then the
        // whole function is zombied. But, we don't have to mark the defs within a function as zombie,
        // because the defs can't escape the function.
        // HACK(eddyb) one exception to this is function-local variables, or the
        // `OpBitcast`s of pointer casts, either of which which may actually be
        // unused and as such cannot be allowed to always zombie the function.
        for func in &module.functions {
            let func_id = func.def_id().unwrap();
            if self.id_to_zombie_kind.contains_key(&func_id) {
                // Func is already zombie, no need to scan it again.
                continue;
            }

            let mut all_zombie_uses_in_func = vec![];
            let mut debug_src_loc_inst = None;
            for inst in func.all_inst_iter() {
                match inst.class.opcode {
                    Op::Line => debug_src_loc_inst = Some(inst),
                    // NOTE(eddyb) each block starts out with cleared debuginfo.
                    Op::Label | Op::NoLine => debug_src_loc_inst = None,
                    Op::ExtInst
                        if Some(inst.operands[0].unwrap_id_ref())
                            == self.custom_ext_inst_set_import =>
                    {
                        match CustomOp::decode_from_ext_inst(inst) {
                            CustomOp::SetDebugSrcLoc => debug_src_loc_inst = Some(inst),
                            CustomOp::ClearDebugSrcLoc => debug_src_loc_inst = None,
                            _ => {}
                        }
                    }
                    _ => {}
                }

                if [Op::Variable, Op::Bitcast].contains(&inst.class.opcode) {
                    let result_id = inst.result_id.unwrap();
                    if self.id_to_zombie_kind.contains_key(&result_id) {
                        continue;
                    }
                    let zombie_uses: Vec<_> = self
                        .zombies_used_from_inst(inst)
                        .map(|zombie| ZombieUse {
                            used_zombie_id: zombie.id,
                            use_debug_src_loc_inst: debug_src_loc_inst,
                            origin: UseOrigin::IntraFuncOperandOrResultType {
                                parent_func_id: func_id,
                            },
                        })
                        .collect();
                    if !zombie_uses.is_empty() {
                        self.id_to_zombie_kind
                            .insert(result_id, ZombieKind::Uses(zombie_uses));
                        any = true;
                    }
                    continue;
                }

                all_zombie_uses_in_func.extend(
                    inst.result_id
                        .and_then(|result_id| self.get_zombie_by_id(result_id))
                        .into_iter()
                        .chain(self.zombies_used_from_inst(inst))
                        .map(|zombie| {
                            let origin = if inst.class.opcode == Op::FunctionCall
                                && inst.operands[0] == Operand::IdRef(zombie.id)
                            {
                                UseOrigin::CallCalleeOperand {
                                    caller_func_id: func_id,
                                }
                            } else {
                                UseOrigin::IntraFuncOperandOrResultType {
                                    parent_func_id: func_id,
                                }
                            };
                            ZombieUse {
                                used_zombie_id: zombie.id,
                                use_debug_src_loc_inst: debug_src_loc_inst,
                                origin,
                            }
                        }),
                );
            }
            if !all_zombie_uses_in_func.is_empty() {
                self.id_to_zombie_kind
                    .insert(func_id, ZombieKind::Uses(all_zombie_uses_in_func));
                any = true;
            }
        }
        any
    }
}

struct ZombieReporter<'a> {
    sess: &'a Session,
    module: &'a Module,
    zombies: &'a Zombies<'a>,

    id_to_name: Option<FxHashMap<Word, &'a str>>,
    span_regen: SpanRegenerator<'a>,
}
impl<'a> ZombieReporter<'a> {
    fn new(sess: &'a Session, module: &'a Module, zombies: &'a Zombies<'a>) -> Self {
        Self {
            sess,
            module,
            zombies,

            id_to_name: None,
            span_regen: SpanRegenerator::new(sess.source_map(), module),
        }
    }

    // If an entry point references a zombie'd value, then the entry point would normally get removed.
    // That's an absolutely horrible experience to debug, though, so instead, create a nice error
    // message containing the stack trace of how the entry point got to the zombie value.
    fn report_all(mut self) -> super::Result<()> {
        let mut result = Ok(());
        // FIXME(eddyb) this loop means that every entry-point can potentially
        // list out all the leaves, but that shouldn't be a huge issue.
        for root_id in super::dce::collect_roots(self.module) {
            if let Some(zombie) = self.zombies.get_zombie_by_id(root_id) {
                for (_, mut err) in self.build_errors_keyed_by_leaf_id(zombie) {
                    result = Err(err.emit());
                }
            }
        }
        result
    }

    fn add_use_note_to_err(
        &mut self,
        err: &mut DiagnosticBuilder<'a, ErrorGuaranteed>,
        span: Span,
        zombie: Zombie<'_>,
        zombie_use: &ZombieUse<'_>,
    ) {
        let mut id_to_name = |id, kind| {
            self.id_to_name
                .get_or_insert_with(|| get_names(self.module))
                .get(&id)
                .map_or_else(
                    || format!("unnamed {kind} (%{id})"),
                    |&name| format!("`{name}`"),
                )
        };
        let note = match zombie_use.origin {
            UseOrigin::GlobalOperandOrResultType => {
                format!("used by {}", id_to_name(zombie.id, "global"))
            }
            UseOrigin::IntraFuncOperandOrResultType { parent_func_id } => {
                format!(
                    "used from within {}",
                    id_to_name(parent_func_id, "function")
                )
            }
            UseOrigin::CallCalleeOperand { caller_func_id } => {
                format!("called by {}", id_to_name(caller_func_id, "function"))
            }
        };
        let span = zombie_use
            .use_debug_src_loc_inst
            .and_then(|inst| self.span_regen.src_loc_from_debug_inst(inst))
            .and_then(|src_loc| self.span_regen.src_loc_to_rustc(src_loc))
            .unwrap_or(span);
        err.span_note(span, note);
    }

    fn build_errors_keyed_by_leaf_id(
        &mut self,
        zombie: Zombie<'_>,
    ) -> FxIndexMap<Word, DiagnosticBuilder<'a, ErrorGuaranteed>> {
        // FIXME(eddyb) this is a bit inefficient, compared to some kind of
        // "small map", but this is the error path, and being correct is more
        // important here - in particular, we don't want to ignore *any* leaves.
        let mut errors_keyed_by_leaf_id = FxIndexMap::default();

        let span = self
            .span_regen
            .src_loc_for_id(zombie.id)
            .and_then(|src_loc| self.span_regen.src_loc_to_rustc(src_loc))
            .unwrap_or(DUMMY_SP);
        match zombie.kind {
            ZombieKind::Leaf => {
                let reason = self.span_regen.zombie_for_id(zombie.id).unwrap().reason;
                errors_keyed_by_leaf_id.insert(
                    zombie.id,
                    self.sess.struct_span_err(span, reason.to_string()),
                );
            }
            ZombieKind::Uses(zombie_uses) => {
                for zombie_use in zombie_uses {
                    let used_zombie = self
                        .zombies
                        .get_zombie_by_id(zombie_use.used_zombie_id)
                        .unwrap();
                    for (leaf_id, err) in self.build_errors_keyed_by_leaf_id(used_zombie) {
                        use rustc_data_structures::fx::IndexEntry as Entry;
                        match errors_keyed_by_leaf_id.entry(leaf_id) {
                            Entry::Occupied(_) => err.cancel(),
                            Entry::Vacant(entry) => {
                                self.add_use_note_to_err(
                                    entry.insert(err),
                                    span,
                                    zombie,
                                    zombie_use,
                                );
                            }
                        }
                    }
                }
            }
        }
        errors_keyed_by_leaf_id
    }
}

pub fn report_and_remove_zombies(
    sess: &Session,
    opts: &super::Options,
    module: &mut Module,
) -> super::Result<()> {
    let mut zombies = Zombies {
        // FIXME(eddyb) avoid repeating this across different passes/helpers.
        custom_ext_inst_set_import: module
            .ext_inst_imports
            .iter()
            .find(|inst| {
                assert_eq!(inst.class.opcode, Op::ExtInstImport);
                inst.operands[0].unwrap_literal_string() == &custom_insts::CUSTOM_EXT_INST_SET[..]
            })
            .map(|inst| inst.result_id.unwrap()),

        id_to_zombie_kind: ZombieDecoration::decode_all(module)
            .map(|(id, _)| (id, ZombieKind::Leaf))
            .collect(),
    };
    // Note: This is O(n^2).
    while zombies.spread(module) {}

    let result = ZombieReporter::new(sess, module, &zombies).report_all();

    // FIXME(eddyb) use `log`/`tracing` instead.
    if opts.print_all_zombie {
        let mut span_regen = SpanRegenerator::new(sess.source_map(), module);
        for &zombie_id in zombies.id_to_zombie_kind.keys() {
            let mut zombie_leaf_id = zombie_id;
            let mut infection_chain = SmallVec::<[_; 4]>::new();
            loop {
                zombie_leaf_id = match zombies.get_zombie_by_id(zombie_leaf_id).unwrap().kind {
                    ZombieKind::Leaf => break,
                    // FIXME(eddyb) this is all very lossy and should probably go away.
                    ZombieKind::Uses(zombie_uses) => zombie_uses[0].used_zombie_id,
                };
                infection_chain.push(zombie_leaf_id);
            }

            let reason = span_regen.zombie_for_id(zombie_leaf_id).unwrap().reason;
            eprint!("zombie'd %{zombie_id} because {reason}");
            if !infection_chain.is_empty() {
                eprint!(" (infected via {:?})", infection_chain);
            }
            eprintln!();
        }
    }

    if opts.print_zombie {
        let mut span_regen = SpanRegenerator::new(sess.source_map(), module);
        let names = get_names(module);
        for f in &module.functions {
            if let Some(zombie) = zombies.get_zombie_by_id(f.def_id().unwrap()) {
                let mut zombie_leaf_id = zombie.id;
                loop {
                    zombie_leaf_id = match zombies.get_zombie_by_id(zombie_leaf_id).unwrap().kind {
                        ZombieKind::Leaf => break,
                        // FIXME(eddyb) this is all very lossy and should probably go away.
                        ZombieKind::Uses(zombie_uses) => zombie_uses[0].used_zombie_id,
                    };
                }

                let name = get_name(&names, f.def_id().unwrap());
                let reason = span_regen.zombie_for_id(zombie_leaf_id).unwrap().reason;
                eprintln!("function removed {name:?} because {reason:?}");
            }
        }
    }

    // FIXME(eddyb) this should be unnecessary, either something is unused, and
    // it will get DCE'd *anyway*, or it caused an error.
    {
        // HACK(eddyb) cannot use the original map because it borrows the `Module`.
        let all_zombies: FxHashSet<_> = zombies.id_to_zombie_kind.into_keys().collect();
        let keep = |inst: &Instruction| {
            if let Some(result_id) = inst.result_id {
                !all_zombies.contains(&result_id)
            } else {
                let mut inst_ids = inst
                    .result_type
                    .into_iter()
                    .chain(inst.operands.iter().filter_map(|op| op.id_ref_any()));
                !inst_ids.any(|id| all_zombies.contains(&id))
            }
        };
        module.capabilities.retain(keep);
        module.extensions.retain(keep);
        module.ext_inst_imports.retain(keep);
        module.memory_model = module.memory_model.take().filter(keep);
        module.entry_points.retain(keep);
        module.execution_modes.retain(keep);
        module.debug_string_source.retain(keep);
        module.debug_names.retain(keep);
        module.debug_module_processed.retain(keep);
        module.annotations.retain(keep);
        module.types_global_values.retain(keep);
        module.functions.retain(|f| keep(f.def.as_ref().unwrap()));
    }

    result
}
