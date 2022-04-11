mod constant;
mod declare;
mod entry;
mod type_;

use crate::builder::{ExtInst, InstructionTable};
use crate::builder_spirv::{BuilderCursor, BuilderSpirv, SpirvConst, SpirvValue, SpirvValueKind};
use crate::decorations::{
    CustomDecoration, SerializedSpan, UnrollLoopsDecoration, ZombieDecoration,
};
use crate::spirv_type::{SpirvType, SpirvTypePrinter, TypeCache};
use crate::symbols::Symbols;
use crate::target::SpirvTarget;

use rspirv::dr::{Module, Operand};
use rspirv::spirv::{Decoration, LinkageType, Op, Word};
use rustc_ast::ast::{InlineAsmOptions, InlineAsmTemplatePiece};
use rustc_codegen_ssa::mir::debuginfo::{FunctionDebugContext, VariableKind};
use rustc_codegen_ssa::traits::{
    AsmMethods, BackendTypes, CoverageInfoMethods, DebugInfoMethods, GlobalAsmOperandRef,
    MiscMethods,
};
use rustc_data_structures::fx::{FxHashMap, FxHashSet};
use rustc_middle::mir::mono::CodegenUnit;
use rustc_middle::mir::Body;
use rustc_middle::ty::layout::{HasParamEnv, HasTyCtxt};
use rustc_middle::ty::{Instance, ParamEnv, PolyExistentialTraitRef, Ty, TyCtxt};
use rustc_session::Session;
use rustc_span::def_id::{DefId, LOCAL_CRATE};
use rustc_span::symbol::{sym, Symbol};
use rustc_span::{SourceFile, Span, DUMMY_SP};
use rustc_target::abi::call::{FnAbi, PassMode};
use rustc_target::abi::{HasDataLayout, TargetDataLayout};
use rustc_target::spec::{HasTargetSpec, Target};
use std::cell::{Cell, RefCell};
use std::iter::once;
use std::path::Path;
use std::rc::Rc;
use std::str::FromStr;

pub struct CodegenCx<'tcx> {
    pub tcx: TyCtxt<'tcx>,
    pub codegen_unit: &'tcx CodegenUnit<'tcx>,
    /// Spir-v module builder
    pub builder: BuilderSpirv,
    /// Map from MIR function to spir-v function ID
    pub instances: RefCell<FxHashMap<Instance<'tcx>, SpirvValue>>,
    /// Map from function ID to parameter list
    pub function_parameter_values: RefCell<FxHashMap<Word, Vec<SpirvValue>>>,
    pub type_cache: TypeCache<'tcx>,
    /// Cache generated vtables
    pub vtables: RefCell<FxHashMap<(Ty<'tcx>, Option<PolyExistentialTraitRef<'tcx>>), SpirvValue>>,
    pub ext_inst: RefCell<ExtInst>,
    /// Invalid spir-v IDs that should be stripped from the final binary,
    /// each with its own reason and span that should be used for reporting
    /// (in the event that the value is actually needed)
    zombie_decorations: RefCell<FxHashMap<Word, ZombieDecoration>>,
    /// Functions that have `#[spirv(unroll_loops)]`, and therefore should
    /// get `LoopControl::UNROLL` applied to all of their loops' `OpLoopMerge`
    /// instructions, during structuralization.
    unroll_loops_decorations: RefCell<FxHashSet<Word>>,
    /// Cache of all the builtin symbols we need
    pub sym: Rc<Symbols>,
    pub instruction_table: InstructionTable,
    pub libm_intrinsics: RefCell<FxHashMap<Word, super::builder::libm_intrinsics::LibmIntrinsic>>,

    /// Simple `panic!("...")` and builtin panics (from MIR `Assert`s) call `#[lang = "panic"]`.
    pub panic_fn_id: Cell<Option<Word>>,
    /// Intrinsic for loading a <T> from a &[u32]. The PassMode is the mode of the <T>.
    pub buffer_load_intrinsic_fn_id: RefCell<FxHashMap<Word, PassMode>>,
    /// Intrinsic for storing a <T> into a &[u32]. The PassMode is the mode of the <T>.
    pub buffer_store_intrinsic_fn_id: RefCell<FxHashMap<Word, PassMode>>,
    /// Builtin bounds-checking panics (from MIR `Assert`s) call `#[lang = "panic_bounds_check"]`.
    pub panic_bounds_check_fn_id: Cell<Option<Word>>,

    /// Some runtimes (e.g. intel-compute-runtime) disallow atomics on i8 and i16, even though it's allowed by the spec.
    /// This enables/disables them.
    pub i8_i16_atomics_allowed: bool,

    pub codegen_args: CodegenArgs,

    /// Information about the SPIR-V target.
    pub target: SpirvTarget,
}

impl<'tcx> CodegenCx<'tcx> {
    pub fn new(tcx: TyCtxt<'tcx>, codegen_unit: &'tcx CodegenUnit<'tcx>) -> Self {
        let sym = Symbols::get();

        let mut feature_names = tcx
            .sess
            .target_features
            .iter()
            .map(|s| s.as_str())
            .collect::<Vec<_>>();

        // target_features is a HashSet, not a Vec, so we need to sort to have deterministic
        // compilation - otherwise, the order of capabilities in binaries depends on the iteration
        // order of the hashset. Sort by the string, since that's easy.
        feature_names.sort_unstable();

        let features = feature_names
            .into_iter()
            .map(|s| s.parse())
            .collect::<Result<_, String>>()
            .unwrap_or_else(|error| {
                tcx.sess.err(&error);
                Vec::new()
            });

        let codegen_args = CodegenArgs::from_session(tcx.sess);
        let target = tcx.sess.target.llvm_target.parse().unwrap();

        Self {
            tcx,
            codegen_unit,
            builder: BuilderSpirv::new(&sym, &target, &features),
            instances: Default::default(),
            function_parameter_values: Default::default(),
            type_cache: Default::default(),
            vtables: Default::default(),
            ext_inst: Default::default(),
            zombie_decorations: Default::default(),
            unroll_loops_decorations: Default::default(),
            target,
            sym,
            instruction_table: InstructionTable::new(),
            libm_intrinsics: Default::default(),
            panic_fn_id: Default::default(),
            buffer_load_intrinsic_fn_id: Default::default(),
            buffer_store_intrinsic_fn_id: Default::default(),
            panic_bounds_check_fn_id: Default::default(),
            i8_i16_atomics_allowed: false,
            codegen_args,
        }
    }

    /// See comment on `BuilderCursor`
    pub fn emit_global(&self) -> std::cell::RefMut<'_, rspirv::dr::Builder> {
        self.builder.builder(BuilderCursor {
            function: None,
            block: None,
        })
    }

    /// See comment on `BuilderCursor`
    pub fn emit_with_cursor(
        &self,
        cursor: BuilderCursor,
    ) -> std::cell::RefMut<'_, rspirv::dr::Builder> {
        self.builder.builder(cursor)
    }

    #[track_caller]
    pub fn lookup_type(&self, ty: Word) -> SpirvType {
        self.type_cache.lookup(ty)
    }

    pub fn debug_type<'cx>(&'cx self, ty: Word) -> SpirvTypePrinter<'cx, 'tcx> {
        self.lookup_type(ty).debug(ty, self)
    }

    /// Zombie system:
    /// When compiling libcore and other system libraries, if something unrepresentable is
    /// encountered, we don't want to fail the compilation. Instead, we emit something bogus
    /// (usually it's fairly faithful, though, e.g. u128 emits `OpTypeInt 128`), and then mark the
    /// resulting ID as a "zombie". We continue compiling the rest of the crate, then, at the very
    /// end, anything that transtively references a zombie value is stripped from the binary.
    ///
    /// If an exported function is stripped, then we emit a special "zombie export" item, which is
    /// consumed by the linker, which continues to infect other values that reference it.
    ///
    /// Finally, if *user* code is marked as zombie, then this means that the user tried to do
    /// something that isn't supported, and should be an error.
    pub fn zombie_with_span(&self, word: Word, span: Span, reason: &str) {
        if self.is_system_crate() {
            self.zombie_even_in_user_code(word, span, reason);
        } else {
            self.tcx.sess.span_err(span, reason);
        }
    }
    pub fn zombie_no_span(&self, word: Word, reason: &str) {
        self.zombie_with_span(word, DUMMY_SP, reason);
    }
    pub fn zombie_even_in_user_code(&self, word: Word, span: Span, reason: &str) {
        self.zombie_decorations.borrow_mut().insert(
            word,
            ZombieDecoration {
                reason: reason.to_string(),
                span: SerializedSpan::from_rustc(span, self.tcx.sess.source_map()),
            },
        );
    }

    pub fn is_system_crate(&self) -> bool {
        self.tcx
            .sess
            .contains_name(self.tcx.hir().krate_attrs(), sym::compiler_builtins)
            || self.tcx.crate_name(LOCAL_CRATE) == sym::core
            || self.tcx.crate_name(LOCAL_CRATE) == self.sym.spirv_std
            || self.tcx.crate_name(LOCAL_CRATE) == self.sym.libm
            || self.tcx.crate_name(LOCAL_CRATE) == self.sym.num_traits
    }

    pub fn finalize_module(self) -> Module {
        let mut result = self.builder.finalize();
        result.annotations.extend(
            self.zombie_decorations
                .into_inner()
                .into_iter()
                .map(|(id, zombie)| zombie.encode(id))
                .chain(
                    self.unroll_loops_decorations
                        .into_inner()
                        .into_iter()
                        .map(|id| UnrollLoopsDecoration {}.encode(id)),
                ),
        );
        result
    }

    pub fn set_linkage(&self, target: Word, name: String, linkage: LinkageType) {
        self.emit_global().decorate(
            target,
            Decoration::LinkageAttributes,
            once(Operand::LiteralString(name)).chain(once(Operand::LinkageType(linkage))),
        );
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum SpirvMetadata {
    None,
    NameVariables,
    Full,
}

pub struct CodegenArgs {
    pub module_output_type: ModuleOutputType,
    pub disassemble: bool,
    pub disassemble_fn: Option<String>,
    pub disassemble_entry: Option<String>,
    pub disassemble_globals: bool,

    pub spirv_metadata: SpirvMetadata,

    // spirv-val flags
    pub relax_struct_store: bool,
    pub relax_logical_pointer: bool,
    pub relax_block_layout: Option<bool>,
    pub uniform_buffer_standard_layout: bool,
    pub scalar_block_layout: bool,
    pub skip_block_layout: bool,

    // spirv-opt flags
    pub preserve_bindings: bool,
}

impl CodegenArgs {
    pub fn from_session(sess: &Session) -> Self {
        match CodegenArgs::parse(&sess.opts.cg.llvm_args) {
            Ok(ok) => ok,
            Err(err) => sess.fatal(&format!("Unable to parse llvm-args: {}", err)),
        }
    }

    pub fn parse(args: &[String]) -> Result<Self, rustc_session::getopts::Fail> {
        use rustc_session::getopts;
        let mut opts = getopts::Options::new();
        opts.optopt(
            "",
            "module-output",
            "single output or multiple output",
            "[single|multiple]",
        );
        opts.optflagopt("", "disassemble", "print module to stderr", "");
        opts.optopt("", "disassemble-fn", "print function to stderr", "NAME");
        opts.optopt(
            "",
            "disassemble-entry",
            "print entry point to stderr",
            "NAME",
        );
        opts.optflagopt("", "disassemble-globals", "print globals to stderr", "");

        opts.optopt("", "spirv-metadata", "how much metadata to include", "");

        opts.optflagopt("", "relax-struct-store", "Allow store from one struct type to a different type with compatible layout and members.", "");
        opts.optflagopt("", "relax-logical-pointer", "Allow allocating an object of a pointer type and returning a pointer value from a function in logical addressing mode", "");
        opts.optflagopt("", "relax-block-layout", "Enable VK_KHR_relaxed_block_layout when checking standard uniform, storage buffer, and push constant layouts. This is the default when targeting Vulkan 1.1 or later.", "");
        opts.optflagopt("", "uniform-buffer-standard-layout", "Enable VK_KHR_uniform_buffer_standard_layout when checking standard uniform buffer layouts.", "");
        opts.optflagopt("", "scalar-block-layout", "Enable VK_EXT_scalar_block_layout when checking standard uniform, storage buffer, and push constant layouts. Scalar layout rules are more permissive than relaxed block layout so in effect this will override the --relax-block-layout option.", "");
        opts.optflagopt("", "skip-block-layout", "Skip checking standard uniform/storage buffer layout. Overrides any --relax-block-layout or --scalar-block-layout option.", "");

        opts.optflagopt(
            "",
            "preserve-bindings",
            "Preserve unused descriptor bindings. Useful for reflection.",
            "",
        );

        let matches = opts.parse(args)?;
        let module_output_type =
            matches.opt_get_default("module-output", ModuleOutputType::Single)?;
        let disassemble = matches.opt_present("disassemble");
        let disassemble_fn = matches.opt_str("disassemble-fn");
        let disassemble_entry = matches.opt_str("disassemble-entry");
        let disassemble_globals = matches.opt_present("disassemble-globals");

        let spirv_metadata = matches.opt_str("spirv-metadata");

        let relax_struct_store = matches.opt_present("relax-struct-store");
        let relax_logical_pointer = matches.opt_present("relax-logical-pointer");
        let relax_block_layout = matches.opt_present("relax-block-layout");
        let uniform_buffer_standard_layout = matches.opt_present("uniform-buffer-standard-layout");
        let scalar_block_layout = matches.opt_present("scalar-block-layout");
        let skip_block_layout = matches.opt_present("skip-block-layout");

        let preserve_bindings = matches.opt_present("preserve-bindings");

        let relax_block_layout = if relax_block_layout { Some(true) } else { None };

        let spirv_metadata = match spirv_metadata.as_deref() {
            None => SpirvMetadata::None,
            Some("full") => SpirvMetadata::Full,
            Some("name-variables") => SpirvMetadata::NameVariables,
            Some(v) => {
                return Err(rustc_session::getopts::Fail::UnrecognizedOption(
                    v.to_string(),
                ))
            }
        };

        Ok(Self {
            module_output_type,
            disassemble,
            disassemble_fn,
            disassemble_entry,
            disassemble_globals,

            spirv_metadata,

            relax_struct_store,
            relax_logical_pointer,
            relax_block_layout,
            uniform_buffer_standard_layout,
            scalar_block_layout,
            skip_block_layout,

            preserve_bindings,
        })
    }

    pub fn do_disassemble(&self, module: &Module) {
        fn compact_ids(module: &mut rspirv::dr::Function) -> u32 {
            let mut remap = std::collections::HashMap::new();
            let mut insert = |current_id: &mut u32| {
                let len = remap.len();
                *current_id = *remap.entry(*current_id).or_insert_with(|| len as u32 + 1);
            };
            module.all_inst_iter_mut().for_each(|inst| {
                if let Some(ref mut result_id) = &mut inst.result_id {
                    insert(result_id);
                }
                if let Some(ref mut result_type) = &mut inst.result_type {
                    insert(result_type);
                }
                inst.operands.iter_mut().for_each(|op| {
                    if let Some(w) = op.id_ref_any_mut() {
                        insert(w);
                    }
                });
            });
            remap.len() as u32 + 1
        }

        use rspirv::binary::Disassemble;

        if self.disassemble {
            eprintln!("{}", module.disassemble());
        }

        if let Some(func) = &self.disassemble_fn {
            let id = module
                .debug_names
                .iter()
                .find(|inst| {
                    inst.class.opcode == rspirv::spirv::Op::Name
                        && inst.operands[1].unwrap_literal_string() == func
                })
                .unwrap_or_else(|| {
                    panic!(
                        "no function with the name `{}` found in:\n{}\n",
                        func,
                        module.disassemble()
                    )
                })
                .operands[0]
                .unwrap_id_ref();
            let mut func = module
                .functions
                .iter()
                .find(|f| f.def_id().unwrap() == id)
                .unwrap()
                .clone();
            // Compact to make IDs more stable
            compact_ids(&mut func);
            eprintln!("{}", func.disassemble());
        }

        if let Some(entry) = &self.disassemble_entry {
            let id = module
                .entry_points
                .iter()
                .filter(|inst| inst.class.opcode == rspirv::spirv::Op::EntryPoint)
                .find(|inst| inst.operands[2].unwrap_literal_string() == entry)
                .unwrap_or_else(|| {
                    panic!(
                        "no entry point with the name `{}` found in:\n{}\n",
                        entry,
                        module.disassemble()
                    )
                })
                .operands[1]
                .unwrap_id_ref();
            let mut func = module
                .functions
                .iter()
                .find(|f| f.def_id().unwrap() == id)
                .unwrap()
                .clone();
            // Compact to make IDs more stable
            compact_ids(&mut func);
            eprintln!("{}", func.disassemble());
        }

        if self.disassemble_globals {
            for inst in module.global_inst_iter() {
                // HACK: On Windows, paths are printed like `OpString "D:\\dir\\blah"`.
                // Unfortunately, compiletest will only normalize `D:\dir\blah` to `$DIR/blah` -
                // one backslash, not two. So, when disassembling for compiletest, check if the
                // argument to OpString can be parsed as an absolute path, and if it is, replace it
                // with just the filename component of the path.
                if inst.class.opcode == Op::String {
                    let path = Path::new(inst.operands[0].unwrap_literal_string());
                    if path.is_absolute() {
                        if let Some(file_name) = path.file_name() {
                            let mut inst = inst.clone();
                            inst.operands[0] = Operand::LiteralString(format!(
                                "$OPSTRING_FILENAME/{}",
                                file_name.to_string_lossy(),
                            ));
                            eprintln!("{}", inst.disassemble());
                            continue;
                        }
                    }
                }
                eprintln!("{}", inst.disassemble());
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ModuleOutputType {
    Single,
    Multiple,
}

impl FromStr for ModuleOutputType {
    type Err = rustc_session::getopts::Fail;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "single" => Ok(Self::Single),
            "multiple" => Ok(Self::Multiple),
            v => Err(Self::Err::UnrecognizedOption(v.to_string())),
        }
    }
}

impl<'tcx> BackendTypes for CodegenCx<'tcx> {
    type Value = SpirvValue;
    type Function = SpirvValue;

    type BasicBlock = Word;
    type Type = Word;
    // Funclet: A structure representing an active landing pad for the duration of a basic block. (??)
    // https://doc.rust-lang.org/nightly/nightly-rustc/rustc_codegen_llvm/common/struct.Funclet.html
    type Funclet = ();

    type DIScope = ();
    type DILocation = ();
    type DIVariable = ();
}

impl<'tcx> HasTyCtxt<'tcx> for CodegenCx<'tcx> {
    fn tcx(&self) -> TyCtxt<'tcx> {
        self.tcx
    }
}

impl<'tcx> HasDataLayout for CodegenCx<'tcx> {
    fn data_layout(&self) -> &TargetDataLayout {
        &self.tcx.data_layout
    }
}

impl<'tcx> HasTargetSpec for CodegenCx<'tcx> {
    fn target_spec(&self) -> &Target {
        &self.tcx.sess.target
    }
}

impl<'tcx> HasParamEnv<'tcx> for CodegenCx<'tcx> {
    fn param_env(&self) -> ParamEnv<'tcx> {
        ParamEnv::reveal_all()
    }
}

impl<'tcx> MiscMethods<'tcx> for CodegenCx<'tcx> {
    #[allow(clippy::type_complexity)]
    fn vtables(
        &self,
    ) -> &RefCell<FxHashMap<(Ty<'tcx>, Option<PolyExistentialTraitRef<'tcx>>), Self::Value>> {
        &self.vtables
    }

    fn check_overflow(&self) -> bool {
        self.tcx.sess.overflow_checks()
    }

    fn get_fn(&self, instance: Instance<'tcx>) -> Self::Function {
        self.get_fn_ext(instance)
    }

    // NOTE(eddyb) see the comment on `SpirvValueKind::FnAddr`, this should
    // be fixed upstream, so we never see any "function pointer" values being
    // created just to perform direct calls.
    fn get_fn_addr(&self, instance: Instance<'tcx>) -> Self::Value {
        let function = self.get_fn(instance);
        let span = self.tcx.def_span(instance.def_id());

        let ty = SpirvType::Pointer {
            pointee: function.ty,
        }
        .def(span, self);

        if self.is_system_crate() {
            // Create these undefs up front instead of on demand in SpirvValue::def because
            // SpirvValue::def can't use cx.emit()
            self.builder
                .def_constant(ty, SpirvConst::ZombieUndefForFnAddr);
        }

        SpirvValue {
            kind: SpirvValueKind::FnAddr {
                function: function.def_cx(self),
            },
            ty,
        }
    }

    fn eh_personality(&self) -> Self::Value {
        todo!()
    }

    fn sess(&self) -> &Session {
        self.tcx.sess
    }

    fn codegen_unit(&self) -> &'tcx CodegenUnit<'tcx> {
        self.codegen_unit
    }

    fn used_statics(&self) -> &RefCell<Vec<Self::Value>> {
        todo!()
    }

    fn compiler_used_statics(&self) -> &RefCell<Vec<Self::Value>> {
        todo!()
    }

    fn set_frame_pointer_type(&self, _llfn: Self::Function) {
        todo!()
    }

    fn apply_target_cpu_attr(&self, _llfn: Self::Function) {
        todo!()
    }

    fn create_used_variable(&self) {
        todo!()
    }

    fn create_compiler_used_variable(&self) {
        todo!()
    }

    fn declare_c_main(&self, _fn_type: Self::Type) -> Option<Self::Function> {
        todo!()
    }
}

impl<'tcx> DebugInfoMethods<'tcx> for CodegenCx<'tcx> {
    fn create_vtable_metadata(
        &self,
        _ty: Ty<'tcx>,
        _trait_ref: Option<PolyExistentialTraitRef<'tcx>>,
        _vtable: Self::Value,
    ) {
        // Ignore.
    }

    fn dbg_scope_fn(
        &self,
        _: rustc_middle::ty::Instance<'tcx>,
        _: &FnAbi<'tcx, Ty<'tcx>>,
        _: Option<Self::Function>,
    ) -> Self::DIScope {
        todo!()
    }

    fn dbg_loc(&self, _: Self::DIScope, _: Option<Self::DILocation>, _: Span) -> Self::DILocation {
        todo!()
    }

    fn create_function_debug_context(
        &self,
        _instance: Instance<'tcx>,
        _fn_abi: &FnAbi<'tcx, Ty<'tcx>>,
        _llfn: Self::Function,
        _mir: &Body<'_>,
    ) -> Option<FunctionDebugContext<Self::DIScope, Self::DILocation>> {
        // TODO: This is ignored. Do we want to implement this at some point?
        None
    }

    fn extend_scope_to_file(
        &self,
        _scope_metadata: Self::DIScope,
        _file: &SourceFile,
    ) -> Self::DIScope {
        todo!()
    }

    fn debuginfo_finalize(&self) {
        todo!()
    }

    fn create_dbg_var(
        &self,
        _variable_name: Symbol,
        _variable_type: Ty<'tcx>,
        _scope_metadata: Self::DIScope,
        _variable_kind: VariableKind,
        _span: Span,
    ) -> Self::DIVariable {
        todo!()
    }
}

impl<'tcx> CoverageInfoMethods<'tcx> for CodegenCx<'tcx> {
    fn coverageinfo_finalize(&self) {
        todo!()
    }
    fn get_pgo_func_name_var(&self, _: Instance<'tcx>) -> SpirvValue {
        todo!()
    }
    fn define_unused_fn(&self, _: DefId) {
        todo!()
    }
}

impl<'tcx> AsmMethods for CodegenCx<'tcx> {
    fn codegen_global_asm(
        &self,
        _template: &[InlineAsmTemplatePiece],
        _operands: &[GlobalAsmOperandRef],
        _options: InlineAsmOptions,
        _line_spans: &[Span],
    ) {
        todo!()
    }
}
