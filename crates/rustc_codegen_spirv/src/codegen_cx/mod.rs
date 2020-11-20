mod constant;
mod declare;
mod entry;
mod type_;

use crate::builder::{ExtInst, InstructionTable};
use crate::builder_spirv::{BuilderCursor, BuilderSpirv, SpirvValue, SpirvValueKind};
use crate::finalizing_passes::export_zombies;
use crate::spirv_type::{SpirvType, SpirvTypePrinter, TypeCache};
use crate::symbols::Symbols;
use rspirv::dr::{Module, Operand};
use rspirv::spirv::{Decoration, LinkageType, MemoryModel, StorageClass, Word};
use rustc_codegen_ssa::mir::debuginfo::{FunctionDebugContext, VariableKind};
use rustc_codegen_ssa::traits::{
    AsmMethods, BackendTypes, CoverageInfoMethods, DebugInfoMethods, MiscMethods,
};
use rustc_data_structures::fx::FxHashMap;
use rustc_hir::GlobalAsm;
use rustc_middle::mir::mono::CodegenUnit;
use rustc_middle::mir::Body;
use rustc_middle::ty::layout::{HasParamEnv, HasTyCtxt};
use rustc_middle::ty::{Instance, ParamEnv, PolyExistentialTraitRef, Ty, TyCtxt, TyS};
use rustc_session::Session;
use rustc_span::def_id::LOCAL_CRATE;
use rustc_span::source_map::Span;
use rustc_span::symbol::{sym, Symbol};
use rustc_span::SourceFile;
use rustc_target::abi::call::FnAbi;
use rustc_target::abi::{HasDataLayout, TargetDataLayout};
use rustc_target::spec::{HasTargetSpec, Target};
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::iter::once;

pub struct CodegenCx<'tcx> {
    pub tcx: TyCtxt<'tcx>,
    pub codegen_unit: &'tcx CodegenUnit<'tcx>,
    /// Spir-v module builder
    pub builder: BuilderSpirv,
    /// Map from MIR function to spir-v function ID
    pub instances: RefCell<HashMap<Instance<'tcx>, SpirvValue>>,
    /// Map from function ID to parameter list
    pub function_parameter_values: RefCell<HashMap<Word, Vec<SpirvValue>>>,
    pub type_cache: TypeCache<'tcx>,
    /// Cache generated vtables
    pub vtables: RefCell<FxHashMap<(Ty<'tcx>, Option<PolyExistentialTraitRef<'tcx>>), SpirvValue>>,
    pub ext_inst: RefCell<ExtInst>,
    /// Invalid spir-v IDs that should be stripped from the final binary
    zombie_values: RefCell<HashMap<Word, &'static str>>,
    pub kernel_mode: bool,
    /// Cache of all the builtin symbols we need
    pub sym: Box<Symbols>,
    pub instruction_table: InstructionTable,
    pub really_unsafe_ignore_bitcasts: RefCell<HashSet<SpirvValue>>,
    pub zombie_undefs_for_system_constant_pointers: RefCell<HashMap<Word, Word>>,
    pub libm_intrinsics: RefCell<HashMap<Word, super::builder::libm_intrinsics::LibmIntrinsic>>,
    /// Some runtimes (e.g. intel-compute-runtime) disallow atomics on i8 and i16, even though it's allowed by the spec.
    /// This enables/disables them.
    pub i8_i16_atomics_allowed: bool,
}

impl<'tcx> CodegenCx<'tcx> {
    pub fn new(tcx: TyCtxt<'tcx>, codegen_unit: &'tcx CodegenUnit<'tcx>) -> Self {
        let sym = Box::new(Symbols::new());
        let mut spirv_version = None;
        let mut memory_model = None;
        let mut kernel_mode = false;
        for &feature in &tcx.sess.target_features {
            if feature == sym.kernel {
                kernel_mode = true;
            } else if feature == sym.spirv10 {
                spirv_version = Some((1, 0));
            } else if feature == sym.spirv11 {
                spirv_version = Some((1, 1));
            } else if feature == sym.spirv12 {
                spirv_version = Some((1, 2));
            } else if feature == sym.spirv13 {
                spirv_version = Some((1, 3));
            } else if feature == sym.spirv14 {
                spirv_version = Some((1, 4));
            } else if feature == sym.spirv15 {
                spirv_version = Some((1, 5));
            } else if feature == sym.simple {
                memory_model = Some(MemoryModel::Simple);
            } else if feature == sym.vulkan {
                memory_model = Some(MemoryModel::Vulkan);
            } else if feature == sym.glsl450 {
                memory_model = Some(MemoryModel::GLSL450);
            } else {
                tcx.sess.err(&format!("Unknown feature {}", feature));
            }
        }
        Self {
            tcx,
            codegen_unit,
            builder: BuilderSpirv::new(spirv_version, memory_model, kernel_mode),
            instances: Default::default(),
            function_parameter_values: Default::default(),
            type_cache: Default::default(),
            vtables: Default::default(),
            ext_inst: Default::default(),
            zombie_values: Default::default(),
            kernel_mode,
            sym,
            instruction_table: InstructionTable::new(),
            really_unsafe_ignore_bitcasts: Default::default(),
            zombie_undefs_for_system_constant_pointers: Default::default(),
            libm_intrinsics: Default::default(),
            i8_i16_atomics_allowed: false,
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
    pub fn zombie_with_span(&self, word: Word, span: Span, reason: &'static str) {
        if self.is_system_crate() {
            self.zombie_values.borrow_mut().insert(word, reason);
        } else {
            self.tcx.sess.span_err(span, reason);
        }
    }
    pub fn zombie_no_span(&self, word: Word, reason: &'static str) {
        if self.is_system_crate() {
            self.zombie_values.borrow_mut().insert(word, reason);
        } else {
            self.tcx.sess.err(reason);
        }
    }
    pub fn zombie_even_in_user_code(&self, word: Word, reason: &'static str) {
        self.zombie_values.borrow_mut().insert(word, reason);
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
        export_zombies(&mut result, &self.zombie_values.borrow());
        result
    }

    pub fn set_linkage(&self, target: Word, name: String, linkage: LinkageType) {
        self.emit_global().decorate(
            target,
            Decoration::LinkageAttributes,
            once(Operand::LiteralString(name)).chain(once(Operand::LinkageType(linkage))),
        )
    }

    /// See note on `SpirvValueKind::ConstantPointer`
    pub fn make_constant_pointer(&self, value: SpirvValue) -> SpirvValue {
        let ty = SpirvType::Pointer {
            storage_class: StorageClass::Function,
            pointee: value.ty,
        }
        .def(self);
        if self.is_system_crate() {
            // Create these undefs up front instead of on demand in SpirvValue::def because
            // SpirvValue::def can't use cx.emit()
            self.zombie_undefs_for_system_constant_pointers
                .borrow_mut()
                .entry(ty)
                .or_insert_with(|| {
                    // We want a unique ID for these undefs, so don't use the caching system.
                    self.emit_global().undef(ty, None)
                });
        }
        SpirvValue {
            kind: SpirvValueKind::ConstantPointer(value.def_cx(self)),
            ty,
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

    fn get_fn_addr(&self, instance: Instance<'tcx>) -> Self::Value {
        let function = self.get_fn(instance);
        self.make_constant_pointer(function)
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

    fn set_frame_pointer_elimination(&self, _llfn: Self::Function) {
        todo!()
    }

    fn apply_target_cpu_attr(&self, _llfn: Self::Function) {
        todo!()
    }

    fn create_used_variable(&self) {
        todo!()
    }

    fn declare_c_main(&self, _fn_type: Self::Type) -> Option<Self::Function> {
        todo!()
    }
}

impl<'tcx> DebugInfoMethods<'tcx> for CodegenCx<'tcx> {
    fn create_vtable_metadata(&self, _ty: Ty<'tcx>, _vtable: Self::Value) {
        // Ignore.
    }

    fn dbg_scope_fn(
        &self,
        _: rustc_middle::ty::Instance<'tcx>,
        _: &FnAbi<'tcx, &'tcx TyS<'tcx>>,
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

impl<'tcx> CoverageInfoMethods for CodegenCx<'tcx> {
    fn coverageinfo_finalize(&self) {
        todo!()
    }
}

impl<'tcx> AsmMethods for CodegenCx<'tcx> {
    fn codegen_global_asm(&self, _ga: &GlobalAsm) {
        todo!()
    }
}
