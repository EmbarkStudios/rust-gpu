mod constant;
mod declare;
mod type_;

use crate::builder::ExtInst;
use crate::builder_spirv::{BuilderCursor, BuilderSpirv, SpirvValue, SpirvValueExt};
use crate::finalizing_passes::{block_ordering_pass, delete_dead_blocks, zombie_pass};
use crate::spirv_type::{SpirvType, SpirvTypePrinter, TypeCache};
use crate::symbols::Symbols;
use rspirv::dr::{Module, Operand};
use rspirv::spirv::{Decoration, LinkageType, StorageClass, Word};
use rustc_codegen_ssa::mir::debuginfo::{FunctionDebugContext, VariableKind};
use rustc_codegen_ssa::traits::{
    AsmMethods, BackendTypes, CoverageInfoMethods, DebugInfoMethods, MiscMethods,
};
use rustc_data_structures::fx::FxHashMap;
use rustc_hir::GlobalAsm;
use rustc_middle::mir::mono::CodegenUnit;
use rustc_middle::mir::Body;
use rustc_middle::ty::layout::{HasParamEnv, HasTyCtxt};
use rustc_middle::ty::{Instance, ParamEnv, PolyExistentialTraitRef, Ty, TyCtxt};
use rustc_session::Session;
use rustc_span::def_id::CrateNum;
use rustc_span::source_map::Span;
use rustc_span::symbol::Symbol;
use rustc_span::SourceFile;
use rustc_target::abi::call::FnAbi;
use rustc_target::abi::{HasDataLayout, TargetDataLayout};
use rustc_target::spec::{HasTargetSpec, Target};
use std::cell::RefCell;
use std::collections::HashMap;
use std::iter::once;

pub struct CodegenCx<'tcx> {
    pub tcx: TyCtxt<'tcx>,
    pub codegen_unit: &'tcx CodegenUnit<'tcx>,
    /// Spir-v module builder
    pub builder: BuilderSpirv,
    /// Used for the DeclareMethods API (not sure what it is yet)
    pub declared_values: RefCell<HashMap<String, SpirvValue>>,
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
    /// Functions created in get_fn_addr
    function_pointers: RefCell<HashMap<SpirvValue, SpirvValue>>,
}

impl<'tcx> CodegenCx<'tcx> {
    pub fn new(tcx: TyCtxt<'tcx>, codegen_unit: &'tcx CodegenUnit<'tcx>) -> Self {
        Self {
            tcx,
            codegen_unit,
            builder: BuilderSpirv::new(),
            declared_values: Default::default(),
            instances: Default::default(),
            function_parameter_values: Default::default(),
            type_cache: Default::default(),
            vtables: Default::default(),
            ext_inst: Default::default(),
            zombie_values: Default::default(),
            kernel_mode: true,
            sym: Box::new(Symbols::new()),
            function_pointers: Default::default(),
        }
    }

    /// See comment on BuilderCursor
    pub fn emit_global(&self) -> std::cell::RefMut<rspirv::dr::Builder> {
        self.builder.builder(BuilderCursor {
            function: None,
            block: None,
        })
    }

    /// See comment on BuilderCursor
    pub fn emit_with_cursor(
        &self,
        cursor: BuilderCursor,
    ) -> std::cell::RefMut<rspirv::dr::Builder> {
        self.builder.builder(cursor)
    }

    pub fn lookup_type(&self, ty: Word) -> SpirvType {
        self.type_cache.lookup(ty)
    }

    pub fn debug_type<'cx>(&'cx self, ty: Word) -> SpirvTypePrinter<'cx, 'tcx> {
        self.lookup_type(ty).debug(ty, self)
    }

    /// Zombie system:
    /// When compiling libcore and other system libraries, if something unrepresentable is encountered, we don't want to
    /// fail the compilation. Instead, we emit something bogus (usually it's fairly faithful, though, e.g. u128 emits
    /// OpTypeInt 128), and then mark the resulting ID as a "zombie". We continue compiling the rest of the crate, then,
    /// at the very end, anything that transtively references a zombie value is stripped from the binary.
    ///
    /// If an exported function is stripped, then we emit a special "zombie export" item, which is consumed by the
    /// linker, which continues to infect other values that reference it.
    ///
    /// Finally, if *user* code is marked as zombie, then this means that the user tried to do something that isn't
    /// supported, and should be an error.
    pub fn zombie(&self, word: Word, reason: &'static str) {
        self.zombie_values.borrow_mut().insert(word, reason);
    }

    pub fn finalize_module(self) -> Module {
        let mut result = self.builder.finalize();
        // defs go before fns
        result.functions.sort_by_key(|f| !f.blocks.is_empty());
        for function in &mut result.functions {
            // Should delete dead code blocks before zombie pass, in case a dead block references a zombie id.
            delete_dead_blocks(function);
            block_ordering_pass(function);
        }
        zombie_pass(&mut result, &mut self.zombie_values.borrow_mut());
        result
    }

    pub fn set_linkage(&self, target: Word, name: String, linkage: LinkageType) {
        self.emit_global().decorate(
            target,
            Decoration::LinkageAttributes,
            once(Operand::LiteralString(name)).chain(once(Operand::LinkageType(linkage))),
        )
    }

    /// Function pointer registration:
    /// LLVM, and therefore codegen_ssa, is very murky with function values vs. function pointers. So, codegen_ssa has a
    /// pattern where *even for direct function calls*, it uses get_fn_*addr*, and then uses that function *pointer* when
    /// calling BuilderMethods::call(). However, spir-v doesn't support function pointers! So, instead, when get_fn_addr
    /// is called, we register a "token" (via OpUndef), storing it in a dictionary. Then, when BuilderMethods::call() is
    /// called, and it's calling a function pointer, we check the dictionary, and if it is, invoke the function directly.
    /// It's kind of conceptually similar to a constexpr deref, except specialized to just functions.
    pub fn register_fn_ptr(&self, function: SpirvValue) -> SpirvValue {
        let ty = SpirvType::Pointer {
            storage_class: StorageClass::Function,
            pointee: function.ty,
        }
        .def(self);
        // We want a unique ID for these undefs, so don't use the caching system.
        let result = self.emit_global().undef(ty, None).with_type(ty);
        // It's obviously invalid, so zombie it.
        self.zombie(result.def, "get_fn_addr");
        self.function_pointers.borrow_mut().insert(result, function);
        result
    }

    /// See comment on register_fn_ptr
    pub fn lookup_fn_ptr(&self, pointer: SpirvValue) -> Option<SpirvValue> {
        self.function_pointers.borrow().get(&pointer).cloned()
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
        &self.tcx.sess.target.target
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
        let function = self.get_fn_ext(instance);
        self.register_fn_ptr(function)
    }

    fn eh_personality(&self) -> Self::Value {
        todo!()
    }

    fn sess(&self) -> &Session {
        &self.tcx.sess
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
}

impl<'tcx> DebugInfoMethods<'tcx> for CodegenCx<'tcx> {
    fn create_vtable_metadata(&self, _ty: Ty<'tcx>, _vtable: Self::Value) {
        // Ignore.
    }

    fn create_function_debug_context(
        &self,
        _instance: Instance<'tcx>,
        _fn_abi: &FnAbi<'tcx, Ty<'tcx>>,
        _llfn: Self::Function,
        _mir: &Body<'_>,
    ) -> Option<FunctionDebugContext<Self::DIScope>> {
        // TODO: This is ignored. Do we want to implement this at some point?
        None
    }

    fn extend_scope_to_file(
        &self,
        _scope_metadata: Self::DIScope,
        _file: &SourceFile,
        _defining_crate: CrateNum,
    ) -> Self::DIScope {
        todo!()
    }

    fn debuginfo_finalize(&self) {
        todo!()
    }

    fn create_dbg_var(
        &self,
        _dbg_context: &FunctionDebugContext<Self::DIScope>,
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
