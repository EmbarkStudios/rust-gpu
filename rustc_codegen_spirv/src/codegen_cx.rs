use crate::things::ModuleSpirv;
use rspirv::spirv::Word;
use rustc_codegen_ssa::common::TypeKind;
use rustc_codegen_ssa::mir::debuginfo::FunctionDebugContext;
use rustc_codegen_ssa::mir::debuginfo::VariableKind;
use rustc_codegen_ssa::mir::place::PlaceRef;
use rustc_codegen_ssa::traits::{
    AsmMethods, BackendTypes, BaseTypeMethods, ConstMethods, CoverageInfoMethods, DebugInfoMethods,
    DeclareMethods, LayoutTypeMethods, MiscMethods, PreDefineMethods, StaticMethods,
};
use rustc_data_structures::fx::FxHashMap;
use rustc_hir::GlobalAsm;
use rustc_middle::mir::interpret::Allocation;
use rustc_middle::mir::mono::CodegenUnit;
use rustc_middle::mir::mono::Linkage;
use rustc_middle::mir::mono::Visibility;
use rustc_middle::mir::Body;
use rustc_middle::ty::layout::{HasParamEnv, HasTyCtxt, LayoutError, TyAndLayout};
use rustc_middle::ty::Instance;
use rustc_middle::ty::PolyExistentialTraitRef;
use rustc_middle::ty::{ParamEnv, Ty, TyCtxt};
use rustc_mir::interpret::Scalar;
use rustc_session::Session;
use rustc_span::def_id::CrateNum;
use rustc_span::def_id::DefId;
use rustc_span::source_map::{Span, DUMMY_SP};
use rustc_span::symbol::Symbol;
use rustc_span::SourceFile;
use rustc_target::abi;
use rustc_target::abi::call::{CastTarget, FnAbi, Reg};
use rustc_target::abi::AddressSpace;
use rustc_target::abi::Align;
use rustc_target::abi::{HasDataLayout, LayoutOf, Size, TargetDataLayout};
use rustc_target::spec::{HasTargetSpec, Target};
use std::cell::RefCell;

pub struct CodegenCx<'spv, 'tcx> {
    pub tcx: TyCtxt<'tcx>,
    pub codegen_unit: &'tcx CodegenUnit<'tcx>,
    pub spirv_module: &'spv ModuleSpirv,
}

impl<'spv, 'tcx> CodegenCx<'spv, 'tcx> {
    pub fn new(
        tcx: TyCtxt<'tcx>,
        codegen_unit: &'tcx CodegenUnit<'tcx>,
        spirv_module: &'spv ModuleSpirv,
    ) -> Self {
        Self {
            tcx,
            codegen_unit,
            spirv_module,
        }
    }
}

impl<'spv, 'tcx> BackendTypes for CodegenCx<'spv, 'tcx> {
    type Value = Word;
    type Function = Word;

    type BasicBlock = Word;
    type Type = Word;
    // Funclet: A structure representing an active landing pad for the duration of a basic block. (??)
    // https://doc.rust-lang.org/nightly/nightly-rustc/rustc_codegen_llvm/common/struct.Funclet.html
    type Funclet = ();

    type DIScope = ();
    type DIVariable = ();
}

impl<'spv, 'tcx> LayoutOf for CodegenCx<'spv, 'tcx> {
    type Ty = Ty<'tcx>;
    type TyAndLayout = TyAndLayout<'tcx>;

    fn layout_of(&self, ty: Ty<'tcx>) -> Self::TyAndLayout {
        self.spanned_layout_of(ty, DUMMY_SP)
    }

    fn spanned_layout_of(&self, ty: Ty<'tcx>, span: Span) -> Self::TyAndLayout {
        self.tcx
            .layout_of(ParamEnv::reveal_all().and(ty))
            .unwrap_or_else(|e| {
                if let LayoutError::SizeOverflow(_) = e {
                    self.tcx.sess.span_fatal(span, &e.to_string())
                } else {
                    panic!("failed to get layout for `{}`: {}", ty, e)
                }
            })
    }
}

impl<'spv, 'tcx> HasTyCtxt<'tcx> for CodegenCx<'spv, 'tcx> {
    fn tcx(&self) -> TyCtxt<'tcx> {
        self.tcx
    }
}

impl<'spv, 'tcx> HasDataLayout for CodegenCx<'spv, 'tcx> {
    fn data_layout(&self) -> &TargetDataLayout {
        &self.tcx.data_layout
    }
}

impl<'spv, 'tcx> HasTargetSpec for CodegenCx<'spv, 'tcx> {
    fn target_spec(&self) -> &Target {
        &self.tcx.sess.target.target
    }
}

impl<'spv, 'tcx> HasParamEnv<'tcx> for CodegenCx<'spv, 'tcx> {
    fn param_env(&self) -> ParamEnv<'tcx> {
        ParamEnv::reveal_all()
    }
}

impl<'spv, 'tcx> LayoutTypeMethods<'tcx> for CodegenCx<'spv, 'tcx> {
    fn backend_type(&self, layout: TyAndLayout<'tcx>) -> Self::Type {
        todo!()
    }

    fn immediate_backend_type(&self, layout: TyAndLayout<'tcx>) -> Self::Type {
        todo!()
    }

    fn is_backend_immediate(&self, layout: TyAndLayout<'tcx>) -> bool {
        todo!()
    }

    fn is_backend_scalar_pair(&self, layout: TyAndLayout<'tcx>) -> bool {
        todo!()
    }

    fn backend_field_index(&self, layout: TyAndLayout<'tcx>, index: usize) -> u64 {
        todo!()
    }

    fn scalar_pair_element_backend_type(
        &self,
        layout: TyAndLayout<'tcx>,
        index: usize,
        immediate: bool,
    ) -> Self::Type {
        todo!()
    }

    fn cast_backend_type(&self, ty: &CastTarget) -> Self::Type {
        todo!()
    }

    fn fn_ptr_backend_type(&self, fn_abi: &FnAbi<'tcx, Ty<'tcx>>) -> Self::Type {
        todo!()
    }

    fn reg_backend_type(&self, ty: &Reg) -> Self::Type {
        todo!()
    }
}

impl<'spv, 'tcx> BaseTypeMethods<'tcx> for CodegenCx<'spv, 'tcx> {
    fn type_i1(&self) -> Self::Type {
        todo!()
    }
    fn type_i8(&self) -> Self::Type {
        todo!()
    }
    fn type_i16(&self) -> Self::Type {
        todo!()
    }
    fn type_i32(&self) -> Self::Type {
        todo!()
    }
    fn type_i64(&self) -> Self::Type {
        todo!()
    }
    fn type_i128(&self) -> Self::Type {
        todo!()
    }
    fn type_isize(&self) -> Self::Type {
        todo!()
    }

    fn type_f32(&self) -> Self::Type {
        todo!()
    }
    fn type_f64(&self) -> Self::Type {
        todo!()
    }

    fn type_func(&self, args: &[Self::Type], ret: Self::Type) -> Self::Type {
        todo!()
    }
    fn type_struct(&self, els: &[Self::Type], packed: bool) -> Self::Type {
        todo!()
    }
    fn type_kind(&self, ty: Self::Type) -> TypeKind {
        todo!()
    }
    fn type_ptr_to(&self, ty: Self::Type) -> Self::Type {
        todo!()
    }
    fn type_ptr_to_ext(&self, ty: Self::Type, address_space: AddressSpace) -> Self::Type {
        todo!()
    }
    fn element_type(&self, ty: Self::Type) -> Self::Type {
        todo!()
    }

    /// Returns the number of elements in `self` if it is a LLVM vector type.
    fn vector_length(&self, ty: Self::Type) -> usize {
        todo!()
    }

    fn float_width(&self, ty: Self::Type) -> usize {
        todo!()
    }

    /// Retrieves the bit width of the integer type `self`.
    fn int_width(&self, ty: Self::Type) -> u64 {
        todo!()
    }

    fn val_ty(&self, v: Self::Value) -> Self::Type {
        todo!()
    }
}

impl<'spv, 'tcx> StaticMethods for CodegenCx<'spv, 'tcx> {
    fn static_addr_of(&self, cv: Self::Value, align: Align, kind: Option<&str>) -> Self::Value {
        todo!()
    }
    fn codegen_static(&self, def_id: DefId, is_mutable: bool) {
        todo!()
    }

    /// Mark the given global value as "used", to prevent a backend from potentially removing a
    /// static variable that may otherwise appear unused.
    ///
    /// Static variables in Rust can be annotated with the `#[used]` attribute to direct the `rustc`
    /// compiler to mark the variable as a "used global".
    fn add_used_global(&self, global: Self::Value) {
        todo!()
    }
}

impl<'spv, 'tcx> MiscMethods<'tcx> for CodegenCx<'spv, 'tcx> {
    fn vtables(
        &self,
    ) -> &RefCell<FxHashMap<(Ty<'tcx>, Option<PolyExistentialTraitRef<'tcx>>), Self::Value>> {
        todo!()
    }

    fn check_overflow(&self) -> bool {
        todo!()
    }

    fn get_fn(&self, instance: Instance<'tcx>) -> Self::Function {
        todo!()
    }

    fn get_fn_addr(&self, instance: Instance<'tcx>) -> Self::Value {
        todo!()
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

    fn set_frame_pointer_elimination(&self, llfn: Self::Function) {
        todo!()
    }

    fn apply_target_cpu_attr(&self, llfn: Self::Function) {
        todo!()
    }

    fn create_used_variable(&self) {
        todo!()
    }
}

impl<'spv, 'tcx> PreDefineMethods<'tcx> for CodegenCx<'spv, 'tcx> {
    fn predefine_static(
        &self,
        def_id: DefId,
        linkage: Linkage,
        visibility: Visibility,
        symbol_name: &str,
    ) {
        todo!()
    }

    fn predefine_fn(
        &self,
        instance: Instance<'tcx>,
        linkage: Linkage,
        visibility: Visibility,
        symbol_name: &str,
    ) {
        todo!()
    }
}

impl<'spv, 'tcx> DeclareMethods<'tcx> for CodegenCx<'spv, 'tcx> {
    fn declare_global(&self, name: &str, ty: Self::Type) -> Self::Value {
        todo!()
    }

    fn declare_cfn(&self, name: &str, fn_type: Self::Type) -> Self::Function {
        todo!()
    }

    fn declare_fn(&self, name: &str, fn_abi: &FnAbi<'tcx, Ty<'tcx>>) -> Self::Function {
        todo!()
    }

    fn define_global(&self, name: &str, ty: Self::Type) -> Option<Self::Value> {
        todo!()
    }

    fn define_private_global(&self, ty: Self::Type) -> Self::Value {
        todo!()
    }

    fn get_declared_value(&self, name: &str) -> Option<Self::Value> {
        todo!()
    }

    fn get_defined_value(&self, name: &str) -> Option<Self::Value> {
        todo!()
    }
}

impl<'spv, 'tcx> DebugInfoMethods<'tcx> for CodegenCx<'spv, 'tcx> {
    fn create_vtable_metadata(&self, ty: Ty<'tcx>, vtable: Self::Value) {
        todo!()
    }

    fn create_function_debug_context(
        &self,
        instance: Instance<'tcx>,
        fn_abi: &FnAbi<'tcx, Ty<'tcx>>,
        llfn: Self::Function,
        mir: &Body<'_>,
    ) -> Option<FunctionDebugContext<Self::DIScope>> {
        todo!()
    }

    fn extend_scope_to_file(
        &self,
        scope_metadata: Self::DIScope,
        file: &SourceFile,
        defining_crate: CrateNum,
    ) -> Self::DIScope {
        todo!()
    }

    fn debuginfo_finalize(&self) {
        todo!()
    }

    fn create_dbg_var(
        &self,
        dbg_context: &FunctionDebugContext<Self::DIScope>,
        variable_name: Symbol,
        variable_type: Ty<'tcx>,
        scope_metadata: Self::DIScope,
        variable_kind: VariableKind,
        span: Span,
    ) -> Self::DIVariable {
        todo!()
    }
}

impl<'spv, 'tcx> CoverageInfoMethods for CodegenCx<'spv, 'tcx> {
    fn coverageinfo_finalize(&self) {
        todo!()
    }
}

impl<'spv, 'tcx> ConstMethods<'tcx> for CodegenCx<'spv, 'tcx> {
    fn const_null(&self, t: Self::Type) -> Self::Value {
        todo!()
    }
    fn const_undef(&self, t: Self::Type) -> Self::Value {
        todo!()
    }
    fn const_int(&self, t: Self::Type, i: i64) -> Self::Value {
        todo!()
    }
    fn const_uint(&self, t: Self::Type, i: u64) -> Self::Value {
        todo!()
    }
    fn const_uint_big(&self, t: Self::Type, u: u128) -> Self::Value {
        todo!()
    }
    fn const_bool(&self, val: bool) -> Self::Value {
        todo!()
    }
    fn const_i32(&self, i: i32) -> Self::Value {
        todo!()
    }
    fn const_u32(&self, i: u32) -> Self::Value {
        todo!()
    }
    fn const_u64(&self, i: u64) -> Self::Value {
        todo!()
    }
    fn const_usize(&self, i: u64) -> Self::Value {
        todo!()
    }
    fn const_u8(&self, i: u8) -> Self::Value {
        todo!()
    }
    fn const_real(&self, t: Self::Type, val: f64) -> Self::Value {
        todo!()
    }

    fn const_str(&self, s: Symbol) -> (Self::Value, Self::Value) {
        todo!()
    }
    fn const_struct(&self, elts: &[Self::Value], packed: bool) -> Self::Value {
        todo!()
    }

    fn const_to_opt_uint(&self, v: Self::Value) -> Option<u64> {
        todo!()
    }
    fn const_to_opt_u128(&self, v: Self::Value, sign_ext: bool) -> Option<u128> {
        todo!()
    }

    fn scalar_to_backend(&self, cv: Scalar, layout: &abi::Scalar, llty: Self::Type) -> Self::Value {
        todo!()
    }
    fn from_const_alloc(
        &self,
        layout: TyAndLayout<'tcx>,
        alloc: &Allocation,
        offset: Size,
    ) -> PlaceRef<'tcx, Self::Value> {
        todo!()
    }

    fn const_ptrcast(&self, val: Self::Value, ty: Self::Type) -> Self::Value {
        todo!()
    }
}

impl<'spv, 'tcx> AsmMethods for CodegenCx<'spv, 'tcx> {
    fn codegen_global_asm(&self, ga: &GlobalAsm) {
        todo!()
    }
}
