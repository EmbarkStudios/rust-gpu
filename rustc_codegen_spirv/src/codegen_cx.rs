use crate::abi::trans_type;
use crate::things::{BuilderCursor, BuilderSpirv, ModuleSpirv};
use rspirv::spirv::{FunctionControl, Word};
use rustc_codegen_ssa::common::TypeKind;
use rustc_codegen_ssa::mir::debuginfo::{FunctionDebugContext, VariableKind};
use rustc_codegen_ssa::mir::place::PlaceRef;
use rustc_codegen_ssa::traits::{
    AsmMethods, BackendTypes, BaseTypeMethods, ConstMethods, CoverageInfoMethods, DebugInfoMethods,
    DeclareMethods, LayoutTypeMethods, MiscMethods, PreDefineMethods, StaticMethods,
};
use rustc_data_structures::fx::FxHashMap;
use rustc_hir::GlobalAsm;
use rustc_middle::mir::interpret::Allocation;
use rustc_middle::mir::mono::{CodegenUnit, Linkage, Visibility};
use rustc_middle::mir::Body;
use rustc_middle::ty::layout::{FnAbiExt, HasParamEnv, HasTyCtxt, LayoutError, TyAndLayout};
use rustc_middle::ty::{Instance, PolyExistentialTraitRef};
use rustc_middle::ty::{ParamEnv, Ty, TyCtxt};
use rustc_mir::interpret::Scalar;
use rustc_session::Session;
use rustc_span::def_id::{CrateNum, DefId};
use rustc_span::source_map::{Span, DUMMY_SP};
use rustc_span::symbol::Symbol;
use rustc_span::SourceFile;
use rustc_target::abi;
use rustc_target::abi::call::{CastTarget, FnAbi, Reg};
use rustc_target::abi::{
    Abi, AddressSpace, Align, HasDataLayout, LayoutOf, Primitive, Size, TargetDataLayout,
};
use rustc_target::spec::{HasTargetSpec, Target};
use std::cell::RefCell;
use std::collections::HashMap;

pub struct CodegenCx<'spv, 'tcx> {
    pub tcx: TyCtxt<'tcx>,
    pub codegen_unit: &'tcx CodegenUnit<'tcx>,
    pub spirv_module: &'spv ModuleSpirv,
    pub builder: BuilderSpirv,
    pub function_defs: RefCell<HashMap<Instance<'tcx>, Word>>,
    pub function_parameter_values: RefCell<HashMap<Word, Vec<Word>>>,
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
            builder: BuilderSpirv::new(),
            function_defs: RefCell::new(HashMap::new()),
            function_parameter_values: RefCell::new(HashMap::new()),
        }
    }

    pub fn emit_global(&self) -> std::cell::RefMut<rspirv::dr::Builder> {
        self.builder.builder(BuilderCursor {
            function: None,
            block: None,
        })
    }

    pub fn emit_with_cursor(
        &self,
        cursor: BuilderCursor,
    ) -> std::cell::RefMut<rspirv::dr::Builder> {
        self.builder.builder(cursor)
    }

    pub fn finalize_module(self) {
        let result = self.builder.finalize();
        let mut output = self.spirv_module.module.lock().unwrap();
        if output.is_some() {
            panic!("finalize_module was called twice");
        }
        *output = Some(result);
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
        panic!("TODO: Unknown backend_type: {:?}", layout);
    }

    fn immediate_backend_type(&self, layout: TyAndLayout<'tcx>) -> Self::Type {
        trans_type(self.tcx, &mut self.emit_global(), layout)
    }

    fn is_backend_immediate(&self, layout: TyAndLayout<'tcx>) -> bool {
        match layout.abi {
            Abi::Scalar(_) | Abi::Vector { .. } => true,
            Abi::ScalarPair(..) => false,
            Abi::Uninhabited | Abi::Aggregate { .. } => layout.is_zst(),
        }
    }

    fn is_backend_scalar_pair(&self, layout: TyAndLayout<'tcx>) -> bool {
        match layout.abi {
            Abi::ScalarPair(..) => true,
            Abi::Uninhabited | Abi::Scalar(_) | Abi::Vector { .. } | Abi::Aggregate { .. } => false,
        }
    }

    fn backend_field_index(&self, _layout: TyAndLayout<'tcx>, _index: usize) -> u64 {
        todo!()
    }

    fn scalar_pair_element_backend_type(
        &self,
        _layout: TyAndLayout<'tcx>,
        _index: usize,
        _immediate: bool,
    ) -> Self::Type {
        todo!()
    }

    fn cast_backend_type(&self, _ty: &CastTarget) -> Self::Type {
        todo!()
    }

    fn fn_ptr_backend_type(&self, _fn_abi: &FnAbi<'tcx, Ty<'tcx>>) -> Self::Type {
        todo!()
    }

    fn reg_backend_type(&self, _ty: &Reg) -> Self::Type {
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

    fn type_func(&self, _args: &[Self::Type], __ret: Self::Type) -> Self::Type {
        todo!()
    }
    fn type_struct(&self, _els: &[Self::Type], _packed: bool) -> Self::Type {
        todo!()
    }
    fn type_kind(&self, _ty: Self::Type) -> TypeKind {
        todo!()
    }
    fn type_ptr_to(&self, _ty: Self::Type) -> Self::Type {
        todo!()
    }
    fn type_ptr_to_ext(&self, _ty: Self::Type, _address_space: AddressSpace) -> Self::Type {
        todo!()
    }
    fn element_type(&self, _ty: Self::Type) -> Self::Type {
        todo!()
    }

    /// Returns the number of elements in `self` if it is a LLVM vector type.
    fn vector_length(&self, _ty: Self::Type) -> usize {
        todo!()
    }

    fn float_width(&self, _ty: Self::Type) -> usize {
        todo!()
    }

    /// Retrieves the bit width of the integer type `self`.
    fn int_width(&self, _ty: Self::Type) -> u64 {
        todo!()
    }

    fn val_ty(&self, _v: Self::Value) -> Self::Type {
        todo!()
    }
}

impl<'spv, 'tcx> StaticMethods for CodegenCx<'spv, 'tcx> {
    fn static_addr_of(&self, _cv: Self::Value, _align: Align, _kind: Option<&str>) -> Self::Value {
        todo!()
    }
    fn codegen_static(&self, _def_id: DefId, _is_mutable: bool) {
        todo!()
    }

    /// Mark the given global value as "used", to prevent a backend from potentially removing a
    /// static variable that may otherwise appear unused.
    ///
    /// Static variables in Rust can be annotated with the `#[used]` attribute to direct the `rustc`
    /// compiler to mark the variable as a "used global".
    fn add_used_global(&self, _global: Self::Value) {
        todo!()
    }
}

impl<'spv, 'tcx> MiscMethods<'tcx> for CodegenCx<'spv, 'tcx> {
    #[allow(clippy::type_complexity)]
    fn vtables(
        &self,
    ) -> &RefCell<FxHashMap<(Ty<'tcx>, Option<PolyExistentialTraitRef<'tcx>>), Self::Value>> {
        todo!()
    }

    fn check_overflow(&self) -> bool {
        todo!()
    }

    fn get_fn(&self, instance: Instance<'tcx>) -> Self::Function {
        self.function_defs.borrow()[&instance]
    }

    fn get_fn_addr(&self, _instance: Instance<'tcx>) -> Self::Value {
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

impl<'spv, 'tcx> PreDefineMethods<'tcx> for CodegenCx<'spv, 'tcx> {
    fn predefine_static(
        &self,
        _def_id: DefId,
        _linkage: Linkage,
        _visibility: Visibility,
        _symbol_name: &str,
    ) {
        todo!()
    }

    fn predefine_fn(
        &self,
        instance: Instance<'tcx>,
        _linkage: Linkage,
        _visibility: Visibility,
        _symbol_name: &str,
    ) {
        let mut emit = self.emit_global();
        let fn_abi = FnAbi::of_instance(self, instance, &[]);
        let argument_types = fn_abi
            .args
            .iter()
            .map(|arg| trans_type(self.tcx, &mut emit, arg.layout))
            .collect::<Vec<_>>();
        let return_type = trans_type(self.tcx, &mut emit, fn_abi.ret.layout);
        let control = FunctionControl::NONE;
        let function_id = None;
        let function_type = emit.type_function(return_type, &argument_types);

        let fn_id = emit
            .begin_function(return_type, function_id, control, function_type)
            .unwrap();
        let parameter_values = argument_types
            .iter()
            .map(|&ty| emit.function_parameter(ty).unwrap())
            .collect::<Vec<_>>();
        emit.end_function().unwrap();

        self.function_defs.borrow_mut().insert(instance, fn_id);
        self.function_parameter_values
            .borrow_mut()
            .insert(fn_id, parameter_values);
    }
}

impl<'spv, 'tcx> DeclareMethods<'tcx> for CodegenCx<'spv, 'tcx> {
    fn declare_global(&self, _name: &str, _ty: Self::Type) -> Self::Value {
        todo!()
    }

    fn declare_cfn(&self, _name: &str, _fn_type: Self::Type) -> Self::Function {
        todo!()
    }

    fn declare_fn(&self, _name: &str, _fn_abi: &FnAbi<'tcx, Ty<'tcx>>) -> Self::Function {
        todo!()
    }

    fn define_global(&self, _name: &str, _ty: Self::Type) -> Option<Self::Value> {
        todo!()
    }

    fn define_private_global(&self, _ty: Self::Type) -> Self::Value {
        todo!()
    }

    fn get_declared_value(&self, _name: &str) -> Option<Self::Value> {
        todo!()
    }

    fn get_defined_value(&self, _name: &str) -> Option<Self::Value> {
        todo!()
    }
}

impl<'spv, 'tcx> DebugInfoMethods<'tcx> for CodegenCx<'spv, 'tcx> {
    fn create_vtable_metadata(&self, _ty: Ty<'tcx>, _vtable: Self::Value) {
        todo!()
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

impl<'spv, 'tcx> CoverageInfoMethods for CodegenCx<'spv, 'tcx> {
    fn coverageinfo_finalize(&self) {
        todo!()
    }
}

impl<'spv, 'tcx> ConstMethods<'tcx> for CodegenCx<'spv, 'tcx> {
    fn const_null(&self, _t: Self::Type) -> Self::Value {
        todo!()
    }
    fn const_undef(&self, ty: Self::Type) -> Self::Value {
        self.emit_global().undef(ty, None)
    }
    fn const_int(&self, _t: Self::Type, _i: i64) -> Self::Value {
        todo!()
    }
    fn const_uint(&self, _t: Self::Type, _i: u64) -> Self::Value {
        todo!()
    }
    fn const_uint_big(&self, _t: Self::Type, _u: u128) -> Self::Value {
        todo!()
    }
    fn const_bool(&self, _val: bool) -> Self::Value {
        todo!()
    }
    fn const_i32(&self, _i: i32) -> Self::Value {
        todo!()
    }
    fn const_u32(&self, _i: u32) -> Self::Value {
        todo!()
    }
    fn const_u64(&self, _i: u64) -> Self::Value {
        todo!()
    }
    fn const_usize(&self, _i: u64) -> Self::Value {
        todo!()
    }
    fn const_u8(&self, _i: u8) -> Self::Value {
        todo!()
    }
    fn const_real(&self, _t: Self::Type, _val: f64) -> Self::Value {
        todo!()
    }

    fn const_str(&self, _s: Symbol) -> (Self::Value, Self::Value) {
        todo!()
    }
    fn const_struct(&self, _elts: &[Self::Value], _packed: bool) -> Self::Value {
        todo!()
    }

    fn const_to_opt_uint(&self, _v: Self::Value) -> Option<u64> {
        todo!()
    }
    fn const_to_opt_u128(&self, _v: Self::Value, _sign_ext: bool) -> Option<u128> {
        todo!()
    }

    fn scalar_to_backend(
        &self,
        scalar: Scalar,
        layout: &abi::Scalar,
        ty: Self::Type,
    ) -> Self::Value {
        // TODO: Is it better to go through trans_type here?
        match scalar {
            Scalar::Raw { data, size } => match layout.value {
                Primitive::Int(_size, _signedness) => match size {
                    4 => self.emit_global().constant_u32(ty, data as u32),
                    8 => self.emit_global().constant_u64(ty, data as u64),
                    size => panic!(
                        "TODO: scalar_to_backend int size {} not implemented yet",
                        size
                    ),
                },
                Primitive::F32 => self
                    .emit_global()
                    .constant_f32(ty, f32::from_bits(data as u32)),
                Primitive::F64 => self
                    .emit_global()
                    .constant_f64(ty, f64::from_bits(data as u64)),
                Primitive::Pointer => {
                    panic!("TODO: scalar_to_backend Primitive::Ptr not implemented yet")
                }
            },
            Scalar::Ptr(ptr) => panic!(
                "TODO: scalar_to_backend Scalar::Ptr not implemented yet: {:?}",
                ptr
            ),
        }
    }
    fn from_const_alloc(
        &self,
        _layout: TyAndLayout<'tcx>,
        _alloc: &Allocation,
        _offset: Size,
    ) -> PlaceRef<'tcx, Self::Value> {
        todo!()
    }

    fn const_ptrcast(&self, _val: Self::Value, _ty: Self::Type) -> Self::Value {
        todo!()
    }
}

impl<'spv, 'tcx> AsmMethods for CodegenCx<'spv, 'tcx> {
    fn codegen_global_asm(&self, _ga: &GlobalAsm) {
        todo!()
    }
}
