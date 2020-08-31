use crate::abi::{SpirvType, SpirvTypePrinter};
use crate::builder_spirv::{BuilderCursor, BuilderSpirv, ModuleSpirv, SpirvValue, SpirvValueExt};
use rspirv::spirv::{FunctionControl, StorageClass, Word};
use rustc_codegen_ssa::common::TypeKind;
use rustc_codegen_ssa::mir::debuginfo::{FunctionDebugContext, VariableKind};
use rustc_codegen_ssa::mir::place::PlaceRef;
use rustc_codegen_ssa::traits::{
    AsmMethods, BackendTypes, BaseTypeMethods, ConstMethods, CoverageInfoMethods, DebugInfoMethods,
    DeclareMethods, LayoutTypeMethods, MiscMethods, PreDefineMethods, StaticMethods,
};
use rustc_data_structures::fx::FxHashMap;
use rustc_hir::GlobalAsm;
use rustc_middle::mir::interpret::{Allocation, GlobalAlloc};
use rustc_middle::mir::mono::{CodegenUnit, Linkage, Visibility};
use rustc_middle::mir::Body;
use rustc_middle::ty::layout::{FnAbiExt, HasParamEnv, HasTyCtxt, LayoutError, TyAndLayout};
use rustc_middle::ty::{Instance, ParamEnv, PolyExistentialTraitRef, Ty, TyCtxt, TypeFoldable};
use rustc_mir::interpret::Scalar;
use rustc_session::Session;
use rustc_span::def_id::{CrateNum, DefId};
use rustc_span::source_map::{Span, DUMMY_SP};
use rustc_span::symbol::Symbol;
use rustc_span::SourceFile;
use rustc_target::abi::call::{CastTarget, FnAbi, Reg};
use rustc_target::abi::{
    self, Abi, AddressSpace, Align, HasDataLayout, LayoutOf, Primitive, Size, TargetDataLayout,
};
use rustc_target::spec::{HasTargetSpec, Target};
use std::cell::RefCell;
use std::collections::HashMap;

// TODO: How do you merge this macro with the one in builder::builder_methods::assert_ty_eq? idk how macros work.
macro_rules! assert_ty_eq {
    ($codegen_cx:expr, $left:expr, $right:expr) => {
        assert_eq!(
            $left,
            $right,
            "Expected types to be equal:\n{}\n==\n{}",
            $codegen_cx.debug_type($left),
            $codegen_cx.debug_type($right)
        )
    };
}

pub struct CodegenCx<'spv, 'tcx> {
    pub tcx: TyCtxt<'tcx>,
    pub codegen_unit: &'tcx CodegenUnit<'tcx>,
    /// Not actually used much, builder is probably what you want (see comment on ModuleSpirv type)
    pub spirv_module: &'spv ModuleSpirv,
    /// Spir-v module builder
    pub builder: BuilderSpirv,
    /// Used for the DeclareMethods API (not sure what it is yet)
    pub declared_values: RefCell<HashMap<String, SpirvValue>>,
    /// Map from MIR function to spir-v function ID
    pub function_defs: RefCell<HashMap<Instance<'tcx>, SpirvValue>>,
    /// Map from function ID to parameter list
    pub function_parameter_values: RefCell<HashMap<Word, Vec<SpirvValue>>>,
    /// Map from ID to structure
    pub type_defs: RefCell<HashMap<Word, SpirvType>>,
    /// Inverse of type_defs (used to cache generating types)
    pub type_cache: RefCell<HashMap<SpirvType, Word>>,
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
            declared_values: RefCell::new(HashMap::new()),
            function_defs: RefCell::new(HashMap::new()),
            function_parameter_values: RefCell::new(HashMap::new()),
            type_defs: RefCell::new(HashMap::new()),
            type_cache: RefCell::new(HashMap::new()),
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

    // returns (function_type, return_type, argument_types)
    pub fn trans_fnabi(&self, ty: &FnAbi<'tcx, Ty<'tcx>>) -> (Word, Word, Vec<Word>) {
        use crate::abi::trans_fnabi;
        trans_fnabi(self, ty)
    }

    pub fn trans_type(&self, ty: TyAndLayout<'tcx>) -> Word {
        use crate::abi::trans_type;
        trans_type(self, ty)
    }

    pub fn trans_type_immediate(&self, ty: TyAndLayout<'tcx>) -> Word {
        use crate::abi::trans_type_immediate;
        trans_type_immediate(self, ty)
    }

    pub fn lookup_type(&self, ty: Word) -> SpirvType {
        self.type_defs
            .borrow()
            .get(&ty)
            .expect("Tried to lookup value that wasn't a type, or has no definition")
            .clone()
    }

    pub fn get_static(&self, _def_id: DefId) -> SpirvValue {
        todo!()
    }

    // Useful for printing out types when debugging
    pub fn debug_type<'cx>(&'cx self, ty: Word) -> SpirvTypePrinter<'cx, 'spv, 'tcx> {
        self.lookup_type(ty).debug(self)
    }

    pub fn finalize_module(self) {
        let result = self.builder.finalize();
        let mut output = self.spirv_module.module.lock().unwrap();
        if output.is_some() {
            panic!("finalize_module was called twice");
        }
        *output = Some(result);
    }

    // Presumably these methods will get used eventually, so allow(dead_code) to not have to rewrite when needed.
    #[allow(dead_code)]
    pub fn constant_u8(&self, val: u32) -> SpirvValue {
        let ty = SpirvType::Integer(8, false).def(self);
        self.builder.constant_u32(ty, val).with_type(ty)
    }

    #[allow(dead_code)]
    pub fn constant_u16(&self, val: u32) -> SpirvValue {
        let ty = SpirvType::Integer(16, false).def(self);
        self.builder.constant_u32(ty, val).with_type(ty)
    }

    pub fn constant_u32(&self, val: u32) -> SpirvValue {
        let ty = SpirvType::Integer(32, false).def(self);
        self.builder.constant_u32(ty, val).with_type(ty)
    }

    pub fn constant_u64(&self, val: u64) -> SpirvValue {
        let ty = SpirvType::Integer(64, false).def(self);
        self.builder.constant_u64(ty, val).with_type(ty)
    }

    #[allow(dead_code)]
    pub fn constant_f32(&self, val: f32) -> SpirvValue {
        let ty = SpirvType::Float(32).def(self);
        self.builder.constant_f32(ty, val).with_type(ty)
    }

    #[allow(dead_code)]
    pub fn constant_f64(&self, val: f64) -> SpirvValue {
        let ty = SpirvType::Float(64).def(self);
        self.builder.constant_f64(ty, val).with_type(ty)
    }
}

impl<'spv, 'tcx> BackendTypes for CodegenCx<'spv, 'tcx> {
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
        self.trans_type(layout)
    }

    fn immediate_backend_type(&self, layout: TyAndLayout<'tcx>) -> Self::Type {
        self.trans_type_immediate(layout)
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

    fn backend_field_index(&self, _layout: TyAndLayout<'tcx>, index: usize) -> u64 {
        // TODO: Probably need to update this when enums are supported?
        index as u64
        // This is only used as a direct argument to struct_gep. codegen_llvm implements this as:
        // FieldsShape::Array { .. } => index as u64,
        // FieldsShape::Arbitrary { .. } => 1 + (layout.fields.memory_index(index) as u64) * 2,
        // (I'm guessing the *2 is due to llvm padding fields?)
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
    // TODO: llvm types are signless, as in neither signed nor unsigned (I think?), so these are expected to be
    // signless. Do we want a SpirvType::Integer(_, Signless) to indicate the sign is unknown, and to do conversions at
    // appropriate places?
    fn type_i1(&self) -> Self::Type {
        SpirvType::Bool.def(self)
    }
    fn type_i8(&self) -> Self::Type {
        SpirvType::Integer(8, false).def(self)
    }
    fn type_i16(&self) -> Self::Type {
        SpirvType::Integer(16, false).def(self)
    }
    fn type_i32(&self) -> Self::Type {
        SpirvType::Integer(32, false).def(self)
    }
    fn type_i64(&self) -> Self::Type {
        SpirvType::Integer(64, false).def(self)
    }
    fn type_i128(&self) -> Self::Type {
        SpirvType::Integer(128, false).def(self)
    }
    fn type_isize(&self) -> Self::Type {
        let ptr_size = self.tcx.data_layout.pointer_size.bits() as u32;
        SpirvType::Integer(ptr_size, false).def(self)
    }

    fn type_f32(&self) -> Self::Type {
        SpirvType::Float(32).def(self)
    }
    fn type_f64(&self) -> Self::Type {
        SpirvType::Float(64).def(self)
    }

    fn type_func(&self, args: &[Self::Type], ret: Self::Type) -> Self::Type {
        SpirvType::Function {
            return_type: ret,
            arguments: args.to_vec(),
        }
        .def(self)
    }
    fn type_struct(&self, els: &[Self::Type], _packed: bool) -> Self::Type {
        SpirvType::Adt {
            field_types: els.to_vec(),
            field_offsets: None,
        }
        .def(self)
    }
    fn type_kind(&self, _ty: Self::Type) -> TypeKind {
        todo!()
    }
    fn type_ptr_to(&self, ty: Self::Type) -> Self::Type {
        SpirvType::Pointer {
            storage_class: StorageClass::Generic,
            pointee: ty,
        }
        .def(self)
    }
    fn type_ptr_to_ext(&self, ty: Self::Type, address_space: AddressSpace) -> Self::Type {
        if address_space != AddressSpace::DATA {
            panic!("TODO: Unimplemented AddressSpace {:?}", address_space)
        }
        SpirvType::Pointer {
            storage_class: StorageClass::Generic,
            pointee: ty,
        }
        .def(self)
    }
    fn element_type(&self, ty: Self::Type) -> Self::Type {
        match self.lookup_type(ty) {
            SpirvType::Pointer {
                storage_class: _,
                pointee,
            } => pointee,
            SpirvType::Vector { element, .. } => element,
            spirv_type => panic!("element_type called on invalid type: {:?}", spirv_type),
        }
    }

    /// Returns the number of elements in `self` if it is a LLVM vector type.
    fn vector_length(&self, ty: Self::Type) -> usize {
        match self.lookup_type(ty) {
            SpirvType::Vector { count, .. } => count as usize,
            ty => panic!("vector_length called on non-vector type: {:?}", ty),
        }
    }

    fn float_width(&self, ty: Self::Type) -> usize {
        match self.lookup_type(ty) {
            SpirvType::Float(width) => width as usize,
            ty => panic!("float_width called on non-float type: {:?}", ty),
        }
    }

    /// Retrieves the bit width of the integer type `self`.
    fn int_width(&self, ty: Self::Type) -> u64 {
        match self.lookup_type(ty) {
            SpirvType::Integer(width, _) => width as u64,
            ty => panic!("int_width called on non-integer type: {:?}", ty),
        }
    }

    fn val_ty(&self, v: Self::Value) -> Self::Type {
        v.ty
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

pub fn get_fn<'spv, 'tcx>(cx: &CodegenCx<'spv, 'tcx>, instance: Instance<'tcx>) -> SpirvValue {
    assert!(!instance.substs.needs_infer());
    assert!(!instance.substs.has_escaping_bound_vars());

    if let Some(&func) = cx.function_defs.borrow().get(&instance) {
        return func;
    }

    let sym = cx.tcx.symbol_name(instance).name;

    let fn_abi = FnAbi::of_instance(cx, instance, &[]);

    let llfn = if let Some(llfn) = cx.get_declared_value(&sym) {
        llfn
    } else {
        cx.declare_fn(&sym, &fn_abi)
    };

    cx.function_defs.borrow_mut().insert(instance, llfn);

    llfn
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
        get_fn(self, instance)
    }

    fn get_fn_addr(&self, instance: Instance<'tcx>) -> Self::Value {
        get_fn(self, instance)
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
        // TODO: Implement statics
    }

    fn predefine_fn(
        &self,
        instance: Instance<'tcx>,
        _linkage: Linkage,
        _visibility: Visibility,
        symbol_name: &str,
    ) {
        let fn_abi = FnAbi::of_instance(self, instance, &[]);
        let declared = self.declare_fn(symbol_name, &fn_abi);
        self.function_defs.borrow_mut().insert(instance, declared);
    }
}

impl<'spv, 'tcx> DeclareMethods<'tcx> for CodegenCx<'spv, 'tcx> {
    fn declare_global(&self, _name: &str, _ty: Self::Type) -> Self::Value {
        todo!()
    }

    fn declare_cfn(&self, _name: &str, _fn_type: Self::Type) -> Self::Function {
        todo!()
    }

    fn declare_fn(&self, name: &str, fn_abi: &FnAbi<'tcx, Ty<'tcx>>) -> Self::Function {
        let control = FunctionControl::NONE;
        let function_id = None;

        let (function_type, return_type, argument_types) = self.trans_fnabi(fn_abi);

        let mut emit = self.emit_global();
        let fn_id = emit
            .begin_function(return_type, function_id, control, function_type)
            .unwrap();
        let parameter_values = argument_types
            .iter()
            .map(|&ty| emit.function_parameter(ty).unwrap().with_type(ty))
            .collect::<Vec<_>>();
        emit.end_function().unwrap();
        emit.name(fn_id, name);

        self.function_parameter_values
            .borrow_mut()
            .insert(fn_id, parameter_values);
        let result = fn_id.with_type(function_type);
        self.declared_values
            .borrow_mut()
            .insert(name.to_string(), result);
        result
    }

    fn define_global(&self, _name: &str, _ty: Self::Type) -> Option<Self::Value> {
        todo!()
    }

    fn define_private_global(&self, _ty: Self::Type) -> Self::Value {
        todo!()
    }

    fn get_declared_value(&self, name: &str) -> Option<Self::Value> {
        self.declared_values.borrow().get(name).copied()
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
    fn const_null(&self, t: Self::Type) -> Self::Value {
        self.emit_global().constant_null(t).with_type(t)
    }
    fn const_undef(&self, ty: Self::Type) -> Self::Value {
        self.emit_global().undef(ty, None).with_type(ty)
    }
    fn const_int(&self, t: Self::Type, i: i64) -> Self::Value {
        match self.lookup_type(t) {
            SpirvType::Integer(width, _signedness) => {
                if width > 32 {
                    self.builder.constant_u64(t, i as u64).with_type(t)
                } else {
                    self.builder.constant_u32(t, i as u32).with_type(t)
                }
            }
            other => panic!(
                "const_int not implemented for type: {:#?}",
                other.debug(self)
            ),
        }
    }
    fn const_uint(&self, t: Self::Type, i: u64) -> Self::Value {
        match self.lookup_type(t) {
            SpirvType::Integer(width, _signedness) => {
                if width > 32 {
                    self.builder.constant_u64(t, i as u64).with_type(t)
                } else {
                    self.builder.constant_u32(t, i as u32).with_type(t)
                }
            }
            other => panic!(
                "const_uint not implemented for type: {:#?}",
                other.debug(self)
            ),
        }
    }
    fn const_uint_big(&self, t: Self::Type, u: u128) -> Self::Value {
        if u > u64::MAX as u128 {
            panic!("u128 literals not supported yet: {}", u);
        }
        match self.lookup_type(t) {
            SpirvType::Integer(width, _) => {
                if width > 32 {
                    self.builder.constant_u64(t, u as u64).with_type(t)
                } else {
                    assert!(u <= u32::MAX as u128);
                    self.builder.constant_u32(t, u as u32).with_type(t)
                }
            }
            other => panic!("const_uint_big invalid on type {}", other.debug(self)),
        }
    }
    fn const_bool(&self, val: bool) -> Self::Value {
        let bool = SpirvType::Bool.def(self);
        if val {
            self.emit_global().constant_true(bool)
        } else {
            self.emit_global().constant_false(bool)
        }
        .with_type(bool)
    }
    fn const_i32(&self, i: i32) -> Self::Value {
        let t = SpirvType::Integer(32, true).def(self);
        self.builder.constant_u32(t, i as u32).with_type(t)
    }
    fn const_u32(&self, i: u32) -> Self::Value {
        let t = SpirvType::Integer(32, false).def(self);
        self.builder.constant_u32(t, i).with_type(t)
    }
    fn const_u64(&self, i: u64) -> Self::Value {
        let t = SpirvType::Integer(64, false).def(self);
        self.builder.constant_u64(t, i).with_type(t)
    }
    fn const_usize(&self, i: u64) -> Self::Value {
        let ptr_size = self.tcx.data_layout.pointer_size.bits() as u32;
        let t = SpirvType::Integer(ptr_size, false).def(self);
        if ptr_size > 32 {
            self.builder.constant_u64(t, i as u64)
        } else {
            self.builder.constant_u32(t, i as u32)
        }
        .with_type(t)
    }
    fn const_u8(&self, i: u8) -> Self::Value {
        let t = SpirvType::Integer(8, false).def(self);
        self.builder.constant_u32(t, i as u32).with_type(t)
    }
    fn const_real(&self, t: Self::Type, val: f64) -> Self::Value {
        match self.lookup_type(t) {
            SpirvType::Float(width) => {
                if width > 32 {
                    self.builder.constant_f64(t, val).with_type(t)
                } else {
                    self.builder.constant_f32(t, val as f32).with_type(t)
                }
            }
            other => panic!(
                "const_real not implemented for type: {:#?}",
                other.debug(self)
            ),
        }
    }

    fn const_str(&self, _s: Symbol) -> (Self::Value, Self::Value) {
        todo!()
    }
    fn const_struct(&self, _elts: &[Self::Value], _packed: bool) -> Self::Value {
        todo!()
    }

    fn const_to_opt_uint(&self, v: Self::Value) -> Option<u64> {
        self.builder.lookup_const_u64(v.def).ok()
    }
    fn const_to_opt_u128(&self, v: Self::Value, sign_ext: bool) -> Option<u128> {
        self.builder.lookup_const_u64(v.def).ok().map(|v| {
            if sign_ext {
                v as i64 as i128 as u128
            } else {
                v as u128
            }
        })
    }

    fn scalar_to_backend(
        &self,
        scalar: Scalar,
        layout: &abi::Scalar,
        ty: Self::Type,
    ) -> Self::Value {
        match scalar {
            Scalar::Raw { data, size } => match layout.value {
                Primitive::Int(_size, _signedness) => match size {
                    1 => self.builder.constant_u32(ty, data as u32).with_type(ty),
                    2 => self.builder.constant_u32(ty, data as u32).with_type(ty),
                    4 => self.builder.constant_u32(ty, data as u32).with_type(ty),
                    8 => self.builder.constant_u64(ty, data as u64).with_type(ty),
                    size => panic!(
                        "TODO: scalar_to_backend int size {} not implemented yet",
                        size
                    ),
                },
                Primitive::F32 => self
                    .builder
                    .constant_f32(ty, f32::from_bits(data as u32))
                    .with_type(ty),
                Primitive::F64 => self
                    .builder
                    .constant_f64(ty, f64::from_bits(data as u64))
                    .with_type(ty),
                Primitive::Pointer => {
                    panic!("TODO: scalar_to_backend Primitive::Ptr not implemented yet")
                }
            },
            Scalar::Ptr(ptr) => {
                let (base_addr, _base_addr_space) = match self.tcx.global_alloc(ptr.alloc_id) {
                    GlobalAlloc::Memory(_alloc) => {
                        panic!("TODO: scalar_to_backend GlobalAlloc::Memory not supported yet")
                        // let init = const_alloc_to_llvm(self, alloc);
                        // let value = self.static_addr_of(init, alloc.align, None);
                        // (value, AddressSpace::DATA)
                    }
                    GlobalAlloc::Function(fn_instance) => (
                        self.get_fn_addr(fn_instance.polymorphize(self.tcx)),
                        self.data_layout().instruction_address_space,
                    ),
                    GlobalAlloc::Static(def_id) => {
                        assert!(self.tcx.is_static(def_id));
                        assert!(!self.tcx.is_thread_local_static(def_id));
                        (self.get_static(def_id), AddressSpace::DATA)
                    }
                };
                let value = if ptr.offset.bytes() == 0 {
                    base_addr
                } else {
                    panic!("Non-constant scalar_to_backend ptr.offset not supported")
                    // let offset = self.constant_u64(ptr.offset.bytes());
                    // self.gep(base_addr, once(offset))
                };
                if layout.value != Primitive::Pointer {
                    panic!("Non-pointer-typed scalar_to_backend Scalar::Ptr not supported");
                // unsafe { llvm::LLVMConstPtrToInt(llval, llty) }
                } else {
                    assert_ty_eq!(self, value.ty, ty);
                    value
                }
            }
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
