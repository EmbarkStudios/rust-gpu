use crate::abi::ConvSpirvType;
use crate::builder::ExtInst;
use crate::builder_spirv::{BuilderCursor, BuilderSpirv, SpirvValue, SpirvValueExt};
use crate::poison_pass::poison_pass;
use crate::spirv_type::{SpirvType, SpirvTypePrinter, TypeCache};
use rspirv::dr::{Module, Operand};
use rspirv::spirv::{Decoration, FunctionControl, LinkageType, StorageClass, Word};
use rustc_codegen_ssa::common::TypeKind;
use rustc_codegen_ssa::mir::debuginfo::{FunctionDebugContext, VariableKind};
use rustc_codegen_ssa::mir::place::PlaceRef;
use rustc_codegen_ssa::traits::{
    AsmMethods, BackendTypes, BaseTypeMethods, ConstMethods, CoverageInfoMethods, DebugInfoMethods,
    DeclareMethods, LayoutTypeMethods, MiscMethods, PreDefineMethods, StaticMethods,
};
use rustc_data_structures::fx::FxHashMap;
use rustc_hir::GlobalAsm;
use rustc_middle::mir::interpret::{
    read_target_uint, Allocation, ConstValue, GlobalAlloc, Pointer,
};
use rustc_middle::mir::mono::{CodegenUnit, Linkage, MonoItem, Visibility};
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
    self, Abi, AddressSpace, Align, FieldsShape, HasDataLayout, LayoutOf, Primitive, Size,
    TargetDataLayout,
};
use rustc_target::spec::{HasTargetSpec, Target};
use std::cell::RefCell;
use std::collections::HashMap;
use std::collections::HashSet;
use std::iter::once;
use std::ops::Range;

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
    poisoned_values: RefCell<HashSet<Word>>,
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
            poisoned_values: Default::default(),
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

    pub fn lookup_type(&self, ty: Word) -> SpirvType {
        self.type_cache.lookup(ty)
    }

    pub fn get_static(&self, def_id: DefId) -> SpirvValue {
        let instance = Instance::mono(self.tcx, def_id);
        if let Some(&g) = self.instances.borrow().get(&instance) {
            return g;
        }

        let defined_in_current_codegen_unit = self
            .codegen_unit
            .items()
            .contains_key(&MonoItem::Static(def_id));
        assert!(
            !defined_in_current_codegen_unit,
            "get_static() should always hit the cache for statics defined in the same CGU, but did not for `{:?}`",
            def_id
        );

        let ty = instance.ty(self.tcx, ParamEnv::reveal_all());
        let sym = self.tcx.symbol_name(instance).name;
        let g = self.declare_global(sym, self.layout_of(ty).spirv_type(self));
        self.instances.borrow_mut().insert(instance, g);
        g
    }

    // Useful for printing out types when debugging
    pub fn debug_type<'cx>(&'cx self, ty: Word) -> SpirvTypePrinter<'cx, 'tcx> {
        self.lookup_type(ty).debug(ty, self)
    }

    pub fn poison(&self, word: Word) {
        self.poisoned_values.borrow_mut().insert(word);
    }

    pub fn finalize_module(self) -> Module {
        let mut result = self.builder.finalize();
        poison_pass(&mut result, &mut self.poisoned_values.borrow_mut());
        result
    }

    // Presumably these methods will get used eventually, so allow(dead_code) to not have to rewrite when needed.
    pub fn constant_u8(&self, val: u8) -> SpirvValue {
        let ty = SpirvType::Integer(8, false).def(self);
        self.builder
            .def_constant(ty, Operand::LiteralInt32(val as u32))
    }

    pub fn constant_u16(&self, val: u16) -> SpirvValue {
        let ty = SpirvType::Integer(16, false).def(self);
        self.builder
            .def_constant(ty, Operand::LiteralInt32(val as u32))
    }

    pub fn constant_i32(&self, val: i32) -> SpirvValue {
        let ty = SpirvType::Integer(32, true).def(self);
        self.builder
            .def_constant(ty, Operand::LiteralInt32(val as u32))
    }

    pub fn constant_u32(&self, val: u32) -> SpirvValue {
        let ty = SpirvType::Integer(32, false).def(self);
        self.builder.def_constant(ty, Operand::LiteralInt32(val))
    }

    pub fn constant_u64(&self, val: u64) -> SpirvValue {
        let ty = SpirvType::Integer(64, false).def(self);
        self.builder.def_constant(ty, Operand::LiteralInt64(val))
    }

    pub fn constant_int(&self, ty: Word, val: u64) -> SpirvValue {
        match self.lookup_type(ty) {
            SpirvType::Integer(8, false) => self
                .builder
                .def_constant(ty, Operand::LiteralInt32(val as u8 as u32)),
            SpirvType::Integer(16, false) => self
                .builder
                .def_constant(ty, Operand::LiteralInt32(val as u16 as u32)),
            SpirvType::Integer(32, false) => self
                .builder
                .def_constant(ty, Operand::LiteralInt32(val as u32)),
            SpirvType::Integer(64, false) => {
                self.builder.def_constant(ty, Operand::LiteralInt64(val))
            }
            SpirvType::Integer(8, true) => self
                .builder
                .def_constant(ty, Operand::LiteralInt32(val as i64 as i8 as u32)),
            SpirvType::Integer(16, true) => self
                .builder
                .def_constant(ty, Operand::LiteralInt32(val as i64 as i16 as u32)),
            SpirvType::Integer(32, true) => self
                .builder
                .def_constant(ty, Operand::LiteralInt32(val as i64 as i32 as u32)),
            SpirvType::Integer(64, true) => {
                self.builder.def_constant(ty, Operand::LiteralInt64(val))
            }
            SpirvType::Bool => match val {
                0 => self.emit_global().constant_false(ty),
                1 => self.emit_global().constant_true(ty),
                _ => panic!("Invalid constant value for bool: {}", val),
            }
            .with_type(ty),
            SpirvType::Integer(128, _) => {
                let result = self.emit_global().undef(ty, None);
                self.poison(result);
                result.with_type(ty)
            }
            other => panic!("constant_int invalid on type {}", other.debug(ty, self)),
        }
    }

    pub fn constant_f32(&self, val: f32) -> SpirvValue {
        let ty = SpirvType::Float(32).def(self);
        self.builder.def_constant(ty, Operand::LiteralFloat32(val))
    }

    pub fn constant_f64(&self, val: f64) -> SpirvValue {
        let ty = SpirvType::Float(64).def(self);
        self.builder.def_constant(ty, Operand::LiteralFloat64(val))
    }

    pub fn constant_float(&self, ty: Word, val: f64) -> SpirvValue {
        match self.lookup_type(ty) {
            SpirvType::Float(32) => self
                .builder
                .def_constant(ty, Operand::LiteralFloat32(val as f32)),
            SpirvType::Float(64) => self.builder.def_constant(ty, Operand::LiteralFloat64(val)),
            other => panic!("constant_float invalid on type {}", other.debug(ty, self)),
        }
    }

    #[allow(dead_code)]
    pub fn set_linkage_export(&self, target: Word, name: String) {
        self.emit_global().decorate(
            target,
            Decoration::LinkageAttributes,
            once(Operand::LiteralString(name))
                .chain(once(Operand::LinkageType(LinkageType::Export))),
        )
    }

    #[allow(dead_code)]
    pub fn set_linkage_import(&self, target: Word, name: String) {
        self.emit_global().decorate(
            target,
            Decoration::LinkageAttributes,
            once(Operand::LiteralString(name))
                .chain(once(Operand::LinkageType(LinkageType::Import))),
        )
    }

    pub fn align_of(&self, ty: Ty<'tcx>) -> Align {
        self.layout_of(ty).align.abi
    }

    pub fn size_of(&self, ty: Ty<'tcx>) -> Size {
        self.layout_of(ty).size
    }

    #[allow(dead_code)]
    pub fn size_and_align_of(&self, ty: Ty<'tcx>) -> (Size, Align) {
        let layout = self.layout_of(ty);
        (layout.size, layout.align.abi)
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

impl<'tcx> LayoutOf for CodegenCx<'tcx> {
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

impl<'tcx> LayoutTypeMethods<'tcx> for CodegenCx<'tcx> {
    fn backend_type(&self, layout: TyAndLayout<'tcx>) -> Self::Type {
        layout.spirv_type(self)
    }

    fn immediate_backend_type(&self, layout: TyAndLayout<'tcx>) -> Self::Type {
        layout.spirv_type_immediate(self)
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

    fn backend_field_index(&self, layout: TyAndLayout<'tcx>, index: usize) -> u64 {
        match layout.abi {
            Abi::Scalar(_) | Abi::ScalarPair(..) => {
                panic!("backend_field_index({:?}): not applicable", layout)
            }
            _ => {}
        }
        match layout.fields {
            FieldsShape::Primitive | FieldsShape::Union(_) => {
                panic!("backend_field_index({:?}): not applicable", layout)
            }
            FieldsShape::Array { .. } => index as u64,
            // note: codegen_llvm implements this as 1+index*2 due to padding fields
            FieldsShape::Arbitrary { .. } => layout.fields.memory_index(index) as u64,
        }
    }

    fn scalar_pair_element_backend_type(
        &self,
        layout: TyAndLayout<'tcx>,
        index: usize,
        immediate: bool,
    ) -> Self::Type {
        crate::abi::scalar_pair_element_backend_type(self, layout, index, immediate)
    }

    fn cast_backend_type(&self, ty: &CastTarget) -> Self::Type {
        ty.spirv_type(self)
    }

    fn fn_ptr_backend_type(&self, fn_abi: &FnAbi<'tcx, Ty<'tcx>>) -> Self::Type {
        fn_abi.spirv_type(self)
    }

    fn reg_backend_type(&self, ty: &Reg) -> Self::Type {
        ty.spirv_type(self)
    }
}

impl<'tcx> BaseTypeMethods<'tcx> for CodegenCx<'tcx> {
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
        let (field_offsets, size, align) = crate::abi::auto_struct_layout(self, &els);
        SpirvType::Adt {
            name: "<generated_struct>".to_string(),
            align,
            size,
            field_types: els.to_vec(),
            field_offsets,
            field_names: None,
        }
        .def(self)
    }
    fn type_kind(&self, ty: Self::Type) -> TypeKind {
        match self.lookup_type(ty) {
            SpirvType::Void => TypeKind::Void,
            SpirvType::Bool => TypeKind::Integer, // thanks llvm
            SpirvType::Integer(_, _) => TypeKind::Integer,
            SpirvType::Float(width) => match width {
                16 => TypeKind::Half,
                32 => TypeKind::Float,
                64 => TypeKind::Double,
                other => panic!("Invalid float width in type_kind: {}", other),
            },
            SpirvType::Adt { .. } => TypeKind::Struct,
            SpirvType::Opaque { .. } => TypeKind::Struct,
            SpirvType::Vector { .. } => TypeKind::Vector,
            SpirvType::Array { .. } => TypeKind::Array,
            SpirvType::RuntimeArray { .. } => TypeKind::Array,
            SpirvType::Pointer { .. } => TypeKind::Pointer,
            SpirvType::Function { .. } => TypeKind::Function,
        }
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

impl<'tcx> StaticMethods for CodegenCx<'tcx> {
    fn static_addr_of(&self, cv: Self::Value, _align: Align, _kind: Option<&str>) -> Self::Value {
        // TODO: Integrate this into define_static and whatnot?
        if let Some(already_defined) = self.builder.find_global_constant_variable(cv.def) {
            return already_defined;
        }
        let ty = SpirvType::Pointer {
            storage_class: StorageClass::Generic,
            pointee: cv.ty,
        }
        .def(self);
        self.emit_global()
            .variable(ty, None, StorageClass::Generic, Some(cv.def))
            .with_type(ty)
    }

    fn codegen_static(&self, def_id: DefId, _is_mutable: bool) {
        let g = self.get_static(def_id);

        let alloc = match self.tcx.const_eval_poly(def_id) {
            Ok(ConstValue::ByRef { alloc, offset }) if offset.bytes() == 0 => alloc,
            Ok(val) => panic!("static const eval returned {:#?}", val),
            // Error has already been reported
            Err(_) => return,
        };
        let value_ty = match self.lookup_type(g.ty) {
            SpirvType::Pointer { pointee, .. } => pointee,
            other => panic!("global had non-pointer type {}", other.debug(g.ty, self)),
        };
        let v = create_const_alloc(self, alloc, value_ty);

        if self.lookup_type(v.ty) == SpirvType::Bool {
            // convert bool -> i8
            todo!();
        }

        // let instance = Instance::mono(self.tcx, def_id);
        // let ty = instance.ty(self.tcx, ParamEnv::reveal_all());
        // let llty = self.layout_of(ty).llvm_type(self);

        assert_ty_eq!(self, value_ty, v.ty);
        self.builder.set_global_initializer(g.def, v.def);

        // if attrs.flags.contains(CodegenFnAttrFlags::USED) {
        //     self.add_used_global(g);
        // }
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

pub fn get_fn<'tcx>(cx: &CodegenCx<'tcx>, instance: Instance<'tcx>) -> SpirvValue {
    assert!(!instance.substs.needs_infer());
    assert!(!instance.substs.has_escaping_bound_vars());

    if let Some(&func) = cx.instances.borrow().get(&instance) {
        return func;
    }

    let sym = cx.tcx.symbol_name(instance).name;

    let fn_abi = FnAbi::of_instance(cx, instance, &[]);

    let llfn = if let Some(llfn) = cx.get_declared_value(&sym) {
        llfn
    } else {
        cx.declare_fn(&sym, &fn_abi)
    };

    cx.instances.borrow_mut().insert(instance, llfn);

    llfn
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
        get_fn(self, instance)
    }

    fn get_fn_addr(&self, instance: Instance<'tcx>) -> Self::Value {
        let function = get_fn(self, instance);
        self.static_addr_of(function, Align::from_bytes(0).unwrap(), None)
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

impl<'tcx> PreDefineMethods<'tcx> for CodegenCx<'tcx> {
    fn predefine_static(
        &self,
        def_id: DefId,
        _linkage: Linkage,
        _visibility: Visibility,
        symbol_name: &str,
    ) {
        let instance = Instance::mono(self.tcx, def_id);
        let ty = instance.ty(self.tcx, ParamEnv::reveal_all());
        let spvty = self.layout_of(ty).spirv_type(self);

        let g = self.define_global(symbol_name, spvty).unwrap_or_else(|| {
            self.sess().span_fatal(
                self.tcx.def_span(def_id),
                &format!("symbol `{}` is already defined", symbol_name),
            )
        });

        // unsafe {
        //     llvm::LLVMRustSetLinkage(g, base::linkage_to_llvm(linkage));
        //     llvm::LLVMRustSetVisibility(g, base::visibility_to_llvm(visibility));
        // }

        self.instances.borrow_mut().insert(instance, g);
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
        self.instances.borrow_mut().insert(instance, declared);
    }
}

impl<'tcx> DeclareMethods<'tcx> for CodegenCx<'tcx> {
    fn declare_global(&self, name: &str, ty: Self::Type) -> Self::Value {
        let ptr_ty = SpirvType::Pointer {
            storage_class: StorageClass::Generic,
            pointee: ty,
        }
        .def(self);
        let result = self
            .emit_global()
            .variable(ptr_ty, None, StorageClass::Generic, None)
            .with_type(ptr_ty);
        self.declared_values
            .borrow_mut()
            .insert(name.to_string(), result);
        result
    }

    fn declare_cfn(&self, _name: &str, _fn_type: Self::Type) -> Self::Function {
        todo!()
    }

    fn declare_fn(&self, name: &str, fn_abi: &FnAbi<'tcx, Ty<'tcx>>) -> Self::Function {
        let control = FunctionControl::NONE;
        let function_id = None;

        let function_type = fn_abi.spirv_type(self);
        let (return_type, argument_types) = match self.lookup_type(function_type) {
            SpirvType::Function {
                return_type,
                arguments,
            } => (return_type, arguments),
            other => panic!("fn_abi type {}", other.debug(function_type, self)),
        };

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

    fn define_global(&self, name: &str, ty: Self::Type) -> Option<Self::Value> {
        if self.get_defined_value(name).is_some() {
            None
        } else {
            Some(self.declare_global(name, ty))
        }
    }

    fn define_private_global(&self, _ty: Self::Type) -> Self::Value {
        todo!()
    }

    fn get_declared_value(&self, name: &str) -> Option<Self::Value> {
        self.declared_values.borrow().get(name).copied()
    }

    fn get_defined_value(&self, name: &str) -> Option<Self::Value> {
        self.get_declared_value(name)
        // self.get_declared_value(name).and_then(|val| {
        //     let declaration = unsafe { llvm::LLVMIsDeclaration(val) != 0 };
        //     if !declaration {
        //         Some(val)
        //     } else {
        //         None
        //     }
        // })
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

impl<'tcx> ConstMethods<'tcx> for CodegenCx<'tcx> {
    fn const_null(&self, t: Self::Type) -> Self::Value {
        self.emit_global().constant_null(t).with_type(t)
    }
    fn const_undef(&self, ty: Self::Type) -> Self::Value {
        self.emit_global().undef(ty, None).with_type(ty)
    }
    fn const_int(&self, t: Self::Type, i: i64) -> Self::Value {
        self.constant_int(t, i as u64)
    }
    fn const_uint(&self, t: Self::Type, i: u64) -> Self::Value {
        self.constant_int(t, i)
    }
    fn const_uint_big(&self, t: Self::Type, u: u128) -> Self::Value {
        self.constant_int(t, u as u64)
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
        self.constant_i32(i)
    }
    fn const_u32(&self, i: u32) -> Self::Value {
        self.constant_u32(i)
    }
    fn const_u64(&self, i: u64) -> Self::Value {
        self.constant_u64(i)
    }
    fn const_usize(&self, i: u64) -> Self::Value {
        let ptr_size = self.tcx.data_layout.pointer_size.bits() as u32;
        let t = SpirvType::Integer(ptr_size, false).def(self);
        self.constant_int(t, i)
    }
    fn const_u8(&self, i: u8) -> Self::Value {
        self.constant_u8(i)
    }
    fn const_real(&self, t: Self::Type, val: f64) -> Self::Value {
        self.constant_float(t, val)
    }

    fn const_str(&self, s: Symbol) -> (Self::Value, Self::Value) {
        let len = s.as_str().len();
        let raw_bytes = const_bytes(self, s.as_str().as_bytes());
        let ptr = self
            .builder
            .find_global_constant_variable(raw_bytes.def)
            .unwrap_or_else(|| {
                let ty = self.type_ptr_to(self.layout_of(self.tcx.types.str_).spirv_type(self));
                self.emit_global()
                    .variable(ty, None, StorageClass::Generic, Some(raw_bytes.def))
                    .with_type(ty)
            });
        // let cs = consts::ptrcast(
        //     self.const_cstr(s, false),
        //     self.type_ptr_to(self.layout_of(self.tcx.types.str_).llvm_type(self)),
        // );
        (ptr, self.const_usize(len as u64))
    }
    fn const_struct(&self, elts: &[Self::Value], _packed: bool) -> Self::Value {
        // Presumably this will get bitcasted to the right type?
        let field_types = elts.iter().map(|f| f.ty).collect::<Vec<_>>();
        let (field_offsets, size, align) = crate::abi::auto_struct_layout(self, &field_types);
        let struct_ty = SpirvType::Adt {
            name: "<const_struct>".to_string(),
            size,
            align,
            field_types,
            field_offsets,
            field_names: None,
        }
        .def(self);
        self.emit_global()
            .constant_composite(struct_ty, elts.iter().map(|f| f.def))
            .with_type(struct_ty)
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
                Primitive::Int(int_size, int_signedness) => {
                    assert_eq!(size as u64, int_size.size().bytes());
                    match self.lookup_type(ty) {
                        SpirvType::Integer(width, spirv_signedness) => {
                            assert_eq!(width as u64, int_size.size().bits());
                            assert_eq!(spirv_signedness, int_signedness);
                            self.constant_int(ty, data as u64)
                        }
                        SpirvType::Bool => match data {
                            0 => self.emit_global().constant_false(ty).with_type(ty),
                            1 => self.emit_global().constant_true(ty).with_type(ty),
                            _ => panic!("Invalid constant value for bool: {}", data),
                        },
                        other => panic!(
                            "scalar_to_backend Primitive::Int not supported on type {}",
                            other.debug(ty, self)
                        ),
                    }
                }
                Primitive::F32 => {
                    let res = self.constant_f32(f32::from_bits(data as u32));
                    assert_eq!(res.ty, ty);
                    res
                }
                Primitive::F64 => {
                    let res = self.constant_f64(f64::from_bits(data as u64));
                    assert_eq!(res.ty, ty);
                    res
                }
                Primitive::Pointer => {
                    panic!("TODO: scalar_to_backend Primitive::Ptr not implemented yet")
                }
            },
            Scalar::Ptr(ptr) => {
                let (base_addr, _base_addr_space) = match self.tcx.global_alloc(ptr.alloc_id) {
                    GlobalAlloc::Memory(alloc) => {
                        let pointee = match self.lookup_type(ty) {
                            SpirvType::Pointer { pointee, .. } => pointee,
                            other => panic!(
                                "GlobalAlloc::Memory type not implemented: {}",
                                other.debug(ty, self)
                            ),
                        };
                        let init = create_const_alloc(self, alloc, pointee);
                        let value = self.static_addr_of(init, alloc.align, None);
                        (value, AddressSpace::DATA)
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
        layout: TyAndLayout<'tcx>,
        alloc: &Allocation,
        offset: Size,
    ) -> PlaceRef<'tcx, Self::Value> {
        assert_eq!(offset, Size::ZERO);
        let ty = layout.spirv_type(self);
        let init = create_const_alloc(self, alloc, ty);
        let result = self.static_addr_of(init, alloc.align, None);
        PlaceRef::new_sized(result, layout)
    }

    fn const_ptrcast(&self, val: Self::Value, ty: Self::Type) -> Self::Value {
        // TODO: hack to get things working, this *will* fail spirv-val
        val.def.with_type(ty)
    }
}

fn create_const_alloc(cx: &CodegenCx<'_>, alloc: &Allocation, ty: Word) -> SpirvValue {
    // println!(
    //     "Creating const alloc of type {} with {} bytes",
    //     cx.debug_type(ty),
    //     alloc.len()
    // );
    let mut offset = Size::ZERO;
    let result = create_const_alloc2(cx, alloc, &mut offset, ty);
    assert_eq!(
        offset.bytes_usize(),
        alloc.len(),
        "create_const_alloc must consume all bytes of an Allocation"
    );
    // println!("Done creating alloc of type {}", cx.debug_type(ty));
    result
}

fn create_const_alloc2(
    cx: &CodegenCx<'_>,
    alloc: &Allocation,
    offset: &mut Size,
    ty: Word,
) -> SpirvValue {
    let ty_concrete = cx.lookup_type(ty);
    *offset = offset.align_to(ty_concrete.alignof(cx));
    // these print statements are really useful for debugging, so leave them easily available
    // println!("const at {}: {}", offset.bytes(), cx.debug_type(ty));
    match ty_concrete {
        SpirvType::Void => panic!("Cannot create const alloc of type void"),
        SpirvType::Bool => match read_alloc_val(cx, alloc, offset, 1) != 0 {
            true => cx.emit_global().constant_true(ty),
            false => cx.emit_global().constant_false(ty),
        }
        .with_type(ty),
        SpirvType::Integer(width, _) => {
            let v = read_alloc_val(cx, alloc, offset, (width / 8) as usize);
            cx.constant_int(ty, v as u64)
        }
        SpirvType::Float(width) => {
            let v = read_alloc_val(cx, alloc, offset, (width / 8) as usize);
            match width {
                32 => cx.constant_f32(f32::from_bits(v as u32)),
                64 => cx.constant_f64(f64::from_bits(v as u64)),
                other => panic!("invalid float width {}", other),
            }
        }
        SpirvType::Adt {
            size,
            field_types,
            field_offsets,
            ..
        } => {
            let base = *offset;
            let mut values = Vec::with_capacity(field_types.len());
            let mut occupied_spaces = Vec::with_capacity(field_types.len());
            for (&ty, &field_offset) in field_types.iter().zip(field_offsets.iter()) {
                let total_offset_start = base + field_offset;
                let mut total_offset_end = total_offset_start;
                values.push(create_const_alloc2(cx, alloc, &mut total_offset_end, ty).def);
                occupied_spaces.push(total_offset_start..total_offset_end);
            }
            if let Some(size) = size {
                *offset += size;
            } else {
                assert_eq!(
                    offset.bytes_usize(),
                    alloc.len(),
                    "create_const_alloc must consume all bytes of an Allocation after an unsized struct"
                );
            }
            assert_uninit(alloc, base, *offset, occupied_spaces);
            cx.emit_global()
                .constant_composite(ty, values)
                .with_type(ty)
        }
        SpirvType::Opaque { name } => panic!("Cannot create const alloc of type opaque: {}", name),
        SpirvType::Vector { element, count } | SpirvType::Array { element, count } => {
            let count = cx.builder.lookup_const_u64(count).unwrap() as usize;
            let values = (0..count)
                .map(|_| create_const_alloc2(cx, alloc, offset, element).def)
                .collect::<Vec<_>>();
            cx.emit_global()
                .constant_composite(ty, values)
                .with_type(ty)
        }
        SpirvType::RuntimeArray { element } => {
            let mut values = Vec::new();
            while offset.bytes_usize() != alloc.len() {
                values.push(create_const_alloc2(cx, alloc, offset, element).def);
            }
            cx.emit_global()
                .constant_composite(ty, values)
                .with_type(ty)
        }
        SpirvType::Pointer { .. } => {
            let ptr = read_alloc_ptr(cx, alloc, offset);
            cx.scalar_to_backend(
                ptr.into(),
                &abi::Scalar {
                    value: Primitive::Pointer,
                    valid_range: 0..=!0,
                },
                ty,
            )
        }
        SpirvType::Function { .. } => {
            panic!("TODO: SpirvType::Function not supported yet in create_const_alloc")
        }
    }
}

// Advances offset by len
fn read_alloc_val<'a>(
    cx: &CodegenCx<'_>,
    alloc: &'a Allocation,
    offset: &mut Size,
    len: usize,
) -> u128 {
    let off = offset.bytes_usize();
    let bytes = alloc.inspect_with_uninit_and_ptr_outside_interpreter(off..(off + len));
    // check relocations (pointer values)
    assert!({
        let start = off.saturating_sub(cx.data_layout().pointer_size.bytes_usize() - 1);
        let end = off + len;
        alloc
            .relocations()
            .range(Size::from_bytes(start)..Size::from_bytes(end))
            .is_empty()
    });
    // check init
    alloc
        .init_mask()
        .is_range_initialized(*offset, *offset + Size::from_bytes(len))
        .unwrap();
    *offset += Size::from_bytes(len);
    read_target_uint(cx.data_layout().endian, bytes).unwrap()
}

// Advances offset by ptr size
fn read_alloc_ptr<'a>(cx: &CodegenCx<'_>, alloc: &'a Allocation, offset: &mut Size) -> Pointer {
    let off = offset.bytes_usize();
    let len = cx.data_layout().pointer_size.bytes_usize();
    // check init
    alloc
        .init_mask()
        .is_range_initialized(*offset, *offset + Size::from_bytes(len))
        .unwrap();
    let bytes = alloc.inspect_with_uninit_and_ptr_outside_interpreter(off..(off + len));
    let inner_offset = read_target_uint(cx.data_layout().endian, bytes).unwrap();
    let &((), alloc_id) = alloc.relocations().get(&Size::from_bytes(off)).unwrap();
    *offset += Size::from_bytes(len);
    Pointer::new_with_tag(alloc_id, Size::from_bytes(inner_offset), ())
}

fn assert_uninit(alloc: &Allocation, start: Size, end: Size, occupied_ranges: Vec<Range<Size>>) {
    // Range<Size> doesn't impl Iterator, so manually do it.
    let mut index = start;
    while index < end {
        assert_eq!(
            occupied_ranges.iter().any(|range| range.contains(&index)),
            alloc.init_mask().get(index)
        );
        index += Size::from_bytes(1);
    }
}

fn const_bytes(cx: &CodegenCx<'_>, bytes: &[u8]) -> SpirvValue {
    let ty = SpirvType::Array {
        element: SpirvType::Integer(8, false).def(cx),
        count: cx.constant_u32(bytes.len() as u32).def,
    }
    .def(cx);
    let values = bytes
        .iter()
        .map(|&b| cx.constant_u8(b).def)
        .collect::<Vec<_>>();
    cx.emit_global()
        .constant_composite(ty, values)
        .with_type(ty)
}

impl<'tcx> AsmMethods for CodegenCx<'tcx> {
    fn codegen_global_asm(&self, _ga: &GlobalAsm) {
        todo!()
    }
}
