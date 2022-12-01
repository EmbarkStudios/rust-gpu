use super::CodegenCx;
use crate::abi::ConvSpirvType;
use crate::spirv_type::SpirvType;
use rspirv::spirv::Word;
use rustc_codegen_ssa::common::TypeKind;
use rustc_codegen_ssa::traits::{BaseTypeMethods, LayoutTypeMethods};
use rustc_middle::ty::layout::{
    FnAbiError, FnAbiOfHelpers, FnAbiRequest, LayoutError, LayoutOfHelpers, TyAndLayout,
};
use rustc_middle::ty::Ty;
use rustc_middle::{bug, span_bug};
use rustc_span::source_map::{Span, DUMMY_SP};
use rustc_target::abi::call::{CastTarget, FnAbi, Reg};
use rustc_target::abi::{Abi, AddressSpace, FieldsShape};

impl<'tcx> LayoutOfHelpers<'tcx> for CodegenCx<'tcx> {
    type LayoutOfResult = TyAndLayout<'tcx>;

    #[inline]
    fn handle_layout_err(&self, err: LayoutError<'tcx>, span: Span, ty: Ty<'tcx>) -> ! {
        if let LayoutError::SizeOverflow(_) = err {
            self.tcx.sess.span_fatal(span, err.to_string())
        } else {
            span_bug!(span, "failed to get layout for `{}`: {}", ty, err)
        }
    }
}

impl<'tcx> FnAbiOfHelpers<'tcx> for CodegenCx<'tcx> {
    type FnAbiOfResult = &'tcx FnAbi<'tcx, Ty<'tcx>>;

    #[inline]
    fn handle_fn_abi_err(
        &self,
        err: FnAbiError<'tcx>,
        span: Span,
        fn_abi_request: FnAbiRequest<'tcx>,
    ) -> ! {
        if let FnAbiError::Layout(LayoutError::SizeOverflow(_)) = err {
            self.tcx.sess.span_fatal(span, err.to_string())
        } else {
            match fn_abi_request {
                FnAbiRequest::OfFnPtr { sig, extra_args } => {
                    span_bug!(
                        span,
                        "`fn_abi_of_fn_ptr({}, {:?})` failed: {}",
                        sig,
                        extra_args,
                        err
                    );
                }
                FnAbiRequest::OfInstance {
                    instance,
                    extra_args,
                } => {
                    span_bug!(
                        span,
                        "`fn_abi_of_instance({}, {:?})` failed: {}",
                        instance,
                        extra_args,
                        err
                    );
                }
            }
        }
    }
}

impl<'tcx> LayoutTypeMethods<'tcx> for CodegenCx<'tcx> {
    fn backend_type(&self, layout: TyAndLayout<'tcx>) -> Self::Type {
        layout.spirv_type(DUMMY_SP, self)
    }

    fn cast_backend_type(&self, ty: &CastTarget) -> Self::Type {
        bug!(
            "cast_backend_type({:?}): query hooks should've made `PassMode::Cast` impossible",
            ty
        )
    }

    fn fn_decl_backend_type(&self, fn_abi: &FnAbi<'tcx, Ty<'tcx>>) -> Self::Type {
        fn_abi.spirv_type(DUMMY_SP, self)
    }

    fn fn_ptr_backend_type(&self, fn_abi: &FnAbi<'tcx, Ty<'tcx>>) -> Self::Type {
        self.type_ptr_to(self.fn_decl_backend_type(fn_abi))
    }

    fn reg_backend_type(&self, ty: &Reg) -> Self::Type {
        bug!(
            "reg_backend_type({:?}): query hooks should've made `PassMode::Cast` impossible",
            ty
        )
    }

    fn immediate_backend_type(&self, layout: TyAndLayout<'tcx>) -> Self::Type {
        layout.spirv_type(DUMMY_SP, self)
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
                bug!("backend_field_index({:?}): not applicable", layout);
            }
            _ => {}
        }
        match layout.fields {
            FieldsShape::Primitive | FieldsShape::Union(_) => {
                bug!("backend_field_index({:?}): not applicable", layout)
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
        _immediate: bool,
    ) -> Self::Type {
        crate::abi::scalar_pair_element_backend_type(self, DUMMY_SP, layout, index)
    }
}

impl<'tcx> CodegenCx<'tcx> {
    pub fn type_usize(&self) -> Word {
        let ptr_size = self.tcx.data_layout.pointer_size.bits() as u32;
        SpirvType::Integer(ptr_size, false).def(DUMMY_SP, self)
    }
}

impl<'tcx> BaseTypeMethods<'tcx> for CodegenCx<'tcx> {
    fn type_i1(&self) -> Self::Type {
        SpirvType::Bool.def(DUMMY_SP, self)
    }
    fn type_i8(&self) -> Self::Type {
        SpirvType::Integer(8, false).def(DUMMY_SP, self)
    }
    fn type_i16(&self) -> Self::Type {
        SpirvType::Integer(16, false).def(DUMMY_SP, self)
    }
    fn type_i32(&self) -> Self::Type {
        SpirvType::Integer(32, false).def(DUMMY_SP, self)
    }
    fn type_i64(&self) -> Self::Type {
        SpirvType::Integer(64, false).def(DUMMY_SP, self)
    }
    fn type_i128(&self) -> Self::Type {
        SpirvType::Integer(128, false).def(DUMMY_SP, self)
    }
    fn type_isize(&self) -> Self::Type {
        let ptr_size = self.tcx.data_layout.pointer_size.bits() as u32;
        SpirvType::Integer(ptr_size, false).def(DUMMY_SP, self)
    }

    fn type_f32(&self) -> Self::Type {
        SpirvType::Float(32).def(DUMMY_SP, self)
    }
    fn type_f64(&self) -> Self::Type {
        SpirvType::Float(64).def(DUMMY_SP, self)
    }

    fn type_func(&self, args: &[Self::Type], ret: Self::Type) -> Self::Type {
        SpirvType::Function {
            return_type: ret,
            arguments: args,
        }
        .def(DUMMY_SP, self)
    }
    fn type_struct(&self, els: &[Self::Type], _packed: bool) -> Self::Type {
        // FIXME(eddyb) use `AccumulateVec`s just like `rustc` itself does.
        let (field_offsets, size, align) = crate::abi::auto_struct_layout(self, els);
        SpirvType::Adt {
            def_id: None,
            align,
            size,
            field_types: els,
            field_offsets: &field_offsets,
            field_names: None,
        }
        .def(DUMMY_SP, self)
    }
    fn type_kind(&self, ty: Self::Type) -> TypeKind {
        match self.lookup_type(ty) {
            SpirvType::Void => TypeKind::Void,
            SpirvType::Bool | // thanks llvm
            SpirvType::Integer(_, _) => TypeKind::Integer,
            SpirvType::Float(width) => match width {
                16 => TypeKind::Half,
                32 => TypeKind::Float,
                64 => TypeKind::Double,
                other => self
                    .tcx
                    .sess
                    .fatal(format!("Invalid float width in type_kind: {}", other)),
            },
            SpirvType::Adt { .. } | SpirvType::InterfaceBlock { .. } => {
                TypeKind::Struct
            }
            SpirvType::Vector { .. } => TypeKind::Vector,
            SpirvType::Array { .. } | SpirvType::RuntimeArray { .. } | SpirvType::Matrix { .. } => TypeKind::Array,
            SpirvType::Pointer { .. } => TypeKind::Pointer,
            SpirvType::Function { .. } => TypeKind::Function,
            // HACK(eddyb) this is probably the closest `TypeKind` (which is still
            // very much LLVM-specific, sadly) has to offer to "resource handle".
            | SpirvType::Image { .. }
            | SpirvType::Sampler
            | SpirvType::SampledImage { .. }
            | SpirvType::AccelerationStructureKhr
            | SpirvType::RayQueryKhr
                => TypeKind::Token,
        }
    }
    fn type_ptr_to(&self, ty: Self::Type) -> Self::Type {
        SpirvType::Pointer { pointee: ty }.def(DUMMY_SP, self)
    }
    fn type_ptr_to_ext(&self, ty: Self::Type, _address_space: AddressSpace) -> Self::Type {
        SpirvType::Pointer { pointee: ty }.def(DUMMY_SP, self)
    }
    fn element_type(&self, ty: Self::Type) -> Self::Type {
        match self.lookup_type(ty) {
            SpirvType::Pointer { pointee } => pointee,
            SpirvType::Vector { element, .. } => element,
            spirv_type => self.tcx.sess.fatal(format!(
                "element_type called on invalid type: {:?}",
                spirv_type
            )),
        }
    }

    /// Returns the number of elements in `self` if it is a LLVM vector type.
    fn vector_length(&self, ty: Self::Type) -> usize {
        match self.lookup_type(ty) {
            SpirvType::Vector { count, .. } => count as usize,
            ty => self
                .tcx
                .sess
                .fatal(format!("vector_length called on non-vector type: {:?}", ty)),
        }
    }

    fn float_width(&self, ty: Self::Type) -> usize {
        match self.lookup_type(ty) {
            SpirvType::Float(width) => width as usize,
            ty => self
                .tcx
                .sess
                .fatal(format!("float_width called on non-float type: {:?}", ty)),
        }
    }

    /// Retrieves the bit width of the integer type `self`.
    fn int_width(&self, ty: Self::Type) -> u64 {
        match self.lookup_type(ty) {
            SpirvType::Integer(width, _) => width as u64,
            ty => self
                .tcx
                .sess
                .fatal(format!("int_width called on non-integer type: {:?}", ty)),
        }
    }

    fn val_ty(&self, v: Self::Value) -> Self::Type {
        v.ty
    }
}
