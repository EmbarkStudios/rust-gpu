use super::CodegenCx;
use crate::abi::ConvSpirvType;
use crate::builder_spirv::{SpirvConst, SpirvValue, SpirvValueExt, SpirvValueKind};
use crate::spirv_type::SpirvType;
use rspirv::spirv::Word;
use rustc_codegen_ssa::traits::{BaseTypeMethods, ConstMethods, MiscMethods, StaticMethods};
use rustc_middle::bug;
use rustc_middle::mir::interpret::{alloc_range, ConstAllocation, GlobalAlloc, Scalar};
use rustc_middle::ty::layout::LayoutOf;
use rustc_span::{Span, DUMMY_SP};
use rustc_target::abi::{self, AddressSpace, HasDataLayout, Integer, Primitive, Size};

impl<'tcx> CodegenCx<'tcx> {
    pub fn def_constant(&self, ty: Word, val: SpirvConst<'_, 'tcx>) -> SpirvValue {
        self.builder.def_constant_cx(ty, val, self)
    }

    pub fn constant_u8(&self, span: Span, val: u8) -> SpirvValue {
        let ty = SpirvType::Integer(8, false).def(span, self);
        self.def_constant(ty, SpirvConst::U32(val as u32))
    }

    pub fn constant_i16(&self, span: Span, val: i16) -> SpirvValue {
        let ty = SpirvType::Integer(16, true).def(span, self);
        self.def_constant(ty, SpirvConst::U32(val as u32))
    }

    pub fn constant_u16(&self, span: Span, val: u16) -> SpirvValue {
        let ty = SpirvType::Integer(16, false).def(span, self);
        self.def_constant(ty, SpirvConst::U32(val as u32))
    }

    pub fn constant_i32(&self, span: Span, val: i32) -> SpirvValue {
        let ty = SpirvType::Integer(32, true).def(span, self);
        self.def_constant(ty, SpirvConst::U32(val as u32))
    }

    pub fn constant_u32(&self, span: Span, val: u32) -> SpirvValue {
        let ty = SpirvType::Integer(32, false).def(span, self);
        self.def_constant(ty, SpirvConst::U32(val))
    }

    pub fn constant_u64(&self, span: Span, val: u64) -> SpirvValue {
        let ty = SpirvType::Integer(64, false).def(span, self);
        self.def_constant(ty, SpirvConst::U64(val))
    }

    pub fn constant_int(&self, ty: Word, val: u64) -> SpirvValue {
        match self.lookup_type(ty) {
            SpirvType::Integer(bits @ 8..=32, signed) => {
                let size = Size::from_bits(bits);
                let val = val as u128;
                self.def_constant(
                    ty,
                    SpirvConst::U32(if signed {
                        size.sign_extend(val)
                    } else {
                        size.truncate(val)
                    } as u32),
                )
            }
            SpirvType::Integer(64, _) => self.def_constant(ty, SpirvConst::U64(val)),
            SpirvType::Bool => match val {
                0 | 1 => self.def_constant(ty, SpirvConst::Bool(val != 0)),
                _ => self
                    .tcx
                    .sess
                    .fatal(format!("Invalid constant value for bool: {val}")),
            },
            SpirvType::Integer(128, _) => {
                let result = self.undef(ty);
                self.zombie_no_span(result.def_cx(self), "u128 constant");
                result
            }
            other => self.tcx.sess.fatal(format!(
                "constant_int invalid on type {}",
                other.debug(ty, self)
            )),
        }
    }

    pub fn constant_f32(&self, span: Span, val: f32) -> SpirvValue {
        let ty = SpirvType::Float(32).def(span, self);
        self.def_constant(ty, SpirvConst::F32(val.to_bits()))
    }

    pub fn constant_f64(&self, span: Span, val: f64) -> SpirvValue {
        let ty = SpirvType::Float(64).def(span, self);
        self.def_constant(ty, SpirvConst::F64(val.to_bits()))
    }

    pub fn constant_float(&self, ty: Word, val: f64) -> SpirvValue {
        match self.lookup_type(ty) {
            SpirvType::Float(32) => self.def_constant(ty, SpirvConst::F32((val as f32).to_bits())),
            SpirvType::Float(64) => self.def_constant(ty, SpirvConst::F64(val.to_bits())),
            other => self.tcx.sess.fatal(format!(
                "constant_float invalid on type {}",
                other.debug(ty, self)
            )),
        }
    }

    pub fn constant_bool(&self, span: Span, val: bool) -> SpirvValue {
        let ty = SpirvType::Bool.def(span, self);
        self.def_constant(ty, SpirvConst::Bool(val))
    }

    pub fn constant_composite(&self, ty: Word, fields: impl Iterator<Item = Word>) -> SpirvValue {
        // FIXME(eddyb) use `AccumulateVec`s just like `rustc` itself does.
        self.def_constant(ty, SpirvConst::Composite(&fields.collect::<Vec<_>>()))
    }

    pub fn constant_null(&self, ty: Word) -> SpirvValue {
        self.def_constant(ty, SpirvConst::Null)
    }

    pub fn undef(&self, ty: Word) -> SpirvValue {
        self.def_constant(ty, SpirvConst::Undef)
    }
}

impl<'tcx> ConstMethods<'tcx> for CodegenCx<'tcx> {
    fn const_null(&self, t: Self::Type) -> Self::Value {
        self.constant_null(t)
    }
    fn const_undef(&self, ty: Self::Type) -> Self::Value {
        self.undef(ty)
    }
    fn const_poison(&self, ty: Self::Type) -> Self::Value {
        // No distinction between undef and poison.
        self.const_undef(ty)
    }
    fn const_int(&self, t: Self::Type, i: i64) -> Self::Value {
        self.constant_int(t, i as u64)
    }
    fn const_uint(&self, t: Self::Type, i: u64) -> Self::Value {
        self.constant_int(t, i)
    }
    // FIXME(eddyb) support `u128`.
    fn const_uint_big(&self, t: Self::Type, i: u128) -> Self::Value {
        let i_as_u64 = i as u64;
        let c = self.constant_int(t, i_as_u64);
        match self.lookup_type(t) {
            SpirvType::Integer(width, _) if width > 64 => {
                if u128::from(i_as_u64) != i {
                    self.zombie_no_span(
                        c.def_cx(self),
                        "const_uint_big truncated a 128-bit constant to 64 bits",
                    );
                }
            }
            _ => {}
        }
        c
    }
    fn const_bool(&self, val: bool) -> Self::Value {
        self.constant_bool(DUMMY_SP, val)
    }
    fn const_i16(&self, i: i16) -> Self::Value {
        self.constant_i16(DUMMY_SP, i)
    }
    fn const_i32(&self, i: i32) -> Self::Value {
        self.constant_i32(DUMMY_SP, i)
    }
    fn const_u32(&self, i: u32) -> Self::Value {
        self.constant_u32(DUMMY_SP, i)
    }
    fn const_u64(&self, i: u64) -> Self::Value {
        self.constant_u64(DUMMY_SP, i)
    }
    fn const_u128(&self, i: u128) -> Self::Value {
        let ty = SpirvType::Integer(128, false).def(DUMMY_SP, self);
        self.const_uint_big(ty, i)
    }
    fn const_usize(&self, i: u64) -> Self::Value {
        let ptr_size = self.tcx.data_layout.pointer_size.bits() as u32;
        let t = SpirvType::Integer(ptr_size, false).def(DUMMY_SP, self);
        self.constant_int(t, i)
    }
    fn const_u8(&self, i: u8) -> Self::Value {
        self.constant_u8(DUMMY_SP, i)
    }
    fn const_real(&self, t: Self::Type, val: f64) -> Self::Value {
        self.constant_float(t, val)
    }

    fn const_str(&self, s: &str) -> (Self::Value, Self::Value) {
        let len = s.len();
        let str_ty = self
            .layout_of(self.tcx.types.str_)
            .spirv_type(DUMMY_SP, self);
        (
            self.def_constant(
                self.type_ptr_to(str_ty),
                SpirvConst::PtrTo {
                    pointee: self
                        .constant_composite(
                            str_ty,
                            s.bytes().map(|b| self.const_u8(b).def_cx(self)),
                        )
                        .def_cx(self),
                },
            ),
            self.const_usize(len as u64),
        )
    }
    fn const_struct(&self, elts: &[Self::Value], _packed: bool) -> Self::Value {
        // Presumably this will get bitcasted to the right type?
        // FIXME(eddyb) use `AccumulateVec`s just like `rustc` itself does.
        let field_types = elts.iter().map(|f| f.ty).collect::<Vec<_>>();
        let (field_offsets, size, align) = crate::abi::auto_struct_layout(self, &field_types);
        let struct_ty = SpirvType::Adt {
            def_id: None,
            size,
            align,
            field_types: &field_types,
            field_offsets: &field_offsets,
            field_names: None,
        }
        .def(DUMMY_SP, self);
        self.constant_composite(struct_ty, elts.iter().map(|f| f.def_cx(self)))
    }

    fn const_to_opt_uint(&self, v: Self::Value) -> Option<u64> {
        self.builder.lookup_const_u64(v)
    }
    fn const_to_opt_u128(&self, v: Self::Value, sign_ext: bool) -> Option<u128> {
        self.builder.lookup_const_u64(v).map(|v| {
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
        layout: abi::Scalar,
        ty: Self::Type,
    ) -> Self::Value {
        match scalar {
            Scalar::Int(int) => {
                assert_eq!(int.size(), layout.primitive().size(self));
                let data = int.to_bits(int.size()).unwrap();

                match layout.primitive() {
                    Primitive::Int(int_size, int_signedness) => match self.lookup_type(ty) {
                        SpirvType::Integer(width, spirv_signedness) => {
                            assert_eq!(width as u64, int_size.size().bits());
                            assert_eq!(spirv_signedness, int_signedness);
                            self.constant_int(ty, data as u64)
                        }
                        SpirvType::Bool => match data {
                            0 => self.constant_bool(DUMMY_SP, false),
                            1 => self.constant_bool(DUMMY_SP, true),
                            _ => self
                                .tcx
                                .sess
                                .fatal(format!("Invalid constant value for bool: {data}")),
                        },
                        other => self.tcx.sess.fatal(format!(
                            "scalar_to_backend Primitive::Int not supported on type {}",
                            other.debug(ty, self)
                        )),
                    },
                    Primitive::F32 => {
                        let res = self.constant_f32(DUMMY_SP, f32::from_bits(data as u32));
                        assert_eq!(res.ty, ty);
                        res
                    }
                    Primitive::F64 => {
                        let res = self.constant_f64(DUMMY_SP, f64::from_bits(data as u64));
                        assert_eq!(res.ty, ty);
                        res
                    }
                    Primitive::Pointer(_) => {
                        if data == 0 {
                            self.constant_null(ty)
                        } else {
                            let result = self.undef(ty);
                            self.zombie_no_span(
                                result.def_cx(self),
                                "pointer has non-null integer address",
                            );
                            result
                        }
                    }
                }
            }
            Scalar::Ptr(ptr, _) => {
                let (alloc_id, offset) = ptr.into_parts();
                let (base_addr, _base_addr_space) = match self.tcx.global_alloc(alloc_id) {
                    GlobalAlloc::Memory(alloc) => {
                        let pointee = match self.lookup_type(ty) {
                            SpirvType::Pointer { pointee } => pointee,
                            other => self.tcx.sess.fatal(format!(
                                "GlobalAlloc::Memory type not implemented: {}",
                                other.debug(ty, self)
                            )),
                        };
                        let init = self.create_const_alloc(alloc, pointee);
                        let value = self.static_addr_of(init, alloc.inner().align, None);
                        (value, AddressSpace::DATA)
                    }
                    GlobalAlloc::Function(fn_instance) => (
                        self.get_fn_addr(fn_instance.polymorphize(self.tcx)),
                        self.data_layout().instruction_address_space,
                    ),
                    GlobalAlloc::VTable(vty, trait_ref) => {
                        let alloc = self
                            .tcx
                            .global_alloc(self.tcx.vtable_allocation((vty, trait_ref)))
                            .unwrap_memory();
                        let pointee = match self.lookup_type(ty) {
                            SpirvType::Pointer { pointee } => pointee,
                            other => self.tcx.sess.fatal(format!(
                                "GlobalAlloc::VTable type not implemented: {}",
                                other.debug(ty, self)
                            )),
                        };
                        let init = self.create_const_alloc(alloc, pointee);
                        let value = self.static_addr_of(init, alloc.inner().align, None);
                        (value, AddressSpace::DATA)
                    }
                    GlobalAlloc::Static(def_id) => {
                        assert!(self.tcx.is_static(def_id));
                        assert!(!self.tcx.is_thread_local_static(def_id));
                        (self.get_static(def_id), AddressSpace::DATA)
                    }
                };
                let value = if offset.bytes() == 0 {
                    base_addr
                } else {
                    self.tcx
                        .sess
                        .fatal("Non-zero scalar_to_backend ptr.offset not supported")
                    // let offset = self.constant_u64(ptr.offset.bytes());
                    // self.gep(base_addr, once(offset))
                };
                if let Primitive::Pointer(_) = layout.primitive() {
                    assert_ty_eq!(self, value.ty, ty);
                    value
                } else {
                    self.tcx
                        .sess
                        .fatal("Non-pointer-typed scalar_to_backend Scalar::Ptr not supported");
                    // unsafe { llvm::LLVMConstPtrToInt(llval, llty) }
                }
            }
        }
    }

    // HACK(eddyb) this uses a symbolic `ConstDataFromAlloc`, to allow deferring
    // the actual value generation until after a pointer to this value is cast
    // to its final type (e.g. that will be loaded as).
    // FIXME(eddyb) replace this with `qptr` handling of constant data.
    fn const_data_from_alloc(&self, alloc: ConstAllocation<'tcx>) -> Self::Value {
        let void_type = SpirvType::Void.def(DUMMY_SP, self);
        self.def_constant(void_type, SpirvConst::ConstDataFromAlloc(alloc))
    }

    // FIXME(eddyb) is this just redundant with `const_bitcast`?!
    fn const_ptrcast(&self, val: Self::Value, ty: Self::Type) -> Self::Value {
        if val.ty == ty {
            val
        } else {
            // FIXME(eddyb) implement via `OpSpecConstantOp`.
            // FIXME(eddyb) this zombies the original value without creating a new one.
            let result = val.def_cx(self).with_type(ty);
            self.zombie_no_span(result.def_cx(self), "const_ptrcast");
            result
        }
    }
    fn const_bitcast(&self, val: Self::Value, ty: Self::Type) -> Self::Value {
        // HACK(eddyb) special-case `const_data_from_alloc` + `static_addr_of`
        // as the old `from_const_alloc` (now `OperandRef::from_const_alloc`).
        if let SpirvValueKind::IllegalConst(_) = val.kind {
            if let Some(SpirvConst::PtrTo { pointee }) = self.builder.lookup_const(val) {
                if let Some(SpirvConst::ConstDataFromAlloc(alloc)) =
                    self.builder.lookup_const_by_id(pointee)
                {
                    if let SpirvType::Pointer { pointee } = self.lookup_type(ty) {
                        let init = self.create_const_alloc(alloc, pointee);
                        return self.static_addr_of(init, alloc.inner().align, None);
                    }
                }
            }
        }

        if val.ty == ty {
            val
        } else {
            // FIXME(eddyb) implement via `OpSpecConstantOp`.
            // FIXME(eddyb) this zombies the original value without creating a new one.
            let result = val.def_cx(self).with_type(ty);
            self.zombie_no_span(result.def_cx(self), "const_bitcast");
            result
        }
    }
    fn const_ptr_byte_offset(&self, val: Self::Value, offset: Size) -> Self::Value {
        if offset == Size::ZERO {
            val
        } else {
            // FIXME(eddyb) implement via `OpSpecConstantOp`.
            // FIXME(eddyb) this zombies the original value without creating a new one.
            let result = val;
            self.zombie_no_span(result.def_cx(self), "const_ptr_byte_offset");
            result
        }
    }
}

impl<'tcx> CodegenCx<'tcx> {
    // This function comes from `ty::layout`'s `layout_of_uncached`,
    // where it's named `scalar_unit`.
    pub fn primitive_to_scalar(&self, value: Primitive) -> abi::Scalar {
        let bits = value.size(self.data_layout()).bits();
        assert!(bits <= 128);
        abi::Scalar::Initialized {
            value,
            valid_range: abi::WrappingRange {
                start: 0,
                end: (!0 >> (128 - bits)),
            },
        }
    }

    pub fn create_const_alloc(&self, alloc: ConstAllocation<'tcx>, ty: Word) -> SpirvValue {
        // println!(
        //     "Creating const alloc of type {} with {} bytes",
        //     self.debug_type(ty),
        //     alloc.len()
        // );
        let mut offset = Size::ZERO;
        let result = self.create_const_alloc2(alloc, &mut offset, ty);
        assert_eq!(
            offset.bytes_usize(),
            alloc.inner().len(),
            "create_const_alloc must consume all bytes of an Allocation"
        );
        // println!("Done creating alloc of type {}", self.debug_type(ty));
        result
    }

    fn create_const_alloc2(
        &self,
        alloc: ConstAllocation<'tcx>,
        offset: &mut Size,
        ty: Word,
    ) -> SpirvValue {
        let ty_concrete = self.lookup_type(ty);
        *offset = offset.align_to(ty_concrete.alignof(self));
        // these print statements are really useful for debugging, so leave them easily available
        // println!("const at {}: {}", offset.bytes(), self.debug_type(ty));
        match ty_concrete {
            SpirvType::Void => self
                .tcx
                .sess
                .fatal("cannot create const alloc of type void"),
            SpirvType::Bool
            | SpirvType::Integer(..)
            | SpirvType::Float(_)
            | SpirvType::Pointer { .. } => {
                let size = ty_concrete.sizeof(self).unwrap();
                let primitive = match ty_concrete {
                    SpirvType::Bool => Primitive::Int(Integer::fit_unsigned(0), false),
                    SpirvType::Integer(int_size, int_signedness) => {
                        let integer = match int_size {
                            8 => Integer::I8,
                            16 => Integer::I16,
                            32 => Integer::I32,
                            64 => Integer::I64,
                            128 => Integer::I128,
                            other => {
                                self.tcx
                                    .sess
                                    .fatal(format!("invalid size for integer: {other}"));
                            }
                        };
                        Primitive::Int(integer, int_signedness)
                    }
                    SpirvType::Float(float_size) => match float_size {
                        32 => Primitive::F32,
                        64 => Primitive::F64,
                        other => {
                            self.tcx
                                .sess
                                .fatal(format!("invalid size for float: {other}"));
                        }
                    },
                    SpirvType::Pointer { .. } => Primitive::Pointer(AddressSpace::DATA),
                    unsupported_spirv_type => bug!(
                        "invalid spirv type internal to create_alloc_const2: {:?}",
                        unsupported_spirv_type
                    ),
                };
                // alloc_id is not needed by read_scalar, so we just use 0. If the context
                // refers to a pointer, read_scalar will find the the actual alloc_id. It
                // only uses the input alloc_id in the case that the scalar is uninitilized
                // as part of the error output
                // tldr, the pointer here is only needed for the offset
                let value = match alloc.inner().read_scalar(
                    self,
                    alloc_range(*offset, size),
                    matches!(primitive, Primitive::Pointer(_)),
                ) {
                    Ok(scalar) => {
                        self.scalar_to_backend(scalar, self.primitive_to_scalar(primitive), ty)
                    }
                    _ => self.undef(ty),
                };
                *offset += size;
                value
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
                    values.push(
                        self.create_const_alloc2(alloc, &mut total_offset_end, ty)
                            .def_cx(self),
                    );
                    occupied_spaces.push(total_offset_start..total_offset_end);
                }
                if let Some(size) = size {
                    *offset += size;
                } else {
                    assert_eq!(
                        offset.bytes_usize(),
                        alloc.inner().len(),
                        "create_const_alloc must consume all bytes of an Allocation after an unsized struct"
                    );
                }
                self.constant_composite(ty, values.into_iter())
            }
            SpirvType::Array { element, count } => {
                let count = self.builder.lookup_const_u64(count).unwrap() as usize;
                let values = (0..count).map(|_| {
                    self.create_const_alloc2(alloc, offset, element)
                        .def_cx(self)
                });
                self.constant_composite(ty, values)
            }
            SpirvType::Vector { element, count } => {
                let total_size = ty_concrete
                    .sizeof(self)
                    .expect("create_const_alloc: Vectors must be sized");
                let final_offset = *offset + total_size;
                let values = (0..count).map(|_| {
                    self.create_const_alloc2(alloc, offset, element)
                        .def_cx(self)
                });
                let result = self.constant_composite(ty, values);
                assert!(*offset <= final_offset);
                // Vectors sometimes have padding at the end (e.g. vec3), skip over it.
                *offset = final_offset;
                result
            }
            SpirvType::Matrix { element, count } => {
                let total_size = ty_concrete
                    .sizeof(self)
                    .expect("create_const_alloc: Matrices must be sized");
                let final_offset = *offset + total_size;
                let values = (0..count).map(|_| {
                    self.create_const_alloc2(alloc, offset, element)
                        .def_cx(self)
                });
                let result = self.constant_composite(ty, values);
                assert!(*offset <= final_offset);
                // Matrices sometimes have padding at the end (e.g. Mat4x3), skip over it.
                *offset = final_offset;
                result
            }
            SpirvType::RuntimeArray { element } => {
                let mut values = Vec::new();
                while offset.bytes_usize() != alloc.inner().len() {
                    values.push(
                        self.create_const_alloc2(alloc, offset, element)
                            .def_cx(self),
                    );
                }
                let result = self.constant_composite(ty, values.into_iter());
                // TODO: Figure out how to do this. Compiling the below crashes both clspv *and* llvm-spirv:
                /*
                __constant struct A {
                    float x;
                    int y[];
                } a = {1, {2, 3, 4}};

                __kernel void foo(__global int* data, __constant int* c) {
                __constant struct A* asdf = &a;
                *data = *c + asdf->y[*c];
                }
                */
                // NOTE(eddyb) the above description is a bit outdated, it's now
                // clear `OpTypeRuntimeArray` does not belong in user code, and
                // is only for dynamically-sized SSBOs and descriptor indexing,
                // and a general solution looks similar to `union` handling, but
                // for the length of a fixed-length array.
                self.zombie_no_span(result.def_cx(self), "constant `OpTypeRuntimeArray` value");
                result
            }
            SpirvType::Function { .. } => self
                .tcx
                .sess
                .fatal("TODO: SpirvType::Function not supported yet in create_const_alloc"),
            SpirvType::Image { .. } => self.tcx.sess.fatal("cannot create a constant image value"),
            SpirvType::Sampler => self
                .tcx
                .sess
                .fatal("cannot create a constant sampler value"),
            SpirvType::SampledImage { .. } => self
                .tcx
                .sess
                .fatal("cannot create a constant sampled image value"),
            SpirvType::InterfaceBlock { .. } => self
                .tcx
                .sess
                .fatal("cannot create a constant interface block value"),
            SpirvType::AccelerationStructureKhr => self
                .tcx
                .sess
                .fatal("cannot create a constant acceleration structure"),
            SpirvType::RayQueryKhr => self.tcx.sess.fatal("cannot create a constant ray query"),
        }
    }
}
