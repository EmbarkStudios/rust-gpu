use super::CodegenCx;
use crate::abi::ConvSpirvType;
use crate::builder_spirv::{SpirvConst, SpirvValue, SpirvValueExt};
use crate::spirv_type::SpirvType;
use rspirv::spirv::Word;
use rustc_codegen_ssa::mir::place::PlaceRef;
use rustc_codegen_ssa::traits::{BaseTypeMethods, ConstMethods, MiscMethods, StaticMethods};
use rustc_middle::bug;
use rustc_middle::mir::interpret::{read_target_uint, Allocation, GlobalAlloc, Pointer};
use rustc_middle::ty::layout::TyAndLayout;
use rustc_mir::interpret::Scalar;
use rustc_span::symbol::Symbol;
use rustc_target::abi::{self, AddressSpace, HasDataLayout, LayoutOf, Primitive, Size};
use std::ops::Range;

impl<'tcx> CodegenCx<'tcx> {
    pub fn constant_u8(&self, val: u8) -> SpirvValue {
        let ty = SpirvType::Integer(8, false).def(self);
        self.builder.def_constant(SpirvConst::U32(ty, val as u32))
    }

    pub fn constant_u16(&self, val: u16) -> SpirvValue {
        let ty = SpirvType::Integer(16, false).def(self);
        self.builder.def_constant(SpirvConst::U32(ty, val as u32))
    }

    pub fn constant_i32(&self, val: i32) -> SpirvValue {
        let ty = SpirvType::Integer(32, !self.kernel_mode).def(self);
        self.builder.def_constant(SpirvConst::U32(ty, val as u32))
    }

    pub fn constant_u32(&self, val: u32) -> SpirvValue {
        let ty = SpirvType::Integer(32, false).def(self);
        self.builder.def_constant(SpirvConst::U32(ty, val))
    }

    pub fn constant_u64(&self, val: u64) -> SpirvValue {
        let ty = SpirvType::Integer(64, false).def(self);
        self.builder.def_constant(SpirvConst::U64(ty, val))
    }

    pub fn constant_int(&self, ty: Word, val: u64) -> SpirvValue {
        match self.lookup_type(ty) {
            SpirvType::Integer(8, false) => self
                .builder
                .def_constant(SpirvConst::U32(ty, val as u8 as u32)),
            SpirvType::Integer(16, false) => self
                .builder
                .def_constant(SpirvConst::U32(ty, val as u16 as u32)),
            SpirvType::Integer(32, false) => {
                self.builder.def_constant(SpirvConst::U32(ty, val as u32))
            }
            SpirvType::Integer(64, false) => self.builder.def_constant(SpirvConst::U64(ty, val)),
            SpirvType::Integer(8, true) => self
                .builder
                .def_constant(SpirvConst::U32(ty, val as i64 as i8 as u32)),
            SpirvType::Integer(16, true) => self
                .builder
                .def_constant(SpirvConst::U32(ty, val as i64 as i16 as u32)),
            SpirvType::Integer(32, true) => self
                .builder
                .def_constant(SpirvConst::U32(ty, val as i64 as i32 as u32)),
            SpirvType::Integer(64, true) => self.builder.def_constant(SpirvConst::U64(ty, val)),
            SpirvType::Bool => match val {
                0 => self.builder.def_constant(SpirvConst::Bool(ty, false)),
                1 => self.builder.def_constant(SpirvConst::Bool(ty, true)),
                _ => self
                    .tcx
                    .sess
                    .fatal(&format!("Invalid constant value for bool: {}", val)),
            },
            SpirvType::Integer(128, _) => {
                let result = self.undef(ty);
                self.zombie_no_span(result.def, "u128 constant");
                result
            }
            other => self.tcx.sess.fatal(&format!(
                "constant_int invalid on type {}",
                other.debug(ty, self)
            )),
        }
    }

    pub fn constant_f32(&self, val: f32) -> SpirvValue {
        let ty = SpirvType::Float(32).def(self);
        self.builder
            .def_constant(SpirvConst::F32(ty, val.to_bits()))
    }

    pub fn constant_f64(&self, val: f64) -> SpirvValue {
        let ty = SpirvType::Float(64).def(self);
        self.builder
            .def_constant(SpirvConst::F64(ty, val.to_bits()))
    }

    pub fn constant_float(&self, ty: Word, val: f64) -> SpirvValue {
        match self.lookup_type(ty) {
            SpirvType::Float(32) => self
                .builder
                .def_constant(SpirvConst::F32(ty, (val as f32).to_bits())),
            SpirvType::Float(64) => self
                .builder
                .def_constant(SpirvConst::F64(ty, val.to_bits())),
            other => self.tcx.sess.fatal(&format!(
                "constant_float invalid on type {}",
                other.debug(ty, self)
            )),
        }
    }

    pub fn constant_bool(&self, val: bool) -> SpirvValue {
        let ty = SpirvType::Bool.def(self);
        self.builder.def_constant(SpirvConst::Bool(ty, val))
    }

    pub fn constant_composite(&self, ty: Word, val: Vec<Word>) -> SpirvValue {
        self.builder.def_constant(SpirvConst::Composite(ty, val))
    }

    pub fn constant_null(&self, ty: Word) -> SpirvValue {
        self.builder.def_constant(SpirvConst::Null(ty))
    }

    pub fn undef(&self, ty: Word) -> SpirvValue {
        self.builder.def_constant(SpirvConst::Undef(ty))
    }
}

impl<'tcx> ConstMethods<'tcx> for CodegenCx<'tcx> {
    fn const_null(&self, t: Self::Type) -> Self::Value {
        self.constant_null(t)
    }
    fn const_undef(&self, ty: Self::Type) -> Self::Value {
        self.undef(ty)
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
        self.constant_bool(val)
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
        let ty = self.type_ptr_to(self.layout_of(self.tcx.types.str_).spirv_type(self));
        let result = self.undef(ty);
        self.zombie_no_span(result.def, "constant string");
        (result, self.const_usize(len as u64))
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
        self.constant_composite(struct_ty, elts.iter().map(|f| f.def).collect())
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
                            if !self.kernel_mode {
                                assert_eq!(spirv_signedness, int_signedness);
                            }
                            self.constant_int(ty, data as u64)
                        }
                        SpirvType::Bool => match data {
                            0 => self.constant_bool(false),
                            1 => self.constant_bool(true),
                            _ => self
                                .tcx
                                .sess
                                .fatal(&format!("Invalid constant value for bool: {}", data)),
                        },
                        other => self.tcx.sess.fatal(&format!(
                            "scalar_to_backend Primitive::Int not supported on type {}",
                            other.debug(ty, self)
                        )),
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
                Primitive::Pointer => bug!("scalar_to_backend Primitive::Ptr is an invalid state"),
            },
            Scalar::Ptr(ptr) => {
                let (base_addr, _base_addr_space) = match self.tcx.global_alloc(ptr.alloc_id) {
                    GlobalAlloc::Memory(alloc) => {
                        let pointee = match self.lookup_type(ty) {
                            SpirvType::Pointer { pointee, .. } => pointee,
                            other => self.tcx.sess.fatal(&format!(
                                "GlobalAlloc::Memory type not implemented: {}",
                                other.debug(ty, self)
                            )),
                        };
                        let init = self.create_const_alloc(alloc, pointee);
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
                    self.tcx
                        .sess
                        .fatal("Non-constant scalar_to_backend ptr.offset not supported")
                    // let offset = self.constant_u64(ptr.offset.bytes());
                    // self.gep(base_addr, once(offset))
                };
                if layout.value != Primitive::Pointer {
                    self.tcx
                        .sess
                        .fatal("Non-pointer-typed scalar_to_backend Scalar::Ptr not supported");
                // unsafe { llvm::LLVMConstPtrToInt(llval, llty) }
                } else {
                    match (self.lookup_type(value.ty), self.lookup_type(ty)) {
                        (
                            SpirvType::Pointer {
                                storage_class: a_space,
                                pointee: a,
                            },
                            SpirvType::Pointer {
                                storage_class: b_space,
                                pointee: b,
                            },
                        ) => {
                            if a_space != b_space {
                                // TODO: Emit the correct type that is passed into this function.
                                self.zombie_no_span(value.def, "invalid pointer space in constant");
                            }
                            assert_ty_eq!(self, a, b);
                        }
                        _ => assert_ty_eq!(self, value.ty, ty),
                    }
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
        let init = self.create_const_alloc(alloc, ty);
        let result = self.static_addr_of(init, alloc.align, None);
        PlaceRef::new_sized(result, layout)
    }

    fn const_ptrcast(&self, val: Self::Value, ty: Self::Type) -> Self::Value {
        if val.ty == ty {
            val
        } else {
            // constant ptrcast is not supported in spir-v
            let result = val.def.with_type(ty);
            self.zombie_no_span(result.def, "const_ptrcast");
            result
        }
    }
}

impl<'tcx> CodegenCx<'tcx> {
    pub fn create_const_alloc(&self, alloc: &Allocation, ty: Word) -> SpirvValue {
        // println!(
        //     "Creating const alloc of type {} with {} bytes",
        //     self.debug_type(ty),
        //     alloc.len()
        // );
        let mut offset = Size::ZERO;
        let result = self.create_const_alloc2(alloc, &mut offset, ty);
        assert_eq!(
            offset.bytes_usize(),
            alloc.len(),
            "create_const_alloc must consume all bytes of an Allocation"
        );
        // println!("Done creating alloc of type {}", self.debug_type(ty));
        result
    }

    fn create_const_alloc2(&self, alloc: &Allocation, offset: &mut Size, ty: Word) -> SpirvValue {
        let ty_concrete = self.lookup_type(ty);
        *offset = offset.align_to(ty_concrete.alignof(self));
        // these print statements are really useful for debugging, so leave them easily available
        // println!("const at {}: {}", offset.bytes(), self.debug_type(ty));
        match ty_concrete {
            SpirvType::Void => self
                .tcx
                .sess
                .fatal("Cannot create const alloc of type void"),
            SpirvType::Bool => self.constant_bool(self.read_alloc_val(alloc, offset, 1) != 0),
            SpirvType::Integer(width, _) => {
                let v = self.read_alloc_val(alloc, offset, (width / 8) as usize);
                self.constant_int(ty, v as u64)
            }
            SpirvType::Float(width) => {
                let v = self.read_alloc_val(alloc, offset, (width / 8) as usize);
                match width {
                    32 => self.constant_f32(f32::from_bits(v as u32)),
                    64 => self.constant_f64(f64::from_bits(v as u64)),
                    other => self
                        .tcx
                        .sess
                        .fatal(&format!("invalid float width {}", other)),
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
                    values.push(
                        self.create_const_alloc2(alloc, &mut total_offset_end, ty)
                            .def,
                    );
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
                Self::assert_uninit(alloc, base, *offset, occupied_spaces);
                self.constant_composite(ty, values)
            }
            SpirvType::Opaque { name } => self.tcx.sess.fatal(&format!(
                "Cannot create const alloc of type opaque: {}",
                name
            )),
            SpirvType::Array { element, count } => {
                let count = self.builder.lookup_const_u64(count).unwrap() as usize;
                let values = (0..count)
                    .map(|_| self.create_const_alloc2(alloc, offset, element).def)
                    .collect::<Vec<_>>();
                self.constant_composite(ty, values)
            }
            SpirvType::Vector { element, count } => {
                let total_size = ty_concrete
                    .sizeof(self)
                    .expect("create_const_alloc: Vectors must be sized");
                let final_offset = *offset + total_size;
                let values = (0..count)
                    .map(|_| self.create_const_alloc2(alloc, offset, element).def)
                    .collect::<Vec<_>>();
                assert!(*offset <= final_offset);
                // Vectors sometimes have padding at the end (e.g. vec3), skip over it.
                *offset = final_offset;
                self.constant_composite(ty, values)
            }
            SpirvType::RuntimeArray { element } => {
                let mut values = Vec::new();
                while offset.bytes_usize() != alloc.len() {
                    values.push(self.create_const_alloc2(alloc, offset, element).def);
                }
                let result = self.constant_composite(ty, values);
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
                self.zombie_no_span(result.def, "constant runtime array value");
                result
            }
            SpirvType::Pointer { .. } => {
                let ptr = self.read_alloc_ptr(alloc, offset);
                self.scalar_to_backend(
                    ptr.into(),
                    &abi::Scalar {
                        value: Primitive::Pointer,
                        valid_range: 0..=!0,
                    },
                    ty,
                )
            }
            SpirvType::Function { .. } => self
                .tcx
                .sess
                .fatal("TODO: SpirvType::Function not supported yet in create_const_alloc"),
        }
    }

    // Advances offset by len
    fn read_alloc_val<'a>(&self, alloc: &'a Allocation, offset: &mut Size, len: usize) -> u128 {
        let off = offset.bytes_usize();
        let bytes = alloc.inspect_with_uninit_and_ptr_outside_interpreter(off..(off + len));
        // check relocations (pointer values)
        assert!({
            let start = off.saturating_sub(self.data_layout().pointer_size.bytes_usize() - 1);
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
        read_target_uint(self.data_layout().endian, bytes).unwrap()
    }

    // Advances offset by ptr size
    fn read_alloc_ptr<'a>(&self, alloc: &'a Allocation, offset: &mut Size) -> Pointer {
        let off = offset.bytes_usize();
        let len = self.data_layout().pointer_size.bytes_usize();
        // check init
        alloc
            .init_mask()
            .is_range_initialized(*offset, *offset + Size::from_bytes(len))
            .unwrap();
        let bytes = alloc.inspect_with_uninit_and_ptr_outside_interpreter(off..(off + len));
        let inner_offset = read_target_uint(self.data_layout().endian, bytes).unwrap();
        let &((), alloc_id) = alloc.relocations().get(&Size::from_bytes(off)).unwrap();
        *offset += Size::from_bytes(len);
        Pointer::new_with_tag(alloc_id, Size::from_bytes(inner_offset), ())
    }

    fn assert_uninit(
        alloc: &Allocation,
        start: Size,
        end: Size,
        occupied_ranges: Vec<Range<Size>>,
    ) {
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
}
