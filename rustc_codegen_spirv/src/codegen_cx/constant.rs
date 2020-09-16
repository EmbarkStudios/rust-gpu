use super::CodegenCx;
use crate::abi::ConvSpirvType;
use crate::builder_spirv::{SpirvValue, SpirvValueExt};
use crate::spirv_type::SpirvType;
use rspirv::dr::Operand;
use rspirv::spirv::{StorageClass, Word};
use rustc_codegen_ssa::mir::place::PlaceRef;
use rustc_codegen_ssa::traits::{BaseTypeMethods, ConstMethods, MiscMethods, StaticMethods};
use rustc_middle::mir::interpret::{read_target_uint, Allocation, GlobalAlloc, Pointer};
use rustc_middle::ty::layout::TyAndLayout;
use rustc_mir::interpret::Scalar;
use rustc_span::symbol::Symbol;
use rustc_target::abi::{self, AddressSpace, HasDataLayout, LayoutOf, Primitive, Size};
use std::ops::Range;

impl<'tcx> CodegenCx<'tcx> {
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
        let ty = SpirvType::Integer(32, !self.kernel_mode).def(self);
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
                self.zombie(result, "u128 constant");
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
        let raw_bytes = self.const_bytes(s.as_str().as_bytes());
        let ptr = self
            .builder
            .find_global_constant_variable(raw_bytes.def)
            .unwrap_or_else(|| {
                let ty = self.type_ptr_to(self.layout_of(self.tcx.types.str_).spirv_type(self));
                let result = self
                    .emit_global()
                    .variable(ty, None, StorageClass::Function, Some(raw_bytes.def))
                    .with_type(ty);
                // The types don't line up (dynamic array vs. constant array)
                self.zombie(result.def, "constant string");
                result
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
                            if !self.kernel_mode {
                                assert_eq!(spirv_signedness, int_signedness);
                            }
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
                    panic!("scalar_to_backend Primitive::Ptr is an invalid state")
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
                    panic!("Non-constant scalar_to_backend ptr.offset not supported")
                    // let offset = self.constant_u64(ptr.offset.bytes());
                    // self.gep(base_addr, once(offset))
                };
                if layout.value != Primitive::Pointer {
                    panic!("Non-pointer-typed scalar_to_backend Scalar::Ptr not supported");
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
                                self.zombie(value.def, "invalid pointer space in constant");
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
            self.zombie(result.def, "const_ptrcast");
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
            SpirvType::Void => panic!("Cannot create const alloc of type void"),
            SpirvType::Bool => match self.read_alloc_val(alloc, offset, 1) != 0 {
                true => self.emit_global().constant_true(ty),
                false => self.emit_global().constant_false(ty),
            }
            .with_type(ty),
            SpirvType::Integer(width, _) => {
                let v = self.read_alloc_val(alloc, offset, (width / 8) as usize);
                self.constant_int(ty, v as u64)
            }
            SpirvType::Float(width) => {
                let v = self.read_alloc_val(alloc, offset, (width / 8) as usize);
                match width {
                    32 => self.constant_f32(f32::from_bits(v as u32)),
                    64 => self.constant_f64(f64::from_bits(v as u64)),
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
                self.emit_global()
                    .constant_composite(ty, values)
                    .with_type(ty)
            }
            SpirvType::Opaque { name } => {
                panic!("Cannot create const alloc of type opaque: {}", name)
            }
            SpirvType::Array { element, count } => {
                let count = self.builder.lookup_const_u64(count).unwrap() as usize;
                let values = (0..count)
                    .map(|_| self.create_const_alloc2(alloc, offset, element).def)
                    .collect::<Vec<_>>();
                self.emit_global()
                    .constant_composite(ty, values)
                    .with_type(ty)
            }
            SpirvType::Vector { element, count } => {
                let values = (0..count)
                    .map(|_| self.create_const_alloc2(alloc, offset, element).def)
                    .collect::<Vec<_>>();
                self.emit_global()
                    .constant_composite(ty, values)
                    .with_type(ty)
            }
            SpirvType::RuntimeArray { element } => {
                let mut values = Vec::new();
                while offset.bytes_usize() != alloc.len() {
                    values.push(self.create_const_alloc2(alloc, offset, element).def);
                }
                let result = self
                    .emit_global()
                    .constant_composite(ty, values)
                    .with_type(ty);
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
                self.zombie(result.def, "constant runtime array value");
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
            SpirvType::Function { .. } => {
                panic!("TODO: SpirvType::Function not supported yet in create_const_alloc")
            }
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

    fn const_bytes(&self, bytes: &[u8]) -> SpirvValue {
        let ty = SpirvType::Array {
            element: SpirvType::Integer(8, false).def(self),
            count: self.constant_u32(bytes.len() as u32).def,
        }
        .def(self);
        let values = bytes
            .iter()
            .map(|&b| self.constant_u8(b).def)
            .collect::<Vec<_>>();
        self.emit_global()
            .constant_composite(ty, values)
            .with_type(ty)
    }
}
