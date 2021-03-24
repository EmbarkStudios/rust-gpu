use rustc_middle::bug;

use super::Builder;
use crate::builder_spirv::{BuilderCursor, SpirvValue, SpirvValueExt};
use crate::codegen_cx::BindlessDescriptorSets;
use crate::rustc_codegen_ssa::traits::BuilderMethods;
use crate::spirv_type::SpirvType;
use rspirv::spirv::Word;
use rustc_span::DUMMY_SP;
use rustc_target::abi::{Abi, Align, Scalar, Size};

impl<'a, 'tcx> Builder<'a, 'tcx> {
    // walk down every member in the ADT recursively and load their values as uints
    // this will break up larger data types into uint sized sections, for
    // each load, this also has an offset in dwords.
    fn recurse_adt_for_stores(
        &mut self,
        uint_ty: u32,
        val: SpirvValue,
        base_offset: u32,
        uint_values_and_offsets: &mut Vec<(u32, SpirvValue)>,
    ) {
        let ty = self.lookup_type(val.ty);

        match ty {
            SpirvType::Adt {
                ref field_types,
                ref field_offsets,
                ..
            } => {
                for (element_idx, (ty, offset)) in
                    field_types.iter().zip(field_offsets.iter()).enumerate()
                {
                    let load_res = self.extract_value(val, element_idx as u64);
                    let offset = offset.bytes() as u32 / 4;

                    self.recurse_adt_for_stores(
                        uint_ty,
                        load_res,
                        base_offset + offset,
                        uint_values_and_offsets,
                    );
                }
            }
            SpirvType::Vector { count, element } => {
                for offset in 0..count {
                    let load_res = self.extract_value(val, offset as u64);

                    self.recurse_adt_for_stores(
                        uint_ty,
                        load_res,
                        base_offset + offset,
                        uint_values_and_offsets,
                    );
                }
            }
            SpirvType::Float(bits) => {
                let unsigned_ty = SpirvType::Integer(bits, false).def(rustc_span::DUMMY_SP, self);
                let val_def = val.def(self);

                let bitcast_res = self
                    .emit()
                    .bitcast(unsigned_ty, None, val_def)
                    .unwrap()
                    .with_type(unsigned_ty);

                self.store_as_u32(
                    bits,
                    false,
                    uint_ty,
                    bitcast_res,
                    base_offset,
                    uint_values_and_offsets,
                );
            }
            SpirvType::Integer(bits, signed) => {
                self.store_as_u32(
                    bits,
                    signed,
                    uint_ty,
                    val,
                    base_offset,
                    uint_values_and_offsets,
                );
            }
            _ => {
                bug!(
                    "codegen_internal_buffer_store doesn't support this type: {:?}",
                    val
                );
            }
        }
    }

    fn store_as_u32(
        &mut self,
        bits: u32,
        signed: bool,
        uint_ty: u32,
        val: SpirvValue,
        base_offset: u32,
        uint_values_and_offsets: &mut Vec<(u32, SpirvValue)>,
    ) {
        let val_def = val.def(self);

        match (bits, signed) {
            (16, _) => {
                bug!("16 bit integer stores are currently un-tested");
                // jb-todo: if we have two 16-bit types, adjacent we should bit-or them together into one u32

                let (ushort_ty, ushort_data) = if signed {
                    // bitcast from i16 into a u16 first, then proceed
                    let ushort_ty = SpirvType::Integer(16, false).def(rustc_span::DUMMY_SP, self);

                    let bitcast_res = self.emit().bitcast(ushort_ty, None, val_def).unwrap();

                    (ushort_ty, bitcast_res)
                } else {
                    (val.ty, val_def)
                };

                let up_casted = self
                    .emit()
                    .u_convert(uint_ty, None, ushort_data)
                    .unwrap()
                    .with_type(uint_ty);

                uint_values_and_offsets.push((base_offset, up_casted));
            }
            (32, false) => uint_values_and_offsets.push((base_offset, val)),
            (32, true) => {
                // need a bitcast to go from signed to unsigned
                let bitcast_res = self
                    .emit()
                    .bitcast(uint_ty, None, val_def)
                    .unwrap()
                    .with_type(uint_ty);

                uint_values_and_offsets.push((base_offset, bitcast_res));
            }
            (64, _) => {
                let (ulong_ty, ulong_data) = if signed {
                    // bitcast from i64 into a u64 first, then proceed
                    let ulong_ty = SpirvType::Integer(64, false).def(rustc_span::DUMMY_SP, self);

                    let bitcast_res = self.emit().bitcast(ulong_ty, None, val_def).unwrap();

                    (ulong_ty, bitcast_res)
                } else {
                    (val.ty, val_def)
                };

                // [base] => uint(ulong_data)
                // [base + 1] => uint(ulong_data >> 32)
                let lower = self
                    .emit()
                    .u_convert(uint_ty, None, ulong_data)
                    .unwrap()
                    .with_type(uint_ty);
                uint_values_and_offsets.push((base_offset, lower));

                let const_32 = self.constant_int(uint_ty, 32).def(self);
                let shifted = self
                    .emit()
                    .shift_right_logical(ulong_ty, None, ulong_data, const_32)
                    .unwrap();
                let upper = self
                    .emit()
                    .u_convert(uint_ty, None, shifted)
                    .unwrap()
                    .with_type(uint_ty);
                uint_values_and_offsets.push((base_offset + 1, upper));
            }
            _ => bug!(
                "codegen_internal_buffer_store doesn't support integers of {}-bits signed: {}",
                bits,
                signed
            ),
        }
    }

    pub(crate) fn codegen_internal_buffer_store(
        &mut self,
        result_type: Word,
        args: &[SpirvValue],
    ) -> SpirvValue {
        let uint_ty = SpirvType::Integer(32, false).def(rustc_span::DUMMY_SP, self);

        let uniform_uint_ptr =
            SpirvType::Pointer { pointee: uint_ty }.def(rustc_span::DUMMY_SP, self);

        let zero = self.constant_int(uint_ty, 0).def(self);

        let sets = self.bindless_descriptor_sets.borrow().unwrap();

        let mut data = self.lookup_type(args[2].ty);

        let bindless_idx = args[0].def(self);
        let offset_arg = args[1].def(self);

        let two = self.constant_int(uint_ty, 2).def(self);

        let dword_offset = self
            .emit()
            .shift_right_arithmetic(uint_ty, None, offset_arg, two)
            .unwrap();

        let mut uint_values_and_offsets = vec![];
        self.recurse_adt_for_stores(uint_ty, args[2], 0, &mut uint_values_and_offsets);

        for (offset, uint_value) in uint_values_and_offsets {
            let offset = if offset > 0 {
                let element_offset = self.constant_int(uint_ty, offset as u64).def(self);

                self.emit()
                    .i_add(uint_ty, None, dword_offset, element_offset)
                    .unwrap()
            } else {
                dword_offset
            };

            let indices = vec![bindless_idx, zero, offset];

            let access_chain = self
                .emit()
                .access_chain(uniform_uint_ptr, None, sets.buffers, indices)
                .unwrap()
                .with_type(uniform_uint_ptr);

            self.store(uint_value, access_chain, Align::from_bytes(0).unwrap());
        }

        self.emit_global()
            .type_void()
            .with_type(SpirvType::Void.def(rustc_span::DUMMY_SP, self))
    }

    pub(crate) fn codegen_internal_buffer_load(
        &mut self,
        result_type: Word,
        args: &[SpirvValue],
    ) -> SpirvValue {
        let uint_ty = SpirvType::Integer(32, false).def(rustc_span::DUMMY_SP, self);

        let uniform_uint_ptr =
            SpirvType::Pointer { pointee: uint_ty }.def(rustc_span::DUMMY_SP, self);

        let two = self.constant_int(uint_ty, 2).def(self);

        let offset_arg = args[1].def(self);

        let base_offset_var = self
            .emit()
            .shift_right_arithmetic(uint_ty, None, offset_arg, two)
            .unwrap();

        let bindless_idx = args[0].def(self);

        let sets = self.bindless_descriptor_sets.borrow().unwrap();

        self.recurse_adt_for_loads(
            uint_ty,
            uniform_uint_ptr,
            bindless_idx,
            base_offset_var,
            0,
            result_type,
            &sets,
        )
    }

    fn load_from_u32(
        &mut self,
        bits: u32,
        signed: bool,
        target_ty: Word,
        uint_ty: u32,
        uniform_uint_ptr: u32,
        bindless_idx: u32,
        base_offset_var: Word,
        element_offset_literal: u32,
        sets: &BindlessDescriptorSets,
    ) -> SpirvValue {
        let zero = self.constant_int(uint_ty, 0).def(self);

        let offset = if element_offset_literal > 0 {
            let element_offset = self
                .constant_int(uint_ty, element_offset_literal as u64)
                .def(self);

            self.emit()
                .i_add(uint_ty, None, base_offset_var, element_offset)
                .unwrap()
        } else {
            base_offset_var
        };

        let indices = vec![bindless_idx, zero, offset];

        let result = self
            .emit()
            .access_chain(uniform_uint_ptr, None, sets.buffers, indices)
            .unwrap();

        match (bits, signed) {
            (32, false) => self
                .emit()
                .load(uint_ty, None, result, None, std::iter::empty())
                .unwrap()
                .with_type(uint_ty),
            (32, true) => {
                let load_res = self
                    .emit()
                    .load(uint_ty, None, result, None, std::iter::empty())
                    .unwrap();

                self.emit()
                    .bitcast(target_ty, None, load_res)
                    .unwrap()
                    .with_type(target_ty)
            }
            (64, _) => {
                // lower = u64(base[0])
                // upper = u64(base[1])
                // result = lower | (upper << 32)
                let ulong_ty = SpirvType::Integer(64, false).def(rustc_span::DUMMY_SP, self);

                let lower = self
                    .emit()
                    .load(uint_ty, None, result, None, std::iter::empty())
                    .unwrap();

                let lower = self.emit().u_convert(ulong_ty, None, lower).unwrap();

                let const_one = self.constant_int(uint_ty, 1 as u64).def(self);

                let upper_offset = self.emit().i_add(uint_ty, None, offset, const_one).unwrap();

                let indices = vec![bindless_idx, zero, upper_offset];

                let upper_chain = self
                    .emit()
                    .access_chain(uniform_uint_ptr, None, sets.buffers, indices)
                    .unwrap();

                let upper = self
                    .emit()
                    .load(uint_ty, None, upper_chain, None, std::iter::empty())
                    .unwrap();

                let upper = self.emit().u_convert(ulong_ty, None, upper).unwrap();

                let thirty_two = self.constant_int(uint_ty, 32).def(self);

                let upper_shifted = self
                    .emit()
                    .shift_left_logical(ulong_ty, None, upper, thirty_two)
                    .unwrap();

                let value = self
                    .emit()
                    .bitwise_or(ulong_ty, None, upper_shifted, lower)
                    .unwrap();

                if signed {
                    self.emit()
                        .bitcast(target_ty, None, value)
                        .unwrap()
                        .with_type(target_ty)
                } else {
                    value.with_type(ulong_ty)
                }
            }
            _ => panic!("Invalid load bits: {} signed: {}", bits, signed),
        }
    }

    fn recurse_adt_for_loads(
        &mut self,
        uint_ty: u32,
        uniform_uint_ptr: u32,
        bindless_idx: u32,
        base_offset_var: Word,
        element_offset_literal: u32,
        result_type: u32,
        sets: &BindlessDescriptorSets,
    ) -> SpirvValue {
        let data = self.lookup_type(result_type);

        match data {
            SpirvType::Adt {
                ref field_types,
                ref field_offsets,
                ..
            } => {
                let mut composite_components = vec![];

                for (ty, offset) in field_types.iter().zip(field_offsets.iter()) {
                    let offset = offset.bytes() as u32 / 4;

                    composite_components.push(
                        self.recurse_adt_for_loads(
                            uint_ty,
                            uniform_uint_ptr,
                            bindless_idx,
                            base_offset_var,
                            element_offset_literal + offset,
                            *ty,
                            sets,
                        )
                        .def(self),
                    );
                }

                let adt = data.def(rustc_span::DUMMY_SP, self);

                self.emit()
                    .composite_construct(adt, None, composite_components)
                    .unwrap()
                    .with_type(adt)
            }
            SpirvType::Vector { count, element } => {
                let mut composite_components = vec![];

                for offset in 0..count {
                    composite_components.push(
                        self.recurse_adt_for_loads(
                            uint_ty,
                            uniform_uint_ptr,
                            bindless_idx,
                            base_offset_var,
                            element_offset_literal + offset,
                            element,
                            sets,
                        )
                        .def(self),
                    );
                }

                let adt = data.def(rustc_span::DUMMY_SP, self);

                self.emit()
                    .composite_construct(adt, None, composite_components)
                    .unwrap()
                    .with_type(adt)
            }
            SpirvType::Float(bits) => {
                let loaded_as_int = self
                    .load_from_u32(
                        bits,
                        false,
                        uint_ty,
                        uint_ty,
                        uniform_uint_ptr,
                        bindless_idx,
                        base_offset_var,
                        element_offset_literal,
                        sets,
                    )
                    .def(self);

                self.emit()
                    .bitcast(result_type, None, loaded_as_int)
                    .unwrap()
                    .with_type(result_type)
            }
            SpirvType::Integer(bits, signed) => self.load_from_u32(
                bits,
                signed,
                result_type,
                uint_ty,
                uniform_uint_ptr,
                bindless_idx,
                base_offset_var,
                element_offset_literal,
                sets,
            ),
            _ => {
                bug!(
                    "Unhandled case for `internal_buffer_load` return / args: {:?}",
                    data
                );
            }
        }
    }
}
