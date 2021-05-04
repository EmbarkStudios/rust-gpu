use super::Builder;
use crate::builder_spirv::{SpirvValue, SpirvValueExt};
use crate::codegen_cx::BindlessDescriptorSets;
use crate::rustc_codegen_ssa::traits::BuilderMethods;
use crate::spirv_type::SpirvType;
use rspirv::spirv::Word;
use rustc_target::abi::Align;
use std::convert::TryInto;

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
                ref field_names,
                ..
            } => {
                for (element_idx, (_ty, offset)) in
                    field_types.iter().zip(field_offsets.iter()).enumerate()
                {
                    let load_res = self.extract_value(val, element_idx as u64);

                    if offset.bytes() as u32 % 4 != 0 {
                        let adt_name = self.type_cache.lookup_name(val.ty);
                        let field_name = if let Some(field_names) = field_names {
                            &field_names[element_idx]
                        } else {
                            "<unknown>"
                        };

                        self.err(&format!(
                            "Trying to store to unaligned field: `{}::{}`. Field must be aligned to multiple of 4 bytes, but has offset {}",
                            adt_name,
                            field_name,
                            offset.bytes() as u32));
                    }

                    let offset = offset.bytes() as u32 / 4;

                    self.recurse_adt_for_stores(
                        uint_ty,
                        load_res,
                        base_offset + offset,
                        uint_values_and_offsets,
                    );
                }
            }
            SpirvType::Vector { count, element: _ } => {
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
            SpirvType::Array { element: _, count } => {
                let count = self
                    .cx
                    .builder
                    .lookup_const_u64(count)
                    .expect("Array type has invalid count value");

                for offset in 0..count {
                    let load_res = self.extract_value(val, offset);
                    let offset : u32 = offset.try_into().expect("Array count needs to fit in u32");

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
            SpirvType::Void => self.err("Type () unsupported for bindless buffer stores"),
            SpirvType::Bool => self.err("Type bool unsupported for bindless buffer stores"),
            SpirvType::Opaque { ref name } => self.err(&format!("Opaque type {} unsupported for bindless buffer stores", name)),
            SpirvType::RuntimeArray { element: _ } =>
                self.err("Type `RuntimeArray` unsupported for bindless buffer stores"),
            SpirvType::Pointer { pointee: _ } =>
                self.err("Pointer type unsupported for bindless buffer stores"),
            SpirvType::Function {
                return_type: _,
                arguments: _,
            } => self.err("Function type unsupported for bindless buffer stores"),
            SpirvType::Image {
                sampled_type: _,
                dim: _,
                depth: _,
                arrayed: _,
                multisampled: _,
                sampled: _,
                image_format: _,
                access_qualifier: _,
            } => self.err("Image type unsupported for bindless buffer stores (use a bindless Texture type instead)"),
            SpirvType::Sampler => self.err("Sampler type unsupported for bindless buffer stores"),
            SpirvType::SampledImage { image_type: _ }  => self.err("SampledImage type unsupported for bindless buffer stores"),
            SpirvType::InterfaceBlock { inner_type: _ } => self.err("InterfaceBlock type unsupported for bindless buffer stores"),
            SpirvType::AccelerationStructureKhr => self.fatal("AccelerationStructureKhr type unsupported for bindless buffer stores"),
            SpirvType::RayQueryKhr => self.fatal("RayQueryKhr type unsupported for bindless buffer stores"),
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

                // note: assumes little endian
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
            _ => {
                let mut err = self
                    .tcx
                    .sess
                    .struct_err("Unsupported integer type for `codegen_internal_buffer_store`");
                err.note(&format!("bits: `{:?}`", bits));
                err.note(&format!("signed: `{:?}`", signed));
                err.emit();
            }
        }
    }

    pub(crate) fn codegen_internal_buffer_store(&mut self, args: &[SpirvValue]) {
        if !self.bindless() {
            self.fatal("Need to run the compiler with -Ctarget-feature=+bindless to be able to use the bindless features");
        }

        let uint_ty = SpirvType::Integer(32, false).def(rustc_span::DUMMY_SP, self);

        let uniform_uint_ptr =
            SpirvType::Pointer { pointee: uint_ty }.def(rustc_span::DUMMY_SP, self);

        let zero = self.constant_int(uint_ty, 0).def(self);

        let sets = self.bindless_descriptor_sets.borrow().unwrap();

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
    }

    pub(crate) fn codegen_internal_buffer_load(
        &mut self,
        result_type: Word,
        args: &[SpirvValue],
    ) -> SpirvValue {
        if !self.bindless() {
            self.fatal("Need to run the compiler with -Ctarget-feature=+bindless to be able to use the bindless features");
        }

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

    #[allow(clippy::too_many_arguments)]
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
                // note: assumes little endian
                // lower = u64(base[0])
                // upper = u64(base[1])
                // result = lower | (upper << 32)
                let ulong_ty = SpirvType::Integer(64, false).def(rustc_span::DUMMY_SP, self);

                let lower = self
                    .emit()
                    .load(uint_ty, None, result, None, std::iter::empty())
                    .unwrap();

                let lower = self.emit().u_convert(ulong_ty, None, lower).unwrap();

                let const_one = self.constant_int(uint_ty, 1u64).def(self);

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
            _ => self.fatal(&format!(
                "Trying to load invalid data type: {}{}",
                if signed { "i" } else { "u" },
                bits
            )),
        }
    }

    #[allow(clippy::too_many_arguments)]
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
                ref field_names,
                def_id: _,
                ..
            } => {
                let mut composite_components = vec![];

                for (idx, (ty, offset)) in field_types.iter().zip(field_offsets.iter()).enumerate()
                {
                    if offset.bytes() as u32 % 4 != 0 {
                        let adt_name = self.type_cache.lookup_name(result_type);
                        let field_name = if let Some(field_names) = field_names {
                            &field_names[idx]
                        } else {
                            "<unknown>"
                        };

                        self.fatal(&format!(
                            "Trying to load from unaligned field: `{}::{}`. Field must be aligned to multiple of 4 bytes, but has offset {}",
                            adt_name,
                            field_name,
                            offset.bytes() as u32));
                    }

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
            SpirvType::Array { element, count } => {
                let count = self
                    .cx
                    .builder
                    .lookup_const_u64(count)
                    .expect("Array type has invalid count value");

                let mut composite_components = vec![];

                for offset in 0..count {
                    let offset : u32 = offset.try_into().expect("Array count needs to fit in u32");

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
            SpirvType::Void => self.fatal("Type () unsupported for bindless buffer loads"),
            SpirvType::Bool => self.fatal("Type bool unsupported for bindless buffer loads"),
            SpirvType::Opaque { ref name } => self.fatal(&format!("Opaque type {} unsupported for bindless buffer loads", name)),
            SpirvType::RuntimeArray { element: _ } =>
                self.fatal("Type `RuntimeArray` unsupported for bindless buffer loads"),
            SpirvType::Pointer { pointee: _ } =>
                self.fatal("Pointer type unsupported for bindless buffer loads"),
            SpirvType::Function {
                return_type: _,
                arguments: _,
            } => self.fatal("Function type unsupported for bindless buffer loads"),
            SpirvType::Image {
                sampled_type: _,
                dim: _,
                depth: _,
                arrayed: _,
                multisampled: _,
                sampled: _,
                image_format: _,
                access_qualifier: _,
            } => self.fatal("Image type unsupported for bindless buffer loads (use a bindless Texture type instead)"),
            SpirvType::Sampler => self.fatal("Sampler type unsupported for bindless buffer loads"),
            SpirvType::SampledImage { image_type: _ }  => self.fatal("SampledImage type unsupported for bindless buffer loads"),
            SpirvType::InterfaceBlock { inner_type: _ } => self.fatal("InterfaceBlock type unsupported for bindless buffer loads"),
            SpirvType::AccelerationStructureKhr => self.fatal("AccelerationStructureKhr type unsupported for bindless buffer loads"),
            SpirvType::RayQueryKhr => self.fatal("RayQueryKhr type unsupported for bindless buffer loads"),
        }
    }
}
