use super::Builder;
use crate::builder_spirv::{SpirvValue, SpirvValueExt, SpirvValueKind};
use crate::spirv_type::SpirvType;
use rspirv::spirv::Word;
use rustc_codegen_ssa::traits::{BaseTypeMethods, BuilderMethods};
use rustc_errors::ErrorGuaranteed;
use rustc_span::DUMMY_SP;
use rustc_target::abi::call::PassMode;
use rustc_target::abi::{Align, Size};

impl<'a, 'tcx> Builder<'a, 'tcx> {
    fn load_err(&mut self, original_type: Word, invalid_type: Word) -> SpirvValue {
        let mut err = self.struct_err(&format!(
            "Cannot load type {} in an untyped buffer load",
            self.debug_type(original_type)
        ));
        if original_type != invalid_type {
            err.note(&format!(
                "due to containing type {}",
                self.debug_type(invalid_type)
            ));
        }
        err.emit();
        self.undef(invalid_type)
    }

    fn load_u32(
        &mut self,
        array: SpirvValue,
        dynamic_index: SpirvValue,
        constant_offset: u32,
    ) -> SpirvValue {
        let actual_index = if constant_offset != 0 {
            let const_offset_val = self.constant_u32(DUMMY_SP, constant_offset);
            self.add(dynamic_index, const_offset_val)
        } else {
            dynamic_index
        };
        let u32_ty = SpirvType::Integer(32, false).def(DUMMY_SP, self);
        let u32_ptr = self.type_ptr_to(u32_ty);
        let ptr = self
            .emit()
            .in_bounds_access_chain(u32_ptr, None, array.def(self), [actual_index.def(self)])
            .unwrap()
            .with_type(u32_ptr);
        self.load(u32_ty, ptr, Align::ONE)
    }

    #[allow(clippy::too_many_arguments)]
    fn load_vec_mat_arr(
        &mut self,
        original_type: Word,
        result_type: Word,
        array: SpirvValue,
        dynamic_word_index: SpirvValue,
        constant_word_offset: u32,
        element: Word,
        count: u32,
    ) -> SpirvValue {
        let element_size_bytes = match self.lookup_type(element).sizeof(self) {
            Some(size) => size,
            None => return self.load_err(original_type, result_type),
        };
        if element_size_bytes.bytes() % 4 != 0 {
            return self.load_err(original_type, result_type);
        }
        let element_size_words = (element_size_bytes.bytes() / 4) as u32;
        let args = (0..count)
            .map(|index| {
                self.recurse_load_type(
                    original_type,
                    element,
                    array,
                    dynamic_word_index,
                    constant_word_offset + element_size_words * index,
                )
                .def(self)
            })
            .collect::<Vec<_>>();
        self.emit()
            .composite_construct(result_type, None, args)
            .unwrap()
            .with_type(result_type)
    }

    fn recurse_load_type(
        &mut self,
        original_type: Word,
        result_type: Word,
        array: SpirvValue,
        dynamic_word_index: SpirvValue,
        constant_word_offset: u32,
    ) -> SpirvValue {
        match self.lookup_type(result_type) {
            SpirvType::Integer(32, signed) => {
                let val = self.load_u32(array, dynamic_word_index, constant_word_offset);
                self.intcast(val, result_type, signed)
            }
            SpirvType::Float(32) => {
                let val = self.load_u32(array, dynamic_word_index, constant_word_offset);
                self.bitcast(val, result_type)
            }
            SpirvType::Vector { element, count } | SpirvType::Matrix { element, count } => self
                .load_vec_mat_arr(
                    original_type,
                    result_type,
                    array,
                    dynamic_word_index,
                    constant_word_offset,
                    element,
                    count,
                ),
            SpirvType::Array { element, count } => {
                let count = match self.builder.lookup_const_u64(count) {
                    Some(count) => count as u32,
                    None => return self.load_err(original_type, result_type),
                };
                self.load_vec_mat_arr(
                    original_type,
                    result_type,
                    array,
                    dynamic_word_index,
                    constant_word_offset,
                    element,
                    count,
                )
            }
            SpirvType::Adt {
                size: Some(_),
                field_types,
                field_offsets,
                ..
            } => {
                let args = field_types
                    .iter()
                    .zip(field_offsets)
                    .map(|(&field_type, byte_offset)| {
                        if byte_offset.bytes() % 4 != 0 {
                            return None;
                        }
                        let word_offset = (byte_offset.bytes() / 4) as u32;
                        Some(
                            self.recurse_load_type(
                                original_type,
                                field_type,
                                array,
                                dynamic_word_index,
                                constant_word_offset + word_offset,
                            )
                            .def(self),
                        )
                    })
                    .collect::<Option<Vec<_>>>();
                match args {
                    None => self.load_err(original_type, result_type),
                    Some(args) => self
                        .emit()
                        .composite_construct(result_type, None, args)
                        .unwrap()
                        .with_type(result_type),
                }
            }

            _ => self.load_err(original_type, result_type),
        }
    }

    /// Note: DOES NOT do bounds checking! Bounds checking is expected to be done in the caller.
    pub fn codegen_buffer_load_intrinsic(
        &mut self,
        result_type: Word,
        args: &[SpirvValue],
        pass_mode: &PassMode,
    ) -> SpirvValue {
        match pass_mode {
            PassMode::Ignore => {
                return SpirvValue {
                    kind: SpirvValueKind::IllegalTypeUsed(result_type),
                    ty: result_type,
                }
            }
            // PassMode::Pair is identical to PassMode::Direct - it's returned as a struct
            PassMode::Direct(_) | PassMode::Pair(_, _) => (),
            PassMode::Cast(_, _) => {
                self.fatal("PassMode::Cast not supported in codegen_buffer_load_intrinsic")
            }
            PassMode::Indirect { .. } => {
                self.fatal("PassMode::Indirect not supported in codegen_buffer_load_intrinsic")
            }
        }

        // Signature: fn load<T>(array: &[u32], index: u32) -> T;
        if args.len() != 3 {
            self.fatal(&format!(
                "buffer_load_intrinsic should have 3 args, it has {}",
                args.len()
            ));
        }
        // Note that the &[u32] gets split into two arguments - pointer, length
        let array = args[0];
        let byte_index = args[2];
        let two = self.constant_u32(DUMMY_SP, 2);
        let word_index = self.lshr(byte_index, two);
        self.recurse_load_type(result_type, result_type, array, word_index, 0)
    }

    fn store_err(&mut self, original_type: Word, value: SpirvValue) -> Result<(), ErrorGuaranteed> {
        let mut err = self.struct_err(&format!(
            "Cannot store type {} in an untyped buffer store",
            self.debug_type(original_type)
        ));
        if original_type != value.ty {
            err.note(&format!("due to containing type {}", value.ty));
        }
        Err(err.emit())
    }

    fn store_u32(
        &mut self,
        array: SpirvValue,
        dynamic_index: SpirvValue,
        constant_offset: u32,
        value: SpirvValue,
    ) -> Result<(), ErrorGuaranteed> {
        let actual_index = if constant_offset != 0 {
            let const_offset_val = self.constant_u32(DUMMY_SP, constant_offset);
            self.add(dynamic_index, const_offset_val)
        } else {
            dynamic_index
        };
        let u32_ty = SpirvType::Integer(32, false).def(DUMMY_SP, self);
        let u32_ptr = self.type_ptr_to(u32_ty);
        let ptr = self
            .emit()
            .in_bounds_access_chain(u32_ptr, None, array.def(self), [actual_index.def(self)])
            .unwrap()
            .with_type(u32_ptr);
        self.store(value, ptr, Align::ONE);
        Ok(())
    }

    #[allow(clippy::too_many_arguments)]
    fn store_vec_mat_arr(
        &mut self,
        original_type: Word,
        value: SpirvValue,
        array: SpirvValue,
        dynamic_word_index: SpirvValue,
        constant_word_offset: u32,
        element: Word,
        count: u32,
    ) -> Result<(), ErrorGuaranteed> {
        let element_size_bytes = match self.lookup_type(element).sizeof(self) {
            Some(size) => size,
            None => return self.store_err(original_type, value),
        };
        if element_size_bytes.bytes() % 4 != 0 {
            return self.store_err(original_type, value);
        }
        let element_size_words = (element_size_bytes.bytes() / 4) as u32;
        for index in 0..count {
            let element = self.extract_value(value, index as u64);
            self.recurse_store_type(
                original_type,
                element,
                array,
                dynamic_word_index,
                constant_word_offset + element_size_words * index,
            )?;
        }
        Ok(())
    }

    fn recurse_store_type(
        &mut self,
        original_type: Word,
        value: SpirvValue,
        array: SpirvValue,
        dynamic_word_index: SpirvValue,
        constant_word_offset: u32,
    ) -> Result<(), ErrorGuaranteed> {
        match self.lookup_type(value.ty) {
            SpirvType::Integer(32, signed) => {
                let u32_ty = SpirvType::Integer(32, false).def(DUMMY_SP, self);
                let value_u32 = self.intcast(value, u32_ty, signed);
                self.store_u32(array, dynamic_word_index, constant_word_offset, value_u32)
            }
            SpirvType::Float(32) => {
                let u32_ty = SpirvType::Integer(32, false).def(DUMMY_SP, self);
                let value_u32 = self.bitcast(value, u32_ty);
                self.store_u32(array, dynamic_word_index, constant_word_offset, value_u32)
            }
            SpirvType::Vector { element, count } | SpirvType::Matrix { element, count } => self
                .store_vec_mat_arr(
                    original_type,
                    value,
                    array,
                    dynamic_word_index,
                    constant_word_offset,
                    element,
                    count,
                ),
            SpirvType::Array { element, count } => {
                let count = match self.builder.lookup_const_u64(count) {
                    Some(count) => count as u32,
                    None => return self.store_err(original_type, value),
                };
                self.store_vec_mat_arr(
                    original_type,
                    value,
                    array,
                    dynamic_word_index,
                    constant_word_offset,
                    element,
                    count,
                )
            }
            SpirvType::Adt {
                size: Some(_),
                field_offsets,
                ..
            } => {
                for (index, byte_offset) in field_offsets.iter().enumerate() {
                    if byte_offset.bytes() % 4 != 0 {
                        return self.store_err(original_type, value);
                    }
                    let word_offset = (byte_offset.bytes() / 4) as u32;
                    let field = self.extract_value(value, index as u64);
                    self.recurse_store_type(
                        original_type,
                        field,
                        array,
                        dynamic_word_index,
                        constant_word_offset + word_offset,
                    )?;
                }
                Ok(())
            }

            _ => self.store_err(original_type, value),
        }
    }

    /// Note: DOES NOT do bounds checking! Bounds checking is expected to be done in the caller.
    pub fn codegen_buffer_store_intrinsic(&mut self, args: &[SpirvValue], pass_mode: &PassMode) {
        // Signature: fn store<T>(array: &[u32], index: u32, value: T);
        let is_pair = match pass_mode {
            // haha shrug
            PassMode::Ignore => return,
            PassMode::Direct(_) => false,
            PassMode::Pair(_, _) => true,
            PassMode::Cast(_, _) => {
                self.fatal("PassMode::Cast not supported in codegen_buffer_store_intrinsic")
            }
            PassMode::Indirect { .. } => {
                self.fatal("PassMode::Indirect not supported in codegen_buffer_store_intrinsic")
            }
        };
        let expected_args = if is_pair { 5 } else { 4 };
        if args.len() != expected_args {
            self.fatal(&format!(
                "buffer_store_intrinsic should have {} args, it has {}",
                expected_args,
                args.len()
            ));
        }
        // Note that the &[u32] gets split into two arguments - pointer, length
        let array = args[0];
        let byte_index = args[2];
        let two = self.constant_u32(DUMMY_SP, 2);
        let word_index = self.lshr(byte_index, two);
        if is_pair {
            let value_one = args[3];
            let value_two = args[4];
            let one_result = self.recurse_store_type(value_one.ty, value_one, array, word_index, 0);

            let size_of_one = self.lookup_type(value_one.ty).sizeof(self);
            if one_result.is_ok() && size_of_one != Some(Size::from_bytes(4)) {
                self.fatal("Expected PassMode::Pair first element to have size 4");
            }

            let _ = self.recurse_store_type(value_two.ty, value_two, array, word_index, 1);
        } else {
            let value = args[3];
            let _ = self.recurse_store_type(value.ty, value, array, word_index, 0);
        }
    }
}
