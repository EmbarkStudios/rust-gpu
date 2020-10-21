use super::Builder;
use crate::builder_spirv::{BuilderCursor, SpirvValue, SpirvValueExt};
use crate::spirv_type::SpirvType;
use rspirv::dr::{InsertPoint, Instruction, Operand};
use rspirv::spirv::{AddressingModel, Capability, MemorySemantics, Op, Scope, StorageClass, Word};
use rustc_codegen_ssa::common::{
    AtomicOrdering, AtomicRmwBinOp, IntPredicate, RealPredicate, SynchronizationScope,
};
use rustc_codegen_ssa::mir::operand::{OperandRef, OperandValue};
use rustc_codegen_ssa::mir::place::PlaceRef;
use rustc_codegen_ssa::traits::{BuilderMethods, ConstMethods, LayoutTypeMethods, OverflowOp};
use rustc_codegen_ssa::MemFlags;
use rustc_middle::ty::Ty;
use rustc_span::Span;
use rustc_target::abi::{Abi, Align, Scalar, Size};
use std::iter::empty;
use std::ops::Range;

macro_rules! simple_op {
    ($func_name:ident, $inst_name:ident) => {
        fn $func_name(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
            assert_ty_eq!(self, lhs.ty, rhs.ty);
            let result_type = lhs.ty;
            self.emit()
                .$inst_name(result_type, None, lhs.def, rhs.def)
                .unwrap()
                .with_type(result_type)
        }
    };
}

// shl and shr allow different types as their operands
macro_rules! simple_op_unchecked_type {
    ($func_name:ident, $inst_name:ident) => {
        fn $func_name(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
            self.emit()
                .$inst_name(lhs.ty, None, lhs.def, rhs.def)
                .unwrap()
                .with_type(lhs.ty)
        }
    };
}

macro_rules! simple_uni_op {
    ($func_name:ident, $inst_name:ident) => {
        fn $func_name(&mut self, val: Self::Value) -> Self::Value {
            self.emit()
                .$inst_name(val.ty, None, val.def)
                .unwrap()
                .with_type(val.ty)
        }
    };
}

fn ordering_to_semantics(ordering: AtomicOrdering) -> MemorySemantics {
    use AtomicOrdering::*;
    // TODO: Someone verify/fix this, I don't know atomics well
    match ordering {
        NotAtomic => MemorySemantics::NONE,
        Unordered => MemorySemantics::NONE,
        Monotonic => MemorySemantics::NONE,
        Acquire => MemorySemantics::ACQUIRE,
        Release => MemorySemantics::RELEASE,
        AcquireRelease => MemorySemantics::ACQUIRE_RELEASE,
        SequentiallyConsistent => MemorySemantics::SEQUENTIALLY_CONSISTENT,
    }
}

fn memset_fill_u16(b: u8) -> u16 {
    b as u16 | ((b as u16) << 8)
}

fn memset_fill_u32(b: u8) -> u32 {
    b as u32 | ((b as u32) << 8) | ((b as u32) << 16) | ((b as u32) << 24)
}

fn memset_fill_u64(b: u8) -> u64 {
    b as u64
        | ((b as u64) << 8)
        | ((b as u64) << 16)
        | ((b as u64) << 24)
        | ((b as u64) << 32)
        | ((b as u64) << 40)
        | ((b as u64) << 48)
        | ((b as u64) << 56)
}

fn memset_dynamic_scalar(
    builder: &Builder<'_, '_>,
    fill_var: Word,
    byte_width: usize,
    is_float: bool,
) -> Word {
    let composite_type = SpirvType::Vector {
        element: SpirvType::Integer(8, false).def(builder),
        count: byte_width as u32,
    }
    .def(builder);
    let composite = builder
        .emit()
        .composite_construct(
            composite_type,
            None,
            std::iter::repeat(fill_var).take(byte_width),
        )
        .unwrap();
    let result_type = if is_float {
        SpirvType::Float(byte_width as u32 * 8)
    } else {
        SpirvType::Integer(byte_width as u32 * 8, false)
    };
    builder
        .emit()
        .bitcast(result_type.def(builder), None, composite)
        .unwrap()
}

impl<'a, 'tcx> Builder<'a, 'tcx> {
    fn memset_const_pattern(&self, ty: &SpirvType, fill_byte: u8) -> Word {
        match *ty {
            SpirvType::Void => self.fatal("memset invalid on void pattern"),
            SpirvType::Bool => self.fatal("memset invalid on bool pattern"),
            SpirvType::Integer(width, _signedness) => match width {
                8 => self.constant_u8(fill_byte).def,
                16 => self.constant_u16(memset_fill_u16(fill_byte)).def,
                32 => self.constant_u32(memset_fill_u32(fill_byte)).def,
                64 => self.constant_u64(memset_fill_u64(fill_byte)).def,
                _ => self.fatal(&format!(
                    "memset on integer width {} not implemented yet",
                    width
                )),
            },
            SpirvType::Float(width) => match width {
                32 => {
                    self.constant_f32(f32::from_bits(memset_fill_u32(fill_byte)))
                        .def
                }
                64 => {
                    self.constant_f64(f64::from_bits(memset_fill_u64(fill_byte)))
                        .def
                }
                _ => self.fatal(&format!(
                    "memset on float width {} not implemented yet",
                    width
                )),
            },
            SpirvType::Adt { .. } => self.fatal("memset on structs not implemented yet"),
            SpirvType::Opaque { .. } => self.fatal("memset on opaque type is invalid"),
            SpirvType::Vector { element, count } => {
                let elem_pat = self.memset_const_pattern(&self.lookup_type(element), fill_byte);
                self.constant_composite(ty.clone().def(self), vec![elem_pat; count as usize])
                    .def
            }
            SpirvType::Array { element, count } => {
                let elem_pat = self.memset_const_pattern(&self.lookup_type(element), fill_byte);
                let count = self.builder.lookup_const_u64(count).unwrap() as usize;
                self.constant_composite(ty.clone().def(self), vec![elem_pat; count])
                    .def
            }
            SpirvType::RuntimeArray { .. } => {
                self.fatal("memset on runtime arrays not implemented yet")
            }
            SpirvType::Pointer { .. } => self.fatal("memset on pointers not implemented yet"),
            SpirvType::Function { .. } => self.fatal("memset on functions not implemented yet"),
        }
    }

    fn memset_dynamic_pattern(&self, ty: &SpirvType, fill_var: Word) -> Word {
        match *ty {
            SpirvType::Void => self.fatal("memset invalid on void pattern"),
            SpirvType::Bool => self.fatal("memset invalid on bool pattern"),
            SpirvType::Integer(width, _signedness) => match width {
                8 => fill_var,
                16 => memset_dynamic_scalar(self, fill_var, 2, false),
                32 => memset_dynamic_scalar(self, fill_var, 4, false),
                64 => memset_dynamic_scalar(self, fill_var, 8, false),
                _ => self.fatal(&format!(
                    "memset on integer width {} not implemented yet",
                    width
                )),
            },
            SpirvType::Float(width) => match width {
                32 => memset_dynamic_scalar(self, fill_var, 4, true),
                64 => memset_dynamic_scalar(self, fill_var, 8, true),
                _ => self.fatal(&format!(
                    "memset on float width {} not implemented yet",
                    width
                )),
            },
            SpirvType::Adt { .. } => self.fatal("memset on structs not implemented yet"),
            SpirvType::Opaque { .. } => self.fatal("memset on opaque type is invalid"),
            SpirvType::Array { element, count } => {
                let elem_pat = self.memset_dynamic_pattern(&self.lookup_type(element), fill_var);
                let count = self.builder.lookup_const_u64(count).unwrap() as usize;
                self.emit()
                    .composite_construct(
                        ty.clone().def(self),
                        None,
                        std::iter::repeat(elem_pat).take(count),
                    )
                    .unwrap()
            }
            SpirvType::Vector { element, count } => {
                let elem_pat = self.memset_dynamic_pattern(&self.lookup_type(element), fill_var);
                self.emit()
                    .composite_construct(
                        ty.clone().def(self),
                        None,
                        std::iter::repeat(elem_pat).take(count as usize),
                    )
                    .unwrap()
            }
            SpirvType::RuntimeArray { .. } => {
                self.fatal("memset on runtime arrays not implemented yet")
            }
            SpirvType::Pointer { .. } => self.fatal("memset on pointers not implemented yet"),
            SpirvType::Function { .. } => self.fatal("memset on functions not implemented yet"),
        }
    }

    fn memset_constant_size(&mut self, ptr: SpirvValue, pat: SpirvValue, size_bytes: u64) {
        let size_elem = self
            .lookup_type(pat.ty)
            .sizeof(self)
            .expect("Memset on unsized values not supported");
        let count = size_bytes / size_elem.bytes();
        if count == 1 {
            self.store(pat, ptr, Align::from_bytes(0).unwrap());
        } else {
            for index in 0..count {
                let const_index = self.constant_u32(index as u32);
                let gep_ptr = self.gep(ptr, &[const_index]);
                self.store(pat, gep_ptr, Align::from_bytes(0).unwrap());
            }
        }
    }

    // TODO: Test this is correct
    fn memset_dynamic_size(&mut self, ptr: SpirvValue, pat: SpirvValue, size_bytes: SpirvValue) {
        let size_elem = self
            .lookup_type(pat.ty)
            .sizeof(self)
            .expect("Unable to memset a dynamic sized object");
        let size_elem_const = self.constant_int(size_bytes.ty, size_elem.bytes());
        let zero = self.constant_int(size_bytes.ty, 0);
        let one = self.constant_int(size_bytes.ty, 1);
        let zero_align = Align::from_bytes(0).unwrap();

        let mut header = self.build_sibling_block("memset_header");
        let mut body = self.build_sibling_block("memset_body");
        let exit = self.build_sibling_block("memset_exit");

        let count = self.udiv(size_bytes, size_elem_const);
        let index = self.alloca(count.ty, zero_align);
        self.store(zero, index, zero_align);
        self.br(header.llbb());

        let current_index = header.load(index, zero_align);
        let cond = header.icmp(IntPredicate::IntULT, current_index, count);
        header.cond_br(cond, body.llbb(), exit.llbb());

        let gep_ptr = body.gep(ptr, &[current_index]);
        body.store(pat, gep_ptr, zero_align);
        let current_index_plus_1 = body.add(current_index, one);
        body.store(current_index_plus_1, index, zero_align);
        body.br(header.llbb());

        *self = exit;
    }

    fn zombie_convert_ptr_to_u(&self, def: Word) {
        if !self.builder.has_capability(Capability::Addresses)
            && !self
                .builder
                .has_capability(Capability::PhysicalStorageBufferAddresses)
        {
            self.zombie(
                def,
                "OpConvertPtrToU without OpCapability Addresses or PhysicalStorageBufferAddresses",
            );
        }
    }

    fn zombie_convert_u_to_ptr(&self, def: Word) {
        if !self.builder.has_capability(Capability::Addresses)
            && !self
                .builder
                .has_capability(Capability::PhysicalStorageBufferAddresses)
        {
            self.zombie(
                def,
                "OpConvertUToPtr OpCapability Addresses or PhysicalStorageBufferAddresses",
            );
        }
    }

    fn zombie_bitcast_ptr(&self, def: Word) {
        let is_logical = self
            .emit()
            .module_ref()
            .memory_model
            .as_ref()
            .map_or(false, |inst| {
                inst.operands[0].unwrap_addressing_model() == AddressingModel::Logical
            });
        if is_logical {
            self.zombie(def, "OpBitcast on ptr without AddressingModel != Logical")
        }
    }

    // Sometimes, when accessing the first field of a struct, vector, etc., instead of calling
    // struct_gep, codegen_ssa will call pointercast. This will then try to catch those cases and
    // translate them back to a struct_gep, instead of failing to compile the OpBitcast (which is
    // unsupported on shader target)
    fn try_pointercast_via_gep(&self, mut val: Word, field: Word) -> Option<Vec<u32>> {
        let mut indices = Vec::new();
        while val != field {
            match self.lookup_type(val) {
                SpirvType::Adt {
                    field_types,
                    field_offsets,
                    ..
                } => {
                    let index = field_offsets.iter().position(|&off| off == Size::ZERO)?;
                    indices.push(index as u32);
                    val = field_types[index];
                }
                SpirvType::Vector { element, .. }
                | SpirvType::Array { element, .. }
                | SpirvType::RuntimeArray { element } => {
                    indices.push(0);
                    val = element;
                }
                _ => return None,
            }
        }
        Some(indices)
    }
}

impl<'a, 'tcx> BuilderMethods<'a, 'tcx> for Builder<'a, 'tcx> {
    fn new_block<'b>(cx: &'a Self::CodegenCx, llfn: Self::Function, _name: &'b str) -> Self {
        let cursor_fn = cx.builder.select_function_by_id(llfn.def);
        let label = cx.emit_with_cursor(cursor_fn).begin_block(None).unwrap();
        let cursor = cx.builder.select_block_by_id(label);
        Self {
            cx,
            cursor,
            current_fn: llfn,
            basic_block: label,
            current_span: Default::default(),
        }
    }

    fn with_cx(cx: &'a Self::CodegenCx) -> Self {
        // Note: all defaults here *must* be filled out by position_at_end
        Self {
            cx,
            cursor: Default::default(),
            current_fn: Default::default(),
            basic_block: Default::default(),
            current_span: Default::default(),
        }
    }

    fn build_sibling_block(&self, _name: &str) -> Self {
        let mut builder = self.emit_with_cursor(BuilderCursor {
            function: self.cursor.function,
            block: None,
        });
        let new_bb = builder.begin_block(None).unwrap();
        let new_cursor = BuilderCursor {
            function: self.cursor.function,
            block: builder.selected_block(),
        };
        Self {
            cx: self.cx,
            cursor: new_cursor,
            current_fn: self.current_fn,
            basic_block: new_bb,
            current_span: Default::default(),
        }
    }

    fn cx(&self) -> &Self::CodegenCx {
        self.cx
    }

    fn llbb(&self) -> Self::BasicBlock {
        self.basic_block
    }

    fn set_span(&mut self, span: Span) {
        self.current_span = Some(span);
    }

    fn position_at_end(&mut self, llbb: Self::BasicBlock) {
        let cursor = self.cx.builder.select_block_by_id(llbb);
        let current_fn = {
            let emit = self.emit_with_cursor(cursor);
            let selected_function = emit.selected_function().unwrap();
            let selected_function = &emit.module_ref().functions[selected_function];
            let def_inst = selected_function.def.as_ref().unwrap();
            let def = def_inst.result_id.unwrap();
            let ty = def_inst.operands[1].unwrap_id_ref();
            def.with_type(ty)
        };
        self.cursor = cursor;
        self.current_fn = current_fn;
        self.basic_block = llbb;
    }

    fn ret_void(&mut self) {
        self.emit().ret().unwrap();
    }

    fn ret(&mut self, value: Self::Value) {
        self.emit().ret_value(value.def).unwrap();
    }

    fn br(&mut self, dest: Self::BasicBlock) {
        if !self.kernel_mode && self.basic_block == dest {
            // TODO: Remove once structurizer is done.
            self.zombie_even_in_user_code(dest, "Infinite loop before structurizer is done");
        }
        self.emit().branch(dest).unwrap()
    }

    fn cond_br(
        &mut self,
        cond: Self::Value,
        then_llbb: Self::BasicBlock,
        else_llbb: Self::BasicBlock,
    ) {
        self.emit()
            .branch_conditional(cond.def, then_llbb, else_llbb, empty())
            .unwrap()
    }

    fn switch(
        &mut self,
        v: Self::Value,
        else_llbb: Self::BasicBlock,
        cases: impl ExactSizeIterator<Item = (u128, Self::BasicBlock)>,
    ) {
        if !self.kernel_mode {
            // TODO: Remove once structurizer is done.
            self.zombie(else_llbb, "OpSwitch before structurizer is done");
        }

        fn construct_8(self_: &Builder<'_, '_>, signed: bool, v: u128) -> Operand {
            if v > u8::MAX as u128 {
                self_.fatal(&format!(
                    "Switches to values above u8::MAX not supported: {:?}",
                    v
                ))
            } else if signed {
                // this cast chain can probably be collapsed, but, whatever, be safe
                Operand::LiteralInt32(v as u8 as i8 as i32 as u32)
            } else {
                Operand::LiteralInt32(v as u8 as u32)
            }
        }
        fn construct_16(self_: &Builder<'_, '_>, signed: bool, v: u128) -> Operand {
            if v > u16::MAX as u128 {
                self_.fatal(&format!(
                    "Switches to values above u16::MAX not supported: {:?}",
                    v
                ))
            } else if signed {
                Operand::LiteralInt32(v as u16 as i16 as i32 as u32)
            } else {
                Operand::LiteralInt32(v as u16 as u32)
            }
        }
        fn construct_32(self_: &Builder<'_, '_>, _signed: bool, v: u128) -> Operand {
            if v > u32::MAX as u128 {
                self_.fatal(&format!(
                    "Switches to values above u32::MAX not supported: {:?}",
                    v
                ))
            } else {
                Operand::LiteralInt32(v as u32)
            }
        }
        fn construct_64(self_: &Builder<'_, '_>, _signed: bool, v: u128) -> Operand {
            if v > u64::MAX as u128 {
                self_.fatal(&format!(
                    "Switches to values above u64::MAX not supported: {:?}",
                    v
                ))
            } else {
                Operand::LiteralInt64(v as u64)
            }
        }
        // pass in signed into the closure to be able to unify closure types
        let (signed, construct_case) = match self.lookup_type(v.ty) {
            SpirvType::Integer(width, signed) => {
                let construct_case = match width {
                    8 => construct_8,
                    16 => construct_16,
                    32 => construct_32,
                    64 => construct_64,
                    other => self.fatal(&format!(
                        "switch selector cannot have width {} (only 8, 16, 32, and 64 bits allowed)",
                        other
                    )),
                };
                (signed, construct_case)
            }
            other => self.fatal(&format!(
                "switch selector cannot have non-integer type {}",
                other.debug(v.ty, self)
            )),
        };
        let cases = cases
            .map(|(i, b)| (construct_case(self, signed, i), b))
            .collect::<Vec<_>>();
        self.emit().switch(v.def, else_llbb, cases).unwrap()
    }

    fn invoke(
        &mut self,
        llfn: Self::Value,
        args: &[Self::Value],
        then: Self::BasicBlock,
        _catch: Self::BasicBlock,
        funclet: Option<&Self::Funclet>,
    ) -> Self::Value {
        // Exceptions don't exist, jump directly to then block
        let result = self.call(llfn, args, funclet);
        self.emit().branch(then).unwrap();
        result
    }

    fn unreachable(&mut self) {
        self.emit().unreachable().unwrap()
    }

    simple_op! {add, i_add}
    simple_op! {fadd, f_add}
    simple_op! {fadd_fast, f_add} // fast=normal
    simple_op! {sub, i_sub}
    simple_op! {fsub, f_sub}
    simple_op! {fsub_fast, f_sub} // fast=normal
    simple_op! {mul, i_mul}
    simple_op! {fmul, f_mul}
    simple_op! {fmul_fast, f_mul} // fast=normal
    simple_op! {udiv, u_div}
    simple_op! {exactudiv, u_div} // ignore
    simple_op! {sdiv, s_div}
    simple_op! {exactsdiv, s_div} // ignore
    simple_op! {fdiv, f_div}
    simple_op! {fdiv_fast, f_div} // fast=normal
    simple_op! {urem, u_mod}
    simple_op! {srem, s_rem}
    simple_op! {frem, f_rem}
    simple_op! {frem_fast, f_rem} // fast=normal
    simple_op_unchecked_type! {shl, shift_left_logical}
    simple_op_unchecked_type! {lshr, shift_right_logical}
    simple_op_unchecked_type! {ashr, shift_right_arithmetic}
    simple_op! {unchecked_sadd, i_add} // already unchecked by default
    simple_op! {unchecked_uadd, i_add} // already unchecked by default
    simple_op! {unchecked_ssub, i_sub} // already unchecked by default
    simple_op! {unchecked_usub, i_sub} // already unchecked by default
    simple_op! {unchecked_smul, i_mul} // already unchecked by default
    simple_op! {unchecked_umul, i_mul} // already unchecked by default
    simple_uni_op! {neg, s_negate}
    simple_uni_op! {fneg, f_negate}

    fn and(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        assert_ty_eq!(self, lhs.ty, rhs.ty);
        let ty = lhs.ty;
        match self.lookup_type(ty) {
            SpirvType::Integer(_, _) => self.emit().bitwise_and(ty, None, lhs.def, rhs.def),
            SpirvType::Bool => self.emit().logical_and(ty, None, lhs.def, rhs.def),
            o => self.fatal(&format!(
                "and() not implemented for type {}",
                o.debug(ty, self)
            )),
        }
        .unwrap()
        .with_type(ty)
    }
    fn or(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        assert_ty_eq!(self, lhs.ty, rhs.ty);
        let ty = lhs.ty;
        match self.lookup_type(ty) {
            SpirvType::Integer(_, _) => self.emit().bitwise_or(ty, None, lhs.def, rhs.def),
            SpirvType::Bool => self.emit().logical_or(ty, None, lhs.def, rhs.def),
            o => self.fatal(&format!(
                "or() not implemented for type {}",
                o.debug(ty, self)
            )),
        }
        .unwrap()
        .with_type(ty)
    }
    fn xor(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        assert_ty_eq!(self, lhs.ty, rhs.ty);
        let ty = lhs.ty;
        match self.lookup_type(ty) {
            SpirvType::Integer(_, _) => self.emit().bitwise_xor(ty, None, lhs.def, rhs.def),
            SpirvType::Bool => self.emit().logical_not_equal(ty, None, lhs.def, rhs.def),
            o => self.fatal(&format!(
                "xor() not implemented for type {}",
                o.debug(ty, self)
            )),
        }
        .unwrap()
        .with_type(ty)
    }
    fn not(&mut self, val: Self::Value) -> Self::Value {
        match self.lookup_type(val.ty) {
            SpirvType::Integer(_, _) => self.emit().not(val.ty, None, val.def),
            SpirvType::Bool => {
                let true_ = self.constant_bool(true);
                // intel-compute-runtime doesn't like OpLogicalNot
                self.emit()
                    .logical_not_equal(val.ty, None, val.def, true_.def)
            }
            o => self.fatal(&format!(
                "not() not implemented for type {}",
                o.debug(val.ty, self)
            )),
        }
        .unwrap()
        .with_type(val.ty)
    }

    fn checked_binop(
        &mut self,
        oop: OverflowOp,
        _ty: Ty<'_>,
        lhs: Self::Value,
        rhs: Self::Value,
    ) -> (Self::Value, Self::Value) {
        let fals = self.constant_bool(false);
        match oop {
            OverflowOp::Add => (self.add(lhs, rhs), fals),
            OverflowOp::Sub => (self.sub(lhs, rhs), fals),
            OverflowOp::Mul => (self.mul(lhs, rhs), fals),
        }
    }

    fn from_immediate(&mut self, val: Self::Value) -> Self::Value {
        if self.lookup_type(val.ty) == SpirvType::Bool {
            let i8 = SpirvType::Integer(8, false).def(self);
            self.zext(val, i8)
        } else {
            val
        }
    }

    fn to_immediate_scalar(&mut self, val: Self::Value, scalar: &Scalar) -> Self::Value {
        if scalar.is_bool() {
            let bool = SpirvType::Bool.def(self);
            return self.trunc(val, bool);
        }
        val
    }

    fn alloca(&mut self, ty: Self::Type, _align: Align) -> Self::Value {
        let ptr_ty = SpirvType::Pointer {
            storage_class: StorageClass::Function,
            pointee: ty,
        }
        .def(self);
        // "All OpVariable instructions in a function must be the first instructions in the first block."
        let mut builder = self.emit();
        builder.select_block(Some(0)).unwrap();
        let index = {
            let block = &builder.module_ref().functions[builder.selected_function().unwrap()]
                .blocks[builder.selected_block().unwrap()];
            block
                .instructions
                .iter()
                .enumerate()
                .find_map(|(index, inst)| {
                    if inst.class.opcode != Op::Variable {
                        Some(InsertPoint::FromBegin(index))
                    } else {
                        None
                    }
                })
                .unwrap_or(InsertPoint::End)
        };
        // TODO: rspirv doesn't have insert_variable function
        let result_id = builder.id();
        let inst = Instruction::new(
            Op::Variable,
            Some(ptr_ty),
            Some(result_id),
            vec![Operand::StorageClass(StorageClass::Function)],
        );
        builder.insert_into_block(index, inst).unwrap();
        result_id.with_type(ptr_ty)
    }

    fn dynamic_alloca(&mut self, ty: Self::Type, align: Align) -> Self::Value {
        self.alloca(ty, align)
    }

    fn array_alloca(&mut self, _ty: Self::Type, _len: Self::Value, _align: Align) -> Self::Value {
        self.fatal("TODO: array_alloca not supported yet")
    }

    fn load(&mut self, ptr: Self::Value, _align: Align) -> Self::Value {
        let ty = match self.lookup_type(ptr.ty) {
            SpirvType::Pointer {
                storage_class: _,
                pointee,
            } => pointee,
            ty => self.fatal(&format!(
                "load called on variable that wasn't a pointer: {:?}",
                ty
            )),
        };
        self.emit()
            .load(ty, None, ptr.def, None, empty())
            .unwrap()
            .with_type(ty)
    }

    fn volatile_load(&mut self, ptr: Self::Value) -> Self::Value {
        // TODO: Can we do something here?
        self.load(ptr, Align::from_bytes(0).unwrap())
    }

    fn atomic_load(&mut self, ptr: Self::Value, order: AtomicOrdering, _size: Size) -> Self::Value {
        let ty = match self.lookup_type(ptr.ty) {
            SpirvType::Pointer {
                storage_class: _,
                pointee,
            } => pointee,
            ty => self.fatal(&format!(
                "atomic_load called on variable that wasn't a pointer: {:?}",
                ty
            )),
        };
        // TODO: Default to device scope
        let memory = self.constant_u32(Scope::Device as u32);
        let semantics = self.constant_u32(ordering_to_semantics(order).bits());
        let result = self
            .emit()
            .atomic_load(ty, None, ptr.def, memory.def, semantics.def)
            .unwrap()
            .with_type(ty);
        self.validate_atomic(ty, result.def);
        result
    }

    fn load_operand(
        &mut self,
        place: PlaceRef<'tcx, Self::Value>,
    ) -> OperandRef<'tcx, Self::Value> {
        if place.layout.is_zst() {
            return OperandRef::new_zst(self, place.layout);
        }

        let val = if let Some(llextra) = place.llextra {
            OperandValue::Ref(place.llval, Some(llextra), place.align)
        } else if self.cx.is_backend_immediate(place.layout) {
            let llval = self.load(place.llval, place.align);
            OperandValue::Immediate(self.to_immediate(llval, place.layout))
        } else if let Abi::ScalarPair(ref a, ref b) = place.layout.abi {
            let b_offset = a.value.size(self).align_to(b.value.align(self).abi);

            let mut load = |i, scalar: &Scalar, align| {
                let llptr = self.struct_gep(place.llval, i as u64);
                let load = self.load(llptr, align);
                // WARN! This does not go through to_immediate due to only having a Scalar, not a Ty, but it still does
                // whatever to_immediate does!
                if scalar.is_bool() {
                    self.trunc(load, SpirvType::Bool.def(self))
                } else {
                    load
                }
            };

            OperandValue::Pair(
                load(0, a, place.align),
                load(1, b, place.align.restrict_for_offset(b_offset)),
            )
        } else {
            OperandValue::Ref(place.llval, None, place.align)
        };
        OperandRef {
            val,
            layout: place.layout,
        }
    }

    /// Called for `Rvalue::Repeat` when the elem is neither a ZST nor optimizable using memset.
    fn write_operand_repeatedly(
        mut self,
        cg_elem: OperandRef<'tcx, Self::Value>,
        count: u64,
        dest: PlaceRef<'tcx, Self::Value>,
    ) -> Self {
        let zero = self.const_usize(0);
        let start = dest.project_index(&mut self, zero).llval;

        let align = dest
            .align
            .restrict_for_offset(dest.layout.field(self.cx(), 0).size);

        for i in 0..count {
            let current = self.inbounds_gep(start, &[self.const_usize(i)]);
            cg_elem.val.store(
                &mut self,
                PlaceRef::new_sized_aligned(current, cg_elem.layout, align),
            );
        }

        self
        /*
        let zero = self.const_usize(0);
        let count = self.const_usize(count);
        let start = dest.project_index(&mut self, zero).llval;
        let end = dest.project_index(&mut self, count).llval;

        let mut header_bx = self.build_sibling_block("repeat_loop_header");
        let mut body_bx = self.build_sibling_block("repeat_loop_body");
        let next_bx = self.build_sibling_block("repeat_loop_next");

        self.br(header_bx.llbb());
        let current = header_bx.phi(start.ty, &[start], &[self.llbb()]);

        let keep_going = header_bx.icmp(IntPredicate::IntNE, current, end);
        header_bx.cond_br(keep_going, body_bx.llbb(), next_bx.llbb());

        let align = dest
            .align
            .restrict_for_offset(dest.layout.field(self.cx(), 0).size);
        cg_elem.val.store(
            &mut body_bx,
            PlaceRef::new_sized_aligned(current, cg_elem.layout, align),
        );

        let next = body_bx.inbounds_gep(current, &[self.const_usize(1)]);
        body_bx.br(header_bx.llbb());
        header_bx.add_incoming_to_phi(current, next, body_bx.llbb());

        next_bx
        */
    }

    fn range_metadata(&mut self, _load: Self::Value, _range: Range<u128>) {
        // ignore
    }

    fn nonnull_metadata(&mut self, _load: Self::Value) {
        // ignore
    }

    fn store(&mut self, val: Self::Value, ptr: Self::Value, _align: Align) -> Self::Value {
        let ptr_elem_ty = match self.lookup_type(ptr.ty) {
            SpirvType::Pointer {
                storage_class: _,
                pointee,
            } => pointee,
            ty => self.fatal(&format!(
                "store called on variable that wasn't a pointer: {:?}",
                ty
            )),
        };
        assert_ty_eq!(self, ptr_elem_ty, val.ty);
        self.emit().store(ptr.def, val.def, None, empty()).unwrap();
        val
    }

    fn store_with_flags(
        &mut self,
        val: Self::Value,
        ptr: Self::Value,
        align: Align,
        _flags: MemFlags,
    ) -> Self::Value {
        self.store(val, ptr, align)
    }

    fn atomic_store(
        &mut self,
        val: Self::Value,
        ptr: Self::Value,
        order: AtomicOrdering,
        _size: Size,
    ) {
        let ptr_elem_ty = match self.lookup_type(ptr.ty) {
            SpirvType::Pointer {
                storage_class: _,
                pointee,
            } => pointee,
            ty => self.fatal(&format!(
                "atomic_store called on variable that wasn't a pointer: {:?}",
                ty
            )),
        };
        assert_ty_eq!(self, ptr_elem_ty, val.ty);
        // TODO: Default to device scope
        let memory = self.constant_u32(Scope::Device as u32);
        let semantics = self.constant_u32(ordering_to_semantics(order).bits());
        self.validate_atomic(val.ty, ptr.def);
        self.emit()
            .atomic_store(ptr.def, memory.def, semantics.def, val.def)
            .unwrap();
    }

    fn gep(&mut self, ptr: Self::Value, indices: &[Self::Value]) -> Self::Value {
        self.gep_help(ptr, indices, false)
    }

    fn inbounds_gep(&mut self, ptr: Self::Value, indices: &[Self::Value]) -> Self::Value {
        self.gep_help(ptr, indices, true)
    }

    fn struct_gep(&mut self, ptr: Self::Value, idx: u64) -> Self::Value {
        let (storage_class, result_pointee_type) = match self.lookup_type(ptr.ty) {
            SpirvType::Pointer {
                storage_class,
                pointee,
            } => match self.lookup_type(pointee) {
                SpirvType::Adt { field_types, .. } => (storage_class, field_types[idx as usize]),
                SpirvType::Array { element, .. }
                | SpirvType::RuntimeArray { element, .. }
                | SpirvType::Vector { element, .. } => (storage_class, element),
                other => self.fatal(&format!(
                    "struct_gep not on struct, array, or vector type: {:?}, index {}",
                    other, idx
                )),
            },
            other => self.fatal(&format!(
                "struct_gep not on pointer type: {:?}, index {}",
                other, idx
            )),
        };
        let result_type = SpirvType::Pointer {
            storage_class,
            pointee: result_pointee_type,
        }
        .def(self);
        // Important! LLVM, and therefore intel-compute-runtime, require the `getelementptr` instruction (and therefore
        // OpAccessChain) on structs to be a constant i32. Not i64! i32.
        if idx > u32::MAX as u64 {
            self.fatal("struct_gep bigger than u32::MAX");
        }
        let index_const = self.constant_u32(idx as u32).def;
        self.emit()
            .access_chain(result_type, None, ptr.def, [index_const].iter().cloned())
            .unwrap()
            .with_type(result_type)
    }

    // intcast has the logic for dealing with bools, so use that
    fn trunc(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        self.intcast(val, dest_ty, false)
    }
    fn sext(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        self.intcast(val, dest_ty, true)
    }
    fn fptoui_sat(&mut self, _val: Self::Value, _dest_ty: Self::Type) -> Option<Self::Value> {
        None
    }

    fn fptosi_sat(&mut self, _val: Self::Value, _dest_ty: Self::Type) -> Option<Self::Value> {
        None
    }

    fn fptosui_may_trap(&self, _val: Self::Value, _dest_ty: Self::Type) -> bool {
        false
    }

    fn fptoui(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        if val.ty == dest_ty {
            val
        } else {
            self.emit()
                .convert_f_to_u(dest_ty, None, val.def)
                .unwrap()
                .with_type(dest_ty)
        }
    }

    fn fptosi(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        if val.ty == dest_ty {
            val
        } else {
            self.emit()
                .convert_f_to_s(dest_ty, None, val.def)
                .unwrap()
                .with_type(dest_ty)
        }
    }

    fn uitofp(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        if val.ty == dest_ty {
            val
        } else {
            self.emit()
                .convert_u_to_f(dest_ty, None, val.def)
                .unwrap()
                .with_type(dest_ty)
        }
    }

    fn sitofp(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        if val.ty == dest_ty {
            val
        } else {
            self.emit()
                .convert_s_to_f(dest_ty, None, val.def)
                .unwrap()
                .with_type(dest_ty)
        }
    }

    fn fptrunc(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        if val.ty == dest_ty {
            val
        } else {
            self.emit()
                .f_convert(dest_ty, None, val.def)
                .unwrap()
                .with_type(dest_ty)
        }
    }

    fn fpext(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        if val.ty == dest_ty {
            val
        } else {
            self.emit()
                .f_convert(dest_ty, None, val.def)
                .unwrap()
                .with_type(dest_ty)
        }
    }

    fn ptrtoint(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        match self.lookup_type(val.ty) {
            SpirvType::Pointer { .. } => (),
            other => self.fatal(&format!(
                "ptrtoint called on non-pointer source type: {:?}",
                other
            )),
        }
        if val.ty == dest_ty {
            val
        } else {
            let result = self
                .emit()
                .convert_ptr_to_u(dest_ty, None, val.def)
                .unwrap()
                .with_type(dest_ty);
            self.zombie_convert_ptr_to_u(result.def);
            result
        }
    }

    fn inttoptr(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        match self.lookup_type(dest_ty) {
            SpirvType::Pointer { .. } => (),
            other => self.fatal(&format!(
                "inttoptr called on non-pointer dest type: {:?}",
                other
            )),
        }
        if val.ty == dest_ty {
            val
        } else {
            let result = self
                .emit()
                .convert_u_to_ptr(dest_ty, None, val.def)
                .unwrap()
                .with_type(dest_ty);
            self.zombie_convert_u_to_ptr(result.def);
            result
        }
    }

    fn bitcast(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        if val.ty == dest_ty {
            val
        } else {
            let result = self
                .emit()
                .bitcast(dest_ty, None, val.def)
                .unwrap()
                .with_type(dest_ty);
            let val_is_ptr = matches!(self.lookup_type(val.ty), SpirvType::Pointer{..});
            let dest_is_ptr = matches!(self.lookup_type(dest_ty), SpirvType::Pointer{..});
            if val_is_ptr || dest_is_ptr {
                self.zombie_bitcast_ptr(result.def);
            }
            result
        }
    }

    fn intcast(&mut self, val: Self::Value, dest_ty: Self::Type, is_signed: bool) -> Self::Value {
        if val.ty == dest_ty {
            // I guess?
            return val;
        }
        match (self.lookup_type(val.ty), self.lookup_type(dest_ty)) {
            // sign change
            (
                SpirvType::Integer(val_width, val_signedness),
                SpirvType::Integer(dest_width, dest_signedness),
            ) if val_width == dest_width && val_signedness != dest_signedness => self
                .emit()
                .bitcast(dest_ty, None, val.def)
                .unwrap()
                .with_type(dest_ty),
            // width change, and optional sign change
            (SpirvType::Integer(_, _), SpirvType::Integer(_, dest_signedness)) => {
                // spir-v spec doesn't seem to say that signedness needs to match the operands, only that the signedness
                // of the destination type must match the instruction's signedness.
                if dest_signedness {
                    self.emit().s_convert(dest_ty, None, val.def)
                } else {
                    self.emit().u_convert(dest_ty, None, val.def)
                }
                .unwrap()
                .with_type(dest_ty)
            }
            // bools are ints in llvm, so we have to implement this here
            (SpirvType::Bool, SpirvType::Integer(_, _)) => {
                // spir-v doesn't have a direct conversion instruction
                let if_true = self.constant_int(dest_ty, 1);
                let if_false = self.constant_int(dest_ty, 0);
                self.emit()
                    .select(dest_ty, None, val.def, if_true.def, if_false.def)
                    .unwrap()
                    .with_type(dest_ty)
            }
            (SpirvType::Integer(_, _), SpirvType::Bool) => {
                // spir-v doesn't have a direct conversion instruction, glslang emits OpINotEqual
                let zero = self.constant_int(val.ty, 0);
                self.emit()
                    .i_not_equal(dest_ty, None, val.def, zero.def)
                    .unwrap()
                    .with_type(dest_ty)
            }
            (val_ty, dest_ty_spv) => self.fatal(&format!(
                "TODO: intcast not implemented yet: val={:?} val.ty={:?} dest_ty={:?} is_signed={}",
                val, val_ty, dest_ty_spv, is_signed
            )),
        }
    }

    fn pointercast(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        let val_pointee = match self.lookup_type(val.ty) {
            SpirvType::Pointer { pointee, .. } => pointee,
            other => self.fatal(&format!(
                "pointercast called on non-pointer source type: {:?}",
                other
            )),
        };
        let dest_pointee = match self.lookup_type(dest_ty) {
            SpirvType::Pointer { pointee, .. } => pointee,
            other => self.fatal(&format!(
                "pointercast called on non-pointer dest type: {:?}",
                other
            )),
        };
        if val.ty == dest_ty {
            val
        } else if let Some(indices) = self.try_pointercast_via_gep(val_pointee, dest_pointee) {
            let indices = indices
                .into_iter()
                .map(|idx| self.constant_u32(idx).def)
                .collect::<Vec<_>>();
            self.emit()
                .access_chain(dest_ty, None, val.def, indices)
                .unwrap()
                .with_type(dest_ty)
        } else if self
            .really_unsafe_ignore_bitcasts
            .borrow()
            .contains(&self.current_fn)
        {
            val
        } else {
            let result = self
                .emit()
                .bitcast(dest_ty, None, val.def)
                .unwrap()
                .with_type(dest_ty);
            self.zombie_bitcast_ptr(result.def);
            result
        }
    }

    fn icmp(&mut self, op: IntPredicate, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        // Note: the signedness of the opcode doesn't have to match the signedness of the operands.
        use IntPredicate::*;
        assert_ty_eq!(self, lhs.ty, rhs.ty);
        let b = SpirvType::Bool.def(self);
        match self.lookup_type(lhs.ty) {
            SpirvType::Integer(_, _) => match op {
                IntEQ => self.emit().i_equal(b, None, lhs.def, rhs.def),
                IntNE => self.emit().i_not_equal(b, None, lhs.def, rhs.def),
                IntUGT => self.emit().u_greater_than(b, None, lhs.def, rhs.def),
                IntUGE => self.emit().u_greater_than_equal(b, None, lhs.def, rhs.def),
                IntULT => self.emit().u_less_than(b, None, lhs.def, rhs.def),
                IntULE => self.emit().u_less_than_equal(b, None, lhs.def, rhs.def),
                IntSGT => self.emit().s_greater_than(b, None, lhs.def, rhs.def),
                IntSGE => self.emit().s_greater_than_equal(b, None, lhs.def, rhs.def),
                IntSLT => self.emit().s_less_than(b, None, lhs.def, rhs.def),
                IntSLE => self.emit().s_less_than_equal(b, None, lhs.def, rhs.def),
            },
            SpirvType::Pointer { .. } => match op {
                IntEQ => {
                    if self.emit().version().unwrap() > (1, 3) {
                        self.emit().ptr_equal(b, None, lhs.def, rhs.def)
                    } else {
                        let int_ty = self.type_usize();
                        let lhs = self.emit().convert_ptr_to_u(int_ty, None, lhs.def).unwrap();
                        self.zombie_convert_ptr_to_u(lhs);
                        let rhs = self.emit().convert_ptr_to_u(int_ty, None, rhs.def).unwrap();
                        self.zombie_convert_ptr_to_u(rhs);
                        self.emit().i_not_equal(b, None, lhs, rhs)
                    }
                }
                IntNE => {
                    if self.emit().version().unwrap() > (1, 3) {
                        self.emit().ptr_not_equal(b, None, lhs.def, rhs.def)
                    } else {
                        let int_ty = self.type_usize();
                        let lhs = self.emit().convert_ptr_to_u(int_ty, None, lhs.def).unwrap();
                        self.zombie_convert_ptr_to_u(lhs);
                        let rhs = self.emit().convert_ptr_to_u(int_ty, None, rhs.def).unwrap();
                        self.zombie_convert_ptr_to_u(rhs);
                        self.emit().i_not_equal(b, None, lhs, rhs)
                    }
                }
                IntUGT => {
                    let int_ty = self.type_usize();
                    let lhs = self.emit().convert_ptr_to_u(int_ty, None, lhs.def).unwrap();
                    self.zombie_convert_ptr_to_u(lhs);
                    let rhs = self.emit().convert_ptr_to_u(int_ty, None, rhs.def).unwrap();
                    self.zombie_convert_ptr_to_u(rhs);
                    self.emit().u_greater_than(b, None, lhs, rhs)
                }
                IntUGE => {
                    let int_ty = self.type_usize();
                    let lhs = self.emit().convert_ptr_to_u(int_ty, None, lhs.def).unwrap();
                    self.zombie_convert_ptr_to_u(lhs);
                    let rhs = self.emit().convert_ptr_to_u(int_ty, None, rhs.def).unwrap();
                    self.zombie_convert_ptr_to_u(rhs);
                    self.emit().u_greater_than_equal(b, None, lhs, rhs)
                }
                IntULT => {
                    let int_ty = self.type_usize();
                    let lhs = self.emit().convert_ptr_to_u(int_ty, None, lhs.def).unwrap();
                    self.zombie_convert_ptr_to_u(lhs);
                    let rhs = self.emit().convert_ptr_to_u(int_ty, None, rhs.def).unwrap();
                    self.zombie_convert_ptr_to_u(rhs);
                    self.emit().u_less_than(b, None, lhs, rhs)
                }
                IntULE => {
                    let int_ty = self.type_usize();
                    let lhs = self.emit().convert_ptr_to_u(int_ty, None, lhs.def).unwrap();
                    self.zombie_convert_ptr_to_u(lhs);
                    let rhs = self.emit().convert_ptr_to_u(int_ty, None, rhs.def).unwrap();
                    self.zombie_convert_ptr_to_u(rhs);
                    self.emit().u_less_than_equal(b, None, lhs, rhs)
                }
                IntSGT => self.fatal("TODO: pointer operator IntSGT not implemented yet"),
                IntSGE => self.fatal("TODO: pointer operator IntSGE not implemented yet"),
                IntSLT => self.fatal("TODO: pointer operator IntSLT not implemented yet"),
                IntSLE => self.fatal("TODO: pointer operator IntSLE not implemented yet"),
            },
            SpirvType::Bool => match op {
                IntEQ => self.emit().logical_equal(b, None, lhs.def, rhs.def),
                IntNE => self.emit().logical_not_equal(b, None, lhs.def, rhs.def),
                // x > y  =>  x && !y
                IntUGT => {
                    // intel-compute-runtime doesn't like OpLogicalNot
                    let true_ = self.constant_bool(true);
                    let rhs = self
                        .emit()
                        .logical_not_equal(b, None, rhs.def, true_.def)
                        .unwrap();
                    self.emit().logical_and(b, None, lhs.def, rhs)
                }
                // x >= y  =>  x || !y
                IntUGE => {
                    let true_ = self.constant_bool(true);
                    let rhs = self
                        .emit()
                        .logical_not_equal(b, None, rhs.def, true_.def)
                        .unwrap();
                    self.emit().logical_or(b, None, lhs.def, rhs)
                }
                // x < y  =>  !x && y
                IntULE => {
                    let true_ = self.constant_bool(true);
                    let lhs = self
                        .emit()
                        .logical_not_equal(b, None, lhs.def, true_.def)
                        .unwrap();
                    self.emit().logical_and(b, None, lhs, rhs.def)
                }
                // x <= y  =>  !x || y
                IntULT => {
                    let true_ = self.constant_bool(true);
                    let lhs = self
                        .emit()
                        .logical_not_equal(b, None, lhs.def, true_.def)
                        .unwrap();
                    self.emit().logical_or(b, None, lhs, rhs.def)
                }
                IntSGT => self.fatal("TODO: boolean operator IntSGT not implemented yet"),
                IntSGE => self.fatal("TODO: boolean operator IntSGE not implemented yet"),
                IntSLT => self.fatal("TODO: boolean operator IntSLT not implemented yet"),
                IntSLE => self.fatal("TODO: boolean operator IntSLE not implemented yet"),
            },
            other => self.fatal(&format!(
                "Int comparison not implemented on {}",
                other.debug(lhs.ty, self)
            )),
        }
        .unwrap()
        .with_type(b)
    }

    fn fcmp(&mut self, op: RealPredicate, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        use RealPredicate::*;
        assert_ty_eq!(self, lhs.ty, rhs.ty);
        let b = SpirvType::Bool.def(self);
        match op {
            RealPredicateFalse => return self.cx.constant_bool(false),
            RealPredicateTrue => return self.cx.constant_bool(true),
            RealOEQ => self.emit().f_ord_equal(b, None, lhs.def, rhs.def),
            RealOGT => self.emit().f_ord_greater_than(b, None, lhs.def, rhs.def),
            RealOGE => self
                .emit()
                .f_ord_greater_than_equal(b, None, lhs.def, rhs.def),
            RealOLT => self.emit().f_ord_less_than(b, None, lhs.def, rhs.def),
            RealOLE => self.emit().f_ord_less_than_equal(b, None, lhs.def, rhs.def),
            RealONE => self.emit().f_ord_not_equal(b, None, lhs.def, rhs.def),
            RealORD => self.emit().ordered(b, None, lhs.def, rhs.def),
            RealUNO => self.emit().unordered(b, None, lhs.def, rhs.def),
            RealUEQ => self.emit().f_unord_equal(b, None, lhs.def, rhs.def),
            RealUGT => self.emit().f_unord_greater_than(b, None, lhs.def, rhs.def),
            RealUGE => self
                .emit()
                .f_unord_greater_than_equal(b, None, lhs.def, rhs.def),
            RealULT => self.emit().f_unord_less_than(b, None, lhs.def, rhs.def),
            RealULE => self
                .emit()
                .f_unord_less_than_equal(b, None, lhs.def, rhs.def),
            RealUNE => self.emit().f_unord_not_equal(b, None, lhs.def, rhs.def),
        }
        .unwrap()
        .with_type(b)
    }

    fn memcpy(
        &mut self,
        dst: Self::Value,
        _dst_align: Align,
        src: Self::Value,
        _src_align: Align,
        size: Self::Value,
        _flags: MemFlags,
    ) {
        self.emit()
            .copy_memory_sized(dst.def, src.def, size.def, None, None, empty())
            .unwrap();
        if !self.builder.has_capability(Capability::Addresses) {
            self.zombie(dst.def, "OpCopyMemorySized without OpCapability Addresses");
        }
    }

    fn memmove(
        &mut self,
        dst: Self::Value,
        _dst_align: Align,
        src: Self::Value,
        _src_align: Align,
        size: Self::Value,
        _flags: MemFlags,
    ) {
        self.emit()
            .copy_memory_sized(dst.def, src.def, size.def, None, None, empty())
            .unwrap();
        if !self.builder.has_capability(Capability::Addresses) {
            self.zombie(dst.def, "OpCopyMemorySized without OpCapability Addresses");
        }
    }

    fn memset(
        &mut self,
        ptr: Self::Value,
        fill_byte: Self::Value,
        size: Self::Value,
        _align: Align,
        _flags: MemFlags,
    ) {
        let elem_ty = match self.lookup_type(ptr.ty) {
            SpirvType::Pointer { pointee, .. } => pointee,
            _ => self.fatal(&format!(
                "memset called on non-pointer type: {}",
                self.debug_type(ptr.ty)
            )),
        };
        let elem_ty_spv = self.lookup_type(elem_ty);
        let pat = match self.builder.lookup_const_u64(fill_byte) {
            Some(fill_byte) => self.memset_const_pattern(&elem_ty_spv, fill_byte as u8),
            None => self.memset_dynamic_pattern(&elem_ty_spv, fill_byte.def),
        }
        .with_type(elem_ty);
        match self.builder.lookup_const_u64(size) {
            Some(size) => self.memset_constant_size(ptr, pat, size),
            None => self.memset_dynamic_size(ptr, pat, size),
        }
    }

    fn select(
        &mut self,
        cond: Self::Value,
        then_val: Self::Value,
        else_val: Self::Value,
    ) -> Self::Value {
        assert_ty_eq!(self, then_val.ty, else_val.ty);
        let result_type = then_val.ty;
        self.emit()
            .select(result_type, None, cond.def, then_val.def, else_val.def)
            .unwrap()
            .with_type(result_type)
    }

    fn va_arg(&mut self, _list: Self::Value, _ty: Self::Type) -> Self::Value {
        todo!()
    }

    fn extract_element(&mut self, vec: Self::Value, idx: Self::Value) -> Self::Value {
        let result_type = match self.lookup_type(vec.ty) {
            SpirvType::Vector { element, .. } => element,
            other => self.fatal(&format!(
                "extract_element not implemented on type {:?}",
                other
            )),
        };
        match self.builder.lookup_const_u64(idx) {
            Some(const_index) => self.emit().composite_extract(
                result_type,
                None,
                vec.def,
                [const_index as u32].iter().cloned(),
            ),
            None => self
                .emit()
                .vector_extract_dynamic(result_type, None, vec.def, idx.def),
        }
        .unwrap()
        .with_type(result_type)
    }

    fn vector_splat(&mut self, num_elts: usize, elt: Self::Value) -> Self::Value {
        let result_type = SpirvType::Vector {
            element: elt.ty,
            count: num_elts as u32,
        }
        .def(self);
        if self.builder.lookup_const(elt).is_some() {
            self.constant_composite(result_type, vec![elt.def; num_elts])
        } else {
            self.emit()
                .composite_construct(result_type, None, std::iter::repeat(elt.def).take(num_elts))
                .unwrap()
                .with_type(result_type)
        }
    }

    fn extract_value(&mut self, agg_val: Self::Value, idx: u64) -> Self::Value {
        let result_type = match self.lookup_type(agg_val.ty) {
            SpirvType::Adt { field_types, .. } => field_types[idx as usize],
            other => self.fatal(&format!(
                "extract_value not implemented on type {:?}",
                other
            )),
        };
        self.emit()
            .composite_extract(result_type, None, agg_val.def, [idx as u32].iter().cloned())
            .unwrap()
            .with_type(result_type)
    }

    fn insert_value(&mut self, agg_val: Self::Value, elt: Self::Value, idx: u64) -> Self::Value {
        match self.lookup_type(agg_val.ty) {
            SpirvType::Adt { field_types, .. } => {
                assert_ty_eq!(self, field_types[idx as usize], elt.ty)
            }
            other => self.fatal(&format!("insert_value not implemented on type {:?}", other)),
        };
        self.emit()
            .composite_insert(
                agg_val.ty,
                None,
                elt.def,
                agg_val.def,
                [idx as u32].iter().cloned(),
            )
            .unwrap()
            .with_type(agg_val.ty)
    }

    fn landing_pad(
        &mut self,
        _ty: Self::Type,
        _pers_fn: Self::Value,
        _num_clauses: usize,
    ) -> Self::Value {
        todo!()
    }

    fn set_cleanup(&mut self, _landing_pad: Self::Value) {
        todo!()
    }

    fn resume(&mut self, _exn: Self::Value) -> Self::Value {
        todo!()
    }

    fn cleanup_pad(
        &mut self,
        _parent: Option<Self::Value>,
        _args: &[Self::Value],
    ) -> Self::Funclet {
        todo!()
    }

    fn cleanup_ret(
        &mut self,
        _funclet: &Self::Funclet,
        _unwind: Option<Self::BasicBlock>,
    ) -> Self::Value {
        todo!()
    }

    fn catch_pad(&mut self, _parent: Self::Value, _args: &[Self::Value]) -> Self::Funclet {
        todo!()
    }

    fn catch_switch(
        &mut self,
        _parent: Option<Self::Value>,
        _unwind: Option<Self::BasicBlock>,
        _num_handlers: usize,
    ) -> Self::Value {
        todo!()
    }

    fn add_handler(&mut self, _catch_switch: Self::Value, _handler: Self::BasicBlock) {
        todo!()
    }

    fn set_personality_fn(&mut self, _personality: Self::Value) {
        todo!()
    }

    fn atomic_cmpxchg(
        &mut self,
        dst: Self::Value,
        cmp: Self::Value,
        src: Self::Value,
        order: AtomicOrdering,
        failure_order: AtomicOrdering,
        _weak: bool,
    ) -> Self::Value {
        let dst_pointee_ty = match self.lookup_type(dst.ty) {
            SpirvType::Pointer {
                storage_class: _,
                pointee,
            } => pointee,
            ty => self.fatal(&format!(
                "atomic_cmpxchg called on variable that wasn't a pointer: {:?}",
                ty
            )),
        };
        assert_ty_eq!(self, dst_pointee_ty, cmp.ty);
        assert_ty_eq!(self, dst_pointee_ty, src.ty);
        self.validate_atomic(dst_pointee_ty, dst.def);
        // TODO: Default to device scope
        let memory = self.constant_u32(Scope::Device as u32);
        let semantics_equal = self.constant_u32(ordering_to_semantics(order).bits());
        let semantics_unequal = self.constant_u32(ordering_to_semantics(failure_order).bits());
        // Note: OpAtomicCompareExchangeWeak is deprecated, and has the same semantics
        self.emit()
            .atomic_compare_exchange(
                src.ty,
                None,
                dst.def,
                memory.def,
                semantics_equal.def,
                semantics_unequal.def,
                src.def,
                cmp.def,
            )
            .unwrap()
            .with_type(src.ty)
    }

    fn atomic_rmw(
        &mut self,
        op: AtomicRmwBinOp,
        dst: Self::Value,
        src: Self::Value,
        order: AtomicOrdering,
    ) -> Self::Value {
        let dst_pointee_ty = match self.lookup_type(dst.ty) {
            SpirvType::Pointer {
                storage_class: _,
                pointee,
            } => pointee,
            ty => self.fatal(&format!(
                "atomic_rmw called on variable that wasn't a pointer: {:?}",
                ty
            )),
        };
        assert_ty_eq!(self, dst_pointee_ty, src.ty);
        self.validate_atomic(dst_pointee_ty, dst.def);
        // TODO: Default to device scope
        let memory = self.constant_u32(Scope::Device as u32).def;
        let semantics = self.constant_u32(ordering_to_semantics(order).bits()).def;
        let mut emit = self.emit();
        use AtomicRmwBinOp::*;
        match op {
            AtomicXchg => emit.atomic_exchange(src.ty, None, dst.def, memory, semantics, src.def),
            AtomicAdd => emit.atomic_i_add(src.ty, None, dst.def, memory, semantics, src.def),
            AtomicSub => emit.atomic_i_sub(src.ty, None, dst.def, memory, semantics, src.def),
            AtomicAnd => emit.atomic_and(src.ty, None, dst.def, memory, semantics, src.def),
            AtomicNand => self.fatal("atomic nand is not supported"),
            AtomicOr => emit.atomic_or(src.ty, None, dst.def, memory, semantics, src.def),
            AtomicXor => emit.atomic_xor(src.ty, None, dst.def, memory, semantics, src.def),
            AtomicMax => emit.atomic_s_max(src.ty, None, dst.def, memory, semantics, src.def),
            AtomicMin => emit.atomic_s_min(src.ty, None, dst.def, memory, semantics, src.def),
            AtomicUMax => emit.atomic_u_max(src.ty, None, dst.def, memory, semantics, src.def),
            AtomicUMin => emit.atomic_u_min(src.ty, None, dst.def, memory, semantics, src.def),
        }
        .unwrap()
        .with_type(src.ty)
    }

    fn atomic_fence(&mut self, order: AtomicOrdering, _scope: SynchronizationScope) {
        // Ignore sync scope (it only has "single thread" and "cross thread")
        // TODO: Default to device scope
        let memory = self.constant_u32(Scope::Device as u32).def;
        let semantics = self.constant_u32(ordering_to_semantics(order).bits()).def;
        self.emit().memory_barrier(memory, semantics).unwrap();
    }

    fn set_invariant_load(&mut self, _load: Self::Value) {
        // ignore
    }

    /// Called for `StorageLive`
    fn lifetime_start(&mut self, _ptr: Self::Value, _size: Size) {
        // ignore
    }

    /// Called for `StorageDead`
    fn lifetime_end(&mut self, _ptr: Self::Value, _size: Size) {
        // ignore
    }

    fn instrprof_increment(
        &mut self,
        _fn_name: Self::Value,
        _hash: Self::Value,
        _num_counters: Self::Value,
        _index: Self::Value,
    ) {
        todo!()
    }

    fn call(
        &mut self,
        mut llfn: Self::Value,
        args: &[Self::Value],
        funclet: Option<&Self::Funclet>,
    ) -> Self::Value {
        if funclet.is_some() {
            self.fatal("TODO: Funclets are not supported");
        }
        // dereference pointers
        let (result_type, argument_types) = loop {
            match self.lookup_type(llfn.ty) {
                SpirvType::Pointer { pointee, .. } => {
                    // See comment on register_fn_ptr
                    if let Some(func) = self.lookup_fn_ptr(llfn) {
                        llfn = func;
                    } else {
                        llfn = self
                            .emit()
                            .load(pointee, None, llfn.def, None, empty())
                            .unwrap()
                            .with_type(pointee)
                    }
                }
                SpirvType::Function {
                    return_type,
                    arguments,
                } => break (return_type, arguments),
                ty => self.fatal(&format!("Calling non-function type: {:?}", ty)),
            }
        };
        for (argument, argument_type) in args.iter().zip(argument_types) {
            assert_ty_eq!(self, argument.ty, argument_type);
        }
        let args = args.iter().map(|arg| arg.def).collect::<Vec<_>>();
        self.emit()
            .function_call(result_type, None, llfn.def, args)
            .unwrap()
            .with_type(result_type)
    }

    fn zext(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        self.intcast(val, dest_ty, false)
    }

    unsafe fn delete_basic_block(&mut self, _bb: Self::BasicBlock) {
        // Ignore: If we were to delete the block, then other builder's selected_block index would become invalid, due
        // to shifting blocks.
    }

    fn do_not_inline(&mut self, _llret: Self::Value) {
        // Ignore
    }
}
