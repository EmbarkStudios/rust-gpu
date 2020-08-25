use super::Builder;
use crate::abi::SpirvType;
use crate::builder_spirv::{BuilderCursor, SpirvValueExt};
use rspirv::spirv::StorageClass;
use rustc_codegen_ssa::base::to_immediate;
use rustc_codegen_ssa::common::{
    AtomicOrdering, AtomicRmwBinOp, IntPredicate, RealPredicate, SynchronizationScope,
};
use rustc_codegen_ssa::mir::operand::{OperandRef, OperandValue};
use rustc_codegen_ssa::mir::place::PlaceRef;
use rustc_codegen_ssa::traits::LayoutTypeMethods;
use rustc_codegen_ssa::traits::{BuilderMethods, OverflowOp};
use rustc_codegen_ssa::MemFlags;
use rustc_middle::ty::Ty;
use rustc_target::abi::{Abi, Align, Size};
use std::iter::empty;
use std::ops::Range;

macro_rules! simple_op {
    ($func_name:ident, $inst_name:ident) => {
        fn $func_name(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
            assert_eq!(lhs.ty, rhs.ty);
            let result_type = lhs.ty;
            self.emit()
                .$inst_name(result_type, None, lhs.def, rhs.def)
                .unwrap()
                .with_type(result_type)
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

impl<'a, 'spv, 'tcx> BuilderMethods<'a, 'tcx> for Builder<'a, 'spv, 'tcx> {
    fn with_cx(cx: &'a Self::CodegenCx) -> Self {
        // Note: all defaults here *must* be filled out by position_at_end
        Self {
            cx,
            cursor: Default::default(),
            current_fn: Default::default(),
            basic_block: Default::default(),
        }
    }

    fn position_at_end(&mut self, llbb: Self::BasicBlock) {
        let cursor = self.cx.builder.select_block_by_id(llbb);
        let current_fn = {
            let emit = self.emit_with_cursor(cursor);
            let selected_function = emit.selected_function().unwrap();
            let selected_function = &emit.module_ref().functions[selected_function];
            selected_function.def.as_ref().unwrap().result_id.unwrap()
        };
        self.cursor = cursor;
        self.current_fn = current_fn;
        self.basic_block = llbb;
    }

    fn new_block<'b>(cx: &'a Self::CodegenCx, llfn: Self::Function, _name: &'b str) -> Self {
        let cursor_fn = cx.builder.select_function_by_id(llfn);
        let label = cx.emit_with_cursor(cursor_fn).begin_block(None).unwrap();
        let cursor = cx.builder.select_block_by_id(label);
        Self {
            cx,
            cursor,
            current_fn: llfn,
            basic_block: label,
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
        }
    }

    fn cx(&self) -> &Self::CodegenCx {
        self.cx
    }

    fn llbb(&self) -> Self::BasicBlock {
        self.basic_block
    }

    fn ret_void(&mut self) {
        self.emit().ret().unwrap();
    }

    fn ret(&mut self, value: Self::Value) {
        self.emit().ret_value(value.def).unwrap();
    }

    fn br(&mut self, dest: Self::BasicBlock) {
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
        let cases = cases
            .map(|(i, b)| {
                if i > u32::MAX as u128 {
                    panic!("Switches to values above u32::MAX not supported: {:?}", i)
                } else {
                    (i as u32, b)
                }
            })
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
    simple_op! {shl, shift_left_logical}
    simple_op! {lshr, shift_right_logical}
    simple_op! {ashr, shift_right_arithmetic}
    simple_op! {unchecked_sadd, i_add} // already unchecked by default
    simple_op! {unchecked_uadd, i_add} // already unchecked by default
    simple_op! {unchecked_ssub, i_sub} // already unchecked by default
    simple_op! {unchecked_usub, i_sub} // already unchecked by default
    simple_op! {unchecked_smul, i_mul} // already unchecked by default
    simple_op! {unchecked_umul, i_mul} // already unchecked by default
    simple_op! {and, bitwise_and}
    simple_op! {or, bitwise_or}
    simple_op! {xor, bitwise_xor}
    simple_uni_op! {neg, s_negate}
    simple_uni_op! {fneg, f_negate}
    simple_uni_op! {not, not}

    fn checked_binop(
        &mut self,
        _oop: OverflowOp,
        _ty: Ty<'_>,
        _lhs: Self::Value,
        _rhs: Self::Value,
    ) -> (Self::Value, Self::Value) {
        panic!("TODO: Checked binary operations are not supported yet");
    }

    fn alloca(&mut self, ty: Self::Type, _align: Align) -> Self::Value {
        let ptr_ty = SpirvType::Pointer {
            storage_class: StorageClass::Function,
            pointee: ty,
        }
        .def(self);
        self.emit()
            .variable(ptr_ty, None, StorageClass::Function, None)
            .with_type(ptr_ty)
    }

    fn dynamic_alloca(&mut self, ty: Self::Type, align: Align) -> Self::Value {
        self.alloca(ty, align)
    }

    fn array_alloca(&mut self, _ty: Self::Type, _len: Self::Value, _align: Align) -> Self::Value {
        panic!("TODO: array_alloca not supported yet")
    }

    fn load(&mut self, ptr: Self::Value, _align: Align) -> Self::Value {
        let ty = match self.lookup_type(ptr.ty) {
            SpirvType::Pointer {
                storage_class: _,
                pointee,
            } => pointee,
            ty => panic!("load called on variable that wasn't a pointer: {:?}", ty),
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

    fn atomic_load(
        &mut self,
        _ptr: Self::Value,
        _order: AtomicOrdering,
        _size: Size,
    ) -> Self::Value {
        panic!("TODO: atomic_load not supported yet")
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
            OperandValue::Immediate(to_immediate(self, llval, place.layout))
        } else if let Abi::ScalarPair(ref a, ref b) = place.layout.abi {
            let b_offset = a.value.size(self).align_to(b.value.align(self).abi);

            let mut load = |i, align| {
                let llptr = self.struct_gep(place.llval, i as u64);
                self.load(llptr, align)
            };

            OperandValue::Pair(
                load(0, place.align),
                load(1, place.align.restrict_for_offset(b_offset)),
            )
        } else {
            OperandValue::Ref(place.llval, None, place.align)
        };
        OperandRef {
            val,
            layout: place.layout,
        }
    }

    /// Called for Rvalue::Repeat when the elem is neither a ZST nor optimizable using memset.
    fn write_operand_repeatedly(
        self,
        _elem: OperandRef<'tcx, Self::Value>,
        _count: u64,
        _dest: PlaceRef<'tcx, Self::Value>,
    ) -> Self {
        todo!()
    }

    fn range_metadata(&mut self, _load: Self::Value, _range: Range<u128>) {
        todo!()
    }

    fn nonnull_metadata(&mut self, _load: Self::Value) {
        todo!()
    }

    fn store(&mut self, val: Self::Value, ptr: Self::Value, _align: Align) -> Self::Value {
        let ptr_elem_ty = match self.lookup_type(ptr.ty) {
            SpirvType::Pointer {
                storage_class: _,
                pointee,
            } => pointee,
            ty => panic!("store called on variable that wasn't a pointer: {:?}", ty),
        };
        assert_eq!(ptr_elem_ty, val.ty);
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
        _val: Self::Value,
        _ptr: Self::Value,
        _order: AtomicOrdering,
        _size: Size,
    ) {
        panic!("TODO: atomic_store not supported yet")
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
                SpirvType::Adt { field_types } => (storage_class, field_types[idx as usize]),
                other => panic!("struct_gep not on struct type: {:?}", other),
            },
            other => panic!("struct_gep not on struct pointer type: {:?}", other),
        };
        let result_type = SpirvType::Pointer {
            storage_class,
            pointee: result_pointee_type,
        }
        .def(self);
        let u64 = SpirvType::Integer(64, false).def(self);
        let index_const = self.builder.constant_u64(u64, idx);
        self.emit()
            .access_chain(result_type, None, ptr.def, [index_const].iter().cloned())
            .unwrap()
            .with_type(result_type)
    }

    fn trunc(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        self.emit()
            .u_convert(dest_ty, None, val.def)
            .unwrap()
            .with_type(dest_ty)
    }

    fn zext(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        self.emit()
            .u_convert(dest_ty, None, val.def)
            .unwrap()
            .with_type(dest_ty)
    }

    fn sext(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        self.emit()
            .s_convert(dest_ty, None, val.def)
            .unwrap()
            .with_type(dest_ty)
    }

    fn fptoui_sat(&mut self, _val: Self::Value, _dest_ty: Self::Type) -> Option<Self::Value> {
        None
    }

    fn fptosi_sat(&mut self, _val: Self::Value, _dest_ty: Self::Type) -> Option<Self::Value> {
        None
    }

    fn fptoui(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        self.emit()
            .convert_f_to_u(dest_ty, None, val.def)
            .unwrap()
            .with_type(dest_ty)
    }

    fn fptosi(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        self.emit()
            .convert_f_to_s(dest_ty, None, val.def)
            .unwrap()
            .with_type(dest_ty)
    }

    fn uitofp(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        self.emit()
            .convert_u_to_f(dest_ty, None, val.def)
            .unwrap()
            .with_type(dest_ty)
    }

    fn sitofp(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        self.emit()
            .convert_s_to_f(dest_ty, None, val.def)
            .unwrap()
            .with_type(dest_ty)
    }

    fn fptrunc(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        self.emit()
            .f_convert(dest_ty, None, val.def)
            .unwrap()
            .with_type(dest_ty)
    }

    fn fpext(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        self.emit()
            .f_convert(dest_ty, None, val.def)
            .unwrap()
            .with_type(dest_ty)
    }

    fn ptrtoint(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        match self.lookup_type(val.ty) {
            SpirvType::Pointer { .. } => (),
            other => panic!("ptrtoint called on non-pointer source type: {:?}", other),
        }
        self.emit()
            .bitcast(dest_ty, None, val.def)
            .unwrap()
            .with_type(dest_ty)
    }

    fn inttoptr(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        match self.lookup_type(dest_ty) {
            SpirvType::Pointer { .. } => (),
            other => panic!("inttoptr called on non-pointer dest type: {:?}", other),
        }
        self.emit()
            .bitcast(dest_ty, None, val.def)
            .unwrap()
            .with_type(dest_ty)
    }

    fn bitcast(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        self.emit()
            .bitcast(dest_ty, None, val.def)
            .unwrap()
            .with_type(dest_ty)
    }

    fn intcast(&mut self, val: Self::Value, dest_ty: Self::Type, is_signed: bool) -> Self::Value {
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
            (val_ty, dest_ty_spv) => panic!(
                "TODO: intcast not implemented yet: val={:?} val.ty={:?} dest_ty={:?} is_signed={}",
                val, val_ty, dest_ty_spv, is_signed
            ),
        }
    }

    fn pointercast(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        match self.lookup_type(val.ty) {
            SpirvType::Pointer { .. } => (),
            other => panic!("pointercast called on non-pointer source type: {:?}", other),
        }
        match self.lookup_type(dest_ty) {
            SpirvType::Pointer { .. } => (),
            other => panic!("pointercast called on non-pointer dest type: {:?}", other),
        }
        self.emit()
            .bitcast(dest_ty, None, val.def)
            .unwrap()
            .with_type(dest_ty)
    }

    fn icmp(&mut self, op: IntPredicate, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        // TODO: Do we want to assert signedness matches the opcode? Is it possible to have one that doesn't match? Does
        // spir-v allow nonmatching instructions?
        use IntPredicate::*;
        assert_eq!(lhs.ty, rhs.ty);
        let b = SpirvType::Bool.def(self);
        let mut e = self.emit();
        match op {
            IntEQ => e.i_equal(b, None, lhs.def, rhs.def),
            IntNE => e.i_not_equal(b, None, lhs.def, rhs.def),
            IntUGT => e.u_greater_than(b, None, lhs.def, rhs.def),
            IntUGE => e.u_greater_than_equal(b, None, lhs.def, rhs.def),
            IntULT => e.u_less_than(b, None, lhs.def, rhs.def),
            IntULE => e.u_less_than_equal(b, None, lhs.def, rhs.def),
            IntSGT => e.s_greater_than(b, None, lhs.def, rhs.def),
            IntSGE => e.s_greater_than_equal(b, None, lhs.def, rhs.def),
            IntSLT => e.s_less_than(b, None, lhs.def, rhs.def),
            IntSLE => e.s_less_than_equal(b, None, lhs.def, rhs.def),
        }
        .unwrap()
        .with_type(b)
    }

    fn fcmp(&mut self, op: RealPredicate, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        use RealPredicate::*;
        assert_eq!(lhs.ty, rhs.ty);
        let b = SpirvType::Bool.def(self);
        let mut e = self.emit();
        match op {
            RealPredicateFalse => return e.constant_false(b).with_type(b),
            RealPredicateTrue => return e.constant_true(b).with_type(b),
            RealOEQ => e.f_ord_equal(b, None, lhs.def, rhs.def),
            RealOGT => e.f_ord_greater_than(b, None, lhs.def, rhs.def),
            RealOGE => e.f_ord_greater_than_equal(b, None, lhs.def, rhs.def),
            RealOLT => e.f_ord_less_than(b, None, lhs.def, rhs.def),
            RealOLE => e.f_ord_less_than_equal(b, None, lhs.def, rhs.def),
            RealONE => e.f_ord_not_equal(b, None, lhs.def, rhs.def),
            RealORD => e.ordered(b, None, lhs.def, rhs.def),
            RealUNO => e.unordered(b, None, lhs.def, rhs.def),
            RealUEQ => e.f_unord_equal(b, None, lhs.def, rhs.def),
            RealUGT => e.f_unord_greater_than(b, None, lhs.def, rhs.def),
            RealUGE => e.f_unord_greater_than_equal(b, None, lhs.def, rhs.def),
            RealULT => e.f_unord_less_than(b, None, lhs.def, rhs.def),
            RealULE => e.f_unord_less_than_equal(b, None, lhs.def, rhs.def),
            RealUNE => e.f_unord_not_equal(b, None, lhs.def, rhs.def),
        }
        .unwrap()
        .with_type(b)
    }

    fn memcpy(
        &mut self,
        _dst: Self::Value,
        _dst_align: Align,
        _src: Self::Value,
        _src_align: Align,
        _size: Self::Value,
        _flags: MemFlags,
    ) {
        todo!()
    }

    fn memmove(
        &mut self,
        _dst: Self::Value,
        _dst_align: Align,
        _src: Self::Value,
        _src_align: Align,
        _size: Self::Value,
        _flags: MemFlags,
    ) {
        todo!()
    }

    fn memset(
        &mut self,
        _ptr: Self::Value,
        _fill_byte: Self::Value,
        _size: Self::Value,
        _align: Align,
        _flags: MemFlags,
    ) {
        todo!()
    }

    fn select(
        &mut self,
        cond: Self::Value,
        then_val: Self::Value,
        else_val: Self::Value,
    ) -> Self::Value {
        assert_eq!(then_val.ty, else_val.ty);
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
            other => panic!("extract_element not implemented on type {:?}", other),
        };
        match self.builder.lookup_const_u64(idx.def) {
            Ok(const_index) => self.emit().composite_extract(
                result_type,
                None,
                vec.def,
                [const_index as u32].iter().cloned(),
            ),
            Err(_) => self
                .emit()
                .vector_extract_dynamic(result_type, None, vec.def, idx.def),
        }
        .unwrap()
        .with_type(result_type)
    }

    fn vector_splat(&mut self, num_elts: usize, elt: Self::Value) -> Self::Value {
        let u32 = SpirvType::Integer(32, false).def(self);
        let count = self.builder.constant_u32(u32, num_elts as u32);
        let result_type = SpirvType::Vector {
            element: elt.ty,
            count,
        }
        .def(self);
        if self.builder.lookup_const(elt.def).is_ok() {
            // TODO: Cache this?
            self.emit()
                .constant_composite(result_type, std::iter::repeat(elt.def).take(num_elts))
        } else {
            self.emit()
                .composite_construct(result_type, None, std::iter::repeat(elt.def).take(num_elts))
                .unwrap()
        }
        .with_type(result_type)
    }

    fn extract_value(&mut self, agg_val: Self::Value, idx: u64) -> Self::Value {
        let result_type = match self.lookup_type(agg_val.ty) {
            SpirvType::Adt { field_types } => field_types[idx as usize],
            other => panic!("extract_value not implemented on type {:?}", other),
        };
        self.emit()
            .composite_extract(result_type, None, agg_val.def, [idx as u32].iter().cloned())
            .unwrap()
            .with_type(result_type)
    }

    fn insert_value(&mut self, agg_val: Self::Value, elt: Self::Value, idx: u64) -> Self::Value {
        match self.lookup_type(agg_val.ty) {
            SpirvType::Adt { field_types } => assert_eq!(field_types[idx as usize], elt.ty),
            other => panic!("insert_value not implemented on type {:?}", other),
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
        _dst: Self::Value,
        _cmp: Self::Value,
        _src: Self::Value,
        _order: AtomicOrdering,
        _failure_order: AtomicOrdering,
        _weak: bool,
    ) -> Self::Value {
        todo!()
    }

    fn atomic_rmw(
        &mut self,
        _op: AtomicRmwBinOp,
        _dst: Self::Value,
        _src: Self::Value,
        _order: AtomicOrdering,
    ) -> Self::Value {
        todo!()
    }

    fn atomic_fence(&mut self, _order: AtomicOrdering, _scope: SynchronizationScope) {
        todo!()
    }

    fn set_invariant_load(&mut self, _load: Self::Value) {
        todo!()
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
    ) -> Self::Value {
        todo!()
    }

    fn call(
        &mut self,
        llfn: Self::Value,
        args: &[Self::Value],
        funclet: Option<&Self::Funclet>,
    ) -> Self::Value {
        if funclet.is_some() {
            panic!("TODO: Funclets are not supported");
        }
        let result_type = match self.lookup_type(llfn.ty) {
            SpirvType::Function { return_type, .. } => return_type,
            ty => panic!("Calling non-function type: {:?}", ty),
        };
        let args = args.iter().map(|arg| arg.def).collect::<Vec<_>>();
        self.emit()
            .function_call(result_type, None, llfn.def, args)
            .unwrap()
            .with_type(result_type)
    }

    unsafe fn delete_basic_block(&mut self, _bb: Self::BasicBlock) {
        todo!()
    }

    fn do_not_inline(&mut self, _llret: Self::Value) {
        todo!()
    }
}
