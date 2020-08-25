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
        _llfn: Self::Value,
        _args: &[Self::Value],
        _then: Self::BasicBlock,
        _catch: Self::BasicBlock,
        _funclet: Option<&Self::Funclet>,
    ) -> Self::Value {
        todo!()
    }

    fn unreachable(&mut self) {
        self.emit().unreachable().unwrap()
    }

    fn add(&mut self, _lhs: Self::Value, _rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn fadd(&mut self, _lhs: Self::Value, _rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn fadd_fast(&mut self, _lhs: Self::Value, _rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn sub(&mut self, _lhs: Self::Value, _rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn fsub(&mut self, _lhs: Self::Value, _rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn fsub_fast(&mut self, _lhs: Self::Value, _rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn mul(&mut self, _lhs: Self::Value, _rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn fmul(&mut self, _lhs: Self::Value, _rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn fmul_fast(&mut self, _lhs: Self::Value, _rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn udiv(&mut self, _lhs: Self::Value, _rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn exactudiv(&mut self, _lhs: Self::Value, _rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn sdiv(&mut self, _lhs: Self::Value, _rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn exactsdiv(&mut self, _lhs: Self::Value, _rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn fdiv(&mut self, _lhs: Self::Value, _rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn fdiv_fast(&mut self, _lhs: Self::Value, _rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn urem(&mut self, _lhs: Self::Value, _rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn srem(&mut self, _lhs: Self::Value, _rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn frem(&mut self, _lhs: Self::Value, _rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn frem_fast(&mut self, _lhs: Self::Value, _rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn shl(&mut self, _lhs: Self::Value, _rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn lshr(&mut self, _lhs: Self::Value, _rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn ashr(&mut self, _lhs: Self::Value, _rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn unchecked_sadd(&mut self, _lhs: Self::Value, _rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn unchecked_uadd(&mut self, _lhs: Self::Value, _rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn unchecked_ssub(&mut self, _lhs: Self::Value, _rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn unchecked_usub(&mut self, _lhs: Self::Value, _rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn unchecked_smul(&mut self, _lhs: Self::Value, _rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn unchecked_umul(&mut self, _lhs: Self::Value, _rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn and(&mut self, _lhs: Self::Value, _rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn or(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        assert_eq!(lhs.ty, rhs.ty);
        let result_type = lhs.ty;
        self.emit()
            .bitwise_or(result_type, None, lhs.def, rhs.def)
            .unwrap()
            .with_type(result_type)
    }

    fn xor(&mut self, _lhs: Self::Value, _rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn neg(&mut self, _v: Self::Value) -> Self::Value {
        todo!()
    }

    fn fneg(&mut self, _v: Self::Value) -> Self::Value {
        todo!()
    }

    fn not(&mut self, _v: Self::Value) -> Self::Value {
        todo!()
    }

    fn checked_binop(
        &mut self,
        _oop: OverflowOp,
        _ty: Ty<'_>,
        _lhs: Self::Value,
        _rhs: Self::Value,
    ) -> (Self::Value, Self::Value) {
        todo!()
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
        todo!()
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
        todo!()
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
        todo!()
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

    fn trunc(&mut self, _val: Self::Value, _dest_ty: Self::Type) -> Self::Value {
        todo!()
    }

    fn sext(&mut self, _val: Self::Value, _dest_ty: Self::Type) -> Self::Value {
        todo!()
    }

    fn fptoui_sat(&mut self, _val: Self::Value, _dest_ty: Self::Type) -> Option<Self::Value> {
        todo!()
    }

    fn fptosi_sat(&mut self, _val: Self::Value, _dest_ty: Self::Type) -> Option<Self::Value> {
        todo!()
    }

    fn fptoui(&mut self, _val: Self::Value, _dest_ty: Self::Type) -> Self::Value {
        todo!()
    }

    fn fptosi(&mut self, _val: Self::Value, _dest_ty: Self::Type) -> Self::Value {
        todo!()
    }

    fn uitofp(&mut self, _val: Self::Value, _dest_ty: Self::Type) -> Self::Value {
        todo!()
    }

    fn sitofp(&mut self, _val: Self::Value, _dest_ty: Self::Type) -> Self::Value {
        todo!()
    }

    fn fptrunc(&mut self, _val: Self::Value, _dest_ty: Self::Type) -> Self::Value {
        todo!()
    }

    fn fpext(&mut self, _val: Self::Value, _dest_ty: Self::Type) -> Self::Value {
        todo!()
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

    fn icmp(&mut self, _op: IntPredicate, _lhs: Self::Value, _rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn fcmp(&mut self, _op: RealPredicate, _lhs: Self::Value, _rhs: Self::Value) -> Self::Value {
        todo!()
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
        _cond: Self::Value,
        _then_val: Self::Value,
        _else_val: Self::Value,
    ) -> Self::Value {
        todo!()
    }

    fn va_arg(&mut self, _list: Self::Value, _ty: Self::Type) -> Self::Value {
        todo!()
    }

    fn extract_element(&mut self, _vec: Self::Value, _idx: Self::Value) -> Self::Value {
        todo!()
    }

    fn vector_splat(&mut self, _num_elts: usize, _elt: Self::Value) -> Self::Value {
        todo!()
    }

    fn extract_value(&mut self, _agg_val: Self::Value, _idx: u64) -> Self::Value {
        todo!()
    }

    fn insert_value(&mut self, _agg_val: Self::Value, _elt: Self::Value, _idx: u64) -> Self::Value {
        todo!()
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

    fn zext(&mut self, _val: Self::Value, _dest_ty: Self::Type) -> Self::Value {
        todo!()
    }

    unsafe fn delete_basic_block(&mut self, _bb: Self::BasicBlock) {
        todo!()
    }

    fn do_not_inline(&mut self, _llret: Self::Value) {
        todo!()
    }
}
