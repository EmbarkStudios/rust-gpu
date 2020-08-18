use super::Builder;
use rustc_codegen_ssa::common::{
    AtomicOrdering, AtomicRmwBinOp, IntPredicate, RealPredicate, SynchronizationScope,
};
use rustc_codegen_ssa::mir::operand::OperandRef;
use rustc_codegen_ssa::mir::place::PlaceRef;
use rustc_codegen_ssa::traits::{BuilderMethods, OverflowOp};
use rustc_codegen_ssa::MemFlags;
use rustc_middle::ty::Ty;
use rustc_target::abi::{Align, Size};
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
        let cursor = cx.builder.select_function_by_id(llfn);
        let label = cx.emit_with_cursor(cursor).begin_block(None).unwrap();
        Self {
            cx,
            cursor,
            current_fn: llfn,
            basic_block: label,
        }
    }

    fn build_sibling_block(&self, _name: &str) -> Self {
        todo!()
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
        self.emit().ret_value(value).unwrap();
    }

    fn br(&mut self, _dest: Self::BasicBlock) {
        todo!()
    }

    fn cond_br(
        &mut self,
        _cond: Self::Value,
        _then_llbb: Self::BasicBlock,
        _else_llbb: Self::BasicBlock,
    ) {
        todo!()
    }

    fn switch(
        &mut self,
        _v: Self::Value,
        _else_llbb: Self::BasicBlock,
        _cases: impl ExactSizeIterator<Item = (u128, Self::BasicBlock)>,
    ) {
        todo!()
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
        todo!()
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
        // TODO: implement result_type
        let result_type = Default::default();
        self.emit().bitwise_or(result_type, None, lhs, rhs).unwrap()
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

    fn alloca(&mut self, _ty: Self::Type, _align: Align) -> Self::Value {
        todo!()
    }

    fn dynamic_alloca(&mut self, _ty: Self::Type, _align: Align) -> Self::Value {
        todo!()
    }

    fn array_alloca(&mut self, _ty: Self::Type, _len: Self::Value, _align: Align) -> Self::Value {
        todo!()
    }

    fn load(&mut self, _ptr: Self::Value, _align: Align) -> Self::Value {
        todo!()
    }

    fn volatile_load(&mut self, _ptr: Self::Value) -> Self::Value {
        todo!()
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
        _place: PlaceRef<'tcx, Self::Value>,
    ) -> OperandRef<'tcx, Self::Value> {
        todo!()
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

    fn store(&mut self, _val: Self::Value, _ptr: Self::Value, _align: Align) -> Self::Value {
        todo!()
    }

    fn store_with_flags(
        &mut self,
        _val: Self::Value,
        _ptr: Self::Value,
        _align: Align,
        _flags: MemFlags,
    ) -> Self::Value {
        todo!()
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

    fn gep(&mut self, _ptr: Self::Value, _indices: &[Self::Value]) -> Self::Value {
        todo!()
    }

    fn inbounds_gep(&mut self, _ptr: Self::Value, _indices: &[Self::Value]) -> Self::Value {
        todo!()
    }

    fn struct_gep(&mut self, _ptr: Self::Value, _idx: u64) -> Self::Value {
        todo!()
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

    fn ptrtoint(&mut self, _val: Self::Value, _dest_ty: Self::Type) -> Self::Value {
        todo!()
    }

    fn inttoptr(&mut self, _val: Self::Value, _dest_ty: Self::Type) -> Self::Value {
        todo!()
    }

    fn bitcast(&mut self, _val: Self::Value, _dest_ty: Self::Type) -> Self::Value {
        todo!()
    }

    fn intcast(
        &mut self,
        _val: Self::Value,
        _dest_ty: Self::Type,
        _is_signed: bool,
    ) -> Self::Value {
        todo!()
    }

    fn pointercast(&mut self, _val: Self::Value, _dest_ty: Self::Type) -> Self::Value {
        todo!()
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
        todo!()
    }

    /// Called for `StorageDead`
    fn lifetime_end(&mut self, _ptr: Self::Value, _size: Size) {
        todo!()
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
        _llfn: Self::Value,
        _args: &[Self::Value],
        _funclet: Option<&Self::Funclet>,
    ) -> Self::Value {
        todo!()
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
