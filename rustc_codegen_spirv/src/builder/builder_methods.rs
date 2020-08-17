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
    fn new_block<'b>(cx: &'a Self::CodegenCx, llfn: Self::Function, name: &'b str) -> Self {
        todo!()
    }

    fn with_cx(cx: &'a Self::CodegenCx) -> Self {
        let spirv = rspirv::dr::Builder::new();
        Self { spirv, cx }
    }

    fn build_sibling_block(&self, name: &str) -> Self {
        todo!()
    }

    fn cx(&self) -> &Self::CodegenCx {
        self.cx
    }

    fn llbb(&self) -> Self::BasicBlock {
        todo!()
    }

    fn position_at_end(&mut self, llbb: Self::BasicBlock) {
        todo!()
    }

    fn ret_void(&mut self) {
        todo!()
    }

    fn ret(&mut self, v: Self::Value) {
        todo!()
    }

    fn br(&mut self, dest: Self::BasicBlock) {
        todo!()
    }

    fn cond_br(
        &mut self,
        cond: Self::Value,
        then_llbb: Self::BasicBlock,
        else_llbb: Self::BasicBlock,
    ) {
        todo!()
    }

    fn switch(
        &mut self,
        v: Self::Value,
        else_llbb: Self::BasicBlock,
        cases: impl ExactSizeIterator<Item = (u128, Self::BasicBlock)>,
    ) {
        todo!()
    }

    fn invoke(
        &mut self,
        llfn: Self::Value,
        args: &[Self::Value],
        then: Self::BasicBlock,
        catch: Self::BasicBlock,
        funclet: Option<&Self::Funclet>,
    ) -> Self::Value {
        todo!()
    }

    fn unreachable(&mut self) {
        todo!()
    }

    fn add(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn fadd(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn fadd_fast(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn sub(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn fsub(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn fsub_fast(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn mul(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn fmul(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn fmul_fast(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn udiv(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn exactudiv(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn sdiv(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn exactsdiv(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn fdiv(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn fdiv_fast(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn urem(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn srem(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn frem(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn frem_fast(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn shl(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn lshr(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn ashr(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn unchecked_sadd(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn unchecked_uadd(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn unchecked_ssub(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn unchecked_usub(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn unchecked_smul(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn unchecked_umul(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn and(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn or(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn xor(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn neg(&mut self, v: Self::Value) -> Self::Value {
        todo!()
    }

    fn fneg(&mut self, v: Self::Value) -> Self::Value {
        todo!()
    }

    fn not(&mut self, v: Self::Value) -> Self::Value {
        todo!()
    }

    fn checked_binop(
        &mut self,
        oop: OverflowOp,
        ty: Ty<'_>,
        lhs: Self::Value,
        rhs: Self::Value,
    ) -> (Self::Value, Self::Value) {
        todo!()
    }

    fn alloca(&mut self, ty: Self::Type, align: Align) -> Self::Value {
        todo!()
    }

    fn dynamic_alloca(&mut self, ty: Self::Type, align: Align) -> Self::Value {
        todo!()
    }

    fn array_alloca(&mut self, ty: Self::Type, len: Self::Value, align: Align) -> Self::Value {
        todo!()
    }

    fn load(&mut self, ptr: Self::Value, align: Align) -> Self::Value {
        todo!()
    }

    fn volatile_load(&mut self, ptr: Self::Value) -> Self::Value {
        todo!()
    }

    fn atomic_load(&mut self, ptr: Self::Value, order: AtomicOrdering, size: Size) -> Self::Value {
        todo!()
    }

    fn load_operand(
        &mut self,
        place: PlaceRef<'tcx, Self::Value>,
    ) -> OperandRef<'tcx, Self::Value> {
        todo!()
    }

    /// Called for Rvalue::Repeat when the elem is neither a ZST nor optimizable using memset.
    fn write_operand_repeatedly(
        self,
        elem: OperandRef<'tcx, Self::Value>,
        count: u64,
        dest: PlaceRef<'tcx, Self::Value>,
    ) -> Self {
        todo!()
    }

    fn range_metadata(&mut self, load: Self::Value, range: Range<u128>) {
        todo!()
    }

    fn nonnull_metadata(&mut self, load: Self::Value) {
        todo!()
    }

    fn store(&mut self, val: Self::Value, ptr: Self::Value, align: Align) -> Self::Value {
        todo!()
    }

    fn store_with_flags(
        &mut self,
        val: Self::Value,
        ptr: Self::Value,
        align: Align,
        flags: MemFlags,
    ) -> Self::Value {
        todo!()
    }

    fn atomic_store(
        &mut self,
        val: Self::Value,
        ptr: Self::Value,
        order: AtomicOrdering,
        size: Size,
    ) {
        todo!()
    }

    fn gep(&mut self, ptr: Self::Value, indices: &[Self::Value]) -> Self::Value {
        todo!()
    }

    fn inbounds_gep(&mut self, ptr: Self::Value, indices: &[Self::Value]) -> Self::Value {
        todo!()
    }

    fn struct_gep(&mut self, ptr: Self::Value, idx: u64) -> Self::Value {
        todo!()
    }

    fn trunc(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        todo!()
    }

    fn sext(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        todo!()
    }

    fn fptoui_sat(&mut self, val: Self::Value, dest_ty: Self::Type) -> Option<Self::Value> {
        todo!()
    }

    fn fptosi_sat(&mut self, val: Self::Value, dest_ty: Self::Type) -> Option<Self::Value> {
        todo!()
    }

    fn fptoui(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        todo!()
    }

    fn fptosi(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        todo!()
    }

    fn uitofp(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        todo!()
    }

    fn sitofp(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        todo!()
    }

    fn fptrunc(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        todo!()
    }

    fn fpext(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        todo!()
    }

    fn ptrtoint(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        todo!()
    }

    fn inttoptr(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        todo!()
    }

    fn bitcast(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        todo!()
    }

    fn intcast(&mut self, val: Self::Value, dest_ty: Self::Type, is_signed: bool) -> Self::Value {
        todo!()
    }

    fn pointercast(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        todo!()
    }

    fn icmp(&mut self, op: IntPredicate, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn fcmp(&mut self, op: RealPredicate, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn memcpy(
        &mut self,
        dst: Self::Value,
        dst_align: Align,
        src: Self::Value,
        src_align: Align,
        size: Self::Value,
        flags: MemFlags,
    ) {
        todo!()
    }

    fn memmove(
        &mut self,
        dst: Self::Value,
        dst_align: Align,
        src: Self::Value,
        src_align: Align,
        size: Self::Value,
        flags: MemFlags,
    ) {
        todo!()
    }

    fn memset(
        &mut self,
        ptr: Self::Value,
        fill_byte: Self::Value,
        size: Self::Value,
        align: Align,
        flags: MemFlags,
    ) {
        todo!()
    }

    fn select(
        &mut self,
        cond: Self::Value,
        then_val: Self::Value,
        else_val: Self::Value,
    ) -> Self::Value {
        todo!()
    }

    fn va_arg(&mut self, list: Self::Value, ty: Self::Type) -> Self::Value {
        todo!()
    }

    fn extract_element(&mut self, vec: Self::Value, idx: Self::Value) -> Self::Value {
        todo!()
    }

    fn vector_splat(&mut self, num_elts: usize, elt: Self::Value) -> Self::Value {
        todo!()
    }

    fn extract_value(&mut self, agg_val: Self::Value, idx: u64) -> Self::Value {
        todo!()
    }

    fn insert_value(&mut self, agg_val: Self::Value, elt: Self::Value, idx: u64) -> Self::Value {
        todo!()
    }

    fn landing_pad(
        &mut self,
        ty: Self::Type,
        pers_fn: Self::Value,
        num_clauses: usize,
    ) -> Self::Value {
        todo!()
    }

    fn set_cleanup(&mut self, landing_pad: Self::Value) {
        todo!()
    }

    fn resume(&mut self, exn: Self::Value) -> Self::Value {
        todo!()
    }

    fn cleanup_pad(&mut self, parent: Option<Self::Value>, args: &[Self::Value]) -> Self::Funclet {
        todo!()
    }

    fn cleanup_ret(
        &mut self,
        funclet: &Self::Funclet,
        unwind: Option<Self::BasicBlock>,
    ) -> Self::Value {
        todo!()
    }

    fn catch_pad(&mut self, parent: Self::Value, args: &[Self::Value]) -> Self::Funclet {
        todo!()
    }

    fn catch_switch(
        &mut self,
        parent: Option<Self::Value>,
        unwind: Option<Self::BasicBlock>,
        num_handlers: usize,
    ) -> Self::Value {
        todo!()
    }

    fn add_handler(&mut self, catch_switch: Self::Value, handler: Self::BasicBlock) {
        todo!()
    }

    fn set_personality_fn(&mut self, personality: Self::Value) {
        todo!()
    }

    fn atomic_cmpxchg(
        &mut self,
        dst: Self::Value,
        cmp: Self::Value,
        src: Self::Value,
        order: AtomicOrdering,
        failure_order: AtomicOrdering,
        weak: bool,
    ) -> Self::Value {
        todo!()
    }

    fn atomic_rmw(
        &mut self,
        op: AtomicRmwBinOp,
        dst: Self::Value,
        src: Self::Value,
        order: AtomicOrdering,
    ) -> Self::Value {
        todo!()
    }

    fn atomic_fence(&mut self, order: AtomicOrdering, scope: SynchronizationScope) {
        todo!()
    }

    fn set_invariant_load(&mut self, load: Self::Value) {
        todo!()
    }

    /// Called for `StorageLive`
    fn lifetime_start(&mut self, ptr: Self::Value, size: Size) {
        todo!()
    }

    /// Called for `StorageDead`
    fn lifetime_end(&mut self, ptr: Self::Value, size: Size) {
        todo!()
    }

    fn instrprof_increment(
        &mut self,
        fn_name: Self::Value,
        hash: Self::Value,
        num_counters: Self::Value,
        index: Self::Value,
    ) -> Self::Value {
        todo!()
    }

    fn call(
        &mut self,
        llfn: Self::Value,
        args: &[Self::Value],
        funclet: Option<&Self::Funclet>,
    ) -> Self::Value {
        todo!()
    }

    fn zext(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        todo!()
    }

    unsafe fn delete_basic_block(&mut self, bb: Self::BasicBlock) {
        todo!()
    }

    fn do_not_inline(&mut self, llret: Self::Value) {
        todo!()
    }
}
