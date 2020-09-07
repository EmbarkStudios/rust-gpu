mod builder_methods;

use crate::abi::ConvSpirvType;
use crate::builder_spirv::{BuilderCursor, SpirvValue, SpirvValueExt};
use crate::codegen_cx::CodegenCx;
use crate::spirv_type::SpirvType;
use rspirv::spirv::StorageClass;
use rustc_ast::ast::{InlineAsmOptions, InlineAsmTemplatePiece};
use rustc_codegen_ssa::common::span_invalid_monomorphization_error;
use rustc_codegen_ssa::common::IntPredicate;
use rustc_codegen_ssa::glue;
use rustc_codegen_ssa::mir::operand::{OperandRef, OperandValue};
use rustc_codegen_ssa::mir::place::PlaceRef;
use rustc_codegen_ssa::traits::{
    AbiBuilderMethods, ArgAbiMethods, AsmBuilderMethods, BackendTypes, BaseTypeMethods,
    BuilderMethods, ConstMethods, CoverageInfoBuilderMethods, DebugInfoBuilderMethods, HasCodegen,
    InlineAsmOperandRef, IntrinsicCallMethods, OverflowOp, StaticBuilderMethods,
};
use rustc_codegen_ssa::MemFlags;
use rustc_hir::LlvmInlineAsmInner;
use rustc_middle::bug;
use rustc_middle::mir::coverage::{
    CodeRegion, CounterValueReference, ExpressionOperandId, InjectedExpressionIndex, Op,
};
use rustc_middle::ty::layout::{HasParamEnv, HasTyCtxt, TyAndLayout};
use rustc_middle::ty::{FnDef, Instance, ParamEnv, Ty, TyCtxt, TyKind};
use rustc_span::def_id::DefId;
use rustc_span::source_map::Span;
use rustc_span::sym;
use rustc_target::abi::call::{ArgAbi, FnAbi, PassMode};
use rustc_target::abi::{HasDataLayout, LayoutOf, Size, TargetDataLayout};
use rustc_target::spec::{HasTargetSpec, Target};
use std::ops::Deref;

macro_rules! math_intrinsic {
    ($self:ident, $arg_tys:ident, $args:ident, $int:ident, $uint:ident, $float:ident) => {{
        assert_eq!($arg_tys[0], $arg_tys[1]);
        match &$arg_tys[0].kind() {
            TyKind::Int(_) => $self.$int($args[0].immediate(), $args[1].immediate()),
            TyKind::Uint(_) => $self.$uint($args[0].immediate(), $args[1].immediate()),
            TyKind::Float(_) => $self.$float($args[0].immediate(), $args[1].immediate()),
            other => panic!("Unimplemented intrinsic type: {:#?}", other),
        }
    }};
}
macro_rules! math_intrinsic_int {
    ($self:ident, $arg_tys:ident, $args:ident, $int:ident, $uint:ident) => {{
        assert_eq!($arg_tys[0], $arg_tys[1]);
        match &$arg_tys[0].kind() {
            TyKind::Int(_) => $self.$int($args[0].immediate(), $args[1].immediate()),
            TyKind::Uint(_) => $self.$uint($args[0].immediate(), $args[1].immediate()),
            other => panic!("Unimplemented intrinsic type: {:#?}", other),
        }
    }};
}

pub struct Builder<'a, 'spv, 'tcx> {
    cx: &'a CodegenCx<'spv, 'tcx>,
    cursor: BuilderCursor,
    current_fn: <Self as BackendTypes>::Function,
    basic_block: <Self as BackendTypes>::BasicBlock,
}

impl<'a, 'spv, 'tcx> Builder<'a, 'spv, 'tcx> {
    pub fn emit(&self) -> std::cell::RefMut<rspirv::dr::Builder> {
        self.emit_with_cursor(self.cursor)
    }

    pub fn gep_help(
        &self,
        ptr: SpirvValue,
        indices: &[SpirvValue],
        is_inbounds: bool,
    ) -> SpirvValue {
        // The first index is an offset to the pointer, the rest are actual members.
        // https://llvm.org/docs/GetElementPtr.html
        // "An OpAccessChain instruction is the equivalent of an LLVM getelementptr instruction where the first index element is zero."
        // https://github.com/gpuweb/gpuweb/issues/33
        let mut result_indices = Vec::with_capacity(indices.len() - 1);
        let (storage_class, mut result_pointee_type) = match self.lookup_type(ptr.ty) {
            SpirvType::Pointer {
                storage_class,
                pointee,
            } => (storage_class, pointee),
            other_type => panic!("GEP first deref not implemented for type {:?}", other_type),
        };
        for index in indices.iter().cloned().skip(1) {
            result_indices.push(index.def);
            result_pointee_type = match self.lookup_type(result_pointee_type) {
                SpirvType::Array { element, .. } | SpirvType::RuntimeArray { element } => element,
                _ => panic!(
                    "GEP not implemented for type {}",
                    self.debug_type(result_pointee_type)
                ),
            };
        }
        let result_type = SpirvType::Pointer {
            storage_class,
            pointee: result_pointee_type,
        }
        .def(self);
        if self.builder.lookup_const_u64(indices[0].def) == Ok(0) {
            if is_inbounds {
                self.emit()
                    .in_bounds_access_chain(result_type, None, ptr.def, result_indices)
                    .unwrap()
                    .with_type(result_type)
            } else {
                self.emit()
                    .access_chain(result_type, None, ptr.def, result_indices)
                    .unwrap()
                    .with_type(result_type)
            }
        } else if is_inbounds {
            self.emit()
                .in_bounds_ptr_access_chain(
                    result_type,
                    None,
                    ptr.def,
                    indices[0].def,
                    result_indices,
                )
                .unwrap()
                .with_type(result_type)
        } else {
            self.emit()
                .ptr_access_chain(result_type, None, ptr.def, indices[0].def, result_indices)
                .unwrap()
                .with_type(result_type)
        }
    }

    // TODO: Definitely add tests to make sure this impl is right.
    fn rotate(&mut self, value: SpirvValue, shift: SpirvValue, is_left: bool) -> SpirvValue {
        let (int_size, mask, zero) = match self.lookup_type(shift.ty) {
            SpirvType::Integer(width, _) => {
                if width > 32 {
                    (
                        self.builder
                            .constant_u64(shift.ty, width as u64)
                            .with_type(shift.ty),
                        self.builder
                            .constant_u64(shift.ty, (width - 1) as u64)
                            .with_type(shift.ty),
                        self.builder.constant_u64(shift.ty, 0).with_type(shift.ty),
                    )
                } else {
                    (
                        self.builder
                            .constant_u32(shift.ty, width as u32)
                            .with_type(shift.ty),
                        self.builder
                            .constant_u32(shift.ty, (width - 1) as u32)
                            .with_type(shift.ty),
                        self.builder.constant_u32(shift.ty, 0).with_type(shift.ty),
                    )
                }
            }
            other => panic!(
                "Cannot rotate non-integer type: {}",
                other.debug(shift.ty, self)
            ),
        };
        let bool = SpirvType::Bool.def(self);
        // https://stackoverflow.com/a/10134877
        let mask_shift = self.and(shift, mask);
        let sub = self.sub(int_size, mask_shift);
        let (lhs, rhs) = if is_left {
            (self.shl(value, mask_shift), self.lshr(value, sub))
        } else {
            (self.lshr(value, mask_shift), self.shl(value, sub))
        };
        let or = self.or(lhs, rhs);
        // "The result is undefined if Shift is greater than or equal to the bit width of the components of Base."
        // So we need to check for zero shift, and don't use the shift result if it is.
        let mask_is_zero = self
            .emit()
            .i_not_equal(bool, None, mask_shift.def, zero.def)
            .unwrap()
            .with_type(bool);
        self.select(mask_is_zero, value, or)
    }
}

// Important: This lets us use CodegenCx methods on Builder
impl<'a, 'spv, 'tcx> Deref for Builder<'a, 'spv, 'tcx> {
    type Target = CodegenCx<'spv, 'tcx>;

    fn deref(&self) -> &Self::Target {
        self.cx
    }
}

impl<'a, 'spv, 'tcx> CoverageInfoBuilderMethods<'tcx> for Builder<'a, 'spv, 'tcx> {
    fn create_pgo_func_name_var(&self, _instance: Instance<'tcx>) -> Self::Value {
        todo!()
    }

    fn add_counter_region(
        &mut self,
        _instance: Instance<'tcx>,
        _function_source_hash: u64,
        _id: CounterValueReference,
        _region: CodeRegion,
    ) {
        todo!()
    }

    fn add_counter_expression_region(
        &mut self,
        _instance: Instance<'tcx>,
        _id: InjectedExpressionIndex,
        _lhs: ExpressionOperandId,
        _op: Op,
        _rhs: ExpressionOperandId,
        _region: CodeRegion,
    ) {
        todo!()
    }

    fn add_unreachable_region(&mut self, _instance: Instance<'tcx>, _region: CodeRegion) {
        todo!()
    }
}

impl<'a, 'spv, 'tcx> DebugInfoBuilderMethods for Builder<'a, 'spv, 'tcx> {
    fn dbg_var_addr(
        &mut self,
        _dbg_var: Self::DIVariable,
        _scope_metadata: Self::DIScope,
        _variable_alloca: Self::Value,
        _direct_offset: Size,
        // NB: each offset implies a deref (i.e. they're steps in a pointer chain).
        _indirect_offsets: &[Size],
        _span: Span,
    ) {
        todo!()
    }

    fn set_source_location(&mut self, _scope: Self::DIScope, _span: Span) {
        todo!()
    }

    fn insert_reference_to_gdb_debug_scripts_section_global(&mut self) {
        todo!()
    }

    fn set_var_name(&mut self, _value: Self::Value, _name: &str) {
        todo!()
    }
}

impl<'a, 'spv, 'tcx> ArgAbiMethods<'tcx> for Builder<'a, 'spv, 'tcx> {
    fn store_fn_arg(
        &mut self,
        arg_abi: &ArgAbi<'tcx, Ty<'tcx>>,
        idx: &mut usize,
        dst: PlaceRef<'tcx, Self::Value>,
    ) {
        fn next<'a, 'spv, 'tcx>(bx: &mut Builder<'a, 'spv, 'tcx>, idx: &mut usize) -> SpirvValue {
            let val = bx.function_parameter_values.borrow()[&bx.current_fn.def][*idx];
            *idx += 1;
            val
        }
        match arg_abi.mode {
            PassMode::Ignore => (),
            PassMode::Pair(..) => {
                OperandValue::Pair(next(self, idx), next(self, idx)).store(self, dst)
            }
            PassMode::Indirect(_, Some(_)) => OperandValue::Ref(
                next(self, idx),
                Some(next(self, idx)),
                arg_abi.layout.align.abi,
            )
            .store(self, dst),
            PassMode::Direct(_) | PassMode::Indirect(_, None) | PassMode::Cast(_) => {
                let next_arg = next(self, idx);
                self.store_arg(arg_abi, next_arg, dst)
            }
        }
    }

    fn store_arg(
        &mut self,
        arg_abi: &ArgAbi<'tcx, Ty<'tcx>>,
        val: Self::Value,
        dst: PlaceRef<'tcx, Self::Value>,
    ) {
        if arg_abi.is_ignore() {
            return;
        }
        if arg_abi.is_sized_indirect() {
            OperandValue::Ref(val, None, arg_abi.layout.align.abi).store(self, dst);
        } else if arg_abi.is_unsized_indirect() {
            panic!("unsized `ArgAbi` must be handled through `store_fn_arg`");
        } else if let PassMode::Cast(cast) = arg_abi.mode {
            let cast_ty = cast.spirv_type(self);
            let cast_ptr_ty = SpirvType::Pointer {
                storage_class: StorageClass::Generic,
                pointee: cast_ty,
            }
            .def(self);
            let cast_dst = self.pointercast(dst.llval, cast_ptr_ty);
            self.store(val, cast_dst, arg_abi.layout.align.abi);
        } else {
            // TODO: Does this need a from_immediate? The LLVM backend doesn't have one here.
            OperandValue::Immediate(val).store(self, dst);
        }
    }

    fn arg_memory_ty(&self, arg_abi: &ArgAbi<'tcx, Ty<'tcx>>) -> Self::Type {
        arg_abi.layout.spirv_type(self)
    }
}

impl<'a, 'spv, 'tcx> AbiBuilderMethods<'tcx> for Builder<'a, 'spv, 'tcx> {
    fn apply_attrs_callsite(&mut self, _fn_abi: &FnAbi<'tcx, Ty<'tcx>>, _callsite: Self::Value) {
        // TODO: Implement this?
    }

    fn get_param(&self, index: usize) -> Self::Value {
        self.function_parameter_values.borrow()[&self.current_fn.def][index]
    }
}

fn int_type_width_signed(ty: Ty<'_>, cx: &CodegenCx<'_, '_>) -> Option<(u64, bool)> {
    match ty.kind() {
        TyKind::Int(t) => Some((
            t.bit_width().unwrap_or(cx.tcx.sess.target.ptr_width as u64),
            true,
        )),
        TyKind::Uint(t) => Some((
            t.bit_width().unwrap_or(cx.tcx.sess.target.ptr_width as u64),
            false,
        )),
        _ => None,
    }
}

fn float_type_width(ty: Ty<'_>) -> Option<u64> {
    match ty.kind() {
        TyKind::Float(t) => Some(t.bit_width()),
        _ => None,
    }
}

impl<'a, 'spv, 'tcx> IntrinsicCallMethods<'tcx> for Builder<'a, 'spv, 'tcx> {
    fn codegen_intrinsic_call(
        &mut self,
        instance: Instance<'tcx>,
        fn_abi: &FnAbi<'tcx, Ty<'tcx>>,
        args: &[OperandRef<'tcx, Self::Value>],
        llresult: Self::Value,
        span: Span,
    ) {
        let callee_ty = instance.ty(self.tcx, ParamEnv::reveal_all());

        let (def_id, substs) = match *callee_ty.kind() {
            FnDef(def_id, substs) => (def_id, substs),
            _ => panic!("expected fn item type, found {}", callee_ty),
        };

        let sig = callee_ty.fn_sig(self.tcx);
        let sig = self
            .tcx
            .normalize_erasing_late_bound_regions(ParamEnv::reveal_all(), &sig);
        let arg_tys = sig.inputs();
        let ret_ty = sig.output();
        let name = self.tcx.item_name(def_id);
        // let name_str = &*name.as_str();

        // let spirv_ret_ty = self.trans_type(self.layout_of(ret_ty));
        let result = PlaceRef::new_sized(llresult, fn_abi.ret.layout);

        let value = match name {
            sym::size_of_val => {
                let tp_ty = substs.type_at(0);
                if let OperandValue::Pair(_, meta) = args[0].val {
                    let (llsize, _) = glue::size_and_align_of_dst(self, tp_ty, Some(meta));
                    llsize
                } else {
                    self.const_usize(self.size_of(tp_ty).bytes())
                }
            }
            sym::min_align_of_val => {
                let tp_ty = substs.type_at(0);
                if let OperandValue::Pair(_, meta) = args[0].val {
                    let (_, llalign) = glue::size_and_align_of_dst(self, tp_ty, Some(meta));
                    llalign
                } else {
                    self.const_usize(self.align_of(tp_ty).bytes())
                }
            }
            sym::size_of
            | sym::pref_align_of
            | sym::min_align_of
            | sym::needs_drop
            | sym::type_id
            | sym::type_name
            | sym::variant_count => {
                let value = self
                    .tcx
                    .const_eval_instance(ParamEnv::reveal_all(), instance, None)
                    .unwrap();
                OperandRef::from_const(self, value, ret_ty).immediate_or_packed_pair(self)
            }

            sym::copy_nonoverlapping
            | sym::copy
            | sym::volatile_copy_nonoverlapping_memory
            | sym::volatile_copy_memory => {
                let dst = args[0].immediate();
                let src = args[1].immediate();
                let count = args[2].immediate();
                self.memcpy(
                    dst,
                    self.align_of(arg_tys[0]),
                    src,
                    self.align_of(arg_tys[1]),
                    count,
                    MemFlags::empty(),
                );
                assert!(fn_abi.ret.is_ignore());
                return;
            }
            sym::write_bytes | sym::volatile_set_memory => {
                let ty = substs.type_at(0);
                let dst = args[0].immediate();
                let val = args[1].immediate();
                let count = args[2].immediate();
                self.memset(dst, val, count, self.align_of(ty), MemFlags::empty());
                assert!(fn_abi.ret.is_ignore());
                return;
            }
            sym::volatile_load | sym::unaligned_volatile_load => {
                let tp_ty = substs.type_at(0);
                let mut ptr = args[0].immediate();
                if let PassMode::Cast(ty) = fn_abi.ret.mode {
                    ptr = self.pointercast(ptr, self.type_ptr_to(ty.spirv_type(self)));
                }
                let load = self.volatile_load(ptr);
                self.to_immediate(load, self.layout_of(tp_ty))
            }
            sym::volatile_store => {
                // rust-analyzer gets sad if you call args[0].deref()
                let dst = OperandRef::deref(args[0], self.cx());
                args[1].val.volatile_store(self, dst);
                return;
            }
            sym::unaligned_volatile_store => {
                // rust-analyzer gets sad if you call args[0].deref()
                let dst = OperandRef::deref(args[0], self.cx());
                args[1].val.unaligned_volatile_store(self, dst);
                return;
            }
            sym::prefetch_read_data
            | sym::prefetch_write_data
            | sym::prefetch_read_instruction
            | sym::prefetch_write_instruction => {
                // ignore
                assert!(fn_abi.ret.is_ignore());
                return;
            }

            sym::offset => {
                let ptr = args[0].immediate();
                let offset = args[1].immediate();
                self.inbounds_gep(ptr, &[offset])
            }
            sym::arith_offset => {
                let ptr = args[0].immediate();
                let offset = args[1].immediate();
                self.gep(ptr, &[offset])
            }

            sym::discriminant_value => {
                if ret_ty.is_integral() {
                    // rust-analyzer gets sad if you call args[0].deref()
                    OperandRef::deref(args[0], self.cx).codegen_get_discr(self, ret_ty)
                } else {
                    panic!("Invalid discriminant type for `{:?}`", arg_tys[0])
                }
            }

            sym::nontemporal_store => {
                // rust-analyzer gets sad if you call args[0].deref()
                let dst = OperandRef::deref(args[0], self.cx);
                args[1].val.nontemporal_store(self, dst);
                return;
            }

            sym::ptr_guaranteed_eq | sym::ptr_guaranteed_ne => {
                let a = args[0].immediate();
                let b = args[1].immediate();
                if name == sym::ptr_guaranteed_eq {
                    self.icmp(IntPredicate::IntEQ, a, b)
                } else {
                    self.icmp(IntPredicate::IntNE, a, b)
                }
            }

            sym::ptr_offset_from => {
                let ty = substs.type_at(0);
                let pointee_size = self.size_of(ty);

                // This is the same sequence that Clang emits for pointer subtraction.
                // It can be neither `nsw` nor `nuw` because the input is treated as
                // unsigned but then the output is treated as signed, so neither works.
                let a = args[0].immediate();
                let b = args[1].immediate();
                let a = self.ptrtoint(a, self.type_isize());
                let b = self.ptrtoint(b, self.type_isize());
                let d = self.sub(a, b);
                let pointee_size = self.const_usize(pointee_size.bytes());
                // this is where the signed magic happens (notice the `s` in `exactsdiv`)
                self.exactsdiv(d, pointee_size)
            }

            sym::forget => {
                // Effectively no-op
                assert!(fn_abi.ret.is_ignore());
                return;
            }

            sym::assume => {
                // Drop @llvm.assume(i1 %cond). TODO: Is there a spir-v equivalent?
                assert!(fn_abi.ret.is_ignore());
                return;
            }
            sym::likely | sym::unlikely => {
                // Ignore these for now.
                args[0].immediate()
            }

            sym::abort | sym::breakpoint => {
                self.abort();
                assert!(fn_abi.ret.is_ignore());
                return;
            }

            sym::unreachable => {
                self.unreachable();
                assert!(fn_abi.ret.is_ignore());
                return;
            }

            sym::add_with_overflow | sym::sub_with_overflow | sym::mul_with_overflow => {
                let op = match name {
                    sym::add_with_overflow => OverflowOp::Add,
                    sym::sub_with_overflow => OverflowOp::Sub,
                    sym::mul_with_overflow => OverflowOp::Mul,
                    _ => panic!(),
                };
                let (val, overflow) =
                    self.checked_binop(op, arg_tys[0], args[0].immediate(), args[1].immediate());
                // Ret type is (int, u8), not (int, bool), due to not-immediate type rules.
                let u8 = SpirvType::Integer(8, false).def(self);
                let overflow = self.zext(overflow, u8);
                let dest = result.project_field(self, 0);
                self.store(val, dest.llval, dest.align);
                let dest = result.project_field(self, 1);
                self.store(overflow, dest.llval, dest.align);
                return;
            }
            sym::wrapping_add => math_intrinsic! {self, arg_tys, args, add, add, fadd},
            sym::wrapping_sub => math_intrinsic! {self, arg_tys, args, sub, sub, fsub},
            sym::wrapping_mul => math_intrinsic! {self, arg_tys, args, mul, mul, fmul},
            sym::saturating_add => math_intrinsic! {self, arg_tys, args, add, add, fadd},
            sym::saturating_sub => math_intrinsic! {self, arg_tys, args, sub, sub, fsub},
            sym::unchecked_add => math_intrinsic! {self, arg_tys, args, add, add, fadd},
            sym::unchecked_sub => math_intrinsic! {self, arg_tys, args, sub, sub, fsub},
            sym::unchecked_mul => math_intrinsic! {self, arg_tys, args, mul, mul, fmul},
            sym::unchecked_div => math_intrinsic! {self, arg_tys, args, sdiv, udiv, fdiv},
            sym::unchecked_rem => math_intrinsic! {self, arg_tys, args, srem, urem, frem},
            sym::unchecked_shl => math_intrinsic_int! {self, arg_tys, args, shl, shl},
            sym::unchecked_shr => math_intrinsic_int! {self, arg_tys, args, ashr, lshr},
            sym::exact_div => math_intrinsic! {self, arg_tys, args, sdiv, udiv, fdiv},

            sym::fadd_fast | sym::fsub_fast | sym::fmul_fast | sym::fdiv_fast | sym::frem_fast => {
                match float_type_width(arg_tys[0]) {
                    Some(_width) => match name {
                        sym::fadd_fast => self.fadd_fast(args[0].immediate(), args[1].immediate()),
                        sym::fsub_fast => self.fsub_fast(args[0].immediate(), args[1].immediate()),
                        sym::fmul_fast => self.fmul_fast(args[0].immediate(), args[1].immediate()),
                        sym::fdiv_fast => self.fdiv_fast(args[0].immediate(), args[1].immediate()),
                        sym::frem_fast => self.frem_fast(args[0].immediate(), args[1].immediate()),
                        _ => bug!(),
                    },
                    None => {
                        span_invalid_monomorphization_error(
                            self.tcx.sess,
                            span,
                            &format!(
                                "invalid monomorphization of `{}` intrinsic: \
                                      expected basic float type, found `{}`",
                                name, arg_tys[0]
                            ),
                        );
                        return;
                    }
                }
            }

            sym::float_to_int_unchecked => {
                if float_type_width(arg_tys[0]).is_none() {
                    span_invalid_monomorphization_error(
                        self.tcx.sess,
                        span,
                        &format!(
                            "invalid monomorphization of `float_to_int_unchecked` \
                                  intrinsic: expected basic float type, \
                                  found `{}`",
                            arg_tys[0]
                        ),
                    );
                    return;
                }
                let (width, signed) = match int_type_width_signed(ret_ty, self.cx) {
                    Some(pair) => pair,
                    None => {
                        span_invalid_monomorphization_error(
                            self.tcx.sess,
                            span,
                            &format!(
                                "invalid monomorphization of `float_to_int_unchecked` \
                                      intrinsic:  expected basic integer type, \
                                      found `{}`",
                                ret_ty
                            ),
                        );
                        return;
                    }
                };
                if signed {
                    let ty = SpirvType::Integer(width as u32, true).def(self);
                    self.fptosi(args[0].immediate(), ty)
                } else {
                    let ty = SpirvType::Integer(width as u32, false).def(self);
                    self.fptoui(args[0].immediate(), ty)
                }
            }

            sym::rotate_left | sym::rotate_right => {
                let is_left = name == sym::rotate_left;
                let val = args[0].immediate();
                let shift = args[1].immediate();
                self.rotate(val, shift, is_left)
            }

            // TODO: Do we want to manually implement these instead of using intel instructions?
            sym::ctlz | sym::ctlz_nonzero => self
                .emit()
                .u_count_leading_zeros_intel(args[0].immediate().ty, None, args[0].immediate().def)
                .unwrap()
                .with_type(args[0].immediate().ty),
            sym::cttz | sym::cttz_nonzero => self
                .emit()
                .u_count_trailing_zeros_intel(args[0].immediate().ty, None, args[0].immediate().def)
                .unwrap()
                .with_type(args[0].immediate().ty),

            sym::ctpop => self
                .emit()
                .bit_count(args[0].immediate().ty, None, args[0].immediate().def)
                .unwrap()
                .with_type(args[0].immediate().ty),
            sym::bitreverse => self
                .emit()
                .bit_reverse(args[0].immediate().ty, None, args[0].immediate().def)
                .unwrap()
                .with_type(args[0].immediate().ty),

            sym::bswap => {
                // https://github.com/KhronosGroup/SPIRV-LLVM/pull/221/files
                // TODO: Definitely add tests to make sure this impl is right.
                let arg = args[0].immediate();
                match int_type_width_signed(arg_tys[0], self)
                    .expect("bswap must have integer argument")
                    .0
                {
                    8 => arg,
                    16 => {
                        let offset8 = self.constant_u16(8);
                        let tmp1 = self.shl(arg, offset8);
                        let tmp2 = self.lshr(arg, offset8);
                        self.or(tmp1, tmp2)
                    }
                    32 => {
                        let offset8 = self.constant_u32(8);
                        let offset24 = self.constant_u32(24);
                        let mask16 = self.constant_u32(0xFF00);
                        let mask24 = self.constant_u32(0xFF0000);
                        let tmp4 = self.shl(arg, offset24);
                        let tmp3 = self.shl(arg, offset8);
                        let tmp2 = self.lshr(arg, offset8);
                        let tmp1 = self.lshr(arg, offset24);
                        let tmp3 = self.and(tmp3, mask24);
                        let tmp2 = self.and(tmp2, mask16);
                        let res1 = self.or(tmp1, tmp2);
                        let res2 = self.or(tmp3, tmp4);
                        self.or(res1, res2)
                    }
                    64 => {
                        let offset8 = self.constant_u64(8);
                        let offset24 = self.constant_u64(24);
                        let offset40 = self.constant_u64(40);
                        let offset56 = self.constant_u64(56);
                        let mask16 = self.constant_u64(0xff00);
                        let mask24 = self.constant_u64(0xff0000);
                        let mask32 = self.constant_u64(0xff000000);
                        let mask40 = self.constant_u64(0xff00000000);
                        let mask48 = self.constant_u64(0xff0000000000);
                        let mask56 = self.constant_u64(0xff000000000000);
                        let tmp8 = self.shl(arg, offset56);
                        let tmp7 = self.shl(arg, offset40);
                        let tmp6 = self.shl(arg, offset24);
                        let tmp5 = self.shl(arg, offset8);
                        let tmp4 = self.lshr(arg, offset8);
                        let tmp3 = self.lshr(arg, offset24);
                        let tmp2 = self.lshr(arg, offset40);
                        let tmp1 = self.lshr(arg, offset56);
                        let tmp7 = self.and(tmp7, mask56);
                        let tmp6 = self.and(tmp6, mask48);
                        let tmp5 = self.and(tmp5, mask40);
                        let tmp4 = self.and(tmp4, mask32);
                        let tmp3 = self.and(tmp3, mask24);
                        let tmp2 = self.and(tmp2, mask16);
                        let res1 = self.or(tmp8, tmp7);
                        let res2 = self.or(tmp6, tmp5);
                        let res3 = self.or(tmp4, tmp3);
                        let res4 = self.or(tmp2, tmp1);
                        let res1 = self.or(res1, res2);
                        let res3 = self.or(res3, res4);
                        self.or(res1, res3)
                    }
                    other => panic!("bswap not implemented for int width {}", other),
                }
            }

            _ => panic!("TODO: Unknown intrinsic '{}'", name),
        };

        if !fn_abi.ret.is_ignore() {
            if let PassMode::Cast(_ty) = fn_abi.ret.mode {
                panic!("TODO: PassMode::Cast not implemented yet in intrinsics");
            } else {
                OperandRef::from_immediate_or_packed_pair(self, value, result.layout)
                    .val
                    .store(self, result);
            }
        }
    }

    fn abort(&mut self) {
        self.emit().kill().unwrap();
    }

    fn assume(&mut self, _val: Self::Value) {
        // TODO: llvm.assume
    }

    fn expect(&mut self, cond: Self::Value, _expected: bool) -> Self::Value {
        // TODO: llvm.expect
        cond
    }

    fn sideeffect(&mut self) {
        // TODO: This is currently ignored.
        // It corresponds to the llvm.sideeffect intrinsic - does spir-v have an equivalent?
    }

    fn va_start(&mut self, _val: Self::Value) -> Self::Value {
        todo!()
    }

    fn va_end(&mut self, _val: Self::Value) -> Self::Value {
        todo!()
    }
}

impl<'a, 'spv, 'tcx> AsmBuilderMethods<'tcx> for Builder<'a, 'spv, 'tcx> {
    fn codegen_llvm_inline_asm(
        &mut self,
        _ia: &LlvmInlineAsmInner,
        _outputs: Vec<PlaceRef<'tcx, Self::Value>>,
        _inputs: Vec<Self::Value>,
        _span: Span,
    ) -> bool {
        todo!()
    }

    fn codegen_inline_asm(
        &mut self,
        _template: &[InlineAsmTemplatePiece],
        _operands: &[InlineAsmOperandRef<'tcx, Self>],
        _options: InlineAsmOptions,
        _line_spans: &[Span],
    ) {
        todo!()
    }
}
impl<'a, 'spv, 'tcx> StaticBuilderMethods for Builder<'a, 'spv, 'tcx> {
    fn get_static(&mut self, def_id: DefId) -> Self::Value {
        self.cx.get_static(def_id)
    }
}

impl<'a, 'spv, 'tcx> BackendTypes for Builder<'a, 'spv, 'tcx> {
    type Value = <CodegenCx<'spv, 'tcx> as BackendTypes>::Value;
    type Function = <CodegenCx<'spv, 'tcx> as BackendTypes>::Function;
    type BasicBlock = <CodegenCx<'spv, 'tcx> as BackendTypes>::BasicBlock;
    type Type = <CodegenCx<'spv, 'tcx> as BackendTypes>::Type;
    type Funclet = <CodegenCx<'spv, 'tcx> as BackendTypes>::Funclet;

    type DIScope = <CodegenCx<'spv, 'tcx> as BackendTypes>::DIScope;
    type DIVariable = <CodegenCx<'spv, 'tcx> as BackendTypes>::DIVariable;
}

impl<'a, 'spv, 'tcx> HasCodegen<'tcx> for Builder<'a, 'spv, 'tcx> {
    type CodegenCx = CodegenCx<'spv, 'tcx>;
}

impl<'a, 'spv, 'tcx> HasParamEnv<'tcx> for Builder<'a, 'spv, 'tcx> {
    fn param_env(&self) -> ParamEnv<'tcx> {
        self.cx.param_env()
    }
}

impl<'a, 'spv, 'tcx> HasTargetSpec for Builder<'a, 'spv, 'tcx> {
    fn target_spec(&self) -> &Target {
        &self.cx.target_spec()
    }
}

impl<'a, 'spv, 'tcx> HasTyCtxt<'tcx> for Builder<'a, 'spv, 'tcx> {
    fn tcx(&self) -> TyCtxt<'tcx> {
        self.cx.tcx
    }
}

impl<'a, 'spv, 'tcx> HasDataLayout for Builder<'a, 'spv, 'tcx> {
    fn data_layout(&self) -> &TargetDataLayout {
        self.cx.data_layout()
    }
}

impl<'a, 'spv, 'tcx> LayoutOf for Builder<'a, 'spv, 'tcx> {
    type Ty = Ty<'tcx>;
    type TyAndLayout = TyAndLayout<'tcx>;

    fn layout_of(&self, ty: Ty<'tcx>) -> Self::TyAndLayout {
        self.cx.layout_of(ty)
    }
}
