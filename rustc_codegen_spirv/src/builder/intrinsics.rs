use super::Builder;
use crate::abi::ConvSpirvType;
use crate::builder_spirv::SpirvValueExt;
use crate::codegen_cx::CodegenCx;
use crate::spirv_type::SpirvType;
use rspirv::spirv::GLOp;
use rustc_codegen_ssa::common::span_invalid_monomorphization_error;
use rustc_codegen_ssa::common::IntPredicate;
use rustc_codegen_ssa::glue;
use rustc_codegen_ssa::mir::operand::{OperandRef, OperandValue};
use rustc_codegen_ssa::mir::place::PlaceRef;
use rustc_codegen_ssa::traits::{
    BaseTypeMethods, BuilderMethods, ConstMethods, IntrinsicCallMethods, MiscMethods, OverflowOp,
};
use rustc_codegen_ssa::MemFlags;
use rustc_middle::bug;
use rustc_middle::ty::{FnDef, Instance, ParamEnv, Ty, TyKind};
use rustc_span::source_map::Span;
use rustc_span::sym;
use rustc_target::abi::call::{FnAbi, PassMode};
use rustc_target::abi::LayoutOf;

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

fn int_type_width_signed(ty: Ty<'_>, cx: &CodegenCx<'_>) -> Option<(u64, bool)> {
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

impl<'a, 'tcx> IntrinsicCallMethods<'tcx> for Builder<'a, 'tcx> {
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
        let name_str = &*name.as_str();

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

            // This requires that atomic intrinsics follow a specific naming pattern:
            // "atomic_<operation>[_<ordering>]", and no ordering means SeqCst
            name if name_str.starts_with("atomic_") => {
                use rustc_codegen_ssa::common::AtomicOrdering::*;
                use rustc_codegen_ssa::common::{AtomicRmwBinOp, SynchronizationScope};

                let split: Vec<&str> = name_str.split('_').collect();

                let is_cxchg = split[1] == "cxchg" || split[1] == "cxchgweak";
                let (order, failorder) = match split.len() {
                    2 => (SequentiallyConsistent, SequentiallyConsistent),
                    3 => match split[2] {
                        "unordered" => (Unordered, Unordered),
                        "relaxed" => (Monotonic, Monotonic),
                        "acq" => (Acquire, Acquire),
                        "rel" => (Release, Monotonic),
                        "acqrel" => (AcquireRelease, Acquire),
                        "failrelaxed" if is_cxchg => (SequentiallyConsistent, Monotonic),
                        "failacq" if is_cxchg => (SequentiallyConsistent, Acquire),
                        _ => self.sess().fatal("unknown ordering in atomic intrinsic"),
                    },
                    4 => match (split[2], split[3]) {
                        ("acq", "failrelaxed") if is_cxchg => (Acquire, Monotonic),
                        ("acqrel", "failrelaxed") if is_cxchg => (AcquireRelease, Monotonic),
                        _ => self.sess().fatal("unknown ordering in atomic intrinsic"),
                    },
                    _ => self.sess().fatal("Atomic intrinsic not in correct format"),
                };

                let invalid_monomorphization = |ty| {
                    span_invalid_monomorphization_error(
                        self.tcx.sess,
                        span,
                        &format!(
                            "invalid monomorphization of `{}` intrinsic: \
                                  expected basic integer type, found `{}`",
                            name, ty
                        ),
                    );
                };

                match split[1] {
                    "cxchg" | "cxchgweak" => {
                        let ty = substs.type_at(0);
                        if int_type_width_signed(ty, self).is_some() {
                            let weak = split[1] == "cxchgweak";
                            let pair = self.atomic_cmpxchg(
                                args[0].immediate(),
                                args[1].immediate(),
                                args[2].immediate(),
                                order,
                                failorder,
                                weak,
                            );
                            let val = self.extract_value(pair, 0);
                            let success = self.extract_value(pair, 1);
                            let success = self.zext(success, SpirvType::Bool.def(self));

                            let dest = result.project_field(self, 0);
                            self.store(val, dest.llval, dest.align);
                            let dest = result.project_field(self, 1);
                            self.store(success, dest.llval, dest.align);
                            return;
                        } else {
                            return invalid_monomorphization(ty);
                        }
                    }

                    "load" => {
                        let ty = substs.type_at(0);
                        if int_type_width_signed(ty, self).is_some() {
                            let size = self.size_of(ty);
                            self.atomic_load(args[0].immediate(), order, size)
                        } else {
                            return invalid_monomorphization(ty);
                        }
                    }

                    "store" => {
                        let ty = substs.type_at(0);
                        if int_type_width_signed(ty, self).is_some() {
                            let size = self.size_of(ty);
                            self.atomic_store(
                                args[1].immediate(),
                                args[0].immediate(),
                                order,
                                size,
                            );
                            return;
                        } else {
                            return invalid_monomorphization(ty);
                        }
                    }

                    "fence" => {
                        self.atomic_fence(order, SynchronizationScope::CrossThread);
                        return;
                    }

                    "singlethreadfence" => {
                        self.atomic_fence(order, SynchronizationScope::SingleThread);
                        return;
                    }

                    // These are all AtomicRMW ops
                    op => {
                        let atom_op = match op {
                            "xchg" => AtomicRmwBinOp::AtomicXchg,
                            "xadd" => AtomicRmwBinOp::AtomicAdd,
                            "xsub" => AtomicRmwBinOp::AtomicSub,
                            "and" => AtomicRmwBinOp::AtomicAnd,
                            "nand" => AtomicRmwBinOp::AtomicNand,
                            "or" => AtomicRmwBinOp::AtomicOr,
                            "xor" => AtomicRmwBinOp::AtomicXor,
                            "max" => AtomicRmwBinOp::AtomicMax,
                            "min" => AtomicRmwBinOp::AtomicMin,
                            "umax" => AtomicRmwBinOp::AtomicUMax,
                            "umin" => AtomicRmwBinOp::AtomicUMin,
                            _ => self.sess().fatal("unknown atomic operation"),
                        };

                        let ty = substs.type_at(0);
                        if int_type_width_signed(ty, self).is_some() {
                            self.atomic_rmw(
                                atom_op,
                                args[0].immediate(),
                                args[1].immediate(),
                                order,
                            )
                        } else {
                            return invalid_monomorphization(ty);
                        }
                    }
                }
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
                // codegen_llvm ignores this intrinsic, presumably because an unreachable instruction will be emitted
                // directly afterwards:
                // https://github.com/rust-lang/rust/blob/0855263dcd329878b7183aa44b4ecdfdee229e6d/compiler/rustc_codegen_ssa/src/mir/block.rs#L706
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

            // TODO: Configure these to be ocl vs. gl ext instructions, etc.
            sym::sqrtf32 | sym::sqrtf64 => self.gl_op(GLOp::Sqrt, [args[0].immediate()]),
            sym::powif32 | sym::powif64 => {
                let float = self.sitofp(args[1].immediate(), args[0].immediate().ty);
                self.gl_op(GLOp::Pow, [args[0].immediate(), float])
            }
            sym::sinf32 | sym::sinf64 => self.gl_op(GLOp::Sin, [args[0].immediate()]),
            sym::cosf32 | sym::cosf64 => self.gl_op(GLOp::Cos, [args[0].immediate()]),
            sym::powf32 | sym::powf64 => {
                self.gl_op(GLOp::Pow, [args[0].immediate(), args[1].immediate()])
            }
            sym::expf32 | sym::expf64 => self.gl_op(GLOp::Exp, [args[0].immediate()]),
            sym::exp2f32 | sym::exp2f64 => self.gl_op(GLOp::Exp2, [args[0].immediate()]),
            sym::logf32 | sym::logf64 => self.gl_op(GLOp::Log, [args[0].immediate()]),
            sym::log2f32 | sym::log2f64 => self.gl_op(GLOp::Log2, [args[0].immediate()]),
            sym::log10f32 | sym::log10f64 => {
                // spir-v doesn't have log10, so,
                // log10(x) == (1 / ln(10)) * ln(x)
                let mul = self.constant_float(args[0].immediate().ty, 1.0 / 10.0f64.ln());
                let ln = self.gl_op(GLOp::Log, [args[0].immediate()]);
                self.mul(mul, ln)
            }
            sym::fmaf32 | sym::fmaf64 => self.gl_op(GLOp::Fma, [args[0].immediate()]),
            sym::fabsf32 | sym::fabsf64 => self.gl_op(GLOp::FAbs, [args[0].immediate()]),
            sym::minnumf32 | sym::minnumf64 => self.gl_op(GLOp::FMin, [args[0].immediate()]),
            sym::maxnumf32 | sym::maxnumf64 => self.gl_op(GLOp::FMax, [args[0].immediate()]),
            // sym::copysignf32 => "llvm.copysign.f32",
            // sym::copysignf64 => "llvm.copysign.f64",
            sym::floorf32 | sym::floorf64 => self.gl_op(GLOp::Floor, [args[0].immediate()]),
            sym::ceilf32 | sym::ceilf64 => self.gl_op(GLOp::Ceil, [args[0].immediate()]),
            sym::truncf32 | sym::truncf64 => self.gl_op(GLOp::Trunc, [args[0].immediate()]),
            // TODO: Correctness of round
            sym::rintf32
            | sym::rintf64
            | sym::nearbyintf32
            | sym::nearbyintf64
            | sym::roundf32
            | sym::roundf64 => self.gl_op(GLOp::Round, [args[1].immediate()]),

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
        // codegen_llvm uses call(llvm.trap) here, so it is not a block terminator
        self.emit().kill().unwrap();
        *self = self.build_sibling_block("abort_continue");
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
