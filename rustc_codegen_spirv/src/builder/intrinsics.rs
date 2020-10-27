use super::Builder;
use crate::abi::ConvSpirvType;
use crate::builder_spirv::SpirvValueExt;
use crate::codegen_cx::CodegenCx;
use crate::spirv_type::SpirvType;
use rspirv::spirv::{CLOp, GLOp};
use rustc_codegen_ssa::mir::operand::OperandRef;
use rustc_codegen_ssa::mir::place::PlaceRef;
use rustc_codegen_ssa::traits::{BaseTypeMethods, BuilderMethods, IntrinsicCallMethods};
use rustc_middle::bug;
use rustc_middle::ty::{FnDef, Instance, ParamEnv, Ty, TyKind};
use rustc_span::source_map::Span;
use rustc_span::sym;
use rustc_target::abi::call::{FnAbi, PassMode};
use rustc_target::abi::LayoutOf;

fn int_type_width_signed(ty: Ty<'_>, cx: &CodegenCx<'_>) -> Option<(u64, bool)> {
    match ty.kind() {
        TyKind::Int(t) => Some((
            t.bit_width()
                .unwrap_or(cx.tcx.sess.target.pointer_width as u64),
            true,
        )),
        TyKind::Uint(t) => Some((
            t.bit_width()
                .unwrap_or(cx.tcx.sess.target.pointer_width as u64),
            false,
        )),
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
        _span: Span,
    ) {
        let callee_ty = instance.ty(self.tcx, ParamEnv::reveal_all());

        let (def_id, substs) = match *callee_ty.kind() {
            FnDef(def_id, substs) => (def_id, substs),
            _ => bug!("expected fn item type, found {}", callee_ty),
        };

        let sig = callee_ty.fn_sig(self.tcx);
        let sig = self
            .tcx
            .normalize_erasing_late_bound_regions(ParamEnv::reveal_all(), &sig);
        let arg_tys = sig.inputs();
        let name = self.tcx.item_name(def_id);

        let result = PlaceRef::new_sized(llresult, fn_abi.ret.layout);

        let value = match name {
            sym::likely | sym::unlikely => {
                // Ignore these for now.
                args[0].immediate()
            }

            sym::breakpoint => {
                self.abort();
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

            sym::prefetch_read_data
            | sym::prefetch_write_data
            | sym::prefetch_read_instruction
            | sym::prefetch_write_instruction => {
                // ignore
                assert!(fn_abi.ret.is_ignore());
                return;
            }

            sym::saturating_add => {
                assert_eq!(arg_tys[0], arg_tys[1]);
                let result = match &arg_tys[0].kind() {
                    TyKind::Int(_) | TyKind::Uint(_) => {
                        self.add(args[0].immediate(), args[1].immediate())
                    }
                    TyKind::Float(_) => self.fadd(args[0].immediate(), args[1].immediate()),
                    other => self.fatal(&format!(
                        "Unimplemented saturating_add intrinsic type: {:#?}",
                        other
                    )),
                };
                // TODO: Implement this
                self.zombie(result.def, "saturating_add is not implemented yet");
                result
            }
            sym::saturating_sub => {
                assert_eq!(arg_tys[0], arg_tys[1]);
                let result = match &arg_tys[0].kind() {
                    TyKind::Int(_) | TyKind::Uint(_) => {
                        self.sub(args[0].immediate(), args[1].immediate())
                    }
                    TyKind::Float(_) => self.fsub(args[0].immediate(), args[1].immediate()),
                    other => self.fatal(&format!(
                        "Unimplemented saturating_sub intrinsic type: {:#?}",
                        other
                    )),
                };
                // TODO: Implement this
                self.zombie(result.def, "saturating_sub is not implemented yet");
                result
            }

            // TODO: Configure these to be ocl vs. gl ext instructions, etc.
            sym::sqrtf32 | sym::sqrtf64 => {
                if self.kernel_mode {
                    self.cl_op(CLOp::sqrt, [args[0].immediate()])
                } else {
                    self.gl_op(GLOp::Sqrt, [args[0].immediate()])
                }
            }
            sym::powif32 | sym::powif64 => {
                if self.kernel_mode {
                    self.cl_op(CLOp::pown, [args[0].immediate(), args[1].immediate()])
                } else {
                    let float = self.sitofp(args[1].immediate(), args[0].immediate().ty);
                    self.gl_op(GLOp::Pow, [args[0].immediate(), float])
                }
            }
            sym::sinf32 | sym::sinf64 => {
                if self.kernel_mode {
                    self.cl_op(CLOp::sin, [args[0].immediate()])
                } else {
                    self.gl_op(GLOp::Sin, [args[0].immediate()])
                }
            }
            sym::cosf32 | sym::cosf64 => {
                if self.kernel_mode {
                    self.cl_op(CLOp::cos, [args[0].immediate()])
                } else {
                    self.gl_op(GLOp::Cos, [args[0].immediate()])
                }
            }
            sym::powf32 | sym::powf64 => {
                if self.kernel_mode {
                    self.cl_op(CLOp::pow, [args[0].immediate(), args[1].immediate()])
                } else {
                    self.gl_op(GLOp::Pow, [args[0].immediate(), args[1].immediate()])
                }
            }
            sym::expf32 | sym::expf64 => {
                if self.kernel_mode {
                    self.cl_op(CLOp::exp, [args[0].immediate()])
                } else {
                    self.gl_op(GLOp::Exp, [args[0].immediate()])
                }
            }
            sym::exp2f32 | sym::exp2f64 => {
                if self.kernel_mode {
                    self.cl_op(CLOp::exp2, [args[0].immediate()])
                } else {
                    self.gl_op(GLOp::Exp2, [args[0].immediate()])
                }
            }
            sym::logf32 | sym::logf64 => {
                if self.kernel_mode {
                    self.cl_op(CLOp::log, [args[0].immediate()])
                } else {
                    self.gl_op(GLOp::Log, [args[0].immediate()])
                }
            }
            sym::log2f32 | sym::log2f64 => {
                if self.kernel_mode {
                    self.cl_op(CLOp::log2, [args[0].immediate()])
                } else {
                    self.gl_op(GLOp::Log2, [args[0].immediate()])
                }
            }
            sym::log10f32 | sym::log10f64 => {
                if self.kernel_mode {
                    self.cl_op(CLOp::log10, [args[0].immediate()])
                } else {
                    // spir-v glsl doesn't have log10, so,
                    // log10(x) == (1 / ln(10)) * ln(x)
                    let mul = self.constant_float(args[0].immediate().ty, 1.0 / 10.0f64.ln());
                    let ln = self.gl_op(GLOp::Log, [args[0].immediate()]);
                    self.mul(mul, ln)
                }
            }
            sym::fmaf32 | sym::fmaf64 => {
                if self.kernel_mode {
                    self.cl_op(
                        CLOp::fma,
                        [
                            args[0].immediate(),
                            args[1].immediate(),
                            args[2].immediate(),
                        ],
                    )
                } else {
                    self.gl_op(
                        GLOp::Fma,
                        [
                            args[0].immediate(),
                            args[1].immediate(),
                            args[2].immediate(),
                        ],
                    )
                }
            }
            sym::fabsf32 | sym::fabsf64 => {
                if self.kernel_mode {
                    self.cl_op(CLOp::fabs, [args[0].immediate()])
                } else {
                    self.gl_op(GLOp::FAbs, [args[0].immediate()])
                }
            }
            sym::minnumf32 | sym::minnumf64 => {
                if self.kernel_mode {
                    self.cl_op(CLOp::fmin, [args[0].immediate(), args[1].immediate()])
                } else {
                    self.gl_op(GLOp::FMin, [args[0].immediate(), args[1].immediate()])
                }
            }
            sym::maxnumf32 | sym::maxnumf64 => {
                if self.kernel_mode {
                    self.cl_op(CLOp::fmax, [args[0].immediate(), args[1].immediate()])
                } else {
                    self.gl_op(GLOp::FMax, [args[0].immediate(), args[1].immediate()])
                }
            }
            sym::copysignf32 | sym::copysignf64 => {
                if self.kernel_mode {
                    self.cl_op(CLOp::copysign, [args[0].immediate(), args[1].immediate()])
                } else {
                    let arg0 = args[0].immediate();
                    let arg1 = args[1].immediate();
                    let width = match self.lookup_type(arg0.ty) {
                        SpirvType::Float(width) => width,
                        other => panic!(
                            "copysign must have float argument, not {}",
                            other.debug(arg0.ty, self)
                        ),
                    };
                    let int_ty = SpirvType::Integer(width, false).def(self);
                    let (mask_sign, mask_value) = match width {
                        32 => (
                            self.constant_u32(1 << 31),
                            self.constant_u32(u32::max_value() >> 1),
                        ),
                        64 => (
                            self.constant_u64(1 << 63),
                            self.constant_u64(u64::max_value() >> 1),
                        ),
                        _ => panic!("bitcast not supported for width {}", width),
                    };
                    let arg0_int = self.bitcast(arg0, int_ty);
                    let arg1_int = self.bitcast(arg1, int_ty);
                    let arg0_and = self.and(arg0_int, mask_value);
                    let arg1_and = self.and(arg1_int, mask_sign);
                    let result_int = self.or(arg0_and, arg1_and);
                    self.bitcast(result_int, arg0.ty)
                }
            }
            sym::floorf32 | sym::floorf64 => {
                if self.kernel_mode {
                    self.cl_op(CLOp::floor, [args[0].immediate()])
                } else {
                    self.gl_op(GLOp::Floor, [args[0].immediate()])
                }
            }
            sym::ceilf32 | sym::ceilf64 => {
                if self.kernel_mode {
                    self.cl_op(CLOp::ceil, [args[0].immediate()])
                } else {
                    self.gl_op(GLOp::Ceil, [args[0].immediate()])
                }
            }
            sym::truncf32 | sym::truncf64 => {
                if self.kernel_mode {
                    self.cl_op(CLOp::trunc, [args[0].immediate()])
                } else {
                    self.gl_op(GLOp::Trunc, [args[0].immediate()])
                }
            }
            // TODO: Correctness of all these rounds
            sym::rintf32 | sym::rintf64 => {
                if self.kernel_mode {
                    self.cl_op(CLOp::rint, [args[0].immediate()])
                } else {
                    self.gl_op(GLOp::Round, [args[0].immediate()])
                }
            }
            sym::nearbyintf32 | sym::nearbyintf64 | sym::roundf32 | sym::roundf64 => {
                if self.kernel_mode {
                    self.cl_op(CLOp::round, [args[0].immediate()])
                } else {
                    self.gl_op(GLOp::Round, [args[0].immediate()])
                }
            }

            sym::rotate_left | sym::rotate_right => {
                let is_left = name == sym::rotate_left;
                let val = args[0].immediate();
                let shift = args[1].immediate();
                self.rotate(val, shift, is_left)
            }

            // TODO: Do we want to manually implement these instead of using intel instructions?
            sym::ctlz | sym::ctlz_nonzero => {
                if self.kernel_mode {
                    self.cl_op(CLOp::clz, [args[0].immediate()])
                } else {
                    self.ext_inst
                        .borrow_mut()
                        .import_integer_functions_2_intel(self);
                    self.emit()
                        .u_count_leading_zeros_intel(
                            args[0].immediate().ty,
                            None,
                            args[0].immediate().def,
                        )
                        .unwrap()
                        .with_type(args[0].immediate().ty)
                }
            }
            sym::cttz | sym::cttz_nonzero => {
                if self.kernel_mode {
                    self.cl_op(CLOp::ctz, [args[0].immediate()])
                } else {
                    self.emit()
                        .u_count_trailing_zeros_intel(
                            args[0].immediate().ty,
                            None,
                            args[0].immediate().def,
                        )
                        .unwrap()
                        .with_type(args[0].immediate().ty)
                }
            }

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
                    other => self.fatal(&format!("bswap not implemented for int width {}", other)),
                }
            }

            _ => self.fatal(&format!("TODO: Unknown intrinsic '{}'", name)),
        };

        if !fn_abi.ret.is_ignore() {
            if let PassMode::Cast(_ty) = fn_abi.ret.mode {
                self.fatal("TODO: PassMode::Cast not implemented yet in intrinsics");
            } else {
                OperandRef::from_immediate_or_packed_pair(self, value, result.layout)
                    .val
                    .store(self, result);
            }
        }
    }

    fn abort(&mut self) {
        // codegen_llvm uses call(llvm.trap) here, so it is not a block terminator
        if !self.kernel_mode {
            self.emit().kill().unwrap();
            *self = self.build_sibling_block("abort_continue");
        }
        // TODO: Figure out an appropriate instruction for kernel mode.
    }

    fn assume(&mut self, _val: Self::Value) {
        // TODO: llvm.assume
    }

    fn expect(&mut self, cond: Self::Value, _expected: bool) -> Self::Value {
        // TODO: llvm.expect
        cond
    }

    fn sideeffect(&mut self, _: bool) {
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
