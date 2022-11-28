use super::Builder;
use crate::builder_spirv::{SpirvValue, SpirvValueExt};
use rspirv::spirv::{GLOp, Word};
use rustc_codegen_ssa::traits::BuilderMethods;

#[derive(Copy, Clone, Debug)]
pub enum LibmCustomIntrinsic {
    CopySign,
    Cbrt,
    Erf,
    Erfc,
    Exp10,
    Expm1,
    Fdim,
    Fmod,
    Log10,
    Hypot,
    Ilogb,
    J0,
    Y0,
    J1,
    Y1,
    Jn,
    Yn,
    Lgamma,
    LgammaR,
    Tgamma,
    Log1p,
    NextAfter,
    Remainder,
    RemQuo,
    Scalbn,
    SinCos,
}

#[derive(Copy, Clone, Debug)]
pub enum LibmIntrinsic {
    GLOp(GLOp),
    Custom(LibmCustomIntrinsic),
}

pub const TABLE: &[(&str, LibmIntrinsic)] = &[
    ("acos", LibmIntrinsic::GLOp(GLOp::Acos)),
    ("acosf", LibmIntrinsic::GLOp(GLOp::Acos)),
    ("acosh", LibmIntrinsic::GLOp(GLOp::Acosh)),
    ("acoshf", LibmIntrinsic::GLOp(GLOp::Acosh)),
    ("asin", LibmIntrinsic::GLOp(GLOp::Asin)),
    ("asinf", LibmIntrinsic::GLOp(GLOp::Asin)),
    ("asinh", LibmIntrinsic::GLOp(GLOp::Asinh)),
    ("asinhf", LibmIntrinsic::GLOp(GLOp::Asinh)),
    ("atan2", LibmIntrinsic::GLOp(GLOp::Atan2)),
    ("atan2f", LibmIntrinsic::GLOp(GLOp::Atan2)),
    ("atan", LibmIntrinsic::GLOp(GLOp::Atan)),
    ("atanf", LibmIntrinsic::GLOp(GLOp::Atan)),
    ("atanh", LibmIntrinsic::GLOp(GLOp::Atanh)),
    ("atanhf", LibmIntrinsic::GLOp(GLOp::Atanh)),
    ("cbrt", LibmIntrinsic::Custom(LibmCustomIntrinsic::Cbrt)),
    ("cbrtf", LibmIntrinsic::Custom(LibmCustomIntrinsic::Cbrt)),
    ("ceil", LibmIntrinsic::GLOp(GLOp::Ceil)),
    ("ceilf", LibmIntrinsic::GLOp(GLOp::Ceil)),
    (
        "copysign",
        LibmIntrinsic::Custom(LibmCustomIntrinsic::CopySign),
    ),
    (
        "copysignf",
        LibmIntrinsic::Custom(LibmCustomIntrinsic::CopySign),
    ),
    ("cos", LibmIntrinsic::GLOp(GLOp::Cos)),
    ("cosf", LibmIntrinsic::GLOp(GLOp::Cos)),
    ("cosh", LibmIntrinsic::GLOp(GLOp::Cosh)),
    ("coshf", LibmIntrinsic::GLOp(GLOp::Cosh)),
    ("erf", LibmIntrinsic::Custom(LibmCustomIntrinsic::Erf)),
    ("erff", LibmIntrinsic::Custom(LibmCustomIntrinsic::Erf)),
    ("erfc", LibmIntrinsic::Custom(LibmCustomIntrinsic::Erfc)),
    ("erfcf", LibmIntrinsic::Custom(LibmCustomIntrinsic::Erfc)),
    ("exp10", LibmIntrinsic::Custom(LibmCustomIntrinsic::Exp10)),
    ("exp10f", LibmIntrinsic::Custom(LibmCustomIntrinsic::Exp10)),
    ("exp2", LibmIntrinsic::GLOp(GLOp::Exp2)),
    ("exp2f", LibmIntrinsic::GLOp(GLOp::Exp2)),
    ("exp", LibmIntrinsic::GLOp(GLOp::Exp)),
    ("expf", LibmIntrinsic::GLOp(GLOp::Exp)),
    ("expm1", LibmIntrinsic::Custom(LibmCustomIntrinsic::Expm1)),
    ("expm1f", LibmIntrinsic::Custom(LibmCustomIntrinsic::Expm1)),
    ("fabs", LibmIntrinsic::GLOp(GLOp::FAbs)),
    ("fabsf", LibmIntrinsic::GLOp(GLOp::FAbs)),
    ("fdim", LibmIntrinsic::Custom(LibmCustomIntrinsic::Fdim)),
    ("fdimf", LibmIntrinsic::Custom(LibmCustomIntrinsic::Fdim)),
    ("floor", LibmIntrinsic::GLOp(GLOp::Floor)),
    ("floorf", LibmIntrinsic::GLOp(GLOp::Floor)),
    ("fma", LibmIntrinsic::GLOp(GLOp::Fma)),
    ("fmaf", LibmIntrinsic::GLOp(GLOp::Fma)),
    ("fmax", LibmIntrinsic::GLOp(GLOp::FMax)),
    ("fmaxf", LibmIntrinsic::GLOp(GLOp::FMax)),
    ("fmin", LibmIntrinsic::GLOp(GLOp::FMin)),
    ("fminf", LibmIntrinsic::GLOp(GLOp::FMin)),
    ("fmod", LibmIntrinsic::Custom(LibmCustomIntrinsic::Fmod)),
    ("fmodf", LibmIntrinsic::Custom(LibmCustomIntrinsic::Fmod)),
    ("frexp", LibmIntrinsic::GLOp(GLOp::FrexpStruct)),
    ("frexpf", LibmIntrinsic::GLOp(GLOp::FrexpStruct)),
    ("hypot", LibmIntrinsic::Custom(LibmCustomIntrinsic::Hypot)),
    ("hypotf", LibmIntrinsic::Custom(LibmCustomIntrinsic::Hypot)),
    ("ilogb", LibmIntrinsic::Custom(LibmCustomIntrinsic::Ilogb)),
    ("ilogbf", LibmIntrinsic::Custom(LibmCustomIntrinsic::Ilogb)),
    ("j0", LibmIntrinsic::Custom(LibmCustomIntrinsic::J0)),
    ("j0f", LibmIntrinsic::Custom(LibmCustomIntrinsic::J0)),
    ("y0", LibmIntrinsic::Custom(LibmCustomIntrinsic::Y0)),
    ("y0f", LibmIntrinsic::Custom(LibmCustomIntrinsic::Y0)),
    ("j1", LibmIntrinsic::Custom(LibmCustomIntrinsic::J1)),
    ("j1f", LibmIntrinsic::Custom(LibmCustomIntrinsic::J1)),
    ("y1", LibmIntrinsic::Custom(LibmCustomIntrinsic::Y1)),
    ("y1f", LibmIntrinsic::Custom(LibmCustomIntrinsic::Y1)),
    ("jn", LibmIntrinsic::Custom(LibmCustomIntrinsic::Jn)),
    ("jnf", LibmIntrinsic::Custom(LibmCustomIntrinsic::Jn)),
    ("yn", LibmIntrinsic::Custom(LibmCustomIntrinsic::Yn)),
    ("ynf", LibmIntrinsic::Custom(LibmCustomIntrinsic::Yn)),
    ("ldexp", LibmIntrinsic::GLOp(GLOp::Ldexp)),
    ("ldexpf", LibmIntrinsic::GLOp(GLOp::Ldexp)),
    ("lgamma", LibmIntrinsic::Custom(LibmCustomIntrinsic::Lgamma)),
    (
        "lgammaf",
        LibmIntrinsic::Custom(LibmCustomIntrinsic::Lgamma),
    ),
    (
        "lgamma_r",
        LibmIntrinsic::Custom(LibmCustomIntrinsic::LgammaR),
    ),
    (
        "lgammaf_r",
        LibmIntrinsic::Custom(LibmCustomIntrinsic::LgammaR),
    ),
    ("tgamma", LibmIntrinsic::Custom(LibmCustomIntrinsic::Tgamma)),
    (
        "tgammaf",
        LibmIntrinsic::Custom(LibmCustomIntrinsic::Tgamma),
    ),
    ("log10", LibmIntrinsic::Custom(LibmCustomIntrinsic::Log10)),
    ("log10f", LibmIntrinsic::Custom(LibmCustomIntrinsic::Log10)),
    ("log1p", LibmIntrinsic::Custom(LibmCustomIntrinsic::Log1p)),
    ("log1pf", LibmIntrinsic::Custom(LibmCustomIntrinsic::Log1p)),
    ("log2", LibmIntrinsic::GLOp(GLOp::Log2)),
    ("log2f", LibmIntrinsic::GLOp(GLOp::Log2)),
    ("log", LibmIntrinsic::GLOp(GLOp::Log)),
    ("logf", LibmIntrinsic::GLOp(GLOp::Log)),
    ("modf", LibmIntrinsic::GLOp(GLOp::ModfStruct)),
    ("modff", LibmIntrinsic::GLOp(GLOp::ModfStruct)),
    (
        "nextafter",
        LibmIntrinsic::Custom(LibmCustomIntrinsic::NextAfter),
    ),
    (
        "nextafterf",
        LibmIntrinsic::Custom(LibmCustomIntrinsic::NextAfter),
    ),
    ("pow", LibmIntrinsic::GLOp(GLOp::Pow)),
    ("powf", LibmIntrinsic::GLOp(GLOp::Pow)),
    (
        "remainder",
        LibmIntrinsic::Custom(LibmCustomIntrinsic::Remainder),
    ),
    (
        "remainderf",
        LibmIntrinsic::Custom(LibmCustomIntrinsic::Remainder),
    ),
    ("remquo", LibmIntrinsic::Custom(LibmCustomIntrinsic::RemQuo)),
    (
        "remquof",
        LibmIntrinsic::Custom(LibmCustomIntrinsic::RemQuo),
    ),
    ("rint", LibmIntrinsic::GLOp(GLOp::RoundEven)),
    ("rintf", LibmIntrinsic::GLOp(GLOp::RoundEven)),
    ("round", LibmIntrinsic::GLOp(GLOp::Round)),
    ("roundf", LibmIntrinsic::GLOp(GLOp::Round)),
    ("scalbn", LibmIntrinsic::Custom(LibmCustomIntrinsic::Scalbn)),
    (
        "scalbnf",
        LibmIntrinsic::Custom(LibmCustomIntrinsic::Scalbn),
    ),
    ("sin", LibmIntrinsic::GLOp(GLOp::Sin)),
    ("sincos", LibmIntrinsic::Custom(LibmCustomIntrinsic::SinCos)),
    (
        "sincosf",
        LibmIntrinsic::Custom(LibmCustomIntrinsic::SinCos),
    ),
    ("sinf", LibmIntrinsic::GLOp(GLOp::Sin)),
    ("sinh", LibmIntrinsic::GLOp(GLOp::Sinh)),
    ("sinhf", LibmIntrinsic::GLOp(GLOp::Sinh)),
    ("sqrt", LibmIntrinsic::GLOp(GLOp::Sqrt)),
    ("sqrtf", LibmIntrinsic::GLOp(GLOp::Sqrt)),
    ("tan", LibmIntrinsic::GLOp(GLOp::Tan)),
    ("tanf", LibmIntrinsic::GLOp(GLOp::Tan)),
    ("tanh", LibmIntrinsic::GLOp(GLOp::Tanh)),
    ("tanhf", LibmIntrinsic::GLOp(GLOp::Tanh)),
    ("trunc", LibmIntrinsic::GLOp(GLOp::Trunc)),
    ("truncf", LibmIntrinsic::GLOp(GLOp::Trunc)),
];

impl Builder<'_, '_> {
    pub fn call_libm_intrinsic(
        &mut self,
        intrinsic: LibmIntrinsic,
        result_type: Word,
        args: &[SpirvValue],
    ) -> SpirvValue {
        match intrinsic {
            LibmIntrinsic::GLOp(op) => self.gl_op(op, result_type, args),
            LibmIntrinsic::Custom(LibmCustomIntrinsic::SinCos) => {
                assert_eq!(args.len(), 1);
                let x = args[0];
                let sin = self.gl_op(GLOp::Sin, x.ty, [x]).def(self);
                let cos = self.gl_op(GLOp::Cos, x.ty, [x]).def(self);
                self.emit()
                    .composite_construct(result_type, None, [sin, cos].iter().copied())
                    .unwrap()
                    .with_type(result_type)
            }
            LibmIntrinsic::Custom(LibmCustomIntrinsic::Fmod) => {
                assert_eq!(args.len(), 2);
                self.emit()
                    .f_mod(result_type, None, args[0].def(self), args[1].def(self))
                    .unwrap()
                    .with_type(result_type)
            }
            LibmIntrinsic::Custom(LibmCustomIntrinsic::CopySign) => {
                assert_eq!(args.len(), 2);
                self.copysign(args[0], args[1])
            }
            LibmIntrinsic::Custom(LibmCustomIntrinsic::Cbrt) => {
                assert_eq!(args.len(), 1);
                self.gl_op(
                    GLOp::Pow,
                    result_type,
                    [args[0], self.constant_float(args[0].ty, 1.0 / 3.0)],
                )
            }
            LibmIntrinsic::Custom(LibmCustomIntrinsic::Log10) => {
                assert_eq!(args.len(), 1);
                // log10(x) == (1 / ln(10)) * ln(x)
                let mul = self.constant_float(args[0].ty, 1.0 / 10.0f64.ln());
                let ln = self.gl_op(GLOp::Log, result_type, [args[0]]);
                self.mul(mul, ln)
            }
            LibmIntrinsic::Custom(LibmCustomIntrinsic::Log1p) => {
                assert_eq!(args.len(), 1);
                let one = self.constant_float(args[0].ty, 1.0);
                let add = self.add(args[0], one);
                self.gl_op(GLOp::Log, result_type, [add])
            }
            LibmIntrinsic::Custom(LibmCustomIntrinsic::Exp10) => {
                assert_eq!(args.len(), 1);
                // exp10(x) == exp(x * log(10));
                let log10 = self.constant_float(args[0].ty, 10.0f64.ln());
                let mul = self.mul(args[0], log10);
                self.gl_op(GLOp::Exp, result_type, [mul])
            }
            LibmIntrinsic::Custom(LibmCustomIntrinsic::Expm1) => {
                let exp = self.gl_op(GLOp::Exp, args[0].ty, [args[0]]);
                let one = self.constant_float(exp.ty, 1.0);
                self.sub(exp, one)
            }
            LibmIntrinsic::Custom(LibmCustomIntrinsic::Erf) => {
                let undef = self.undef(result_type);
                self.zombie(undef.def(self), "Erf not supported yet");
                undef
            }
            LibmIntrinsic::Custom(LibmCustomIntrinsic::Erfc) => {
                let undef = self.undef(result_type);
                self.zombie(undef.def(self), "Erfc not supported yet");
                undef
            }
            LibmIntrinsic::Custom(LibmCustomIntrinsic::Fdim) => {
                let undef = self.undef(result_type);
                self.zombie(undef.def(self), "Fdim not supported yet");
                undef
            }
            LibmIntrinsic::Custom(LibmCustomIntrinsic::Hypot) => {
                let undef = self.undef(result_type);
                self.zombie(undef.def(self), "Hypot not supported yet");
                undef
            }
            LibmIntrinsic::Custom(LibmCustomIntrinsic::Ilogb) => {
                let undef = self.undef(result_type);
                self.zombie(undef.def(self), "Ilogb not supported yet");
                undef
            }
            LibmIntrinsic::Custom(LibmCustomIntrinsic::J0) => {
                let undef = self.undef(result_type);
                self.zombie(undef.def(self), "J0 not supported yet");
                undef
            }
            LibmIntrinsic::Custom(LibmCustomIntrinsic::Y0) => {
                let undef = self.undef(result_type);
                self.zombie(undef.def(self), "Y0 not supported yet");
                undef
            }
            LibmIntrinsic::Custom(LibmCustomIntrinsic::J1) => {
                let undef = self.undef(result_type);
                self.zombie(undef.def(self), "J1 not supported yet");
                undef
            }
            LibmIntrinsic::Custom(LibmCustomIntrinsic::Y1) => {
                let undef = self.undef(result_type);
                self.zombie(undef.def(self), "Y1 not supported yet");
                undef
            }
            LibmIntrinsic::Custom(LibmCustomIntrinsic::Jn) => {
                let undef = self.undef(result_type);
                self.zombie(undef.def(self), "Jn not supported yet");
                undef
            }
            LibmIntrinsic::Custom(LibmCustomIntrinsic::Yn) => {
                let undef = self.undef(result_type);
                self.zombie(undef.def(self), "Yn not supported yet");
                undef
            }
            LibmIntrinsic::Custom(LibmCustomIntrinsic::Lgamma) => {
                let undef = self.undef(result_type);
                self.zombie(undef.def(self), "Lgamma not supported yet");
                undef
            }
            LibmIntrinsic::Custom(LibmCustomIntrinsic::LgammaR) => {
                let undef = self.undef(result_type);
                self.zombie(undef.def(self), "LgammaR not supported yet");
                undef
            }
            LibmIntrinsic::Custom(LibmCustomIntrinsic::Tgamma) => {
                let undef = self.undef(result_type);
                self.zombie(undef.def(self), "Tgamma not supported yet");
                undef
            }
            LibmIntrinsic::Custom(LibmCustomIntrinsic::NextAfter) => {
                let undef = self.undef(result_type);
                self.zombie(undef.def(self), "NextAfter not supported yet");
                undef
            }
            LibmIntrinsic::Custom(LibmCustomIntrinsic::Remainder) => {
                let undef = self.undef(result_type);
                self.zombie(undef.def(self), "Remainder not supported yet");
                undef
            }
            LibmIntrinsic::Custom(LibmCustomIntrinsic::RemQuo) => {
                let undef = self.undef(result_type);
                self.zombie(undef.def(self), "RemQuo not supported yet");
                undef
            }
            LibmIntrinsic::Custom(LibmCustomIntrinsic::Scalbn) => {
                let undef = self.undef(result_type);
                self.zombie(undef.def(self), "Scalbn not supported yet");
                undef
            }
        }
    }
}
