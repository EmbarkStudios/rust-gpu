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

pub fn get_intrinsic(sym: &str) -> Option<LibmIntrinsic> {
    let res = match sym {
        "_ZN4libm4math4acos4acos17h2f6ea16e1144eec4E" => LibmIntrinsic::GLOp(GLOp::Acos), // math::acos::acos
        "_ZN4libm4math5acosf5acosf17h7dc44a6172098bb8E" => LibmIntrinsic::GLOp(GLOp::Acos), // math::acosf::acosf
        "_ZN4libm4math5acosh5acosh17h709e6a13bc0166a3E" => LibmIntrinsic::GLOp(GLOp::Acosh), // math::acosh::acosh
        "_ZN4libm4math6acoshf6acoshf17h47fd9455eaf4648fE" => LibmIntrinsic::GLOp(GLOp::Acosh), // math::acoshf::acoshf
        "_ZN4libm4math4asin4asin17h2d0215c70c90d51eE" => LibmIntrinsic::GLOp(GLOp::Asin), // math::asin::asin
        "_ZN4libm4math5asinf5asinf17hf8acc20de364ec70E" => LibmIntrinsic::GLOp(GLOp::Asin), // math::asinf::asinf
        "_ZN4libm4math5asinh5asinh17h374951d8f979138bE" => LibmIntrinsic::GLOp(GLOp::Asinh), // math::asinh::asinh
        "_ZN4libm4math6asinhf6asinhf17hbc482c1749c82bb9E" => LibmIntrinsic::GLOp(GLOp::Asinh), // math::asinhf::asinhf
        "_ZN4libm4math5atan25atan217h12320797bb45df01E" => LibmIntrinsic::GLOp(GLOp::Atan2), // math::atan2::atan2
        "_ZN4libm4math6atan2f6atan2f17habfd2ad531d5bd4aE" => LibmIntrinsic::GLOp(GLOp::Atan2), // math::atan2f::atan2f
        "_ZN4libm4math4atan4atan17h271ab57c89229fc6E" => LibmIntrinsic::GLOp(GLOp::Atan), // math::atan::atan
        "_ZN4libm4math5atanf5atanf17haec9e9a829090540E" => LibmIntrinsic::GLOp(GLOp::Atan), // math::atanf::atanf
        "_ZN4libm4math5atanh5atanh17h003671fc65a0adb4E" => LibmIntrinsic::GLOp(GLOp::Atanh), // math::atanh::atanh
        "_ZN4libm4math6atanhf6atanhf17h7083d4d238f38150E" => LibmIntrinsic::GLOp(GLOp::Atanh), // math::atanhf::atanhf
        "_ZN4libm4math4cbrt4cbrt17h4b5f474c9408014fE" => {
            LibmIntrinsic::Custom(LibmCustomIntrinsic::Cbrt)
        } // math::cbrt::cbrt
        "_ZN4libm4math5cbrtf5cbrtf17h7afc4e1abc4053caE" => {
            LibmIntrinsic::Custom(LibmCustomIntrinsic::Cbrt)
        } // math::cbrtf::cbrtf
        "_ZN4libm4math4ceil4ceil17h362483a29577a21bE" => LibmIntrinsic::GLOp(GLOp::Ceil), // math::ceil::ceil
        "_ZN4libm4math5ceilf5ceilf17h12f8f2d5ecd700f6E" => LibmIntrinsic::GLOp(GLOp::Ceil), // math::ceilf::ceilf
        "_ZN4libm4math8copysign8copysign17h36b0cc510e3fea10E" => {
            LibmIntrinsic::Custom(LibmCustomIntrinsic::CopySign)
        } // math::copysign::copysign
        "_ZN4libm4math9copysignf9copysignf17h8cf89e555f5b83f1E" => {
            LibmIntrinsic::Custom(LibmCustomIntrinsic::CopySign)
        } // math::copysignf::copysignf
        "_ZN4libm4math3cos3cos17h6a044df9c643bd8bE" => LibmIntrinsic::GLOp(GLOp::Cos), // math::cos::cos
        "_ZN4libm4math4cosf4cosf17h9c3fecaf90cdf5d4E" => LibmIntrinsic::GLOp(GLOp::Cos), // math::cosf::cosf
        "_ZN4libm4math4cosh4cosh17hfac5d50a429af085E" => LibmIntrinsic::GLOp(GLOp::Cosh), // math::cosh::cosh
        "_ZN4libm4math5coshf5coshf17h35538c09eba9f3fdE" => LibmIntrinsic::GLOp(GLOp::Cosh), // math::coshf::coshf
        "_ZN4libm4math3erf3erf17hbb28517d6a526b0aE" => {
            LibmIntrinsic::Custom(LibmCustomIntrinsic::Erf)
        } // math::erf::erf
        "_ZN4libm4math4erff4erff17h160f8ab43602bba8E" => {
            LibmIntrinsic::Custom(LibmCustomIntrinsic::Erf)
        } // math::erff::erff
        "_ZN4libm4math3erf4erfc17hff8b421883a2fb30E" => {
            LibmIntrinsic::Custom(LibmCustomIntrinsic::Erfc)
        } // math::erf::erfc
        "_ZN4libm4math4erff5erfcf17hbfce9604e30c7cbfE" => {
            LibmIntrinsic::Custom(LibmCustomIntrinsic::Erfc)
        } // math::erff::erfcf
        "_ZN4libm4math5exp105exp1017hc0c9d7885ab84b95E" => {
            LibmIntrinsic::Custom(LibmCustomIntrinsic::Exp10)
        } // math::exp10::exp10
        "_ZN4libm4math6exp10f6exp10f17hb237810a6bf01547E" => {
            LibmIntrinsic::Custom(LibmCustomIntrinsic::Exp10)
        } // math::exp10f::exp10f
        "_ZN4libm4math4exp24exp217h86197b26bde28beeE" => LibmIntrinsic::GLOp(GLOp::Exp2), // math::exp2::exp2
        "_ZN4libm4math5exp2f5exp2f17hd0cf0a42df71661cE" => LibmIntrinsic::GLOp(GLOp::Exp2), // math::exp2f::exp2f
        "_ZN4libm4math3exp3exp17he4e96dae8bde0417E" => LibmIntrinsic::GLOp(GLOp::Exp), // math::exp::exp
        "_ZN4libm4math4expf4expf17hd6afc39de469ecc0E" => LibmIntrinsic::GLOp(GLOp::Exp), // math::expf::expf
        "_ZN4libm4math5expm15expm117hdf8188d0c5bdf1f4E" => {
            LibmIntrinsic::Custom(LibmCustomIntrinsic::Expm1)
        } // math::expm1::expm1
        "_ZN4libm4math6expm1f6expm1f17h212b7f8c46db9613E" => {
            LibmIntrinsic::Custom(LibmCustomIntrinsic::Expm1)
        } // math::expm1f::expm1f
        "_ZN4libm4math4fabs4fabs17haaa73b6a858fd5b7E" => LibmIntrinsic::GLOp(GLOp::FAbs), // math::fabs::fabs
        "_ZN4libm4math5fabsf5fabsf17h707e83c17cd69215E" => LibmIntrinsic::GLOp(GLOp::FAbs), // math::fabsf::fabsf
        "_ZN4libm4math4fdim4fdim17h19d7f81646fef1b7E" => {
            LibmIntrinsic::Custom(LibmCustomIntrinsic::Fdim)
        } // math::fdim::fdim
        "_ZN4libm4math5fdimf5fdimf17h0d9dc8963dceb297E" => {
            LibmIntrinsic::Custom(LibmCustomIntrinsic::Fdim)
        } // math::fdimf::fdimf
        "_ZN4libm4math5floor5floor17h752019c61181f58dE" => LibmIntrinsic::GLOp(GLOp::Floor), // math::floor::floor
        "_ZN4libm4math6floorf6floorf17h7e32b576d69261d3E" => LibmIntrinsic::GLOp(GLOp::Floor), // math::floorf::floorf
        "_ZN4libm4math3fma3fma17h27ddefd808bc1227E" => LibmIntrinsic::GLOp(GLOp::Fma), // math::fma::fma
        "_ZN4libm4math4fmaf4fmaf17ha38e19c884422c79E" => LibmIntrinsic::GLOp(GLOp::Fma), // math::fmaf::fmaf
        "_ZN4libm4math4fmax4fmax17hc73d6c45b0668f5cE" => LibmIntrinsic::GLOp(GLOp::FMax), // math::fmax::fmax
        "_ZN4libm4math5fmaxf5fmaxf17he3a340959a9209bbE" => LibmIntrinsic::GLOp(GLOp::FMax), // math::fmaxf::fmaxf
        "_ZN4libm4math4fmin4fmin17hcfd64f55caeb22afE" => LibmIntrinsic::GLOp(GLOp::FMin), // math::fmin::fmin
        "_ZN4libm4math5fminf5fminf17h61e7230d61b75069E" => LibmIntrinsic::GLOp(GLOp::FMin), // math::fminf::fminf
        "_ZN4libm4math4fmod4fmod17he4fa9f65d2d20e22E" => {
            LibmIntrinsic::Custom(LibmCustomIntrinsic::Fmod)
        } // math::fmod::fmod
        "_ZN4libm4math5fmodf5fmodf17hb0f6f4df6a73810eE" => {
            LibmIntrinsic::Custom(LibmCustomIntrinsic::Fmod)
        } // math::fmodf::fmodf
        "_ZN4libm4math5frexp5frexp17hffef1424c9c5aa7bE" => LibmIntrinsic::GLOp(GLOp::FrexpStruct), // math::frexp::frexp
        "_ZN4libm4math6frexpf6frexpf17h948abdf903ddc056E" => LibmIntrinsic::GLOp(GLOp::FrexpStruct), // math::frexpf::frexpf
        "_ZN4libm4math5hypot5hypot17h2781304d053af725E" => {
            LibmIntrinsic::Custom(LibmCustomIntrinsic::Hypot)
        } // math::hypot::hypot
        "_ZN4libm4math6hypotf6hypotf17hb30aaf4012b7f3c2E" => {
            LibmIntrinsic::Custom(LibmCustomIntrinsic::Hypot)
        } // math::hypotf::hypotf
        "_ZN4libm4math5ilogb5ilogb17hde3ac3d55c3bba2bE" => {
            LibmIntrinsic::Custom(LibmCustomIntrinsic::Ilogb)
        } // math::ilogb::ilogb
        "_ZN4libm4math6ilogbf6ilogbf17h2ea375987bcf82f3E" => {
            LibmIntrinsic::Custom(LibmCustomIntrinsic::Ilogb)
        } // math::ilogbf::ilogbf
        "_ZN4libm4math2j02j017h9de80adf61021272E" => LibmIntrinsic::Custom(LibmCustomIntrinsic::J0), // math::j0::j0
        "_ZN4libm4math3j0f3j0f17hb2a88874e9599f4eE" => {
            LibmIntrinsic::Custom(LibmCustomIntrinsic::J0)
        } // math::j0f::j0f
        "_ZN4libm4math2j02y017h3887ec7e9dbd2535E" => LibmIntrinsic::Custom(LibmCustomIntrinsic::Y0), // math::j0::y0
        "_ZN4libm4math3j0f3y0f17he18bc628207ac77cE" => {
            LibmIntrinsic::Custom(LibmCustomIntrinsic::Y0)
        } // math::j0f::y0f
        "_ZN4libm4math2j12j117h9b70b562edb62bd5E" => LibmIntrinsic::Custom(LibmCustomIntrinsic::J1), // math::j1::j1
        "_ZN4libm4math3j1f3j1f17h23db32120f8e81aeE" => {
            LibmIntrinsic::Custom(LibmCustomIntrinsic::J1)
        } // math::j1f::j1f
        "_ZN4libm4math2j12y117ha75e5a1297847cf4E" => LibmIntrinsic::Custom(LibmCustomIntrinsic::Y1), // math::j1::y1
        "_ZN4libm4math3j1f3y1f17h496be69dd8e6754bE" => {
            LibmIntrinsic::Custom(LibmCustomIntrinsic::Y1)
        } // math::j1f::y1f
        "_ZN4libm4math2jn2jn17h61aeb225dadae70fE" => LibmIntrinsic::Custom(LibmCustomIntrinsic::Jn), // math::jn::jn
        "_ZN4libm4math3jnf3jnf17hab333fd6b3a09006E" => {
            LibmIntrinsic::Custom(LibmCustomIntrinsic::Jn)
        } // math::jnf::jnf
        "_ZN4libm4math2jn2yn17h980f04f3b59bb2ebE" => LibmIntrinsic::Custom(LibmCustomIntrinsic::Yn), // math::jn::yn
        "_ZN4libm4math3jnf3ynf17h58781449066e6b74E" => {
            LibmIntrinsic::Custom(LibmCustomIntrinsic::Yn)
        } // math::jnf::ynf
        "_ZN4libm4math5ldexp5ldexp17h38b048dfe1b8be63E" => LibmIntrinsic::GLOp(GLOp::Ldexp), // math::ldexp::ldexp
        "_ZN4libm4math6ldexpf6ldexpf17hcd5e98c8788f1a9cE" => LibmIntrinsic::GLOp(GLOp::Ldexp), // math::ldexpf::ldexpf
        "_ZN4libm4math6lgamma6lgamma17hb323d234bf977b2fE" => {
            LibmIntrinsic::Custom(LibmCustomIntrinsic::Lgamma)
        } // math::lgamma::lgamma
        "_ZN4libm4math7lgammaf7lgammaf17h46931c6d7bd78862E" => {
            LibmIntrinsic::Custom(LibmCustomIntrinsic::Lgamma)
        } // math::lgammaf::lgammaf
        "_ZN4libm4math8lgamma_r8lgamma_r17h5317a1cecf2e267dE" => {
            LibmIntrinsic::Custom(LibmCustomIntrinsic::LgammaR)
        } // math::lgamma_r::lgamma_r
        "_ZN4libm4math9lgammaf_r9lgammaf_r17h77e24ebaff2c8524E" => {
            LibmIntrinsic::Custom(LibmCustomIntrinsic::LgammaR)
        } // math::lgammaf_r::lgammaf_r
        "_ZN4libm4math6tgamma6tgamma17h3856e2dc41958bebE" => {
            LibmIntrinsic::Custom(LibmCustomIntrinsic::Tgamma)
        } // math::tgamma::tgamma
        "_ZN4libm4math7tgammaf7tgammaf17hf8edc303b26d61d6E" => {
            LibmIntrinsic::Custom(LibmCustomIntrinsic::Tgamma)
        } // math::tgammaf::tgammaf
        "_ZN4libm4math5log105log1017hc6fc5cc3b14ad852E" => {
            LibmIntrinsic::Custom(LibmCustomIntrinsic::Log10)
        } // math::log10::log10
        "_ZN4libm4math6log10f6log10f17h08e158da84d27c6eE" => {
            LibmIntrinsic::Custom(LibmCustomIntrinsic::Log10)
        } // math::log10f::log10f
        "_ZN4libm4math5log1p5log1p17h6086295b7b832ca3E" => {
            LibmIntrinsic::Custom(LibmCustomIntrinsic::Log1p)
        } // math::log1p::log1p
        "_ZN4libm4math6log1pf6log1pf17h00c299d38982057aE" => {
            LibmIntrinsic::Custom(LibmCustomIntrinsic::Log1p)
        } // math::log1pf::log1pf
        "_ZN4libm4math4log24log217hc56db9cd245ec609E" => LibmIntrinsic::GLOp(GLOp::Log2), // math::log2::log2
        "_ZN4libm4math5log2f5log2f17habcc0364eefd4291E" => LibmIntrinsic::GLOp(GLOp::Log2), // math::log2f::log2f
        "_ZN4libm4math3log3log17hc4d8979654e34ad3E" => LibmIntrinsic::GLOp(GLOp::Log), // math::log::log
        "_ZN4libm4math4logf4logf17h274343011f263b94E" => LibmIntrinsic::GLOp(GLOp::Log), // math::logf::logf
        "_ZN4libm4math4modf4modf17hececdff5218be6eaE" => LibmIntrinsic::GLOp(GLOp::ModfStruct), // math::modf::modf
        "_ZN4libm4math5modff5modff17hda3e8f03144a7726E" => LibmIntrinsic::GLOp(GLOp::ModfStruct), // math::modff::modff
        "_ZN4libm4math9nextafter9nextafter17h47213e3bac7efd95E" => {
            LibmIntrinsic::Custom(LibmCustomIntrinsic::NextAfter)
        } // math::nextafter::nextafter
        "_ZN4libm4math10nextafterf10nextafterf17ha81d69f4b26cea8fE" => {
            LibmIntrinsic::Custom(LibmCustomIntrinsic::NextAfter)
        } // math::nextafterf::nextafterf
        "_ZN4libm4math3pow3pow17hd78bad2a608baf6bE" => LibmIntrinsic::GLOp(GLOp::Pow), // math::pow::pow
        "_ZN4libm4math4powf4powf17h7fd1cd5e2b4c9783E" => LibmIntrinsic::GLOp(GLOp::Pow), // math::powf::powf
        "_ZN4libm4math9remainder9remainder17h07882671783553d7E" => {
            LibmIntrinsic::Custom(LibmCustomIntrinsic::Remainder)
        } // math::remainder::remainder
        "_ZN4libm4math10remainderf10remainderf17hab3f628ce44e8fafE" => {
            LibmIntrinsic::Custom(LibmCustomIntrinsic::Remainder)
        } // math::remainderf::remainderf
        "_ZN4libm4math6remquo6remquo17h2476dac7541ba108E" => {
            LibmIntrinsic::Custom(LibmCustomIntrinsic::RemQuo)
        } // math::remquo::remquo
        "_ZN4libm4math7remquof7remquof17h8cb54bebc10b68e6E" => {
            LibmIntrinsic::Custom(LibmCustomIntrinsic::RemQuo)
        } // math::remquof::remquof
        "_ZN4libm4math5round5round17hdfc544d16997c663E" => LibmIntrinsic::GLOp(GLOp::Round), // math::round::round
        "_ZN4libm4math6roundf6roundf17hf9e34870ea5a626eE" => LibmIntrinsic::GLOp(GLOp::Round), // math::roundf::roundf
        "_ZN4libm4math6scalbn6scalbn17hfcb5cc2162b27388E" => {
            LibmIntrinsic::Custom(LibmCustomIntrinsic::Scalbn)
        } // math::scalbn::scalbn
        "_ZN4libm4math7scalbnf7scalbnf17he930032788b814a0E" => {
            LibmIntrinsic::Custom(LibmCustomIntrinsic::Scalbn)
        } // math::scalbnf::scalbnf
        "_ZN4libm4math3sin3sin17h6b2fc3ac4644f1f2E" => LibmIntrinsic::GLOp(GLOp::Sin), // math::sin::sin
        "_ZN4libm4math6sincos6sincos17h3128c207d10585c2E" => {
            LibmIntrinsic::Custom(LibmCustomIntrinsic::SinCos)
        } // math::sincos::sincos
        "_ZN4libm4math7sincosf7sincosf17h481fa4ff66a116cfE" => {
            LibmIntrinsic::Custom(LibmCustomIntrinsic::SinCos)
        } // math::sincosf::sincosf
        "_ZN4libm4math4sinf4sinf17h498f0ce121a8b58eE" => LibmIntrinsic::GLOp(GLOp::Sin), // math::sinf::sinf
        "_ZN4libm4math4sinh4sinh17hdd82b50e0339f64bE" => LibmIntrinsic::GLOp(GLOp::Sinh), // math::sinh::sinh
        "_ZN4libm4math5sinhf5sinhf17hb89e68353a4f40c1E" => LibmIntrinsic::GLOp(GLOp::Sinh), // math::sinhf::sinhf
        "_ZN4libm4math4sqrt4sqrt17he29bb1acace0b4a0E" => LibmIntrinsic::GLOp(GLOp::Sqrt), // math::sqrt::sqrt
        "_ZN4libm4math5sqrtf5sqrtf17h2d9d63ed22c9d3b0E" => LibmIntrinsic::GLOp(GLOp::Sqrt), // math::sqrtf::sqrtf
        "_ZN4libm4math3tan3tan17h9459ee48dfa3f29aE" => LibmIntrinsic::GLOp(GLOp::Tan), // math::tan::tan
        "_ZN4libm4math4tanf4tanf17h8a532d29d0ee1292E" => LibmIntrinsic::GLOp(GLOp::Tan), // math::tanf::tanf
        "_ZN4libm4math4tanh4tanh17h18e20d55f6144b5dE" => LibmIntrinsic::GLOp(GLOp::Tanh), // math::tanh::tanh
        "_ZN4libm4math5tanhf5tanhf17h84506970bf1bca9fE" => LibmIntrinsic::GLOp(GLOp::Tanh), // math::tanhf::tanhf
        "_ZN4libm4math5trunc5trunc17ha75805e376ba9aa2E" => LibmIntrinsic::GLOp(GLOp::Trunc), // math::trunc::trunc
        "_ZN4libm4math6truncf6truncf17h64b5ded9471003b1E" => LibmIntrinsic::GLOp(GLOp::Trunc), // math::truncf::truncf
        _ => return None,
    };
    Some(res)
}

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
                let sin = self.gl_op(GLOp::Sin, x.ty, &[x]).def(self);
                let cos = self.gl_op(GLOp::Cos, x.ty, &[x]).def(self);
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
                    &[args[0], self.constant_float(args[0].ty, 1.0 / 3.0)],
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
                self.gl_op(GLOp::Log, result_type, &[add])
            }
            LibmIntrinsic::Custom(LibmCustomIntrinsic::Exp10) => {
                assert_eq!(args.len(), 1);
                // exp10(x) == exp(x * log(10));
                let log10 = self.constant_float(args[0].ty, 10.0f64.ln());
                let mul = self.mul(args[0], log10);
                self.gl_op(GLOp::Exp, result_type, [mul])
            }
            LibmIntrinsic::Custom(LibmCustomIntrinsic::Expm1) => {
                let exp = self.gl_op(GLOp::Exp, args[0].ty, &[args[0]]);
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
