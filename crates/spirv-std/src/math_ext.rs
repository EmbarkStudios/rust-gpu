pub trait MathExt {
    fn pow(self, factor: Self) -> Self;
    fn sqrt(self) -> Self;
    fn log2(self) -> Self;
    fn abs(self) -> Self;
    fn cos(self) -> Self;
    fn sin(self) -> Self;
    fn sin_cos(self) -> (f32, f32);
    fn round(self) -> Self;
    fn floor(self) -> Self;
    fn ceil(self) -> Self;
    fn exp(self) -> Self;
    fn saturate(self) -> Self;
    fn trunc(self) -> Self;
    fn fract(self) -> Self;

    fn signum(self) -> Self;
    fn copysign(self, sign: Self) -> Self;

    // TODO: implement but need new intrinsic in core?
    //fn tan(self) -> Self;
}

impl MathExt for f32 {
    fn pow(self, factor: f32) -> f32 {
        unsafe { core::intrinsics::powf32(self, factor) }
    }

    fn sqrt(self) -> f32 {
        unsafe { core::intrinsics::sqrtf32(self) }
    }

    fn log2(self) -> f32 {
        unsafe { core::intrinsics::log2f32(self) }
    }

    fn abs(self) -> f32 {
        unsafe { core::intrinsics::fabsf32(self) }
    }

    fn cos(self) -> f32 {
        unsafe { core::intrinsics::cosf32(self) }
    }

    fn sin(self) -> f32 {
        unsafe { core::intrinsics::sinf32(self) }
    }

    // TODO: implement but need new intrinsic in core?
    //fn tan(self) -> f32 {

    fn sin_cos(self) -> (f32, f32) {
        (self.sin(), self.cos())
    }

    fn round(self) -> f32 {
        unsafe { core::intrinsics::roundf32(self) }
    }

    fn floor(self) -> f32 {
        unsafe { core::intrinsics::floorf32(self) }
    }

    fn ceil(self) -> f32 {
        unsafe { core::intrinsics::ceilf32(self) }
    }

    fn exp(self) -> f32 {
        unsafe { core::intrinsics::expf32(self) }
    }

    fn saturate(self) -> f32 {
        self.max(0.0).min(1.0)
    }

    fn trunc(self) -> f32 {
        unsafe { core::intrinsics::truncf32(self) }
    }

    fn fract(self) -> f32 {
        self - self.trunc()
    }

    fn signum(self) -> f32 {
        if self.is_nan() {
            Self::NAN
        } else {
            1.0_f32.copysign(self)
        }
    }

    fn copysign(self, sign: f32) -> f32 {
        unsafe { core::intrinsics::copysignf32(self, sign) }
    }
}
