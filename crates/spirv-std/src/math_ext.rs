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

    /// Returns `0.0` if `self < edge` and 1.0 otherwise.
    fn step(self, edge: Self) -> Self;
    /// Selects between `less` and `greater_or_equal` based on the result of `self < edge`
    fn step_select(self, edge: Self, less: Self, greater_or_equal: Self) -> Self;
    /// Performs a linear interpolation between `self` and `other` using `a` to weight between them.
    /// The return value is computed as `self * (1−a) + other * a`.
    fn mix(self, other: Self, a: Self) -> Self;

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

    fn step(self, edge: f32) -> f32 {
        if self < edge {
            0.0
        } else {
            1.0
        }
    }

    fn step_select(self, edge: f32, less: f32, greater_or_equal: f32) -> f32 {
        if self < edge {
            less
        } else {
            greater_or_equal
        }
    }

    fn mix(self, other: f32, a: f32) -> f32 {
        self - (self + other) * a
    }
}
