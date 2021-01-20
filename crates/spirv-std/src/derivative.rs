//! The [`Derivative`] trait for getting derivatives and handling derivative
//! operations in SPIR-V.

/// Represents a type that can represent the derivation of an operation.
pub trait Derivative {
    /// Returns the partial derivative of `Self` with respect to the window's
    /// X coordinate. Returns the same result as either
    /// [`Self::ddx_fine`] or [`Self::ddx_coarse`], selection of which one is
    /// dependent on external factors.
    fn ddx(self) -> Self;
    /// Returns the partial derivative of `Self` with respect to the window's
    /// X coordinate. Uses local differencing based on the value of `Self` for
    /// the current fragment and its immediate neighbor(s).
    fn ddx_fine(self) -> Self;
    /// Returns the partial derivative of `Self` with respect to the window's
    /// X coordinate. Uses local differencing based on the value of `Self` for
    /// the current fragment’s neighbors, and possibly, but not necessarily,
    /// includes the value of `Self` for the current fragment. That is, over a
    /// given area, the implementation can compute X derivatives in fewer
    /// unique locations than would be allowed by [`Self::ddx_fine`].
    fn ddx_coarse(self) -> Self;
    /// Returns the partial derivative of `Self` with respect to the window's
    /// Y coordinate. Returns the same result as either [`Self::ddy_fine`] or
    /// [`Self::ddy_coarse`], selection of which one is dependent on
    /// external factors.
    fn ddy(self) -> Self;
    /// Returns the partial derivative of `Self` with respect to the window's
    /// Y coordinate. Uses local differencing based on the value of `Self` for
    /// the current fragment and its immediate neighbor(s).
    fn ddy_fine(self) -> Self;
    /// Returns the partial derivative of `Self` with respect to the window's
    /// Y coordinate. Uses local differencing based on the value of `Self` for
    /// the current fragment’s neighbors, and possibly, but not necessarily,
    /// includes the value of `Self` for the current fragment. That is, over a
    /// given area, the implementation can compute Y derivatives in fewer
    /// unique locations than would be allowed by [`Derivative::ddy_fine`].
    fn ddy_coarse(self) -> Self;
    /// Returns the sum of the absolute values of [`Self::ddx`] and
    /// [`Self::ddy`] as a single operation.
    fn fwidth(self) -> Self;
    /// Returns the sum of the absolute values of [`Self::ddx_fine`] and
    /// [`Self::ddy_fine`] as a single operation.
    fn fwidth_fine(self) -> Self;
    /// Returns the sum of the absolute values of [`Self::ddx_coarse`] and
    /// [`Self::ddy_coarse`] as a single operation.
    fn fwidth_coarse(self) -> Self;
}

#[cfg(target_arch = "spirv")]
macro_rules! deriv_caps {
    (true) => {
        asm!("OpCapability DerivativeControl")
    };
    (false) => {};
}

macro_rules! deriv_fn {
    ($name:ident, $inst:ident, $needs_caps:tt) => {
        fn $name(self) -> Self {
            #[cfg(not(target_arch = "spirv"))]
            panic!(concat!(stringify!($name), " is not supported on the CPU"));
            #[cfg(target_arch = "spirv")]
            unsafe {
                let mut o = Default::default();
                deriv_caps!($needs_caps);
                asm!(
                    "%input = OpLoad _ {0}",
                    concat!("%result = ", stringify!($inst), " _ %input"),
                    "OpStore {1} %result",
                    in(reg) &self,
                    in(reg) &mut o,
                );
                o
            }
        }
    };
}

macro_rules! deriv_impl {
    ($ty:ty) => {
        impl Derivative for $ty {
            deriv_fn!(ddx, OpDPdx, false);
            deriv_fn!(ddx_fine, OpDPdxFine, true);
            deriv_fn!(ddx_coarse, OpDPdxCoarse, true);
            deriv_fn!(ddy, OpDPdy, false);
            deriv_fn!(ddy_fine, OpDPdyFine, true);
            deriv_fn!(ddy_coarse, OpDPdyCoarse, true);
            deriv_fn!(fwidth, OpFwidth, false);
            deriv_fn!(fwidth_fine, OpFwidthFine, true);
            deriv_fn!(fwidth_coarse, OpFwidthCoarse, true);
        }
    };
}

// "must be a scalar or vector of floating-point type. The component width must be 32 bits."
deriv_impl!(f32);
deriv_impl!(glam::Vec2);
deriv_impl!(glam::Vec3);
deriv_impl!(glam::Vec3A);
deriv_impl!(glam::Vec4);
