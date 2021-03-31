//! The [`Derivative`] trait for getting derivatives and handling derivative
//! operations in SPIR-V.
use crate::float::Float;

#[cfg(target_arch = "spirv")]
macro_rules! deriv_caps {
    (true) => {
        asm!("OpCapability DerivativeControl")
    };
    (false) => {};
}

#[cfg(target_arch = "spirv")]
macro_rules! deriv_fn {
    ($p:ident, $inst:ident, $needs_caps:tt) => {
        unsafe {
            let mut o = Default::default();
            deriv_caps!($needs_caps);
            asm!(
                "%input = OpLoad _ {0}",
                concat!("%result = ", stringify!($inst), " _ %input"),
                "OpStore {1} %result",
                in(reg) &$p,
                in(reg) &mut o,
            );
            o
        }
    };
}

/// Returns the partial derivative of `Self` with respect to the window's
/// X coordinate. Returns the same result as either
/// [`Self::ddx_fine`] or [`Self::ddx_coarse`], selection of which one is
/// dependent on external factors.
#[spirv_std_macros::vectorized]
#[spirv_std_macros::gpu_only]
pub fn ddx<F: Float>(component: F) -> F {
    deriv_fn!(component, OpDPdx, false)
}

/// Returns the partial derivative of `Self` with respect to the window's
/// X coordinate. Uses local differencing based on the value of `Self` for
/// the current fragment and its immediate neighbor(s).
#[spirv_std_macros::vectorized]
#[spirv_std_macros::gpu_only]
pub fn ddx_fine<F: Float>(component: F) -> F {
    deriv_fn!(component, OpDPdxFine, true)
}

/// Returns the partial derivative of `Self` with respect to the window's
/// X coordinate. Uses local differencing based on the value of `Self` for
/// the current fragment’s neighbors, and possibly, but not necessarily,
/// includes the value of `Self` for the current fragment. That is, over a
/// given area, the implementation can compute X derivatives in fewer
/// unique locations than would be allowed by [`Self::ddx_fine`].
#[spirv_std_macros::vectorized]
#[spirv_std_macros::gpu_only]
pub fn ddx_coarse<F: Float>(component: F) -> F {
    deriv_fn!(component, OpDPdxCoarse, true)
}

/// Returns the partial derivative of `Self` with respect to the window's
/// Y coordinate. Returns the same result as either [`Self::ddy_fine`] or
/// [`Self::ddy_coarse`], selection of which one is dependent on
/// external factors.
#[spirv_std_macros::vectorized]
#[spirv_std_macros::gpu_only]
pub fn ddy<F: Float>(component: F) -> F {
    deriv_fn!(component, OpDPdy, false)
}

/// Returns the partial derivative of `Self` with respect to the window's
/// Y coordinate. Uses local differencing based on the value of `Self` for
/// the current fragment and its immediate neighbor(s).
#[spirv_std_macros::vectorized]
#[spirv_std_macros::gpu_only]
pub fn ddy_fine<F: Float>(component: F) -> F {
    deriv_fn!(component, OpDPdyFine, true)
}

/// Returns the partial derivative of `Self` with respect to the window's
/// Y coordinate. Uses local differencing based on the value of `Self` for
/// the current fragment’s neighbors, and possibly, but not necessarily,
/// includes the value of `Self` for the current fragment. That is, over a
/// given area, the implementation can compute Y derivatives in fewer
/// unique locations than would be allowed by [`Derivative::ddy_fine`].
#[spirv_std_macros::vectorized]
#[spirv_std_macros::gpu_only]
pub fn ddy_coarse<F: Float>(component: F) -> F {
    deriv_fn!(component, OpDPdyCoarse, true)
}

/// Returns the sum of the absolute values of [`Self::ddx`] and
/// [`Self::ddy`] as a single operation.
#[spirv_std_macros::vectorized]
#[spirv_std_macros::gpu_only]
pub fn fwidth<F: Float>(component: F) -> F {
    deriv_fn!(component, OpFwidth, false)
}

/// Returns the sum of the absolute values of [`Self::ddx_fine`] and
/// [`Self::ddy_fine`] as a single operation.
#[spirv_std_macros::vectorized]
#[spirv_std_macros::gpu_only]
pub fn fwidth_fine<F: Float>(component: F) -> F {
    deriv_fn!(component, OpFwidthFine, true)
}

/// Returns the sum of the absolute values of [`Self::ddx_coarse`] and
/// [`Self::ddy_coarse`] as a single operation.
#[spirv_std_macros::vectorized]
#[spirv_std_macros::gpu_only]
pub fn fwidth_coarse<F: Float>(component: F) -> F {
    deriv_fn!(component, OpFwidthCoarse, true)
}
