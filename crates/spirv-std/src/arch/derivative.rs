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

/// Returns the partial derivative of `component` with respect to the window's X
/// coordinate. Returns the same result as either [`ddx_fine`] or
/// [`ddx_coarse`], selection of which one is dependent on external factors.
#[crate::macros::vectorized]
#[crate::macros::gpu_only]
pub fn ddx<F: Float>(component: F) -> F {
    deriv_fn!(component, OpDPdx, false)
}

/// Returns the partial derivative of `component` with respect to the window's X
/// coordinate. Uses local differencing based on the value of `component` for
/// the current fragment and its immediate neighbor(s).
#[crate::macros::vectorized]
#[crate::macros::gpu_only]
pub fn ddx_fine<F: Float>(component: F) -> F {
    deriv_fn!(component, OpDPdxFine, true)
}

/// Returns the partial derivative of `component` with respect to the window's X
/// coordinate. Uses local differencing based on the value of `component` for
/// the current fragment’s neighbors, and possibly, but not necessarily,
/// includes the value of `component` for the current fragment. That is, over a
/// given area, the implementation can compute X derivatives in fewer unique
/// locations than would be allowed by [`ddx_fine`].
#[crate::macros::vectorized]
#[crate::macros::gpu_only]
pub fn ddx_coarse<F: Float>(component: F) -> F {
    deriv_fn!(component, OpDPdxCoarse, true)
}

/// Returns the partial derivative of `component` with respect to the window's Y
/// coordinate. Returns the same result as either [`ddy_fine`] or
/// [`ddy_coarse`], selection of which one is dependent on external factors.
#[crate::macros::vectorized]
#[crate::macros::gpu_only]
pub fn ddy<F: Float>(component: F) -> F {
    deriv_fn!(component, OpDPdy, false)
}

/// Returns the partial derivative of `component` with respect to the window's Y
/// coordinate. Uses local differencing based on the value of `component` for
/// the current fragment and its immediate neighbor(s).
#[crate::macros::vectorized]
#[crate::macros::gpu_only]
pub fn ddy_fine<F: Float>(component: F) -> F {
    deriv_fn!(component, OpDPdyFine, true)
}

/// Returns the partial derivative of `component` with respect to the window's Y
/// coordinate. Uses local differencing based on the value of `component` for
/// the current fragment’s neighbors, and possibly, but not necessarily,
/// includes the value of `component` for the current fragment. That is, over a
/// given area, the implementation can compute Y derivatives in fewer unique
/// locations than would be allowed by [`ddy_fine`].
#[crate::macros::vectorized]
#[crate::macros::gpu_only]
pub fn ddy_coarse<F: Float>(component: F) -> F {
    deriv_fn!(component, OpDPdyCoarse, true)
}

/// Returns the sum of the absolute values of [`ddx`] and [`ddy`] as a single
/// operation.
#[crate::macros::vectorized]
#[crate::macros::gpu_only]
pub fn fwidth<F: Float>(component: F) -> F {
    deriv_fn!(component, OpFwidth, false)
}

/// Returns the sum of the absolute values of [`ddx_fine`] and [`ddy_fine`] as a
/// single operation.
#[crate::macros::vectorized]
#[crate::macros::gpu_only]
pub fn fwidth_fine<F: Float>(component: F) -> F {
    deriv_fn!(component, OpFwidthFine, true)
}

/// Returns the sum of the absolute values of [`ddx_coarse`] and [`ddy_coarse`]
/// as a single operation.
#[crate::macros::vectorized]
#[crate::macros::gpu_only]
pub fn fwidth_coarse<F: Float>(component: F) -> F {
    deriv_fn!(component, OpFwidthCoarse, true)
}
