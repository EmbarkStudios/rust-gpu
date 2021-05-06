use crate::{float::Float, vector::Vector};

/// Emits the current values of all output variables to the current output
/// primitive. After execution, the values of all output variables
/// are undefined.  Requires capability `Geometry`.
///
/// # Safety
/// This instruction must only be used when only one stream is present.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpFOrdEqual")]
#[inline]
pub fn f_ord_equal_vector<V: Vector<bool, N>, F: Float, const N: usize>(
    lhs: impl Vector<F, N>,
    rhs: impl Vector<F, N>,
) -> V {
    let mut result: V = Default::default();

    unsafe {
        asm! {
            "%bool = OpTypeBool",
            "%vbool = OpTypeVector %bool {LENGTH}",
            "%u32 = OpTypeInt 32 0",
            "%u32_0 = OpConstant %u32 0",
            "%u32_1 = OpConstant %u32 1",
            "%lhs = OpLoad typeof*{lhs} {lhs}",
            "%rhs = OpLoad typeof*{rhs} {rhs}",
            "%result = OpFOrdEqual %vbool %lhs %rhs",
            "OpStore {result} %result",
            LENGTH = const N,
            lhs = in(reg) &lhs,
            rhs = in(reg) &rhs,
            result = in(reg) &mut result,
        }
    }

    result
}
