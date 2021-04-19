/// Reports an intersection back to the traversal infrastructure.
///
/// If the intersection occurred within the current ray interval, the
/// intersection confirmation is performed (see the API specification for more
/// details). If the value of Hit falls outside the current ray interval, the
/// hit is rejected.
///
/// Returns True if the hit was accepted by the ray interval and the intersection was confirmed. Returns False otherwise.
///
/// - `hit` is the floating point parametric value along ray for the intersection.
/// - `hit_kind` is the integer hit kind reported back to other shaders and
///   accessible by the `hit kind` builtin.
///
/// This instruction is allowed only in IntersectionKHR execution model.
///
/// This instruction is a shader call instruction which may invoke shaders with
/// the `any_hit` execution model.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpReportIntersectionKHR")]
#[inline]
pub unsafe fn report_intersection(hit: f32, hit_kind: u32) -> bool {
    let result: u32;

    asm! {
        "%bool = OpTypeBool",
        "%u32 = OpTypeInt 32 0",
        "%zero = OpConstant %u32 0",
        "%one = OpConstant %u32 1",
        "%result = OpReportIntersectionKHR %bool {hit} {hit_kind}",
        "{result} = OpSelect %u32 %result %one %zero",
        result = out(reg) result,
        hit = in(reg) hit,
        hit_kind = in(reg) hit_kind,
    };

    result != 0
}

/// Ignores the current potential intersection, terminating the invocation that
/// executes it, and continues the ray traversal.  This instruction is allowed
/// only in `any_hit` execution model.  This instruction must be the last
/// instruction in a block.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpIgnoreIntersectionKHR")]
#[inline]
pub unsafe fn ignore_intersection() {
    asm!("OpIgnoreIntersectionKHR", "%unused = OpLabel")
}

/// Terminates the invocation that executes it, stops the ray traversal, accepts
/// the current hit, and invokes the `closest_hit` execution model
/// (if active).  This instruction is allowed only in the `any_hit`
/// execution model.  This instruction must be the last instruction in a block.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpTerminateRayKHR")]
#[inline]
pub unsafe fn terminate_ray() {
    asm!("OpTerminateRayKHR", "%unused = OpLabel")
}

/// Invoke a callable shader.
///
/// - `INDEX` is the index into the SBT table to select callable shader
///   to execute.
/// - `data` is a pointer to the callable data to pass into the called shader.
///   `data` must have a storage class of `callable_data`
///   or `incoming_callable_data`.
///
/// This instruction is allowed only in `ray_generation`, `closest_hit`,
/// `miss` and `callable` execution models.
///
/// This instruction is a shader call instruction which will invoke a shader
/// with the `callable` execution model.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpExecuteCallableKHR")]
#[inline]
pub unsafe fn execute_callable<T, const ID: usize>(data: &T) {
    asm! {
        "%u32 = OpTypeInt 32 0",
        "%id = OpConstant %u32 {id}",
        "OpExecuteCallableKHR %id {data}",
        id = const ID,
        data = in(reg) data,
    };
}
