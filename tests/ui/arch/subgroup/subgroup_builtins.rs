// build-pass
// compile-flags: -C target-feature=+GroupNonUniformBallot,+ext:SPV_KHR_vulkan_memory_model

use spirv_std::arch::SubgroupMask;
use spirv_std::spirv;

#[spirv(compute(threads(1, 1, 1)))]
pub fn main(
    #[spirv(subgroup_id)] subgroup_id: u32,
    #[spirv(subgroup_local_invocation_id)] subgroup_local_invocation_id: u32,
    #[spirv(subgroup_eq_mask)] subgroup_eq_mask: SubgroupMask,
    #[spirv(subgroup_ge_mask)] subgroup_ge_mask: SubgroupMask,
    #[spirv(subgroup_gt_mask)] subgroup_gt_mask: SubgroupMask,
    #[spirv(subgroup_le_mask)] subgroup_le_mask: SubgroupMask,
    #[spirv(subgroup_lt_mask)] subgroup_lt_mask: SubgroupMask,
) {
}
