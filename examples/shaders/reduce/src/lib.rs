#![cfg_attr(target_arch = "spirv", no_std)]
#![allow(clippy::too_many_arguments, clippy::missing_safety_doc)]
// HACK(eddyb) can't easily see warnings otherwise from `spirv-builder` builds.
#![deny(warnings)]
use spirv_std::glam::UVec3;
#[cfg(target_arch = "spirv")]
use spirv_std::memory::Scope;
use spirv_std::spirv;

#[doc(alias = "OpGroupNonUniformIAdd")]
#[cfg(target_arch = "spirv")]
#[inline]
pub unsafe fn subgroup_add(value: u32) -> u32 {
    const EXECUTION: u32 = Scope::Subgroup as _;
    let mut result = 0;
    asm! {
        "%u32 = OpTypeInt 32 0",
        "%execution = OpConstant %u32 {execution}",
        "%result = OpGroupNonUniformIAdd _ %execution Reduce {value}",
        "OpStore {result} %result",
        execution = const EXECUTION,
        value = in(reg) value,
        result = in(reg) &mut result,
    }
    result
}

#[cfg(not(target_arch = "spirv"))]
pub unsafe fn subgroup_add(_value: u32) -> u32 {
    panic!()
}

#[spirv(compute(threads(256)))]
pub fn main(
    #[spirv(global_invocation_id)] global_invocation_id: UVec3,
    #[spirv(local_invocation_id)] local_invocation_id: UVec3,
    #[spirv(subgroup_local_invocation_id)] subgroup_local_invocation_id: u32,
    #[spirv(workgroup_id)] workgroup_id: UVec3,
    #[spirv(subgroup_id)] subgroup_id: u32,
    #[spirv(num_subgroups)] num_subgroups: u32,
    #[spirv(storage_buffer, descriptor_set = 0, binding = 0)] input: &[u32],
    #[spirv(storage_buffer, descriptor_set = 0, binding = 1)] output: &mut [u32],
    #[spirv(workgroup)] shared: &mut [u32; 256],
) {
    let global_invocation_id_x = global_invocation_id.x as usize;
    let local_invocation_id_x = local_invocation_id.x as usize;
    let workgroup_id_x = workgroup_id.x as usize;

    let mut sum = 0;
    if global_invocation_id_x < input.len() {
        sum = input[global_invocation_id_x];
    }
    sum = unsafe { subgroup_add(sum) };
    if subgroup_local_invocation_id == 0 {
        shared[subgroup_id as usize] = sum;
    }
    unsafe { spirv_std::arch::workgroup_memory_barrier_with_group_sync() };
    let mut sum = 0;
    if subgroup_id == 0 {
        if subgroup_local_invocation_id < num_subgroups {
            sum = shared[subgroup_local_invocation_id as usize];
        }
        sum = unsafe { subgroup_add(sum) };
    }
    if local_invocation_id_x == 0 {
        output[workgroup_id_x] = sum;
    }
}
