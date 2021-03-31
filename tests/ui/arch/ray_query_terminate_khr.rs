// build-pass

use glam::Vec3;
use spirv_std::ray_tracing::{AccelerationStructure, RayFlags, RayQuery};

#[spirv(fragment)]
pub fn main(#[spirv(descriptor_set = 0, binding = 0)] accel: &AccelerationStructure) {
    unsafe {
        asm!(r#"OpExtension "SPV_KHR_ray_query""#);
        asm!("OpCapability RayQueryKHR");
        spirv_std::ray_query!(let mut handle);
        handle.initialize(accel, RayFlags::NONE, 0, Vec3::ZERO, 0.0, Vec3::ZERO, 0.0);
        handle.terminate();
    }
}
