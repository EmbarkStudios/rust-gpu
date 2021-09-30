// build-pass
// compile-flags: -Ctarget-feature=+Int64,+ShaderClockKHR,+ext:SPV_KHR_shader_clock

use glam::UVec2;
use spirv_std::arch::{read_clock_khr, read_clock_uvec2_khr};

#[spirv(fragment)]
pub fn main() {
    let clock_time = unsafe {
        read_clock_khr()
    };

    let clock_time_uvec2: UVec2 = unsafe {
        read_clock_uvec2_khr()
    };
}
