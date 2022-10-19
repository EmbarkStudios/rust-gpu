// build-pass

use spirv_std::spirv;

#[spirv(fragment)]
pub fn main() {
    spirv_std::arch::kill();
}
