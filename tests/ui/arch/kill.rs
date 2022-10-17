use spirv_std::spirv;

// build-pass

#[spirv(fragment)]
pub fn main() {
    spirv_std::arch::kill();
}
