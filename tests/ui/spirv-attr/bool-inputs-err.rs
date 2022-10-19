// build-fail

use spirv_std::spirv;

pub struct Boolthing {
    x: u32,
    y: u32,
    b: bool,
}

#[spirv(fragment)]
pub fn fragment(
    input: bool,
    output: &mut bool,
    #[spirv(push_constant)] push: &bool,
    #[spirv(uniform)] uniform: &Boolthing,
) {
}
