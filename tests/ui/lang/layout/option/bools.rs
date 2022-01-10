// build-pass

use spirv_std as _;

#[spirv(fragment)]
pub fn main(
    #[spirv(uniform, descriptor_set = 0, binding = 0)] uniform: &Option<usize>,
    #[spirv(flat)] output: &mut u32,
) {
    *output = uniform.map(|x| x == 42).map_or(2, |x| x as u32);
}
