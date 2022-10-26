// Test `&'static T` constants where the `T` values themselves contain references,
// nested in `OpConstantComposite` (structs/arrays) - currently these are disallowed.

// build-fail

use spirv_std::spirv;

#[inline(never)]
fn pair_deep_load(r: &'static (&'static u32, &'static f32)) -> (u32, f32) {
    (*r.0, *r.1)
}

#[inline(never)]
fn array3_deep_load(r: &'static [&'static u32; 3]) -> [u32; 3] {
    [*r[0], *r[1], *r[2]]
}

#[spirv(fragment)]
pub fn main_pair(pair_out: &mut (u32, f32)) {
    *pair_out = pair_deep_load(&(&123, &3.14));
}

#[spirv(fragment)]
pub fn main_array3(array3_out: &mut [u32; 3]) {
    *array3_out = array3_deep_load(&[&0, &1, &2]);
}
