// build-pass

use spirv_std as _;

const OFFSETS: [f32; 18] = [
    0.000000, 1.494118, 3.486275, 5.478431, 7.470588, 9.462745, 11.454902, 13.447059, 15.439216,
    17.431373, 19.423529, 21.415686, 23.407843, 25.400000, 27.392157, 29.384314, 31.376471,
    33.368627,
];

#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main(#[spirv(flat)] x: &mut u32) {
    *x = OFFSETS.len() as u32;
}
