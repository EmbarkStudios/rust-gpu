// build-pass

use spirv_std as _;

#[repr(C)]
pub enum ReprCTrivial {
    This,
    That,
}

#[repr(C)]
pub enum ReprCWithFields {
    This(u32),
    That(u32, f32),
}

#[spirv(fragment)]
pub fn main(
    #[spirv(flat)] choose: ReprCTrivial,
    #[spirv(flat)] i: u32,
    f: f32,
    #[spirv(flat)] output: &mut ReprCWithFields,
) {
    *output = match choose {
        ReprCTrivial::This => ReprCWithFields::This(i),
        ReprCTrivial::That => ReprCWithFields::That(i, f),
    }
}
