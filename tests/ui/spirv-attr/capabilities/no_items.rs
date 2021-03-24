use spirv_std as _;

#[spirv(fragment)]
#[spirv(capabilities)]
//~^ ERROR `#[spirv(capabilities())]` requires a list.
pub fn main() { }

