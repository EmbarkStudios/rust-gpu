use spirv_std as _;

#[spirv(fragment)]
#[spirv(capability)]
//~^ ERROR `#[spirv(capability())]` requires a list.
pub fn main() { }

