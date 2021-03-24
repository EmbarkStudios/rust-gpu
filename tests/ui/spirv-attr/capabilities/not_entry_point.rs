use spirv_std as _;

#[spirv(capability(multi_view))]
//~^ ERROR `#[spirv(capability)]` can only be used with entry point functions.
pub fn main() { }

#[spirv(fragment)]
pub fn real_main() { }

