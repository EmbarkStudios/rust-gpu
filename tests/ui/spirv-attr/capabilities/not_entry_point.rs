use spirv_std as _;

#[spirv(capabilities(multi_view))]
//~^ ERROR `#[spirv(capabilities)]` can only be used with entry point functions.
pub fn main() { }

#[spirv(fragment)]
pub fn real_main() { }

