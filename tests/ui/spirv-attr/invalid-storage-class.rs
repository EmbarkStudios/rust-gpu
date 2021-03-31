// Tests that certain storage class `#[spirv(...)]` attributes are disallowed.

// build-fail

#[spirv(vertex)]
fn _entry(
    #[spirv(input)] _: (),
    #[spirv(output)] _: (),
    #[spirv(private)] _: (),
    #[spirv(function)] _: (),
    #[spirv(generic)] _: (),
) {
}
