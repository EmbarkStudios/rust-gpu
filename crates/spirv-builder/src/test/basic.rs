use super::{dis_fn, val, val_vulkan};

#[test]
fn hello_world() {
    val(r#"
#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main() {
}
"#);
}

#[test]
fn add_two_ints() {
    dis_fn(
        r#"
fn add_two_ints(x: u32, y: u32) -> u32 {
    x + y
}
#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main() {
    add_two_ints(2, 3);
}
"#,
        "add_two_ints",
        r#"%1 = OpFunction %2 None %3
%4 = OpFunctionParameter %2
%5 = OpFunctionParameter %2
%6 = OpLabel
%7 = OpIAdd %2 %4 %5
OpReturnValue %7
OpFunctionEnd"#,
    );
}

#[test]
fn asm() {
    dis_fn(
        r#"
fn asm() {
    unsafe {
        asm!(
            "%int = OpTypeInt 32 0",
            "%scope = OpConstant %int 2",
            "%semantics = OpConstant %int 8452",
            "OpMemoryBarrier %scope %semantics",
        );
    }
}
#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main() {
    asm();
}
"#,
        "asm",
        // note: the OpConstants get hoisted out to global in the linker merge pass
        r#"%1 = OpFunction %2 None %3
%4 = OpLabel
OpMemoryBarrier %5 %6
OpReturn
OpFunctionEnd"#,
    );
}

#[test]
fn asm_add_two_ints() {
    dis_fn(
        r#"
fn add_two_ints(x: u32, y: u32) -> u32 {
    let result;
    unsafe {
        asm!(
            "{0} = OpIAdd typeof{0} {1} {2}",
            out(reg) result,
            in(reg) x,
            in(reg) y,
        );
    }
    result
}
#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main() {
    add_two_ints(2, 3);
}
"#,
        "add_two_ints",
        r#"%1 = OpFunction %2 None %3
%4 = OpFunctionParameter %2
%5 = OpFunctionParameter %2
%6 = OpLabel
%7 = OpIAdd %2 %4 %5
OpReturnValue %7
OpFunctionEnd"#,
    );
}

// TODO: While the "checked sub is not implemented yet" issue is fixed and so this repro should be
// fixed, a further underlying issue gets triggered instead, which is that the current structurizer
// doesn't handle this case. Remove `#[ignore]` once fixed.
#[test]
#[ignore]
fn logical_and() {
    val(r#"
fn f(x: bool, y: bool) -> bool {
    x && y
}
#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main() {
    f(false, true);
}"#);
}

// TODO: Implement strings to make this compile
#[test]
#[ignore]
fn panic() {
    val(r#"
#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main() {
    panic!("aaa");
}
"#);
}

// NOTE(eddyb) this won't pass Vulkan validation (see `push_constant_vulkan`),
// but should still pass the basline SPIR-V validation.
#[test]
fn push_constant() {
    val(r#"
#[derive(Copy, Clone)]
pub struct ShaderConstants {
    pub width: u32,
    pub height: u32,
    pub time: f32,
}

#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main(
    #[spirv(push_constant)] constants: PushConstant<ShaderConstants>,
) {
    let _constants = constants.load();
}
"#);
}

// NOTE(eddyb) we specifically run Vulkan validation here, as the default
// validation rules are more lax and don't require a `Block` decoration
// (`#[spirv(block)]` here) on `struct ShaderConstants`.
#[test]
fn push_constant_vulkan() {
    val_vulkan(
        r#"
#[derive(Copy, Clone)]
#[allow(unused_attributes)]
#[spirv(block)]
pub struct ShaderConstants {
    pub width: u32,
    pub height: u32,
    pub time: f32,
}

#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main(
    #[spirv(push_constant)] constants: PushConstant<ShaderConstants>,
) {
    let _constants = constants.load();
}
"#,
    );
}
