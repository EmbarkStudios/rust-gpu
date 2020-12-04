use super::{dis_fn, val, val_vulkan};
use std::ffi::OsStr;

struct SetEnvVar<'a> {
    k: &'a OsStr,
}

impl<'a> SetEnvVar<'a> {
    fn new(k: &'a impl AsRef<OsStr>, v: impl AsRef<OsStr>) -> Self {
        let k = k.as_ref();
        std::env::set_var(k, v);
        Self { k }
    }
}

impl<'a> Drop for SetEnvVar<'a> {
    fn drop(&mut self) {
        std::env::remove_var(self.k)
    }
}

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
// blocked on: https://github.com/EmbarkStudios/rust-gpu/issues/69
#[ignore]
fn no_dce() {
    let _var = SetEnvVar::new(&"NO_DCE", "1");
    val(r#"
#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn no_dce() {
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

#[test]
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

#[test]
fn panic() {
    val(r#"
#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main() {
    panic!("aaa");
}
"#);
}

#[test]
fn panic_builtin() {
    val(r#"
fn int_div(x: usize) -> usize {
    1 / x
}

#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main() {
    int_div(0);
}
"#);
}

#[test]
fn panic_builtin_bounds_check() {
    val(r#"
fn array_bounds_check(x: [u32; 4], i: usize) -> u32 {
    x[i]
}

#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main() {
    array_bounds_check([0, 1, 2, 3], 5);
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
pub fn main(constants: PushConstant<ShaderConstants>) {
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
pub fn main(constants: PushConstant<ShaderConstants>) {
    let _constants = constants.load();
}
"#,
    );
}

#[test]
fn infinite_loop() {
    val(r#"
#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main() {
    loop {}
}"#);
}
