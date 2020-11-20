use super::{dis_fn, val};

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
