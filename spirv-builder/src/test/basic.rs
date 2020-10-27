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
