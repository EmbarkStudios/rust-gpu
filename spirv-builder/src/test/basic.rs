use super::val;

#[test]
fn hello_world() {
    val(r#"
#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main() {
}
"#);
}
