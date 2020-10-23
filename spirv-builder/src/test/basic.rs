use super::val;

#[test]
fn hello_world() {
    val(r#"
#[allow(unused_attributes)]
#[spirv(entry = "fragment")]
pub fn main() {
}
"#);
}
