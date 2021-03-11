use super::val;

#[test]
fn any() {
    val(r#"

#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main() {
    let vector = glam::BVec2::new(true, false);
    assert!(arch::any(vector));
}
"#);
}

#[test]
fn all() {
    val(r#"
#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main() {
    let vector = glam::BVec2::new(true, true);
    assert!(spirv_std::arch::all(vector));
}
"#);
}

#[test]
fn s_negate() {
    val(r#"
#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main() {
    let operand: i32 = -5;
    let vector = glam::IVec2::new(-5, -0);
    assert!(arch::s_negate_vector(vector) == glam::IVec2::new(5, 0));
}
"#);
}

#[test]
fn f_negate() {
    val(r#"
#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main() {
    let operand: f32 = -5.0;
    let vector = glam::Vec2::new(-5.0, -0.0);
    assert!(arch::f_negate_vector(vector) == glam::Vec2::new(5.0, 0.0));
}
"#);
}

#[test]
fn i_add() {
    val(r#"
#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main() {
    let x = 5;
    let y = 2;
    let vx = glam::IVec2::new(2, 5);
    let vy = glam::IVec2::new(5, 2);
    assert!(arch::i_add_vector(vx, vy) == glam::IVec2::new(7, 7));
}
"#);
}

#[test]
fn f_add() {
    val(r#"
#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main() {
    let x = 5.0;
    let y = 2.0;
    let vx = glam::Vec2::new(2.0, 5.0);
    let vy = glam::Vec2::new(5.0, 2.0);
    assert!(arch::f_add_vector(vx, vy) == glam::Vec2::new(7.0, 7.0));
}
"#);
}

#[test]
fn i_sub() {
    val(r#"
#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main() {
    let x = 5;
    let y = 5;
    let vx = glam::IVec2::new(5, 7);
    let vy = glam::IVec2::new(5, 7);
    assert!(arch::i_sub_vector(vx, vy) == glam::IVec2::new(0, 0));
}
"#);
}

#[test]
fn f_sub() {
    val(r#"
#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main() {
    let x = 5.0;
    let y = 5.0;
    let vx = glam::Vec2::new(5.0, 7.0);
    let vy = glam::Vec2::new(5.0, 7.0);
    assert!(arch::f_sub_vector(vx, vy) == glam::Vec2::new(0.0, 0.0));
}
"#);
}

#[test]
fn i_mul() {
    val(r#"
#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main() {
    let x = 5;
    let y = 2;
    let vx = glam::IVec2::new(5, 2);
    let vy = glam::IVec2::new(2, 5);
    assert!(arch::i_mul_vector(vx, vy) == glam::IVec2::new(10, 10));
}
"#);
}

#[test]
fn f_mul() {
    val(r#"
#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main() {
    let x = 5.0;
    let y = 2.0;
    let vx = glam::Vec2::new(5.0, 2.0);
    let vy = glam::Vec2::new(2.0, 5.0);
    assert!(arch::f_mul_vector(vx, vy) == glam::Vec2::new(10.0, 10.0));
}
"#);
}

#[test]
fn s_div() {
    val(r#"
#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main() {
    let x = 10;
    let y = 2;
    let vx = glam::IVec2::new(10, 10);
    let vy = glam::IVec2::new(2, 2);
    assert!(arch::s_div_vector(vx, vy) == glam::IVec2::new(5, 5));
}
"#);
}

#[test]
fn u_div() {
    val(r#"
#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main() {
    let x: u32 = 10;
    let y = 2;
    let vx = glam::UVec2::new(10, 10);
    let vy = glam::UVec2::new(2, 2);
    assert!(arch::u_div_vector(vx, vy) == glam::UVec2::new(5, 5));
}
"#);
}

#[test]
fn f_div() {
    val(r#"
#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main() {
    let x = 10.0;
    let y = 2.0;
    let vx = glam::Vec2::new(10.0, 10.0);
    let vy = glam::Vec2::new(2.0, 2.0);
    assert!(arch::f_div_vector(vx, vy) == glam::Vec2::new(5.0, 5.0));
}
"#);
}

#[test]
fn u_mod() {
    val(r#"
#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main() {
    let x: u32 = 10;
    let y = 2;
    let vx = glam::UVec2::new(10, 10);
    let vy = glam::UVec2::new(2, 2);
    assert!(arch::u_mod_vector(vx, vy) == glam::UVec2::new(0, 0));
}
"#);
}

#[test]
fn s_mod() {
    val(r#"
#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main() {
    let x = 10;
    let y = 2;
    let vx = glam::IVec2::new(10, 10);
    let vy = glam::IVec2::new(2, 2);
    assert!(arch::s_mod_vector(vx, vy) == glam::IVec2::new(0, 0));
}
"#);
}

#[test]
fn s_rem() {
    val(r#"
#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main() {
    let x = -10;
    let y = -2;
    let vx = glam::IVec2::new(-10, -10);
    let vy = glam::IVec2::new(-2, -2);
    assert!(arch::s_rem_vector(vx, vy) == glam::IVec2::new(-0, -0));
}
"#);
}

#[test]
fn f_mod() {
    val(r#"
#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main() {
    let x = 10.0;
    let y = 2.0;
    let vx = glam::Vec2::new(10.0, 10.0);
    let vy = glam::Vec2::new(2.0, 2.0);
    assert!(arch::f_mod_vector(vx, vy) == glam::Vec2::new(0.0, 0.0));
}
"#);
}

#[test]
fn f_rem() {
    val(r#"
#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main() {
    let x = -10.0;
    let y = -2.0;
    let vx = glam::Vec2::new(-10.0, -10.0);
    let vy = glam::Vec2::new(-2.0, -2.0);
    assert!(arch::f_mod_vector(vx, vy) == glam::Vec2::new(-0.0, -0.0));
}
"#);
}

#[test]
fn vector_times_scalar() {
    val(r#"
#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main() {
    let vector = glam::Vec2::new(10.0, 10.0);
    let scalar = 2.0;
    assert!(arch::vector_times_scalar(vector, scalar) == glam::Vec2::new(20.0, 20.0));
}
"#);
}

#[test]
fn vector_extract_dynamic() {
    val(r#"
#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main() {
    let vector = glam::Vec2::new(1.0, 2.0);
    let element = unsafe { spirv_std::arch::vector_extract_dynamic(vector, 1) };
    assert!(2.0 == element);
}
"#);
}

#[test]
fn vector_insert_dynamic() {
    val(r#"
#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main() {
    let vector = glam::Vec2::new(1.0, 2.0);
    let expected = glam::Vec2::new(1.0, 3.0);
    let new_vector = unsafe { spirv_std::arch::vector_insert_dynamic(vector, 1, 3.0) };
    assert!(new_vector == expected);
}
"#);
}


