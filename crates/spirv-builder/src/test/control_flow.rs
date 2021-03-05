use super::val;

#[test]
fn cf_while() {
    val(r#"
#[spirv(fragment)]
pub fn main(i: Input<i32>) {
    while *i < 10 {
    }
}
"#);
}

#[test]
fn cf_while_while() {
    val(r#"
#[spirv(fragment)]
pub fn main(i: Input<i32>) {
    while *i < 20 {
        while *i < 10 {
        }
    }
}
"#);
}

#[test]
fn cf_while_while_break() {
    val(r#"
#[spirv(fragment)]
pub fn main(i: Input<i32>) {
    while *i < 20 {
        while *i < 10 {
            break;
        }
    }
}
"#);
}

#[test]
fn cf_while_while_if_break() {
    val(r#"
#[spirv(fragment)]
pub fn main(i: Input<i32>) {
    while *i < 20 {
        while *i < 10 {
            if *i > 10 {
                break;
            }
        }
    }
}
"#);
}

#[test]
fn cf_while_break() {
    val(r#"
#[spirv(fragment)]
pub fn main(i: Input<i32>) {
    while *i < 10 {
        break;
    }
}
"#);
}

#[test]
fn cf_while_if_break() {
    val(r#"
#[spirv(fragment)]
pub fn main(i: Input<i32>) {
    while *i < 10 {
        if *i == 0 {
            break;
        }
    }
}
"#);
}

#[test]
fn cf_while_if_break_else_break() {
    val(r#"
#[spirv(fragment)]
pub fn main(i: Input<i32>) {
    while *i < 10 {
        if *i == 0 {
            break;
        } else {
            break;
        }
    }
}
"#);
}

#[test]
fn cf_while_if_break_if_break() {
    val(r#"
#[spirv(fragment)]
pub fn main(i: Input<i32>) {
    while *i < 10 {
        if *i == 0 {
            break;
        }
        if *i == 1 {
            break;
        }
    }
}
"#);
}

#[test]
fn cf_while_while_continue() {
    val(r#"
#[spirv(fragment)]
pub fn main(i: Input<i32>) {
    while *i < 20 {
        while *i < 10 {
            continue;
        }
    }
}
"#);
}

#[test]
fn cf_while_while_if_continue() {
    val(r#"
#[spirv(fragment)]
pub fn main(i: Input<i32>) {
    while *i < 20 {
        while *i < 10 {
            if *i > 5 {
                continue;
            }
        }
    }
}
"#);
}

#[test]
fn cf_while_continue() {
    val(r#"
#[spirv(fragment)]
pub fn main(i: Input<i32>) {
    while *i < 10 {
        continue;
    }
}
"#);
}

#[test]
fn cf_while_if_continue() {
    val(r#"
#[spirv(fragment)]
pub fn main(i: Input<i32>) {
    while *i < 10 {
        if *i == 0 {
            continue;
        }
    }
}
"#);
}

#[test]
fn cf_while_if_continue_else_continue() {
    val(r#"
#[spirv(fragment)]
pub fn main(i: Input<i32>) {
    while *i < 10 {
        if *i == 0 {
            continue;
        } else {
            continue;
        }
    }
}
"#);
}

#[test]
fn cf_while_return() {
    val(r#"
#[spirv(fragment)]
pub fn main(i: Input<i32>) {
    while *i < 10 {
        return;
    }
}
"#);
}

#[test]
fn cf_if_return_else() {
    val(r#"
#[spirv(fragment)]
pub fn main(i: Input<i32>) {
    if *i < 10 {
        return;
    } else {
    }
}
"#);
}

#[test]
fn cf_if_return_else_return() {
    val(r#"
#[spirv(fragment)]
pub fn main(i: Input<i32>) {
    if *i < 10 {
        return;
    } else {
        return;
    }
}
"#);
}

#[test]
fn cf_if_while() {
    val(r#"
#[spirv(fragment)]
pub fn main(i: Input<i32>) {
    if *i == 0 {
        while *i < 10 {
        }
    }
}
"#);
}

#[test]
fn cf_if() {
    val(r#"
#[spirv(fragment)]
pub fn main(i: Input<i32>) {
    if *i > 0 {

    }
}
"#);
}
#[test]
fn cf_ifx2() {
    val(r#"
#[spirv(fragment)]
pub fn main(i: Input<i32>) {
    if *i > 0 {

    }
    if *i > 1 {

    }
}
"#);
}

#[test]
fn cf_if_else() {
    val(r#"
#[spirv(fragment)]
pub fn main(i: Input<i32>) {
    if *i > 0 {

    } else {

    }
}
"#);
}

#[test]
fn cf_if_elseif_else() {
    val(r#"
#[spirv(fragment)]
pub fn main(i: Input<i32>) {
    if *i > 0 {

    } else if *i < 0 {

    } else {

    }
}
"#);
}

#[test]
fn cf_if_if() {
    val(r#"
#[spirv(fragment)]
pub fn main(i: Input<i32>) {
    if *i > 0 {
        if *i < 10 {

        }
    }
}
"#);
}

#[test]
fn cf_defer() {
    val(r#"
#[spirv(fragment)]
pub fn main(i: Input<i32>) {
    while *i < 32 {
        let current_position = 0;
        if *i < current_position {
            break;
        }
        if *i < current_position {
            break;
        }
    }
}
"#);
}

#[test]
fn issue_283() {
    // version of issue 283 with loop uncommented and warnings fixed
    // https://github.com/EmbarkStudios/rust-gpu/issues/283
    val(r#"
use glam::*;
fn sphere_sdf(p: Vec3) -> f32 {
    p.length() - 1.0
}

// Global scene to render
fn scene_sdf(p: Vec3) -> f32 {
    sphere_sdf(p)
}

fn render(eye: Vec3, dir: Vec3, start: f32, end: f32) -> f32 {
    let max_marching_steps: i32 = 255;
    let epsilon: f32 = 0.0001;

    let mut depth = start;
    let mut i = 0;

    loop {
        if i < max_marching_steps {
            break;
        }

        let dist = scene_sdf(eye + depth * dir);

        if dist < epsilon {
            return depth;
        }

        depth += dist;

        if depth >= end {
            return end;
        }

        i += 1;
    }

    end
}

#[spirv(fragment)]
pub fn main() {
    let v = Vec3::new(1.0, 1.0, 1.0);
    render(v, v, 1.0, 2.0);
}"#);
}

// HACK(eddyb) test that `for` loop desugaring (with its call to `Iterator::next`
// and matching on the resulting `Option`) works, without a working `Range`
// iterator (due to the use of `mem::swap` and its block-wise implementation).
#[test]
fn cf_for_with_custom_range_iter() {
    val(r#"
use num_traits::Num;
use core::ops::Range;

struct RangeIter<T>(Range<T>);

impl<T: Num + Ord + Copy> Iterator for RangeIter<T> {
    type Item = T;
    fn next(&mut self) -> Option<T> {
        let x = self.0.start;
        if x >= self.0.end {
            None
        } else {
            self.0.start = x + T::one();
            Some(x)
        }
    }
}

#[spirv(fragment)]
pub fn main(i: Input<i32>) {
    for _ in RangeIter(0..*i) {
    }
}
"#);
}
