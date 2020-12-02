use super::val;

#[test]
fn cf_while() {
    val(r#"
#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main(i: Input<i32>) {
    while i.load() < 10 {
    }
}
"#);
}

#[test]
fn cf_while_while() {
    val(r#"
#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main(i: Input<i32>) {
    while i.load() < 20 {
        while i.load() < 10 {
        }
    }
}
"#);
}

#[test]
fn cf_while_while_break() {
    val(r#"
#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main(i: Input<i32>) {
    while i.load() < 20 {
        while i.load() < 10 {
            break;
        }
    }
}
"#);
}

#[test]
fn cf_while_while_if_break() {
    val(r#"
#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main(i: Input<i32>) {
    while i.load() < 20 {
        while i.load() < 10 {
            if i.load() > 10 {
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
#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main(i: Input<i32>) {
    while i.load() < 10 {
        break;
    }
}
"#);
}

#[test]
fn cf_while_if_break() {
    val(r#"
#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main(i: Input<i32>) {
    while i.load() < 10 {
        if i.load() == 0 {
            break;
        }
    }
}
"#);
}

#[test]
fn cf_while_if_break_else_break() {
    val(r#"
#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main(i: Input<i32>) {
    while i.load() < 10 {
        if i.load() == 0 {
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
#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main(i: Input<i32>) {
    while i.load() < 10 {
        if i.load() == 0 {
            break;
        }
        if i.load() == 1 {
            break;
        }
    }
}
"#);
}

#[test]
fn cf_while_while_continue() {
    val(r#"
#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main(i: Input<i32>) {
    while i.load() < 20 {
        while i.load() < 10 {
            continue;
        }
    }
}
"#);
}

#[test]
fn cf_while_while_if_continue() {
    val(r#"
#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main(i: Input<i32>) {
    while i.load() < 20 {
        while i.load() < 10 {
            if i.load() > 5 {
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
#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main(i: Input<i32>) {
    while i.load() < 10 {
        continue;
    }
}
"#);
}

#[test]
fn cf_while_if_continue() {
    val(r#"
#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main(i: Input<i32>) {
    while i.load() < 10 {
        if i.load() == 0 {
            continue;
        }
    }
}
"#);
}

#[test]
fn cf_while_if_continue_else_continue() {
    val(r#"
#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main(i: Input<i32>) {
    while i.load() < 10 {
        if i.load() == 0 {
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
#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main(i: Input<i32>) {
    while i.load() < 10 {
        return;
    }
}
"#);
}

#[test]
fn cf_if_return_else() {
    val(r#"
#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main(i: Input<i32>) {
    if i.load() < 10 {
        return;
    } else {
    }
}
"#);
}

#[test]
fn cf_if_return_else_return() {
    val(r#"
#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main(i: Input<i32>) {
    if i.load() < 10 {
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
#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main(i: Input<i32>) {
    if i.load() == 0 {
        while i.load() < 10 {
        }
    }
}
"#);
}

#[test]
fn cf_if() {
    val(r#"
#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main(i: Input<i32>) {
    if i.load() > 0 {

    }
}
"#);
}
#[test]
fn cf_ifx2() {
    val(r#"
#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main(i: Input<i32>) {
    if i.load() > 0 {

    }
    if i.load() > 1 {

    }
}
"#);
}

#[test]
fn cf_if_else() {
    val(r#"
#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main(i: Input<i32>) {
    if i.load() > 0 {

    } else {

    }
}
"#);
}

#[test]
fn cf_if_elseif_else() {
    val(r#"
#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main(i: Input<i32>) {
    if i.load() > 0 {

    } else if i.load() < 0 {

    } else {

    }
}
"#);
}

#[test]
fn cf_if_if() {
    val(r#"
#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main(i: Input<i32>) {
    if i.load() > 0 {
        if i.load() < 10 {

        }
    }
}
"#);
}

#[test]
fn cf_defer() {
    val(r#"
#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main(i: Input<i32>) {
    while i.load() < 32 {
        let current_position = 0;
        if i.load() < current_position {
            break;
        }
        if i.load() < current_position {
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
use spirv_std::glam::*;
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

#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main() {
    let v = Vec3::new(1.0, 1.0, 1.0);
    render(v, v, 1.0, 2.0);
}"#);
}
