// build-pass

use spirv_std::spirv;

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
}
