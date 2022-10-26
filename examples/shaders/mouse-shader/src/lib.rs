#![cfg_attr(target_arch = "spirv", no_std)]
// HACK(eddyb) can't easily see warnings otherwise from `spirv-builder` builds.
#![deny(warnings)]

use core::f32::consts::PI;
use glam::{vec2, vec3, vec4, Mat2, Vec2, Vec3, Vec4, Vec4Swizzles};
use shared::*;
use spirv_std::spirv;

// Note: This cfg is incorrect on its surface, it really should be "are we compiling with std", but
// we tie #[no_std] above to the same condition, so it's fine.
#[cfg(target_arch = "spirv")]
use spirv_std::num_traits::Float;

trait Shape: Copy {
    /// Distances indicate where the point is in relation to the shape:
    /// * negative distance: the point is "inside" the shape
    /// * distance of `0.0`: the point is "on" the shape
    /// * positive distance: the point is "outside" the shape
    fn distance(self, p: Vec2) -> f32;

    fn union<S>(self, other: S) -> Union<Self, S> {
        Union(self, other)
    }

    fn intersect<S>(self, other: S) -> Intersect<Self, S> {
        Intersect(self, other)
    }

    fn stroke(self, thickness: f32) -> Stroke<Self> {
        Stroke {
            shape: self,
            thickness,
        }
    }
}

#[derive(Copy, Clone)]
struct Union<A, B>(A, B);

impl<A: Shape, B: Shape> Shape for Union<A, B> {
    fn distance(self, p: Vec2) -> f32 {
        self.0.distance(p).min(self.1.distance(p))
    }
}

#[derive(Copy, Clone)]
struct Intersect<A, B>(A, B);

impl<A: Shape, B: Shape> Shape for Intersect<A, B> {
    fn distance(self, p: Vec2) -> f32 {
        self.0.distance(p).max(self.1.distance(p))
    }
}

#[derive(Copy, Clone)]
struct Stroke<S> {
    shape: S,
    thickness: f32,
}

impl<S: Shape> Shape for Stroke<S> {
    fn distance(self, p: Vec2) -> f32 {
        self.shape.distance(p).abs() - self.thickness / 2.0
    }
}

#[derive(Copy, Clone)]
struct Line(Vec2, Vec2);

impl Shape for Line {
    fn distance(self, p: Vec2) -> f32 {
        let Line(a, b) = self;
        let ap = p - a;
        let ab = b - a;

        // The projection of `p` into the line passing through `a` and `b`,
        // in relative terms, i.e. `proj` is:
        // * `0` when the projection is on `a`
        // * between `0` and `1` when the projection between `a` and `b`
        // * `1` when the project is on `b`
        let proj = ap.dot(ab) / ab.dot(ab);

        // Distance from `p` onto the line, i.e. from `p` to its projection.
        let dist_ortho = p.distance(a.lerp(b, proj));

        // Distance from the projection of `p` on the line, away from
        // either end of the line segment (i.e. towards its "outside").
        let dist_in_line = (0.0 - proj).max(proj - 1.0).max(0.0) * ab.length();

        dist_ortho.max(dist_in_line)
    }
}

#[derive(Copy, Clone)]
struct Circle {
    center: Vec2,
    radius: f32,
}

impl Shape for Circle {
    fn distance(self, p: Vec2) -> f32 {
        p.distance(self.center) - self.radius
    }
}

#[derive(Copy, Clone)]
struct Rectangle {
    center: Vec2,
    size: Vec2,
}

impl Shape for Rectangle {
    fn distance(self, p: Vec2) -> f32 {
        let diff = p - self.center;
        let diff = vec2(diff.x.abs(), diff.y.abs());
        (diff - self.size / 2.0).max_element()
    }
}

struct Painter {
    frag_coord: Vec2,
    color: Vec3,
}

impl Painter {
    fn fill(&mut self, shape: impl Shape, color: Vec4) {
        let alpha = smoothstep(2.0, 0.0, shape.distance(self.frag_coord));
        self.color = self.color.lerp(color.xyz(), alpha * color.w);
    }

    /// Fill and add a constrasting border (0.5px thick stroke of inverted color).
    fn fill_with_contrast_border(&mut self, shape: impl Shape, color: Vec4) {
        self.fill(shape, color);
        self.fill(shape.stroke(0.5), (Vec3::ONE - color.xyz()).extend(color.w));
    }
}

#[spirv(fragment)]
pub fn main_fs(
    #[spirv(frag_coord)] in_frag_coord: Vec4,
    #[spirv(push_constant)] constants: &ShaderConstants,
    output: &mut Vec4,
) {
    let frag_coord = vec2(in_frag_coord.x, in_frag_coord.y);

    let cursor = vec2(constants.cursor_x, constants.cursor_y);
    let drag_start = vec2(constants.drag_start_x, constants.drag_start_y);
    let drag_end = vec2(constants.drag_end_x, constants.drag_end_y);

    let background = {
        let resolution = vec2(constants.width as f32, constants.height as f32);
        let from_coord = move |coord: Vec2| (coord / resolution) * 2.0 - Vec2::splat(1.0);
        let v = from_coord(frag_coord);
        let mut distance = v.length();
        let to_frag = v - from_coord(drag_start);
        let start_to_end = from_coord(drag_end) - from_coord(drag_start);
        let det = to_frag.perp_dot(start_to_end).abs();
        distance /= 1.0 + det.powf(2.0);
        let t = constants.time;
        let rot = move |factor: f32| {
            (Mat2::from_angle((t / 3.0 + distance * factor).sin() * 3.0) * v).normalize()
        };
        let seed = rot(7.0).x;
        let rg = rot(2.0 + seed); // yellow
        let rb = rot(3.0 + seed + rg.y); // magenta
        let gb = rot(5.0 + seed + rb.x); // cyan
        let color = (vec3(
            (rg.x - rb.x).abs(),
            (rg.y - gb.x).abs(),
            (rb.y - gb.y).abs(),
        ) / 2.0)
            .min(Vec3::splat(1.0))
            .powf(1.2);
        let vignette = smoothstep(1.0, 0.0, (v.x * v.y).abs());
        let grayscale = Vec3::splat((color.x + color.y + color.z) / 3.0);
        grayscale.lerp(color, vignette) * vignette
    };

    let mut painter = Painter {
        frag_coord,
        color: background,
    };

    const WHITE: Vec4 = vec4(1.0, 1.0, 1.0, 1.0);
    const RED: Vec4 = vec4(1.0, 0.0, 0.0, 1.0);

    if drag_start.distance_squared(drag_end) > f32::EPSILON {
        let drag_dir = (drag_end - drag_start).normalize();
        let arrow_head = |p: Vec2| {
            Line(p - Mat2::from_angle(-PI / 4.0) * drag_dir * 16.0, p)
                .union(Line(p - Mat2::from_angle(PI / 4.0) * drag_dir * 16.0, p))
        };
        let arrow = arrow_head(drag_start - drag_dir).union(Line(drag_start, drag_end));

        if drag_end.distance_squared(cursor) > f32::EPSILON {
            painter.fill_with_contrast_border(
                arrow.union(arrow_head(drag_end + drag_dir)).stroke(4.0),
                WHITE,
            );
        } else {
            painter.fill_with_contrast_border(arrow.stroke(4.0), WHITE);
        }
    }

    let mouse_circle = Circle {
        center: cursor,
        radius: 32.0,
    };
    let mouse_button = |i: usize| {
        let size = Vec2::splat(mouse_circle.radius * 2.0) / vec2(3.0, 2.0);
        Rectangle {
            center: mouse_circle.center + size * vec2(i as f32 - 1.0, -0.5),
            size,
        }
        .intersect(mouse_circle)
    };

    for i in 0..3 {
        painter.fill(
            mouse_button(i),
            RED.lerp(
                WHITE
                    .xyz()
                    .extend(((constants.mouse_button_pressed >> i) & 1) as f32),
                smoothstep(
                    0.0,
                    1.0,
                    constants.time - constants.mouse_button_press_time[i],
                ),
            ),
        );
    }

    painter.fill_with_contrast_border(
        mouse_circle
            .stroke(4.0)
            .union(mouse_button(0).stroke(3.0))
            .union(mouse_button(1).stroke(3.0))
            .union(mouse_button(2).stroke(3.0)),
        WHITE,
    );

    *output = painter.color.extend(1.0);
}

#[spirv(vertex)]
pub fn main_vs(#[spirv(vertex_index)] vert_idx: i32, #[spirv(position)] builtin_pos: &mut Vec4) {
    // Create a "full screen triangle" by mapping the vertex index.
    // ported from https://www.saschawillems.de/blog/2016/08/13/vulkan-tutorial-on-rendering-a-fullscreen-quad-without-buffers/
    let uv = vec2(((vert_idx << 1) & 2) as f32, (vert_idx & 2) as f32);
    let pos = 2.0 * uv - Vec2::ONE;

    *builtin_pos = pos.extend(0.0).extend(1.0);
}
