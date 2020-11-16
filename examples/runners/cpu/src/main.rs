use minifb::{Key, Window, WindowOptions};
use rayon::prelude::*;
use spirv_std::glam::{vec2, Vec2, Vec4};
use std::time::Instant;

use sky_shader as shader_module;

// apply the srgb OETF (i.e. do "linear to sRGB")
fn srgb_oetf(x: f32) -> f32 {
    if x <= 0.0031308 {
        x * 12.92
    } else {
        1.055 * x.powf(1.0 / 2.4) - 0.055
    }
}

fn color_u32_from_vec4(v: Vec4) -> u32 {
    let convert = |f: f32| -> u32 { (f.min(1.0).max(0.0) * 255.0).round() as u32 };

    convert(srgb_oetf(v.z()))
        | convert(srgb_oetf(v.y())) << 8
        | convert(srgb_oetf(v.x())) << 16
        | convert(v.w()) << 24
}

fn main() {
    const WIDTH: usize = 1280;
    const HEIGHT: usize = 720;

    let mut window = Window::new(
        "Rust GPU - CPU shader evaluation",
        WIDTH,
        HEIGHT,
        WindowOptions::default(),
    )
    .expect("Window creation failed");

    let push_constants = shader_module::ShaderConstants {
        width: WIDTH as u32,
        height: HEIGHT as u32,
        time: 0f32,
    };

    // Limit to max ~60 fps update rate
    window.limit_update_rate(Some(std::time::Duration::from_micros(16600)));

    let start_time = Instant::now();

    let buffer = (0..WIDTH * HEIGHT)
        .into_par_iter()
        .map(|i| {
            let screen_pos = vec2(
                (i % WIDTH) as f32 / WIDTH as f32 * 2.0 - 1.0,
                -((i / WIDTH) as f32 / HEIGHT as f32 * 2.0 - 1.0),
            );

            let frag_coord = (vec2(screen_pos.x(), -screen_pos.y()) + Vec2::one())
                / Vec2::splat(2.0)
                * vec2(WIDTH as f32, HEIGHT as f32);

            // evaluate the fragment shader for the specific pixel
            let color = shader_module::fs(&push_constants, frag_coord);

            color_u32_from_vec4(color)
        })
        .collect::<Vec<_>>();

    println!(
        "Evaluating {} pixels took {} ms",
        buffer.len(),
        start_time.elapsed().as_millis()
    );

    while window.is_open() && !window.is_key_down(Key::Escape) {
        // We unwrap here as we want this code to exit if it fails. Real applications may want to handle this in a different way
        window.update_with_buffer(&buffer, WIDTH, HEIGHT).unwrap();
    }
}
