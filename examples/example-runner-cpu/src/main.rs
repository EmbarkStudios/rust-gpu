use minifb::{Key, Window, WindowOptions};
use rayon::prelude::*;
use spirv_std::{vec2, Vec4};
use std::time::Instant;

fn color_u32_from_vec4(v: Vec4) -> u32 {
    let convert = |f: f32| -> u32 { (f.min(1.0).max(0.0) * 255.0).round() as u32 };

    convert(v.z()) | convert(v.y()) << 8 | convert(v.x()) << 16 | convert(v.w()) << 24
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

    // Limit to max ~60 fps update rate
    window.limit_update_rate(Some(std::time::Duration::from_micros(16600)));

    let start_time = Instant::now();

    let buffer = (0..WIDTH * HEIGHT)
        .into_par_iter()
        .map(|i| {
            let screen_pos = vec2(
                (i % WIDTH) as f32 / WIDTH as f32 * 2.0 - 1.0,
                (i / HEIGHT) as f32 / HEIGHT as f32 * 2.0 - 1.0,
            );

            // evaluate the fragment shader for the specific pixel
            let color = example_shader::fs(screen_pos);

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
