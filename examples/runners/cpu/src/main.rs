// standard Embark lints
#![deny(unsafe_code)]
#![warn(
    clippy::all,
    clippy::await_holding_lock,
    clippy::dbg_macro,
    clippy::debug_assert_with_mut_call,
    clippy::doc_markdown,
    clippy::empty_enum,
    clippy::enum_glob_use,
    clippy::exit,
    clippy::explicit_into_iter_loop,
    clippy::filter_map_next,
    clippy::fn_params_excessive_bools,
    clippy::if_let_mutex,
    clippy::imprecise_flops,
    clippy::inefficient_to_string,
    clippy::let_unit_value,
    clippy::linkedlist,
    clippy::lossy_float_literal,
    clippy::macro_use_imports,
    clippy::map_flatten,
    clippy::map_unwrap_or,
    clippy::match_on_vec_items,
    clippy::match_wildcard_for_single_variants,
    clippy::mem_forget,
    clippy::mismatched_target_os,
    clippy::needless_borrow,
    clippy::needless_continue,
    clippy::option_option,
    clippy::pub_enum_variant_names,
    clippy::ref_option_ref,
    clippy::rest_pat_in_fully_bound_structs,
    clippy::string_to_string,
    clippy::suboptimal_flops,
    clippy::todo,
    clippy::unnested_or_patterns,
    clippy::unused_self,
    clippy::verbose_file_reads,
    future_incompatible,
    nonstandard_style,
    rust_2018_idioms
)]

use minifb::{Key, Window, WindowOptions};
use rayon::prelude::*;
use shared::glam::{vec2, Vec2, Vec4};
use shared::ShaderConstants;
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

    convert(srgb_oetf(v.z))
        | convert(srgb_oetf(v.y)) << 8
        | convert(srgb_oetf(v.x)) << 16
        | convert(v.w) << 24
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

    let push_constants = ShaderConstants {
        width: WIDTH as u32,
        height: HEIGHT as u32,
        time: 0f32,

        // FIXME(eddyb) implement mouse support for the cpu runner.
        cursor_x: 0.0,
        cursor_y: 0.0,
        drag_start_x: 0.0,
        drag_start_y: 0.0,
        drag_end_x: 0.0,
        drag_end_y: 0.0,
        mouse_button_pressed: 0,
        mouse_button_press_time: [f32::NEG_INFINITY; 3],
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

            let frag_coord = (vec2(screen_pos.x, -screen_pos.y) + Vec2::ONE) / Vec2::splat(2.0)
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
