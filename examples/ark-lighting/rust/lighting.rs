struct SurfaceParams {
    diffuse_color: Vec3,
    world_position: Vec3,
    world_normal: Vec3,
}

struct EnvParams {
    sun_world_direction: Vec3,
    sun_color: Vec3,
    ground_color: Vec3,
    sky_color: Vec3,
    horizon_color: Vec3,
}

struct ViewParams {
    camera_world_position: Vec3
}

fn ark_light(surface: SurfaceParams, view: ViewParams, env: EnvParams) -> Vec3 {
    let l = env.sun_world_direction.normalize();
    let n = surface.world_normal.normalize();
    let v = (view.camera_world_position - surface.world_position).normalize();
    let h = (v + l).normalize();

    let l_dot_n = n.dot(l).max(0.0);
    let h_dot_n = h.dot(n).max(0.0);
    let specular = h_dot_n.pow(80.0);

    let fresnel = (1.0 - v.dot(h)).max(0.0).pow(5.0);

    let specular_color = env.sun_color * lerp(0.04, 1.0, fresnel);

    let ambient_light = if n.y < 0.0 {
        lerp(env.horizon_color, env.ground_color, -N.y)
    } else {
        lerp(env.horizon_color, env.sky_color, N.y)
    };

    let diffuse_light = l_dot_n * env.sun_color;
    let specular_light = specular * specular_color;

    surface.diffuse_color.rgb * (ambient_light + diffuse_light) +
        specular_light
}


fn encode_srgb(linear_rgb: Vec3) -> Vec3 {
    let a = 12.92 * linear_rgb;
    let b = 1.055 * linear_rgb.pow(Vec3::splat(1.0 / 2.4)) - 0.055;
    let c = step(Vec3::splat(0.0031308), linear_rgb);

    lerp(a, b, c)
}

fn decode_srgb(screen_rgb: Vec3) -> Vec3 {
    let a = screen_rgb / 12.92;
    let b = ((screen_rgb + 0.055) / 1.055).pow(Vec3::splat(2.4);
    let c = step(Vec3::splat(0.04045), screen_rgb);

    lerp(a, b, c)
}

fn degamma(v: Vec4) -> Vec4 { 
    decode_srgb(v.xyz).extend(v.w)
}

fn hsv_to_rgb(c: Vec3) -> Vec3 {
    let K = Vec4::new(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);
    let p = ((c.xxx + K.xyz).fract() * 6.0 - K.www).abs();
    c.z * lerp(K.xxx, (p - K.xxx).clamp(0.0, 1.0), c.y)
}

fn extract_conservative_scale_from_transform(t: Mat4) -> f32 {
    let v1 = t.x_axis().truncate();
    let v2 = t.y_axis().truncate();
    let v3 = t.z_axis().truncate();

    let l1 = v1.dot(v1);
    let l2 = v2.dot(v2);
    let l3 = v3.dot(v3);

    l1.max(l2.max(l3)).sqrt()
}
