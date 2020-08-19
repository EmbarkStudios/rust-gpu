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

    let ambient_light = if n.y() < 0.0 {
        lerp(env.horizon_color, env.ground_color, -n.y())
    } else {
        lerp(env.horizon_color, env.sky_color, n.y())
    };

    let diffuse_light = l_dot_n * env.sun_color;
    let specular_light = specular * specular_color;

    surface.diffuse_color.rgb * (ambient_light + diffuse_light) +
        specular_light
}
