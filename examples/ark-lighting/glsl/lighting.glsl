struct SurfaceParams {
    vec3 diffuse_color;
    vec3 world_position;
    vec3 world_normal;
};

SurfaceParams surface_params(vec3 diffuse_color, vec3 world_position, vec3 world_normal) {
    return SurfaceParams(diffuse_color, world_position, world_normal);
}

struct EnvParams {
    vec3 sun_world_direction;
    vec3 sun_color;
    vec3 ground_color;
    vec3 sky_color;
    vec3 horizon_color;
};

EnvParams env_params(vec3 sun_world_direction,
                     vec3 sun_color,
                     vec3 ground_color,
                     vec3 sky_color,
                     vec3 horizon_color) {
    return EnvParams(sun_world_direction, sun_color, ground_color, sky_color, horizon_color);
}

struct ViewParams {
    vec3 camera_world_position;
};

ViewParams view_params(vec3 camera_world_position) {
    return ViewParams(camera_world_position);
}

vec3 ark_light(in SurfaceParams surface, in ViewParams view, in EnvParams env) {
    vec3 L = normalize(env.sun_world_direction);
    vec3 N = normalize(surface.world_normal);
    vec3 V = normalize(view.camera_world_position - surface.world_position);
    vec3 H = normalize(V + L);

    float LdotN = max(0.0, dot(N, L));
    float HdotN = max(0.0, dot(H, N));
    float specular = pow(HdotN, 80.0);

    // Microfacet fresnel.
    float fresnel = pow(max(0.0, 1.0 - dot(V, H)), 5.0);

    // Assuming the common IOR of 1.4, F0 is around 0.04.
    vec3 specular_color = env.sun_color * mix(0.04, 1.0, fresnel);

    // Do a two part gradient, from ground to horizon to sky, to match our
    // current simplistic sky model.
    vec3 ambient_light;
    if (N.y < 0.0) {
        ambient_light = mix(env.horizon_color, env.ground_color, -N.y);
    } else {
        ambient_light = mix(env.horizon_color, env.sky_color, N.y);
    }

    vec3 diffuse_light = LdotN * env.sun_color;
    vec3 specular_light = specular * specular_color;
    return (surface.diffuse_color.rgb * (ambient_light + diffuse_light) +
            specular_light);
}
