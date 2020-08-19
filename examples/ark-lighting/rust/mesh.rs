use lighting;

bitflags! {
    struct Flags: u32 {
        const LIGHTING_ENABLE = 0b00000001;
        const ALPHA_BLENDING_ENABLE = 0b00000010;
        const FLAT_SHADING_ENABLE = 0b00001000;
        const HSV_TRANSFORM_ENABLE = 0b00010000;
        const PREMULTIPLIED_ALPHA_ENABLE = 0b00100000;
        const BILLBOARD_ENABLE = 0b01000000;

        const VERTEX_COLOR_ENABLE = 1 << 16;
        const NORMALS_ENABLE = 1 << 16;
    }
}

struct Constants {
    env_params: lighting::EnvParams,
    view_params: lighting::ViewParams,
}

struct Inputs {
    diffuse_color: Vec4,
    world_normal: Vec3,
    world_pos: Vec3,
    opacity: Vec3,

    #[flat]
    flags: Flags,
}

fn main(
    constants: Constants,
    inputs: Inputs,
) -> Vec4 {
    if input.flags.contains(LIGHTING_ENABLE) {
        let normal = if input.flags.contains(FLAT_SHADING_ENABLE) {
            let world_pos = input.world_pos;
            // don't quite know how to model screen space derivatives:
            (-world_pos.df_dx().cross(world_pos.df_dy())).normalize()
        } else {
            inputs.world_normal;
        };

        let surface_params = lighting::SurfaceParams {
            diffuse_color: diffuse_color.truncate(),
            world_pos: input.world_pos,
            normal: input.world_normal,
        };
        
        lighting::ark_light(surface_params, constants.view_params, constants.env_params).extend(input.opacity)
    } else {
        (input.diffuse_color.truncate() * input.diffuse_color.w()).extend(input.opacity)
    }
}