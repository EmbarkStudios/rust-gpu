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

#[pixel_shader]
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

use std::spirv; //?

#[vertex_shader]
fn main() -> Vec4 {
    let instance = instance_data[spirv::instance_index()]; // gl_InstanceIndex

    let mut diffuse_color = if instance.flags.contains(VERTEX_COLOR_ENABLE) {
        degamma(in_color)
    } else {
        Vec4::splat(1.0)
    };

    if instance.flags.contains(HSV_TRANSFORM_ENABLE) {
        let rgb = diffuse_color.rgb;
        let hsv = rgb_to_hsv(rgb);

        let hsv = Vec3::new(
            (hsv.x() + instance.hsv_transform_data.x()).fract(),
            (hsv.y() + instance.hsv_transform_data.y()).clamp(0.0, 1.0),
            (hsv.z() + instance.hsv_transform_data.z()).clamp(0.0, 1.0),
        );

        let rgb = hsv_to_rgb(hsv);

        diffuse_color = rgb.extend(diffuse_color.w());
    }

    diffuse_color = diffuse_color * instance.diffuse_tint;

    let mut object_to_world = instance.object_to_world;

    if instance.flags.contains(BILLBOARD_ENABLE) {
        let scale = extract_conservative_scale_from_transform(object_to_world);
        object_to_world = Mat4::from_mat3(
            if spirv::view_index() == 0 {
                global_uniforms.view_to_world.to_mat3()
            } else {
                global_uniforms.view_to_world_stereo.to_mat3()
            } * Mat3::from_scale(scale)
        )

        object_to_world.set_z_axis(instance.object_to_world.z_axis());
    }

    let world_pos = (object_to_world * in_pos.extend(1.0)).truncate();

    let world_normal = if instance.flags.contains(NORMALS_ENABLE) {
        object_to_world.to_mat3() * in_normal
    } else {
        Vec3::new(0.0, 1.0, 0.0)
    };

    if instance.flags.contains(ALPHA_BLENDING_ENABLE | PREMULTIPLIED_ALPHA_ENABLE) {
        if instance.flags.contains(PREMULTIPLIED_ALPHA_ENABLE)  {
            opacity = diffuse_color.a;
            diffuse_color.a = 1.0;
        } else {
            opacity = diffuse_color.a;
        }
    } else {
        opacity = 1.0;
        diffuse_color.a = 1.0;
    }

    let position = if spirv::view_index() == 0 {
        global_uniforms.world_to_clip * world_pos.truncate().extend(1.0)
    } else {
        global_uniforms.world_to_clip_stereo * world_pos.truncate().extend(1.0)
    };

    position.set_y(-position.y())

    position
}