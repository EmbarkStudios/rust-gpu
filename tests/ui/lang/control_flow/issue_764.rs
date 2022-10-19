// build-pass

use glam::UVec3;
use spirv_std::glam;
use spirv_std::glam::{Mat3, Vec3, Vec4};
use spirv_std::spirv;

fn index_to_transform(index: usize, raw_data: &[u8]) -> Transform2D {
    Transform2D {
        own_transform: Mat3::IDENTITY,
        parent_offset: 0,
    }
}

const SIZE_OF_TRANSFORM: usize = core::mem::size_of::<Transform2D>();

#[derive(Clone)]
struct Transform2D {
    own_transform: Mat3,
    parent_offset: i32,
}

trait GivesFinalTransform {
    fn get_final_transform(&self, raw_data: &[u8]) -> Mat3;
}

impl GivesFinalTransform for (i32, Transform2D) {
    fn get_final_transform(&self, raw_data: &[u8]) -> Mat3 {
        if self.1.parent_offset == 0 {
            self.1.own_transform
        } else {
            let parent_index = self.0 + self.1.parent_offset;
            self.1.own_transform.mul_mat3(
                &((
                    parent_index as i32,
                    index_to_transform(parent_index as usize, raw_data),
                )
                    .get_final_transform(raw_data)),
            )
        }
    }
}

#[spirv(compute(threads(64)))]
pub fn main_cs(
    #[spirv(global_invocation_id)] id: UVec3,
    #[spirv(storage_buffer, descriptor_set = 0, binding = 0)] raw_data: &mut [u8],
    #[spirv(position)] output_position: &mut Vec4,
) {
    let index = id.x as usize;
    let final_transform =
        (index as i32, index_to_transform(index, raw_data)).get_final_transform(raw_data);
    *output_position = final_transform
        .mul_vec3(Vec3::new(0.1, 0.2, 0.3))
        .extend(0.0);
}
