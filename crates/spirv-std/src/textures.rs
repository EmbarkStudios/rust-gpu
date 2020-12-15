use glam::{Vec2, Vec3A, Vec4};

#[allow(unused_attributes)]
#[spirv(sampler)]
#[derive(Copy, Clone)]
pub struct Sampler {
    _x: u32,
}

#[allow(unused_attributes)]
#[spirv(image(
    // sampled_type is hardcoded to f32 for now
    dim = "Dim2D",
    depth = 0,
    arrayed = 0,
    multisampled = 0,
    sampled = 1,
    image_format = "Unknown"
))]
#[derive(Copy, Clone)]
pub struct Image2d {
    _x: u32,
}

impl Image2d {
    pub fn sample(&self, sampler: Sampler, coord: Vec2) -> Vec4 {
        #[cfg(not(target_arch = "spirv"))]
        {
            let _ = sampler;
            let _ = coord;
            panic!("Image sampling not supported on CPU");
        }
        #[cfg(target_arch = "spirv")]
        unsafe {
            let mut result = Default::default();
            asm!(
                "%typeSampledImage = OpTypeSampledImage typeof*{1}",
                "%image = OpLoad typeof*{1} {1}",
                "%sampler = OpLoad typeof*{2} {2}",
                "%coord = OpLoad typeof*{3} {3}",
                "%sampledImage = OpSampledImage %typeSampledImage %image %sampler",
                "%result = OpImageSampleImplicitLod typeof*{0} %sampledImage %coord",
                "OpStore {0} %result",
                in(reg) &mut result,
                in(reg) self,
                in(reg) &sampler,
                in(reg) &coord
            );
            result
        }
    }
}

#[allow(unused_attributes)]
#[spirv(image(
    // sampled_type is hardcoded to f32 for now
    dim = "Dim2D",
    depth = 0,
    arrayed = 1,
    multisampled = 0,
    sampled = 1,
    image_format = "Unknown"
))]
#[derive(Copy, Clone)]
pub struct Image2dArray {
    _x: u32,
}

impl Image2dArray {
    pub fn sample(&self, sampler: Sampler, coord: Vec3A) -> Vec4 {
        #[cfg(not(target_arch = "spirv"))]
        {
            let _ = sampler;
            let _ = coord;
            panic!("Image sampling not supported on CPU");
        }
        #[cfg(target_arch = "spirv")]
        unsafe {
            let mut result = Default::default();
            asm!(
                "%typeSampledImage = OpTypeSampledImage typeof*{1}",
                "%image = OpLoad typeof*{1} {1}",
                "%sampler = OpLoad typeof*{2} {2}",
                "%coord = OpLoad typeof*{3} {3}",
                "%sampledImage = OpSampledImage %typeSampledImage %image %sampler",
                "%result = OpImageSampleImplicitLod typeof*{0} %sampledImage %coord",
                "OpStore {0} %result",
                in(reg) &mut result,
                in(reg) self,
                in(reg) &sampler,
                in(reg) &coord
            );
            result
        }
    }
}

#[allow(unused_attributes)]
#[spirv(sampled_image)]
#[derive(Copy, Clone)]
pub struct SampledImage<I> {
    _image: I,
}

impl SampledImage<Image2d> {
    pub fn sample(&self, coord: Vec3A) -> Vec4 {
        #[cfg(not(target_arch = "spirv"))]
        {
            let _ = coord;
            panic!("Image sampling not supported on CPU");
        }
        #[cfg(target_arch = "spirv")]
        unsafe {
            let mut result = Default::default();
            asm!(
                "%sampledImage = OpLoad typeof*{1} {1}",
                "%coord = OpLoad typeof*{2} {2}",
                "%result = OpImageSampleImplicitLod typeof*{0} %sampledImage %coord",
                "OpStore {0} %result",
                in(reg) &mut result,
                in(reg) self,
                in(reg) &coord
            );
            result
        }
    }
}
