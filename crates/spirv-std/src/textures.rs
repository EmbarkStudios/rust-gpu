use core::marker::PhantomData;
use glam::{Vec2, Vec3A, Vec4};

#[allow(unused_attributes)]
#[spirv(sampler)]
#[derive(Copy, Clone)]
pub struct Sampler {
    _x: u32,
}

#[allow(unused_attributes)]
#[spirv(sampled_image)]
#[derive(Copy, Clone)]
pub struct SampledImage<I: Copy> {
    _image: I,
}

/// Image memory.
///
/// A traditional texture or image; SPIR-V has this single name for these.
/// An image does not include any information about how to access, filter,
/// or sample it.
#[allow(unused_attributes)]
#[spirv(image)]
#[derive(Copy, Clone)]
pub struct Image<
    T: sealed_traits::SampledType + Copy,
    Dims: sealed_traits::ImageDims,
    Depth: sealed_traits::ImageDepth,
    Sampled: sealed_traits::ImageSampled,
    Format: sealed_traits::ImageFormat,
    Arrayed: sealed_traits::ImageArrayed,
    Multisampled: sealed_traits::ImageMultisampled,
> {
    _opaque: u32,
    marker: PhantomData<(T, Dims, Depth, Sampled, Format, Arrayed, Multisampled)>,
}

pub type Image2d =
    Image<f32, dims::D2, depth::No, sample::No, format::Unknown, array::No, multisample::No>;

pub type Image2dArray =
    Image<f32, dims::D2, depth::No, sample::No, format::Unknown, array::Yes, multisample::No>;

impl Image2d {
    #[spirv_std_macros::gpu_only]
    pub fn sample(&self, sampler: Sampler, coord: Vec2) -> Vec4 {
        unsafe {
            let mut result = Default::default();
            asm!(
                "%image = OpLoad _ {1}",
                "%sampler = OpLoad _ {2}",
                "%coord = OpLoad _ {3}",
                "%sampledImage = OpSampledImage _ %image %sampler",
                "%result = OpImageSampleImplicitLod _ %sampledImage %coord",
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

impl Image2dArray {
    #[spirv_std_macros::gpu_only]
    pub fn sample(&self, sampler: Sampler, coord: Vec3A) -> Vec4 {
        unsafe {
            let mut result = Default::default();
            asm!(
                "%image = OpLoad _ {1}",
                "%sampler = OpLoad _ {2}",
                "%coord = OpLoad _ {3}",
                "%sampledImage = OpSampledImage _ %image %sampler",
                "%result = OpImageSampleImplicitLod _ %sampledImage %coord",
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

impl SampledImage<Image2d> {
    #[spirv_std_macros::gpu_only]
    pub fn sample(&self, coord: Vec2) -> Vec4 {
        unsafe {
            let mut result = Default::default();
            asm!(
                "%sampledImage = OpLoad _ {1}",
                "%coord = OpLoad _ {2}",
                "%result = OpImageSampleImplicitLod _ %sampledImage %coord",
                "OpStore {0} %result",
                in(reg) &mut result,
                in(reg) self,
                in(reg) &coord
            );
            result
        }
    }
}

use image_options::*;
pub mod image_options {
    use super::sealed_structs;
    pub mod dims {
        // These definitions must be kept in line with ImageDims in rspirv/spirv spec
        use super::sealed_structs::ImageDims;
        pub type D1 = ImageDims<0>;
        pub type D2 = ImageDims<1>;
        pub type D3 = ImageDims<2>;
        pub type Cube = ImageDims<3>;
        pub type Rect = ImageDims<4>;
        pub type Buffer = ImageDims<5>;
        pub type Subpass = ImageDims<6>;
    }

    pub mod depth {
        // these values must be kept in line with rspirv/spirv spec depth param in OpTypeImage
        use super::sealed_structs::ImageDepth;
        pub type No = ImageDepth<0>;
        pub type Yes = ImageDepth<1>;
        pub type Maybe = ImageDepth<2>;
    }

    pub mod sample {
        // these values must be kept in line with rspirv/spirv spec sampled param in OpTypeImage
        use super::sealed_structs::ImageSampled;
        pub type Maybe = ImageSampled<0>;
        pub type Yes = ImageSampled<1>;
        pub type No = ImageSampled<2>;
    }

    pub mod format {
        // These definitions must be kept in line with ImageFormat in rspirv/spirv spec
        use super::sealed_structs::ImageFormat;
        pub type Unknown = ImageFormat<0>;
        pub type Rgba32f = ImageFormat<1>;
        pub type Rgba16f = ImageFormat<2>;
        pub type R32f = ImageFormat<3>;
        pub type Rgba8 = ImageFormat<4>;
        pub type Rgba8Snorm = ImageFormat<5>;
        pub type Rg32f = ImageFormat<6>;
        pub type Rg16f = ImageFormat<7>;
        pub type R11fG11fB10f = ImageFormat<8>;
        pub type R16f = ImageFormat<9>;
        pub type Rgba16 = ImageFormat<10>;
        pub type Rgb10A2 = ImageFormat<11>;
        pub type Rg16 = ImageFormat<12>;
        pub type Rg8 = ImageFormat<13>;
        pub type R16 = ImageFormat<14>;
        pub type R8 = ImageFormat<15>;
        pub type Rgba16Snorm = ImageFormat<16>;
        pub type Rg16Snorm = ImageFormat<17>;
        pub type Rg8Snorm = ImageFormat<18>;
        pub type R16Snorm = ImageFormat<19>;
        pub type R8Snorm = ImageFormat<20>;
        pub type Rgba32i = ImageFormat<21>;
        pub type Rgba16i = ImageFormat<22>;
        pub type Rgba8i = ImageFormat<23>;
        pub type R32i = ImageFormat<24>;
        pub type Rg32i = ImageFormat<25>;
        pub type Rg16i = ImageFormat<26>;
        pub type Rg8i = ImageFormat<27>;
        pub type R16i = ImageFormat<28>;
        pub type R8i = ImageFormat<29>;
        pub type Rgba32ui = ImageFormat<30>;
        pub type Rgba16ui = ImageFormat<31>;
        pub type Rgba8ui = ImageFormat<32>;
        pub type R32ui = ImageFormat<33>;
        pub type Rgb10a2ui = ImageFormat<34>;
        pub type Rg32ui = ImageFormat<35>;
        pub type Rg16ui = ImageFormat<36>;
        pub type Rg8ui = ImageFormat<37>;
        pub type R16ui = ImageFormat<38>;
        pub type R8ui = ImageFormat<39>;
        pub type R64ui = ImageFormat<40>;
        pub type R64i = ImageFormat<41>;
    }

    pub mod array {
        use super::sealed_structs::ImageArrayed;
        pub type No = ImageArrayed<0>;
        pub type Yes = ImageArrayed<1>;
    }

    pub mod multisample {
        use super::sealed_structs::ImageMultisampled;
        pub type No = ImageMultisampled<0>;
        pub type Yes = ImageMultisampled<1>;
    }
}

mod sealed_structs {
    /// FORMAT values must be kept in line with `ImageFormat` enum in rspirv
    #[derive(Copy, Clone)]
    pub struct ImageFormat<const FORMAT: usize>;

    /// DIMS values must be kept in line with `ImageFormat` enum in rspirv
    #[derive(Copy, Clone)]
    pub struct ImageDims<const DIMS: usize>;

    #[derive(Copy, Clone)]
    pub struct ImageDepth<const DEPTH: usize>;
    #[derive(Copy, Clone)]
    pub struct ImageSampled<const SAMPLED: usize>;
    #[derive(Copy, Clone)]
    pub struct ImageArrayed<const ARRAYED: usize>;
    #[derive(Copy, Clone)]
    pub struct ImageMultisampled<const MS: usize>;
}

mod sealed_traits {
    pub trait Image {}
    impl<
            'a,
            T: SampledType + Copy,
            Dims: ImageDims,
            Depth: ImageDepth,
            Sampled: ImageSampled,
            Format: ImageFormat,
            Arrayed: ImageArrayed,
            Multisampled: ImageMultisampled,
        > Image for super::Image<T, Dims, Depth, Sampled, Format, Arrayed, Multisampled>
    {
    }

    pub trait ImageFormat {}
    impl<const FORMAT: usize> ImageFormat for super::sealed_structs::ImageFormat<FORMAT> {}

    pub trait ImageDims {}
    impl<const DIMS: usize> ImageDims for super::sealed_structs::ImageDims<DIMS> {}

    pub trait SampledType {}
    impl SampledType for () {}
    impl SampledType for f32 {}
    impl SampledType for f64 {}
    impl SampledType for u8 {}
    impl SampledType for u16 {}
    impl SampledType for u32 {}
    impl SampledType for u64 {}
    impl SampledType for i8 {}
    impl SampledType for i16 {}
    impl SampledType for i32 {}
    impl SampledType for i64 {}

    pub trait ImageDepth {}
    impl<const DEPTH: usize> ImageDepth for super::sealed_structs::ImageDepth<DEPTH> {}

    pub trait ImageSampled {}
    impl<const SAMPLED: usize> ImageSampled for super::sealed_structs::ImageSampled<SAMPLED> {}
    pub trait ImageArrayed {}
    impl<const ARRAYED: usize> ImageArrayed for super::sealed_structs::ImageArrayed<ARRAYED> {}
    pub trait ImageMultisampled {}
    impl<const MS: usize> ImageMultisampled for super::sealed_structs::ImageMultisampled<MS> {}
}
