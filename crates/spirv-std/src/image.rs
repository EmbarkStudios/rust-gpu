//! Image types

// Rustfmt formats long marker trait impls over multiple lines which makes them
// harder to read.
#[rustfmt::skip]
mod params;

pub use self::params::{ImageCoordinate, SampleType};
pub use crate::macros::Image;
pub use spirv_types::image_params::{
    AccessQualifier, Arrayed, Dimensionality, ImageDepth, ImageFormat, Multisampled, Sampled,
};

use crate::{float::Float, integer::Integer, vector::Vector, Sampler};

/// Re-export of primitive types to ensure the `Image` proc macro always points
/// to the right type.
#[doc(hidden)]
pub mod __private {
    pub use {f32, f64, i16, i32, i64, i8, u16, u32, u64, u8};
}

pub type Image2d = crate::Image!(2D, type=f32, sampled, __crate_root=crate);
pub type Cubemap = crate::Image!(cube, type=f32, sampled, __crate_root=crate);
pub type Image2dArray = crate::Image!(cube, type=f32, sampled, arrayed, __crate_root=crate);
pub type StorageImage2d = crate::Image!(cube, type=f32, sampled=false, __crate_root=crate);

/// An opaque image type. Corresponds to `OpTypeImage`.
#[spirv(generic_image_type)]
#[derive(Copy, Clone)]
pub struct Image<
    SampledType: SampleType<FORMAT>,
    const DIM: Dimensionality,
    const DEPTH: ImageDepth,
    const ARRAYED: Arrayed,
    const MULTISAMPLED: Multisampled,
    const SAMPLED: Sampled,
    const FORMAT: ImageFormat,
    const ACCESS_QUALIFIER: Option<AccessQualifier>,
> {
    _x: u32,
    _marker: core::marker::PhantomData<SampledType>,
}

impl<
        SampledType: SampleType<FORMAT>,
        const DIM: Dimensionality,
        const DEPTH: ImageDepth,
        const ARRAYED: Arrayed,
        const MULTISAMPLED: Multisampled,
        const FORMAT: ImageFormat,
        const ACCESS_QUALIFIER: Option<AccessQualifier>,
    >
    Image<
        SampledType,
        DIM,
        DEPTH,
        ARRAYED,
        MULTISAMPLED,
        { Sampled::Yes },
        FORMAT,
        ACCESS_QUALIFIER,
    >
{
    /// Fetch a single texel with a sampler set at compile time
    #[crate::macros::gpu_only]
    #[doc(alias = "OpImageFetch")]
    pub fn fetch<V, I>(&self, coordinate: impl ImageCoordinate<I, DIM, ARRAYED>) -> V
    where
        V: Vector<SampledType, 4>,
        I: Integer,
    {
        let mut result = V::default();
        unsafe {
            asm! {
                "%image = OpLoad _ {this}",
                "%coordinate = OpLoad _ {coordinate}",
                "%result = OpImageFetch typeof*{result} %image %coordinate",
                "OpStore {result} %result",
                result = in(reg) &mut result,
                this = in(reg) self,
                coordinate = in(reg) &coordinate,
            }
        }
        result
    }
}

impl<
        SampledType: SampleType<FORMAT>,
        const DIM: Dimensionality,
        const DEPTH: ImageDepth,
        const FORMAT: ImageFormat,
        const ARRAYED: Arrayed,
        const SAMPLED: Sampled,
        const ACCESS_QUALIFIER: Option<AccessQualifier>,
    >
    Image<
        SampledType,
        DIM,
        DEPTH,
        ARRAYED,
        { Multisampled::False },
        SAMPLED,
        FORMAT,
        ACCESS_QUALIFIER,
    >
{
    /// Sample texels at `coord` from the image using `sampler`.
    #[crate::macros::gpu_only]
    pub fn sample<F, V>(&self, sampler: Sampler, coord: impl ImageCoordinate<F, DIM, ARRAYED>) -> V
    where
        F: Float,
        V: Vector<SampledType, 4>,
    {
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

    /// Fetch a single texel with a sampler set at compile time
    #[crate::macros::gpu_only]
    #[doc(alias = "OpImageSampleExplicitLod")]
    /// Sample the image at a coordinate by a lod
    pub fn sample_by_lod<F, V>(
        &self,
        sampler: Sampler,
        coordinate: impl ImageCoordinate<F, DIM, ARRAYED>,
        lod: f32,
    ) -> V
    where
        F: Float,
        V: Vector<SampledType, 4>,
    {
        let mut result = Default::default();
        unsafe {
            asm!(
                "%image = OpLoad _ {this}",
                "%sampler = OpLoad _ {sampler}",
                "%coordinate = OpLoad _ {coordinate}",
                "%lod = OpLoad _ {lod}",
                "%sampledImage = OpSampledImage _ %image %sampler",
                "%result = OpImageSampleExplicitLod _ %sampledImage %coordinate Lod %lod",
                "OpStore {result} %result",
                result = in(reg) &mut result,
                this = in(reg) self,
                sampler = in(reg) &sampler,
                coordinate = in(reg) &coordinate,
                lod = in(reg) &lod
            );
        }
        result
    }

    #[crate::macros::gpu_only]
    #[doc(alias = "OpImageSampleExplicitLod")]
    /// Sample the image based on a gradient formed by (dx, dy). Specifically, ([du/dx, dv/dx], [du/dy, dv/dy])
    pub fn sample_by_gradient<F, V>(
        &self,
        sampler: Sampler,
        coordinate: impl ImageCoordinate<F, DIM, ARRAYED>,
        gradient_dx: impl ImageCoordinate<F, DIM, { Arrayed::False }>,
        gradient_dy: impl ImageCoordinate<F, DIM, { Arrayed::False }>,
    ) -> V
    where
        F: Float,
        V: Vector<SampledType, 4>,
    {
        let mut result = Default::default();
        unsafe {
            asm!(
                "%image = OpLoad _ {this}",
                "%sampler = OpLoad _ {sampler}",
                "%coordinate = OpLoad _ {coordinate}",
                "%gradient_dx = OpLoad _ {gradient_dx}",
                "%gradient_dy = OpLoad _ {gradient_dy}",
                "%sampledImage = OpSampledImage _ %image %sampler",
                "%result = OpImageSampleExplicitLod _ %sampledImage %coordinate Grad %gradient_dx %gradient_dy",
                "OpStore {result} %result",
                result = in(reg) &mut result,
                this = in(reg) self,
                sampler = in(reg) &sampler,
                coordinate = in(reg) &coordinate,
                gradient_dx = in(reg) &gradient_dx,
                gradient_dy = in(reg) &gradient_dy,
            );
        }
        result
    }

    #[crate::macros::gpu_only]
    #[doc(alias = "OpImageSampleDrefImplicitLod")]
    /// Sample the image's depth reference
    pub fn sample_depth_reference<F>(
        &self,
        sampler: Sampler,
        coordinate: impl ImageCoordinate<F, DIM, ARRAYED>,
        depth_reference: f32,
    ) -> SampledType
    where
        F: Float,
    {
        let mut result = Default::default();
        unsafe {
            asm!(
                "%image = OpLoad _ {this}",
                "%sampler = OpLoad _ {sampler}",
                "%coordinate = OpLoad _ {coordinate}",
                "%depth_reference = OpLoad _ {depth_reference}", // not required to do this way, but done for consistency
                "%sampledImage = OpSampledImage _ %image %sampler",
                "%result = OpImageSampleDrefImplicitLod _ %sampledImage %coordinate %depth_reference",
                "OpStore {result} %result",
                result = in(reg) &mut result,
                this = in(reg) self,
                sampler = in(reg) &sampler,
                coordinate = in(reg) &coordinate,
                depth_reference = in(reg) &depth_reference,
            );
        }
        result
    }

    #[crate::macros::gpu_only]
    #[doc(alias = "OpImageSampleDrefExplicitLod")]
    /// Sample the image's depth reference based on an explicit lod
    pub fn sample_depth_reference_by_lod<F>(
        &self,
        sampler: Sampler,
        coordinate: impl ImageCoordinate<F, DIM, ARRAYED>,
        depth_reference: f32,
        lod: f32,
    ) -> SampledType
    where
        F: Float,
    {
        let mut result = Default::default();
        unsafe {
            asm!(
                "%image = OpLoad _ {this}",
                "%sampler = OpLoad _ {sampler}",
                "%coordinate = OpLoad _ {coordinate}",
                "%depth_reference = OpLoad _ {depth_reference}",
                "%lod = OpLoad _ {lod}",
                "%sampledImage = OpSampledImage _ %image %sampler",
                "%result = OpImageSampleDrefExplicitLod _ %sampledImage %coordinate %depth_reference Lod %lod",
                "OpStore {result} %result",
                result = in(reg) &mut result,
                this = in(reg) self,
                sampler = in(reg) &sampler,
                coordinate = in(reg) &coordinate,
                depth_reference = in(reg) &depth_reference,
                lod = in(reg) &lod,
            )
        }
        result
    }

    #[crate::macros::gpu_only]
    #[doc(alias = "OpImageSampleDrefExplicitLod")]
    /// Sample the image's depth reference based on a gradient formed by (dx, dy).
    /// Specifically, ([du/dx, dv/dx], [du/dy, dv/dy])
    pub fn sample_depth_reference_by_gradient<F>(
        &self,
        sampler: Sampler,
        coordinate: impl ImageCoordinate<F, DIM, ARRAYED>,
        depth_reference: f32,
        gradient_dx: impl ImageCoordinate<F, DIM, { Arrayed::False }>,
        gradient_dy: impl ImageCoordinate<F, DIM, { Arrayed::False }>,
    ) -> SampledType
    where
        F: Float,
    {
        let mut result = Default::default();
        unsafe {
            asm!(
                "%image = OpLoad _ {this}",
                "%sampler = OpLoad _ {sampler}",
                "%coordinate = OpLoad _ {coordinate}",
                "%depth_reference = OpLoad _ {depth_reference}",
                "%gradient_dx = OpLoad _ {gradient_dx}",
                "%gradient_dy = OpLoad _ {gradient_dy}",
                "%sampledImage = OpSampledImage _ %image %sampler",
                "%result = OpImageSampleDrefExplicitLod _ %sampledImage %coordinate %depth_reference Grad %gradient_dx %gradient_dy",
                "OpStore {result} %result",
                result = in(reg) &mut result,
                this = in(reg) self,
                sampler = in(reg) &sampler,
                coordinate = in(reg) &coordinate,
                depth_reference = in(reg) &depth_reference,
                gradient_dx = in(reg) &gradient_dx,
                gradient_dy = in(reg) &gradient_dy,
            );
        }
        result
    }
}

impl<
        SampledType: SampleType<FORMAT>,
        const DIM: Dimensionality,
        const DEPTH: ImageDepth,
        const SAMPLED: Sampled,
        const FORMAT: ImageFormat,
        const ACCESS_QUALIFIER: Option<AccessQualifier>,
    >
    Image<
        SampledType,
        DIM,
        DEPTH,
        { Arrayed::False },
        { Multisampled::False },
        SAMPLED,
        FORMAT,
        ACCESS_QUALIFIER,
    >
{
    /// Fetch a single texel with a sampler set at compile time
    #[crate::macros::gpu_only]
    #[doc(alias = "OpImageFetch")]
    pub fn sample_with_project_coordinate<F, V>(
        &self,
        sampler: Sampler,
        project_coordinate: impl ImageCoordinate<F, DIM, { Arrayed::True }>,
    ) -> V
    where
        F: Float,
        V: Vector<SampledType, 4>,
    {
        unsafe {
            let mut result = Default::default();
            asm!(
                "%image = OpLoad _ {this}",
                "%sampler = OpLoad _ {sampler}",
                "%project_coordinate = OpLoad _ {project_coordinate}",
                "%sampledImage = OpSampledImage _ %image %sampler",
                "%result = OpImageSampleProjImplicitLod _ %sampledImage %project_coordinate",
                "OpStore {result} %result",
                result = in(reg) &mut result,
                this = in(reg) self,
                sampler = in(reg) &sampler,
                project_coordinate = in(reg) &project_coordinate,
            );
            result
        }
    }

    #[crate::macros::gpu_only]
    #[doc(alias = "OpImageSampleProjExplicitLod")]
    /// Sample the image with a project coordinate by a lod
    pub fn sample_with_project_coordinate_by_lod<F, V>(
        &self,
        sampler: Sampler,
        project_coordinate: impl ImageCoordinate<F, DIM, { Arrayed::True }>,
        lod: f32,
    ) -> V
    where
        F: Float,
        V: Vector<SampledType, 4>,
    {
        let mut result = Default::default();
        unsafe {
            asm!(
                "%image = OpLoad _ {this}",
                "%sampler = OpLoad _ {sampler}",
                "%project_coordinate = OpLoad _ {project_coordinate}",
                "%lod = OpLoad _ {lod}",
                "%sampledImage = OpSampledImage _ %image %sampler",
                "%result = OpImageSampleProjExplicitLod _ %sampledImage %project_coordinate Lod %lod",
                "OpStore {result} %result",
                result = in(reg) &mut result,
                this = in(reg) self,
                sampler = in(reg) &sampler,
                project_coordinate = in(reg) &project_coordinate,
                lod = in(reg) &lod
            );
        }
        result
    }

    #[crate::macros::gpu_only]
    #[doc(alias = "OpImageSampleProjExplicitLod")]
    /// Sample the image with a project coordinate based on a gradient formed by (dx, dy). Specifically, ([du/dx, dv/dx], [du/dy, dv/dy])
    pub fn sample_with_project_coordinate_by_gradient<F, V>(
        &self,
        sampler: Sampler,
        project_coordinate: impl ImageCoordinate<F, DIM, { Arrayed::True }>,
        gradient_dx: impl ImageCoordinate<F, DIM, { Arrayed::False }>,
        gradient_dy: impl ImageCoordinate<F, DIM, { Arrayed::False }>,
    ) -> V
    where
        F: Float,
        V: Vector<SampledType, 4>,
    {
        let mut result = Default::default();
        unsafe {
            asm!(
                "%image = OpLoad _ {this}",
                "%sampler = OpLoad _ {sampler}",
                "%project_coordinate = OpLoad _ {project_coordinate}",
                "%gradient_dx = OpLoad _ {gradient_dx}",
                "%gradient_dy = OpLoad _ {gradient_dy}",
                "%sampledImage = OpSampledImage _ %image %sampler",
                "%result = OpImageSampleProjExplicitLod _ %sampledImage %project_coordinate Grad %gradient_dx %gradient_dy",
                "OpStore {result} %result",
                result = in(reg) &mut result,
                this = in(reg) self,
                sampler = in(reg) &sampler,
                project_coordinate = in(reg) &project_coordinate,
                gradient_dx = in(reg) &gradient_dx,
                gradient_dy = in(reg) &gradient_dy,
            );
        }
        result
    }

    #[crate::macros::gpu_only]
    #[doc(alias = "OpImageSampleProjDrefImplicitLod")]
    /// Sample the image's depth reference with the project coordinate
    pub fn sample_depth_reference_with_project_coordinate<F>(
        &self,
        sampler: Sampler,
        project_coordinate: impl ImageCoordinate<F, DIM, { Arrayed::True }>,
        depth_reference: f32,
    ) -> SampledType
    where
        F: Float,
    {
        let mut result = Default::default();
        unsafe {
            asm!(
                "%image = OpLoad _ {this}",
                "%sampler = OpLoad _ {sampler}",
                "%project_coordinate = OpLoad _ {project_coordinate}",
                "%depth_reference = OpLoad _ {depth_reference}", // not required to do this way, but done for consistency
                "%sampledImage = OpSampledImage _ %image %sampler",
                "%result = OpImageSampleProjDrefImplicitLod _ %sampledImage %project_coordinate %depth_reference",
                "OpStore {result} %result",
                result = in(reg) &mut result,
                this = in(reg) self,
                sampler = in(reg) &sampler,
                project_coordinate = in(reg) &project_coordinate,
                depth_reference = in(reg) &depth_reference,
            );
        }
        result
    }

    #[crate::macros::gpu_only]
    #[doc(alias = "OpImageSampleProjDrefExplicitLod")]
    /// Sample the image's depth reference with the project coordinate based on an explicit lod
    pub fn sample_depth_reference_with_project_coordinate_by_lod<F>(
        &self,
        sampler: Sampler,
        coordinate: impl ImageCoordinate<F, DIM, { Arrayed::True }>,
        depth_reference: f32,
        lod: f32,
    ) -> SampledType
    where
        F: Float,
    {
        let mut result = Default::default();
        unsafe {
            asm!(
                "%image = OpLoad _ {this}",
                "%sampler = OpLoad _ {sampler}",
                "%coordinate = OpLoad _ {coordinate}",
                "%depth_reference = OpLoad _ {depth_reference}",
                "%lod = OpLoad _ {lod}",
                "%sampledImage = OpSampledImage _ %image %sampler",
                "%result = OpImageSampleProjDrefExplicitLod _ %sampledImage %coordinate %depth_reference Lod %lod",
                "OpStore {result} %result",
                result = in(reg) &mut result,
                this = in(reg) self,
                sampler = in(reg) &sampler,
                coordinate = in(reg) &coordinate,
                depth_reference = in(reg) &depth_reference,
                lod = in(reg) &lod,
            )
        }
        result
    }

    #[crate::macros::gpu_only]
    #[doc(alias = "OpImageSampleProjDrefExplicitLod")]
    /// Sample the image's depth reference with the project coordinate based on a gradient formed by (dx, dy).
    /// Specifically, ([du/dx, dv/dx], [du/dy, dv/dy])
    pub fn sample_depth_reference_with_project_coordinate_by_gradient<F>(
        &self,
        sampler: Sampler,
        coordinate: impl ImageCoordinate<F, DIM, { Arrayed::True }>,
        depth_reference: f32,
        gradient_dx: impl ImageCoordinate<F, DIM, { Arrayed::False }>,
        gradient_dy: impl ImageCoordinate<F, DIM, { Arrayed::False }>,
    ) -> SampledType
    where
        F: Float,
    {
        let mut result = Default::default();
        unsafe {
            asm!(
                "%image = OpLoad _ {this}",
                "%sampler = OpLoad _ {sampler}",
                "%coordinate = OpLoad _ {coordinate}",
                "%depth_reference = OpLoad _ {depth_reference}",
                "%gradient_dx = OpLoad _ {gradient_dx}",
                "%gradient_dy = OpLoad _ {gradient_dy}",
                "%sampledImage = OpSampledImage _ %image %sampler",
                "%result = OpImageSampleProjDrefExplicitLod _ %sampledImage %coordinate %depth_reference Grad %gradient_dx %gradient_dy",
                "OpStore {result} %result",
                result = in(reg) &mut result,
                this = in(reg) self,
                sampler = in(reg) &sampler,
                coordinate = in(reg) &coordinate,
                depth_reference = in(reg) &depth_reference,
                gradient_dx = in(reg) &gradient_dx,
                gradient_dy = in(reg) &gradient_dy,
            );
        }
        result
    }
}

impl<
        SampledType: SampleType<FORMAT>,
        const DIM: Dimensionality,
        const DEPTH: ImageDepth,
        const ARRAYED: Arrayed,
        const MULTISAMPLED: Multisampled,
        const FORMAT: ImageFormat,
        const ACCESS_QUALIFIER: Option<AccessQualifier>,
    >
    Image<SampledType, DIM, DEPTH, ARRAYED, MULTISAMPLED, { Sampled::No }, FORMAT, ACCESS_QUALIFIER>
{
    /// Read a texel from an image without a sampler.
    #[crate::macros::gpu_only]
    #[doc(alias = "OpImageRead")]
    pub fn read<I, V, const N: usize>(&self, coordinate: impl ImageCoordinate<I, DIM, ARRAYED>) -> V
    where
        I: Integer,
        V: Vector<SampledType, N>,
    {
        let mut result = V::default();

        unsafe {
            asm! {
                "%image = OpLoad _ {this}",
                "%coordinate = OpLoad _ {coordinate}",
                "%result = OpImageRead typeof*{result} %image %coordinate",
                "OpStore {result} %result",
                this = in(reg) self,
                coordinate = in(reg) &coordinate,
                result = in(reg) &mut result,
            }
        }

        result
    }

    /// Write a texel to an image without a sampler.
    #[crate::macros::gpu_only]
    #[doc(alias = "OpImageWrite")]
    pub unsafe fn write<I, const N: usize>(
        &self,
        coordinate: impl ImageCoordinate<I, DIM, ARRAYED>,
        texels: impl Vector<SampledType, N>,
    ) where
        I: Integer,
    {
        asm! {
            "%image = OpLoad _ {this}",
            "%coordinate = OpLoad _ {coordinate}",
            "%texels = OpLoad _ {texels}",
            "OpImageWrite %image %coordinate %texels",
            this = in(reg) self,
            coordinate = in(reg) &coordinate,
            texels = in(reg) &texels,
        }
    }
}

impl<
        SampledType: SampleType<FORMAT>,
        const DIM: Dimensionality,
        const DEPTH: ImageDepth,
        const FORMAT: ImageFormat,
        const ARRAYED: Arrayed,
        const MULTISAMPLED: Multisampled,
        const ACCESS_QUALIFIER: Option<AccessQualifier>,
    >
    Image<
        SampledType,
        DIM,
        DEPTH,
        ARRAYED,
        MULTISAMPLED,
        { Sampled::Unknown },
        FORMAT,
        ACCESS_QUALIFIER,
    >
{
    /// Read a texel from an image without a sampler.
    #[crate::macros::gpu_only]
    #[doc(alias = "OpImageRead")]
    pub fn read<I, V, const N: usize>(&self, coordinate: impl ImageCoordinate<I, DIM, ARRAYED>) -> V
    where
        I: Integer,
        V: Vector<SampledType, N>,
    {
        let mut result = V::default();

        unsafe {
            asm! {
                "%image = OpLoad _ {this}",
                "%coordinate = OpLoad _ {coordinate}",
                "%result = OpImageRead typeof*{result} %image %coordinate",
                "OpStore {result} %result",
                this = in(reg) self,
                coordinate = in(reg) &coordinate,
                result = in(reg) &mut result,
            }
        }

        result
    }

    /// Write a texel to an image without a sampler.
    #[crate::macros::gpu_only]
    #[doc(alias = "OpImageWrite")]
    pub unsafe fn write<I, const N: usize>(
        &self,
        coordinate: impl ImageCoordinate<I, DIM, ARRAYED>,
        texels: impl Vector<SampledType, N>,
    ) where
        I: Integer,
    {
        asm! {
            "%image = OpLoad _ {this}",
            "%coordinate = OpLoad _ {coordinate}",
            "%texels = OpLoad _ {texels}",
            "OpImageWrite %image %coordinate %texels",
            this = in(reg) self,
            coordinate = in(reg) &coordinate,
            texels = in(reg) &texels,
        }
    }
}

/// An image combined with a sampler, enabling filtered accesses of the
/// image’s contents.
#[spirv(sampled_image)]
#[derive(Copy, Clone)]
pub struct SampledImage<I> {
    _image: I,
}

impl<
        SampledType: SampleType<FORMAT>,
        const DIM: Dimensionality,
        const DEPTH: ImageDepth,
        const ARRAYED: Arrayed,
        const SAMPLED: Sampled,
        const FORMAT: ImageFormat,
        const ACCESS_QUALIFIER: Option<AccessQualifier>,
    >
    SampledImage<
        Image<
            SampledType,
            DIM,
            DEPTH,
            ARRAYED,
            { Multisampled::False },
            SAMPLED,
            FORMAT,
            ACCESS_QUALIFIER,
        >,
    >
{
    /// Sample texels at `coord` from the sampled image.
    ///
    /// # Safety
    /// Sampling with a type (`S`) that doesn't match the image's image format
    /// will result in undefined behaviour.
    #[crate::macros::gpu_only]
    pub unsafe fn sample<F, V>(&self, coord: impl ImageCoordinate<F, DIM, ARRAYED>) -> V
    where
        F: Float,
        V: Vector<SampledType, 4>,
    {
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
