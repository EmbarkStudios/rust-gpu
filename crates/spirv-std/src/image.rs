//! Image types

#[cfg(target_arch = "spirv")]
use core::arch::asm;

// Rustfmt formats long marker trait impls over multiple lines which makes them
// harder to read.
#[rustfmt::skip]
mod params;

pub use self::params::{ImageCoordinate, ImageCoordinateSubpassData, SampleType};
pub use crate::macros::Image;
pub use spirv_std_types::image_params::{
    AccessQualifier, Arrayed, Dimensionality, ImageDepth, ImageFormat, Multisampled, Sampled,
};

use crate::{float::Float, integer::Integer, vector::Vector, Sampler};

/// Re-export of primitive types to ensure the `Image` proc macro always points
/// to the right type.
#[doc(hidden)]
pub mod __private {
    pub use {f32, f64, i16, i32, i64, i8, u16, u32, u64, u8};
}

/// A 1d image used with a sampler.
pub type Image1d = crate::Image!(1D, type=f32, sampled, __crate_root=crate);
/// A 2d image used with a sampler. This is pretty typical and probably what you want.
pub type Image2d = crate::Image!(2D, type=f32, sampled, __crate_root=crate);
/// A 3d image used with a sampler.
pub type Image3d = crate::Image!(3D, type=f32, sampled, __crate_root=crate);
/// A 1d image used with a sampler, containing unsigned integer data.
pub type Image1dU = crate::Image!(1D, type=u32, sampled, __crate_root=crate);
/// A 2d image used with a sampler, containing unsigned integer data.
pub type Image2dU = crate::Image!(2D, type=u32, sampled, __crate_root=crate);
/// A 3d image used with a sampler, containing unsigned integer data.
pub type Image3dU = crate::Image!(3D, type=u32, sampled, __crate_root=crate);
/// A 1d image used with a sampler, containing signed integer data.
pub type Image1dI = crate::Image!(1D, type=i32, sampled, __crate_root=crate);
/// A 2d image used with a sampler, containing signed integer data.
pub type Image2dI = crate::Image!(2D, type=i32, sampled, __crate_root=crate);
/// A 3d image used with a sampler, containing signed integer data.
pub type Image3dI = crate::Image!(3D, type=i32, sampled, __crate_root=crate);

/// An array of 1d images, used with a sampler.
pub type Image1dArray = crate::Image!(1D, type=f32, sampled, arrayed, __crate_root=crate);
/// An array of 2d images, used with a sampler.
pub type Image2dArray = crate::Image!(2D, type=f32, sampled, arrayed, __crate_root=crate);
/// An array of 3d images, used with a sampler.
pub type Image3dArray = crate::Image!(3D, type=f32, sampled, arrayed, __crate_root=crate);
/// An array of 1d images, used with a sampler, each containing unsigned integer data.
pub type Image1dUArray = crate::Image!(1D, type=u32, sampled, arrayed, __crate_root=crate);
/// An array of 2d images, used with a sampler, each containing unsigned integer data.
pub type Image2dUArray = crate::Image!(2D, type=u32, sampled, arrayed, __crate_root=crate);
/// An array of 3d images, used with a sampler, each containing unsigned integer data.
pub type Image3dUArray = crate::Image!(3D, type=u32, sampled, arrayed, __crate_root=crate);
/// An array of 1d images, used with a sampler, each containing signed integer data.
pub type Image1dIArray = crate::Image!(1D, type=i32, sampled, arrayed, __crate_root=crate);
/// An array of 2d images, used with a sampler, each containing signed integer data.
pub type Image2dIArray = crate::Image!(2D, type=i32, sampled, arrayed, __crate_root=crate);
/// An array of 3d images, used with a sampler, each containing signed integer data.
pub type Image3dIArray = crate::Image!(3D, type=i32, sampled, arrayed, __crate_root=crate);

/// A 1d storage image, directly accessed, without using a sampler.
pub type StorageImage1d = crate::Image!(1D, type=f32, sampled=false, __crate_root=crate);
/// A 2d storage image, directly accessed, without using a sampler.
pub type StorageImage2d = crate::Image!(2D, type=f32, sampled=false, __crate_root=crate);
/// A 3d storage image, directly accessed, without using a sampler.
pub type StorageImage3d = crate::Image!(3D, type=f32, sampled=false, __crate_root=crate);
/// A 1d storage image, directly accessed without a sampler, containing unsigned integer data.
pub type StorageImage1dU = crate::Image!(1D, type=u32, sampled=false, __crate_root=crate);
/// A 2d storage image, directly accessed without a sampler, containing unsigned integer data.
pub type StorageImage2dU = crate::Image!(2D, type=u32, sampled=false, __crate_root=crate);
/// A 3d storage image, directly accessed without a sampler, containing unsigned integer data.
pub type StorageImage3dU = crate::Image!(3D, type=u32, sampled=false, __crate_root=crate);
/// A 1d storage image, directly accessed without a sampler, containing signed integer data.
pub type StorageImage1dI = crate::Image!(1D, type=i32, sampled=false, __crate_root=crate);
/// A 2d storage image, directly accessed without a sampler, containing signed integer data.
pub type StorageImage2dI = crate::Image!(2D, type=i32, sampled=false, __crate_root=crate);
/// A 3d storage image, directly accessed without a sampler, containing signed integer data.
pub type StorageImage3dI = crate::Image!(3D, type=i32, sampled=false, __crate_root=crate);

/// A cubemap, i.e. a cube of 6 textures, sampled using a direction rather than image coordinates.
pub type Cubemap = crate::Image!(cube, type=f32, sampled, __crate_root=crate);

// TODO: Migrate Image parameters back to their enum values once #![feature(adt_const_params)] is
// stabilized.

/// An opaque image type. Corresponds to `OpTypeImage`.
///
/// You likely want to write this type using the [`crate::Image!`] macro helper, as the generic
/// arguments here can get extremely verbose.
///
/// See SPIR-V OpTypeImage specification for the meaning of integer parameters.
#[spirv(generic_image_type)]
#[derive(Copy, Clone)]
pub struct Image<
    SampledType: SampleType<FORMAT>,
    const DIM: u32,          // Dimensionality,
    const DEPTH: u32,        // ImageDepth,
    const ARRAYED: u32,      // Arrayed,
    const MULTISAMPLED: u32, // Multisampled,
    const SAMPLED: u32,      // Sampled,
    const FORMAT: u32,       // ImageFormat,
> {
    _x: u32,
    _marker: core::marker::PhantomData<SampledType>,
}

impl<
        SampledType: SampleType<FORMAT>,
        const DIM: u32,
        const DEPTH: u32,
        const ARRAYED: u32,
        const MULTISAMPLED: u32,
        const FORMAT: u32,
    > Image<SampledType, DIM, DEPTH, ARRAYED, MULTISAMPLED, { Sampled::Yes as u32 }, FORMAT>
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
        const DIM: u32,
        const DEPTH: u32,
        const FORMAT: u32,
        const ARRAYED: u32,
        const SAMPLED: u32,
    > Image<SampledType, DIM, DEPTH, ARRAYED, { Multisampled::False as u32 }, SAMPLED, FORMAT>
{
    // Note: #[inline] is needed because in vulkan, the component must be a constant expression.
    /// Gathers the requested component from four texels.
    #[crate::macros::gpu_only]
    #[doc(alias = "OpImageGather")]
    #[inline]
    pub fn gather<F, V>(
        &self,
        sampler: Sampler,
        coordinate: impl ImageCoordinate<F, DIM, ARRAYED>,
        component: u32,
    ) -> V
    where
        Self: HasGather,
        F: Float,
        V: Vector<SampledType, 4>,
    {
        let mut result = V::default();
        unsafe {
            asm! {
                "%typeSampledImage = OpTypeSampledImage typeof*{this}",
                "%image = OpLoad _ {this}",
                "%sampler = OpLoad _ {sampler}",
                "%coordinate = OpLoad _ {coordinate}",
                "%sampledImage = OpSampledImage %typeSampledImage %image %sampler",
                "%result = OpImageGather typeof*{result} %sampledImage %coordinate {component}",
                "OpStore {result} %result",
                result = in(reg) &mut result,
                this = in(reg) self,
                sampler = in(reg) &sampler,
                coordinate = in(reg) &coordinate,
                component = in(reg) component,
            }
        }
        result
    }

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

    /// Sample texels at `coord` from the image using `sampler`, after adding the input bias to the
    /// implicit level of detail.
    #[crate::macros::gpu_only]
    pub fn sample_bias<F, V>(
        &self,
        sampler: Sampler,
        coord: impl ImageCoordinate<F, DIM, ARRAYED>,
        bias: f32,
    ) -> V
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
                "%result = OpImageSampleImplicitLod typeof*{0} %sampledImage %coord Bias {4}",
                "OpStore {0} %result",
                in(reg) &mut result,
                in(reg) self,
                in(reg) &sampler,
                in(reg) &coord,
                in(reg) bias,
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
        gradient_dx: impl ImageCoordinate<F, DIM, { Arrayed::False as u32 }>,
        gradient_dy: impl ImageCoordinate<F, DIM, { Arrayed::False as u32 }>,
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
        gradient_dx: impl ImageCoordinate<F, DIM, { Arrayed::False as u32 }>,
        gradient_dy: impl ImageCoordinate<F, DIM, { Arrayed::False as u32 }>,
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
        const DIM: u32,
        const DEPTH: u32,
        const SAMPLED: u32,
        const FORMAT: u32,
    >
    Image<
        SampledType,
        DIM,
        DEPTH,
        { Arrayed::False as u32 },
        { Multisampled::False as u32 },
        SAMPLED,
        FORMAT,
    >
{
    /// Sample the image with a project coordinate
    #[crate::macros::gpu_only]
    #[doc(alias = "OpImageSampleProjImplicitLod")]
    pub fn sample_with_project_coordinate<F, V>(
        &self,
        sampler: Sampler,
        project_coordinate: impl ImageCoordinate<F, DIM, { Arrayed::True as u32 }>,
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
        project_coordinate: impl ImageCoordinate<F, DIM, { Arrayed::True as u32 }>,
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
        project_coordinate: impl ImageCoordinate<F, DIM, { Arrayed::True as u32 }>,
        gradient_dx: impl ImageCoordinate<F, DIM, { Arrayed::False as u32 }>,
        gradient_dy: impl ImageCoordinate<F, DIM, { Arrayed::False as u32 }>,
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
        project_coordinate: impl ImageCoordinate<F, DIM, { Arrayed::True as u32 }>,
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
        coordinate: impl ImageCoordinate<F, DIM, { Arrayed::True as u32 }>,
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
        coordinate: impl ImageCoordinate<F, DIM, { Arrayed::True as u32 }>,
        depth_reference: f32,
        gradient_dx: impl ImageCoordinate<F, DIM, { Arrayed::False as u32 }>,
        gradient_dy: impl ImageCoordinate<F, DIM, { Arrayed::False as u32 }>,
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
        const DIM: u32,
        const DEPTH: u32,
        const ARRAYED: u32,
        const MULTISAMPLED: u32,
        const FORMAT: u32,
    > Image<SampledType, DIM, DEPTH, ARRAYED, MULTISAMPLED, { Sampled::No as u32 }, FORMAT>
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
        const DIM: u32,
        const DEPTH: u32,
        const FORMAT: u32,
        const ARRAYED: u32,
        const MULTISAMPLED: u32,
    > Image<SampledType, DIM, DEPTH, ARRAYED, MULTISAMPLED, { Sampled::Unknown as u32 }, FORMAT>
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
        const DEPTH: u32,
        const ARRAYED: u32,
        const MULTISAMPLED: u32,
        const FORMAT: u32,
    >
    Image<
        SampledType,
        { Dimensionality::SubpassData as u32 },
        DEPTH,
        ARRAYED,
        MULTISAMPLED,
        { Sampled::No as u32 },
        FORMAT,
    >
{
    /// Read a texel from subpass input attachment.
    /// Note: Vulkan only allows the read if the first two components of the coordinate are zero.
    #[crate::macros::gpu_only]
    #[doc(alias = "OpImageRead")]
    pub fn read_subpass<I, V, const N: usize>(
        &self,
        coordinate: impl ImageCoordinateSubpassData<I, ARRAYED>,
    ) -> V
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
}

impl<
        SampledType: SampleType<FORMAT>,
        const DIM: u32,
        const DEPTH: u32,
        const ARRAYED: u32,
        const MULTISAMPLED: u32,
        const SAMPLED: u32,
        const FORMAT: u32,
    > Image<SampledType, DIM, DEPTH, ARRAYED, MULTISAMPLED, SAMPLED, FORMAT>
{
    /// Query the number of mipmap levels.
    #[crate::macros::gpu_only]
    #[doc(alias = "OpImageQueryLevels")]
    pub fn query_levels(&self) -> u32
    where
        Self: HasQueryLevels,
    {
        let result: u32;
        unsafe {
            asm! {
                "%image = OpLoad _ {this}",
                "{result} = OpImageQueryLevels typeof{result} %image",
                this = in(reg) self,
                result = out(reg) result,
            }
        }
        result
    }

    /// Query the mipmap level and the level of detail for a hypothetical sampling of Image at
    /// Coordinate using an implicit level of detail. The first component of the result contains
    /// the mipmap array layer. The second component of the result contains the implicit level of
    /// detail relative to the base level.
    #[crate::macros::gpu_only]
    #[doc(alias = "OpImageQueryLod")]
    pub fn query_lod<V: Vector<f32, 2>>(
        &self,
        sampler: Sampler,
        coord: impl ImageCoordinate<f32, DIM, { Arrayed::False as u32 }>,
    ) -> V
    where
        Self: HasQueryLevels,
    {
        // Note: Arrayed::False isn't a typo in the ImageCoordinate, the spec states:
        // Coordinate must be a scalar or vector of floating-point type or integer type. It
        // contains (u[, v] ... ) as needed by the definition of Sampled Image, **not including any
        // array layer index**. Unless the Kernel capability is being used, it must be floating
        // point.
        let mut result = Default::default();
        unsafe {
            asm! {
                "%typeSampledImage = OpTypeSampledImage typeof*{this}",
                "%image = OpLoad _ {this}",
                "%sampler = OpLoad _ {sampler}",
                "%coord = OpLoad _ {coord}",
                "%sampledImage = OpSampledImage %typeSampledImage %image %sampler",
                "%result = OpImageQueryLod typeof*{result} %sampledImage %coord",
                "OpStore {result} %result",
                result = in(reg) &mut result,
                this = in(reg) self,
                sampler = in(reg) &sampler,
                coord = in(reg) &coord
            }
        }
        result
    }

    /// Query the dimensions of Image, with no level of detail.
    #[crate::macros::gpu_only]
    #[doc(alias = "OpImageQuerySize")]
    pub fn query_size<Size: ImageCoordinate<u32, DIM, ARRAYED> + Default>(&self) -> Size
    where
        Self: HasQuerySize,
    {
        let mut result: Size = Default::default();
        unsafe {
            asm! {
                "%image = OpLoad _ {this}",
                "%result = OpImageQuerySize typeof*{result} %image",
                "OpStore {result} %result",
                this = in(reg) self,
                result = in(reg) &mut result,
            }
        }
        result
    }
}

impl<
        SampledType: SampleType<FORMAT>,
        const DIM: u32,
        const DEPTH: u32,
        const ARRAYED: u32,
        const SAMPLED: u32,
        const FORMAT: u32,
    > Image<SampledType, DIM, DEPTH, ARRAYED, { Multisampled::False as u32 }, SAMPLED, FORMAT>
{
    /// Query the dimensions of Image, with no level of detail.
    #[crate::macros::gpu_only]
    #[doc(alias = "OpImageQuerySizeLod")]
    pub fn query_size_lod<Size: ImageCoordinate<u32, DIM, ARRAYED> + Default>(
        &self,
        lod: u32,
    ) -> Size
    where
        Self: HasQuerySizeLod,
    {
        let mut result: Size = Default::default();
        unsafe {
            asm! {
                "%image = OpLoad _ {this}",
                "%result = OpImageQuerySizeLod typeof*{result} %image {lod}",
                "OpStore {result} %result",
                this = in(reg) self,
                lod = in(reg) lod,
                result = in(reg) &mut result,
            }
        }
        result
    }
}

impl<
        SampledType: SampleType<FORMAT>,
        const DEPTH: u32,
        const ARRAYED: u32,
        const SAMPLED: u32,
        const FORMAT: u32,
    >
    Image<
        SampledType,
        { Dimensionality::TwoD as u32 },
        DEPTH,
        ARRAYED,
        { Multisampled::True as u32 },
        SAMPLED,
        FORMAT,
    >
{
    /// Query the number of samples available per texel fetch in a multisample image.
    #[crate::macros::gpu_only]
    #[doc(alias = "OpImageQuerySamples")]
    pub fn query_samples(&self) -> u32 {
        let result: u32;
        unsafe {
            asm! {
                "%image = OpLoad _ {this}",
                "{result} = OpImageQuerySamples typeof{result} %image",
                this = in(reg) self,
                result = out(reg) result,
            }
        }
        result
    }
}

/// An image combined with a sampler in a single value, enabling filtered accesses of the image's
/// contents. Corresponds to `OpTypeSampledImage`.
///
/// The generic type parameter is the underlying image type, written like
/// `SampledImage<Image!(...)>`.
#[spirv(sampled_image)]
#[derive(Copy, Clone)]
pub struct SampledImage<I> {
    _image: I,
}

impl<
        SampledType: SampleType<FORMAT>,
        const DIM: u32,
        const DEPTH: u32,
        const ARRAYED: u32,
        const SAMPLED: u32,
        const FORMAT: u32,
    >
    SampledImage<
        Image<SampledType, DIM, DEPTH, ARRAYED, { Multisampled::False as u32 }, SAMPLED, FORMAT>,
    >
{
    /// Sample texels at `coord` from the sampled image with an implicit lod.
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

    /// Sample texels at `coord` from the sampled image with an explicit lod.
    ///
    /// # Safety
    /// Sampling with a type (`S`) that doesn't match the image's image format
    /// will result in undefined behaviour.
    #[crate::macros::gpu_only]
    pub unsafe fn sample_by_lod<F, V>(
        &self,
        coord: impl ImageCoordinate<F, DIM, ARRAYED>,
        lod: f32,
    ) -> V
    where
        F: Float,
        V: Vector<SampledType, 4>,
    {
        let mut result = Default::default();
        asm!(
            "%sampledImage = OpLoad typeof*{1} {1}",
            "%coord = OpLoad typeof*{2} {2}",
            "%lod = OpLoad typeof*{3} {3}",
            "%result = OpImageSampleExplicitLod typeof*{0} %sampledImage %coord Lod %lod",
            "OpStore {0} %result",
            in(reg) &mut result,
            in(reg) self,
            in(reg) &coord,
            in(reg) &lod,
        );
        result
    }
}

/// This is a marker trait to represent the constraints on `OpImageGather` too complex to be
/// represented by const generics. Specifically:
///
/// "Its `OpTypeImage` must have a Dim of 2D, Cube, or Rect. The MS operand of the underlying
/// `OpTypeImage` must be 0."
pub trait HasGather {}
impl<
        SampledType: SampleType<FORMAT>,
        const DEPTH: u32,
        const FORMAT: u32,
        const ARRAYED: u32,
        const SAMPLED: u32,
    > HasGather
    for Image<
        SampledType,
        { Dimensionality::TwoD as u32 },
        DEPTH,
        ARRAYED,
        { Multisampled::False as u32 },
        SAMPLED,
        FORMAT,
    >
{
}
impl<
        SampledType: SampleType<FORMAT>,
        const DEPTH: u32,
        const FORMAT: u32,
        const ARRAYED: u32,
        const SAMPLED: u32,
    > HasGather
    for Image<
        SampledType,
        { Dimensionality::Rect as u32 },
        DEPTH,
        ARRAYED,
        { Multisampled::False as u32 },
        SAMPLED,
        FORMAT,
    >
{
}
impl<
        SampledType: SampleType<FORMAT>,
        const DEPTH: u32,
        const FORMAT: u32,
        const ARRAYED: u32,
        const SAMPLED: u32,
    > HasGather
    for Image<
        SampledType,
        { Dimensionality::Cube as u32 },
        DEPTH,
        ARRAYED,
        { Multisampled::False as u32 },
        SAMPLED,
        FORMAT,
    >
{
}

/// This is a marker trait to represent the constraints on `OpImageQueryLevels` and
/// `OpImageQueryLod` too complex to be represented by const generics. Specifically:
///
/// "Its Dim operand must be one of 1D, 2D, 3D, or Cube."
pub trait HasQueryLevels {}
impl<
        SampledType: SampleType<FORMAT>,
        const DEPTH: u32,
        const FORMAT: u32,
        const ARRAYED: u32,
        const MULTISAMPLED: u32,
        const SAMPLED: u32,
    > HasQueryLevels
    for Image<
        SampledType,
        { Dimensionality::OneD as u32 },
        DEPTH,
        ARRAYED,
        MULTISAMPLED,
        SAMPLED,
        FORMAT,
    >
{
}
impl<
        SampledType: SampleType<FORMAT>,
        const DEPTH: u32,
        const FORMAT: u32,
        const ARRAYED: u32,
        const MULTISAMPLED: u32,
        const SAMPLED: u32,
    > HasQueryLevels
    for Image<
        SampledType,
        { Dimensionality::TwoD as u32 },
        DEPTH,
        ARRAYED,
        MULTISAMPLED,
        SAMPLED,
        FORMAT,
    >
{
}
impl<
        SampledType: SampleType<FORMAT>,
        const DEPTH: u32,
        const FORMAT: u32,
        const ARRAYED: u32,
        const MULTISAMPLED: u32,
        const SAMPLED: u32,
    > HasQueryLevels
    for Image<
        SampledType,
        { Dimensionality::ThreeD as u32 },
        DEPTH,
        ARRAYED,
        MULTISAMPLED,
        SAMPLED,
        FORMAT,
    >
{
}
impl<
        SampledType: SampleType<FORMAT>,
        const DEPTH: u32,
        const FORMAT: u32,
        const ARRAYED: u32,
        const MULTISAMPLED: u32,
        const SAMPLED: u32,
    > HasQueryLevels
    for Image<
        SampledType,
        { Dimensionality::Cube as u32 },
        DEPTH,
        ARRAYED,
        MULTISAMPLED,
        SAMPLED,
        FORMAT,
    >
{
}

/// This is a marker trait to represent the constraints on `OpImageQuerySize` too complex to be
/// represented by const generics. Specifically:
///
/// "Its Dim operand must be 1D, 2D, 3D, Cube, Rect, or Buffer. Additionally, if its Dim is 1D, 2D,
/// 3D, or Cube, it must also have either an MS of 1 or a Sampled of 0 or 2."
pub trait HasQuerySize {}
impl<
        SampledType: SampleType<FORMAT>,
        const DEPTH: u32,
        const FORMAT: u32,
        const ARRAYED: u32,
        const SAMPLED: u32,
    > HasQuerySize
    for Image<
        SampledType,
        { Dimensionality::OneD as u32 },
        DEPTH,
        ARRAYED,
        { Multisampled::True as u32 },
        SAMPLED,
        FORMAT,
    >
{
}
impl<SampledType: SampleType<FORMAT>, const DEPTH: u32, const FORMAT: u32, const ARRAYED: u32>
    HasQuerySize
    for Image<
        SampledType,
        { Dimensionality::OneD as u32 },
        DEPTH,
        ARRAYED,
        { Multisampled::False as u32 },
        { Sampled::Unknown as u32 },
        FORMAT,
    >
{
}
impl<SampledType: SampleType<FORMAT>, const DEPTH: u32, const FORMAT: u32, const ARRAYED: u32>
    HasQuerySize
    for Image<
        SampledType,
        { Dimensionality::OneD as u32 },
        DEPTH,
        ARRAYED,
        { Multisampled::False as u32 },
        { Sampled::No as u32 },
        FORMAT,
    >
{
}
impl<
        SampledType: SampleType<FORMAT>,
        const DEPTH: u32,
        const FORMAT: u32,
        const ARRAYED: u32,
        const SAMPLED: u32,
    > HasQuerySize
    for Image<
        SampledType,
        { Dimensionality::TwoD as u32 },
        DEPTH,
        ARRAYED,
        { Multisampled::True as u32 },
        SAMPLED,
        FORMAT,
    >
{
}
impl<SampledType: SampleType<FORMAT>, const DEPTH: u32, const FORMAT: u32, const ARRAYED: u32>
    HasQuerySize
    for Image<
        SampledType,
        { Dimensionality::TwoD as u32 },
        DEPTH,
        ARRAYED,
        { Multisampled::False as u32 },
        { Sampled::Unknown as u32 },
        FORMAT,
    >
{
}
impl<SampledType: SampleType<FORMAT>, const DEPTH: u32, const FORMAT: u32, const ARRAYED: u32>
    HasQuerySize
    for Image<
        SampledType,
        { Dimensionality::TwoD as u32 },
        DEPTH,
        ARRAYED,
        { Multisampled::False as u32 },
        { Sampled::No as u32 },
        FORMAT,
    >
{
}
impl<
        SampledType: SampleType<FORMAT>,
        const DEPTH: u32,
        const FORMAT: u32,
        const ARRAYED: u32,
        const SAMPLED: u32,
    > HasQuerySize
    for Image<
        SampledType,
        { Dimensionality::ThreeD as u32 },
        DEPTH,
        ARRAYED,
        { Multisampled::True as u32 },
        SAMPLED,
        FORMAT,
    >
{
}
impl<SampledType: SampleType<FORMAT>, const DEPTH: u32, const FORMAT: u32, const ARRAYED: u32>
    HasQuerySize
    for Image<
        SampledType,
        { Dimensionality::ThreeD as u32 },
        DEPTH,
        ARRAYED,
        { Multisampled::False as u32 },
        { Sampled::Unknown as u32 },
        FORMAT,
    >
{
}
impl<SampledType: SampleType<FORMAT>, const DEPTH: u32, const FORMAT: u32, const ARRAYED: u32>
    HasQuerySize
    for Image<
        SampledType,
        { Dimensionality::ThreeD as u32 },
        DEPTH,
        ARRAYED,
        { Multisampled::False as u32 },
        { Sampled::No as u32 },
        FORMAT,
    >
{
}
impl<
        SampledType: SampleType<FORMAT>,
        const DEPTH: u32,
        const FORMAT: u32,
        const ARRAYED: u32,
        const SAMPLED: u32,
    > HasQuerySize
    for Image<
        SampledType,
        { Dimensionality::Cube as u32 },
        DEPTH,
        ARRAYED,
        { Multisampled::True as u32 },
        SAMPLED,
        FORMAT,
    >
{
}
impl<SampledType: SampleType<FORMAT>, const DEPTH: u32, const FORMAT: u32, const ARRAYED: u32>
    HasQuerySize
    for Image<
        SampledType,
        { Dimensionality::Cube as u32 },
        DEPTH,
        ARRAYED,
        { Multisampled::False as u32 },
        { Sampled::Unknown as u32 },
        FORMAT,
    >
{
}
impl<SampledType: SampleType<FORMAT>, const DEPTH: u32, const FORMAT: u32, const ARRAYED: u32>
    HasQuerySize
    for Image<
        SampledType,
        { Dimensionality::Cube as u32 },
        DEPTH,
        ARRAYED,
        { Multisampled::False as u32 },
        { Sampled::No as u32 },
        FORMAT,
    >
{
}
impl<
        SampledType: SampleType<FORMAT>,
        const DEPTH: u32,
        const FORMAT: u32,
        const ARRAYED: u32,
        const MULTISAMPLED: u32,
        const SAMPLED: u32,
    > HasQuerySize
    for Image<
        SampledType,
        { Dimensionality::Rect as u32 },
        DEPTH,
        ARRAYED,
        MULTISAMPLED,
        SAMPLED,
        FORMAT,
    >
{
}
impl<
        SampledType: SampleType<FORMAT>,
        const DEPTH: u32,
        const FORMAT: u32,
        const ARRAYED: u32,
        const MULTISAMPLED: u32,
        const SAMPLED: u32,
    > HasQuerySize
    for Image<
        SampledType,
        { Dimensionality::Buffer as u32 },
        DEPTH,
        ARRAYED,
        MULTISAMPLED,
        SAMPLED,
        FORMAT,
    >
{
}

/// This is a marker trait to represent the constraints on `OpImageQuerySizeLod` too complex to be
/// represented by const generics. Specifically:
///
/// "Its Dim operand must be one of 1D, 2D, 3D, or Cube, and its MS must be 0."
pub trait HasQuerySizeLod {}
impl<
        SampledType: SampleType<FORMAT>,
        const DEPTH: u32,
        const FORMAT: u32,
        const ARRAYED: u32,
        const SAMPLED: u32,
    > HasQuerySizeLod
    for Image<
        SampledType,
        { Dimensionality::OneD as u32 },
        DEPTH,
        ARRAYED,
        { Multisampled::False as u32 },
        SAMPLED,
        FORMAT,
    >
{
}
impl<
        SampledType: SampleType<FORMAT>,
        const DEPTH: u32,
        const FORMAT: u32,
        const ARRAYED: u32,
        const SAMPLED: u32,
    > HasQuerySizeLod
    for Image<
        SampledType,
        { Dimensionality::TwoD as u32 },
        DEPTH,
        ARRAYED,
        { Multisampled::False as u32 },
        SAMPLED,
        FORMAT,
    >
{
}
impl<
        SampledType: SampleType<FORMAT>,
        const DEPTH: u32,
        const FORMAT: u32,
        const ARRAYED: u32,
        const SAMPLED: u32,
    > HasQuerySizeLod
    for Image<
        SampledType,
        { Dimensionality::ThreeD as u32 },
        DEPTH,
        ARRAYED,
        { Multisampled::False as u32 },
        SAMPLED,
        FORMAT,
    >
{
}
impl<
        SampledType: SampleType<FORMAT>,
        const DEPTH: u32,
        const FORMAT: u32,
        const ARRAYED: u32,
        const SAMPLED: u32,
    > HasQuerySizeLod
    for Image<
        SampledType,
        { Dimensionality::Cube as u32 },
        DEPTH,
        ARRAYED,
        { Multisampled::False as u32 },
        SAMPLED,
        FORMAT,
    >
{
}
