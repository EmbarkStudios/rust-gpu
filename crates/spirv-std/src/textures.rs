use crate::{integer::Integer, vector::Vector};

pub use crate::sampler::Sampler;

#[cfg_attr(
    feature = "const-generics",
    deprecated = "Legacy image type. Use `spirv_std::image::Image2d` instead."
)]
#[spirv(image_type(
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

#[allow(deprecated)]
impl Image2d {
    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpImageSampleImplicitLod")]
    pub fn sample<V: Vector<f32, 4>>(
        &self,
        sampler: Sampler,
        coordinate: impl Vector<f32, 2>,
    ) -> V {
        unsafe {
            let mut result = Default::default();
            asm!(
                "%image = OpLoad _ {this}",
                "%sampler = OpLoad _ {sampler}",
                "%coordinate = OpLoad _ {coordinate}",
                "%sampledImage = OpSampledImage _ %image %sampler",
                "%result = OpImageSampleImplicitLod _ %sampledImage %coordinate",
                "OpStore {result} %result",
                result = in(reg) &mut result,
                this = in(reg) self,
                sampler = in(reg) &sampler,
                coordinate = in(reg) &coordinate,
            );
            result
        }
    }

    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpImageSampleExplicitLod")]
    /// Sample the image at a coordinate by a lod
    pub fn sample_by_lod<V: Vector<f32, 4>>(
        &self,
        sampler: Sampler,
        coordinate: impl Vector<f32, 2>,
        lod: f32,
    ) -> V {
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

    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpImageSampleExplicitLod")]
    /// Sample the image based on a gradient formed by (dx, dy). Specifically, ([du/dx, dv/dx], [du/dy, dv/dy])
    pub fn sample_by_gradient<V: Vector<f32, 4>>(
        &self,
        sampler: Sampler,
        coordinate: impl Vector<f32, 2>,
        gradient_dx: impl Vector<f32, 2>,
        gradient_dy: impl Vector<f32, 2>,
    ) -> V {
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

    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpImageSampleProjImplicitLod")]
    pub fn sample_with_project_coordinate<V: Vector<f32, 4>>(
        &self,
        sampler: Sampler,
        project_coordinate: impl Vector<f32, 3>,
    ) -> V {
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

    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpImageSampleProjExplicitLod")]
    /// Sample the image with a project coordinate by a lod
    pub fn sample_with_project_coordinate_by_lod<V: Vector<f32, 4>>(
        &self,
        sampler: Sampler,
        project_coordinate: impl Vector<f32, 3>,
        lod: f32,
    ) -> V {
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

    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpImageSampleProjExplicitLod")]
    /// Sample the image with a project coordinate based on a gradient formed by (dx, dy). Specifically, ([du/dx, dv/dx], [du/dy, dv/dy])
    pub fn sample_with_project_coordinate_by_gradient<V: Vector<f32, 4>>(
        &self,
        sampler: Sampler,
        project_coordinate: impl Vector<f32, 3>,
        gradient_dx: impl Vector<f32, 2>,
        gradient_dy: impl Vector<f32, 2>,
    ) -> V {
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

    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpImageSampleDrefImplicitLod")]
    /// Sample the image's depth reference
    pub fn sample_depth_reference(
        &self,
        sampler: Sampler,
        coordinate: impl Vector<f32, 2>,
        depth_reference: f32,
    ) -> f32 {
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

    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpImageSampleDrefExplicitLod")]
    /// Sample the image's depth reference based on an explicit lod
    pub fn sample_depth_reference_by_lod(
        &self,
        sampler: Sampler,
        coordinate: impl Vector<f32, 2>,
        depth_reference: f32,
        lod: f32,
    ) -> f32 {
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

    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpImageSampleDrefExplicitLod")]
    /// Sample the image's depth reference based on a gradient formed by (dx, dy).
    /// Specifically, ([du/dx, dv/dx], [du/dy, dv/dy])
    pub fn sample_depth_reference_by_gradient(
        &self,
        sampler: Sampler,
        coordinate: impl Vector<f32, 2>,
        depth_reference: f32,
        gradient_dx: impl Vector<f32, 2>,
        gradient_dy: impl Vector<f32, 2>,
    ) -> f32 {
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

    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpImageSampleProjDrefImplicitLod")]
    /// Sample the image's depth reference with the project coordinate
    pub fn sample_depth_reference_with_project_coordinate(
        &self,
        sampler: Sampler,
        project_coordinate: impl Vector<f32, 3>,
        depth_reference: f32,
    ) -> f32 {
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

    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpImageSampleProjDrefExplicitLod")]
    /// Sample the image's depth reference with the project coordinate based on an explicit lod
    pub fn sample_depth_reference_with_project_coordinate_by_lod(
        &self,
        sampler: Sampler,
        coordinate: impl Vector<f32, 3>,
        depth_reference: f32,
        lod: f32,
    ) -> f32 {
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

    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpImageSampleProjDrefExplicitLod")]
    /// Sample the image's depth reference with the project coordinate based on a gradient formed by (dx, dy).
    /// Specifically, ([du/dx, dv/dx], [du/dy, dv/dy])
    pub fn sample_depth_reference_with_project_coordinate_by_gradient(
        &self,
        sampler: Sampler,
        coordinate: impl Vector<f32, 3>,
        depth_reference: f32,
        gradient_dx: impl Vector<f32, 2>,
        gradient_dy: impl Vector<f32, 2>,
    ) -> f32 {
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

    /// Fetch a single texel with a sampler set at compile time
    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpImageFetch")]
    pub fn fetch<V, I, const N: usize>(&self, coordinate: impl Vector<I, N>) -> V
    where
        V: Vector<f32, 4>,
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

#[cfg_attr(
    feature = "const-generics",
    deprecated = "Legacy image type. Use `spirv_std::image::StorageImage2d` instead."
)]
#[spirv(image_type(
    // sampled_type is hardcoded to f32 for now
    dim = "Dim2D",
    depth = 0,
    arrayed = 0,
    multisampled = 0,
    sampled = 2,
    image_format = "Unknown"
))]
#[derive(Copy, Clone)]
pub struct StorageImage2d {
    _x: u32,
}

#[allow(deprecated)]
impl StorageImage2d {
    /// Read a texel from an image without a sampler.
    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpImageRead")]
    pub fn read<I, V, const N: usize>(&self, coordinate: impl Vector<I, 2>) -> V
    where
        I: Integer,
        V: Vector<f32, N>,
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
    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpImageWrite")]
    pub unsafe fn write<I, const N: usize>(
        &self,
        coordinate: impl Vector<I, 2>,
        texels: impl Vector<f32, N>,
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

#[cfg_attr(
    feature = "const-generics",
    deprecated = "Legacy image type. Use `spirv_std::image::Image2dArray` instead."
)]
#[spirv(image_type(
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

#[allow(deprecated)]
impl Image2dArray {
    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpImageSampleImplicitLod")]
    pub fn sample<V: Vector<f32, 4>>(
        &self,
        sampler: Sampler,
        coordinate: impl Vector<f32, 3>,
    ) -> V {
        unsafe {
            let mut result = V::default();
            asm!(
                "%image = OpLoad _ {this}",
                "%sampler = OpLoad _ {sampler}",
                "%coordinate = OpLoad _ {coordinate}",
                "%sampledImage = OpSampledImage _ %image %sampler",
                "%result = OpImageSampleImplicitLod _ %sampledImage %coordinate",
                "OpStore {result} %result",
                result = in(reg) &mut result,
                this = in(reg) self,
                sampler = in(reg) &sampler,
                coordinate = in(reg) &coordinate,
            );
            result
        }
    }

    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpImageSampleExplicitLod")]
    /// Sample the image at a coordinate by a lod
    pub fn sample_by_lod<V: Vector<f32, 4>>(
        &self,
        sampler: Sampler,
        coordinate: impl Vector<f32, 3>,
        lod: f32,
    ) -> V {
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

    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpImageSampleExplicitLod")]
    /// Sample the image based on a gradient formed by (dx, dy). Specifically, ([du/dx, dv/dx], [du/dy, dv/dy])
    pub fn sample_by_gradient<V: Vector<f32, 4>>(
        &self,
        sampler: Sampler,
        coordinate: impl Vector<f32, 3>,
        gradient_dx: impl Vector<f32, 2>,
        gradient_dy: impl Vector<f32, 2>,
    ) -> V {
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

    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpImageSampleDrefImplicitLod")]
    /// Sample the image's depth reference
    pub fn sample_depth_reference(
        &self,
        sampler: Sampler,
        coordinate: impl Vector<f32, 3>,
        depth_reference: f32,
    ) -> f32 {
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

    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpImageSampleDrefExplicitLod")]
    /// Sample the image's depth reference based on an explicit lod
    pub fn sample_depth_reference_by_lod(
        &self,
        sampler: Sampler,
        coordinate: impl Vector<f32, 3>,
        depth_reference: f32,
        lod: f32,
    ) -> f32 {
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

    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpImageSampleDrefExplicitLod")]
    /// Sample the image's depth reference based on a gradient formed by (dx, dy).
    /// Specifically, ([du/dx, dv/dx], [du/dy, dv/dy])
    pub fn sample_depth_reference_by_gradient(
        &self,
        sampler: Sampler,
        coordinate: impl Vector<f32, 3>,
        depth_reference: f32,
        gradient_dx: impl Vector<f32, 2>,
        gradient_dy: impl Vector<f32, 2>,
    ) -> f32 {
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

#[cfg_attr(
    feature = "const-generics",
    deprecated = "Legacy image type. Use `spirv_std::image::Cubemap` instead."
)]
#[spirv(image_type(
    // sampled_type is hardcoded to f32 for now
    dim = "DimCube",
    depth = 0,
    arrayed = 0,
    multisampled = 0,
    sampled = 1,
    image_format = "Unknown"
))]
#[derive(Copy, Clone)]
pub struct Cubemap {
    _x: u32,
}

#[allow(deprecated)]
impl Cubemap {
    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpSampledImage")]
    pub fn sample<V: Vector<f32, 4>>(
        &self,
        sampler: Sampler,
        coordinate: impl Vector<f32, 3>,
    ) -> V {
        unsafe {
            let mut result = Default::default();
            asm!(
                "%image = OpLoad _ {this}",
                "%sampler = OpLoad _ {sampler}",
                "%coordinate = OpLoad _ {coordinate}",
                "%sampledImage = OpSampledImage _ %image %sampler",
                "%result = OpImageSampleImplicitLod _ %sampledImage %coordinate",
                "OpStore {result} %result",
                result = in(reg) &mut result,
                this = in(reg) self,
                sampler = in(reg) &sampler,
                coordinate = in(reg) &coordinate,
            );
            result
        }
    }

    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpImageSampleExplicitLod")]
    /// Sample the image at a coordinate by a lod
    pub fn sample_by_lod<V: Vector<f32, 4>>(
        &self,
        sampler: Sampler,
        coordinate: impl Vector<f32, 3>,
        lod: f32,
    ) -> V {
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

    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpImageSampleExplicitLod")]
    /// Sample the image based on a gradient formed by (dx, dy). Specifically, ([du/dx, dv/dx, dw/dx], [du/dy, dv/dy, dw/dy])
    pub fn sample_by_gradient<V: Vector<f32, 4>>(
        &self,
        sampler: Sampler,
        coordinate: impl Vector<f32, 3>,
        gradient_dx: impl Vector<f32, 3>,
        gradient_dy: impl Vector<f32, 3>,
    ) -> V {
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

    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpImageSampleDrefImplicitLod")]
    /// Sample the image's depth reference
    pub fn sample_depth_reference(
        &self,
        sampler: Sampler,
        coordinate: impl Vector<f32, 3>,
        depth_reference: f32,
    ) -> f32 {
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

    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpImageSampleDrefExplicitLod")]
    /// Sample the image's depth reference based on an explicit lod
    pub fn sample_depth_reference_by_lod(
        &self,
        sampler: Sampler,
        coordinate: impl Vector<f32, 3>,
        depth_reference: f32,
        lod: f32,
    ) -> f32 {
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

    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpImageSampleDrefExplicitLod")]
    /// Sample the image's depth reference based on a gradient formed by (dx, dy).
    /// Specifically, ([du/dx, dv/dx, dw/dx], [du/dy, dv/dy, dw/dy])
    pub fn sample_depth_reference_by_gradient(
        &self,
        sampler: Sampler,
        coordinate: impl Vector<f32, 3>,
        depth_reference: f32,
        gradient_dx: impl Vector<f32, 3>,
        gradient_dy: impl Vector<f32, 3>,
    ) -> f32 {
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

#[cfg_attr(
    feature = "const-generics",
    deprecated = "Legacy image type. Use `spirv_std::image::SampledImage` instead."
)]
#[spirv(sampled_image)]
#[derive(Copy, Clone)]
pub struct SampledImage<I> {
    _image: I,
}

#[allow(deprecated)]
impl SampledImage<Image2d> {
    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpImageSampleImplicitLod")]
    pub fn sample<V: Vector<f32, 4>>(&self, coordinate: impl Vector<f32, 2>) -> V {
        unsafe {
            let mut result = Default::default();
            asm!(
                "%sampledImage = OpLoad _ {this}",
                "%coordinate = OpLoad _ {coordinate}",
                "%result = OpImageSampleImplicitLod _ %sampledImage %coordinate",
                "OpStore {result} %result",
                result = in(reg) &mut result,
                this = in(reg) self,
                coordinate = in(reg) &coordinate
            );
            result
        }
    }
}
