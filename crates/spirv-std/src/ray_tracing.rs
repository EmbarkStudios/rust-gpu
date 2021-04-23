//! Ray-tracing data types
use crate::vector::Vector;

/// An acceleration structure type which is an opaque reference to an
/// acceleration structure handle as defined in the client API specification.
#[spirv(acceleration_structure)]
#[derive(Copy, Clone)]
pub struct AccelerationStructure {
    pub(crate) _private: u32,
}

impl AccelerationStructure {
    /// Converts a 64-bit integer into an [`AccelerationStructure`].
    /// # Safety
    /// The 64-bit integer must point to a valid acceleration structure.
    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpConvertUToAccelerationStructureKHR")]
    #[allow(clippy::empty_loop)]
    #[inline]
    pub unsafe fn from_u64(id: u64) -> AccelerationStructure {
        // Since we can't represent an uninitalized opaque type in Rust at the
        // moment, we need to create and return the acceleration structure entirely
        // in assembly.
        asm! {
            "%ret = OpTypeAccelerationStructureKHR",
            "%result = OpConvertUToAccelerationStructureKHR %ret {id}",
            "OpReturnValue %result",
            "%blah = OpLabel",
            id = in(reg) id,
        }
        loop {}
    }

    /// Converts a vector of two 32 bit integers into an [`AccelerationStructure`].
    /// # Safety
    /// The combination must point to a valid acceleration structure.
    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpConvertUToAccelerationStructureKHR")]
    #[allow(clippy::empty_loop)]
    #[inline]
    pub unsafe fn from_vec(id: impl Vector<u32, 2>) -> AccelerationStructure {
        // Since we can't represent an uninitalized opaque type in Rust at the
        // moment, we need to create and return the acceleration structure entirely
        // in assembly.
        asm! {
            "%ret = OpTypeAccelerationStructureKHR",
            "%id = OpLoad _ {id}",
            "%result = OpConvertUToAccelerationStructureKHR %ret %id",
            "OpReturnValue %result",
            "%blah = OpLabel",
            id = in(reg) &id,
        }
        loop {}
    }

    #[spirv_std_macros::gpu_only]
    /// Trace a ray into the acceleration structure.
    ///
    /// - `structure` is the descriptor for the acceleration structure to trace into.
    /// - `ray_flags` contains one or more of the Ray Flag values.
    /// - `cull_mask` is the mask to test against the instance mask. Only the 8
    ///   least-significant bits of are used by this instruction - other bits
    ///   are ignored.
    /// - `sbt_offset` and `sbt_stride` control indexing into the SBT (Shader
    ///   Binding Table) for hit shaders called from this trace. Only the 4
    ///   least-significant bits of `sbt_offset` and `sbt_stride` are used by this
    ///   instruction - other bits are ignored.
    /// - `miss_index` is the index of the miss shader to be called from this
    ///   trace call. Only the 16 least-significant bits are used by this
    ///   instruction - other bits are ignored.
    /// - `ray_origin`, `ray_tmin`, `ray_direction`, and `ray_tmax` control the
    ///   basic parameters of the ray to be traced.
    ///
    /// - `payload` is a pointer to the ray payload structure to use for this trace.
    ///   `payload` must have a storage class of `ray_payload`
    ///   or `incoming_ray_payload`.
    ///
    /// This instruction is allowed only in `ray_generation`, `closest_hit` and
    /// `miss` execution models.
    ///
    /// This instruction is a shader call instruction which may invoke shaders with
    /// the `intersection`, `any_hit`, `closest_hit`, and `miss`
    /// execution models.
    #[doc(alias = "OpTraceRayKHR")]
    #[inline]
    #[allow(clippy::too_many_arguments)]
    pub unsafe fn trace_ray<T>(
        &self,
        ray_flags: RayFlags,
        cull_mask: i32,
        sbt_offset: i32,
        sbt_stride: i32,
        miss_index: i32,
        ray_origin: impl Vector<f32, 3>,
        ray_tmin: f32,
        ray_direction: impl Vector<f32, 3>,
        ray_tmax: f32,
        payload: &mut T,
    ) {
        asm! {
            "%ray_origin = OpLoad _ {ray_origin}",
            "%ray_direction = OpLoad _ {ray_direction}",
            "OpTraceRayKHR \
            {acceleration_structure} \
            {ray_flags} \
            {cull_mask} \
            {sbt_offset} \
            {sbt_stride} \
            {miss_index} \
            %ray_origin \
            {ray_tmin} \
            %ray_direction \
            {ray_tmax} \
            {payload}",
            acceleration_structure = in(reg) self,
            ray_flags = in(reg) ray_flags.bits(),
            cull_mask = in(reg) cull_mask,
            sbt_offset = in(reg) sbt_offset,
            sbt_stride = in(reg) sbt_stride,
            miss_index = in(reg) miss_index,
            ray_origin = in(reg) &ray_origin,
            ray_tmin = in(reg) ray_tmin,
            ray_direction = in(reg) &ray_direction,
            ray_tmax = in(reg) ray_tmax,
            payload = in(reg) payload,
        }
    }
}

bitflags::bitflags! {
    /// Flags controlling the properties of an OpTraceRayKHR instruction.
    /// Despite being a mask and allowing multiple bits to be combined, it is
    /// invalid for more than one of these four bits to be set: `OPAQUE`,
    /// `NO_OPAQUE`, `CULL_OPAQUE`, `CULL_NO_OPAQUE`, only one of
    /// `CULL_BACK_FACING_TRIANGLES` and `CULL_FRONT_FACING_TRIANGLES` may
    /// be set.
    pub struct RayFlags: u32 {
        /// No flags specified.
        const NONE = 0;
        /// Force all intersections with the trace to be opaque.
        const OPAQUE = 1;
        /// Force all intersections with the trace to be non-opaque.
        const NO_OPAQUE = 2;
        /// Accept the first hit discovered.
        const TERMINATE_ON_FIRST_HIT = 4;
        /// Do not execute a closest hit shader.
        const SKIP_CLOSEST_HIT_SHADER = 8;
        /// Do not intersect with the back face of triangles.
        const CULL_BACK_FACING_TRIANGLES = 16;
        /// Do not intersect with the front face of triangles.
        const CULL_FRONT_FACING_TRIANGLES = 32;
        /// Do not intersect with opaque geometry.
        const CULL_OPAQUE = 64;
        /// Do not intersect with non-opaque geometry.
        const CULL_NO_OPAQUE = 128;
        /// Do not intersect with any triangle geometries.
        const SKIP_TRIANGLES = 256;
        /// Do not intersect with any AABB (Axis Aligned Bounding Box) geometries.
        const SKIP_AABBS = 512;
    }
}
