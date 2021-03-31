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

/// A ray query type which is an opaque object representing a ray traversal.
#[spirv(ray_query)]
pub struct RayQuery {
    _private: u32,
}

/// Constructs an uninitialized ray query variable. Using the syntax
/// `let (mut)? <name>`. Where `name` is the name of the ray query variable.
#[macro_export]
macro_rules! ray_query {
    (let $name:ident) => {
        $crate::ray_query!(@inner $name)
    };
    (let mut $name:ident) => {
        $crate::ray_query!(@inner $name, mut)
    };
    (@inner $name:ident $(, $mut:tt)?) => {
        let $name: &$($mut)? RayQuery = unsafe {
            let $name : *mut RayQuery;
            asm! {
                "%ray_query = OpTypeRayQueryKHR",
                "%ray_query_ptr = OpTypePointer Generic %ray_query",
                "{name} = OpVariable %ray_query_ptr Function",
                name = out(reg) $name,
            }

            &$($mut)? *$name
        };
    }
}

impl RayQuery {
    /// Initialize a ray query object, defining parameters of traversal. After this
    /// call, a new ray trace can be performed with [`Self::proceed`]. Any
    /// previous traversal state stored in the object is lost.
    ///
    /// - `ray_query` is a pointer to the ray query to initialize.
    /// - `acceleration_structure` is the descriptor for the acceleration structure
    ///   to trace into.
    /// - `ray_flags` contains one or more of the Ray Flag values.
    /// - `cull_mask` is the mask to test against the instance mask.  Only the 8
    ///   least-significant bits of `cull_mask` are used by this instruction - other
    ///   bits are ignored.
    /// - `ray_origin`, `ray_tmin`, `ray_direction`, and `ray_tmax` control the
    ///   basic parameters of the ray to be traced.
    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpRayQueryInitializeKHR")]
    #[inline]
    #[allow(clippy::too_many_arguments)]
    pub unsafe fn initialize(
        &mut self,
        acceleration_structure: &AccelerationStructure,
        ray_flags: RayFlags,
        cull_mask: u32,
        ray_origin: impl Vector<f32, 3>,
        ray_tmin: f32,
        ray_direction: impl Vector<f32, 3>,
        ray_tmax: f32,
    ) {
        asm! {
            "%origin = OpLoad _ {ray_origin}",
            "%direction = OpLoad _ {ray_direction}",
            "OpRayQueryInitializeKHR \
                {ray_query} \
                {acceleration_structure} \
                {ray_flags} \
                {cull_mask} \
                %origin \
                {ray_tmin} \
                %direction \
                {ray_tmax}",
            ray_query = in(reg) self,
            acceleration_structure = in(reg) acceleration_structure,
            ray_flags = in(reg) ray_flags.bits(),
            cull_mask = in(reg) cull_mask,
            ray_origin = in(reg) &ray_origin,
            ray_tmin = in(reg) ray_tmin,
            ray_direction = in(reg) &ray_direction,
            ray_tmax = in(reg) ray_tmax,
        }
    }

    /// Allow traversal to proceed. Returns `true` if traversal is incomplete,
    /// and `false` when it has completed. A previous call to [`Self::proceed`]
    /// with the same ray query object must not have already returned `false`.
    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpRayQueryProceedKHR")]
    #[inline]
    pub unsafe fn proceed(&self) -> bool {
        let result: u32;

        asm! {
            "%u32 = OpTypeInt 32 0",
            "%bool = OpTypeBool",
            "%u32_0 = OpConstant %u32 0",
            "%u32_1 = OpConstant %u32 1",
            "%result = OpRayQueryProceedKHR %bool {ray_query}",
            "{result} = OpSelect %u32 %result %u32_0 %u32_1",
            ray_query = in(reg) self,
            result = out(reg) result,
        }

        result != 0
    }

    /// Terminates further execution of a ray query; further calls to
    /// [`Self::proceed`] will return `false`.The value returned by any prior
    /// execution of [`Self::proceed`] with the same ray query object must have
    /// been true. Refer to the client API specification for more details.
    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpRayQueryTerminateKHR")]
    #[inline]
    pub unsafe fn terminate(&self) {
        asm!("OpRayQueryTerminateKHR {}", in(reg) self)
    }

    /// Returns the type of the current candidate or committed intersection.
    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpRayQueryGetIntersectionTypeKHR")]
    #[inline]
    pub unsafe fn get_intersection_type<const INTERSECTION: u32>(&self) -> u32 {
        let mut result = 0;

        asm! {
            "%u32 = OpTypeInt 32 0",
            "%intersection = OpConstant %u32 {intersection}",
            "%result = OpRayQueryGetIntersectionTypeKHR %u32 {ray_query} %intersection",
            "OpStore {result} %result",
            ray_query = in(reg) self,
            intersection = const INTERSECTION,
            result = in(reg) &mut result,
        }

        result
    }

    /// Returns the "Ray Tmin" value used by the ray query.
    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpRayQueryGetRayTMinKHR")]
    #[inline]
    pub unsafe fn get_ray_t_min(&self) -> f32 {
        let mut result = 0.0;

        asm! {
            "%f32 = OpTypeFloat 32",
            "%result = OpRayQueryGetRayTMinKHR %f32 {ray_query}",
            "OpStore {result} %result",
            ray_query = in(reg) self,
            result = in(reg) &mut result,
        }

        result
    }

    /// Returns the "Ray Flags" value used by the ray query.
    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpRayQueryGetRayFlagsKHR")]
    #[inline]
    pub unsafe fn get_ray_flags(&self) -> RayFlags {
        let mut result = 0;

        asm! {
            "%u32 = OpTypeInt 32 0",
            "%result = OpRayQueryGetRayFlagsKHR %u32 {ray_query}",
            "OpStore {result} %result",
            ray_query = in(reg) self,
            result = in(reg) &mut result,
        }

        RayFlags::from_bits_truncate(result)
    }

    /// Gets the "T" value for the current or previous intersection considered
    /// in a ray query.
    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpRayQueryGetIntersectionTKHR")]
    #[inline]
    pub unsafe fn get_intersection_t<const INTERSECTION: u32>(&self) -> f32 {
        let mut result = 0.0;

        asm! {
            "%f32 = OpTypeFloat 32",
            "%u32 = OpTypeInt 32 0",
            "%intersection = OpConstant %u32 {intersection}",
            "%result = OpRayQueryGetIntersectionTKHR %f32 {ray_query} %intersection",
            "OpStore {result} %result",
            ray_query = in(reg) self,
            intersection = const INTERSECTION,
            result = in(reg) &mut result,
        }

        result
    }

    /// Gets the custom index of the instance for the current intersection
    /// considered in a ray query.
    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpRayQueryGetIntersectionInstanceCustomIndexKHR")]
    #[inline]
    pub unsafe fn get_intersection_instance_custom_index<const INTERSECTION: u32>(&self) -> u32 {
        let mut result = 0;

        asm! {
            "%u32 = OpTypeInt 32 0",
            "%intersection = OpConstant %u32 {intersection}",
            "%result = OpRayQueryGetIntersectionInstanceCustomIndexKHR %u32 {ray_query} %intersection",
            "OpStore {result} %result",
            ray_query = in(reg) self,
            intersection = const INTERSECTION,
            result = in(reg) &mut result,
        }

        result
    }

    /// Gets the id of the instance for the current intersection considered in a
    /// ray query.
    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpRayQueryGetIntersectionInstanceIdKHR")]
    #[inline]
    pub unsafe fn get_intersection_instance_id<const INTERSECTION: u32>(&self) -> u32 {
        let mut result;

        asm! {
            "%u32 = OpTypeInt 32 0",
            "%intersection = OpConstant %u32 {intersection}",
            "{result} = OpRayQueryGetIntersectionInstanceIdKHR %u32 {ray_query} %intersection",
            ray_query = in(reg) self,
            intersection = const INTERSECTION,
            result = out(reg) result,
        }

        result
    }

    /// Gets the shader binding table record offset for the current intersection
    /// considered in a ray query.
    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpRayQueryGetIntersectionInstanceShaderBindingTableRecordOffsetKHR")]
    #[inline]
    pub unsafe fn get_intersection_shader_binding_table_record_offset<const INTERSECTION: u32>(
        &self,
    ) -> u32 {
        let mut result = 0;

        asm! {
            "%u32 = OpTypeInt 32 0",
            "%intersection = OpConstant %u32 {intersection}",
            "%result = OpRayQueryGetIntersectionInstanceShaderBindingTableRecordOffsetKHR %u32 {ray_query} %intersection",
            "OpStore {result} %result",
            ray_query = in(reg) self,
            intersection = const INTERSECTION,
            result = in(reg) &mut result,
        }

        result
    }

    /// Gets the geometry index for the current intersection considered in a
    /// ray query.
    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpRayQueryGetIntersectionGeometryIndexKHR")]
    #[inline]
    pub unsafe fn get_intersection_geometry_index<const INTERSECTION: u32>(&self) -> u32 {
        let mut result = 0;

        asm! {
            "%u32 = OpTypeInt 32 0",
            "%intersection = OpConstant %u32 {intersection}",
            "%result = OpRayQueryGetIntersectionGeometryIndexKHR %u32 {ray_query} %intersection",
            "OpStore {result} %result",
            ray_query = in(reg) self,
            intersection = const INTERSECTION,
            result = in(reg) &mut result,
        }

        result
    }

    /// Gets the primitive index for the current intersection considered in a
    /// ray query.
    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpRayQueryGetIntersectionPrimitiveIndexKHR")]
    #[inline]
    pub unsafe fn get_intersection_primitive_index<const INTERSECTION: u32>(&self) -> u32 {
        let mut result = 0;

        asm! {
            "%u32 = OpTypeInt 32 0",
            "%intersection = OpConstant %u32 {intersection}",
            "%result = OpRayQueryGetIntersectionPrimitiveIndexKHR %u32 {ray_query} %intersection",
            "OpStore {result} %result",
            ray_query = in(reg) self,
            intersection = const INTERSECTION,
            result = in(reg) &mut result,
        }

        result
    }

    /// Gets the second and third barycentric coordinates of the current
    /// intersection considered in a ray query against the primitive it hit.
    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpRayQueryGetIntersectionBarycentricsKHR")]
    #[inline]
    pub unsafe fn get_intersection_barycentrics<V: Vector<f32, 2>, const INTERSECTION: u32>(
        &self,
    ) -> V {
        let mut result = Default::default();

        asm! {
            "%u32 = OpTypeInt 32 0",
            "%f32 = OpTypeFloat 32",
            "%f32x2 = OpTypeVector %f32 2",
            "%intersection = OpConstant %u32 {intersection}",
            "%result = OpRayQueryGetIntersectionBarycentricsKHR %f32x2 {ray_query} %intersection",
            "OpStore {result} %result",
            ray_query = in(reg) self,
            intersection = const INTERSECTION,
            result = in(reg) &mut result,
        }

        result
    }

    /// Returns whether the current intersection considered in a ray query was with
    /// the front face (`true`) or back face (`false`) of a primitive.
    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpRayQueryGetIntersectionFrontFaceKHR")]
    #[inline]
    pub unsafe fn get_intersection_front_face<const INTERSECTION: u32>(&self) -> bool {
        let mut result = 0u8;

        asm! {
            "%u8 = OpTypeInt 8 0",
            "%u32 = OpTypeInt 32 0",
            "%intersection = OpConstant %u32 {intersection}",
            "%result = OpRayQueryGetIntersectionFrontFaceKHR %u8 {ray_query} %intersection",
            "OpStore {result} %result",
            ray_query = in(reg) self,
            intersection = const INTERSECTION,
            result = in(reg) &mut result,
        }

        result != 0
    }

    /// Returns whether a candidate intersection considered in a ray query was with
    /// an opaque AABB (Axis Aligned Bounding Box) or not.
    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpRayQueryGetIntersectionCandidateAABBOpaqueKHR")]
    #[inline]
    pub unsafe fn get_intersection_candidate_aabb_opaque(&self) -> bool {
        let mut result = 0u8;

        asm! {
            "%u8 = OpTypeInt 8 0",
            "%result = OpRayQueryGetIntersectionCandidateAABBOpaqueKHR %u8 {ray_query}",
            "OpStore {result} %result",
            ray_query = in(reg) self,
            result = in(reg) &mut result,
        }

        result != 0
    }

    /// Gets the object-space ray direction for the current intersection considered
    /// in a ray query.
    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpRayQueryGetIntersectionObjectRayDirectionKHR")]
    #[inline]
    pub unsafe fn get_intersection_object_ray_direction<
        V: Vector<f32, 3>,
        const INTERSECTION: u32,
    >(
        &self,
    ) -> V {
        let mut result = Default::default();

        asm! {
            "%u32 = OpTypeInt 32 0",
            "%f32 = OpTypeFloat 32",
            "%f32x3 = OpTypeVector %f32 3",
            "%intersection = OpConstant %u32 {intersection}",
            "%result = OpRayQueryGetIntersectionObjectRayDirectionKHR %f32x3 {ray_query} %intersection",
            "OpStore {result} %result",
            ray_query = in(reg) self,
            intersection = const INTERSECTION,
            result = in(reg) &mut result,
        }

        result
    }

    /// Gets the object-space ray origin for the current intersection considered in
    /// a ray query.
    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpRayQueryGetIntersectionObjectRayOriginKHR")]
    #[inline]
    pub unsafe fn get_intersection_object_ray_origin<V: Vector<f32, 3>, const INTERSECTION: u32>(
        &self,
    ) -> V {
        let mut result = Default::default();

        asm! {
            "%u32 = OpTypeInt 32 0",
            "%f32 = OpTypeFloat 32",
            "%f32x3 = OpTypeVector %f32 3",
            "%intersection = OpConstant %u32 {intersection}",
            "%result = OpRayQueryGetIntersectionObjectRayOriginKHR %f32x3 {ray_query} %intersection",
            "OpStore {result} %result",
            ray_query = in(reg) self,
            intersection = const INTERSECTION,
            result = in(reg) &mut result,
        }

        result
    }

    /// Gets the world-space direction for the ray traced in a ray query.
    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpRayQueryGetWorldRayDirectionKHR")]
    #[inline]
    pub unsafe fn get_world_ray_direction<V: Vector<f32, 3>, const INTERSECTION: u32>(&self) -> V {
        let mut result = Default::default();

        asm! {
            "%u32 = OpTypeInt 32 0",
            "%f32 = OpTypeFloat 32",
            "%f32x3 = OpTypeVector %f32 3",
            "%intersection = OpConstant %u32 {intersection}",
            "%result = OpRayQueryGetWorldRayDirectionKHR %f32x3 {ray_query} %intersection",
            "OpStore {result} %result",
            ray_query = in(reg) self,
            intersection = const INTERSECTION,
            result = in(reg) &mut result,
        }

        result
    }

    /// Gets the world-space origin for the ray traced in a ray query.
    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpRayQueryGetWorldRayOriginKHR")]
    #[inline]
    pub unsafe fn get_world_ray_origin<V: Vector<f32, 3>, const INTERSECTION: u32>(&self) -> V {
        let mut result = Default::default();

        asm! {
            "%u32 = OpTypeInt 32 0",
            "%f32 = OpTypeFloat 32",
            "%f32x3 = OpTypeVector %f32 3",
            "%intersection = OpConstant %u32 {intersection}",
            "%result = OpRayQueryGetWorldRayOriginKHR %f32x3 {ray_query} %intersection",
            "OpStore {result} %result",
            ray_query = in(reg) self,
            intersection = const INTERSECTION,
            result = in(reg) &mut result,
        }

        result
    }

    /// Gets a matrix that transforms values to world-space from the object-space of
    /// the current intersection considered in a ray query.
    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpRayQueryGetIntersectionObjectToWorldKHR")]
    #[inline]
    pub unsafe fn get_intersection_object_to_world<V: Vector<f32, 3>, const INTERSECTION: u32>(
        &self,
    ) -> V {
        let mut result = Default::default();

        asm! {
            "%u32 = OpTypeInt 32 0",
            "%f32 = OpTypeFloat 32",
            "%f32x3 = OpTypeVector %f32 3",
            "%intersection = OpConstant %u32 {intersection}",
            "%result = OpRayQueryGetWorldRayOriginKHR %f32x3 {ray_query} %intersection",
            "OpStore {result} %result",
            ray_query = in(reg) self,
            intersection = const INTERSECTION,
            result = in(reg) &mut result,
        }

        result
    }
}
