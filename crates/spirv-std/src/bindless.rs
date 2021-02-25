use crate::glam::{Vec2, Vec4};

/// A handle that points to a rendering related resource (TLAS, Sampler, Buffer, Texture etc)
/// this handle can be uploaded directly to the GPU to refer to our resources in a bindless
/// fashion and can be plainly stored in buffers directly - even without the help of a `DescriptorSet`
/// the handle isn't guaranteed to live as long as the resource it's associated with so it's up to
/// the user to ensure that their data lives long enough. The handle is versioned to prevent
/// use-after-free bugs however.
///
/// This handle is expected to be used engine-side to refer to descriptors within a descriptor set.
/// To be able to use the bindless system in rust-gpu, an engine is expected to have created
/// four DescriptorSets, each containing a large table of max 1 << 23 elements for each type.
/// And to sub-allocate descriptors from those tables. It must use `RenderResourceHandle` to
/// refer to slots within this table, and it's then expected that these RenderResourceHandle's
/// are freely copied to the GPU to refer to resources there.
///
/// | Buffer Type      | Set |
/// |------------------|-----|
/// | Buffers          | 0   |
/// | Textures         | 1   |
/// | Storage textures | 2   |
/// | Tlas             | 3   |
#[derive(Copy, Clone, Eq, PartialEq, Hash)]
#[repr(transparent)]
pub struct RenderResourceHandle(u32);

#[repr(u8)]
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum RenderResourceTag {
    Sampler,
    Tlas,
    Buffer,
    Texture,
}

impl core::fmt::Debug for RenderResourceHandle {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("RenderResourceHandle")
            .field("version", &self.version())
            .field("tag", &self.tag())
            .field("index", unsafe { &self.index() })
            .finish()
    }
}

impl RenderResourceHandle {
    pub fn new(version: u8, tag: RenderResourceTag, index: u32) -> Self {
        let version = version as u32;
        let tag = tag as u32;
        let index = index as u32;

        assert!(version < 64); // version wraps around, it's just to make sure invalid resources don't get another version
        assert!(tag < 8);
        assert!(index < (1 << 23));

        Self(version << 26 | tag << 23 | index)
    }

    pub fn invalid() -> Self {
        Self(!0)
    }

    pub fn is_valid(self) -> bool {
        self.0 != !0
    }

    pub fn version(self) -> u32 {
        self.0 >> 26
    }

    pub fn tag(self) -> RenderResourceTag {
        match (self.0 >> 23) & 7 {
            0 => RenderResourceTag::Sampler,
            1 => RenderResourceTag::Tlas,
            2 => RenderResourceTag::Buffer,
            3 => RenderResourceTag::Texture,
            invalid_tag => panic!(
                "RenderResourceHandle corrupt: invalid tag ({})",
                invalid_tag
            ),
        }
    }

    /// Safety:
    /// This method can only safely refer to a resource if that resource
    /// is guaranteed to exist by the caller. `RenderResourceHandle` can't
    /// track lifetimes or keep ref-counts between GPU and CPU and thus
    /// requires extra caution from the user.
    pub unsafe fn index(self) -> u32 {
        self.0 & ((1 << 23) - 1)
    }

    /// This function is primarily intended for use in a slot allocator, where the slot
    /// needs to get re-used and it's data updated. This bumps the `version` of the
    /// `RenderResourceHandle` and updates the `tag`.
    pub fn bump_version_and_update_tag(self, tag: RenderResourceTag) -> Self {
        let mut version = self.0 >> 26;
        version = ((version + 1) % 64) << 26;
        let tag = (tag as u32) << 23;
        Self(version | tag | (self.0 & ((1 << 23) - 1)))
    }
}

#[derive(Copy, Clone)]
#[repr(transparent)]
pub struct Buffer(RenderResourceHandle);

#[allow(unused_attributes)]
#[spirv(internal_buffer_load)]
#[spirv_std_macros::gpu_only]
fn internal_buffer_load<T>(buffer: u32, offset: u32) -> T {
    unimplemented!()
} // actually implemented in the compiler

impl Buffer {
    #[spirv_std_macros::gpu_only]
    pub fn load<T>(self, dword_aligned_byte_offset: u32) -> T {
        // jb-todo: figure out why this assert breaks with complaints about pointers
        // assert!(self.0.tag() == RenderResourceTag::Buffer);

        unsafe { internal_buffer_load(self.0.index(), dword_aligned_byte_offset) }
    }
}

#[derive(Copy, Clone)]
#[repr(transparent)]
pub struct Texture2d(RenderResourceHandle);

// #[derive(Copy, Clone)]
// #[repr(transparent)]
// struct SamplerState(RenderResourceHandle);

impl Texture2d {
    #[spirv_std_macros::gpu_only]
    pub fn sample(self, coord: Vec2) -> Vec4 {
        // jb-todo: also do a bindless fetch of the sampler
        unsafe {
            let mut result = Vec4::default();
            asm!(
                "OpExtension \"SPV_EXT_descriptor_indexing\"",
                "OpCapability RuntimeDescriptorArray",
                "OpDecorate %image_2d_var DescriptorSet 1",
                "OpDecorate %image_2d_var Binding 0",
                "%uint                  = OpTypeInt 32 0",
                "%float                 = OpTypeFloat 32",
                "%uint_0                = OpConstant %uint 0",
                "%image_2d              = OpTypeImage %float Dim2D 0 0 0 1 Unknown",
                "%sampled_image_2d      = OpTypeSampledImage %image_2d",
                "%image_array           = OpTypeRuntimeArray %sampled_image_2d",
                "%ptr_image_array       = OpTypePointer UniformConstant %image_array",
                "%image_2d_var          = OpVariable %ptr_image_array UniformConstant",
                "%ptr_sampled_image_2d  = OpTypePointer UniformConstant %sampled_image_2d",
                "", // ^^ type preamble
                "%offset                = OpLoad _ {1}",
                "%24                    = OpAccessChain %ptr_sampled_image_2d %image_2d_var %offset",
                "%25                    = OpLoad %sampled_image_2d %24",
                "%coord                 = OpLoad _ {0}",
                "%result                = OpImageSampleImplicitLod _ %25 %coord",
                "OpStore {2} %result",
                in(reg) &coord,
                in(reg) &self.0.index(),
                in(reg) &mut result,
            );
            result
        }
    }

    #[spirv_std_macros::gpu_only]
    pub fn sample_proj_lod(
        self,
        coord: Vec4,
        ddx: Vec2,
        ddy: Vec2,
        offset_x: i32,
        offset_y: i32,
    ) -> Vec4 {
        // jb-todo: also do a bindless fetch of the sampler
        unsafe {
            let mut result = Vec4::default();
            asm!(
                "OpExtension \"SPV_EXT_descriptor_indexing\"",
                "OpCapability RuntimeDescriptorArray",
                "OpDecorate %image_2d_var DescriptorSet 1",
                "OpDecorate %image_2d_var Binding 0",
                "%uint                  = OpTypeInt 32 0",
                "%int                   = OpTypeInt 32 1",
                "%float                 = OpTypeFloat 32",
                "%v2int                 = OpTypeVector %int 2",
                "%uint_0                = OpConstant %uint 0",
                "%int_0                 = OpConstant %int 0",
                "%image_2d              = OpTypeImage %float Dim2D 0 0 0 1 Unknown",
                "%sampled_image_2d      = OpTypeSampledImage %image_2d",
                "%image_array           = OpTypeRuntimeArray %sampled_image_2d",
                "%ptr_image_array       = OpTypePointer UniformConstant %image_array",
                "%image_2d_var          = OpVariable %ptr_image_array UniformConstant",
                "%ptr_sampled_image_2d  = OpTypePointer UniformConstant %sampled_image_2d",
                "", // ^^ type preamble
                "%offset                = OpLoad _ {1}",
                "%24                    = OpAccessChain %ptr_sampled_image_2d %image_2d_var %offset",
                "%25                    = OpLoad %sampled_image_2d %24",
                "%coord                 = OpLoad _ {0}",
                "%ddx                   = OpLoad _ {3}",
                "%ddy                   = OpLoad _ {4}",
                "%offset_x              = OpLoad _ {5}",
                "%offset_y              = OpLoad _ {6}",
                "%const_offset          = OpConstantComposite %v2int %int_0 %int_0",
                "%result                = OpImageSampleProjExplicitLod _ %25 %coord Grad|ConstOffset %ddx %ddy %const_offset",
                "OpStore {2} %result",
                in(reg) &coord,
                in(reg) &self.0.index(),
                in(reg) &mut result,
                in(reg) &ddx,
                in(reg) &ddy,
                in(reg) &offset_x,
                in(reg) &offset_y,
            );
            result
        }
    }
}
