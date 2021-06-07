use crate::{ray_tracing::{RayFlags, RayQuery}, vector::Vector};

/// A handle that points to a rendering related resource (TLAS, Sampler, Buffer, Texture etc)
/// this handle can be uploaded directly to the GPU to refer to our resources in a bindless
/// fashion and can be plainly stored in buffers directly - even without the help of a `DescriptorSet`
/// the handle isn't guaranteed to live as long as the resource it's associated with so it's up to
/// the user to ensure that their data lives long enough. The handle is versioned to prevent
/// use-after-free bugs however.
///
/// This handle is expected to be used engine-side to refer to descriptors within a descriptor set.
/// To be able to use the bindless system in rust-gpu, an engine is expected to have created
/// four `DescriptorSets`, each containing a large table of max 1 << 23 elements for each type.
/// And to sub-allocate descriptors from those tables. It must use `RenderResourceHandle` to
/// refer to slots within this table, and it's then expected that these `RenderResourceHandle`'s
/// are freely copied to the GPU to refer to resources there.
///
/// | Buffer Type      | Set |
/// |------------------|-----|
/// | Buffers          | 0   |
/// | Textures         | 1   |
/// | Sampler          | 2   |
/// | TLAS             | 3   |
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

    #[inline]
    pub unsafe fn access<T>(self) -> T {
        resource_access(self.index())
    }

    /// # Safety
    /// This method can only safely refer to a resource if that resource
    /// is guaranteed to exist by the caller. `RenderResourceHandle` can't
    /// track lifetimes or keep ref-counts between GPU and CPU and thus
    /// requires extra caution from the user.
    #[inline]
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

#[spirv(resource_access)]
#[spirv_std_macros::gpu_only]
pub extern "unadjusted" fn resource_access<T>(index: u32) -> T {
    unimplemented!()
}


#[derive(Copy, Clone)]
#[repr(transparent)]
pub struct Buffer(RenderResourceHandle);

impl Buffer {
    #[spirv_std_macros::gpu_only]
    #[inline]
    pub extern "unadjusted" fn load<T>(self, dword_aligned_byte_offset: u32) -> T {
        // jb-todo: figure out why this assert breaks with complaints about pointers
        // assert!(self.0.tag() == RenderResourceTag::Buffer);
        // assert!(std::mem::sizeof::<T>() % 4 == 0);
        // assert!(dword_aligned_byte_offset % 4 == 0);

        let buffer: &mut crate::RuntimeArray<u32> = resource_access(unsafe { self.0.index() });
        buffer.load(dword_aligned_byte_offset)
    }

    #[spirv_std_macros::gpu_only]
    pub unsafe extern "unadjusted" fn store<T>(self, dword_aligned_byte_offset: u32, value: T) {
        // jb-todo: figure out why this assert breaks with complaints about pointers
        // assert!(self.0.tag() == RenderResourceTag::Buffer);

        let buffer: &mut crate::RuntimeArray<u32> = resource_access(self.0.index());
        buffer.store(dword_aligned_byte_offset, value)
    }
}

#[derive(Copy, Clone)]
#[repr(transparent)]
pub struct SimpleBuffer<T>(RenderResourceHandle, core::marker::PhantomData<T>);

impl<T> SimpleBuffer<T> {
    #[spirv_std_macros::gpu_only]
    #[inline]
    pub extern "unadjusted" fn load(self) -> T {
        let buffer: &mut crate::RuntimeArray<u32> = resource_access(unsafe { self.0.index() });
        buffer.load(0)
    }

    #[spirv_std_macros::gpu_only]
    pub unsafe extern "unadjusted" fn store(self, value: T) {
        let buffer: &mut crate::RuntimeArray<u32> = resource_access(self.0.index());
        buffer.store(0, value)
    }
}

#[derive(Copy, Clone)]
#[repr(transparent)]
pub struct ArrayBuffer<T>(RenderResourceHandle, core::marker::PhantomData<T>);

impl<T> ArrayBuffer<T> {
    #[spirv_std_macros::gpu_only]
    #[inline]
    pub extern "unadjusted" fn load(self, index: u32) -> T {
        let buffer: &mut crate::RuntimeArray<u32> = resource_access(unsafe { self.0.index() });
        buffer.load(index * core::mem::size_of::<T>() as u32)
    }

    #[spirv_std_macros::gpu_only]
    pub unsafe extern "unadjusted" fn store(self, index: u32, value: T) {
        let buffer: &mut crate::RuntimeArray<u32> = resource_access(self.0.index());
        buffer.store(
            index * core::mem::size_of::<T>() as u32,
            value,
        )
    }
}


#[derive(Copy, Clone)]
#[repr(transparent)]
pub struct Texture2d(RenderResourceHandle);

#[derive(Copy, Clone)]
#[repr(transparent)]
pub struct Sampler(RenderResourceHandle);

#[derive(Copy, Clone)]
#[repr(transparent)]
pub struct AccelerationStructure(RenderResourceHandle);
