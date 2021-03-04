//! # Storage Classes
//!
//! Class of storage for declared variables. These types act as pointers to
//! values either contained in the GPU's memory. For example; `Input<f32>` points to a
//! `f32` that was provided as input from the pipeline, and `Private<f32>`
//! points to a `f32` in the GPU's global memory. Intermediate values do not
//! form a storage class, and unless stated otherwise, storage class-based
//! restrictions are not restrictions on intermediate objects and their types.

use core::{
    marker::PhantomData,
    ops::{Deref, DerefMut, Index, IndexMut},
};

/// Graphics uniform blocks and buffer blocks.
///
/// Shared externally, visible across all functions in all invocations in
/// all work groups. Requires "Shader" capability.
#[allow(unused_attributes)]
#[spirv(uniform)]
pub struct Uniform;
impl Descriptor for Uniform {}
impl DescriptorMut for Uniform {}

/// Graphics storage buffers (buffer blocks).
///
/// Shared externally, readable and writable, visible across all functions
/// in all invocations in all work groups.
#[allow(unused_attributes)]
#[spirv(storage_buffer)]
pub struct StorageBuffer;
impl Descriptor for StorageBuffer {}
impl DescriptorMut for StorageBuffer {}

/// Graphics uniform memory. OpenCL constant memory.
///
/// Shared externally, visible across all functions in all invocations in
/// all work groups. Variables declared with this storage class are
/// read-only. They may have initializers, as allowed by the client API.
#[allow(unused_attributes)]
#[spirv(uniform_constant)]
pub struct UniformConstant;
impl Descriptor for UniformConstant {}

/// Input from pipeline.
///
/// Visible across all functions in the current invocation. Variables
/// declared with this storage class are read-only, and must not
/// have initializers.
#[allow(unused_attributes)]
#[spirv(input)]
pub struct Input<'a, T: ?Sized, Binding: sealed::InputBinding = sealed::CompilerInferred> {
    ptr: &'a T,
    binding: PhantomData<Binding>,
}

impl<'a, T: ?Sized, Binding: sealed::InputBinding> Deref for Input<'a, T, Binding> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.ptr
    }
}

/// Output to pipeline.
///
/// Visible across all functions in the current invocation.
#[allow(unused_attributes)]
#[spirv(output)]
pub struct Output<'a, T: ?Sized, Binding: sealed::OutputBinding = sealed::CompilerInferred> {
    ptr: &'a mut T,
    binding: PhantomData<Binding>,
}

impl<'a, T: ?Sized, Binding: sealed::OutputBinding> Deref for Output<'a, T, Binding> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.ptr
    }
}

impl<'a, T: ?Sized, Binding: sealed::OutputBinding> DerefMut for Output<'a, T, Binding> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.ptr
    }
}

pub struct Location<const LOCATION: usize>;
impl<const LOCATION: usize> Location<LOCATION> {
    pub const LOCATION: usize = LOCATION;
}

mod sealed {
    pub struct CompilerInferred;
    pub trait InputBinding {}
    impl InputBinding for CompilerInferred {}
    impl<const LOCATION: usize> InputBinding for super::Location<LOCATION> {}
    pub trait OutputBinding {}
    impl OutputBinding for CompilerInferred {}
    impl<const LOCATION: usize> OutputBinding for super::Location<LOCATION> {}
}

macro_rules! storage_class {
    ($(#[$($meta:meta)+])* storage_class $name:ident ; $($tt:tt)*) => {
        $(#[$($meta)+])*
        #[allow(unused_attributes)]
        pub struct $name<'value, T: ?Sized> {
            reference: &'value mut T,
        }

        impl<T: ?Sized> Deref for $name<'_, T> {
            type Target = T;
            fn deref(&self) -> &T {
                self.reference
            }
        }

        impl<T: Copy> $name<'_, T> {
            /// Load the value into memory.
            #[deprecated(note = "storage_class::Foo<T> types now implement Deref, and can be used like &T")]
            pub fn load(&self) -> T {
                **self
            }
        }

        storage_class!($($tt)*);
    };

    // Methods available on writeable storage classes.
    ($(#[$($meta:meta)+])* writeable storage_class $name:ident $($tt:tt)+) => {
        storage_class!($(#[$($meta)+])* storage_class $name $($tt)+);

        impl<T: ?Sized> DerefMut for $name<'_, T> {
            fn deref_mut(&mut self) -> &mut T {
                self.reference
            }
        }

        impl<T: Copy> $name<'_, T> {
            /// Store the value in storage.
            #[deprecated(note = "storage_class::Foo<T> types now implement DerefMut, and can be used like &mut T")]
            pub fn store(&mut self, v: T) {
                **self = v
            }

            /// A convenience function to load a value into memory and store it.
            #[deprecated(note = "storage_class::Foo<T> types now implement DerefMut, and can be used like &mut T")]
            pub fn then(&mut self, f: impl FnOnce(T) -> T) {
                **self = f(**self);
            }
        }
    };

    (;) => {};
    () => {};
}

// Make sure the `#[spirv(<string>)]` strings stay synced with symbols.rs
// in `rustc_codegen_spirv`.
storage_class! {
    /// The OpenGL "shared" storage qualifier. OpenCL local memory.
    ///
    /// Shared across all invocations within a work group. Visible across
    /// all functions.
    #[spirv(workgroup)] writeable storage_class Workgroup;

    /// OpenCL global memory.
    ///
    /// Visible across all functions of all invocations of all work groups.
    #[spirv(cross_workgroup)] writeable storage_class CrossWorkgroup;

    /// Regular global memory.
    ///
    /// Visible to all functions in the current invocation. Requires
    /// "Shader" capability.
    #[spirv(private)] writeable storage_class Private;

    /// Regular function memory.
    ///
    /// Visible only within the declaring function of the current invocation.
    #[spirv(function)] writeable storage_class Function;

    /// For generic pointers, which overload the [`Function`], [`Workgroup`],
    /// and [`CrossWorkgroup`] Storage Classes.
    #[spirv(generic)] writeable storage_class Generic;

    /// Push-constant memory.
    ///
    /// Visible across all functions in all invocations in all work groups.
    /// Intended to contain a small bank of values pushed from the client API.
    /// Variables declared with this storage class are read-only, and must not
    /// have initializers.
    #[spirv(push_constant)] storage_class PushConstant;

    /// Atomic counter-specific memory.
    ///
    /// For holding atomic counters. Visible across all functions of the
    /// current invocation.
    #[spirv(atomic_counter)] writeable storage_class AtomicCounter;

    /// Image memory.
    ///
    /// Holds a pointer to a single texel, obtained via OpImageTexelPointer. Use of a pointer
    /// obtained via OpImageTexelPointer is limited to atomic operations.
    ///
    /// If you're looking to create a pointer to an entire image instead of a texel, you probably
    /// want UniformConstant instead.
    #[spirv(image)] writeable storage_class Image;

    /// Used for storing arbitrary data associated with a ray to pass
    /// to callables. (Requires `SPV_KHR_ray_tracing` extension)
    ///
    /// Visible across all functions in the current invocation. Not shared
    /// externally. Variables declared with this storage class can be both read
    /// and written to. Only allowed in `RayGenerationKHR`, `ClosestHitKHR`,
    /// `CallableKHR`, and `MissKHR` execution models.
    #[spirv(callable_data_khr)] writeable storage_class CallableDataKHR;

    /// Used for storing arbitrary data from parent sent to current callable
    /// stage invoked from an `executeCallable` call. (Requires
    /// `SPV_KHR_ray_tracing` extension)
    ///
    /// Visible across all functions in current invocation. Not shared
    /// externally. Variables declared with the storage class are allowed only
    /// in `CallableKHR` execution models. Can be both read and written to in
    /// above execution models.
    #[spirv(incoming_callable_data_khr)] writeable storage_class IncomingCallableDataKHR;

    /// Used for storing payload data associated with a ray. (Requires
    /// `SPV_KHR_ray_tracing` extension)
    ///
    /// Visible across all functions in the current invocation. Not shared
    /// externally. Variables declared with this storage class can be both read
    /// and written to. Only allowed in `RayGenerationKHR`, `AnyHitKHR`,
    /// `ClosestHitKHR` and `MissKHR` execution models.
    #[spirv(ray_payload_khr)] writeable storage_class RayPayloadKHR;


    /// Used for storing attributes of geometry intersected by a ray. (Requires
    /// `SPV_KHR_ray_tracing` extension)
    ///
    /// Visible across all functions in the current invocation. Not shared
    /// externally. Variables declared with this storage class are allowed only
    /// in `IntersectionKHR`, `AnyHitKHR` and `ClosestHitKHR` execution models.
    /// They can be written to only in `IntersectionKHR` execution model and read
    /// from only in `AnyHitKHR` and `ClosestHitKHR` execution models.
    #[spirv(hit_attribute_khr)] writeable storage_class HitAttributeKHR;


    /// Used for storing attributes of geometry intersected by a ray. (Requires
    /// `SPV_KHR_ray_tracing` extension)
    ///
    /// Visible across all functions in the current invocation. Not shared
    /// externally. Variables declared with this storage class are allowed only
    /// in `IntersectionKHR`, `AnyHitKHR` and `ClosestHitKHR` execution models.
    /// They can be written to only in `IntersectionKHR` execution model and
    /// read from only in `AnyHitKHR` and `ClosestHitKHR` execution models. They
    /// cannot have initializers.
    #[spirv(incoming_ray_payload_khr)] writeable storage_class IncomingRayPayloadKHR;

    /// Used for storing data in shader record associated with each unique
    /// shader in ray_tracing pipeline. (Requires
    /// `SPV_KHR_ray_tracing` extension)
    ///
    /// Visible across all functions in current invocation. Can be initialized
    /// externally via API. Variables declared with this storage class are
    /// allowed in RayGenerationKHR, IntersectionKHR, AnyHitKHR, ClosestHitKHR,
    /// MissKHR and CallableKHR execution models, are read-only, and cannot have
    /// initializers. Refer to the client API for details on shader records.
    #[spirv(shader_record_buffer_khr)] writeable storage_class ShaderRecordBufferKHR;

    /// Graphics storage buffers using physical addressing. (SPIR-V 1.5+)
    ///
    /// Shared externally, readable and writable, visible across all functions
    /// in all invocations in all work groups.
    #[spirv(physical_storage_buffer)] writeable storage_class PhysicalStorageBuffer;
}

/// A descriptor set binding.
///
/// The first paramter is the data parameter. It allows DSTs, but they are not supported yet.
/// The second parameter is the storage class or an array or slice of storage class.
/// The last two const parameters are the `Set` then `Binding` numbers.
#[allow(unused_attributes)]
#[spirv(bind)]
pub struct Bind<'a, T: ?Sized, DescriptorType: DescriptorOrDescriptorArray + ?Sized, const SET: usize, const BINDING: usize> {
    ptr: *mut T,
    _phantom: PhantomData<&'a DescriptorType>,
}

impl<'a, T: ?Sized, DescriptorType: Descriptor, const SET: usize, const BINDING: usize>
Deref for Bind<'a, T, DescriptorType, SET, BINDING>
{
    type Target = T;
    fn deref(&self) -> &T {
        unsafe{ &*self.ptr }
    }
}

impl<'a, T: ?Sized, DescriptorType: Descriptor + DescriptorMut, const SET: usize, const BINDING: usize>
DerefMut for Bind<'a, T, DescriptorType, SET, BINDING>
{
    fn deref_mut(&mut self) -> &mut T {
        unsafe { &mut *self.ptr }
    }
}

impl<'a, T: ?Sized, DescriptorType: DescriptorArray + ?Sized, const SET: usize, const BINDING: usize>
Index<usize> for Bind<'a, T, DescriptorType, SET, BINDING>
{
    type Output = T;
    #[allow(unused_attributes)]
    #[spirv(index_descriptor_array)]
    #[allow(unused)]
    fn index(&self, index: usize) -> &T {
        // compiler implemented
        unreachable!()
    }
}

impl<'a, T: ?Sized, DescriptorType: DescriptorArray + DescriptorMut + ?Sized, const SET: usize, const BINDING: usize>
IndexMut<usize> for Bind<'a, T, DescriptorType, SET, BINDING>
{
    #[allow(unused_attributes)]
    #[spirv(index_descriptor_array)]
    #[allow(unused)]
    fn index_mut(&mut self, index: usize) -> &mut T {
        // compiler implemented
        core::unreachable!()
    }
}

pub trait Descriptor {}

pub trait DescriptorMut {}
impl<DescriptorType: DescriptorMut, const N: usize> DescriptorMut for [DescriptorType; N] {}
impl<DescriptorType: DescriptorMut> DescriptorMut for [DescriptorType] {}

pub trait DescriptorOrDescriptorArray {}
impl<DescriptorType: Descriptor> DescriptorOrDescriptorArray for DescriptorType {}
impl<DescriptorType: Descriptor, const N: usize> DescriptorOrDescriptorArray for [DescriptorType; N] {}
impl<DescriptorType: Descriptor> DescriptorOrDescriptorArray for [DescriptorType] {}

pub trait DescriptorArray: DescriptorOrDescriptorArray {}
impl<DescriptorType: Descriptor, const N: usize> DescriptorArray for [DescriptorType; N] {}
impl<DescriptorType: Descriptor> DescriptorArray for [DescriptorType] {}
