
# Summary

State the problem that this RFC is trying to address clearly but briefly

This proposal aims to define a core interface for storage buffers aka "buffer blocks", specifically for compute shaders but potentially applicable for other modes. Accessing mutable global memory from a compute shader is typically unsafe, and may require the use of barriers to ensure correctness. This proposal exposes only a minimal public interface, and a private interface that is intended to be built upon with higher level abstractions.   

# Explanation

Give examples, elaborate on the proposal

## Global Storage

For the purposes of simplicity and consistency, the term "global" will be used both for gl_GlobalInvocationID (and friends) and storage buffers using the StorageBuffer storage class, as well as potentially OpenCL global memory using the CrossWorkgroup storage class. 

Currently, the StorageBuffer struct in spirv-std is used for both mutable and immutable buffers. It has load and store methods, for T: Copy. Instead, Global and GlobalMut structs will be included. This will require an additional attribute, "global", which may convert to either StorageBuffer or CrossWorkgroup (for kernel mode).

    // spirv-std/src/lib.rs  

    #[allow(unused_attributes)]
    #[spirv(global)]
    pub struct Global<'a, T> {
        x: &'a T
    }

Here a "new" method is provide for convenience and privacy:

    impl<'a, T> Global<'a, T> {
        pub(crate) fn new(x: &'a T) -> Self {
            Self { x }
        } 
    }

Global implments the same "load" function common to spirv-std, for Copy types:

    impl<T: Copy> Global<'a, T> {
      pub fn load(&self) -> T {
          *self.x
      }
    }

In addition, Global also acts like a slice, when it wraps a slice. Note that we can't use Index / AsRef / Deref here because those traits return a &T, not a T. Note that "get_unchecked" is unsafe, and potentially accesses out of bounds data. 

    impl<'a, T> Global<'a, [T]> {
      pub fn get<I: SliceIndex<[T]>>(&self, index: I) -> Global<'a, <I as SliceIndex<[T]>>::Output> {
          self.x.get(index)
              .map(|x| Global::new(x))
      } 
      pub unsafe fn get_unchecked<I: SliceIndex<[T]>(&self, index: I) -> Self::Output {
          Global::new(self.x.get_unchecked(index))
      }
    }
  
Similar to Global, GlobalMut will also be introduced:

      #[allow(unused_attributes)]
      #[spirv(global)]
      pub struct GlobalMut<'a, T> {
          x: &'a mut T
      }

      impl<'a, T> GlobalMut<'a, T> {
          pub(crate) fn new(x: &'a mut T) -> Self {
              Self { x }
          }
      }

      impl<T: Copy> GlobalMut<'a, T> {
          pub fn load(&self) -> T {
              *self.x
          }
          pub fn store(&mut self, x: T) {
              *self.x = T;
          }
      }

      impl<'a, T> GlobalMut<'a, [T]> {
          pub fn get<I: SliceIndex<[T]>>(&self, index: I) -> Option<Global<'a, <I as SliceIndex<[T]>>::Output>> {
              self.x.get(index)
                  .map(|x| Global::new(x))
          } 
          pub unsafe fn get_unchecked<I: SliceIndex<[T]>(&self, index: I) -> Global<'a, <I as SliceIndex<[T]>>::Output {
              Global::new(self.x.get_unchecked(index))
          }
          pub fn get_mut<I: SliceIndexMut<[T]>>(&mut self, index: I) -> Option<GlobalMut<'a, <I as SliceIndexMut<[T]>>::Output>> {
              self.x.get(index)
                  .map(|x| GlobalMut::new(x))
          } 
          pub unsafe fn get_unchecked_mut<I: SliceIndex<[T]>(&mut self, index: I) -> GlobalMut<'a, <I as SliceIndexMut<[T]>>::Output> {
              GlobalMut::new(self.x.get_unchecked(index))
          }
      }

The intent with Global and GlobalMut, is that they are returned from higher level abstractions. SPIR-V has a requirement that with several pointer operations, including Load, Store, and AccessChain, the storage class of the pointer matches the storage class of the variable. That means if we have a buffer, ie a *{StorageBuffer} RuntimeArray, we can't return a plain reference or plain slice, ie *{Function} T, to the user to perform the derefence. There are potentially alternatives, but the simplest approach is to just maintain the wrapped Global / GlobalMut struct until the store or load. This is the current design of spirv-std. 

Global and GlobalMut are essentially either &(mut)T or &(mut)\[T\]. They are only to be provided to user code when they are not aliased. Safe abstractions should ensure that this is the case, and then yield these objects. 

Due to the nature of GlobalMut, it is critical that the compiler prevents GlobalMut from being passed in as a parameter, since it marks a safe to access reference or slice. Global, since it is immutable, is not problematic, but might as well be banned as well. Further, the spirv attribute, at least for "storage_buffer" and "cross_workgroup", etc, should be limited only to within the crate. 


# Drawbacks

List potential issues that one would want to have discussed in the RFC comment section

# Alternatives

A list of potential alternatives, though sometimes they can arise from the comments as well.

# Prior art

Usually this will involve looking at current shading languages out there to see if we can either borrow concepts from them, or if we can improve upon existing concepts.
