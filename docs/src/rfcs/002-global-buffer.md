
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

## GlobalBlock

Per the SPIRV-V spec, "buffer blocks" must have a variable with StorageBuffer storage class point to a "block" decorated struct. On certain platforms, the block decoration is ignored, on others the shader will not access the buffer at all. It may be possible to just have one Block struct, but it may be helpful to have one specific to the Global -> StorageBuffer / CrossWorkgroup classes. It is my understanding that Rust requires fields of public structs to be public, even though this was relaxed at one point. This struct is hidden from the docs because it is merely meant to be a placeholder, for the block decoration. The "block" attribute will be added to the compiler. For convenience, this struct will impl Deref and DerefMut. This allows it to be treated as if it is the interior type, since it isn't meant to do anything but be a block decorated wrapper. 

    #[doc(hidden)]
    #[allow(unused_attributes)]
    #[spirv(block)]
    #[repr(transparent)]
    pub struct GlobalBlock<B>(B);
    
    use core::ops::{Deref, DerefMut};
    
    impl<B> Deref for GlobalBlock<B> {
        type Target = B;
        fn deref(&self) -> &B {
            &*self.0
        }
    }
    
    impl<B> DerefMut for GlobalBlock<B> {
        fn deref_mut(&mut self) -> &B {
            &mut *self.0
        }
    }
    
As noted above, SPIR-V requires that storage classes match. In Rust, it's pretty common to borrow an object, especially when calling a method on it. However, this may produce a *Function pointer to it, which may cause problems. Thus, this GlobalBlock is specific to the "Global" storage class, in case hard coding is necessary. 

## AsSlice and AsMutSlice

These traits allow us to be generic over types that can be borrowed as slices. This will be used to acquire a slice from either a slice or an array in GlobalBuffer(Mut):  

    pub(crate) trait AsSlice {
        type Item;
        fn as_slice(&self) -> &[Self::Item];
    }
    
    pub(crate) trait AsMutSlice: AsSlice {
        fn as_mut_slice(&mut self) -> &[Self::Item];
    }
    
    impl<T> AsSlice for [T] {
        type Item = T;
        fn as_slice(&self) -> &[Self::Item] {
            self
        }
    }
    
    impl<T> AsMutSlice for [T] {
        fn as_slice(&mut self) -> &[Self::Item] {
            self
        }
    }
    
    impl<T, const N: usize> AsSlice for [T; N] {
        type Item = T;
        fn as_slice(&self) -> &[Self::Item] {
            self.as_ref()
        }
    }
    
    impl<T, const N: usize> AsSlice for [T; N] {
        fn as_slice(&self) -> &[Self::Item] {
            self.as_mut()
        }
    }

These traits may potentially be private to a "global_buffer" module. Note that the implementation for arrays requires the unstable feature "min_const_generics", but this is set to be stable in 1.50 (currently 1.48). The AsRef trait does not specify T, ie it's a generic parameter to the trait not an associated type, which makes it more cumbersone to use. 


    
    
    

# Drawbacks

List potential issues that one would want to have discussed in the RFC comment section

# Alternatives

A list of potential alternatives, though sometimes they can arise from the comments as well.

# Prior art

Usually this will involve looking at current shading languages out there to see if we can either borrow concepts from them, or if we can improve upon existing concepts.
