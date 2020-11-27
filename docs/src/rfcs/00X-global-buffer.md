
# Summary

This proposal aims to define a core interface for storage buffers aka "buffer blocks", specifically for compute shaders but potentially applicable for other modes. Accessing mutable global memory from a compute shader is typically unsafe, and may require the use of barriers to ensure correctness. This proposal exposes only a minimal public interface, and a private interface that is intended to be built upon with higher level abstractions.   

# Explanation

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
      pub(crate) unsafe fn get_unchecked<I: SliceIndex<[T]>(&self, index: I) -> Self::Output {
          Global::new(self.x.get_unchecked(index))
      }
    }
    
The "get_unchecked" method could be public, as it emulates core::slice::get_unchecked. However, this would of course expose an unsafe public interface. Instead, potentially a future addition would be an Iter (like slice::Iter), which allows for sequential access without repeated bounds checks. The intent is to allow for something like a "Chunks" iterator, which could yield a Global<\[T]>, which can then be used in some user defined function per invocation.  
  
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
          pub(crate) unsafe fn get_unchecked<I: SliceIndex<[T]>(&self, index: I) -> Global<'a, <I as SliceIndex<[T]>>::Output {
              Global::new(self.x.get_unchecked(index))
          }
          pub fn get_mut<I: SliceIndexMut<[T]>>(&mut self, index: I) -> Option<GlobalMut<'a, <I as SliceIndexMut<[T]>>::Output>> {
              self.x.get(index)
                  .map(|x| GlobalMut::new(x))
          } 
          pub(crate) unsafe fn get_unchecked_mut<I: SliceIndex<[T]>(&mut self, index: I) -> GlobalMut<'a, <I as SliceIndexMut<[T]>>::Output> {
              GlobalMut::new(self.x.get_unchecked(index))
          }
      }

The intent with Global and GlobalMut, is that they are returned from higher level abstractions. SPIR-V has a requirement that with several pointer operations, including Load, Store, and AccessChain, the storage class of the pointer matches the storage class of the variable. That means if we have a buffer, ie a *{StorageBuffer} RuntimeArray, we can't return a plain reference or plain slice, ie *{Function} T, to the user to perform the derefence. There are potentially alternatives, but the simplest approach is to just maintain the wrapped Global / GlobalMut struct until the store or load. This is the current design of spirv-std. 

Global and GlobalMut are essentially either &(mut)T or &(mut)\[T\]. They are only to be provided to user code when they are not aliased. Safe abstractions should ensure that this is the case, and then yield these objects. 

Due to the nature of GlobalMut, it is critical that the compiler prevents GlobalMut from being passed in as a parameter, since it marks a safe to access reference or slice. Global, since it is immutable, is not problematic, but might as well be banned as well, as it isn't meant to be used this way, and will not be portable. Further, the spirv attribute, at least for "storage_buffer" and "cross_workgroup", etc, should be limited only to within the crate. 

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

## GlobalBuffer

The GlobalBuffer and GlobalBufferMut structs will be used as entry parameters. They have a reference to a GlobalBlock\<B>, where B may be a slice aka RuntimeArray, an array, or some arbitrary type (like an iterface block of glam Mat or Vec types). For Copy types, GlobalBuffer implements "load", returning a copy of the data. For arrays and slices, it implements "as_slice", which returns a Global<\[T]>. This is safe because it is immutable:

    #[allow(unused_attributes)]
    #[spirv(global)] 
    pub struct GlobalBuffer<'a, B> {
        x: &'a GlobalBlock<B>
    }
    
    impl<'a, B: Copy> GlobalBuffer<'a, B> {
        pub fn load(&self) -> B {
            *self.x
        }
    }

    impl<'a, T, B: AsSlice<Item=T>> GlobalBuffer<'a, B> {
        pub fn as_slice(&self) -> Global<'a, [T]> {
            Global::new(self.x.as_slice())
        }
    }

MutableBuffers are more restricted. There will be no public interface, instead "as_unsafe_slice" and "as_unsafe_mut_slice", private to spirv-std, which are unsafe because multiple invocations could both read and write to it. Potentially, a safe high-level abstraction like an iterator will partition this slice between invocations and perform any necessary barriers. 

    #[allow(unused_attributes)]
    #[spirv(global)] 
    pub struct GlobalBufferMut<'a, B> {
        x: &'a mut GlobalBlock<B>
    }

    impl<'a, T, B: AsSlice<Item=T>> GlobalBufferMut<'a, B> {
        pub(crate) unsafe fn as_unsafe_slice(&self) -> Global<'a, [T]> {
            Global::new(self.x.as_slice())
        }
    }

    impl<'a, T, B: AsMutSlice<Item=T>> GlobalBufferMut<'a, B> {
        pub(crate) unsafe fn as_unsafe_mut_slice(&mut self) -> GlobalMut<'a, [T]> {
            GlobalMut::new(self.x.as_mut_slice())
        } 
    }

## GlobalIndex 

In order to do work in parallel, the shader must acquire the global index, ie gl_GlobalInvocationIndex. Use of this value presents several problems. First, the programmer must take care to have each invocation access different memory, or use appropriate synchronization. The index may be out of bounds, larger than the length of the buffer. Using the value conditionally may lead to non-uniform control flow, impeding optimizations or leading to undefined behavior. Lastly, both non-uniform control flow and invalid memory use may invalidate other safe abstractions. In order to provide some protection, a GlobalIndex struct will wrap the value, which allows it to be returned without allowing the value to be read.
    
    #[derive(Clone, Copy)]
    pub(crate) struct GlobalIndex(u32);   

    pub(crate) fn global_index() -> GlobalIndex;
    
Not sure how to implement "global_index". Currently, an Input<u32x3> with the "GlobalInvocationID" builtin decoration, where u32x3 is a vector of u32's, will then be stored with the global id. In combination with global size, acquired the same way, the global index can be computed. However, this would have to be built in to the compiler, to emit the appropriate input variables and expose them to spirv-std. Or maybe via the asm! macro. At some point it may be useful to have a "global_xyz" function, but I'm uncertain how to handle these. Is everything just a GlobalIndex? Do we have a GlobalDim, a GlobaX, etc? I'm not sure that much can be done from a security standpoint beyond just wrapping the type, and for now just getting the global index is enough to build on. 
    
The "get" and "get_unchecked" methods on Global and (mut equivalents for GlobalMut) are overloaded via the GetExt and GetMutExt traits for GlobalIndex, ensuring that the value is protected. The get / get_mut methods aare safe because the Global(Mut) must have been acquired via a safe method, or the "unsafe_as_slice" and "unsafe_as_mut_slice" methods, for which the caller has already taken responsibility for safety. Futher, these methods are private to spirv-std.  

    pub(crate) trait GetExt<I> {
        type Output;
        fn get(&self, index: I) -> Option<Self::Output>;
        unsafe fn get_unchecked(&self, index: I) -> Self::Output;
    }

    pub(crate) trait GetMutExt<I> {
        type Output;
        fn get_mut(&mut self, index: I) -> Option<Self::Output>;
        unsafe fn get_unchecked_mut(&mut self, index: I) -> Self::Output;
    }


    impl<'a, T> GetExt<GlobalIndex> for Global<'a, [T]> {
        type Output = Global<'a, T>;
        fn get(&self, index: GlobalIndex) -> Option<Self::Output> {
            self.get(index.0)
        }
        unsafe fn get_unchecked(&self, index: GlobalIndex) -> Self::Output [
            self.get_unchecked(index.0)
        }
    }

    impl<'a, T> GetExt<GlobalIndex> for GlobalMut<'a, [T]> {
        type Output = Global<'a, T>;
        fn get(&self, index: GlobalIndex) -> Option<Self::Output> {
            self.get(index.0)
        }
        unsafe fn get_unchecked(&self, index: GlobalIndex) -> Self::Output [
            self.get_unchecked(index.0)
        }
    }

    impl<'a, T> GetMutExt<GlobalIndex> for GlobalMut<'a, [T]> {
        type Output = GlobalMut<'a, T>;
        fn get_mut(&mut self, index: GlobalIndex) -> Option<Self::Output> {
            self.get_mut(index.0)
        }
        unsafe fn get_unchecked_mut(&mut self, index: GlobalIndex) -> Self::Output [
            self.get_unchecked_mut(index.0)
        }
    }
    
## Example 

As an example, suppose that a hypothetical "zip_mut_with" (see ndarray [zip_mut_with](https://docs.rs/ndarray/0.13.1/ndarray/struct.ArrayBase.html#method.zip_mut_with)) function was added to spirv-std. 

    // spirv-std/src/lib.rs
    
    impl<'a, T, const N: usize> GlobalBufferMut<[T; N]> {
        pub fn zip_mut_with<C: Copy>(mut self, rhs: &GlobalBuffer<[T; N]>, constants: C, f: impl fn(GloablMut<T>, Global<T>, C)) {
            let index = global_index();
            barrier(); // barrier for any previous writes to self
            unsafe {
                self.as_unsafe_mut_slice().get_mut(index).zip(rhs.as_slice().get(index))
                    .map(|(lhs, rhs)| f(lhs, rhs, constants));
            }
            // this method consumes self
            // alternatively take self by reference, and emit a barrier here
        }
    }
  
    // scaled_add.rs
    use spirv_std::{Buffer, BufferMut};
    
    type T = f32;
    const N: usize = 1024;
    
    #[allow(unused_attributes)]
    #[spirv(gl_compute(local_size=64)]
    pub fn scaled_add(
        #[spirv(descriptor_set=1, binding=0)] x: GlobalBuffer<[T; N]>,
        #[spirv(descriptor_set=1, binding=1)] mut y: GlobalBufferMut<[T; N]>,
        push_constants: PushConstant<T>
    ) {
        let alpha = push_constants.load();
        y.zip_mut_with(&x, alpha, |(y, x, alpha)| {
            let result = y.load() + alpha * x.load();
            y.store(result);
        });
    }

To be clear, this proposal neglects to include Barriers or PushConstants. Potentially, having functions like "zip_mut_with" or iterators consume their buffers, combined with the fn closure (which doesn't capture), will be secure enough to make public. The idea is that if y is consumed, it can't be used again in a subsequent operation, and thus no barriers are required. But I'm not sure how that fits into a larger ecosystem, where some functions may borrow buffers, so that subsequent operations can be performed. These problems are left for futher work. 

# Drawbacks

Not really a drawback, but it may be necessary to move some code into submodules with spirv-std, in order to maintain privacy, prevent access outside of explicit functions, and hide utility types, traits, or functions. 

This proposal neglects how GlobalBuffer(Mut) will work in other shaders, where access patterns may be different.

Barriers are not adressed in this proposal, but GlobalIndex and the scheme of restricting access all have to work in concert. Without some more thought into how barriers will work and how that interacts with the rest of the system, it won't be possible to allow access to GlobalBufferMut, which is of course necessary to actually use compute shaders. 

GlobalIndex is meant to be a simple way of performing parallel work, but restricting access to the global id means that at some point combinations of operations, combined with global size, will need to be exposed, though potentially this can be handled with iterators. 

# Alternatives 

Potentially, instead of GlobalBuffer and GlobalBufferMut, we might have 2 or 3 variants, for 4 or 6 structs total. Those being one for arbitrary T: Copy, one for Arrays, and one for RuntimeArrays. This eliminates the need for the awkward "AsSlice" trait, which enables the as_slice / as_unsafe_slice methods, which in turn will potentially enable higher abstractions like iterators. That means that AsSlice potentially needs to be visable, or else traits are simply implemented twice. There are different ways to rework this for better ergonomics and stability. 

# Prior art

Typically in gpu code, which is often some superset of c, "buffers" are essentially just pointers and the user just indexes them freely utilizing global_id and friends. While this "works", and is fairly straigtforward as well as being typical to programming in general, it can potentially lead to various memory use errors. In particular, reading / writing out of bounds. The shader doesn't just have access to the buffers it is provided as parameters, it actually has access to the entire gpu memory space, and invalid writes will be written to other buffers used by other shaders / kernels. This is very hard to troubleshoot, because the code causing the problem may fuction correctly by itself, but "poison" other shaders that happen to be used in some sequence. Rust as a language seeks to prevent and or limit such errors to small, well scrutinized blocks of code. 

As far as I know, there isn't any gpu language that hides global invocation builtins like the global id, and shaders can index into declared buffer inputs without restriction, necessary synchronization is the responsibility of the programmer. Note that in some cases, Metal for example, out of bounds writes are specified as ignored. This prevents "poisoning" as described earlier, but doesn't fully protect against security vulnerabilities, and arguably out of bounds reads or writes are bugs, which should be caught and fixed. Safe Rust promises to prevent out of bounds accesses. 

