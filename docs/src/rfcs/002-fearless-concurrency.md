# Fearless Concurrency

## Summary

GPU programs are executed across thousands or millions of independently executing threads, grouped into a hierarchy of [scopes](https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/chap9.html#shaders-scope).

Unlike threading on a CPU, threads on a GPU cannot easily talk to each other - there are limitations on how a group of threads can be synchronized, and how. Values written by one thread are also not automatically visible to other threads with acquire and release operations like on a CPU. With threads grouped into scopes, applications must be explicit about which scopes threads are synchronized within, and to what scope writes become visible. Both of these requirements must be carefully navigated in order to avoid data races.

In GLSL and HLSL (see [Prior art](#prior-art)), applications are explicit about synchronization and visibility, but lack the safety that Rust typically affords. This proposal introduces functions which are also explicit, but integrate into rust's borrow checking to provide safety against data races. This proposal does not deal with storage images, atomic operations, subgroup operations, or any more advanced functionality.

This proposal currently requires the use of the `VulkanMemoryModel` capability, and describes everything in terms of that - however it could be mapped to SPIR-V without this capability in future if that's desirable.

## Explanation

This is a baseline proposal which is expected to be built on top of over time, to incorporate new ideas and more flexibility - for now only the basic functionality of reading and writing to a buffer or workgroup memory is covered.

### Buffer Type

Buffer data is wrapped with the `Buffer<T>` struct, which contains the pointer to all of the data (type `T`) in the resource. It doesn't allow direct access to this pointer (at least not via safe interfaces), but instead provides a number of safe interfaces to load and store the data. This type (and all the borrowed wrapper types) should also provide a solid link to the storage class being accessed.

#### Borrowing

Borrowing is a staple of rust ownership - but in order to be explicit about the scope at which values are visible, the user has to declare this intent when borrowing. Rather than a simple "borrow" and "borrow_mut" method, `Buffer` instead has the following interfaces:

```rust
fn borrow_for_device(&self) -> DeviceVisibleBufferValue<T>;
fn borrow_mut_for_device(&mut self) -> DeviceVisibleMutableBufferValue<T>;
fn borrow_for_workgroup(&self) -> WorkgroupVisibleBufferValue<T>;
fn borrow_mut_for_workgroup(&mut self) -> WorkgroupVisibleMutableBufferValue<T>;
fn borrow_for_subgroup(&self) -> SubgroupVisibleBufferValue<T>;
fn borrow_mut_for_subgroup(&mut self) -> SubgroupVisibleMutableBufferValue<T>;
```

Note that the borrow functions don't directly return a reference to the underlying value - instead they return intermediate types which mediate correct loads and stores, described in the following section.

For each borrow, an `OpMemoryBarrier` instruction is emitted with a `Scope` equal to that of the name of the borrow command, and `Acquire | UniformMemory` memory semantics.

#### Scope Visible Wrappers

Rust doesn't cater for different "types" of memory or annotation at the moment, and adding that into the compiler would be a significant amount of work. It might be possible in future to use some sort of annotated type, but for now this is going down a similar route to that of the `Atomic*` types in the standard library by wrapping underlying variables with types that have dedicated load/store methods.

Each wrapper type is named with its scope (Device/Workgroup/Subgroup), and whether it is mutable or not, reflecting its underlying properties.

##### Loads and Stores

Loads and stores are dedicated methods on the commands returned from the `Buffer` borrows. All of those types have a `load()` method; only the `Mutable` types include the `store()` method.

```rust
fn load(&self) -> T;
fn store(&mut self, value: T);
```

The two methods mostly map exactly to what you'd expect - `OpLoad` and `OpStore`, however each of them will include additional `Memory Operands`.

For `load()`, an `OpLoad` instruction is generated with the `MakePointerVisible` semantic and a following `Scope` parameter equal to the scope in the name of the wrapper type.

For `store()`, an `OpStore` instruction is generated with the `MakePointerAvailable` semantic, and a following `Scope` parameter equal to the scope in the name of the wrapper type.

##### Drop

The `Mutable` variants of the wrapper types implement a custom `drop()` to ensure correct synchronization.

```rust
fn drop(&mut self);
```

In addition to destruction when it goes out of scope, an `OpMemoryBarrier` instruction is emitted with a `Scope` equal to that of the scope of the wrapper type, and `Release | UniformMemory` memory semantics.

### Workgroup Struct

Workgroup memory can be exposed in _exactly_ the same manner as the `Buffer` type. The exhaustive list of changes are:

 - `Workgroup<T>` instead
 - No `borrow_for_device()` or `borrow_mut_for_device()` methods or associated types
 - Wrapper types with `Workgroup` in the name instead of `Buffer`.
 - `Workgroup` storage class used instead of `StorageBuffer` for accesses/initialization
 - `WorkgroupMemory` memory semantic used by `OpMemoryBarrier` in `borrow*`/`drop` methods instead of `UniformMemory`

### Thread Synchronization

SPIR-V/Vulkan offer workgroup and subgroup barriers, although the semantics of each vary in ways that are unusual, and frankly there be dragons here. Efforts are underway to smooth this out, but for now, it's likely safest if we expose only workgroup barriers.

```
fn synchronize_workgroup();
```

Workgroup synchronization is sufficient to make values visible at smaller scopes too (i.e. subgroup) so not having a subgroup barrier is "ok". Notably there's no `synchronize_device()` function, so writing at device scope and then attempting to read from another thread in the same device is always a data race - an execution dependency in the API is currently required to make these work.

This command issues an OpControlBarrier with `Execution Scope` equal to `Workgroup`, `Memory Scope` equal to `Invocation`, and `Memory Semantics` equal to `0`. This instruction does not include memory semantics, as those are already taken care of by the `borrow-*()` and `drop()` methods on resource types.

This method has the probably-hard-to-enforce requirement that it's only ever called in control flow that is fully uniform across a workgroup. Potentially it could be put anywhere and hoisted to uniform control but that likely gets painful pretty quickly. Trying to detect this at compile time might also be rather difficult - although maybe "fully uniform" would be possible to detect? That would be sufficient for 99% of compute use cases.

### Invocation Indexing Information

In order to effectively index into resources from multiple invocations, SPIR-V provides a number of indices to identify which invocation and scope grouping the current shader invocation is executing. Exposing these to applications will be necessary to enable effective indexing.

These are loose functions - they aren't tied to a particular struct or trait.

```rust
fn SubgroupSize() -> usize              // Returns SubgroupSize                             - size of the subgroup
fn SubgroupIndex() -> usize             // Returns SubgroupId                               - Linear index of the subgroup within the dispatch
fn SubgroupInvocationIndex() -> usize   // Returns SubgroupLocalInvocationId                - Linear index of the invocation within the subgroup
fn WorkGroupSize() -> usize             // Returns WorkgroupSize.x * y * z                  - size of the Workgroup
fn WorkGroupIndex() -> usize            // Returns GlobalInvocationId / WorkGroupSize       - Linear index of the Workgroup within the dispatch
fn WorkGroupInvocationIndex() -> usize  // Returns LocalInvocationIndex                     - Linear index of the invocation within the Workgroup
fn DispatchSize() -> usize              // Returns WorkgroupSize.x * y * z * NumWorkgroups  - Linear size of the Dispatch
fn DispatchInvocationIndex() -> usize   // Returns GlobalInvocationId                       - Linear index of the invocation within the dispatch
```

### Future Additions

#### Arrays

There should probably be `borrow()` variants which return a wrapper for a range of values in the resource, since a common pattern might be to write to a single element of an array from each invocation, then read back the entire array of elements later. This can probably be added once everything else is working. There's a few options to play around with here, and it's probably not worth committing the design of this until the rest is accepted.

#### Storage Images

Storage images are going to be pretty similar, but they are multi-dimensional rather than being indexed in a single dimension, which complicates the implicit indexing in this proposal. Not insurmountable, but definitely requires more work.

#### Complex Borrowing

There are applications using interesting access patterns that are (theoretically) data race free, but cannot be boiled down to simply "accessing nearby data". Things like implementing a suballocation or linked lists on a buffer would be impossible with this interface.


#### Atomics

Atomic loads, stores, and other operations might be relatively simple to add in some way, but will require some consideration as to how to make them safe, as they enable more complex code that's potentially harder to validate. In particular, atomics can be used in some places in lieu of a synchronization barrier, but whether they satisfy the synchronization requirements or not is far harder to prove.

## Drawbacks

This proposal doesn't enable an unsafe interface - it's *somewhat* prescriptive, so it might be hard to build very much on top of this directly.

Whilst not really drawbacks, there are still places that a user could shoot themselves in the foot. I've laid out some examples here and how they could potentially be addressed (where possible):

### Not synchronizing threads properly

If a shader does something like write a value in one thread and then read it back in another in the same scope, naively there's no way to guarantee that the threads have been actually synchronized. E.g.:

```rust
let data = myBuffer.borrow_mut_for_workgroup();
data.store(1);

let data = myBuffer.borrow_for_workgroup();
```

In the above code, there's a data race even though the borrow checker should be happy, as the threads are not synchronized. An expert in this area should be able to find this by localised code inspection, so in theory the compiler should be able to figure out - the question is how?

The most naive way I can think to make this work is at runtime, by tracking an extra value which indicates if a synchronization has been performed or not after a mutable value is dropped for a given resource (making the `synchronize_workgroup` function a resource method might make this easier...); if not, then fire an assert when a value is borrowed.

Hopefully there's something that could be done during compilation to figure this out more elegantly and earlier, but this will require someone more familiar with compilation to figure out.

### Not synchronizing outside of the shader

All of the correctness this proposal brings is irrelevant if some other shader/API command/the host accesses the same memory location and doesn't synchronize it properly. Since that falls outside of the scope of a single shader, it's not covered inside this proposal. The likely solution to this will require something like a task graph proposal.

### Aliasing

Yet another "outside of the shader" problem, if two resources input to the shader alias the same memory, there's nothing in the shader that can be done to guarantee safety. Again this will probably need something like a task graph proposal.

## Alternatives

The main clear alternative would be to simply provide unsafe access to a pointer, with synchronization functions similar to those in GLSL/HLSL, and load store functions indicating things like the storage class and make available/visible semantics. It might be desirable to build that interface first, and then build this on proposal on top of it, since that would let this interface be built, as well as many others in the future. 

The unsafe interface could be as simple as exposing a pointer or handle, with the ability to pass that to spir-v intrinsics exposed to Rust. We could then build all of this infrastructure on top of that, along with potentially more later. If desirable, a lower level interface proposal could be spun off from this as an initial target.

## Prior art

HLSL has a simpler model which only distinguishes device and workgroup visibility, with no real notion of other scopes. These were introduced in [shader model 5](https://docs.microsoft.com/en-us/windows/win32/direct3dhlsl/d3d11-graphics-reference-sm5-intrinsics), and haven't been updated since then. The key functions there are:

* AllMemoryBarrier
* AllMemoryBarrierWithGroupSync
* DeviceMemoryBarrier
* DeviceMemoryBarrierWithGroupSync
* GroupMemoryBarrier
* GroupMemoryBarrierWithGroupSync

Distinctions here are only made between global memory and workgroup memory, and the scopes are tied directly to the type of memory involved. The "WithGroupSync" variants act as fences/control barriers as well as memory barriers at the workgroup scope.

GLSL went down a similar route, though the features were spread across multiple extensions, exposing [compute shaders](https://www.khronos.org/registry/OpenGL/extensions/ARB/ARB_compute_shader.txt), [storage images](https://www.khronos.org/registry/OpenGL/extensions/ARB/ARB_shader_image_load_store.txt) and [storage buffers](https://www.khronos.org/registry/OpenGL/extensions/ARB/ARB_shader_storage_buffer_object.txt). Again, the memory type, memory scope, and execution scope are all tied together in these extensions.

More recently, the Vulkan WG realised that the guarantees on this were fairly loose, and testing was quite difficult. In an attempt to formalise the model somewhat, an extended and [more thoroughly tested formal model](https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-model) was introduced.
As part of releasing that, @tobski wrote up a [comparison between this and the C++ memory model](https://www.khronos.org/blog/comparing-the-vulkan-spir-v-memory-model-to-cs), which may be useful to those more familiar with C++.
The new Vulkan memory model is also exposed in GLSL via the [memory scope semantics](https://github.com/KhronosGroup/GLSL/blob/master/extensions/khr/GL_KHR_memory_scope_semantics.txt) extension.

One of the key draws of this extension was to parameterise everything that an implementation might choose to do with memory; the memory type, memory scope, and execution scope all became independently controllable.
GLSL hasn't really taken advantage of this, as it functions fairly closely to SPIR-V, and is thus only really providing a somewhat friendly spir-v wrapper.

Creating the memory model also enabled us to have in-depth (and soemtimes painful) discussions about how implementations were actually behaving when particular code was written, and what app developers typically wanted to do.
A lot of discussion/feedback here was that most developer use cases fall into a fairly narrow band, and whilst that can be much more tightly expressed with the memory model than the current limited barriers, the full suite of parameters the memory model expressed was too much, particularly without a defined execution model (e.g. you can't write spin locks!).
Additionally, whilst GLSL and SPIR-V were necessarily conservative by having no automatic visibility for resource writes, most use cases expected that these writes would be made visible in some way to other invocations.
All of this informed the decisions in this proposal, which suggests a tighter definition than the full memory model, but that also slightly differs from than traditional shading languages by making visibility there "by default" (albeit with explicit scope opt-in), and being relativel prescriptive.
