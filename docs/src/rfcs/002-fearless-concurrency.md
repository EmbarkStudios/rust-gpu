# Fearless Concurrency (V2.0)


## Summary

GPU programs are executed across thousands or millions of independently executing threads, grouped into a hierarchy of [scopes](https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/chap9.html#shaders-scope).

Unlike threading on a CPU, threads on a GPU cannot easily talk to each other - there are limitations on how a group of threads can be synchronized, and how. Values written by one thread are also not automatically visible to other threads with acquire and release operations like on a CPU. With threads grouped into scopes, applications must be explicit about which scopes threads are synchronized within, and to what scope writes become visible. Both of these requirements must be carefully navigated in order to avoid data races.

In GLSL and HLSL (see [Prior art](#prior-art)), applications are explicit about synchronization and visibility, but lack the safety that Rust typically affords. This proposal introduces functions which are also explicit, but integrate into rust's borrow checking to provide safety against data races. This proposal does not deal with storage images, atomic operations, subgroup operations, or any more advanced functionality.

This proposal currently requires the use of the `VulkanMemoryModel` capability, and describes everything in terms of that - however it could be mapped to SPIR-V without this capability in future if that's desirable.

For now, this proposal only deals with barriers and the Workgroup scope as it affects buffer storage and workgroup storage; other scopes and atomics etc. are left to future proposals.

## Explanation

This is a baseline proposal which is expected to be built on top of over time, to incorporate new ideas and more flexibility.

### Resource Types

Storage buffer data is wrapped with the following type:

```rust
struct MutDeviceStorage<T> {
    data: StorageBuffer<T>,
};
```
Workgroup storage is wrapped with a single type:

```rust
struct MutWorkgroupStorage<T> {
    data: Workgroup<T>,
};
```

#### MutDeviceStorage and MutWorkgroupStorage interfaces

For now these interfaces have two functions - but will likely change to include additional scopes (e.g. subgroup) and operations (e.g. reduction, expansion) in future iterations of the proposal:

```rust
impl<T> MutDeviceStorage {
    fn map_workgroup<F, E>(&mut self, f: F) where F: FnMut(E) -> E
    {
        unsafe {
            spv::op_memory_barrier(Workgroup, Acquire);
            let value: E = self.data.load_indexed_from_workgroup(invocation_index());  // OpLoad with NonPrivate|MakeVisible semantics and Workgroup scope.
            let result = f(value);
            self.data.store_indexed_to_workgroup(invocation_index(), result);          // OpStore with NonPrivate|MakeAvailable semantics and Workgroup scope
            spv::op_memory_barrier(Workgroup, Release);
        }
    }
    
    fn map_workgroup_joined<F, E, const RATE: usize>(&mut self, f: F) where F: FnMut([E; RATE]) -> [E; RATE]
    {
        unsafe {
            spv::op_control_barrier(Workgroup, Workgroup, Acquire);
            if (invocation_index % RATE == 0) {
                let value: [E; RATE] = self.data.load_array_indexed_from_workgroup(invocation_index());  // OpLoad with NonPrivate|MakeVisible semantics and Workgroup scope.
                let result = f(value);
                self.data.store_array_indexed_to_workgroup(invocation_index(), result);                  // OpStore with NonPrivate|MakeAvailable semantics and Workgroup scope
            }
            spv::op_control_barrier(Workgroup, Workgroup, Release);
        }
    }
}
```

`map_workgroup` gives you a simple 1:1 interface for mutating values in a buffer, allowing you to mutate invocation-local data. It does so safely, with barriers and visibility automatically executed to provide safety.

`map_workgroup_joined` is similar, but allows a shader to access data shared by multiple threads, so that it can be shuffled or combined across threads. The number of threads that data is shared between is fixed by the `RATE` parameter. This function must only be called in workgroup-uniform control flow, and `RATE` must be a factor of the workgroup size.

The `MutWorkgroupStorage` implementation is identical; it just operates on Workgroup storage instead.

The load and store variants used on the underlying storage here are invented to simplify the interface; what these ultimately look like is contingent on the resource binding proposal, which is still in flight.
The spir-v op codes are similarly somewhat invented, and will also be adjusted to mirror the final syntax for assembly here (or some friendlier wrapper).
These are provided primarily for illustrative purposes.


### Example

Bitonic sort example (unrolled, incomplete...):

```rust
fn bitonic_sort_workgroup(buffer: MutDeviceStorage<u32>) {
    buffer.map_workgroup_joined<2>(|array| {
        [
            max(array[0],array[1]),
            min(array[0],array[1]),
        ]
    });
    buffer.map_workgroup_joined<4>(|array| {
        [
            max(array[0],array[3]),
            min(array[0],array[3]),
            max(array[1],array[2]),
            min(array[1],array[2]),
        ]
    });
    buffer.map_workgroup_joined<2>(|array| {
        [
            max(array[0],array[1]),
            min(array[0],array[1]),
        ]
    });
    buffer.map_workgroup_joined<8>(|array| {
        [
            max(array[0],array[7]),
            min(array[0],array[7]),
            max(array[1],array[6]),
            min(array[1],array[6]),
            max(array[2],array[5]),
            min(array[2],array[5]),
            max(array[3],array[4]),
            min(array[3],array[4]),
        ]
    })
    buffer.map_workgroup_joined<4>(|array| {
        [
            max(array[0],array[1]),
            min(array[0],array[1]),
            max(array[2],array[3]),
            min(array[2],array[3]),
        ]
    });
    buffer.map_workgroup_joined<2>(|array| {
        [
            max(array[0],array[1]),
            min(array[0],array[1]),
        ]
    });
    
    // ... Keep going until the whole workgroup is sorted.
}
```

As a comparison point, the equivalent GLSL for at least the first iteration would look something like this:

```glsl
layout (...) workgroupcoherent Buffer buffer {
    uint array[];
};

void main() {
  if (gl_LocalInvocationID % 2) {
    memoryBarrier(gl_ScopeWorkgroup, gl_StorageSemanticsBuffer, gl_SemanticsAcquire);
    uint max = max(buffer.array[0], buffer.array[1]);
    uint min = min(buffer.array[0], buffer.array[1]);
    buffer.array[0] = max;
    buffer.array[1] = min;
    memoryBarrier(gl_ScopeWorkgroup, gl_StorageSemanticsBuffer, gl_SemanticsRelease);
  }
  ...
}
```

Note that for simplicity, I've done a `map_workgroup_joined` on each iteration, though really the first iteration should use all available threads rather than masking half of them out and wasting perf... this is possibly something that some time should be spent to make things a bit more ergonomic, though this is likely contingent on the overall binding proposal.

### Future Additions

#### Storage Images

Storage images are going to be pretty similar, but they are multi-dimensional rather than being indexed in a single dimension, which complicates the implicit indexing in this proposal. Not difficult, but left as an exercise for a future proposal.

#### More complex access patterns

There are applications using interesting access patterns that are (theoretically) data race free, but cannot be boiled down to simply "accessing nearby data". Things like implementing a suballocation or linked lists on a buffer would be impossible with this interface. There are also local accses patterns like scatters, gathers, and reductions which would be interesting to handle.

#### Multiple Resources

Being able to interleave multiple resources for the map functions in a way similar to the `itertools` crate would likely be useful in future.

#### Atomics

Atomic loads, stores, and other operations might be relatively simple to add in some way, but will require some consideration as to how to make them safe, as they enable more complex code that's potentially harder to validate. In particular, atomics can be used in some places in lieu of a synchronization barrier, but whether they satisfy the synchronization requirements or not is far harder to prove.

## Drawbacks

The main drawback of this proposal perhaps is that it's quite prescriptive, and might not clearly map to what developers in other shading languages are used to. It also doesn't address all possible uses of barriers, and more work will need to be done to address other use cases as time goes on.

One other drawback currently is that as certain arguments are coded as constants (e.g. RATE), it's not straightforward to write loops for algorithms that would typically want them (e.g. see the example).

### Not synchronizing outside of the shader

All of the correctness this proposal brings is irrelevant if some other shader/API command/the host accesses the same memory location and doesn't synchronize it properly. Since that falls outside of the scope of a single shader, it's not covered inside this proposal. The likely solution to this will require something like a task graph proposal.

### Aliasing

Yet another "outside of the shader" problem, if two resources input to the shader alias the same memory, there's nothing in the shader that can be done to guarantee safety. Again this will probably need something like a task graph proposal.

## Alternatives

The main clear options are an unsafe interface similar to GLSL/HLSL, or something safer like this. The V1 version of this was one other proposal, but it was scrapped as fn drop() cannot be relied upon for safety.

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
As part of releasing that, the group wrote up a [comparison between the Vulkan and C++ memory models](https://www.khronos.org/blog/comparing-the-vulkan-spir-v-memory-model-to-cs), which may be useful to those more familiar with C++.
The new Vulkan memory model is also exposed in GLSL via the [memory scope semantics](https://github.com/KhronosGroup/GLSL/blob/master/extensions/khr/GL_KHR_memory_scope_semantics.txt) extension.

One of the key draws of this extension was to parameterise everything that an implementation might choose to do with memory; the memory type, memory scope, and execution scope all became independently controllable.
GLSL hasn't really taken advantage of this, as it functions fairly closely to SPIR-V, and is thus only really providing a somewhat friendly SPIR-V wrapper.

Creating the memory model also enabled us to have in-depth (and sometimes painful) discussions about how implementations were actually behaving when particular code was written, and what app developers typically wanted to do.
A lot of discussion/feedback here was that most developer use cases fall into a fairly narrow band, and whilst that can be much more tightly expressed with the memory model than the current limited barriers, the full suite of parameters the memory model expressed was too much, particularly without a defined execution model (e.g. you can't write spin locks!).
Additionally, whilst GLSL and SPIR-V were necessarily conservative by having no automatic visibility for resource writes, most use cases expected that these writes would be made visible in some way to other invocations.
All of this informed the decisions in this proposal, which suggests a tighter definition than the full memory model, but that also slightly differs from than traditional shading languages by making visibility there "by default" (albeit with explicit scope opt-in), and being relativel prescriptive.
