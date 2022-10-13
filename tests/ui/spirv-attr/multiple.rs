// Tests that multiple `#[rust_gpu::spirv(...)]` attributes that are either identical, or
// part of the same mutually exclusive category, are properly disallowed.

// build-fail

use spirv_std as _;

#[rust_gpu::spirv(sampler, sampler)]
struct _SameIntrinsicType {}

#[rust_gpu::spirv(matrix, matrix)]
struct _SameIntrinsicMatrixType {
    x: glam::Vec3,
    y: glam::Vec3,
}

#[rust_gpu::spirv(sampler, generic_image_type)]
struct _DiffIntrinsicType {}

#[rust_gpu::spirv(sampler, matrix)]
struct _SamplerAndMatrix {
    x: glam::Vec3,
    y: glam::Vec3,
}

#[rust_gpu::spirv(block, block)]
struct _Block {}

#[rust_gpu::spirv(vertex, vertex)]
fn _same_entry() {}

#[rust_gpu::spirv(vertex, fragment)]
fn _diff_entry() {}

#[rust_gpu::spirv(vertex)]
fn _entry(
    #[rust_gpu::spirv(uniform, uniform)] _same_storage_class: (),
    #[rust_gpu::spirv(uniform, push_constant)] _diff_storage_class: (),

    #[rust_gpu::spirv(position, position)] _same_builtin: (),
    #[rust_gpu::spirv(position, vertex_index)] _diff_builtin: (),

    #[rust_gpu::spirv(descriptor_set = 0, descriptor_set = 0)] _same_descriptor_set: (),
    #[rust_gpu::spirv(descriptor_set = 0, descriptor_set = 1)] _diff_descriptor_set: (),

    #[rust_gpu::spirv(binding = 0, binding = 0)] _same_binding: (),
    #[rust_gpu::spirv(binding = 0, binding = 1)] _diff_binding: (),

    #[rust_gpu::spirv(flat, flat)] _flat: (),

    #[rust_gpu::spirv(invariant, invariant)] _invariant: (),
) {
}

#[rust_gpu::spirv(unroll_loops, unroll_loops)]
fn _unroll_loops() {}
