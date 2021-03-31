// Tests that multiple `#[spirv(...)]` attributes that are either identical, or
// part of the same mutually exclusive category, are properly disallowed.

// build-fail

use spirv_std as _;

#[spirv(sampler, sampler)]
struct _SameIntrinsicType {}

#[spirv(
    sampler,
    image_type(
        dim = "Dim2D",
        depth = 0,
        arrayed = 0,
        multisampled = 0,
        sampled = 1,
        image_format = "Unknown"
    )
)]
struct _DiffIntrinsicType {}

#[spirv(block, block)]
struct _Block {}

#[spirv(vertex, vertex)]
fn _same_entry() {}

#[spirv(vertex, fragment)]
fn _diff_entry() {}

#[spirv(vertex)]
fn _entry(
    #[spirv(uniform, uniform)] _same_storage_class: (),
    #[spirv(uniform, push_constant)] _diff_storage_class: (),

    #[spirv(position, position)] _same_builtin: (),
    #[spirv(position, vertex_index)] _diff_builtin: (),

    #[spirv(descriptor_set = 0, descriptor_set = 0)] _same_descriptor_set: (),
    #[spirv(descriptor_set = 0, descriptor_set = 1)] _diff_descriptor_set: (),

    #[spirv(binding = 0, binding = 0)] _same_binding: (),
    #[spirv(binding = 0, binding = 1)] _diff_binding: (),

    #[spirv(flat, flat)] _flat: (),

    #[spirv(invariant, invariant)] _invariant: (),
) {
}

#[spirv(unroll_loops, unroll_loops)]
fn _unroll_loops() {}
