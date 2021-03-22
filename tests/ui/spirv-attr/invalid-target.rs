// Tests that `#[spirv(...)]` attributes cannot be applied to the wrong "targets"
// (i.e. various kinds of definitions and other syntactic categories).

// build-fail

#![feature(extern_types, min_type_alias_impl_trait, stmt_expr_attributes, trait_alias)]

// NOTE(eddyb) in the interest of keeping this test manageable, only one of
// each of the following categories of `#[spirv(...)]` attributes is used:
// * builtin: `position`
// * storage class: `uniform`
// * entry: `vertex`

// NOTE(eddyb) accounting for the number of errors this test actually produces:
// * 422 "attribute is only valid on" errors (see `invalid-target.stderr`)
// * 40 `#[spirv(...)]` (excluding `macro_rules!`, which doesn't get the above error)
// * at most 11 attributes per `#[spirv(...)]`, so an upper bound of `40*11 = 440`
// * the difference between 440 and 422 is 18, i.e. valid attributes, made up of:
//   * 5 on `_Struct`
//   * 8 on functions, i.e. 2 on each of:
//     * `_inherent_method`
//     * `_trait_method_with_default`,
//     * `_trait_method` (in `impl _Trait for ()`)
//     * `_fn`
//   * 4 on `_entry_param`
//   * 1 on `_closure`

#[spirv(
    uniform, sampler, block, sampled_image, // struct-only (incl. `image_type`)
    image_type(dim = "Dim2D", depth = 0, arrayed = 0, multisampled = 0, sampled = 1, image_format = "Unknown"),
    vertex, // fn-only
    position, descriptor_set = 0, binding = 0, flat, // param-only
    unroll_loops, // fn/closure-only
)]
macro_rules! _macro {
    () => {}
}

#[spirv(
    uniform, sampler, block, sampled_image, // struct-only (incl. `image_type`)
    image_type(dim = "Dim2D", depth = 0, arrayed = 0, multisampled = 0, sampled = 1, image_format = "Unknown"),
    vertex, // fn-only
    position, descriptor_set = 0, binding = 0, flat, // param-only
    unroll_loops, // fn/closure-only
)]
extern crate spirv_std as _;

#[spirv(
    uniform, sampler, block, sampled_image, // struct-only (incl. `image_type`)
    image_type(dim = "Dim2D", depth = 0, arrayed = 0, multisampled = 0, sampled = 1, image_format = "Unknown"),
    vertex, // fn-only
    position, descriptor_set = 0, binding = 0, flat, // param-only
    unroll_loops, // fn/closure-only
)]
use spirv_std as _;

#[spirv(
    uniform, sampler, block, sampled_image, // struct-only (incl. `image_type`)
    image_type(dim = "Dim2D", depth = 0, arrayed = 0, multisampled = 0, sampled = 1, image_format = "Unknown"),
    vertex, // fn-only
    position, descriptor_set = 0, binding = 0, flat, // param-only
    unroll_loops, // fn/closure-only
)]
mod _mod {}

#[spirv(
    uniform, sampler, block, sampled_image, // struct-only (incl. `image_type`)
    image_type(dim = "Dim2D", depth = 0, arrayed = 0, multisampled = 0, sampled = 1, image_format = "Unknown"),
    vertex, // fn-only
    position, descriptor_set = 0, binding = 0, flat, // param-only
    unroll_loops, // fn/closure-only
)]
extern "C" {
    #[spirv(
        uniform, sampler, block, sampled_image, // struct-only (incl. `image_type`)
        image_type(dim = "Dim2D", depth = 0, arrayed = 0, multisampled = 0, sampled = 1, image_format = "Unknown"),
        vertex, // fn-only
        position, descriptor_set = 0, binding = 0, flat, // param-only
        unroll_loops, // fn/closure-only
    )]
    type _ForeignTy;

    #[spirv(
        uniform, sampler, block, sampled_image, // struct-only (incl. `image_type`)
        image_type(dim = "Dim2D", depth = 0, arrayed = 0, multisampled = 0, sampled = 1, image_format = "Unknown"),
        vertex, // fn-only
        position, descriptor_set = 0, binding = 0, flat, // param-only
        unroll_loops, // fn/closure-only
    )]
    static _FOREIGN_STATIC: ();

    #[spirv(
        uniform, sampler, block, sampled_image, // struct-only (incl. `image_type`)
        image_type(dim = "Dim2D", depth = 0, arrayed = 0, multisampled = 0, sampled = 1, image_format = "Unknown"),
        vertex, // fn-only
        position, descriptor_set = 0, binding = 0, flat, // param-only
        unroll_loops, // fn/closure-only
    )]
    fn _foreign_fn();
}

#[spirv(
    uniform, sampler, block, sampled_image, // struct-only (incl. `image_type`)
    image_type(dim = "Dim2D", depth = 0, arrayed = 0, multisampled = 0, sampled = 1, image_format = "Unknown"),
    vertex, // fn-only
    position, descriptor_set = 0, binding = 0, flat, // param-only
    unroll_loops, // fn/closure-only
)]
static _STATIC: () = ();

#[spirv(
    uniform, sampler, block, sampled_image, // struct-only (incl. `image_type`)
    image_type(dim = "Dim2D", depth = 0, arrayed = 0, multisampled = 0, sampled = 1, image_format = "Unknown"),
    vertex, // fn-only
    position, descriptor_set = 0, binding = 0, flat, // param-only
    unroll_loops, // fn/closure-only
)]
const _CONST: () = ();

#[spirv(
    uniform, sampler, block, sampled_image, // struct-only (incl. `image_type`)
    image_type(dim = "Dim2D", depth = 0, arrayed = 0, multisampled = 0, sampled = 1, image_format = "Unknown"),
    vertex, // fn-only
    position, descriptor_set = 0, binding = 0, flat, // param-only
    unroll_loops, // fn/closure-only
)]
type _TyAlias = ();

#[spirv(
    uniform, sampler, block, sampled_image, // struct-only (incl. `image_type`)
    image_type(dim = "Dim2D", depth = 0, arrayed = 0, multisampled = 0, sampled = 1, image_format = "Unknown"),
    vertex, // fn-only
    position, descriptor_set = 0, binding = 0, flat, // param-only
    unroll_loops, // fn/closure-only
)]
type _OpaqueTy = impl Copy;

fn _opaque_ty_definer() -> _OpaqueTy { () }

#[spirv(
    uniform, sampler, block, sampled_image, // struct-only (incl. `image_type`)
    image_type(dim = "Dim2D", depth = 0, arrayed = 0, multisampled = 0, sampled = 1, image_format = "Unknown"),
    vertex, // fn-only
    position, descriptor_set = 0, binding = 0, flat, // param-only
    unroll_loops, // fn/closure-only
)]
enum _Enum {
    #[spirv(
        uniform, sampler, block, sampled_image, // struct-only (incl. `image_type`)
        image_type(dim = "Dim2D", depth = 0, arrayed = 0, multisampled = 0, sampled = 1, image_format = "Unknown"),
        vertex, // fn-only
        position, descriptor_set = 0, binding = 0, flat, // param-only
        unroll_loops, // fn/closure-only
    )]
    _Variant {
        #[spirv(
            uniform, sampler, block, sampled_image, // struct-only (incl. `image_type`)
            image_type(dim = "Dim2D", depth = 0, arrayed = 0, multisampled = 0, sampled = 1, image_format = "Unknown"),
            vertex, // fn-only
            position, descriptor_set = 0, binding = 0, flat, // param-only
            unroll_loops, // fn/closure-only
        )]
        _field: ()
    }
}

#[spirv(
    uniform, sampler, block, sampled_image, // struct-only (incl. `image_type`)
    image_type(dim = "Dim2D", depth = 0, arrayed = 0, multisampled = 0, sampled = 1, image_format = "Unknown"),
    vertex, // fn-only
    position, descriptor_set = 0, binding = 0, flat, // param-only
    unroll_loops, // fn/closure-only
)]
union _Union {
    #[spirv(
        uniform, sampler, block, sampled_image, // struct-only (incl. `image_type`)
        image_type(dim = "Dim2D", depth = 0, arrayed = 0, multisampled = 0, sampled = 1, image_format = "Unknown"),
        vertex, // fn-only
        position, descriptor_set = 0, binding = 0, flat, // param-only
        unroll_loops, // fn/closure-only
    )]
    _field: ()
}

#[spirv(
    vertex, // fn-only
    position, descriptor_set = 0, binding = 0, flat, // param-only
    unroll_loops, // fn/closure-only
)]
struct _Struct {
    #[spirv(
        uniform, sampler, block, sampled_image, // struct-only (incl. `image_type`)
        image_type(dim = "Dim2D", depth = 0, arrayed = 0, multisampled = 0, sampled = 1, image_format = "Unknown"),
        vertex, // fn-only
        position, descriptor_set = 0, binding = 0, flat, // param-only
        unroll_loops, // fn/closure-only
    )]
    _field: ()
}

#[spirv(
    uniform, sampler, block, sampled_image, // struct-only (incl. `image_type`)
    image_type(dim = "Dim2D", depth = 0, arrayed = 0, multisampled = 0, sampled = 1, image_format = "Unknown"),
    vertex, // fn-only
    position, descriptor_set = 0, binding = 0, flat, // param-only
    unroll_loops, // fn/closure-only
)]
impl _Struct {
    #[spirv(
        uniform, sampler, block, sampled_image, // struct-only (incl. `image_type`)
        image_type(dim = "Dim2D", depth = 0, arrayed = 0, multisampled = 0, sampled = 1, image_format = "Unknown"),
        vertex, // fn-only
        position, descriptor_set = 0, binding = 0, flat, // param-only
        unroll_loops, // fn/closure-only
    )]
    const _INHERENT_ASSOC_CONST: () = ();

    #[spirv(
        uniform, sampler, block, sampled_image, // struct-only (incl. `image_type`)
        image_type(dim = "Dim2D", depth = 0, arrayed = 0, multisampled = 0, sampled = 1, image_format = "Unknown"),
        position, descriptor_set = 0, binding = 0, flat, // param-only
    )]
    fn _inherent_method() {}
}

#[spirv(
    uniform, sampler, block, sampled_image, // struct-only (incl. `image_type`)
    image_type(dim = "Dim2D", depth = 0, arrayed = 0, multisampled = 0, sampled = 1, image_format = "Unknown"),
    vertex, // fn-only
    position, descriptor_set = 0, binding = 0, flat, // param-only
    unroll_loops, // fn/closure-only
)]
trait _TraitAlias = Copy;

#[spirv(
    uniform, sampler, block, sampled_image, // struct-only (incl. `image_type`)
    image_type(dim = "Dim2D", depth = 0, arrayed = 0, multisampled = 0, sampled = 1, image_format = "Unknown"),
    vertex, // fn-only
    position, descriptor_set = 0, binding = 0, flat, // param-only
    unroll_loops, // fn/closure-only
)]
trait _Trait {
    #[spirv(
        uniform, sampler, block, sampled_image, // struct-only (incl. `image_type`)
        image_type(dim = "Dim2D", depth = 0, arrayed = 0, multisampled = 0, sampled = 1, image_format = "Unknown"),
        vertex, // fn-only
        position, descriptor_set = 0, binding = 0, flat, // param-only
        unroll_loops, // fn/closure-only
    )]
    type _AssocTy;

    #[spirv(
        uniform, sampler, block, sampled_image, // struct-only (incl. `image_type`)
        image_type(dim = "Dim2D", depth = 0, arrayed = 0, multisampled = 0, sampled = 1, image_format = "Unknown"),
        vertex, // fn-only
        position, descriptor_set = 0, binding = 0, flat, // param-only
        unroll_loops, // fn/closure-only
    )]
    const _TRAIT_ASSOC_CONST: ();

    #[spirv(
        uniform, sampler, block, sampled_image, // struct-only (incl. `image_type`)
        image_type(dim = "Dim2D", depth = 0, arrayed = 0, multisampled = 0, sampled = 1, image_format = "Unknown"),
        vertex, // fn-only
        position, descriptor_set = 0, binding = 0, flat, // param-only
        unroll_loops, // fn/closure-only
    )]
    fn _trait_method();

    #[spirv(
        uniform, sampler, block, sampled_image, // struct-only (incl. `image_type`)
        image_type(dim = "Dim2D", depth = 0, arrayed = 0, multisampled = 0, sampled = 1, image_format = "Unknown"),
        position, descriptor_set = 0, binding = 0, flat, // param-only
    )]
    fn _trait_method_with_default() {}
}

#[spirv(
    uniform, sampler, block, sampled_image, // struct-only (incl. `image_type`)
    image_type(dim = "Dim2D", depth = 0, arrayed = 0, multisampled = 0, sampled = 1, image_format = "Unknown"),
    vertex, // fn-only
    position, descriptor_set = 0, binding = 0, flat, // param-only
    unroll_loops, // fn/closure-only
)]
impl _Trait for () {
    #[spirv(
        uniform, sampler, block, sampled_image, // struct-only (incl. `image_type`)
        image_type(dim = "Dim2D", depth = 0, arrayed = 0, multisampled = 0, sampled = 1, image_format = "Unknown"),
        vertex, // fn-only
        position, descriptor_set = 0, binding = 0, flat, // param-only
        unroll_loops, // fn/closure-only
    )]
    type _AssocTy = ();

    #[spirv(
        uniform, sampler, block, sampled_image, // struct-only (incl. `image_type`)
        image_type(dim = "Dim2D", depth = 0, arrayed = 0, multisampled = 0, sampled = 1, image_format = "Unknown"),
        vertex, // fn-only
        position, descriptor_set = 0, binding = 0, flat, // param-only
        unroll_loops, // fn/closure-only
    )]
    const _TRAIT_ASSOC_CONST: () = ();

    #[spirv(
        uniform, sampler, block, sampled_image, // struct-only (incl. `image_type`)
        image_type(dim = "Dim2D", depth = 0, arrayed = 0, multisampled = 0, sampled = 1, image_format = "Unknown"),
        position, descriptor_set = 0, binding = 0, flat, // param-only
    )]
    fn _trait_method() {}
}

#[spirv(
    uniform, sampler, block, sampled_image, // struct-only (incl. `image_type`)
    image_type(dim = "Dim2D", depth = 0, arrayed = 0, multisampled = 0, sampled = 1, image_format = "Unknown"),
    position, descriptor_set = 0, binding = 0, flat, // param-only
)]
fn _fn(
    #[spirv(
        uniform, sampler, block, sampled_image, // struct-only (incl. `image_type`)
        image_type(dim = "Dim2D", depth = 0, arrayed = 0, multisampled = 0, sampled = 1, image_format = "Unknown"),
        vertex, // fn-only
        unroll_loops, // fn/closure-only
    )]
    _entry_param: ()
) {
    #[spirv(
        uniform, sampler, block, sampled_image, // struct-only (incl. `image_type`)
        image_type(dim = "Dim2D", depth = 0, arrayed = 0, multisampled = 0, sampled = 1, image_format = "Unknown"),
        vertex, // fn-only
        position, descriptor_set = 0, binding = 0, flat, // param-only
        unroll_loops, // fn/closure-only
    )]
    let _statement = ();

    let _closure =
        #[spirv(
            uniform, sampler, block, sampled_image, // struct-only (incl. `image_type`)
            image_type(dim = "Dim2D", depth = 0, arrayed = 0, multisampled = 0, sampled = 1, image_format = "Unknown"),
            vertex, // fn-only
            position, descriptor_set = 0, binding = 0, flat, // param-only
        )]
        || {};

    (
        #[spirv(
            uniform, sampler, block, sampled_image, // struct-only (incl. `image_type`)
            image_type(dim = "Dim2D", depth = 0, arrayed = 0, multisampled = 0, sampled = 1, image_format = "Unknown"),
            vertex, // fn-only
            position, descriptor_set = 0, binding = 0, flat, // param-only
            unroll_loops, // fn/closure-only
        )]
        (1, 2, 3) // expression
    );

    match () {
        #[spirv(
            uniform, sampler, block, sampled_image, // struct-only (incl. `image_type`)
            image_type(dim = "Dim2D", depth = 0, arrayed = 0, multisampled = 0, sampled = 1, image_format = "Unknown"),
            vertex, // fn-only
            position, descriptor_set = 0, binding = 0, flat, // param-only
            unroll_loops, // fn/closure-only
        )]
        _arm => {}
    }
}

fn _fn_with_generics<
    #[spirv(
        uniform, sampler, block, sampled_image, // struct-only (incl. `image_type`)
        image_type(dim = "Dim2D", depth = 0, arrayed = 0, multisampled = 0, sampled = 1, image_format = "Unknown"),
        vertex, // fn-only
        position, descriptor_set = 0, binding = 0, flat, // param-only
        unroll_loops, // fn/closure-only
    )]
    '_lifetime_param,

    #[spirv(
        uniform, sampler, block, sampled_image, // struct-only (incl. `image_type`)
        image_type(dim = "Dim2D", depth = 0, arrayed = 0, multisampled = 0, sampled = 1, image_format = "Unknown"),
        vertex, // fn-only
        position, descriptor_set = 0, binding = 0, flat, // param-only
        unroll_loops, // fn/closure-only
    )]
    _TyParam,

    #[spirv(
        uniform, sampler, block, sampled_image, // struct-only (incl. `image_type`)
        image_type(dim = "Dim2D", depth = 0, arrayed = 0, multisampled = 0, sampled = 1, image_format = "Unknown"),
        vertex, // fn-only
        position, descriptor_set = 0, binding = 0, flat, // param-only
        unroll_loops, // fn/closure-only
    )]
    const _CONST_PARAM: usize
>() {}
