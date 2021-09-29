// Tests that `#[spirv(...)]` attributes cannot be applied to the wrong "targets"
// (i.e. various kinds of definitions and other syntactic categories).

// build-fail

#![feature(
    extern_types,
    type_alias_impl_trait, // HACK(eddyb) this comment prevents rustfmt
    stmt_expr_attributes,
    trait_alias
)]

// NOTE(eddyb) in the interest of keeping this test manageable, only one of
// each of the following categories of `#[spirv(...)]` attributes is used:
// * entry: `vertex`
// * storage class: `uniform`
// * builtin: `position`

// NOTE(eddyb) accounting for the number of errors this test actually produces:
// * 473 errors, all "attribute is only valid on" (see `invalid-target.stderr`)
// * 41 uses of `#[spirv(...)]` in this test
// * at most 12 attributes per `#[spirv(...)]`, so an upper bound of `41*12 = 492`
// * the difference between 492 and 473 is 19, i.e. valid attributes, made up of:
//   * 4 on `_Struct`
//   * 8 on functions, i.e. 2 on each of:
//     * `_inherent_method`
//     * `_trait_method_with_default`,
//     * `_trait_method` (in `impl _Trait for ()`)
//     * `_fn`
//   * 6 on `_entry_param`
//   * 1 on `_closure`

#[spirv(
    sampler, block, sampled_image, generic_image_type, // struct-only
    vertex, // fn-only
    uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
    unroll_loops, // fn/closure-only
)]
macro_rules! _macro {
    () => {};
}

#[spirv(
    sampler, block, sampled_image, generic_image_type, // struct-only
    vertex, // fn-only
    uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
    unroll_loops, // fn/closure-only
)]
extern crate spirv_std as _;

#[spirv(
    sampler, block, sampled_image, generic_image_type, // struct-only
    vertex, // fn-only
    uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
    unroll_loops, // fn/closure-only
)]
use spirv_std as _;

#[spirv(
    sampler, block, sampled_image, generic_image_type, // struct-only
    vertex, // fn-only
    uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
    unroll_loops, // fn/closure-only
)]
mod _mod {}

#[spirv(
    sampler, block, sampled_image, generic_image_type, // struct-only
    vertex, // fn-only
    uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
    unroll_loops, // fn/closure-only
)]
extern "C" {
    #[spirv(
        sampler, block, sampled_image, generic_image_type, // struct-only
        vertex, // fn-only
        uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
        unroll_loops, // fn/closure-only
    )]
    type _ForeignTy;

    #[spirv(
        sampler, block, sampled_image, generic_image_type, // struct-only
        vertex, // fn-only
        uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
        unroll_loops, // fn/closure-only
    )]
    static _FOREIGN_STATIC: ();

    #[spirv(
        sampler, block, sampled_image, generic_image_type, // struct-only
        vertex, // fn-only
        uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
        unroll_loops, // fn/closure-only
    )]
    fn _foreign_fn();
}

#[spirv(
    sampler, block, sampled_image, generic_image_type, // struct-only
    vertex, // fn-only
    uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
    unroll_loops, // fn/closure-only
)]
static _STATIC: () = ();

#[spirv(
    sampler, block, sampled_image, generic_image_type, // struct-only
    vertex, // fn-only
    uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
    unroll_loops, // fn/closure-only
)]
const _CONST: () = ();

#[spirv(
    sampler, block, sampled_image, generic_image_type, // struct-only
    vertex, // fn-only
    uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
    unroll_loops, // fn/closure-only
)]
type _TyAlias = ();

#[spirv(
    sampler, block, sampled_image, generic_image_type, // struct-only
    vertex, // fn-only
    uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
    unroll_loops, // fn/closure-only
)]
type _OpaqueTy = impl Copy;

fn _opaque_ty_definer() -> _OpaqueTy {
    ()
}

#[spirv(
    sampler, block, sampled_image, generic_image_type, // struct-only
    vertex, // fn-only
    uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
    unroll_loops, // fn/closure-only
)]
enum _Enum {
    #[spirv(
        sampler, block, sampled_image, generic_image_type, // struct-only
        vertex, // fn-only
        uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
        unroll_loops, // fn/closure-only
    )]
    _Variant {
        #[spirv(
            sampler, block, sampled_image, generic_image_type, // struct-only
            vertex, // fn-only
            uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
            unroll_loops, // fn/closure-only
        )]
        _field: (),
    },
}

#[spirv(
    sampler, block, sampled_image, generic_image_type, // struct-only
    vertex, // fn-only
    uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
    unroll_loops, // fn/closure-only
)]
union _Union {
    #[spirv(
        sampler, block, sampled_image, generic_image_type, // struct-only
        vertex, // fn-only
        uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
        unroll_loops, // fn/closure-only
    )]
    _field: (),
}

#[spirv(
    vertex, // fn-only
    uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
    unroll_loops, // fn/closure-only
)]
struct _Struct {
    #[spirv(
        sampler, block, sampled_image, generic_image_type, // struct-only
        vertex, // fn-only
        uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
        unroll_loops, // fn/closure-only
    )]
    _field: (),
}

#[spirv(
    sampler, block, sampled_image, generic_image_type, // struct-only
    vertex, // fn-only
    uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
    unroll_loops, // fn/closure-only
)]
impl _Struct {
    #[spirv(
        sampler, block, sampled_image, generic_image_type, // struct-only
        vertex, // fn-only
        uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
        unroll_loops, // fn/closure-only
    )]
    const _INHERENT_ASSOC_CONST: () = ();

    #[spirv(
        sampler, block, sampled_image, generic_image_type, // struct-only
        uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
    )]
    fn _inherent_method() {}
}

#[spirv(
    sampler, block, sampled_image, generic_image_type, // struct-only
    vertex, // fn-only
    uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
    unroll_loops, // fn/closure-only
)]
trait _TraitAlias = Copy;

#[spirv(
    sampler, block, sampled_image, generic_image_type, // struct-only
    vertex, // fn-only
    uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
    unroll_loops, // fn/closure-only
)]
trait _Trait {
    #[spirv(
        sampler, block, sampled_image, generic_image_type, // struct-only
        vertex, // fn-only
        uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
        unroll_loops, // fn/closure-only
    )]
    type _AssocTy;

    #[spirv(
        sampler, block, sampled_image, generic_image_type, // struct-only
        vertex, // fn-only
        uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
        unroll_loops, // fn/closure-only
    )]
    const _TRAIT_ASSOC_CONST: ();

    #[spirv(
        sampler, block, sampled_image, generic_image_type, // struct-only
        vertex, // fn-only
        uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
        unroll_loops, // fn/closure-only
    )]
    fn _trait_method();

    #[spirv(
        sampler, block, sampled_image, generic_image_type, // struct-only
        uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
    )]
    fn _trait_method_with_default() {}
}

#[spirv(
    sampler, block, sampled_image, generic_image_type, // struct-only
    vertex, // fn-only
    uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
    unroll_loops, // fn/closure-only
)]
impl _Trait for () {
    #[spirv(
        sampler, block, sampled_image, generic_image_type, // struct-only
        vertex, // fn-only
        uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
        unroll_loops, // fn/closure-only
    )]
    type _AssocTy = ();

    #[spirv(
        sampler, block, sampled_image, generic_image_type, // struct-only
        vertex, // fn-only
        uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
        unroll_loops, // fn/closure-only
    )]
    const _TRAIT_ASSOC_CONST: () = ();

    #[spirv(
        sampler, block, sampled_image, generic_image_type, // struct-only
        uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
    )]
    fn _trait_method() {}
}

#[spirv(
    sampler, block, sampled_image, generic_image_type, // struct-only
    uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
)]
fn _fn(
    #[spirv(
        sampler, block, sampled_image, generic_image_type, // struct-only
        vertex, // fn-only
        unroll_loops, // fn/closure-only
    )]
    _entry_param: (),
) {
    #[spirv(
        sampler, block, sampled_image, generic_image_type, // struct-only
        vertex, // fn-only
        uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
        unroll_loops, // fn/closure-only
    )]
    let _statement = ();

    let _closure = #[spirv(
            sampler, block, sampled_image, generic_image_type, // struct-only
            vertex, // fn-only
            uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
        )]
    || {};

    (
        #[spirv(
            sampler, block, sampled_image, generic_image_type, // struct-only
            vertex, // fn-only
            uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
            unroll_loops, // fn/closure-only
        )]
        (1, 2, 3) // expression
    );

    match () {
        #[spirv(
            sampler, block, sampled_image, generic_image_type, // struct-only
            vertex, // fn-only
            uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
            unroll_loops, // fn/closure-only
        )]
        _arm => {}
    }
}

fn _fn_with_generics<
    #[spirv(
        sampler, block, sampled_image, generic_image_type, // struct-only
        vertex, // fn-only
        uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
        unroll_loops, // fn/closure-only
    )] '_lifetime_param,
    #[spirv(
        sampler, block, sampled_image, generic_image_type, // struct-only
        vertex, // fn-only
        uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
        unroll_loops, // fn/closure-only
    )] _TyParam,
    #[spirv(
        sampler, block, sampled_image, generic_image_type, // struct-only
        vertex, // fn-only
        uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
        unroll_loops, // fn/closure-only
    )] const _CONST_PARAM: usize,
>() {
}
