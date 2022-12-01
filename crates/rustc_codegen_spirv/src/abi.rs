//! This file is responsible for translation from rustc tys (`TyAndLayout`) to spir-v types. It's
//! surprisingly difficult.

use crate::attr::{AggregatedSpirvAttributes, IntrinsicType};
use crate::codegen_cx::CodegenCx;
use crate::spirv_type::SpirvType;
use rspirv::spirv::{StorageClass, Word};
use rustc_data_structures::fx::FxHashMap;
use rustc_errors::ErrorGuaranteed;
use rustc_index::vec::Idx;
use rustc_middle::ty::layout::{FnAbiOf, LayoutOf, TyAndLayout};
use rustc_middle::ty::query::{ExternProviders, Providers};
use rustc_middle::ty::subst::SubstsRef;
use rustc_middle::ty::{
    self, Const, FloatTy, GeneratorSubsts, IntTy, ParamEnv, PolyFnSig, Ty, TyCtxt, TyKind,
    TypeAndMut, UintTy,
};
use rustc_middle::{bug, span_bug};
use rustc_span::def_id::DefId;
use rustc_span::DUMMY_SP;
use rustc_span::{Span, Symbol};
use rustc_target::abi::call::{ArgAbi, ArgAttributes, FnAbi, PassMode};
use rustc_target::abi::{
    Abi, Align, FieldsShape, LayoutS, Primitive, Scalar, Size, TagEncoding, VariantIdx, Variants,
};
use rustc_target::spec::abi::Abi as SpecAbi;
use std::cell::RefCell;
use std::collections::hash_map::Entry;
use std::fmt;

use num_traits::cast::FromPrimitive;

pub(crate) fn provide(providers: &mut Providers) {
    // This is a lil weird: so, we obviously don't support C ABIs at all. However, libcore does declare some extern
    // C functions:
    // https://github.com/rust-lang/rust/blob/5fae56971d8487088c0099c82c0a5ce1638b5f62/library/core/src/slice/cmp.rs#L119
    // However, those functions will be implemented by compiler-builtins:
    // https://github.com/rust-lang/rust/blob/5fae56971d8487088c0099c82c0a5ce1638b5f62/library/core/src/lib.rs#L23-L27
    // This theoretically then should be fine to leave as C, but, there's no backend hook for
    // `FnAbi::adjust_for_cabi`, causing it to panic:
    // https://github.com/rust-lang/rust/blob/5fae56971d8487088c0099c82c0a5ce1638b5f62/compiler/rustc_target/src/abi/call/mod.rs#L603
    // So, treat any `extern "C"` functions as `extern "unadjusted"`, to be able to compile libcore with arch=spirv.
    providers.fn_sig = |tcx, def_id| {
        // We can't capture the old fn_sig and just call that, because fn_sig is a `fn`, not a `Fn`, i.e. it can't
        // capture variables. Fortunately, the defaults are exposed (thanks rustdoc), so use that instead.
        let result = (rustc_interface::DEFAULT_QUERY_PROVIDERS.fn_sig)(tcx, def_id);
        result.map_bound(|mut inner| {
            if let SpecAbi::C { .. } = inner.abi {
                inner.abi = SpecAbi::Unadjusted;
            }
            inner
        })
    };

    // For the Rust ABI, `FnAbi` adjustments are backend-agnostic, but they will
    // use features like `PassMode::Cast`, that are incompatible with SPIR-V.
    // By hooking the queries computing `FnAbi`s, we can recompute the `FnAbi`
    // from the return/args layouts, to e.g. prefer using `PassMode::Direct`.
    fn readjust_fn_abi<'tcx>(
        tcx: TyCtxt<'tcx>,
        fn_abi: &'tcx FnAbi<'tcx, Ty<'tcx>>,
    ) -> &'tcx FnAbi<'tcx, Ty<'tcx>> {
        let readjust_arg_abi = |arg: &ArgAbi<'tcx, Ty<'tcx>>| {
            let mut arg = ArgAbi::new(&tcx, arg.layout, |_, _, _| ArgAttributes::new());

            // Avoid pointlessly passing ZSTs, just like the official Rust ABI.
            if arg.layout.is_zst() {
                arg.mode = PassMode::Ignore;
            }

            arg
        };
        tcx.arena.alloc(FnAbi {
            args: fn_abi.args.iter().map(readjust_arg_abi).collect(),
            ret: readjust_arg_abi(&fn_abi.ret),

            // FIXME(eddyb) validate some of these, and report errors - however,
            // we can't just emit errors from here, since we have no `Span`, so
            // we should have instead a check on MIR for e.g. C variadic calls.
            c_variadic: fn_abi.c_variadic,
            fixed_count: fn_abi.fixed_count,
            conv: fn_abi.conv,
            can_unwind: fn_abi.can_unwind,
        })
    }
    providers.fn_abi_of_fn_ptr = |tcx, key| {
        let result = (rustc_interface::DEFAULT_QUERY_PROVIDERS.fn_abi_of_fn_ptr)(tcx, key);
        Ok(readjust_fn_abi(tcx, result?))
    };
    providers.fn_abi_of_instance = |tcx, key| {
        let result = (rustc_interface::DEFAULT_QUERY_PROVIDERS.fn_abi_of_instance)(tcx, key);
        Ok(readjust_fn_abi(tcx, result?))
    };

    // FIXME(eddyb) remove this by deriving `Clone` for `LayoutS` upstream.
    // FIXME(eddyb) the `S` suffix is a naming antipattern, rename upstream.
    fn clone_layout<'a>(layout: &LayoutS<'a>) -> LayoutS<'a> {
        let LayoutS {
            ref fields,
            ref variants,
            abi,
            largest_niche,
            align,
            size,
        } = *layout;
        LayoutS {
            fields: match *fields {
                FieldsShape::Primitive => FieldsShape::Primitive,
                FieldsShape::Union(count) => FieldsShape::Union(count),
                FieldsShape::Array { stride, count } => FieldsShape::Array { stride, count },
                FieldsShape::Arbitrary {
                    ref offsets,
                    ref memory_index,
                } => FieldsShape::Arbitrary {
                    offsets: offsets.clone(),
                    memory_index: memory_index.clone(),
                },
            },
            variants: match *variants {
                Variants::Single { index } => Variants::Single { index },
                Variants::Multiple {
                    tag,
                    ref tag_encoding,
                    tag_field,
                    ref variants,
                } => Variants::Multiple {
                    tag,
                    tag_encoding: match *tag_encoding {
                        TagEncoding::Direct => TagEncoding::Direct,
                        TagEncoding::Niche {
                            untagged_variant,
                            ref niche_variants,
                            niche_start,
                        } => TagEncoding::Niche {
                            untagged_variant,
                            niche_variants: niche_variants.clone(),
                            niche_start,
                        },
                    },
                    tag_field,
                    variants: variants.clone(),
                },
            },
            abi,
            largest_niche,
            align,
            size,
        }
    }
    providers.layout_of = |tcx, key| {
        let TyAndLayout { ty, mut layout } =
            (rustc_interface::DEFAULT_QUERY_PROVIDERS.layout_of)(tcx, key)?;

        #[allow(clippy::match_like_matches_macro)]
        let hide_niche = match ty.kind() {
            ty::Bool => true,
            _ => false,
        };

        if hide_niche {
            layout = tcx.intern_layout(LayoutS {
                largest_niche: None,
                ..clone_layout(layout.0 .0)
            });
        }

        Ok(TyAndLayout { ty, layout })
    };
}

pub(crate) fn provide_extern(providers: &mut ExternProviders) {
    // Reset providers overriden in `provide`, that need to still go through the
    // `rustc_metadata::rmeta` decoding, as opposed to being locally computed.
    providers.fn_sig = rustc_interface::DEFAULT_EXTERN_QUERY_PROVIDERS.fn_sig;
}

/// If a struct contains a pointer to itself, even indirectly, then doing a naiive recursive walk
/// of the fields will result in an infinite loop. Because pointers are the only thing that are
/// allowed to be recursive, keep track of what pointers we've translated, or are currently in the
/// progress of translating, and break the recursion that way. This struct manages that state
/// tracking.
#[derive(Default)]
pub struct RecursivePointeeCache<'tcx> {
    map: RefCell<FxHashMap<PointeeTy<'tcx>, PointeeDefState>>,
}

impl<'tcx> RecursivePointeeCache<'tcx> {
    fn begin(&self, cx: &CodegenCx<'tcx>, span: Span, pointee: PointeeTy<'tcx>) -> Option<Word> {
        match self.map.borrow_mut().entry(pointee) {
            // State: This is the first time we've seen this type. Record that we're beginning to translate this type,
            // and start doing the translation.
            Entry::Vacant(entry) => {
                entry.insert(PointeeDefState::Defining);
                None
            }
            Entry::Occupied(mut entry) => match *entry.get() {
                // State: This is the second time we've seen this type, and we're already translating this type. If we
                // were to try to translate the type now, we'd get a stack overflow, due to continually recursing. So,
                // emit an OpTypeForwardPointer, and use that ID. (This is the juicy part of this algorithm)
                PointeeDefState::Defining => {
                    let new_id = cx.emit_global().id();
                    // NOTE(eddyb) we emit `StorageClass::Generic` here, but later
                    // the linker will specialize the entire SPIR-V module to use
                    // storage classes inferred from `OpVariable`s.
                    cx.emit_global()
                        .type_forward_pointer(new_id, StorageClass::Generic);
                    entry.insert(PointeeDefState::DefiningWithForward(new_id));
                    cx.zombie_with_span(
                        new_id,
                        span,
                        "Cannot create self-referential types, even through pointers",
                    );
                    Some(new_id)
                }
                // State: This is the third or more time we've seen this type, and we've already emitted an
                // OpTypeForwardPointer. Just use the ID we've already emitted. (Alternatively, we already defined this
                // type, so just use that.)
                PointeeDefState::DefiningWithForward(id) | PointeeDefState::Defined(id) => Some(id),
            },
        }
    }

    fn end(
        &self,
        cx: &CodegenCx<'tcx>,
        span: Span,
        pointee: PointeeTy<'tcx>,
        pointee_spv: Word,
    ) -> Word {
        match self.map.borrow_mut().entry(pointee) {
            // We should have hit begin() on this type already, which always inserts an entry.
            Entry::Vacant(_) => {
                span_bug!(span, "RecursivePointeeCache::end should always have entry")
            }
            Entry::Occupied(mut entry) => match *entry.get() {
                // State: There have been no recursive references to this type while defining it, and so no
                // OpTypeForwardPointer has been emitted. This is the most common case.
                PointeeDefState::Defining => {
                    let id = SpirvType::Pointer {
                        pointee: pointee_spv,
                    }
                    .def(span, cx);
                    entry.insert(PointeeDefState::Defined(id));
                    id
                }
                // State: There was a recursive reference to this type, and so an OpTypeForwardPointer has been emitted.
                // Make sure to use the same ID.
                PointeeDefState::DefiningWithForward(id) => {
                    entry.insert(PointeeDefState::Defined(id));
                    SpirvType::Pointer {
                        pointee: pointee_spv,
                    }
                    .def_with_id(cx, span, id)
                }
                PointeeDefState::Defined(_) => {
                    span_bug!(span, "RecursivePointeeCache::end defined pointer twice")
                }
            },
        }
    }
}

#[derive(Eq, PartialEq, Hash, Copy, Clone, Debug)]
enum PointeeTy<'tcx> {
    Ty(TyAndLayout<'tcx>),
    Fn(PolyFnSig<'tcx>),
}

impl fmt::Display for PointeeTy<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PointeeTy::Ty(ty) => write!(f, "{}", ty.ty),
            PointeeTy::Fn(ty) => write!(f, "{}", ty),
        }
    }
}

enum PointeeDefState {
    Defining,
    DefiningWithForward(Word),
    Defined(Word),
}

/// Various type-like things can be converted to a spirv type - normal types, function types, etc. - and this trait
/// provides a uniform way of translating them.
pub trait ConvSpirvType<'tcx> {
    fn spirv_type(&self, span: Span, cx: &CodegenCx<'tcx>) -> Word;
}

impl<'tcx> ConvSpirvType<'tcx> for PointeeTy<'tcx> {
    fn spirv_type(&self, span: Span, cx: &CodegenCx<'tcx>) -> Word {
        match *self {
            PointeeTy::Ty(ty) => ty.spirv_type(span, cx),
            PointeeTy::Fn(ty) => cx
                .fn_abi_of_fn_ptr(ty, ty::List::empty())
                .spirv_type(span, cx),
        }
    }
}

impl<'tcx> ConvSpirvType<'tcx> for FnAbi<'tcx, Ty<'tcx>> {
    fn spirv_type(&self, span: Span, cx: &CodegenCx<'tcx>) -> Word {
        // FIXME(eddyb) use `AccumulateVec`s just like `rustc` itself does.
        let mut argument_types = Vec::new();

        let return_type = match self.ret.mode {
            PassMode::Ignore => SpirvType::Void.def(span, cx),
            PassMode::Direct(_) | PassMode::Pair(..) => self.ret.layout.spirv_type(span, cx),
            PassMode::Cast(_, _) | PassMode::Indirect { .. } => span_bug!(
                span,
                "query hooks should've made this `PassMode` impossible: {:#?}",
                self.ret
            ),
        };

        for arg in self.args.iter() {
            let arg_type = match arg.mode {
                PassMode::Ignore => continue,
                PassMode::Direct(_) => arg.layout.spirv_type(span, cx),
                PassMode::Pair(_, _) => {
                    argument_types.push(scalar_pair_element_backend_type(cx, span, arg.layout, 0));
                    argument_types.push(scalar_pair_element_backend_type(cx, span, arg.layout, 1));
                    continue;
                }
                PassMode::Cast(_, _) | PassMode::Indirect { .. } => span_bug!(
                    span,
                    "query hooks should've made this `PassMode` impossible: {:#?}",
                    arg
                ),
            };
            argument_types.push(arg_type);
        }

        SpirvType::Function {
            return_type,
            arguments: &argument_types,
        }
        .def(span, cx)
    }
}

impl<'tcx> ConvSpirvType<'tcx> for TyAndLayout<'tcx> {
    fn spirv_type(&self, mut span: Span, cx: &CodegenCx<'tcx>) -> Word {
        if let TyKind::Adt(adt, substs) = *self.ty.kind() {
            if span == DUMMY_SP {
                span = cx.tcx.def_span(adt.did());
            }

            let attrs = AggregatedSpirvAttributes::parse(cx, cx.tcx.get_attrs_unchecked(adt.did()));

            if let Some(intrinsic_type_attr) = attrs.intrinsic_type.map(|attr| attr.value) {
                if let Ok(spirv_type) =
                    trans_intrinsic_type(cx, span, *self, substs, intrinsic_type_attr)
                {
                    return spirv_type;
                }
            }
        }

        // Note: ty.layout is orthogonal to ty.ty, e.g. `ManuallyDrop<Result<isize, isize>>` has abi
        // `ScalarPair`.
        // There's a few layers that we go through here. First we inspect layout.abi, then if relevant, layout.fields, etc.
        match self.abi {
            Abi::Uninhabited => SpirvType::Adt {
                def_id: def_id_for_spirv_type_adt(*self),
                size: Some(Size::ZERO),
                align: Align::from_bytes(0).unwrap(),
                field_types: &[],
                field_offsets: &[],
                field_names: None,
            }
            .def_with_name(cx, span, TyLayoutNameKey::from(*self)),
            Abi::Scalar(scalar) => trans_scalar(cx, span, *self, scalar, Size::ZERO),
            Abi::ScalarPair(a, b) => {
                // NOTE(eddyb) unlike `Abi::Scalar`'s simpler newtype-unpacking
                // behavior, `Abi::ScalarPair` can be composed in two ways:
                // * two `Abi::Scalar` fields (and any number of ZST fields),
                //   gets handled the same as a `struct { a, b }`, further below
                // * an `Abi::ScalarPair` field (and any number of ZST fields),
                //   which requires more work to allow taking a reference to
                //   that field, and there are two potential approaches:
                //   1. wrapping that field's SPIR-V type in a single-field
                //      `OpTypeStruct` - this has the disadvantage that GEPs
                //      would have to inject an extra `0` field index, and other
                //      field-related operations would also need additional work
                //   2. reusing that field's SPIR-V type, instead of defining
                //      a new one, offering the `(a, b)` shape `rustc_codegen_ssa`
                //      expects, while letting noop pointercasts access the sole
                //      `Abi::ScalarPair` field - this is the approach taken here
                let mut non_zst_fields = (0..self.fields.count())
                    .map(|i| (i, self.field(cx, i)))
                    .filter(|(_, field)| !field.is_zst());
                let sole_non_zst_field = match (non_zst_fields.next(), non_zst_fields.next()) {
                    (Some(field), None) => Some(field),
                    _ => None,
                };
                if let Some((i, field)) = sole_non_zst_field {
                    // Only unpack a newtype if the field and the newtype line up
                    // perfectly, in every way that could potentially affect ABI.
                    if self.fields.offset(i) == Size::ZERO
                        && field.size == self.size
                        && field.align == self.align
                        && field.abi == self.abi
                    {
                        return field.spirv_type(span, cx);
                    }
                }

                // Note: We can't use auto_struct_layout here because the spirv types here might be undefined due to
                // recursive pointer types.
                let a_offset = Size::ZERO;
                let b_offset = a.primitive().size(cx).align_to(b.primitive().align(cx).abi);
                let a = trans_scalar(cx, span, *self, a, a_offset);
                let b = trans_scalar(cx, span, *self, b, b_offset);
                let size = if self.is_unsized() {
                    None
                } else {
                    Some(self.size)
                };
                // FIXME(eddyb) use `ArrayVec` here.
                let mut field_names = Vec::new();
                if let TyKind::Adt(adt, _) = self.ty.kind() {
                    if let Variants::Single { index } = self.variants {
                        for i in self.fields.index_by_increasing_offset() {
                            let field = &adt.variants()[index].fields[i];
                            field_names.push(field.name);
                        }
                    }
                }
                SpirvType::Adt {
                    def_id: def_id_for_spirv_type_adt(*self),
                    size,
                    align: self.align.abi,
                    field_types: &[a, b],
                    field_offsets: &[a_offset, b_offset],
                    field_names: if field_names.len() == 2 {
                        Some(&field_names)
                    } else {
                        None
                    },
                }
                .def_with_name(cx, span, TyLayoutNameKey::from(*self))
            }
            Abi::Vector { element, count } => {
                let elem_spirv = trans_scalar(cx, span, *self, element, Size::ZERO);
                SpirvType::Vector {
                    element: elem_spirv,
                    count: count as u32,
                }
                .def(span, cx)
            }
            Abi::Aggregate { sized: _ } => trans_aggregate(cx, span, *self),
        }
    }
}

/// Only pub for `LayoutTypeMethods::scalar_pair_element_backend_type`. Think about what you're
/// doing before calling this.
pub fn scalar_pair_element_backend_type<'tcx>(
    cx: &CodegenCx<'tcx>,
    span: Span,
    ty: TyAndLayout<'tcx>,
    index: usize,
) -> Word {
    let [a, b] = match ty.layout.abi() {
        Abi::ScalarPair(a, b) => [a, b],
        other => span_bug!(
            span,
            "scalar_pair_element_backend_type invalid abi: {:?}",
            other
        ),
    };
    let offset = match index {
        0 => Size::ZERO,
        1 => a.primitive().size(cx).align_to(b.primitive().align(cx).abi),
        _ => unreachable!(),
    };
    trans_scalar(cx, span, ty, [a, b][index], offset)
}

/// A "scalar" is a basic building block: bools, ints, floats, pointers. (i.e. not something complex like a struct)
/// A "scalar pair" is a bit of a strange concept: if there is a `fn f(x: (u32, u32))`, then what's preferred for
/// performance is to compile that ABI to `f(x_1: u32, x_2: u32)`, i.e. splitting out the pair into their own arguments,
/// and pretending that they're one unit. So, there's quite a bit of special handling around these scalar pairs to enable
/// scenarios like that.
/// I say it's "preferred", but spirv doesn't really care - only CPU ABIs really care here. However, following rustc's
/// lead and doing what they want makes things go smoothly, so we'll implement it here too.
fn trans_scalar<'tcx>(
    cx: &CodegenCx<'tcx>,
    span: Span,
    ty: TyAndLayout<'tcx>,
    scalar: Scalar,
    offset: Size,
) -> Word {
    if scalar.is_bool() {
        return SpirvType::Bool.def(span, cx);
    }

    match scalar.primitive() {
        Primitive::Int(width, signedness) => {
            SpirvType::Integer(width.size().bits() as u32, signedness).def(span, cx)
        }
        Primitive::F32 => SpirvType::Float(32).def(span, cx),
        Primitive::F64 => SpirvType::Float(64).def(span, cx),
        Primitive::Pointer => {
            let pointee_ty = dig_scalar_pointee(cx, ty, offset);
            // Pointers can be recursive. So, record what we're currently translating, and if we're already translating
            // the same type, emit an OpTypeForwardPointer and use that ID.
            if let Some(predefined_result) = cx
                .type_cache
                .recursive_pointee_cache
                .begin(cx, span, pointee_ty)
            {
                predefined_result
            } else {
                let pointee = pointee_ty.spirv_type(span, cx);
                cx.type_cache
                    .recursive_pointee_cache
                    .end(cx, span, pointee_ty, pointee)
            }
        }
    }
}

// This is a really weird function, strap in...
// So, rustc_codegen_ssa is designed around scalar pointers being opaque, you shouldn't know the type behind the
// pointer. Unfortunately, that's impossible for us, we need to know the underlying pointee type for various reasons. In
// some cases, this is pretty easy - if it's a TyKind::Ref, then the pointee will be the pointee of the ref (with
// handling for wide pointers, etc.). Unfortunately, there's some pretty advanced processing going on in cx.layout_of:
// for example, `ManuallyDrop<Result<ptr, ptr>>` has abi `ScalarPair`. This means that to figure out the pointee type,
// we have to replicate the logic of cx.layout_of. Part of that is digging into types that are aggregates: for example,
// ManuallyDrop<T> has a single field of type T. We "dig into" that field, and recurse, trying to find a base case that
// we can handle, like TyKind::Ref.
// If the above didn't make sense, please poke Ashley, it's probably easier to explain via conversation.
fn dig_scalar_pointee<'tcx>(
    cx: &CodegenCx<'tcx>,
    layout: TyAndLayout<'tcx>,
    offset: Size,
) -> PointeeTy<'tcx> {
    if let FieldsShape::Primitive = layout.fields {
        assert_eq!(offset, Size::ZERO);
        let pointee = match *layout.ty.kind() {
            TyKind::Ref(_, pointee_ty, _) | TyKind::RawPtr(TypeAndMut { ty: pointee_ty, .. }) => {
                PointeeTy::Ty(cx.layout_of(pointee_ty))
            }
            TyKind::FnPtr(sig) => PointeeTy::Fn(sig),
            _ => bug!("Pointer is not `&T`, `*T` or `fn` pointer: {:#?}", layout),
        };
        return pointee;
    }

    let all_fields = (match &layout.variants {
        Variants::Multiple { variants, .. } => 0..variants.len(),
        Variants::Single { index } => {
            let i = index.as_usize();
            i..i + 1
        }
    })
    .flat_map(|variant_idx| {
        let variant = layout.for_variant(cx, VariantIdx::new(variant_idx));
        (0..variant.fields.count()).map(move |field_idx| {
            (
                variant.field(cx, field_idx),
                variant.fields.offset(field_idx),
            )
        })
    });

    let mut pointee = None;
    for (field, field_offset) in all_fields {
        if field.is_zst() {
            continue;
        }
        if (field_offset..field_offset + field.size).contains(&offset) {
            let new_pointee = dig_scalar_pointee(cx, field, offset - field_offset);
            match pointee {
                Some(old_pointee) if old_pointee != new_pointee => {
                    cx.tcx.sess.fatal(format!(
                        "dig_scalar_pointee: unsupported Pointer with different \
                         pointee types ({:?} vs {:?}) at offset {:?} in {:#?}",
                        old_pointee, new_pointee, offset, layout
                    ));
                }
                _ => pointee = Some(new_pointee),
            }
        }
    }
    pointee.unwrap_or_else(|| {
        bug!(
            "field containing Pointer scalar at offset {:?} not found in {:#?}",
            offset,
            layout
        )
    })
}

fn trans_aggregate<'tcx>(cx: &CodegenCx<'tcx>, span: Span, ty: TyAndLayout<'tcx>) -> Word {
    fn create_zst<'tcx>(cx: &CodegenCx<'tcx>, span: Span, ty: TyAndLayout<'tcx>) -> Word {
        SpirvType::Adt {
            def_id: def_id_for_spirv_type_adt(ty),
            size: Some(Size::ZERO),
            align: Align::from_bytes(0).unwrap(),
            field_types: &[],
            field_offsets: &[],
            field_names: None,
        }
        .def_with_name(cx, span, TyLayoutNameKey::from(ty))
    }
    match ty.fields {
        FieldsShape::Primitive => span_bug!(
            span,
            "trans_aggregate called for FieldsShape::Primitive layout {:#?}",
            ty
        ),
        FieldsShape::Union(_) => {
            assert!(!ty.is_unsized(), "{:#?}", ty);
            if ty.size.bytes() == 0 {
                create_zst(cx, span, ty)
            } else {
                let byte = SpirvType::Integer(8, false).def(span, cx);
                let count = cx.constant_u32(span, ty.size.bytes() as u32);
                SpirvType::Array {
                    element: byte,
                    count,
                }
                .def(span, cx)
            }
        }
        FieldsShape::Array { stride, count } => {
            let element_type = ty.field(cx, 0).spirv_type(span, cx);
            if ty.is_unsized() {
                // There's a potential for this array to be sized, but the element to be unsized, e.g. `[[u8]; 5]`.
                // However, I think rust disallows all these cases, so assert this here.
                assert_eq!(count, 0);
                SpirvType::RuntimeArray {
                    element: element_type,
                }
                .def(span, cx)
            } else if count == 0 {
                // spir-v doesn't support zero-sized arrays
                create_zst(cx, span, ty)
            } else {
                let count_const = cx.constant_u32(span, count as u32);
                let element_spv = cx.lookup_type(element_type);
                let stride_spv = element_spv
                    .sizeof(cx)
                    .expect("Unexpected unsized type in sized FieldsShape::Array")
                    .align_to(element_spv.alignof(cx));
                assert_eq!(stride_spv, stride);
                SpirvType::Array {
                    element: element_type,
                    count: count_const,
                }
                .def(span, cx)
            }
        }
        FieldsShape::Arbitrary {
            offsets: _,
            memory_index: _,
        } => trans_struct(cx, span, ty),
    }
}

// returns (field_offsets, size, align)
pub fn auto_struct_layout<'tcx>(
    cx: &CodegenCx<'tcx>,
    field_types: &[Word],
) -> (Vec<Size>, Option<Size>, Align) {
    // FIXME(eddyb) use `AccumulateVec`s just like `rustc` itself does.
    let mut field_offsets = Vec::with_capacity(field_types.len());
    let mut offset = Some(Size::ZERO);
    let mut max_align = Align::from_bytes(0).unwrap();
    for &field_type in field_types {
        let spirv_type = cx.lookup_type(field_type);
        let field_size = spirv_type.sizeof(cx);
        let field_align = spirv_type.alignof(cx);
        let this_offset = offset
            .expect("Unsized values can only be the last field in a struct")
            .align_to(field_align);

        field_offsets.push(this_offset);
        if field_align > max_align {
            max_align = field_align;
        }
        offset = field_size.map(|size| this_offset + size);
    }
    (field_offsets, offset, max_align)
}

// see struct_llfields in librustc_codegen_llvm for implementation hints
fn trans_struct<'tcx>(cx: &CodegenCx<'tcx>, span: Span, ty: TyAndLayout<'tcx>) -> Word {
    let size = if ty.is_unsized() { None } else { Some(ty.size) };
    let align = ty.align.abi;
    // FIXME(eddyb) use `AccumulateVec`s just like `rustc` itself does.
    let mut field_types = Vec::new();
    let mut field_offsets = Vec::new();
    let mut field_names = Vec::new();
    for i in ty.fields.index_by_increasing_offset() {
        let field_ty = ty.field(cx, i);
        field_types.push(field_ty.spirv_type(span, cx));
        let offset = ty.fields.offset(i);
        field_offsets.push(offset);
        if let Variants::Single { index } = ty.variants {
            if let TyKind::Adt(adt, _) = ty.ty.kind() {
                let field = &adt.variants()[index].fields[i];
                field_names.push(field.name);
            } else {
                // FIXME(eddyb) this looks like something that should exist in rustc.
                field_names.push(Symbol::intern(&format!("{i}")));
            }
        } else {
            if let TyKind::Adt(_, _) = ty.ty.kind() {
            } else {
                span_bug!(span, "Variants::Multiple not TyKind::Adt");
            }
            if i == 0 {
                field_names.push(cx.sym.discriminant);
            } else {
                cx.tcx.sess.fatal("Variants::Multiple has multiple fields")
            }
        };
    }
    SpirvType::Adt {
        def_id: def_id_for_spirv_type_adt(ty),
        size,
        align,
        field_types: &field_types,
        field_offsets: &field_offsets,
        field_names: Some(&field_names),
    }
    .def_with_name(cx, span, TyLayoutNameKey::from(ty))
}

/// Grab a `DefId` from the type if possible to avoid too much deduplication,
/// which could result in one SPIR-V `OpType*` having many names
/// (not in itself an issue, but it makes error reporting harder).
fn def_id_for_spirv_type_adt(layout: TyAndLayout<'_>) -> Option<DefId> {
    match *layout.ty.kind() {
        TyKind::Adt(def, _) => Some(def.did()),
        TyKind::Foreign(def_id) | TyKind::Closure(def_id, _) | TyKind::Generator(def_id, ..) => {
            Some(def_id)
        }
        _ => None,
    }
}

/// Minimal and cheaply comparable/hashable subset of the information contained
/// in `TyLayout` that can be used to generate a name (assuming a nominal type).
#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct TyLayoutNameKey<'tcx> {
    ty: Ty<'tcx>,
    variant: Option<VariantIdx>,
}

impl<'tcx> From<TyAndLayout<'tcx>> for TyLayoutNameKey<'tcx> {
    fn from(layout: TyAndLayout<'tcx>) -> Self {
        TyLayoutNameKey {
            ty: layout.ty,
            variant: match layout.variants {
                Variants::Single { index } => Some(index),
                Variants::Multiple { .. } => None,
            },
        }
    }
}

impl fmt::Display for TyLayoutNameKey<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.ty)?;
        if let (TyKind::Adt(def, _), Some(index)) = (self.ty.kind(), self.variant) {
            if def.is_enum() && !def.variants().is_empty() {
                write!(f, "::{}", def.variants()[index].name)?;
            }
        }
        if let (TyKind::Generator(_, _, _), Some(index)) = (self.ty.kind(), self.variant) {
            write!(f, "::{}", GeneratorSubsts::variant_name(index))?;
        }
        Ok(())
    }
}

fn trans_intrinsic_type<'tcx>(
    cx: &CodegenCx<'tcx>,
    span: Span,
    ty: TyAndLayout<'tcx>,
    substs: SubstsRef<'tcx>,
    intrinsic_type_attr: IntrinsicType,
) -> Result<Word, ErrorGuaranteed> {
    match intrinsic_type_attr {
        IntrinsicType::GenericImageType => {
            // see SpirvType::sizeof
            if ty.size != Size::from_bytes(4) {
                return Err(cx
                    .tcx
                    .sess
                    .err("#[spirv(generic_image)] type must have size 4"));
            }

            // fn type_from_variant_discriminant<'tcx, P: FromPrimitive>(
            //     cx: &CodegenCx<'tcx>,
            //     const_: Const<'tcx>,
            // ) -> P {
            //     let adt_def = const_.ty.ty_adt_def().unwrap();
            //     assert!(adt_def.is_enum());
            //     let destructured = cx.tcx.destructure_const(ParamEnv::reveal_all().and(const_));
            //     let idx = destructured.variant.unwrap();
            //     let value = const_.ty.discriminant_for_variant(cx.tcx, idx).unwrap().val as u64;
            //     <_>::from_u64(value).unwrap()
            // }

            let sampled_type = match substs.type_at(0).kind() {
                TyKind::Int(int) => match int {
                    IntTy::Isize => {
                        SpirvType::Integer(cx.tcx.data_layout.pointer_size.bits() as u32, true)
                            .def(span, cx)
                    }
                    IntTy::I8 => SpirvType::Integer(8, true).def(span, cx),
                    IntTy::I16 => SpirvType::Integer(16, true).def(span, cx),
                    IntTy::I32 => SpirvType::Integer(32, true).def(span, cx),
                    IntTy::I64 => SpirvType::Integer(64, true).def(span, cx),
                    IntTy::I128 => SpirvType::Integer(128, true).def(span, cx),
                },
                TyKind::Uint(uint) => match uint {
                    UintTy::Usize => {
                        SpirvType::Integer(cx.tcx.data_layout.pointer_size.bits() as u32, false)
                            .def(span, cx)
                    }
                    UintTy::U8 => SpirvType::Integer(8, false).def(span, cx),
                    UintTy::U16 => SpirvType::Integer(16, false).def(span, cx),
                    UintTy::U32 => SpirvType::Integer(32, false).def(span, cx),
                    UintTy::U64 => SpirvType::Integer(64, false).def(span, cx),
                    UintTy::U128 => SpirvType::Integer(128, false).def(span, cx),
                },
                TyKind::Float(FloatTy::F32) => SpirvType::Float(32).def(span, cx),
                TyKind::Float(FloatTy::F64) => SpirvType::Float(64).def(span, cx),
                _ => {
                    return Err(cx
                        .tcx
                        .sess
                        .span_err(span, "Invalid sampled type to `Image`."));
                }
            };

            // let dim: spirv::Dim = type_from_variant_discriminant(cx, substs.const_at(1));
            // let depth: u32 = type_from_variant_discriminant(cx, substs.const_at(2));
            // let arrayed: u32 = type_from_variant_discriminant(cx, substs.const_at(3));
            // let multisampled: u32 = type_from_variant_discriminant(cx, substs.const_at(4));
            // let sampled: u32 = type_from_variant_discriminant(cx, substs.const_at(5));
            // let image_format: spirv::ImageFormat =
            //     type_from_variant_discriminant(cx, substs.const_at(6));

            fn const_int_value<'tcx, P: FromPrimitive>(
                cx: &CodegenCx<'tcx>,
                const_: Const<'tcx>,
            ) -> Result<P, ErrorGuaranteed> {
                assert!(const_.ty().is_integral());
                let value = const_.eval_bits(cx.tcx, ParamEnv::reveal_all(), const_.ty());
                match P::from_u128(value) {
                    Some(v) => Ok(v),
                    None => Err(cx
                        .tcx
                        .sess
                        .err(format!("Invalid value for Image const generic: {}", value))),
                }
            }

            let dim = const_int_value(cx, substs.const_at(1))?;
            let depth = const_int_value(cx, substs.const_at(2))?;
            let arrayed = const_int_value(cx, substs.const_at(3))?;
            let multisampled = const_int_value(cx, substs.const_at(4))?;
            let sampled = const_int_value(cx, substs.const_at(5))?;
            let image_format = const_int_value(cx, substs.const_at(6))?;

            let ty = SpirvType::Image {
                sampled_type,
                dim,
                depth,
                arrayed,
                multisampled,
                sampled,
                image_format,
            };
            Ok(ty.def(span, cx))
        }
        IntrinsicType::Sampler => {
            // see SpirvType::sizeof
            if ty.size != Size::from_bytes(4) {
                return Err(cx.tcx.sess.err("#[spirv(sampler)] type must have size 4"));
            }
            Ok(SpirvType::Sampler.def(span, cx))
        }
        IntrinsicType::AccelerationStructureKhr => {
            Ok(SpirvType::AccelerationStructureKhr.def(span, cx))
        }
        IntrinsicType::RayQueryKhr => Ok(SpirvType::RayQueryKhr.def(span, cx)),
        IntrinsicType::SampledImage => {
            // see SpirvType::sizeof
            if ty.size != Size::from_bytes(4) {
                return Err(cx
                    .tcx
                    .sess
                    .err("#[spirv(sampled_image)] type must have size 4"));
            }

            // We use a generic to indicate the underlying image type of the sampled image.
            // The spirv type of it will be generated by querying the type of the first generic.
            if let Some(image_ty) = substs.types().next() {
                // TODO: enforce that the generic param is an image type?
                let image_type = cx.layout_of(image_ty).spirv_type(span, cx);
                Ok(SpirvType::SampledImage { image_type }.def(span, cx))
            } else {
                Err(cx
                    .tcx
                    .sess
                    .err("#[spirv(sampled_image)] type must have a generic image type"))
            }
        }
        IntrinsicType::RuntimeArray => {
            if ty.size != Size::from_bytes(4) {
                return Err(cx
                    .tcx
                    .sess
                    .err("#[spirv(runtime_array)] type must have size 4"));
            }

            // We use a generic to indicate the underlying element type.
            // The spirv type of it will be generated by querying the type of the first generic.
            if let Some(elem_ty) = substs.types().next() {
                let element = cx.layout_of(elem_ty).spirv_type(span, cx);
                Ok(SpirvType::RuntimeArray { element }.def(span, cx))
            } else {
                Err(cx
                    .tcx
                    .sess
                    .err("#[spirv(runtime_array)] type must have a generic element type"))
            }
        }
        IntrinsicType::Matrix => {
            let span = def_id_for_spirv_type_adt(ty)
                .map(|did| cx.tcx.def_span(did))
                .expect("#[spirv(matrix)] must be added to a type which has DefId");

            let field_types = (0..ty.fields.count())
                .map(|i| ty.field(cx, i).spirv_type(span, cx))
                .collect::<Vec<_>>();
            if field_types.len() < 2 {
                return Err(cx
                    .tcx
                    .sess
                    .span_err(span, "#[spirv(matrix)] type must have at least two fields"));
            }
            let elem_type = field_types[0];
            if !field_types.iter().all(|&ty| ty == elem_type) {
                return Err(cx.tcx.sess.span_err(
                    span,
                    "#[spirv(matrix)] type fields must all be the same type",
                ));
            }
            match cx.lookup_type(elem_type) {
                SpirvType::Vector { .. } => (),
                ty => {
                    return Err(cx
                        .tcx
                        .sess
                        .struct_span_err(span, "#[spirv(matrix)] type fields must all be vectors")
                        .note(&format!("field type is {}", ty.debug(elem_type, cx)))
                        .emit());
                }
            }

            Ok(SpirvType::Matrix {
                element: elem_type,
                count: field_types.len() as u32,
            }
            .def(span, cx))
        }
    }
}
