//! This file is responsible for translation from rustc tys (`TyAndLayout`) to spir-v types. It's
//! surprisingly difficult.

use crate::attr::{AggregatedSpirvAttributes, IntrinsicType};
use crate::codegen_cx::CodegenCx;
use crate::spirv_type::SpirvType;
use rspirv::spirv::{Capability, StorageClass, Word};
use rustc_errors::ErrorReported;
use rustc_middle::bug;
use rustc_middle::ty::layout::{FnAbiExt, TyAndLayout};
use rustc_middle::ty::subst::SubstsRef;
use rustc_middle::ty::{GeneratorSubsts, PolyFnSig, Ty, TyKind, TypeAndMut};
use rustc_span::def_id::DefId;
use rustc_span::Span;
use rustc_target::abi::call::{CastTarget, FnAbi, PassMode, Reg, RegKind};
use rustc_target::abi::{
    Abi, Align, FieldsShape, LayoutOf, Primitive, Scalar, Size, TagEncoding, VariantIdx, Variants,
};
use std::cell::RefCell;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::fmt;

/// If a struct contains a pointer to itself, even indirectly, then doing a naiive recursive walk
/// of the fields will result in an infinite loop. Because pointers are the only thing that are
/// allowed to be recursive, keep track of what pointers we've translated, or are currently in the
/// progress of translating, and break the recursion that way. This struct manages that state
/// tracking.
#[derive(Default)]
pub struct RecursivePointeeCache<'tcx> {
    map: RefCell<HashMap<PointeeTy<'tcx>, PointeeDefState>>,
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
                    if !cx.builder.has_capability(Capability::Addresses)
                        && !cx
                            .builder
                            .has_capability(Capability::PhysicalStorageBufferAddresses)
                    {
                        cx.zombie_with_span(
                            new_id,
                            span,
                            "OpTypeForwardPointer without OpCapability \
                            Addresses or PhysicalStorageBufferAddresses",
                        );
                    }
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
            Entry::Vacant(_) => bug!("RecursivePointeeCache::end should always have entry"),
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
                    bug!("RecursivePointeeCache::end defined pointer twice")
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
    /// spirv (and llvm) do not allow storing booleans in memory, they are abstract unsized values.
    /// So, if we're dealing with a "memory type", convert bool to u8. The opposite is an
    /// "immediate type", which keeps bools as bools. See also the functions `from_immediate` and
    /// `to_immediate`, which convert between the two.
    fn spirv_type_immediate(&self, span: Span, cx: &CodegenCx<'tcx>) -> Word {
        self.spirv_type(span, cx)
    }
}

impl<'tcx> ConvSpirvType<'tcx> for PointeeTy<'tcx> {
    fn spirv_type(&self, span: Span, cx: &CodegenCx<'tcx>) -> Word {
        match *self {
            PointeeTy::Ty(ty) => ty.spirv_type(span, cx),
            PointeeTy::Fn(ty) => FnAbi::of_fn_ptr(cx, ty, &[]).spirv_type(span, cx),
        }
    }
    fn spirv_type_immediate(&self, span: Span, cx: &CodegenCx<'tcx>) -> Word {
        match *self {
            PointeeTy::Ty(ty) => ty.spirv_type_immediate(span, cx),
            PointeeTy::Fn(ty) => FnAbi::of_fn_ptr(cx, ty, &[]).spirv_type_immediate(span, cx),
        }
    }
}

impl<'tcx> ConvSpirvType<'tcx> for Reg {
    fn spirv_type(&self, span: Span, cx: &CodegenCx<'tcx>) -> Word {
        match self.kind {
            RegKind::Integer => SpirvType::Integer(self.size.bits() as u32, false).def(span, cx),
            RegKind::Float => SpirvType::Float(self.size.bits() as u32).def(span, cx),
            RegKind::Vector => SpirvType::Vector {
                element: SpirvType::Integer(8, false).def(span, cx),
                count: self.size.bytes() as u32,
            }
            .def(span, cx),
        }
    }
}

impl<'tcx> ConvSpirvType<'tcx> for CastTarget {
    fn spirv_type(&self, span: Span, cx: &CodegenCx<'tcx>) -> Word {
        let rest_ll_unit = self.rest.unit.spirv_type(span, cx);
        let (rest_count, rem_bytes) = if self.rest.unit.size.bytes() == 0 {
            (0, 0)
        } else {
            (
                self.rest.total.bytes() / self.rest.unit.size.bytes(),
                self.rest.total.bytes() % self.rest.unit.size.bytes(),
            )
        };

        if self.prefix.iter().all(|x| x.is_none()) {
            // Simplify to a single unit when there is no prefix and size <= unit size
            if self.rest.total <= self.rest.unit.size {
                return rest_ll_unit;
            }

            // Simplify to array when all chunks are the same size and type
            if rem_bytes == 0 {
                return SpirvType::Array {
                    element: rest_ll_unit,
                    count: cx.constant_u32(span, rest_count as u32),
                }
                .def(span, cx);
            }
        }

        // Create list of fields in the main structure
        let mut args: Vec<_> = self
            .prefix
            .iter()
            .flatten()
            .map(|&kind| {
                Reg {
                    kind,
                    size: self.prefix_chunk_size,
                }
                .spirv_type(span, cx)
            })
            .chain((0..rest_count).map(|_| rest_ll_unit))
            .collect();

        // Append final integer
        if rem_bytes != 0 {
            // Only integers can be really split further.
            assert_eq!(self.rest.unit.kind, RegKind::Integer);
            args.push(SpirvType::Integer(rem_bytes as u32 * 8, false).def(span, cx));
        }

        let size = Some(self.size(cx));
        let align = self.align(cx);
        let (field_offsets, computed_size, computed_align) = auto_struct_layout(cx, &args);
        assert_eq!(size, computed_size, "{:#?}", self);
        assert_eq!(align, computed_align, "{:#?}", self);
        SpirvType::Adt {
            def_id: None,
            size,
            align,
            field_types: args,
            field_offsets,
            field_names: None,
            is_block: false,
        }
        .def(span, cx)
    }
}

impl<'tcx> ConvSpirvType<'tcx> for FnAbi<'tcx, Ty<'tcx>> {
    fn spirv_type(&self, span: Span, cx: &CodegenCx<'tcx>) -> Word {
        let mut argument_types = Vec::new();

        let return_type = match self.ret.mode {
            PassMode::Ignore => SpirvType::Void.def(span, cx),
            PassMode::Direct(_) | PassMode::Pair(..) => {
                self.ret.layout.spirv_type_immediate(span, cx)
            }
            PassMode::Cast(cast_target) => cast_target.spirv_type(span, cx),
            PassMode::Indirect { .. } => {
                let pointee = self.ret.layout.spirv_type(span, cx);
                let pointer = SpirvType::Pointer { pointee }.def(span, cx);
                // Important: the return pointer comes *first*, not last.
                argument_types.push(pointer);
                SpirvType::Void.def(span, cx)
            }
        };

        for arg in &self.args {
            let arg_type = match arg.mode {
                PassMode::Ignore => continue,
                PassMode::Direct(_) => arg.layout.spirv_type_immediate(span, cx),
                PassMode::Pair(_, _) => {
                    argument_types.push(scalar_pair_element_backend_type(
                        cx, span, arg.layout, 0, true,
                    ));
                    argument_types.push(scalar_pair_element_backend_type(
                        cx, span, arg.layout, 1, true,
                    ));
                    continue;
                }
                PassMode::Cast(cast_target) => cast_target.spirv_type(span, cx),
                PassMode::Indirect {
                    extra_attrs: Some(_),
                    ..
                } => {
                    let ptr_ty = cx.tcx.mk_mut_ptr(arg.layout.ty);
                    let ptr_layout = cx.layout_of(ptr_ty);
                    argument_types.push(scalar_pair_element_backend_type(
                        cx, span, ptr_layout, 0, true,
                    ));
                    argument_types.push(scalar_pair_element_backend_type(
                        cx, span, ptr_layout, 1, true,
                    ));
                    continue;
                }
                PassMode::Indirect {
                    extra_attrs: None, ..
                } => {
                    let pointee = arg.layout.spirv_type(span, cx);
                    SpirvType::Pointer { pointee }.def(span, cx)
                }
            };
            argument_types.push(arg_type);
        }

        SpirvType::Function {
            return_type,
            arguments: argument_types,
        }
        .def(span, cx)
    }
}

impl<'tcx> ConvSpirvType<'tcx> for TyAndLayout<'tcx> {
    fn spirv_type(&self, span: Span, cx: &CodegenCx<'tcx>) -> Word {
        trans_type_impl(cx, span, *self, false)
    }
    fn spirv_type_immediate(&self, span: Span, cx: &CodegenCx<'tcx>) -> Word {
        trans_type_impl(cx, span, *self, true)
    }
}

fn trans_type_impl<'tcx>(
    cx: &CodegenCx<'tcx>,
    span: Span,
    ty: TyAndLayout<'tcx>,
    is_immediate: bool,
) -> Word {
    if let TyKind::Adt(adt, substs) = *ty.ty.kind() {
        let attrs = AggregatedSpirvAttributes::parse(cx, cx.tcx.get_attrs(adt.did));
        if attrs.block.is_some() {
            if !adt.is_struct() {
                cx.tcx.sess.span_err(
                    span,
                    &format!(
                        "`#[spirv(block)]` can only be used on a `struct`, \
                             but `{}` is a `{}`",
                        ty.ty,
                        adt.descr(),
                    ),
                );
            }

            if !matches!(ty.abi, Abi::Aggregate { sized: true }) {
                cx.tcx.sess.span_err(
                    span,
                    &format!(
                        "`#[spirv(block)]` can only be used for `Sized` aggregates, \
                             but `{}` has `Abi::{:?}`",
                        ty.ty, ty.abi,
                    ),
                );
            }

            assert!(matches!(ty.fields, FieldsShape::Arbitrary { .. }));

            return trans_struct(cx, span, ty, true);
        }

        if let Some(intrinsic_type_attr) = attrs.intrinsic_type.map(|attr| attr.value) {
            if let Ok(spirv_type) = trans_intrinsic_type(cx, span, ty, substs, intrinsic_type_attr)
            {
                return spirv_type;
            }
        }
    }
    // Note: ty.layout is orthogonal to ty.ty, e.g. `ManuallyDrop<Result<isize, isize>>` has abi
    // `ScalarPair`.
    // There's a few layers that we go through here. First we inspect layout.abi, then if relevant, layout.fields, etc.
    match ty.abi {
        Abi::Uninhabited => SpirvType::Adt {
            def_id: def_id_for_spirv_type_adt(ty),
            size: Some(Size::ZERO),
            align: Align::from_bytes(0).unwrap(),
            field_types: Vec::new(),
            field_offsets: Vec::new(),
            field_names: None,
            is_block: false,
        }
        .def_with_name(cx, span, TyLayoutNameKey::from(ty)),
        Abi::Scalar(ref scalar) => trans_scalar(cx, span, ty, scalar, None, is_immediate),
        Abi::ScalarPair(ref one, ref two) => {
            // Note! Do not pass through is_immediate here - they're wrapped in a struct, hence, not immediate.
            let one_spirv = trans_scalar(cx, span, ty, one, Some(0), false);
            let two_spirv = trans_scalar(cx, span, ty, two, Some(1), false);
            // Note: We can't use auto_struct_layout here because the spirv types here might be undefined due to
            // recursive pointer types.
            let one_offset = Size::ZERO;
            let two_offset = one.value.size(cx).align_to(two.value.align(cx).abi);
            let size = if ty.is_unsized() { None } else { Some(ty.size) };
            SpirvType::Adt {
                def_id: def_id_for_spirv_type_adt(ty),
                size,
                align: ty.align.abi,
                field_types: vec![one_spirv, two_spirv],
                field_offsets: vec![one_offset, two_offset],
                field_names: None,
                is_block: false,
            }
            .def_with_name(cx, span, TyLayoutNameKey::from(ty))
        }
        Abi::Vector { ref element, count } => {
            let elem_spirv = trans_scalar(cx, span, ty, element, None, false);
            SpirvType::Vector {
                element: elem_spirv,
                count: count as u32,
            }
            .def(span, cx)
        }
        Abi::Aggregate { sized: _ } => trans_aggregate(cx, span, ty),
    }
}

/// Only pub for `LayoutTypeMethods::scalar_pair_element_backend_type`. Think about what you're
/// doing before calling this.
pub fn scalar_pair_element_backend_type<'tcx>(
    cx: &CodegenCx<'tcx>,
    span: Span,
    ty: TyAndLayout<'tcx>,
    index: usize,
    is_immediate: bool,
) -> Word {
    let scalar = match &ty.layout.abi {
        Abi::ScalarPair(a, b) => [a, b][index],
        other => bug!("scalar_pair_element_backend_type invalid abi: {:?}", other),
    };
    trans_scalar(cx, span, ty, scalar, Some(index), is_immediate)
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
    scalar: &Scalar,
    index: Option<usize>,
    is_immediate: bool,
) -> Word {
    if is_immediate && scalar.is_bool() {
        return SpirvType::Bool.def(span, cx);
    }

    match scalar.value {
        Primitive::Int(width, mut signedness) => {
            if cx.kernel_mode {
                signedness = false;
            }
            SpirvType::Integer(width.size().bits() as u32, signedness).def(span, cx)
        }
        Primitive::F32 => SpirvType::Float(32).def(span, cx),
        Primitive::F64 => SpirvType::Float(64).def(span, cx),
        Primitive::Pointer => {
            let pointee_ty = dig_scalar_pointee(cx, ty, index);
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
    ty: TyAndLayout<'tcx>,
    index: Option<usize>,
) -> PointeeTy<'tcx> {
    match *ty.ty.kind() {
        TyKind::Ref(_, elem_ty, _) | TyKind::RawPtr(TypeAndMut { ty: elem_ty, .. }) => {
            let elem = cx.layout_of(elem_ty);
            match index {
                None => PointeeTy::Ty(elem),
                Some(index) => {
                    if elem.is_unsized() {
                        dig_scalar_pointee(cx, ty.field(cx, index), None)
                    } else {
                        // This can sometimes happen in weird cases when going through the Adt case below - an ABI
                        // of ScalarPair could be deduced, but it's actually e.g. a sized pointer followed by some other
                        // completely unrelated type, not a wide pointer. So, translate this as a single scalar, one
                        // component of that ScalarPair.
                        PointeeTy::Ty(elem)
                    }
                }
            }
        }
        TyKind::FnPtr(sig) if index.is_none() => PointeeTy::Fn(sig),
        TyKind::Adt(def, _) if def.is_box() => {
            let ptr_ty = cx.layout_of(cx.tcx.mk_mut_ptr(ty.ty.boxed_ty()));
            dig_scalar_pointee(cx, ptr_ty, index)
        }
        TyKind::Tuple(_) | TyKind::Adt(..) | TyKind::Closure(..) => {
            dig_scalar_pointee_adt(cx, ty, index)
        }
        ref kind => cx.tcx.sess.fatal(&format!(
            "TODO: Unimplemented Primitive::Pointer TyKind index={:?} ({:#?}):\n{:#?}",
            index, kind, ty
        )),
    }
}

fn dig_scalar_pointee_adt<'tcx>(
    cx: &CodegenCx<'tcx>,
    ty: TyAndLayout<'tcx>,
    index: Option<usize>,
) -> PointeeTy<'tcx> {
    match &ty.variants {
        // If it's a Variants::Multiple, then we want to emit the type of the dataful variant, not the type of the
        // discriminant. This is because the discriminant can e.g. have type *mut(), whereas we want the full underlying
        // type, only available in the dataful variant.
        Variants::Multiple {
            tag_encoding,
            tag_field,
            variants,
            ..
        } => {
            match *tag_encoding {
                TagEncoding::Direct => cx.tcx.sess.fatal(&format!(
                    "dig_scalar_pointee_adt Variants::Multiple TagEncoding::Direct makes no sense: {:#?}",
                    ty
                )),
                TagEncoding::Niche { dataful_variant, .. } => {
                    // This *should* be something like Option<&T>: a very simple enum.
                    // TODO: This might not be, if it's a scalar pair?
                    assert_eq!(1, ty.fields.count());
                    assert_eq!(1, variants[dataful_variant].fields.count());
                    if let TyKind::Adt(adt, substs) = ty.ty.kind() {
                        assert_eq!(1, adt.variants[dataful_variant].fields.len());
                        assert_eq!(0, *tag_field);
                        let field_ty = adt.variants[dataful_variant].fields[0].ty(cx.tcx, substs);
                        dig_scalar_pointee(cx, cx.layout_of(field_ty), index)
                    } else {
                        bug!("Variants::Multiple not TyKind::Adt: {:#?}", ty)
                    }
                },
            }
        }
        Variants::Single { .. } => {
            let fields = ty
                .fields
                .index_by_increasing_offset()
                .map(|f| ty.field(cx, f))
                .filter(|f| !f.is_zst())
                .collect::<Vec<_>>();
            match index {
                Some(index) => match fields.len() {
                    1 => dig_scalar_pointee(cx, fields[0], Some(index)),
                    // This case right here is the cause of the comment handling TyKind::Ref.
                    2 => dig_scalar_pointee(cx, fields[index], None),
                    other => cx.tcx.sess.fatal(&format!(
                        "Unable to dig scalar pair pointer type: fields length {}",
                        other
                    )),
                },
                None => match fields.len() {
                    1 => dig_scalar_pointee(cx, fields[0], None),
                    other => cx.tcx.sess.fatal(&format!(
                        "Unable to dig scalar pointer type: fields length {}",
                        other
                    )),
                },
            }
        }
    }
}

fn trans_aggregate<'tcx>(cx: &CodegenCx<'tcx>, span: Span, ty: TyAndLayout<'tcx>) -> Word {
    match ty.fields {
        FieldsShape::Primitive => cx.tcx.sess.fatal(&format!(
            "FieldsShape::Primitive not supported yet in trans_type: {:?}",
            ty
        )),
        FieldsShape::Union(_) => {
            assert_ne!(ty.size.bytes(), 0, "{:#?}", ty);
            assert!(!ty.is_unsized(), "{:#?}", ty);
            let byte = SpirvType::Integer(8, false).def(span, cx);
            let count = cx.constant_u32(span, ty.size.bytes() as u32);
            SpirvType::Array {
                element: byte,
                count,
            }
            .def(span, cx)
        }
        FieldsShape::Array { stride, count } => {
            let element_type = trans_type_impl(cx, span, ty.field(cx, 0), false);
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
                SpirvType::Adt {
                    def_id: def_id_for_spirv_type_adt(ty),
                    size: Some(Size::ZERO),
                    align: Align::from_bytes(0).unwrap(),
                    field_types: Vec::new(),
                    field_offsets: Vec::new(),
                    field_names: None,
                    is_block: false,
                }
                .def_with_name(cx, span, TyLayoutNameKey::from(ty))
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
        } => trans_struct(cx, span, ty, false),
    }
}

// returns (field_offsets, size, align)
pub fn auto_struct_layout<'tcx>(
    cx: &CodegenCx<'tcx>,
    field_types: &[Word],
) -> (Vec<Size>, Option<Size>, Align) {
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
fn trans_struct<'tcx>(
    cx: &CodegenCx<'tcx>,
    span: Span,
    ty: TyAndLayout<'tcx>,
    is_block: bool,
) -> Word {
    if let TyKind::Foreign(_) = ty.ty.kind() {
        // "An unsized FFI type that is opaque to Rust", `extern type A;` (currently unstable)
        if cx.kernel_mode {
            // TODO: This should use the name of the struct as the name. However, names are not stable across crates,
            // e.g. core::fmt::Opaque in one crate and fmt::Opaque in core.
            return SpirvType::Opaque {
                name: "".to_string(),
            }
            .def(span, cx);
        }
        // otherwise fall back
    };
    let size = if ty.is_unsized() { None } else { Some(ty.size) };
    let align = ty.align.abi;
    let mut field_types = Vec::new();
    let mut field_offsets = Vec::new();
    let mut field_names = Vec::new();
    for i in ty.fields.index_by_increasing_offset() {
        let field_ty = ty.field(cx, i);
        field_types.push(trans_type_impl(cx, span, field_ty, false));
        let offset = ty.fields.offset(i);
        field_offsets.push(offset);
        if let Variants::Single { index } = ty.variants {
            if let TyKind::Adt(adt, _) = ty.ty.kind() {
                let field = &adt.variants[index].fields[i];
                field_names.push(field.ident.name.to_ident_string());
            } else {
                field_names.push(format!("{}", i));
            }
        } else {
            if let TyKind::Adt(_, _) = ty.ty.kind() {
            } else {
                bug!("Variants::Multiple not TyKind::Adt");
            }
            if i == 0 {
                field_names.push("discriminant".to_string());
            } else {
                cx.tcx.sess.fatal("Variants::Multiple has multiple fields")
            }
        };
    }
    SpirvType::Adt {
        def_id: def_id_for_spirv_type_adt(ty),
        size,
        align,
        field_types,
        field_offsets,
        field_names: Some(field_names),
        is_block,
    }
    .def_with_name(cx, span, TyLayoutNameKey::from(ty))
}

/// Grab a `DefId` from the type if possible to avoid too much deduplication,
/// which could result in one SPIR-V `OpType*` having many names
/// (not in itself an issue, but it makes error reporting harder).
fn def_id_for_spirv_type_adt(layout: TyAndLayout<'_>) -> Option<DefId> {
    match *layout.ty.kind() {
        TyKind::Adt(def, _) => Some(def.did),
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
            if def.is_enum() && !def.variants.is_empty() {
                write!(f, "::{}", def.variants[index].ident)?;
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
) -> Result<Word, ErrorReported> {
    match intrinsic_type_attr {
        IntrinsicType::ImageType {
            dim,
            depth,
            arrayed,
            multisampled,
            sampled,
            image_format,
            access_qualifier,
        } => {
            // see SpirvType::sizeof
            if ty.size != Size::from_bytes(4) {
                cx.tcx
                    .sess
                    .err("#[spirv(image_type)] type must have size 4");
                return Err(ErrorReported);
            }
            // Hardcode to float for now
            let sampled_type = SpirvType::Float(32).def(span, cx);
            let ty = SpirvType::Image {
                sampled_type,
                dim,
                depth,
                arrayed,
                multisampled,
                sampled,
                image_format,
                access_qualifier,
            };
            Ok(ty.def(span, cx))
        }
        IntrinsicType::Sampler => {
            // see SpirvType::sizeof
            if ty.size != Size::from_bytes(4) {
                cx.tcx.sess.err("#[spirv(sampler)] type must have size 4");
                return Err(ErrorReported);
            }
            Ok(SpirvType::Sampler.def(span, cx))
        }
        IntrinsicType::SampledImage => {
            // see SpirvType::sizeof
            if ty.size != Size::from_bytes(4) {
                cx.tcx
                    .sess
                    .err("#[spirv(sampled_image)] type must have size 4");
                return Err(ErrorReported);
            }

            // We use a generic to indicate the underlying image type of the sampled image.
            // The spirv type of it will be generated by querying the type of the first generic.
            if let Some(image_ty) = substs.types().next() {
                // TODO: enforce that the generic param is an image type?
                let image_type = trans_type_impl(cx, span, cx.layout_of(image_ty), false);
                Ok(SpirvType::SampledImage { image_type }.def(span, cx))
            } else {
                cx.tcx
                    .sess
                    .err("#[spirv(sampled_image)] type must have a generic image type");
                Err(ErrorReported)
            }
        }
    }
}
