use crate::codegen_cx::CodegenCx;
use crate::spirv_type::SpirvType;
use rspirv::spirv::{StorageClass, Word};
use rustc_middle::ty::layout::{FnAbiExt, TyAndLayout};
use rustc_middle::ty::{GeneratorSubsts, PolyFnSig, Ty, TyKind};
use rustc_target::abi::call::{CastTarget, FnAbi, PassMode, Reg, RegKind};
use rustc_target::abi::{Abi, Align, FieldsShape, LayoutOf, Primitive, Scalar, Size, Variants};
use std::cell::RefCell;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::fmt::Write;

#[derive(Default)]
pub struct RecursivePointeeCache<'tcx> {
    map: RefCell<HashMap<PointeeTy<'tcx>, PointeeDefState>>,
}

impl<'tcx> RecursivePointeeCache<'tcx> {
    fn begin<'spv>(&self, cx: &CodegenCx<'spv, 'tcx>, pointee: PointeeTy<'tcx>) -> Option<Word> {
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
                    // StorageClass will be fixed up later
                    cx.emit_global()
                        .type_forward_pointer(new_id, StorageClass::Generic);
                    entry.insert(PointeeDefState::DefiningWithForward(new_id));
                    Some(new_id)
                }
                // State: This is the third or more time we've seen this type, and we've already emitted an
                // OpTypeForwardPointer. Just use the ID we've already emitted. (Alternatively, we already defined this
                // type, so just use that.)
                PointeeDefState::DefiningWithForward(id) | PointeeDefState::Defined(id) => Some(id),
            },
        }
    }

    fn end<'spv>(
        &self,
        cx: &CodegenCx<'spv, 'tcx>,
        pointee: PointeeTy<'tcx>,
        storage_class: StorageClass,
        pointee_spv: Word,
    ) -> Word {
        match self.map.borrow_mut().entry(pointee) {
            // We should have hit begin() on this type already, which always inserts an entry.
            Entry::Vacant(_) => panic!("RecursivePointeeCache::end should always have entry"),
            Entry::Occupied(mut entry) => match *entry.get() {
                // State: There have been no recursive references to this type while defining it, and so no
                // OpTypeForwardPointer has been emitted. This is the most common case.
                PointeeDefState::Defining => {
                    let id = SpirvType::Pointer {
                        storage_class,
                        pointee: pointee_spv,
                    }
                    .def(cx);
                    entry.insert(PointeeDefState::Defined(id));
                    id
                }
                // State: There was a recursive reference to this type, and so an OpTypeForwardPointer has been emitted.
                // Make sure to use the same ID.
                PointeeDefState::DefiningWithForward(id) => {
                    entry.insert(PointeeDefState::Defined(id));
                    cx.builder.fix_up_pointer_forward(id, storage_class);
                    SpirvType::Pointer {
                        storage_class,
                        pointee: pointee_spv,
                    }
                    .def_with_id(cx, id)
                }
                PointeeDefState::Defined(_) => {
                    panic!("RecursivePointeeCache::end defined pointer twice")
                }
            },
        }
    }
}

#[derive(Eq, PartialEq, Hash, Copy, Clone)]
enum PointeeTy<'tcx> {
    Ty(TyAndLayout<'tcx>),
    Fn(PolyFnSig<'tcx>),
}

enum PointeeDefState {
    Defining,
    DefiningWithForward(Word),
    Defined(Word),
}

pub trait ConvSpirvType<'spv, 'tcx> {
    fn spirv_type(&self, cx: &CodegenCx<'spv, 'tcx>) -> Word;
    fn spirv_type_immediate(&self, cx: &CodegenCx<'spv, 'tcx>) -> Word {
        self.spirv_type(cx)
    }
}

impl<'spv, 'tcx> ConvSpirvType<'spv, 'tcx> for PointeeTy<'tcx> {
    fn spirv_type(&self, cx: &CodegenCx<'spv, 'tcx>) -> Word {
        match *self {
            PointeeTy::Ty(ty) => ty.spirv_type(cx),
            PointeeTy::Fn(ty) => FnAbi::of_fn_ptr(cx, ty, &[]).spirv_type(cx),
        }
    }
    fn spirv_type_immediate(&self, cx: &CodegenCx<'spv, 'tcx>) -> Word {
        match *self {
            PointeeTy::Ty(ty) => ty.spirv_type_immediate(cx),
            PointeeTy::Fn(ty) => FnAbi::of_fn_ptr(cx, ty, &[]).spirv_type_immediate(cx),
        }
    }
}

impl<'spv, 'tcx> ConvSpirvType<'spv, 'tcx> for Reg {
    fn spirv_type(&self, cx: &CodegenCx<'spv, 'tcx>) -> Word {
        match self.kind {
            RegKind::Integer => SpirvType::Integer(self.size.bits() as u32, false).def(cx),
            RegKind::Float => SpirvType::Float(self.size.bits() as u32).def(cx),
            RegKind::Vector => SpirvType::Vector {
                element: SpirvType::Integer(8, false).def(cx),
                count: cx.constant_u32(self.size.bytes() as u32).def,
            }
            .def(cx),
        }
    }
}

impl<'spv, 'tcx> ConvSpirvType<'spv, 'tcx> for CastTarget {
    fn spirv_type(&self, cx: &CodegenCx<'spv, 'tcx>) -> Word {
        let rest_ll_unit = self.rest.unit.spirv_type(cx);
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
                    count: cx.constant_u32(rest_count as u32).def,
                }
                .def(cx);
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
                    size: self.prefix_chunk,
                }
                .spirv_type(cx)
            })
            .chain((0..rest_count).map(|_| rest_ll_unit))
            .collect();

        // Append final integer
        if rem_bytes != 0 {
            // Only integers can be really split further.
            assert_eq!(self.rest.unit.kind, RegKind::Integer);
            args.push(SpirvType::Integer(rem_bytes as u32 * 8, false).def(cx));
        }

        let size = Some(self.size(cx));
        let align = self.align(cx);
        let (field_offsets, computed_size, computed_align) = auto_struct_layout(cx, &args);
        assert_eq!(size, computed_size, "{:#?}", self);
        assert_eq!(align, computed_align, "{:#?}", self);
        SpirvType::Adt {
            name: "<cast_target>".to_string(),
            size,
            align,
            field_types: args,
            field_offsets,
            field_names: None,
        }
        .def(cx)
    }
}

impl<'spv, 'tcx> ConvSpirvType<'spv, 'tcx> for FnAbi<'tcx, Ty<'tcx>> {
    fn spirv_type(&self, cx: &CodegenCx<'spv, 'tcx>) -> Word {
        let mut argument_types = Vec::new();

        let return_type = match self.ret.mode {
            PassMode::Ignore => SpirvType::Void.def(cx),
            PassMode::Direct(_) | PassMode::Pair(..) => self.ret.layout.spirv_type_immediate(cx),
            PassMode::Cast(cast_target) => cast_target.spirv_type(cx),
            PassMode::Indirect(_arg_attributes, wide_ptr_attrs) => {
                if wide_ptr_attrs.is_some() {
                    panic!("TODO: PassMode::Indirect wide ptr not supported for return type");
                }
                let pointee = self.ret.layout.spirv_type(cx);
                let pointer = SpirvType::Pointer {
                    storage_class: StorageClass::Generic,
                    pointee,
                }
                .def(cx);
                // Important: the return pointer comes *first*, not last.
                argument_types.push(pointer);
                SpirvType::Void.def(cx)
            }
        };

        for arg in &self.args {
            let arg_type = match arg.mode {
                PassMode::Ignore => continue,
                PassMode::Direct(_) => trans_type_impl(cx, arg.layout, true),
                PassMode::Pair(_, _) => {
                    argument_types.push(scalar_pair_element_backend_type(cx, arg.layout, 0, true));
                    argument_types.push(scalar_pair_element_backend_type(cx, arg.layout, 1, true));
                    continue;
                }
                PassMode::Cast(cast_target) => cast_target.spirv_type(cx),
                PassMode::Indirect(_, Some(_)) => {
                    let ptr_ty = cx.tcx.mk_mut_ptr(arg.layout.ty);
                    let ptr_layout = cx.layout_of(ptr_ty);
                    argument_types.push(scalar_pair_element_backend_type(cx, ptr_layout, 0, true));
                    argument_types.push(scalar_pair_element_backend_type(cx, ptr_layout, 1, true));
                    continue;
                }
                PassMode::Indirect(_, None) => {
                    let pointee = arg.layout.spirv_type(cx);
                    SpirvType::Pointer {
                        storage_class: StorageClass::Generic,
                        pointee,
                    }
                    .def(cx)
                }
            };
            argument_types.push(arg_type);
        }

        SpirvType::Function {
            return_type,
            arguments: argument_types,
        }
        .def(cx)
    }
}

impl<'spv, 'tcx> ConvSpirvType<'spv, 'tcx> for TyAndLayout<'tcx> {
    fn spirv_type(&self, cx: &CodegenCx<'spv, 'tcx>) -> Word {
        trans_type_impl(cx, *self, false)
    }
    fn spirv_type_immediate(&self, cx: &CodegenCx<'spv, 'tcx>) -> Word {
        trans_type_impl(cx, *self, true)
    }
}

fn trans_type_impl<'spv, 'tcx>(
    cx: &CodegenCx<'spv, 'tcx>,
    ty: TyAndLayout<'tcx>,
    is_immediate: bool,
) -> Word {
    // Note: ty.abi is orthogonal to ty.variants and ty.fields, e.g. `ManuallyDrop<Result<isize, isize>>` has abi
    // `ScalarPair`.
    match ty.abi {
        _ if ty.is_zst() => {
            // An empty struct is zero-sized
            SpirvType::Adt {
                name: "<zst>".to_string(),
                size: Some(Size::ZERO),
                align: Align::from_bytes(0).unwrap(),
                field_types: Vec::new(),
                field_offsets: Vec::new(),
                field_names: None,
            }
            .def(cx)
        }
        Abi::Uninhabited => panic!(
            "TODO: Abi::Uninhabited not supported yet in trans_type: {:?}",
            ty
        ),
        Abi::Scalar(ref scalar) => trans_scalar(cx, ty, scalar, None, is_immediate),
        Abi::ScalarPair(ref one, ref two) => {
            // Note! Do not pass through is_immediate here - they're wrapped in a struct, hence, not immediate.
            let one_spirv = trans_scalar(cx, ty, one, Some(0), false);
            let two_spirv = trans_scalar(cx, ty, two, Some(1), false);
            // TODO: Note: We can't use auto_struct_layout here because the spirv types here might be undefined.
            let one_offset = Size::ZERO;
            let two_offset = one.value.size(cx).align_to(two.value.align(cx).abi);
            let size = if ty.is_unsized() { None } else { Some(ty.size) };
            SpirvType::Adt {
                name: format!("{}", ty.ty),
                size,
                align: ty.align.abi,
                field_types: vec![one_spirv, two_spirv],
                field_offsets: vec![one_offset, two_offset],
                field_names: None,
            }
            .def(cx)
        }
        Abi::Vector { ref element, count } => {
            let elem_spirv = trans_scalar(cx, ty, element, None, is_immediate);
            let count_spv = cx.constant_u32(count as u32);
            SpirvType::Vector {
                element: elem_spirv,
                count: count_spv.def,
            }
            .def(cx)
        }
        Abi::Aggregate { sized: _ } => trans_aggregate(cx, ty),
    }
}

// only pub for LayoutTypeMethods::scalar_pair_element_backend_type
pub fn scalar_pair_element_backend_type<'spv, 'tcx>(
    cx: &CodegenCx<'spv, 'tcx>,
    ty: TyAndLayout<'tcx>,
    index: usize,
    is_immediate: bool,
) -> Word {
    let scalar = match &ty.layout.abi {
        Abi::ScalarPair(a, b) => [a, b][index],
        other => panic!("scalar_pair_element_backend_type invalid abi: {:?}", other),
    };
    trans_scalar(cx, ty, scalar, Some(index), is_immediate)
}

fn trans_scalar<'spv, 'tcx>(
    cx: &CodegenCx<'spv, 'tcx>,
    ty: TyAndLayout<'tcx>,
    scalar: &Scalar,
    index: Option<usize>,
    is_immediate: bool,
) -> Word {
    if is_immediate && scalar.is_bool() {
        return SpirvType::Bool.def(cx);
    }

    match scalar.value {
        // TODO: Do we use scalar.valid_range?
        Primitive::Int(width, signedness) => {
            SpirvType::Integer(width.size().bits() as u32, signedness).def(cx)
        }
        Primitive::F32 => SpirvType::Float(32).def(cx),
        Primitive::F64 => SpirvType::Float(64).def(cx),
        Primitive::Pointer => {
            let pointee_ty = dig_scalar_pointee(cx, ty, index);
            // Pointers can be recursive. So, record what we're currently translating, and if we're already translating
            // the same type, emit an OpTypeForwardPointer and use that ID.
            if let Some(predefined_result) =
                cx.type_cache.recursive_pointee_cache.begin(cx, pointee_ty)
            {
                predefined_result
            } else {
                let pointee = pointee_ty.spirv_type(cx);
                cx.type_cache.recursive_pointee_cache.end(
                    cx,
                    pointee_ty,
                    StorageClass::Generic,
                    pointee,
                )
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
fn dig_scalar_pointee<'spv, 'tcx>(
    cx: &CodegenCx<'spv, 'tcx>,
    ty: TyAndLayout<'tcx>,
    index: Option<usize>,
) -> PointeeTy<'tcx> {
    match *ty.ty.kind() {
        TyKind::Ref(_region, elem_ty, _mutability) => {
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
        TyKind::RawPtr(type_and_mut) => {
            let elem = cx.layout_of(type_and_mut.ty);
            match index {
                None => PointeeTy::Ty(elem),
                Some(index) => {
                    if elem.is_unsized() {
                        dig_scalar_pointee(cx, ty.field(cx, index), None)
                    } else {
                        // Same comment as TyKind::Ref
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
        // TyKind::Tuple(substs) if substs.len() == 1 => {
        //     let item = cx.layout_of(ty.ty.tuple_fields().next().unwrap());
        //     trans_scalar_known_ty(cx, item, scalar, is_immediate)
        // }
        TyKind::Tuple(_) | TyKind::Adt(..) | TyKind::Closure(..) => {
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
                    other => panic!(
                        "Unable to dig scalar pair pointer type: fields length {}",
                        other
                    ),
                },
                None => match fields.len() {
                    1 => dig_scalar_pointee(cx, fields[0], None),
                    other => panic!("Unable to dig scalar pointer type: fields length {}", other),
                },
            }
        }
        ref kind => panic!(
            "TODO: Unimplemented Primitive::Pointer TyKind index={:?} ({:#?}):\n{:#?}",
            index, kind, ty
        ),
    }
}

fn trans_aggregate<'spv, 'tcx>(cx: &CodegenCx<'spv, 'tcx>, ty: TyAndLayout<'tcx>) -> Word {
    match ty.fields {
        FieldsShape::Primitive => panic!(
            "FieldsShape::Primitive not supported yet in trans_type: {:?}",
            ty
        ),
        // TODO: Is this the right thing to do?
        FieldsShape::Union(_field_count) => {
            assert_ne!(ty.size.bytes(), 0, "{:#?}", ty);
            assert!(!ty.is_unsized(), "{:#?}", ty);
            let byte = SpirvType::Integer(8, false).def(cx);
            let count = cx.constant_u32(ty.size.bytes() as u32).def;
            SpirvType::Array {
                element: byte,
                count,
            }
            .def(cx)
        }
        FieldsShape::Array { stride: _, count } => {
            let element_type = trans_type_impl(cx, ty.field(cx, 0), false);
            if ty.is_unsized() {
                // There's a potential for this array to be sized, but the element to be unsized, e.g. `[[u8]; 5]`.
                // However, I think rust disallows all these cases, so assert this here.
                assert_eq!(count, 0);
                SpirvType::RuntimeArray {
                    element: element_type,
                }
                .def(cx)
            } else {
                // note that zero-sized arrays don't report as .is_zst() for some reason? TODO: investigate why
                assert_ne!(
                    count, 0,
                    "spir-v doesn't support zero-sized arrays: {:#?}",
                    ty
                );
                // TODO: Assert stride is same as spirv's stride?
                let count_const = cx.constant_u32(count as u32).def;
                SpirvType::Array {
                    element: element_type,
                    count: count_const,
                }
                .def(cx)
            }
        }
        FieldsShape::Arbitrary {
            offsets: _,
            memory_index: _,
        } => trans_struct(cx, ty),
    }
}

// returns (field_offsets, size, align)
pub fn auto_struct_layout<'spv, 'tcx>(
    cx: &CodegenCx<'spv, 'tcx>,
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
fn trans_struct<'spv, 'tcx>(cx: &CodegenCx<'spv, 'tcx>, ty: TyAndLayout<'tcx>) -> Word {
    let name = name_of_struct(ty);
    if let TyKind::Foreign(_) = ty.ty.kind() {
        // "An unsized FFI type that is opaque to Rust"
        return SpirvType::Opaque { name }.def(cx);
    };
    let size = if ty.is_unsized() { None } else { Some(ty.size) };
    let align = ty.align.abi;
    let mut field_types = Vec::new();
    let mut field_offsets = Vec::new();
    let mut field_names = Vec::new();
    for i in ty.fields.index_by_increasing_offset() {
        let field_ty = ty.field(cx, i);
        field_types.push(trans_type_impl(cx, field_ty, false));
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
                panic!("Variants::Multiple not supported for non-TyKind::Adt");
            }
            if i == 0 {
                field_names.push("discriminant".to_string());
            } else {
                panic!("Variants::Multiple has multiple fields")
            }
        };
    }
    SpirvType::Adt {
        name,
        size,
        align,
        field_types,
        field_offsets,
        field_names: Some(field_names),
    }
    .def(cx)
}

fn name_of_struct(ty: TyAndLayout<'_>) -> String {
    let mut name = ty.ty.to_string();
    if let (&TyKind::Adt(def, _), &Variants::Single { index }) = (ty.ty.kind(), &ty.variants) {
        if def.is_enum() && !def.variants.is_empty() {
            write!(&mut name, "::{}", def.variants[index].ident).unwrap();
        }
    }
    if let (&TyKind::Generator(_, _, _), &Variants::Single { index }) = (ty.ty.kind(), &ty.variants)
    {
        write!(&mut name, "::{}", GeneratorSubsts::variant_name(index)).unwrap();
    }
    name
}
