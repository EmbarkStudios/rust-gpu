use crate::codegen_cx::CodegenCx;
use crate::spirv_type::SpirvType;
use rspirv::spirv::{StorageClass, Word};
use rustc_middle::ty::layout::{FnAbiExt, TyAndLayout};
use rustc_middle::ty::{GeneratorSubsts, Ty, TyKind};
use rustc_target::abi::call::{CastTarget, FnAbi, PassMode, Reg, RegKind};
use rustc_target::abi::{Abi, Align, FieldsShape, LayoutOf, Primitive, Scalar, Size, Variants};
use std::fmt::Write;

pub trait ConvSpirvType<'spv, 'tcx> {
    fn spirv_type(&self, cx: &CodegenCx<'spv, 'tcx>) -> Word;
    fn spirv_type_immediate(&self, cx: &CodegenCx<'spv, 'tcx>) -> Word {
        self.spirv_type(cx)
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

        let (field_offsets, _, _) = auto_struct_layout(cx, &args);
        SpirvType::Adt {
            name: "<cast_target>".to_string(),
            size: Some(self.size(cx)),
            align: self.align(cx),
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
            // TODO: Deal with wide ptr?
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
                PassMode::Direct(_) => arg.layout.spirv_type_immediate(cx),
                PassMode::Pair(_, _) => {
                    argument_types.push(trans_scalar_pair(cx, &arg.layout, 0, true));
                    argument_types.push(trans_scalar_pair(cx, &arg.layout, 1, true));
                    continue;
                }
                PassMode::Cast(cast_target) => cast_target.spirv_type(cx),
                PassMode::Indirect(_, Some(_)) => {
                    let ptr_ty = cx.tcx.mk_mut_ptr(arg.layout.ty);
                    let ptr_layout = cx.layout_of(ptr_ty);
                    argument_types.push(trans_scalar_pair(cx, &ptr_layout, 0, true));
                    argument_types.push(trans_scalar_pair(cx, &ptr_layout, 1, true));
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
        trans_type_impl(cx, self, false)
    }
    fn spirv_type_immediate(&self, cx: &CodegenCx<'spv, 'tcx>) -> Word {
        trans_type_impl(cx, self, true)
    }
}

fn trans_type_impl<'spv, 'tcx>(
    cx: &CodegenCx<'spv, 'tcx>,
    ty: &TyAndLayout<'tcx>,
    is_immediate: bool,
) -> Word {
    if ty.is_zst() {
        // An empty struct is zero-sized
        return SpirvType::Adt {
            name: "<zst>".to_string(),
            size: Some(Size::ZERO),
            align: Align::from_bytes(0).unwrap(),
            field_types: Vec::new(),
            field_offsets: Vec::new(),
            field_names: None,
        }
        .def(cx);
    }

    // Note: ty.abi is orthogonal to ty.variants and ty.fields, e.g. `ManuallyDrop<Result<isize, isize>>`
    // has abi `ScalarPair`.
    match ty.abi {
        Abi::Uninhabited => panic!(
            "TODO: Abi::Uninhabited not supported yet in trans_type: {:?}",
            ty
        ),
        Abi::Scalar(ref scalar) => trans_scalar_known_ty(cx, ty, scalar, is_immediate),
        Abi::ScalarPair(ref one, ref two) => {
            // Note! Do not pass through is_immediate here - they're wrapped in a struct, hence, not immediate.
            let one_spirv = trans_scalar_pair_impl(cx, ty, one, 0, false);
            let two_spirv = trans_scalar_pair_impl(cx, ty, two, 1, false);
            let (field_offsets, _, _) = auto_struct_layout(cx, &[one_spirv, two_spirv]);
            let size = if ty.is_unsized() { None } else { Some(ty.size) };
            SpirvType::Adt {
                name: format!("{}", ty.ty),
                size,
                align: ty.align.abi,
                field_types: vec![one_spirv, two_spirv],
                field_offsets,
                field_names: None,
            }
            .def(cx)
        }
        Abi::Vector { ref element, count } => {
            let elem_spirv = trans_scalar_known_ty(cx, ty, element, is_immediate);
            SpirvType::Vector {
                element: elem_spirv,
                count: count as u32,
            }
            .def(cx)
        }
        Abi::Aggregate { sized: _ } => trans_aggregate(cx, ty),
    }
}

fn trans_scalar_known_ty<'spv, 'tcx>(
    cx: &CodegenCx<'spv, 'tcx>,
    ty: &TyAndLayout<'tcx>,
    scalar: &Scalar,
    is_immediate: bool,
) -> Word {
    if scalar.value == Primitive::Pointer {
        match ty.ty.kind {
            TyKind::Ref(_region, elem, _mutability) => {
                let pointee = cx.layout_of(elem).spirv_type(cx);
                SpirvType::Pointer {
                    storage_class: StorageClass::Generic,
                    pointee,
                }
                .def(cx)
            }
            TyKind::RawPtr(type_and_mut) => {
                let pointee = cx.layout_of(type_and_mut.ty).spirv_type(cx);
                SpirvType::Pointer {
                    storage_class: StorageClass::Generic,
                    pointee,
                }
                .def(cx)
            }
            TyKind::FnPtr(sig) => {
                let function = FnAbi::of_fn_ptr(cx, sig, &[]).spirv_type(cx);
                SpirvType::Pointer {
                    storage_class: StorageClass::Generic,
                    pointee: function,
                }
                .def(cx)
            }
            TyKind::Adt(def, _) if def.is_box() => {
                let ptr_ty = cx.layout_of(cx.tcx.mk_mut_ptr(ty.ty.boxed_ty()));
                ptr_ty.spirv_type(cx)
            }
            TyKind::Tuple(substs) if substs.len() == 1 => {
                let item = cx.layout_of(ty.ty.tuple_fields().next().unwrap());
                trans_scalar_known_ty(cx, &item, scalar, is_immediate)
            }
            TyKind::Adt(..) | TyKind::Closure(..) => {
                trans_scalar_pointer_struct(cx, ty, scalar, None, is_immediate)
            }
            ref kind => panic!(
                "TODO: Unimplemented Primitive::Pointer TyKind ({:#?}):\n{:#?}",
                kind, ty
            ),
        }
    } else {
        trans_scalar_generic(cx, scalar, is_immediate)
    }
}

// only pub for LayoutTypeMethods::scalar_pair_element_backend_type
pub fn trans_scalar_pair<'spv, 'tcx>(
    cx: &CodegenCx<'spv, 'tcx>,
    ty: &TyAndLayout<'tcx>,
    index: usize,
    is_immediate: bool,
) -> Word {
    let (a, b) = match &ty.layout.abi {
        Abi::ScalarPair(a, b) => (a, b),
        other => panic!("trans_scalar_pair invalid abi: {:?}", other),
    };
    let scalar = [a, b][index];
    trans_scalar_pair_impl(cx, ty, scalar, index, is_immediate)
}

fn trans_scalar_pair_impl<'spv, 'tcx>(
    cx: &CodegenCx<'spv, 'tcx>,
    ty: &TyAndLayout<'tcx>,
    scalar: &Scalar,
    index: usize,
    is_immediate: bool,
) -> Word {
    // When we know the ty, try to fill in the pointer type in case we have it, instead of defaulting to pointer to u8.
    if scalar.value == Primitive::Pointer {
        match ty.ty.kind {
            TyKind::Ref(_, elem, _) => {
                if cx.layout_of(elem).is_unsized() {
                    trans_scalar_known_ty(cx, &ty.field(cx, index), scalar, is_immediate)
                } else {
                    // TODO: This is some old code when I was trying to get this to work. This might actually not be
                    // needed, and the type is always unsized?
                    SpirvType::Pointer {
                        storage_class: StorageClass::Generic,
                        pointee: cx.layout_of(elem).spirv_type(cx),
                    }
                    .def(cx)
                }
            }
            TyKind::RawPtr(ty_and_mut) => {
                let elem = ty_and_mut.ty;
                if cx.layout_of(elem).is_unsized() {
                    trans_scalar_known_ty(cx, &ty.field(cx, index), scalar, is_immediate)
                } else {
                    // Same comment as TyKind::Ref
                    SpirvType::Pointer {
                        storage_class: StorageClass::Generic,
                        pointee: cx.layout_of(elem).spirv_type(cx),
                    }
                    .def(cx)
                }
            }
            TyKind::Adt(def, _) if def.is_box() => {
                let ptr_ty = cx.layout_of(cx.tcx.mk_mut_ptr(ty.ty.boxed_ty()));
                trans_scalar_pair_impl(cx, &ptr_ty, scalar, index, is_immediate)
            }
            TyKind::Tuple(elements) if elements.len() == 1 => {
                // The tuple is merely a wrapper, index into the tuple and retry.
                // This happens in cases like (&[u8],)
                let item = cx.layout_of(ty.ty.tuple_fields().next().unwrap());
                trans_scalar_pair_impl(cx, &item, scalar, index, is_immediate)
            }
            TyKind::Tuple(elements) if elements.len() == 2 => {
                let sub_ty = cx.layout_of(ty.ty.tuple_fields().nth(index).unwrap());
                trans_scalar_known_ty(cx, &sub_ty, scalar, is_immediate)
            }
            TyKind::Adt(..) | TyKind::Closure(..) => {
                trans_scalar_pointer_struct(cx, ty, scalar, Some(index), is_immediate)
            }
            ref kind => panic!(
                "TODO: Unimplemented Primitive::Pointer TyKind in scalar pair ({:#?}):\n{:#?}",
                kind, ty
            ),
        }
    } else {
        trans_scalar_generic(cx, scalar, is_immediate)
    }
}

// This is pretty weird. I have no idea how to explain what's going on here in a doc comment and not a full
// conversation, so poke Ashley and I'll try to explain it.
fn trans_scalar_pointer_struct<'spv, 'tcx>(
    cx: &CodegenCx<'spv, 'tcx>,
    ty: &TyAndLayout<'tcx>,
    scalar: &Scalar,
    index: Option<usize>,
    is_immediate: bool,
) -> Word {
    let fields = ty
        .fields
        .index_by_increasing_offset()
        .map(|f| ty.field(cx, f))
        .filter(|f| !f.is_zst())
        .collect::<Vec<_>>();
    assert!(fields.iter().all(|f| f != ty));
    match index {
        Some(index) => match fields.len() {
            1 => trans_scalar_pair_impl(cx, &fields[0], scalar, index, is_immediate),
            2 => trans_scalar_known_ty(cx, &fields[index], scalar, is_immediate),
            other => panic!(
                "Unable to dig scalar pair pointer type: fields length {}",
                other
            ),
        },
        None => match fields.len() {
            1 => trans_scalar_known_ty(cx, &fields[0], scalar, is_immediate),
            other => panic!("Unable to dig scalar pointer type: fields length {}", other),
        },
    }
}

fn trans_scalar_generic<'spv, 'tcx>(
    cx: &CodegenCx<'spv, 'tcx>,
    scalar: &Scalar,
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
            panic!("trans_scalar_generic Primitive::Pointer should be handled by caller")
        }
    }
}

fn trans_aggregate<'spv, 'tcx>(cx: &CodegenCx<'spv, 'tcx>, ty: &TyAndLayout<'tcx>) -> Word {
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
            let element_type = ty.field(cx, 0).spirv_type(cx);
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
fn trans_struct<'spv, 'tcx>(cx: &CodegenCx<'spv, 'tcx>, ty: &TyAndLayout<'tcx>) -> Word {
    let name = name_of_struct(ty);
    if let TyKind::Foreign(_) = ty.ty.kind {
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
        field_types.push(field_ty.spirv_type(cx));
        let offset = ty.fields.offset(i);
        field_offsets.push(offset);
        if let Variants::Single { index } = ty.variants {
            if let TyKind::Adt(adt, _) = ty.ty.kind {
                let field = &adt.variants[index].fields[i];
                field_names.push(field.ident.name.to_ident_string());
            } else {
                field_names.push(format!("{}", i));
            }
        } else {
            if let TyKind::Adt(_, _) = ty.ty.kind {
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

fn name_of_struct(ty: &TyAndLayout<'_>) -> String {
    let mut name = ty.ty.to_string();
    if let (&TyKind::Adt(def, _), &Variants::Single { index }) = (&ty.ty.kind, &ty.variants) {
        if def.is_enum() && !def.variants.is_empty() {
            write!(&mut name, "::{}", def.variants[index].ident).unwrap();
        }
    }
    if let (&TyKind::Generator(_, _, _), &Variants::Single { index }) = (&ty.ty.kind, &ty.variants)
    {
        write!(&mut name, "::{}", GeneratorSubsts::variant_name(index)).unwrap();
    }
    name
}
