use crate::codegen_cx::CodegenCx;
use rspirv::spirv::{StorageClass, Word};
use rustc_middle::ty::{layout::TyAndLayout, TyKind};
use rustc_target::abi::{Abi, FieldsShape, LayoutOf, Primitive, Scalar, Size};
use std::iter::empty;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SpirvType {
    Void,
    Bool,
    Integer(u32, bool),
    Float(u32),
    // TODO: Do we fold this into Adt?
    /// Zero Sized Type
    ZST,
    /// This uses the rustc definition of "adt", i.e. a struct, enum, or union
    Adt {
        // TODO: enums/unions
        field_types: Vec<Word>,
    },
    Vector {
        element: Word,
        count: u32,
    },
    Array {
        element: Word,
        count: u32,
    },
    Pointer {
        pointee: Word,
    },
    Function {
        return_type: Word,
        arguments: Vec<Word>,
    },
}

pub fn trans_type<'spv, 'tcx>(cx: &CodegenCx<'spv, 'tcx>, ty: TyAndLayout<'tcx>) -> Word {
    if ty.is_zst() {
        let def = SpirvType::ZST;
        let result = cx.emit_global().type_struct(empty());
        cx.def_type(result, def);
        return result;
    }

    // Note: ty.abi is orthogonal to ty.variants and ty.fields, e.g. `ManuallyDrop<Result<isize, isize>>`
    // has abi `ScalarPair`.
    match ty.abi {
        Abi::Uninhabited => panic!(
            "TODO: Abi::Uninhabited not supported yet in trans_type: {:?}",
            ty
        ),
        Abi::Scalar(ref scalar) => trans_scalar(cx, ty, scalar, None),
        Abi::ScalarPair(ref one, ref two) => {
            let one_spirv = trans_scalar(cx, ty, one, Some(0));
            let two_spirv = trans_scalar(cx, ty, two, Some(1));
            let result = cx
                .emit_global()
                .type_struct([one_spirv, two_spirv].iter().cloned());
            let def = SpirvType::Adt {
                field_types: vec![one_spirv, two_spirv],
            };
            cx.def_type(result, def);
            result
        }
        Abi::Vector { ref element, count } => {
            let elem_spirv = trans_scalar(cx, ty, element, None);
            let result = cx.emit_global().type_vector(elem_spirv, count as u32);
            let def = SpirvType::Vector {
                element: elem_spirv,
                count: count as u32,
            };
            cx.def_type(result, def);
            result
        }
        Abi::Aggregate { sized: _ } => trans_aggregate(cx, ty),
    }
}

fn trans_scalar<'spv, 'tcx>(
    cx: &CodegenCx<'spv, 'tcx>,
    ty: TyAndLayout<'tcx>,
    scalar: &Scalar,
    pair_index: Option<usize>,
) -> Word {
    let (ty, def) = match scalar.value {
        Primitive::Int(width, signedness) => {
            // let width_bits = width.size().bits() as u128;
            // let width_max_val = if width_bits == 128 {
            //     u128::MAX
            // } else {
            //     (1 << width_bits) - 1
            // };
            if scalar.valid_range == (0..=1) {
                (cx.emit_global().type_bool(), SpirvType::Bool)
            // } else if scalar.valid_range != (0..=width_max_val) {
            // TODO: Do we handle this specially?
            } else {
                (
                    cx.emit_global()
                        .type_int(width.size().bits() as u32, if signedness { 1 } else { 0 }),
                    SpirvType::Integer(width as u32, signedness),
                )
            }
        }
        Primitive::F32 => (cx.emit_global().type_float(32), SpirvType::Float(32)),
        Primitive::F64 => (cx.emit_global().type_float(64), SpirvType::Float(64)),
        Primitive::Pointer => {
            fn do_normal_ptr<'spv, 'tcx>(
                cx: &CodegenCx<'spv, 'tcx>,
                pair_index: Option<usize>,
                get_pointee_type: impl Fn() -> TyAndLayout<'tcx>,
            ) -> Word {
                if pair_index == Some(1) {
                    let ptr_size = cx.tcx.data_layout.pointer_size.bits() as u32;
                    let result = cx.emit_global().type_int(ptr_size, 0);
                    let def = SpirvType::Integer(ptr_size, false);
                    cx.def_type(result, def);
                    result
                } else {
                    let pointee = trans_type(cx, get_pointee_type());
                    let result =
                        cx.emit_global()
                            .type_pointer(None, StorageClass::Generic, pointee);
                    let def = SpirvType::Pointer { pointee };
                    cx.def_type(result, def);
                    result
                }
            }
            if pair_index == Some(1) {
                let ptr_size = cx.tcx.data_layout.pointer_size.bits() as u32;
                (
                    cx.emit_global().type_int(ptr_size, 0),
                    SpirvType::Integer(ptr_size, false),
                )
            } else {
                match ty.ty.kind {
                    TyKind::Ref(_region, ty, _mutability) => {
                        return do_normal_ptr(cx, pair_index, || cx.layout_of(ty))
                    }
                    TyKind::RawPtr(type_and_mut) => {
                        return do_normal_ptr(cx, pair_index, || cx.layout_of(type_and_mut.ty))
                    }
                    TyKind::Adt(_adt_def, _substs)
                        // if adt_def.is_struct()
                        //     && adt_def.variants[0u32.into()].fields.len()
                        //         == if pair_index.is_some() { 2 } else { 1 } =>
                        =>
                    {
                        // TODO: This is probably wrong
                        let field_index = pair_index.unwrap_or(0);
                        // skip through to the field
                        return trans_type(cx, ty.field(cx, field_index));
                    }
                    ref kind => panic!(
                        "TODO: Unimplemented Primitive::Pointer TyKind ({:#?}):\n{:#?}",
                        kind, ty
                    ),
                };
            }
        }
    };
    cx.def_type(ty, def);
    ty
}

fn trans_aggregate<'spv, 'tcx>(cx: &CodegenCx<'spv, 'tcx>, ty: TyAndLayout<'tcx>) -> Word {
    match ty.fields {
        FieldsShape::Primitive => panic!(
            "FieldsShape::Primitive not supported yet in trans_type: {:?}",
            ty
        ),
        // TODO: Is this the right thing to do?
        FieldsShape::Union(_field_count) => {
            assert_ne!(ty.size.bytes(), 0);
            let byte = cx.emit_global().type_int(8, 0);
            cx.def_type(byte, SpirvType::Integer(8, false));
            let int = cx.emit_global().type_int(8, 0);
            cx.def_type(int, SpirvType::Integer(32, false));
            let length = cx.emit_global().constant_u32(int, ty.size.bytes() as u32);
            let result = cx.emit_global().type_array(byte, length);
            cx.def_type(
                result,
                SpirvType::Array {
                    element: byte,
                    count: ty.size.bytes() as u32,
                },
            );
            result
        }
        FieldsShape::Array { stride: _, count } => {
            // spir-v doesn't support zero-sized arrays
            // note that zero-sized arrays don't report as .is_zst() for some reason? TODO: investigate why
            let nonzero_count = if count == 0 { 1 } else { count };
            // TODO: Assert stride is same as spirv's stride?
            let element_type = trans_type(cx, ty.field(cx, 0));
            let int = cx.emit_global().type_int(8, 0);
            cx.def_type(int, SpirvType::Integer(32, false));
            let length = cx.emit_global().constant_u32(int, nonzero_count as u32);
            let result = cx.emit_global().type_array(element_type, length);
            let def = SpirvType::Array {
                element: element_type,
                count: nonzero_count as u32,
            };
            cx.def_type(result, def);
            result
        }
        FieldsShape::Arbitrary {
            offsets: _,
            memory_index: _,
        } => trans_struct(cx, ty),
    }
}

// see struct_llfields in librustc_codegen_llvm for implementation hints
fn trans_struct<'spv, 'tcx>(cx: &CodegenCx<'spv, 'tcx>, ty: TyAndLayout<'tcx>) -> Word {
    let mut offset = Size::ZERO;
    let mut prev_effective_align = ty.align.abi;
    let mut result: Vec<_> = Vec::new();
    for i in ty.fields.index_by_increasing_offset() {
        let target_offset = ty.fields.offset(i as usize);
        let field = ty.field(cx, i);
        let effective_field_align = ty
            .align
            .abi
            .min(field.align.abi)
            .restrict_for_offset(target_offset);

        assert!(target_offset >= offset);
        let padding = target_offset - offset;
        let padding_align = prev_effective_align.min(effective_field_align);
        assert_eq!(offset.align_to(padding_align) + padding, target_offset);
        if padding != Size::ZERO {
            // TODO: Use OpMemberDecorate to implement padding
            panic!("Padded structs are not supported yet: {:?}", ty);
        }
        // result.push(cx.type_padding_filler(padding, padding_align));

        result.push(trans_type(cx, field));
        offset = target_offset + field.size;
        prev_effective_align = effective_field_align;
    }
    let result_ty = cx.emit_global().type_struct(result.iter().cloned());
    let def = SpirvType::Adt {
        field_types: result,
    };
    cx.def_type(result_ty, def);
    result_ty
}
