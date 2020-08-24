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
            let width_bits = width.size().bits() as u128;
            let width_max_val = if width_bits == 128 {
                u128::MAX
            } else {
                (1 << width_bits) - 1
            };
            if scalar.valid_range == (0..=1) {
                (cx.emit_global().type_bool(), SpirvType::Bool)
            } else if scalar.valid_range != (0..=width_max_val) {
                panic!("TODO: Unimplemented valid_range that's not the size of the int (width={:?}, range={:?}): {:?}", width, scalar.valid_range, scalar)
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
            if pair_index == Some(1) {
                let ptr_size = cx.tcx.data_layout.pointer_size.bits() as u32;
                (
                    cx.emit_global().type_int(ptr_size, 0),
                    SpirvType::Integer(ptr_size, false),
                )
            } else {
                let pointee = match ty.ty.kind {
                    TyKind::Ref(_region, ty, _mutability) => {
                        let pointee_type = trans_type(cx, cx.layout_of(ty));
                        cx.emit_global()
                            .type_pointer(None, StorageClass::Generic, pointee_type)
                    }
                    TyKind::RawPtr(type_and_mut) => {
                        let pointee_type = trans_type(cx, cx.layout_of(type_and_mut.ty));
                        cx.emit_global()
                            .type_pointer(None, StorageClass::Generic, pointee_type)
                    }
                    ref kind => panic!(
                        "TODO: Unimplemented Primitive::Pointer TyKind ({:?}): {:?}",
                        kind, ty
                    ),
                };
                let pointer = cx
                    .emit_global()
                    .type_pointer(None, StorageClass::Generic, pointee);
                (pointer, SpirvType::Pointer { pointee })
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
            let byte = cx.emit_global().type_int(8, 0);
            cx.def_type(byte, SpirvType::Integer(8, false));
            let result = cx.emit_global().type_array(byte, ty.size.bytes() as u32);
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
            // TODO: Assert stride is same as spirv's stride?
            let element_type = trans_type(cx, ty.field(cx, 0));
            let result = cx.emit_global().type_array(element_type, count as u32);
            let def = SpirvType::Array {
                element: element_type,
                count: count as u32,
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
