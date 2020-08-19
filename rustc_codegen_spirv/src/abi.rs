use crate::codegen_cx::CodegenCx;
use rspirv::spirv::Word;
use rustc_middle::ty::layout::TyAndLayout;
use rustc_target::abi::{Abi, FieldsShape, Primitive, Scalar, Size};

/*
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SpirvType {
    Bool(Word),
    Integer(Word, u32, bool),
    Float(Word, u32),
    // TODO: Do we fold this into Adt?
    /// Zero Sized Type
    ZST(Word),
    /// This variant is kind of useless, but it lets us recognize Pointer(Slice(T)), etc.
    // TODO: Actually recognize Pointer(Slice(T)) and generate a wide pointer.
    Slice(Box<SpirvType>),
    /// This uses the rustc definition of "adt", i.e. a struct, enum, or union
    Adt {
        def: Word,
        // TODO: enums/unions
        field_types: Vec<SpirvType>,
    },
    Pointer {
        def: Word,
        pointee: Box<SpirvType>,
    },
}

impl SpirvType {
    pub fn def(&self) -> Word {
        match *self {
            SpirvType::Bool(def) => def,
            SpirvType::Integer(def, _, _) => def,
            SpirvType::Float(def, _) => def,
            SpirvType::ZST(def) => def,
            SpirvType::Slice(ref element) => element.def(),
            SpirvType::Adt { def, .. } => def,
            SpirvType::Pointer { def, .. } => def,
        }
    }
}
*/

pub fn trans_type<'spv, 'tcx>(cx: &CodegenCx<'spv, 'tcx>, ty: TyAndLayout<'tcx>) -> Word {
    if ty.is_zst() {
        return cx.emit_global().type_struct(&[]);
    }

    // Note: ty.abi is orthogonal to ty.variants and ty.fields, e.g. `ManuallyDrop<Result<isize, isize>>`
    // has abi `ScalarPair`.
    match ty.abi {
        Abi::Uninhabited => panic!(
            "TODO: Abi::Uninhabited not supported yet in trans_type: {:?}",
            ty
        ),
        Abi::Scalar(ref scalar) => trans_scalar(cx, scalar),
        Abi::ScalarPair(ref one, ref two) => {
            let one_spirv = trans_scalar(cx, one);
            let two_spirv = trans_scalar(cx, two);
            cx.emit_global().type_struct([one_spirv, two_spirv])
        }
        Abi::Vector { ref element, count } => {
            let elem_spirv = trans_scalar(cx, element);
            cx.emit_global().type_vector(elem_spirv, count as u32)
        }
        Abi::Aggregate { sized: _ } => trans_aggregate(cx, ty),
    }
}

fn trans_scalar<'spv, 'tcx>(cx: &CodegenCx<'spv, 'tcx>, scalar: &Scalar) -> Word {
    match scalar.value {
        Primitive::Int(width, signedness) => cx
            .emit_global()
            .type_int(width.size().bits() as u32, if signedness { 1 } else { 0 }),
        Primitive::F32 => cx.emit_global().type_float(32),
        Primitive::F64 => cx.emit_global().type_float(64),
        Primitive::Pointer => {
            panic!(
                "TODO: Scalar(Pointer) not supported yet in trans_type: {:?}",
                scalar
            );
        }
    }
}

fn trans_aggregate<'spv, 'tcx>(cx: &CodegenCx<'spv, 'tcx>, ty: TyAndLayout<'tcx>) -> Word {
    match ty.fields {
        FieldsShape::Primitive => panic!(
            "FieldsShape::Primitive not supported yet in trans_type: {:?}",
            ty
        ),
        FieldsShape::Union(_field_count) => panic!(
            "FieldsShape::Union not supported yet in trans_type: {:?}",
            ty
        ),
        FieldsShape::Array { stride: _, count } => {
            // TODO: Assert stride is same as spirv's stride?
            let element_type = trans_type(cx, ty.field(cx, 0));
            cx.emit_global().type_array(element_type, count as u32)
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
    cx.emit_global().type_struct(&result)
}
