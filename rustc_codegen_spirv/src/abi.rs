use crate::codegen_cx::CodegenCx;
use rspirv::spirv::{StorageClass, Word};
use rustc_middle::ty::{layout::TyAndLayout, TyKind};
use rustc_target::abi::{Abi, FieldsShape, LayoutOf, Primitive, Scalar, Size};
use std::fmt;
use std::iter::empty;

#[derive(Clone, Debug, PartialEq, Eq, Ord, PartialOrd, Hash)]
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
        count: Word,
    },
    Array {
        element: Word,
        count: Word,
    },
    Pointer {
        storage_class: StorageClass,
        pointee: Word,
    },
    Function {
        return_type: Word,
        arguments: Vec<Word>,
    },
}

impl SpirvType {
    /// Note: Builder::type_* should be called *nowhere else* but here, to ensure CodegenCx::type_defs stays up-to-date
    pub fn def<'spv, 'tcx>(&self, cx: &CodegenCx<'spv, 'tcx>) -> Word {
        // TODO: rspirv does a linear search to dedupe, probably want to cache here.
        let result = match *self {
            SpirvType::Void => cx.emit_global().type_void(),
            SpirvType::Bool => cx.emit_global().type_bool(),
            SpirvType::Integer(width, signedness) => cx
                .emit_global()
                .type_int(width, if signedness { 1 } else { 0 }),
            SpirvType::Float(width) => cx.emit_global().type_float(width),
            SpirvType::ZST => cx.emit_global().type_struct(empty()),
            SpirvType::Adt { ref field_types } => {
                cx.emit_global().type_struct(field_types.iter().cloned())
            }
            SpirvType::Vector { element, count } => cx.emit_global().type_vector(element, count),
            SpirvType::Array { element, count } => cx.emit_global().type_array(element, count),
            SpirvType::Pointer {
                storage_class,
                pointee,
            } => cx.emit_global().type_pointer(None, storage_class, pointee),
            SpirvType::Function {
                return_type,
                ref arguments,
            } => cx
                .emit_global()
                .type_function(return_type, arguments.iter().cloned()),
        };
        cx.type_defs
            .borrow_mut()
            .entry(result)
            .or_insert_with(|| self.clone());
        result
    }

    pub fn debug<'cx, 'spv, 'tcx>(
        self,
        cx: &'cx CodegenCx<'spv, 'tcx>,
    ) -> SpirvTypePrinter<'cx, 'spv, 'tcx> {
        SpirvTypePrinter { ty: self, cx }
    }
}

pub struct SpirvTypePrinter<'cx, 'spv, 'tcx> {
    ty: SpirvType,
    cx: &'cx CodegenCx<'spv, 'tcx>,
}

impl fmt::Debug for SpirvTypePrinter<'_, '_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.ty {
            SpirvType::Void => f.debug_struct("Void").finish(),
            SpirvType::Bool => f.debug_struct("Bool").finish(),
            SpirvType::Integer(width, signedness) => f
                .debug_struct("Integer")
                .field("width", &width)
                .field("signedness", &signedness)
                .finish(),
            SpirvType::Float(width) => f.debug_struct("Float").field("width", &width).finish(),
            SpirvType::ZST => f.debug_struct("ZST").finish(),
            SpirvType::Adt { ref field_types } => {
                let fields = field_types
                    .iter()
                    .map(|&f| self.cx.lookup_type(f).debug(self.cx))
                    .collect::<Vec<_>>();
                f.debug_struct("Adt").field("field_types", &fields).finish()
            }
            SpirvType::Vector { element, count } => f
                .debug_struct("Vector")
                .field("element", &self.cx.lookup_type(element).debug(self.cx))
                .field(
                    "count",
                    &self
                        .cx
                        .builder
                        .lookup_const_u64(count)
                        .expect("Vector type has invalid count value"),
                )
                .finish(),
            SpirvType::Array { element, count } => f
                .debug_struct("Array")
                .field("element", &self.cx.lookup_type(element).debug(self.cx))
                .field(
                    "count",
                    &self
                        .cx
                        .builder
                        .lookup_const_u64(count)
                        .expect("Array type has invalid count value"),
                )
                .finish(),
            SpirvType::Pointer {
                storage_class,
                pointee,
            } => f
                .debug_struct("Pointer")
                .field("storage_class", &storage_class)
                .field("pointee", &self.cx.lookup_type(pointee).debug(self.cx))
                .finish(),
            SpirvType::Function {
                return_type,
                ref arguments,
            } => {
                let args = arguments
                    .iter()
                    .map(|&a| self.cx.lookup_type(a).debug(self.cx))
                    .collect::<Vec<_>>();
                f.debug_struct("Function")
                    .field("return_type", &self.cx.lookup_type(return_type))
                    .field("arguments", &args)
                    .finish()
            }
        }
    }
}

pub fn trans_type<'spv, 'tcx>(cx: &CodegenCx<'spv, 'tcx>, ty: TyAndLayout<'tcx>) -> Word {
    if ty.is_zst() {
        return SpirvType::ZST.def(cx);
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
            SpirvType::Adt {
                field_types: vec![one_spirv, two_spirv],
            }
            .def(cx)
        }
        Abi::Vector { ref element, count } => {
            let elem_spirv = trans_scalar(cx, ty, element, None);
            SpirvType::Vector {
                element: elem_spirv,
                count: count as u32,
            }
            .def(cx)
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
    match scalar.value {
        Primitive::Int(width, signedness) => {
            // let width_bits = width.size().bits() as u128;
            // let width_max_val = if width_bits == 128 {
            //     u128::MAX
            // } else {
            //     (1 << width_bits) - 1
            // };
            if scalar.valid_range == (0..=1) {
                SpirvType::Bool.def(cx)
            // } else if scalar.valid_range != (0..=width_max_val) {
            // TODO: Do we handle this specially?
            } else {
                SpirvType::Integer(width.size().bits() as u32, signedness).def(cx)
            }
        }
        Primitive::F32 => SpirvType::Float(32).def(cx),
        Primitive::F64 => SpirvType::Float(64).def(cx),
        Primitive::Pointer => {
            fn do_normal_ptr<'spv, 'tcx>(
                cx: &CodegenCx<'spv, 'tcx>,
                pair_index: Option<usize>,
                get_pointee_type: impl Fn() -> TyAndLayout<'tcx>,
            ) -> Word {
                if pair_index == Some(1) {
                    let ptr_size = cx.tcx.data_layout.pointer_size.bits() as u32;
                    SpirvType::Integer(ptr_size, false).def(cx)
                } else {
                    let pointee = trans_type(cx, get_pointee_type());
                    SpirvType::Pointer {
                        storage_class: StorageClass::Generic,
                        pointee,
                    }
                    .def(cx)
                }
            }
            if pair_index == Some(1) {
                let ptr_size = cx.tcx.data_layout.pointer_size.bits() as u32;
                SpirvType::Integer(ptr_size, false).def(cx)
            } else {
                match ty.ty.kind {
                    TyKind::Ref(_region, ty, _mutability) => {
                        do_normal_ptr(cx, pair_index, || cx.layout_of(ty))
                    }
                    TyKind::RawPtr(type_and_mut) => {
                        do_normal_ptr(cx, pair_index, || cx.layout_of(type_and_mut.ty))
                    }
                    TyKind::Adt(_adt_def, _substs) => {
                        // if adt_def.is_struct()
                        //     && adt_def.variants[0u32.into()].fields.len()
                        //         == if pair_index.is_some() { 2 } else { 1 } =>
                        // TODO: This is probably wrong
                        let field_index = pair_index.unwrap_or(0);
                        // skip through to the field
                        trans_type(cx, ty.field(cx, field_index))
                    }
                    ref kind => panic!(
                        "TODO: Unimplemented Primitive::Pointer TyKind ({:#?}):\n{:#?}",
                        kind, ty
                    ),
                }
            }
        }
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
            assert_ne!(ty.size.bytes(), 0);
            let byte = SpirvType::Integer(8, false).def(cx);
            let int = SpirvType::Integer(32, false).def(cx);
            let count = cx.builder.constant_u32(int, ty.size.bytes() as u32);
            SpirvType::Array {
                element: byte,
                count,
            }
            .def(cx)
        }
        FieldsShape::Array { stride: _, count } => {
            // spir-v doesn't support zero-sized arrays
            // note that zero-sized arrays don't report as .is_zst() for some reason? TODO: investigate why
            let nonzero_count = if count == 0 { 1 } else { count };
            // TODO: Assert stride is same as spirv's stride?
            let element_type = trans_type(cx, ty.field(cx, 0));
            let int = SpirvType::Integer(32, false).def(cx);
            let count_const = cx.builder.constant_u32(int, nonzero_count as u32);
            SpirvType::Array {
                element: element_type,
                count: count_const,
            }
            .def(cx)
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
    SpirvType::Adt {
        field_types: result,
    }
    .def(cx)
}
