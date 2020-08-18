use rspirv::dr::Builder;
use rspirv::spirv::Word;
use rustc_middle::ty::layout::TyAndLayout;
use rustc_middle::ty::TyCtxt;
use rustc_target::abi::{Abi, Primitive};

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

pub fn trans_type<'tcx>(
    _tcx: TyCtxt<'tcx>,
    builder: &mut Builder,
    layout: TyAndLayout<'tcx>,
) -> Word {
    if let Abi::Scalar(ref scalar) = layout.abi {
        match scalar.value {
            Primitive::Int(width, signedness) => {
                builder.type_int(width.size().bits() as u32, if signedness { 1 } else { 0 })
            }
            Primitive::F32 => builder.type_float(32),
            Primitive::F64 => builder.type_float(64),
            Primitive::Pointer => {
                panic!(
                    "TODO: Scalar(Pointer) not supported yet in immediate_backend_type: {:?}",
                    layout
                );
            }
        }
    } else if layout.is_zst() {
        builder.type_struct(&[])
    } else {
        panic!("Unknown type: {:?}", layout);
    }
    /*
    match ty.kind {
        TyKind::Param(param) => panic!("TyKind::Param in trans_type: {:?}", param),
        TyKind::Bool => SpirvType::Bool(builder.type_bool()),
        TyKind::Tuple(fields) if fields.len() == 0 => SpirvType::ZST(builder.type_struct(&[])),
        TyKind::Int(ty) => {
            let size =
                ty.bit_width()
                    .unwrap_or_else(|| tcx.data_layout.pointer_size.bits()) as u32;
            SpirvType::Integer(builder.type_int(size, 1), size, true)
        }
        TyKind::Uint(ty) => {
            let size =
                ty.bit_width()
                    .unwrap_or_else(|| tcx.data_layout.pointer_size.bits()) as u32;
            SpirvType::Integer(builder.type_int(size, 0), size, false)
        }
        TyKind::Float(ty) => SpirvType::Float(
            builder.type_float(ty.bit_width() as u32),
            ty.bit_width() as u32,
        ),
        TyKind::Slice(ty) => {
            let element = trans_type(tcx, builder, ty);
            SpirvType::Slice(Box::new(element))
        }
        TyKind::RawPtr(type_and_mut) => {
            let pointee_type = trans_type(tcx, builder, type_and_mut.ty);
            SpirvType::Pointer {
                def: builder.type_pointer(None, StorageClass::Generic, pointee_type.def()),
                pointee: Box::new(pointee_type),
            }
        }
        TyKind::Ref(_region, pointee_ty, _mutability) => {
            let pointee_type = trans_type(tcx, builder, pointee_ty);
            SpirvType::Pointer {
                def: builder.type_pointer(None, StorageClass::Generic, pointee_type.def()),
                pointee: Box::new(pointee_type),
            }
        }
        TyKind::Adt(adt, substs) => {
            if adt.variants.len() != 1 {
                panic!("Enums/unions aren't supported yet: {:?}", adt);
            }
            let variant = &adt.variants[0u32.into()];
            let field_types = variant
                .fields
                .iter()
                .map(|field| {
                    let ty = field.ty(tcx, substs);
                    trans_type(tcx, builder, ty)
                })
                .collect::<Vec<_>>();
            let spirv_field_types = field_types.iter().map(|f| f.def());
            let def = builder.type_struct(spirv_field_types.collect::<Vec<_>>());
            SpirvType::Adt { def, field_types }
        }
        ref thing => panic!("Unknown type: {:?}", thing),
    }
    */
}
