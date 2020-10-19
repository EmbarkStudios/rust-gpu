use super::{extract_literal_int_as_u64, DefAnalyzer};
use rspirv::dr::Instruction;
use rspirv::spirv::{AccessQualifier, Dim, ImageFormat, Op, StorageClass};

#[derive(PartialEq, Debug)]
pub enum ScalarType {
    Void,
    Bool,
    Int { width: u32, signed: bool },
    Float { width: u32 },
    Opaque { name: String },
    Event,
    DeviceEvent,
    ReserveId,
    Queue,
    Pipe,
    ForwardPointer { storage_class: StorageClass },
    PipeStorage,
    NamedBarrier,
    Sampler,
}

fn trans_scalar_type(inst: &Instruction) -> Option<ScalarType> {
    Some(match inst.class.opcode {
        Op::TypeVoid => ScalarType::Void,
        Op::TypeBool => ScalarType::Bool,
        Op::TypeEvent => ScalarType::Event,
        Op::TypeDeviceEvent => ScalarType::DeviceEvent,
        Op::TypeReserveId => ScalarType::ReserveId,
        Op::TypeQueue => ScalarType::Queue,
        Op::TypePipe => ScalarType::Pipe,
        Op::TypePipeStorage => ScalarType::PipeStorage,
        Op::TypeNamedBarrier => ScalarType::NamedBarrier,
        Op::TypeSampler => ScalarType::Sampler,
        Op::TypeForwardPointer => ScalarType::ForwardPointer {
            storage_class: inst.operands[0].unwrap_storage_class(),
        },
        Op::TypeInt => ScalarType::Int {
            width: inst.operands[0].unwrap_literal_int32(),
            signed: inst.operands[1].unwrap_literal_int32() != 0,
        },
        Op::TypeFloat => ScalarType::Float {
            width: inst.operands[0].unwrap_literal_int32(),
        },
        Op::TypeOpaque => ScalarType::Opaque {
            name: inst.operands[0].unwrap_literal_string().to_string(),
        },
        _ => return None,
    })
}

impl std::fmt::Display for ScalarType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            Self::Void => f.write_str("void"),
            Self::Bool => f.write_str("bool"),
            Self::Int { width, signed } => {
                if signed {
                    write!(f, "i{}", width)
                } else {
                    write!(f, "u{}", width)
                }
            }
            Self::Float { width } => write!(f, "f{}", width),
            Self::Opaque { ref name } => write!(f, "Opaque{{{}}}", name),
            Self::Event => f.write_str("Event"),
            Self::DeviceEvent => f.write_str("DeviceEvent"),
            Self::ReserveId => f.write_str("ReserveId"),
            Self::Queue => f.write_str("Queue"),
            Self::Pipe => f.write_str("Pipe"),
            Self::ForwardPointer { storage_class } => {
                write!(f, "ForwardPointer{{{:?}}}", storage_class)
            }
            Self::PipeStorage => f.write_str("PipeStorage"),
            Self::NamedBarrier => f.write_str("NamedBarrier"),
            Self::Sampler => f.write_str("Sampler"),
        }
    }
}

#[derive(PartialEq, Debug)]
#[allow(dead_code)]
pub enum AggregateType {
    Scalar(ScalarType),
    Array {
        ty: Box<AggregateType>,
        len: u64,
    },
    Pointer {
        ty: Box<AggregateType>,
        storage_class: StorageClass,
    },
    Image {
        ty: Box<AggregateType>,
        dim: Dim,
        depth: u32,
        arrayed: u32,
        multi_sampled: u32,
        sampled: u32,
        format: ImageFormat,
        access: Option<AccessQualifier>,
    },
    SampledImage {
        ty: Box<AggregateType>,
    },
    Aggregate(Vec<AggregateType>),
    Function(Vec<AggregateType>, Box<AggregateType>),
}

pub(crate) fn trans_aggregate_type(
    def: &DefAnalyzer<'_>,
    inst: &Instruction,
) -> Option<AggregateType> {
    Some(match inst.class.opcode {
        Op::TypeArray => {
            let len_def = def.op_def(&inst.operands[1]);
            assert!(len_def.class.opcode == Op::Constant); // don't support spec constants yet

            let len_value = extract_literal_int_as_u64(&len_def.operands[0]);

            AggregateType::Array {
                ty: Box::new(
                    trans_aggregate_type(def, &def.op_def(&inst.operands[0]))
                        .expect("Expect base type for OpTypeArray"),
                ),
                len: len_value,
            }
        }
        Op::TypePointer => AggregateType::Pointer {
            storage_class: inst.operands[0].unwrap_storage_class(),
            ty: Box::new(
                trans_aggregate_type(def, &def.op_def(&inst.operands[1]))
                    .expect("Expect base type for OpTypePointer"),
            ),
        },
        Op::TypeRuntimeArray | Op::TypeVector | Op::TypeMatrix | Op::TypeSampledImage => {
            AggregateType::Aggregate(
                trans_aggregate_type(def, &def.op_def(&inst.operands[0]))
                    .map_or_else(Vec::new, |v| vec![v]),
            )
        }
        Op::TypeStruct => {
            let mut types = vec![];
            for operand in inst.operands.iter() {
                let op_def = def.op_def(operand);

                match trans_aggregate_type(def, &op_def) {
                    Some(ty) => types.push(ty),
                    None => panic!("Expected type"),
                }
            }

            AggregateType::Aggregate(types)
        }
        Op::TypeFunction => {
            let mut parameters = vec![];
            let ret = trans_aggregate_type(def, &def.op_def(&inst.operands[0])).unwrap();
            for operand in inst.operands.iter().skip(1) {
                let op_def = def.op_def(operand);

                match trans_aggregate_type(def, &op_def) {
                    Some(ty) => parameters.push(ty),
                    None => panic!("Expected type"),
                }
            }

            AggregateType::Function(parameters, Box::new(ret))
        }
        Op::TypeImage => AggregateType::Image {
            ty: Box::new(
                trans_aggregate_type(def, &def.op_def(&inst.operands[0]))
                    .expect("Expect base type for OpTypeImage"),
            ),
            dim: inst.operands[1].unwrap_dim(),
            depth: inst.operands[2].unwrap_literal_int32(),
            arrayed: inst.operands[3].unwrap_literal_int32(),
            multi_sampled: inst.operands[4].unwrap_literal_int32(),
            sampled: inst.operands[5].unwrap_literal_int32(),
            format: inst.operands[6].unwrap_image_format(),
            access: inst.operands.get(7).map(|op| op.unwrap_access_qualifier()),
        },
        _ => {
            if let Some(ty) = trans_scalar_type(inst) {
                AggregateType::Scalar(ty)
            } else {
                return None;
            }
        }
    })
}

impl std::fmt::Display for AggregateType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Scalar(scalar) => write!(f, "{}", scalar),
            Self::Array { ty, len } => write!(f, "[{}; {}]", ty, len),
            Self::Pointer { ty, storage_class } => write!(f, "*{{{:?}}} {}", storage_class, ty),
            Self::Image {
                ty,
                dim,
                depth,
                arrayed,
                multi_sampled,
                sampled,
                format,
                access,
            } => write!(
                f,
                "Image {{ {}, dim:{:?}, depth:{}, arrayed:{}, \
                multi_sampled:{}, sampled:{}, format:{:?}, access:{:?} }}",
                ty, dim, depth, arrayed, multi_sampled, sampled, format, access
            ),
            Self::SampledImage { ty } => write!(f, "SampledImage{{{}}}", ty),
            Self::Aggregate(agg) => {
                f.write_str("struct {")?;
                for elem in agg {
                    write!(f, " {},", elem)?;
                }
                f.write_str(" }")
            }
            Self::Function(args, ret) => {
                f.write_str("fn(")?;
                for elem in args {
                    write!(f, " {},", elem)?;
                }
                write!(f, " ) -> {}", ret)
            }
        }
    }
}
