//! SPIR-V type constraints. Can be used to perform a subset of validation,
//! or for inference purposes.
//!
//! Only type/storage-class equality is currently handled here, no concrete
//! type/storage-class constraints, nor anything involving non-type/storage-class
//! operands. While more constraints could be supported, encoding all the possible
//! rules for them may be challenging.
//!
//! Type constraints could be provided in two representations:
//! * static/generic: the constraints are built up from generic types
//!   (i.e. like iterator combinators), and passed to a generic trait method
//!   to "apply" them, allowing efficient code to be generated, similar to
//!   e.g. handcrafted imperative inference logic, but more flexible
//! * dynamic/value: the constraints are a tree of `enum` values (like an AST)
//!   that has to be visited (using recursion and `match`) to apply, which is
//!   less efficient than the "static representation", but even more flexible
//!   (as full introspection of the constraints is possible)
//!
//! Only the "dynamic representation" is currently implemented, as it's simpler.
//! If the "static representation" ends up being required (for performance reasons),
//! the "dynamic representation" could be generated from it using associated `const`s.

use rspirv::spirv::Op;

/// Helper trait to allow macros to work uniformly across different pattern types.
trait Pat {
    /// Unconstrained pattern, i.e. that matches everything.
    const ANY: Self;
}

/// Pattern for a SPIR-V storage class, dynamic representation (see module-level docs).
#[derive(Debug, PartialEq, Eq)]
pub enum StorageClassPat {
    /// Unconstrained storage class.
    Any,

    /// Storage class variable: all occurrences of `Var(i)` with the same `i` must be
    /// identical storage classes. For convenience, these associated consts are provided:
    /// * `StorageClassPat::S` for `StorageClassPat::Var(0)`
    Var(usize),
}

impl Pat for StorageClassPat {
    const ANY: Self = Self::Any;
}

impl StorageClassPat {
    pub const S: Self = Self::Var(0);
}

/// Pattern for a SPIR-V type, dynamic representation (see module-level docs).
#[derive(Debug, PartialEq, Eq)]
pub enum TyPat<'a> {
    /// Unconstrained type.
    Any,

    /// Type variable: all occurrences of `Var(i)` with the same `i` must be
    /// identical types. For convenience, these associated consts are provided:
    /// * `TyPat::T` for `TyPat::Var(0)`
    Var(usize),

    /// One of the two patterns must match.
    Either(&'a TyPat<'a>, &'a TyPat<'a>),

    /// `OpTypeVoid`: this is the only exception to the limitations of
    /// "no concrete type constraints", and is used solely to disambiguate
    /// image sampling instructions, which are specified using phrasing like:
    /// > Its components must be the same as *Sampled Type* of the underlying
    /// > `OpTypeImage` (unless that underlying *Sampled Type* is `OpTypeVoid`).
    Void,

    /// `OpTypePointer`, with an inner pattern for its *Storage Class* operand,
    /// and another for its *Type* operand.
    Pointer(&'a StorageClassPat, &'a TyPat<'a>),

    // FIXME(eddyb) try to DRY the same-shape patterns below.
    //
    /// `OpTypeArray`, with an inner pattern for its *Element Type* operand.
    Array(&'a TyPat<'a>),

    /// `OpTypeVector`, with an inner pattern for its *Component Type* operand.
    Vector(&'a TyPat<'a>),

    /// `OpTypeVector`, with an inner pattern for its *Component Type* operand,
    /// and a *Component Count* of `4`, used solely for image sampling instructions.
    Vector4(&'a TyPat<'a>),

    /// `OpTypeMatrix`, with an inner pattern for its *Column Type* operand.
    Matrix(&'a TyPat<'a>),

    /// `OpTypeImage`, with an inner pattern for its *Sampled Type* operand.
    Image(&'a TyPat<'a>),

    /// `OpTypePipe`, with an inner pattern for what is referred to as its
    /// "data type", despite `OpTypePipe` itself not taking any operands.
    // FIXME(eddyb) figure out why the spec talks about `OpTypePipe`s' "data type"s
    Pipe(&'a TyPat<'a>),

    /// `OpTypeSampledImage`, with an inner pattern for its *Image Type* operand.
    SampledImage(&'a TyPat<'a>),

    /// `OpTypeStruct`, with an inner pattern for its *Member N Type* operands.
    Struct(&'a TyListPat<'a>),

    /// `OpTypeFunction`, with an inner pattern for its *Return Type* operand,
    /// and another for its *Parameter N Type* operands.
    Function(&'a TyPat<'a>, &'a TyListPat<'a>),

    /// Index the composite type matched by the inner pattern (like a `Var`)
    /// by all of the value operands of the instruction which don't have their
    /// types consumed explicitly, i.e. the indices operands correspond to
    /// the `TyListPat::Any` part of the the `inputs` of the `InstSig`,
    /// e.g. in `sig! { (_, T, _, ..) -> IndexComposite(T) }`, the output type
    /// has to be identical to the result of indexing `T` by all the value
    /// operands *other than* the first 3.
    // FIXME(eddyb) reconsider this design, to be less magical
    IndexComposite(&'a TyPat<'a>),
}

impl Pat for TyPat<'_> {
    const ANY: Self = Self::Any;
}

impl TyPat<'_> {
    pub const T: Self = Self::Var(0);
}

/// Pattern for a list of SPIR-V types, dynamic representation (see module-level docs).
///
/// "Type lists" are used for `OpTypeStruct` fields, `OpTypeFunction` parameters,
/// and operand types of instructions signatures.
#[derive(Debug, PartialEq, Eq)]
pub enum TyListPat<'a> {
    /// Unconstrained type list (any length, and the types can freely vary).
    Any,

    /// Type list variable: all occurrences of `Var(i)` with the same `i` must be
    /// identical type lists. For convenience, these associated consts are provided:
    /// * `TyListPat::TS` for `TyListPat::Var(0)`
    Var(usize),

    /// Uniform repeat type list: equivalent to repeating the inner type list
    /// pattern (which must be finite), enough times to cover the whole list.
    Repeat(&'a TyListPat<'a>),

    /// Empty type list.
    Nil,

    /// One type followed by another type list.
    Cons {
        first: &'a TyPat<'a>,
        suffix: &'a TyListPat<'a>,
    },
}

impl Pat for TyListPat<'_> {
    const ANY: Self = Self::Any;
}

impl TyListPat<'_> {
    pub const TS: Self = Self::Var(0);
}

/// Instruction "signature", dynamic representation (see module-level docs).
#[derive(Copy, Clone, Debug)]
pub struct InstSig<'a> {
    /// Pattern for an instruction's sole storage class operand, if applicable.
    // FIXME(eddyb) integrate this with `input_types` - it's non-trivial because
    // that matches *the types of* ID operands, not the operands themselves.
    pub storage_class: Option<&'a StorageClassPat>,

    /// Patterns for the complete list of types of the instruction's ID operands,
    /// where non-value operands (i.e. IDs of instructions without a *Result Type*)
    /// can only match `TyPat::Any`.
    pub input_types: &'a TyListPat<'a>,

    /// Pattern for the instruction's *Result Type* operand, if applicable.
    pub output_type: Option<&'a TyPat<'a>>,
}

/// Returns an array of valid signatures for an instruction with opcode `op`,
/// or `None` if there aren't any known type constraints for that instruction.
pub fn instruction_signatures(op: Op) -> Option<&'static [InstSig<'static>]> {
    // Restrict the names the `pat!` macro can take as pattern constructors.
    mod pat_ctors {
        pub const S: super::StorageClassPat = super::StorageClassPat::S;
        // NOTE(eddyb) it would be really nice if we could import `TyPat::{* - Any, Var}`,
        // i.e. all but those two variants.
        pub use super::TyPat::{
            Array, Either, Function, Image, IndexComposite, Matrix, Pipe, Pointer, SampledImage,
            Struct, Vector, Vector4, Void,
        };
        pub const T: super::TyPat<'_> = super::TyPat::T;
        pub use super::TyListPat::Repeat;
        pub const TS: super::TyListPat<'_> = super::TyListPat::TS;
    }

    macro_rules! pat {
        (_) => { &Pat::ANY };
        ($ctor:ident $(($($inner:tt $(($($inner_args:tt)+))?),+))?) => {
            &pat_ctors::$ctor $(($(pat!($inner $(($($inner_args)+))?)),+))?
        };
        ([]) => { &TyListPat::Nil };
        ([..]) => { &TyListPat::Any };
        ([...$($rest:tt)+]) => { pat!($($rest)+) };
        ([$first:tt $(($($first_args:tt)+))? $(, $($rest:tt)*)?]) => {
            &TyListPat::Cons {
                first: pat!($first $(($($first_args)+))?),
                suffix: pat!([$($($rest)*)?]),
            }
        }
    }
    macro_rules! optionify {
        () => {
            None
        };
        ($x:expr) => {
            Some($x)
        };
    }
    macro_rules! sig {
        ($(
            $({$($storage_class:tt)*})?
            ($($in_tys:tt)*)
            $(-> $out_ty:tt $(($($out_ty_args:tt)+))?)?
        )|+) => {
            return Some(&[$(InstSig {
                storage_class: optionify!($(pat!($($storage_class)*))?),
                input_types: pat!([$($in_tys)*]),
                output_type: optionify!($(pat!($out_ty $(($($out_ty_args)+))?))?),
            }),*])
        };
    }

    macro_rules! reserved {
        ($ext:ident) => {
            unreachable!(concat!("Op{:?} is reserved for ", stringify!($ext)), op)
        };
    }

    #[allow(clippy::match_same_arms)]
    match op {
        // 3.37.1. Miscellaneous Instructions
        Op::Nop | Op::Undef => {}
        // SPIR-V 1.1, Capability: Addresses
        Op::SizeOf => {}

        // 3.37.2. Debug Instructions
        Op::SourceContinued
        | Op::Source
        | Op::SourceExtension
        | Op::Name
        | Op::MemberName
        | Op::String
        | Op::Line => {}
        //
        Op::NoLine => {}
        // SPIR-V 1.1
        Op::ModuleProcessed => {}

        // 3.37.3. Annotation Instructions
        Op::Decorate
        | Op::MemberDecorate
        | Op::DecorationGroup
        | Op::GroupDecorate
        | Op::GroupMemberDecorate => {}
        // SPIR-V 1.2
        Op::DecorateId => {}
        // SPIR-V 1.4
        Op::DecorateString | Op::MemberDecorateString => {}

        // 3.37.4. Extension Instructions
        Op::Extension | Op::ExtInstImport | Op::ExtInst => {}

        // 3.37.5. Mode-Setting Instructions
        Op::MemoryModel | Op::EntryPoint | Op::ExecutionMode | Op::Capability => {}
        // SPIR-V 1.2
        Op::ExecutionModeId => {}

        // 3.37.6. Type-Declaration Instructions
        Op::TypeVoid
        | Op::TypeBool
        | Op::TypeInt
        | Op::TypeFloat
        | Op::TypeVector
        | Op::TypeMatrix
        | Op::TypeImage
        | Op::TypeSampler
        | Op::TypeSampledImage
        | Op::TypeArray
        | Op::TypeRuntimeArray
        | Op::TypeStruct
        | Op::TypeOpaque
        | Op::TypePointer
        | Op::TypeFunction
        | Op::TypeEvent
        | Op::TypeDeviceEvent
        | Op::TypeReserveId
        | Op::TypeQueue
        | Op::TypePipe
        | Op::TypeForwardPointer => {}
        // SPIR-V 1.1, Capability: PipeStorage
        Op::TypePipeStorage => {}
        // SPIR-V 1.1, Capability: NamedBarrier
        Op::TypeNamedBarrier => {}

        // 3.37.7. Constant-Creation Instructions
        Op::ConstantTrue | Op::ConstantFalse | Op::Constant => {}
        Op::ConstantComposite | Op::SpecConstantComposite => sig! {
            (...TS) -> Struct(TS) |
            (...Repeat([T])) -> Array(T) |
            (...Repeat([T])) -> Vector(T) |
            (...Repeat([T])) -> Matrix(T)
        },
        Op::ConstantSampler
        | Op::ConstantNull
        | Op::SpecConstantTrue
        | Op::SpecConstantFalse
        | Op::SpecConstant => {}
        Op::SpecConstantOp => {
            unreachable!(
                "Op{:?} should be specially handled outside spirv_type_constraints",
                op
            );
        }

        // 3.37.8. Memory Instructions
        Op::Variable => sig! {
            {S} () -> Pointer(S, _) |
            {S} (T) -> Pointer(S, T)
        },
        Op::ImageTexelPointer => sig! { (Pointer(_, Image(T)), _, _) -> Pointer(_, T) },
        Op::Load => sig! { (Pointer(_, T)) -> T },
        Op::Store => sig! { (Pointer(_, T), T) },
        Op::CopyMemory => sig! { (Pointer(_, T), Pointer(_, T)) },
        Op::CopyMemorySized => {}
        Op::AccessChain | Op::InBoundsAccessChain => sig! {
            (Pointer(S, T), ../*indices*/) -> Pointer(S, IndexComposite(T))
        },
        Op::PtrAccessChain | Op::InBoundsPtrAccessChain => sig! {
            (Pointer(S, T), _, ../*indices*/) -> Pointer(S, IndexComposite(T))
        },
        Op::ArrayLength | Op::GenericPtrMemSemantics => {}
        // SPIR-V 1.4
        Op::PtrEqual | Op::PtrNotEqual | Op::PtrDiff => sig! {
            (Pointer(_, T), Pointer(_, T)) -> _
        },

        // 3.37.9. Function Instructions
        Op::Function => {}
        Op::FunctionParameter | Op::FunctionEnd => {
            unreachable!(
                "Op{:?} should be specially handled outside spirv_type_constraints",
                op
            );
        }
        Op::FunctionCall => sig! { (Function(T, TS), ...TS) -> T },

        // 3.37.10. Image Instructions
        Op::SampledImage => sig! { (T, _) -> SampledImage(T) },
        Op::ImageSampleImplicitLod
        | Op::ImageSampleExplicitLod
        | Op::ImageSampleProjImplicitLod
        | Op::ImageSampleProjExplicitLod
        | Op::ImageGather
        | Op::ImageDrefGather => sig! {
            (SampledImage(Image(Either(Void, T))), ..) -> Vector4(T)
        },
        Op::ImageSampleDrefImplicitLod
        | Op::ImageSampleDrefExplicitLod
        | Op::ImageSampleProjDrefImplicitLod
        | Op::ImageSampleProjDrefExplicitLod => sig! {
            (SampledImage(Image(T)), ..) -> T
        },
        Op::ImageFetch => sig! {
            (Image(Either(Void, T)), ..) -> Vector4(T)
        },
        Op::ImageRead => sig! {
            (Image(Either(Void, T)), ..) -> Either(Vector(T), T)
        },
        Op::ImageWrite => sig! {
            (Image(Either(Void, T)), _, Either(Vector(T), T))
        },
        Op::Image => sig! { (SampledImage(T)) -> T },
        Op::ImageQueryFormat
        | Op::ImageQueryOrder
        | Op::ImageQuerySizeLod
        | Op::ImageQuerySize
        | Op::ImageQueryLod
        | Op::ImageQueryLevels
        | Op::ImageQuerySamples => {}
        // Capability: SparseResidency
        Op::ImageSparseSampleImplicitLod
        | Op::ImageSparseSampleExplicitLod
        | Op::ImageSparseSampleProjImplicitLod
        | Op::ImageSparseSampleProjExplicitLod
        | Op::ImageSparseGather
        | Op::ImageSparseDrefGather => sig! {
            (SampledImage(Image(Either(Void, T))), ..) -> Struct([_, Vector4(T)])
        },
        Op::ImageSparseSampleDrefImplicitLod
        | Op::ImageSparseSampleDrefExplicitLod
        | Op::ImageSparseSampleProjDrefImplicitLod
        | Op::ImageSparseSampleProjDrefExplicitLod => sig! {
            (SampledImage(Image(T)), ..) -> Struct([_, T])
        },
        Op::ImageSparseFetch => sig! {
            (Image(Either(Void, T)), ..) -> Struct([_, Vector4(T)])
        },
        Op::ImageSparseTexelsResident => {}
        // Capability: SparseResidency
        Op::ImageSparseRead => sig! {
            (Image(Either(Void, T)), ..) -> Struct([_, Either(Vector(T), T)])
        },
        // SPV_NV_shader_image_footprint
        Op::ImageSampleFootprintNV => {}

        // 3.37.11. Conversion Instructions
        Op::ConvertFToU
        | Op::ConvertFToS
        | Op::ConvertSToF
        | Op::ConvertUToF
        | Op::UConvert
        | Op::SConvert
        | Op::FConvert => {}
        Op::QuantizeToF16 => sig! { (T) -> T },
        Op::ConvertPtrToU | Op::SatConvertSToU | Op::SatConvertUToS | Op::ConvertUToPtr => {}
        Op::PtrCastToGeneric | Op::GenericCastToPtr => sig! { (Pointer(_, T)) -> Pointer(_, T) },
        Op::GenericCastToPtrExplicit => sig! { {S} (Pointer(_, T)) -> Pointer(S, T) },
        Op::Bitcast => {}

        // 3.37.12. Composite Instructions
        Op::VectorExtractDynamic => sig! { (Vector(T), _) -> T },
        Op::VectorInsertDynamic => sig! {
            // FIXME(eddyb) was `(Vector(T), T, _) -> Vector(T)` but that was
            // missing an equality constraint between input and output vectors;
            // we should use `(Vector(T, N), T, _) -> Vector(T, N)`, or constrain
            // input and output vectors to have the same type some other way.
            (T, _, _) -> T
        },
        Op::VectorShuffle => sig! { (Vector(T), Vector(T)) -> Vector(T) },
        Op::CompositeConstruct => sig! {
            (...TS) -> Struct(TS) |
            (...Repeat([T])) -> Array(T) |
            (...Repeat([T])) -> Matrix(T) |
            (...Repeat([Either(Vector(T), T)])) -> Vector(T)
        },
        Op::CompositeExtract => sig! { (T, ../*indices*/) -> IndexComposite(T) },
        Op::CompositeInsert => sig! { (IndexComposite(T), T, ../*indices*/) -> T },
        Op::CopyObject => sig! { (T) -> T },
        Op::Transpose => sig! { (Matrix(Vector(T))) -> Matrix(Vector(T)) },
        // SPIR-V 1.4
        Op::CopyLogical => sig! {
            // FIXME(eddyb) this is shallow right now, it should recurse instead
            (Array(T)) -> Array(T) |
            (Struct(TS)) -> Struct(TS)
        },

        // 3.37.13. Arithmetic Instructions
        Op::SNegate => {}
        Op::FNegate => sig! { (T) -> T },
        Op::IAdd | Op::ISub | Op::IMul | Op::SDiv | Op::SRem | Op::SMod => {}
        Op::FAdd | Op::FSub | Op::FMul | Op::UDiv | Op::FDiv | Op::UMod | Op::FRem | Op::FMod => {
            sig! { (T, T) -> T }
        }
        Op::VectorTimesScalar => sig! {
            // FIXME(eddyb) missing equality constraint between input and output vectors.
            (Vector(T), T) -> Vector(T)
        },
        Op::MatrixTimesScalar => sig! {
            // FIXME(eddyb) missing equality constraint between input and output matrices.
            (Matrix(Vector(T)), T) -> Matrix(Vector(T))
        },
        Op::VectorTimesMatrix => sig! { (Vector(T), Matrix(Vector(T))) -> Vector(T) },
        Op::MatrixTimesVector => sig! {
            // FIXME(eddyb) missing equality constraint between input vector and
            // the matrix' column vector.
            (Matrix(Vector(T)), Vector(T)) -> Vector(T)
        },
        Op::MatrixTimesMatrix => sig! {
            // FIXME(eddyb) missing equality constraint between the column vectors
            // of the first input and of the output.
            (Matrix(Vector(T)), Matrix(Vector(T))) -> Matrix(Vector(T))
        },
        Op::OuterProduct => sig! {
            // FIXME(eddyb) missing equality constraint between first input vector and
            // the matrix' column vector.
            (Vector(T), Vector(T)) -> Matrix(Vector(T))
        },
        Op::Dot => sig! {
            // FIXME(eddyb) missing equality constraint between two vectors
            (Vector(T), T) -> Vector(T)
        },
        Op::IAddCarry | Op::ISubBorrow | Op::UMulExtended | Op::SMulExtended => sig! {
            (T, T) -> Struct([T, T])
        },

        // 3.37.14. Bit Instructions
        Op::ShiftRightLogical
        | Op::ShiftRightArithmetic
        | Op::ShiftLeftLogical
        | Op::BitwiseOr
        | Op::BitwiseXor
        | Op::BitwiseAnd
        | Op::Not => {}
        Op::BitFieldInsert => sig! { (T, T, _, _) -> T },
        Op::BitFieldSExtract | Op::BitFieldUExtract => sig! { (T, _, _) -> T },
        Op::BitReverse => sig! { (T) -> T },
        Op::BitCount => {}

        // 3.37.15. Relational and Logical Instructions
        Op::Any
        | Op::All
        | Op::IsNan
        | Op::IsInf
        | Op::IsFinite
        | Op::IsNormal
        | Op::SignBitSet => {}
        Op::LessOrGreater | Op::Ordered | Op::Unordered => sig! { (T, T) -> _ },
        Op::LogicalEqual | Op::LogicalNotEqual | Op::LogicalOr | Op::LogicalAnd => sig! {
            (T, T) -> T
        },
        Op::LogicalNot => sig! { (T) -> T },
        Op::Select => sig! { (_, T, T) -> T },
        Op::IEqual
        | Op::INotEqual
        | Op::UGreaterThan
        | Op::SGreaterThan
        | Op::UGreaterThanEqual
        | Op::SGreaterThanEqual
        | Op::ULessThan
        | Op::SLessThan
        | Op::ULessThanEqual
        | Op::SLessThanEqual => {}
        Op::FOrdEqual
        | Op::FUnordEqual
        | Op::FOrdNotEqual
        | Op::FUnordNotEqual
        | Op::FOrdLessThan
        | Op::FUnordLessThan
        | Op::FOrdGreaterThan
        | Op::FUnordGreaterThan
        | Op::FOrdLessThanEqual
        | Op::FUnordLessThanEqual
        | Op::FOrdGreaterThanEqual
        | Op::FUnordGreaterThanEqual => sig! { (T, T) -> _ },

        // 3.37.16. Derivative Instructions
        Op::DPdx
        | Op::DPdy
        | Op::Fwidth
        | Op::DPdxFine
        | Op::DPdyFine
        | Op::FwidthFine
        | Op::DPdxCoarse
        | Op::DPdyCoarse
        | Op::FwidthCoarse => sig! { (T) -> T },

        // 3.37.17. Control-Flow Instructions
        Op::Phi => sig! { (...Repeat([T, _])) -> T },
        Op::LoopMerge
        | Op::SelectionMerge
        | Op::Label
        | Op::Branch
        | Op::BranchConditional
        | Op::Switch
        | Op::Kill => {}
        Op::Return | Op::ReturnValue => {
            unreachable!(
                "Op{:?} should be specially handled outside spirv_type_constraints",
                op
            );
        }
        Op::Unreachable | Op::LifetimeStart | Op::LifetimeStop | Op::TerminateInvocation => {}

        // 3.37.18. Atomic Instructions
        Op::AtomicLoad | Op::AtomicIIncrement | Op::AtomicIDecrement => sig! {
            (Pointer(_, T), _, _) -> T
        },
        Op::AtomicStore => sig! { (Pointer(_, T), _, _, T) },
        Op::AtomicExchange
        | Op::AtomicIAdd
        | Op::AtomicISub
        | Op::AtomicSMin
        | Op::AtomicUMin
        | Op::AtomicSMax
        | Op::AtomicUMax
        | Op::AtomicAnd
        | Op::AtomicOr
        | Op::AtomicXor => sig! { (Pointer(_, T), _, _, T) -> T },
        Op::AtomicCompareExchange | Op::AtomicCompareExchangeWeak => sig! {
            (Pointer(_, T), _, _, _, T, T) -> T
        },
        // Capability: Kernel
        Op::AtomicFlagTestAndSet | Op::AtomicFlagClear => {}
        // SPV_EXT_shader_atomic_float_add
        Op::AtomicFAddEXT => sig! { (Pointer(_, T), _, _, T) -> T },

        // 3.37.19. Primitive Instructions
        Op::EmitVertex | Op::EndPrimitive | Op::EmitStreamVertex | Op::EndStreamPrimitive => {}

        // 3.37.20. Barrier Instructions
        Op::ControlBarrier | Op::MemoryBarrier => {}
        // SPIR-V 1.1, Capability: NamedBarrier
        Op::NamedBarrierInitialize | Op::MemoryNamedBarrier => {}

        // 3.37.21. Group and Subgroup Instructions
        Op::GroupAsyncCopy => sig! { (_, Pointer(_, T), Pointer(_, T), _, _, _) -> _ },
        Op::GroupWaitEvents => {}
        Op::GroupAll | Op::GroupAny => {}
        Op::GroupBroadcast => sig! { (_, T, _) -> T },
        Op::GroupIAdd
        | Op::GroupFAdd
        | Op::GroupFMin
        | Op::GroupUMin
        | Op::GroupSMin
        | Op::GroupFMax
        | Op::GroupUMax
        | Op::GroupSMax => sig! { (_, T) -> T },
        // SPV_KHR_shader_ballot
        Op::SubgroupBallotKHR => {}
        Op::SubgroupFirstInvocationKHR => sig! { (T) -> T },
        // SPV_KHR_subgroup_vote
        Op::SubgroupAllKHR | Op::SubgroupAnyKHR | Op::SubgroupAllEqualKHR => {}
        // SPV_KHR_shader_ballot
        Op::SubgroupReadInvocationKHR => sig! { (T, _) -> T },
        // SPV_AMD_shader_ballot
        Op::GroupIAddNonUniformAMD
        | Op::GroupFAddNonUniformAMD
        | Op::GroupFMinNonUniformAMD
        | Op::GroupUMinNonUniformAMD
        | Op::GroupSMinNonUniformAMD
        | Op::GroupFMaxNonUniformAMD
        | Op::GroupUMaxNonUniformAMD
        | Op::GroupSMaxNonUniformAMD => sig! { (_, T) -> T },
        // SPV_INTEL_subgroups
        Op::SubgroupShuffleINTEL | Op::SubgroupShuffleXorINTEL => sig! { (T, _) -> T },
        Op::SubgroupShuffleDownINTEL | Op::SubgroupShuffleUpINTEL => sig! { (T, T, _) -> T },
        Op::SubgroupBlockReadINTEL => sig! { (Pointer(_, T)) -> T },
        Op::SubgroupBlockWriteINTEL => sig! { (Pointer(_, T), T) },
        Op::SubgroupImageBlockReadINTEL | Op::SubgroupImageBlockWriteINTEL => {}
        // SPV_INTEL_media_block_io
        Op::SubgroupImageMediaBlockReadINTEL | Op::SubgroupImageMediaBlockWriteINTEL => {}

        // 3.37.22. Device-Side Enqueue Instructions
        Op::EnqueueMarker
        | Op::EnqueueKernel
        | Op::GetKernelNDrangeSubGroupCount
        | Op::GetKernelNDrangeMaxSubGroupSize
        | Op::GetKernelWorkGroupSize
        | Op::GetKernelPreferredWorkGroupSizeMultiple
        | Op::RetainEvent
        | Op::ReleaseEvent
        | Op::CreateUserEvent
        | Op::IsValidEvent
        | Op::SetUserEventStatus
        | Op::CaptureEventProfilingInfo
        | Op::GetDefaultQueue => {}
        Op::BuildNDRange => sig! { (T, T, T) -> _ },
        // SPIR-V 1.1, Capability: SubgroupDispatch
        Op::GetKernelLocalSizeForSubgroupCount | Op::GetKernelMaxNumSubgroups => {}

        // 3.37.23. Pipe Instructions
        Op::ReadPipe | Op::WritePipe => sig! { (Pipe(T), Pointer(_, T), _, _) -> _ },
        Op::ReservedReadPipe | Op::ReservedWritePipe => sig! {
            (Pipe(T), _, _, Pointer(_, T), _, _) -> _
        },
        Op::ReserveReadPipePackets
        | Op::ReserveWritePipePackets
        | Op::CommitReadPipe
        | Op::CommitWritePipe
        | Op::IsValidReserveId
        | Op::GetNumPipePackets
        | Op::GetMaxPipePackets
        | Op::GroupReserveReadPipePackets
        | Op::GroupReserveWritePipePackets
        | Op::GroupCommitReadPipe
        | Op::GroupCommitWritePipe => {}
        // SPIR-V 1.1, Capability: PipeStorage
        Op::ConstantPipeStorage | Op::CreatePipeFromPipeStorage => {}
        // SPV_INTEL_blocking_pipes
        Op::ReadPipeBlockingINTEL | Op::WritePipeBlockingINTEL => sig! {
            (Pipe(T), Pointer(_, T), _, _)
        },

        // 3.37.24. Non-Uniform Instructions
        Op::GroupNonUniformElect
        | Op::GroupNonUniformAll
        | Op::GroupNonUniformAny
        | Op::GroupNonUniformAllEqual => {}
        Op::GroupNonUniformBroadcast => sig! { (_, T, _) -> T },
        Op::GroupNonUniformBroadcastFirst => sig! { (_, T) -> T },
        Op::GroupNonUniformBallot
        | Op::GroupNonUniformInverseBallot
        | Op::GroupNonUniformBallotBitExtract
        | Op::GroupNonUniformBallotBitCount
        | Op::GroupNonUniformBallotFindLSB
        | Op::GroupNonUniformBallotFindMSB => {}
        Op::GroupNonUniformShuffle
        | Op::GroupNonUniformShuffleXor
        | Op::GroupNonUniformShuffleUp
        | Op::GroupNonUniformShuffleDown
        | Op::GroupNonUniformIAdd
        | Op::GroupNonUniformFAdd
        | Op::GroupNonUniformIMul
        | Op::GroupNonUniformFMul
        | Op::GroupNonUniformSMin
        | Op::GroupNonUniformUMin
        | Op::GroupNonUniformFMin
        | Op::GroupNonUniformSMax
        | Op::GroupNonUniformUMax
        | Op::GroupNonUniformFMax
        | Op::GroupNonUniformBitwiseAnd
        | Op::GroupNonUniformBitwiseOr
        | Op::GroupNonUniformBitwiseXor
        | Op::GroupNonUniformLogicalAnd
        | Op::GroupNonUniformLogicalOr
        | Op::GroupNonUniformLogicalXor
        | Op::GroupNonUniformQuadBroadcast
        | Op::GroupNonUniformQuadSwap => sig! { (_, T, ..) -> T },
        // SPV_NV_shader_subgroup_partitioned
        Op::GroupNonUniformPartitionNV => {}

        // 3.37.25. Reserved Instructions
        // SPV_KHR_ray_tracing
        Op::TraceRayKHR
        | Op::ExecuteCallableKHR
        | Op::ConvertUToAccelerationStructureKHR
        | Op::IgnoreIntersectionKHR
        | Op::TerminateRayKHR => {
            // NOTE(eddyb) we actually use these despite not being in the standard yet.
            // reserved!(SPV_KHR_ray_tracing)
        }
        // SPV_KHR_ray_query
        Op::TypeRayQueryKHR
        | Op::RayQueryInitializeKHR
        | Op::RayQueryTerminateKHR
        | Op::RayQueryGenerateIntersectionKHR
        | Op::RayQueryConfirmIntersectionKHR
        | Op::RayQueryProceedKHR
        | Op::RayQueryGetIntersectionTypeKHR => {
            // NOTE(eddyb) we actually use these despite not being in the standard yet.
            // reserved!(SPV_KHR_ray_query)
        }
        // SPV_AMD_shader_fragment_mask
        Op::FragmentMaskFetchAMD | Op::FragmentFetchAMD => reserved!(SPV_AMD_shader_fragment_mask),
        // SPV_KHR_shader_clock
        Op::ReadClockKHR => {
            // NOTE(eddyb) we actually use these despite not being in the standard yet.
            // reserved!(SPV_KHR_shader_clock)
        }
        // SPV_NV_mesh_shader
        Op::WritePackedPrimitiveIndices4x8NV => reserved!(SPV_NV_mesh_shader),
        // SPV_NV_ray_tracing
        Op::ReportIntersectionNV
        | Op::IgnoreIntersectionNV
        | Op::TerminateRayNV
        | Op::TraceNV
        | Op::TypeAccelerationStructureNV
        | Op::ExecuteCallableNV => {
            // NOTE(eddyb) Some KHR variants are aliased to the the NV instructions.
            // reserved!(SPV_NV_ray_tracing)
        }
        // SPV_NV_cooperative_matrix
        Op::TypeCooperativeMatrixNV
        | Op::CooperativeMatrixLoadNV
        | Op::CooperativeMatrixStoreNV
        | Op::CooperativeMatrixMulAddNV
        | Op::CooperativeMatrixLengthNV => reserved!(SPV_NV_cooperative_matrix),
        // SPV_EXT_fragment_shader_interlock
        Op::BeginInvocationInterlockEXT | Op::EndInvocationInterlockEXT => {
            reserved!(SPV_EXT_fragment_shader_interlock);
        }
        // SPV_EXT_demote_to_helper_invocation
        Op::DemoteToHelperInvocationEXT | Op::IsHelperInvocationEXT => {
            // NOTE(eddyb) we actually use these despite not being in the standard yet.
            // reserved!(SPV_EXT_demote_to_helper_invocation)
        }
        // SPV_INTEL_shader_integer_functions2
        Op::UCountLeadingZerosINTEL | Op::UCountTrailingZerosINTEL => {
            // NOTE(eddyb) we actually use these despite not being in the standard yet.
            // reserved!(SPV_INTEL_shader_integer_functions2)
        }
        Op::AbsISubINTEL
        | Op::AbsUSubINTEL
        | Op::IAddSatINTEL
        | Op::UAddSatINTEL
        | Op::IAverageINTEL
        | Op::UAverageINTEL
        | Op::IAverageRoundedINTEL
        | Op::UAverageRoundedINTEL
        | Op::ISubSatINTEL
        | Op::USubSatINTEL
        | Op::IMul32x16INTEL
        | Op::UMul32x16INTEL => reserved!(SPV_INTEL_shader_integer_functions2),
        // SPV_INTEL_unstructured_loop_controls
        Op::LoopControlINTEL => reserved!(SPV_INTEL_unstructured_loop_controls),
        // SPV_INTEL_fpga_reg
        Op::FPGARegINTEL => reserved!(SPV_INTEL_fpga_reg),
        // SPV_KHR_ray_query
        Op::RayQueryGetRayTMinKHR
        | Op::RayQueryGetRayFlagsKHR
        | Op::RayQueryGetIntersectionTKHR
        | Op::RayQueryGetIntersectionInstanceCustomIndexKHR
        | Op::RayQueryGetIntersectionInstanceIdKHR
        | Op::RayQueryGetIntersectionInstanceShaderBindingTableRecordOffsetKHR
        | Op::RayQueryGetIntersectionGeometryIndexKHR
        | Op::RayQueryGetIntersectionPrimitiveIndexKHR
        | Op::RayQueryGetIntersectionBarycentricsKHR
        | Op::RayQueryGetIntersectionFrontFaceKHR
        | Op::RayQueryGetIntersectionCandidateAABBOpaqueKHR
        | Op::RayQueryGetIntersectionObjectRayDirectionKHR
        | Op::RayQueryGetIntersectionObjectRayOriginKHR
        | Op::RayQueryGetWorldRayDirectionKHR
        | Op::RayQueryGetWorldRayOriginKHR
        | Op::RayQueryGetIntersectionObjectToWorldKHR
        | Op::RayQueryGetIntersectionWorldToObjectKHR => {
            // NOTE(eddyb) we actually use these despite not being in the standard yet.
            // reserved!(SPV_KHR_ray_query)
        }

        // Instructions not present in current SPIR-V specification
        // SPV_INTEL_function_pointers
        Op::FunctionPointerINTEL | Op::FunctionPointerCallINTEL => {
            reserved!(SPV_INTEL_function_pointers);
        }
        // SPV_INTEL_device_side_avc_motion_estimation
        Op::VmeImageINTEL
        | Op::TypeVmeImageINTEL
        | Op::TypeAvcImePayloadINTEL
        | Op::TypeAvcRefPayloadINTEL
        | Op::TypeAvcSicPayloadINTEL
        | Op::TypeAvcMcePayloadINTEL
        | Op::TypeAvcMceResultINTEL
        | Op::TypeAvcImeResultINTEL
        | Op::TypeAvcImeResultSingleReferenceStreamoutINTEL
        | Op::TypeAvcImeResultDualReferenceStreamoutINTEL
        | Op::TypeAvcImeSingleReferenceStreaminINTEL
        | Op::TypeAvcImeDualReferenceStreaminINTEL
        | Op::TypeAvcRefResultINTEL
        | Op::TypeAvcSicResultINTEL
        | Op::SubgroupAvcMceGetDefaultInterBaseMultiReferencePenaltyINTEL
        | Op::SubgroupAvcMceSetInterBaseMultiReferencePenaltyINTEL
        | Op::SubgroupAvcMceGetDefaultInterShapePenaltyINTEL
        | Op::SubgroupAvcMceSetInterShapePenaltyINTEL
        | Op::SubgroupAvcMceGetDefaultInterDirectionPenaltyINTEL
        | Op::SubgroupAvcMceSetInterDirectionPenaltyINTEL
        | Op::SubgroupAvcMceGetDefaultIntraLumaShapePenaltyINTEL
        | Op::SubgroupAvcMceGetDefaultInterMotionVectorCostTableINTEL
        | Op::SubgroupAvcMceGetDefaultHighPenaltyCostTableINTEL
        | Op::SubgroupAvcMceGetDefaultMediumPenaltyCostTableINTEL
        | Op::SubgroupAvcMceGetDefaultLowPenaltyCostTableINTEL
        | Op::SubgroupAvcMceSetMotionVectorCostFunctionINTEL
        | Op::SubgroupAvcMceGetDefaultIntraLumaModePenaltyINTEL
        | Op::SubgroupAvcMceGetDefaultNonDcLumaIntraPenaltyINTEL
        | Op::SubgroupAvcMceGetDefaultIntraChromaModeBasePenaltyINTEL
        | Op::SubgroupAvcMceSetAcOnlyHaarINTEL
        | Op::SubgroupAvcMceSetSourceInterlacedFieldPolarityINTEL
        | Op::SubgroupAvcMceSetSingleReferenceInterlacedFieldPolarityINTEL
        | Op::SubgroupAvcMceSetDualReferenceInterlacedFieldPolaritiesINTEL
        | Op::SubgroupAvcMceConvertToImePayloadINTEL
        | Op::SubgroupAvcMceConvertToImeResultINTEL
        | Op::SubgroupAvcMceConvertToRefPayloadINTEL
        | Op::SubgroupAvcMceConvertToRefResultINTEL
        | Op::SubgroupAvcMceConvertToSicPayloadINTEL
        | Op::SubgroupAvcMceConvertToSicResultINTEL
        | Op::SubgroupAvcMceGetMotionVectorsINTEL
        | Op::SubgroupAvcMceGetInterDistortionsINTEL
        | Op::SubgroupAvcMceGetBestInterDistortionsINTEL
        | Op::SubgroupAvcMceGetInterMajorShapeINTEL
        | Op::SubgroupAvcMceGetInterMinorShapeINTEL
        | Op::SubgroupAvcMceGetInterDirectionsINTEL
        | Op::SubgroupAvcMceGetInterMotionVectorCountINTEL
        | Op::SubgroupAvcMceGetInterReferenceIdsINTEL
        | Op::SubgroupAvcMceGetInterReferenceInterlacedFieldPolaritiesINTEL
        | Op::SubgroupAvcImeInitializeINTEL
        | Op::SubgroupAvcImeSetSingleReferenceINTEL
        | Op::SubgroupAvcImeSetDualReferenceINTEL
        | Op::SubgroupAvcImeRefWindowSizeINTEL
        | Op::SubgroupAvcImeAdjustRefOffsetINTEL
        | Op::SubgroupAvcImeConvertToMcePayloadINTEL
        | Op::SubgroupAvcImeSetMaxMotionVectorCountINTEL
        | Op::SubgroupAvcImeSetUnidirectionalMixDisableINTEL
        | Op::SubgroupAvcImeSetEarlySearchTerminationThresholdINTEL
        | Op::SubgroupAvcImeSetWeightedSadINTEL
        | Op::SubgroupAvcImeEvaluateWithSingleReferenceINTEL
        | Op::SubgroupAvcImeEvaluateWithDualReferenceINTEL
        | Op::SubgroupAvcImeEvaluateWithSingleReferenceStreaminINTEL
        | Op::SubgroupAvcImeEvaluateWithDualReferenceStreaminINTEL
        | Op::SubgroupAvcImeEvaluateWithSingleReferenceStreamoutINTEL
        | Op::SubgroupAvcImeEvaluateWithDualReferenceStreamoutINTEL
        | Op::SubgroupAvcImeEvaluateWithSingleReferenceStreaminoutINTEL
        | Op::SubgroupAvcImeEvaluateWithDualReferenceStreaminoutINTEL
        | Op::SubgroupAvcImeConvertToMceResultINTEL
        | Op::SubgroupAvcImeGetSingleReferenceStreaminINTEL
        | Op::SubgroupAvcImeGetDualReferenceStreaminINTEL
        | Op::SubgroupAvcImeStripSingleReferenceStreamoutINTEL
        | Op::SubgroupAvcImeStripDualReferenceStreamoutINTEL
        | Op::SubgroupAvcImeGetStreamoutSingleReferenceMajorShapeMotionVectorsINTEL
        | Op::SubgroupAvcImeGetStreamoutSingleReferenceMajorShapeDistortionsINTEL
        | Op::SubgroupAvcImeGetStreamoutSingleReferenceMajorShapeReferenceIdsINTEL
        | Op::SubgroupAvcImeGetStreamoutDualReferenceMajorShapeMotionVectorsINTEL
        | Op::SubgroupAvcImeGetStreamoutDualReferenceMajorShapeDistortionsINTEL
        | Op::SubgroupAvcImeGetStreamoutDualReferenceMajorShapeReferenceIdsINTEL
        | Op::SubgroupAvcImeGetBorderReachedINTEL
        | Op::SubgroupAvcImeGetTruncatedSearchIndicationINTEL
        | Op::SubgroupAvcImeGetUnidirectionalEarlySearchTerminationINTEL
        | Op::SubgroupAvcImeGetWeightingPatternMinimumMotionVectorINTEL
        | Op::SubgroupAvcImeGetWeightingPatternMinimumDistortionINTEL
        | Op::SubgroupAvcFmeInitializeINTEL
        | Op::SubgroupAvcBmeInitializeINTEL
        | Op::SubgroupAvcRefConvertToMcePayloadINTEL
        | Op::SubgroupAvcRefSetBidirectionalMixDisableINTEL
        | Op::SubgroupAvcRefSetBilinearFilterEnableINTEL
        | Op::SubgroupAvcRefEvaluateWithSingleReferenceINTEL
        | Op::SubgroupAvcRefEvaluateWithDualReferenceINTEL
        | Op::SubgroupAvcRefEvaluateWithMultiReferenceINTEL
        | Op::SubgroupAvcRefEvaluateWithMultiReferenceInterlacedINTEL
        | Op::SubgroupAvcRefConvertToMceResultINTEL
        | Op::SubgroupAvcSicInitializeINTEL
        | Op::SubgroupAvcSicConfigureSkcINTEL
        | Op::SubgroupAvcSicConfigureIpeLumaINTEL
        | Op::SubgroupAvcSicConfigureIpeLumaChromaINTEL
        | Op::SubgroupAvcSicGetMotionVectorMaskINTEL
        | Op::SubgroupAvcSicConvertToMcePayloadINTEL
        | Op::SubgroupAvcSicSetIntraLumaShapePenaltyINTEL
        | Op::SubgroupAvcSicSetIntraLumaModeCostFunctionINTEL
        | Op::SubgroupAvcSicSetIntraChromaModeCostFunctionINTEL
        | Op::SubgroupAvcSicSetBilinearFilterEnableINTEL
        | Op::SubgroupAvcSicSetSkcForwardTransformEnableINTEL
        | Op::SubgroupAvcSicSetBlockBasedRawSkipSadINTEL
        | Op::SubgroupAvcSicEvaluateIpeINTEL
        | Op::SubgroupAvcSicEvaluateWithSingleReferenceINTEL
        | Op::SubgroupAvcSicEvaluateWithDualReferenceINTEL
        | Op::SubgroupAvcSicEvaluateWithMultiReferenceINTEL
        | Op::SubgroupAvcSicEvaluateWithMultiReferenceInterlacedINTEL
        | Op::SubgroupAvcSicConvertToMceResultINTEL
        | Op::SubgroupAvcSicGetIpeLumaShapeINTEL
        | Op::SubgroupAvcSicGetBestIpeLumaDistortionINTEL
        | Op::SubgroupAvcSicGetBestIpeChromaDistortionINTEL
        | Op::SubgroupAvcSicGetPackedIpeLumaModesINTEL
        | Op::SubgroupAvcSicGetIpeChromaModeINTEL
        | Op::SubgroupAvcSicGetPackedSkcLumaCountThresholdINTEL
        | Op::SubgroupAvcSicGetPackedSkcLumaSumThresholdINTEL
        | Op::SubgroupAvcSicGetInterRawSadsINTEL => {
            reserved!(SPV_INTEL_device_side_avc_motion_estimation);
        }
    }

    None
}
