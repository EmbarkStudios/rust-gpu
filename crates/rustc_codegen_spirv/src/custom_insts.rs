//! SPIR-V (extended) instructions specific to `rustc_codegen_spirv`, produced
//! during the original codegen of a crate, and consumed by the `linker`.

use lazy_static::lazy_static;
use rspirv::dr::{Instruction, Operand};
use rspirv::spirv::Op;
use smallvec::SmallVec;

/// Prefix for `CUSTOM_EXT_INST_SET` (`OpExtInstImport` "instruction set" name),
/// without any of the disambiguating suffixes added for specific revisions.
///
/// This **should not** be changed (if possible), to ensure version mismatches
/// can be detected (i.e. starting with this prefix, but the full name differs).
///
/// See `CUSTOM_EXT_INST_SET`'s docs for further constraints on the full name.
pub const CUSTOM_EXT_INST_SET_PREFIX: &str = concat!("Rust.", env!("CARGO_PKG_NAME"), ".");

lazy_static! {
    /// `OpExtInstImport` "instruction set" name for all Rust-GPU instructions.
    ///
    /// These considerations are relevant to the specific choice of name:
    /// * does *not* start with `NonSemantic.`, as:
    ///   * some custom instructions may need to be semantic
    ///   * these custom instructions are not meant for the final SPIR-V
    ///     (so no third-party support is *technically* required for them)
    ///   * `NonSemantic.` requires SPIR-V 1.6 (or `SPV_KHR_non_semantic_info`)
    /// * always starts with `CUSTOM_EXT_INST_SET_PREFIX` (see also its docs),
    ///   regardless of Rust-GPU version or custom instruction set definition
    /// * contains enough disambiguating information to avoid misinterpretation
    ///   if the definitions of the custom instructions have changed - this is
    ///   achieved by hashing the `SCHEMA` constant from `def_custom_insts!` below
    pub static ref CUSTOM_EXT_INST_SET: String = {
        const VER_MAJOR: &str = env!("CARGO_PKG_VERSION_MAJOR");
        const VER_MINOR: &str = env!("CARGO_PKG_VERSION_MINOR");
        const VER_PATCH: &str = env!("CARGO_PKG_VERSION_PATCH");

        let schema_hash = {
            use rustc_data_structures::stable_hasher::StableHasher;
            use std::hash::Hash;

            let mut hasher = StableHasher::new();
            SCHEMA.hash(&mut hasher);
            let (lo, hi) = hasher.finalize();
            (lo as u128) | ((hi as u128) << 64)
        };

        format!("{CUSTOM_EXT_INST_SET_PREFIX}{VER_MAJOR}_{VER_MINOR}_{VER_PATCH}.{schema_hash:x}")
    };
}

macro_rules! def_custom_insts {
    ($($num:literal => $name:ident $({ $($field:ident),+ $(,)? })?),+ $(,)?) => {
        const SCHEMA: &[(u32, &str, &[&str])] = &[
            $(($num, stringify!($name), &[$($(stringify!($field)),+)?])),+
        ];

        #[repr(u32)]
        #[derive(Copy, Clone, Debug, PartialEq, Eq)]
        pub enum CustomOp { $($name = $num),+ }

        impl CustomOp {
            pub fn decode(i: u32) -> Self {
                match i {
                    $($num => Self::$name,)+
                    _ => unreachable!("{i} is not a valid custom instruction number"),
                }
            }

            pub fn decode_from_ext_inst(inst: &Instruction) -> Self {
                assert_eq!(inst.class.opcode, Op::ExtInst);
                Self::decode(inst.operands[1].unwrap_literal_ext_inst_integer())
            }

            pub fn with_operands<T: Clone>(self, operands: &[T]) -> CustomInst<T> {
                match self {
                    $(Self::$name => match operands {
                        [$($($field),+)?] => CustomInst::$name $({ $($field: $field.clone()),+ })?,
                        _ => unreachable!("{self:?} does not have the right number of operands"),
                    }),+
                }
            }
        }

        #[derive(Copy, Clone, Debug)]
        pub enum CustomInst<T> {
            $($name $({ $($field: T),+ })?),+
        }

        impl<T> CustomInst<T> {
            pub fn op(&self) -> CustomOp {
                match *self {
                    $(Self::$name { .. } => CustomOp::$name),+
                }
            }

            // HACK(eddyb) this should return an iterator, but that's too much effort.
            pub fn into_operands(self) -> SmallVec<[T; 8]> {
                match self {
                    $(Self::$name $({ $($field),+ })? => [$($($field),+)?].into_iter().collect()),+
                }
            }
        }

        impl CustomInst<Operand> {
            pub fn decode(inst: &Instruction) -> Self {
                CustomOp::decode_from_ext_inst(inst).with_operands(&inst.operands[2..])
            }
        }
    }
}

// NOTE(eddyb) several of these are similar to `NonSemantic.Shader.DebugInfo.100`
// instructions, but simpler (to aid implementation, for now).
def_custom_insts! {
    // Like `DebugLine` (from `NonSemantic.Shader.DebugInfo.100`) or `OpLine`.
    0 => SetDebugSrcLoc { file, line_start, line_end, col_start, col_end },
    // Like `DebugNoLine` (from `NonSemantic.Shader.DebugInfo.100`) or `OpNoLine`.
    1 => ClearDebugSrcLoc,

    // Similar to `DebugInlinedAt` (from `NonSemantic.Shader.DebugInfo.100`),
    // but simpler: there are no "scope objects", the location of the inlined
    // callsite is given by other debuginfo (`SetDebugSrcLoc`/`OpLine`) active
    // before this instruction, and only the name of the callee is recorded.
    2 => PushInlinedCallFrame { callee_name },
    // Leave the most recent inlined call frame entered by a `PushInlinedCallFrame`
    // (i.e. the inlined call frames form a virtual call stack in debuginfo).
    3 => PopInlinedCallFrame,

    // [Semantic] Similar to some proposed `OpAbort`, but without any ability to
    // indicate abnormal termination (so it's closer to `OpTerminateInvocation`,
    // which we could theoretically use, but that's limited to fragment shaders).
    //
    // Lowering takes advantage of inlining happening before CFG structurization
    // (by forcing inlining of `Abort`s all the way up to entry-points, as to be
    // able to turn the `Abort`s into regular `OpReturn`s, from an entry-point),
    // but if/when inlining works on structured SPIR-T instead, it's not much
    // harder to make any call to a "may (transitively) abort" function branch on
    // an additional returned `bool`, instead (i.e. a form of emulated unwinding).
    //
    // As this is a custom terminator, it must only appear before `OpUnreachable`,
    // *without* any instructions in between (not even debuginfo ones).
    //
    // FIXME(eddyb) long-term this kind of custom control-flow could be generalized
    // to fully emulate unwinding (resulting in codegen similar to `?` in functions
    // returning `Option` or `Result`), to e.g. run destructors, or even allow
    // users to do `catch_unwind` at the top-level of their shader to handle
    // panics specially (e.g. by appending to a custom buffer, or using some
    // specific color in a fragment shader, to indicate a panic happened).
    4 => Abort,
}

impl CustomOp {
    /// Returns `true` iff this `CustomOp` is a custom debuginfo instruction,
    /// i.e. non-semantic (can/must be ignored wherever `OpLine`/`OpNoLine` are).
    pub fn is_debuginfo(self) -> bool {
        match self {
            CustomOp::SetDebugSrcLoc
            | CustomOp::ClearDebugSrcLoc
            | CustomOp::PushInlinedCallFrame
            | CustomOp::PopInlinedCallFrame => true,

            CustomOp::Abort => false,
        }
    }

    /// Returns `true` iff this `CustomOp` is a custom terminator instruction,
    /// i.e. semantic and must always appear just before an `OpUnreachable`
    /// standard terminator (without even debuginfo in between the two).
    pub fn is_terminator(self) -> bool {
        match self {
            CustomOp::SetDebugSrcLoc
            | CustomOp::ClearDebugSrcLoc
            | CustomOp::PushInlinedCallFrame
            | CustomOp::PopInlinedCallFrame => false,

            CustomOp::Abort => true,
        }
    }
}
