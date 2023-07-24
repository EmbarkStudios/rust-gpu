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

macro_rules! join_cargo_pkg_version_major_minor_patch {
    ($sep:literal) => {
        concat!(
            env!("CARGO_PKG_VERSION_MAJOR"),
            $sep,
            env!("CARGO_PKG_VERSION_MINOR"),
            $sep,
            env!("CARGO_PKG_VERSION_PATCH"),
        )
    };
}

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
        let schema_hash = {
            use rustc_data_structures::stable_hasher::StableHasher;
            use std::hash::Hash;

            let mut hasher = StableHasher::new();
            SCHEMA.hash(&mut hasher);
            let (lo, hi) = hasher.finalize();
            (lo as u128) | ((hi as u128) << 64)
        };
        let version = join_cargo_pkg_version_major_minor_patch!("_");
        format!("{CUSTOM_EXT_INST_SET_PREFIX}{version}.{schema_hash:x}")
    };
}

pub fn register_to_spirt_context(cx: &spirt::Context) {
    use spirt::spv::spec::{ExtInstSetDesc, ExtInstSetInstructionDesc};
    cx.register_custom_ext_inst_set(
        &CUSTOM_EXT_INST_SET,
        ExtInstSetDesc {
            // HACK(eddyb) this is the most compact form I've found, that isn't
            // outright lossy by omitting "Rust vs Rust-GPU" or the version.
            short_alias: Some(
                concat!("Rust-GPU ", join_cargo_pkg_version_major_minor_patch!(".")).into(),
            ),
            instructions: SCHEMA
                .iter()
                .map(|&(i, name, operand_names)| {
                    (
                        i,
                        ExtInstSetInstructionDesc {
                            name: name.into(),
                            operand_names: operand_names
                                .iter()
                                .map(|name| {
                                    name.strip_prefix("..")
                                        .unwrap_or(name)
                                        .replace('_', " ")
                                        .into()
                                })
                                .collect(),
                            is_debuginfo: name.contains("Debug")
                                || name.contains("InlinedCallFrame"),
                        },
                    )
                })
                .collect(),
        },
    );
}

macro_rules! def_custom_insts {
    ($($num:literal => $name:ident $({ $($field:ident),+ $(, ..$variadic_field:ident)? $(,)? })?),+ $(,)?) => {
        const SCHEMA: &[(u32, &str, &[&str])] = &[
            $(($num, stringify!($name), &[$($(stringify!($field),)+ $(stringify!(..$variadic_field),)?)?])),+
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
                        [$($($field,)+ $(ref $variadic_field @ ..)?)?] => CustomInst::$name $({
                            $($field: $field.clone(),)+
                            $($variadic_field: $variadic_field.iter().cloned().collect())?
                        })?,
                        _ => unreachable!("{self:?} does not have the right number of operands"),
                    }),+
                }
            }
        }

        #[derive(Clone, Debug)]
        pub enum CustomInst<T> {
            $($name $({ $($field: T,)+ $($variadic_field: SmallVec<[T; 4]>)? })?),+
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
                    $(Self::$name $({ $($field,)+ $($variadic_field)? })? => {
                        [$($($field),+)?].into_iter() $($(.chain($variadic_field))?)? .collect()
                    })+
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
    // with at most debuginfo instructions (standard or custom), between the two.
    //
    // FIXME(eddyb) long-term this kind of custom control-flow could be generalized
    // to fully emulate unwinding (resulting in codegen similar to `?` in functions
    // returning `Option` or `Result`), to e.g. run destructors, or even allow
    // users to do `catch_unwind` at the top-level of their shader to handle
    // panics specially (e.g. by appending to a custom buffer, or using some
    // specific color in a fragment shader, to indicate a panic happened).
    // NOTE(eddyb) `message_debug_printf` operands form a complete `debugPrintf`
    // invocation (format string followed by inputs) for the "message", while
    // `kind` only distinguishes broad categories like `"abort"` vs `"panic"`.
    4 => Abort { kind, ..message_debug_printf },
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
    /// i.e. semantic and must precede an `OpUnreachable` standard terminator,
    /// with at most debuginfo instructions (standard or custom), between the two.
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
