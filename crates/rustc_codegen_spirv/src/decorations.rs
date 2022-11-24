//! SPIR-V decorations specific to `rustc_codegen_spirv`, produced during
//! the original codegen of a crate, and consumed by the `linker`.

use rspirv::dr::{Instruction, Module, Operand};
use rspirv::spirv::{Decoration, Op, Word};
use rustc_span::{source_map::SourceMap, FileName, Pos, Span};
use serde::{Deserialize, Serialize};
use std::marker::PhantomData;
use std::path::PathBuf;
use std::{iter, slice};

/// Decorations not native to SPIR-V require some form of encoding into existing
/// SPIR-V constructs, for which we use `OpDecorateString` with decoration type
/// `UserTypeGOOGLE` and a JSON-encoded Rust value as the decoration string.
///
/// Each decoration type has to implement this trait, and use a different
/// `ENCODING_PREFIX` from any other decoration type, to disambiguate them.
///
/// Also, all decorations have to be stripped by the linker at some point,
/// ideally as soon as they're no longer needed, because no other tools
/// processing the SPIR-V would understand them correctly.
///
/// TODO: uses `non_semantic` instead of piggybacking off of `UserTypeGOOGLE`
/// <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_non_semantic_info.html>
pub trait CustomDecoration: for<'de> Deserialize<'de> + Serialize {
    const ENCODING_PREFIX: &'static str;

    fn encode(self, id: Word) -> Instruction {
        // FIXME(eddyb) this allocates twice, because there is no functionality
        // in `serde_json` for writing to something that impls `fmt::Write`,
        // only for `io::Write`, which would require performing redundant UTF-8
        // (re)validation, or relying on `unsafe` code, to use with `String`.
        let json = serde_json::to_string(&self).unwrap();
        let encoded = [Self::ENCODING_PREFIX, &json].concat();

        Instruction::new(
            Op::DecorateString,
            None,
            None,
            vec![
                Operand::IdRef(id),
                Operand::Decoration(Decoration::UserTypeGOOGLE),
                Operand::LiteralString(encoded),
            ],
        )
    }

    fn try_decode(inst: &Instruction) -> Option<(Word, LazilyDeserialized<'_, Self>)> {
        if inst.class.opcode == Op::DecorateString
            && inst.operands[1].unwrap_decoration() == Decoration::UserTypeGOOGLE
        {
            let id = inst.operands[0].unwrap_id_ref();
            let encoded = inst.operands[2].unwrap_literal_string();
            let json = encoded.strip_prefix(Self::ENCODING_PREFIX)?;

            Some((
                id,
                LazilyDeserialized {
                    json,
                    _marker: PhantomData,
                },
            ))
        } else {
            None
        }
    }

    fn decode_all(module: &Module) -> DecodeAllIter<'_, Self> {
        module
            .annotations
            .iter()
            .filter_map(Self::try_decode as fn(_) -> _)
    }

    fn remove_all(module: &mut Module) {
        module
            .annotations
            .retain(|inst| Self::try_decode(inst).is_none());
    }
}

// HACK(eddyb) return type of `CustomDecoration::decode_all`, in lieu of
// `-> impl Iterator<Item = (Word, LazilyDeserialized<'_, Self>)` in the trait.
type DecodeAllIter<'a, D> = iter::FilterMap<
    slice::Iter<'a, Instruction>,
    fn(&'a Instruction) -> Option<(Word, LazilyDeserialized<'a, D>)>,
>;

/// Helper allowing full deserialization to be avoided where possible.
#[derive(Copy, Clone)]
pub struct LazilyDeserialized<'a, D> {
    json: &'a str,
    _marker: PhantomData<D>,
}

impl<'a, D: Deserialize<'a>> LazilyDeserialized<'a, D> {
    pub fn deserialize(self) -> D {
        serde_json::from_str(self.json).unwrap()
    }
}

#[derive(Deserialize, Serialize)]
pub struct ZombieDecoration {
    pub reason: String,

    #[serde(flatten)]
    pub span: Option<SerializedSpan>,
}

impl CustomDecoration for ZombieDecoration {
    const ENCODING_PREFIX: &'static str = "Z";
}

/// Representation of a `rustc` `Span` that can be turned into a `Span` again
/// in another compilation, by reloading the file. However, note that this will
/// fail if the file changed since, which is detected using the serialized `hash`.
#[derive(Deserialize, Serialize)]
pub struct SerializedSpan {
    file: PathBuf,
    hash: serde_adapters::SourceFileHash,
    lo: u32,
    hi: u32,
}

// HACK(eddyb) `rustc_span` types implement only `rustc_serialize` traits, but
// not `serde` traits, and the easiest workaround is to have our own types.
mod serde_adapters {
    use serde::{Deserialize, Serialize};

    #[derive(Copy, Clone, PartialEq, Eq, Deserialize, Serialize)]
    pub enum SourceFileHashAlgorithm {
        Md5,
        Sha1,
        Sha256,
    }

    impl From<rustc_span::SourceFileHashAlgorithm> for SourceFileHashAlgorithm {
        fn from(kind: rustc_span::SourceFileHashAlgorithm) -> Self {
            match kind {
                rustc_span::SourceFileHashAlgorithm::Md5 => Self::Md5,
                rustc_span::SourceFileHashAlgorithm::Sha1 => Self::Sha1,
                rustc_span::SourceFileHashAlgorithm::Sha256 => Self::Sha256,
            }
        }
    }

    #[derive(Copy, Clone, PartialEq, Eq, Deserialize, Serialize)]
    pub struct SourceFileHash {
        kind: SourceFileHashAlgorithm,
        value: [u8; 32],
    }

    impl From<rustc_span::SourceFileHash> for SourceFileHash {
        fn from(hash: rustc_span::SourceFileHash) -> Self {
            let bytes = hash.hash_bytes();
            let mut hash = Self {
                kind: hash.kind.into(),
                value: Default::default(),
            };
            hash.value[..bytes.len()].copy_from_slice(bytes);
            hash
        }
    }
}

impl SerializedSpan {
    pub fn from_rustc(span: Span, source_map: &SourceMap) -> Option<Self> {
        // Decorations may not always have valid spans.
        // FIXME(eddyb) reduce the sources of this as much as possible.
        if span.is_dummy() {
            return None;
        }

        let (lo, hi) = (span.lo(), span.hi());
        if lo > hi {
            // FIXME(eddyb) broken `Span` - potentially turn this into an assert?
            return None;
        }

        let file = source_map.lookup_source_file(lo);
        if !(file.start_pos <= lo && hi <= file.end_pos) {
            // FIXME(eddyb) broken `Span` - potentially turn this into an assert?
            return None;
        }

        Some(Self {
            file: match &file.name {
                // We can only support real files, not "synthetic" ones (which
                // are almost never exposed to the compiler backend anyway).
                FileName::Real(real_name) => real_name.local_path()?.to_path_buf(),
                _ => return None,
            },
            hash: file.src_hash.into(),
            lo: (lo - file.start_pos).to_u32(),
            hi: (hi - file.start_pos).to_u32(),
        })
    }

    pub fn to_rustc(&self, source_map: &SourceMap) -> Option<Span> {
        let file = source_map.load_file(&self.file).ok()?;

        // If the file has changed since serializing, there's not much we can do,
        // other than avoid creating invalid/confusing `Span`s.
        // FIXME(eddyb) we could still indicate some of this to the user.
        if self.hash != file.src_hash.into() {
            return None;
        }

        // Sanity check - assuming `SerializedSpan` isn't corrupted, this assert
        // could only ever fail because of a hash collision.
        assert!(self.lo <= self.hi && self.hi <= (file.end_pos.0 - file.start_pos.0));

        Some(Span::with_root_ctxt(
            file.start_pos + Pos::from_u32(self.lo),
            file.start_pos + Pos::from_u32(self.hi),
        ))
    }
}
