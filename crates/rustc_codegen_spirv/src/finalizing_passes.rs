use rspirv::dr::{Instruction, Module, Operand};
use rspirv::spirv::{Decoration, Op, Word};
use rustc_span::{source_map::SourceMap, FileName, Pos, Span};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::PathBuf;

#[derive(Deserialize, Serialize)]
pub struct ZombieDecoration {
    pub reason: String,

    #[serde(flatten)]
    pub span: Option<ZombieSpan>,
}

/// Representation of a `rustc` `Span` that can be turned into a `Span` again
/// in another compilation, by reloading the file. However, note that this will
/// fail if the file changed since, which is detected using the serialized `hash`.
#[derive(Deserialize, Serialize)]
pub struct ZombieSpan {
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

impl ZombieSpan {
    fn from_rustc(span: Span, source_map: &SourceMap) -> Option<Self> {
        // Zombies may not always have valid spans.
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
                FileName::Real(real_name) => real_name.local_path().to_path_buf(),
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

        // Sanity check - assuming `ZombieSpan` isn't corrupted, this assert
        // could only ever fail because of a hash collision.
        assert!(self.lo <= self.hi && self.hi <= file.byte_length());

        Some(Span::with_root_ctxt(
            file.start_pos + Pos::from_u32(self.lo),
            file.start_pos + Pos::from_u32(self.hi),
        ))
    }
}

pub fn export_zombies(
    module: &mut Module,
    zombies: &HashMap<Word, (&'static str, Span)>,
    source_map: &SourceMap,
) {
    for (&id, &(reason, span)) in zombies {
        let encoded = serde_json::to_string(&ZombieDecoration {
            reason: reason.to_string(),
            span: ZombieSpan::from_rustc(span, source_map),
        })
        .unwrap();

        // TODO: Right now we just piggyback off UserTypeGOOGLE since we never use it elsewhere. We should, uh, fix this
        // to use non_semantic or something.
        // https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_non_semantic_info.html
        let inst = Instruction::new(
            Op::DecorateString,
            None,
            None,
            vec![
                Operand::IdRef(id),
                Operand::Decoration(Decoration::UserTypeGOOGLE),
                Operand::LiteralString(encoded),
            ],
        );
        module.annotations.push(inst);
    }
}
