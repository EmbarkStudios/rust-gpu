//! SPIR-V decorations specific to `rustc_codegen_spirv`, produced during
//! the original codegen of a crate, and consumed by the `linker`.

use crate::builder_spirv::BuilderSpirv;
use rspirv::dr::{Instruction, Module, Operand};
use rspirv::spirv::{Decoration, Op, Word};
use rustc_data_structures::fx::{FxHashMap, FxIndexMap};
use rustc_data_structures::sync::Lrc;
use rustc_span::{source_map::SourceMap, Pos, Span};
use rustc_span::{FileName, SourceFile};
use serde::{Deserialize, Serialize};
use smallvec::SmallVec;
use std::borrow::Cow;
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
                    json: json.into(),
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
pub struct LazilyDeserialized<'a, D> {
    json: Cow<'a, str>,
    _marker: PhantomData<D>,
}

impl<D> Clone for LazilyDeserialized<'_, D> {
    fn clone(&self) -> Self {
        let Self { ref json, _marker } = *self;
        Self {
            json: json.clone(),
            _marker,
        }
    }
}

impl<D: for<'a> Deserialize<'a>> LazilyDeserialized<'_, D> {
    pub fn deserialize(&self) -> D {
        serde_json::from_str(&self.json).unwrap()
    }

    pub fn into_owned(self) -> LazilyDeserialized<'static, D> {
        let Self { json, _marker } = self;
        LazilyDeserialized {
            json: json.into_owned().into(),
            _marker,
        }
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
/// in another compilation, by regenerating the `rustc` `SourceFile`.
#[derive(Deserialize, Serialize)]
pub struct SerializedSpan {
    file_name: String,
    // NOTE(eddyb) by keeping `lo` but not `hi`, we mimick `OpLine` limitations
    // (which could be lifted in the future using custom SPIR-T debuginfo).
    lo: u32,
}

impl SerializedSpan {
    pub fn from_rustc(span: Span, builder: &BuilderSpirv<'_>) -> Option<Self> {
        // Decorations may not always have valid spans.
        // FIXME(eddyb) reduce the sources of this as much as possible.
        if span.is_dummy() {
            return None;
        }

        let lo = span.lo();

        let file = builder.source_map.lookup_source_file(lo);
        if !(file.start_pos..=file.end_pos).contains(&lo) {
            // FIXME(eddyb) broken `Span` - potentially turn this into an assert?
            return None;
        }

        // NOTE(eddyb) this emits necessary `OpString`/`OpSource` instructions.
        builder.def_debug_file(file.clone());

        Some(Self {
            file_name: file.name.prefer_remapped().to_string(),
            lo: (lo - file.start_pos).to_u32(),
        })
    }
}

/// Helper type to delay most of the work necessary to turn a `SerializedSpan`
/// back into an usable `Span`, until it's actually needed (i.e. for an error).
pub struct SpanRegenerator<'a> {
    source_map: &'a SourceMap,
    module: &'a Module,

    // HACK(eddyb) this is mostly replicating SPIR-T's module-level debuginfo.
    spv_debug_files: Option<FxIndexMap<&'a str, SpvDebugFile<'a>>>,
}

// HACK(eddyb) this is mostly replicating SPIR-T's module-level debuginfo.
#[derive(Default)]
struct SpvDebugFile<'a> {
    /// Source strings from one `OpSource`, and any number of `OpSourceContinued`.
    op_source_parts: SmallVec<[&'a str; 1]>,

    regenerated_rustc_source_file: Option<Lrc<SourceFile>>,
}

impl<'a> SpanRegenerator<'a> {
    pub fn new(source_map: &'a SourceMap, module: &'a Module) -> Self {
        Self {
            source_map,
            module,
            spv_debug_files: None,
        }
    }

    fn regenerate_rustc_source_file(&mut self, file_name: &str) -> Option<&SourceFile> {
        let spv_debug_files = self.spv_debug_files.get_or_insert_with(|| {
            let mut op_string_by_id = FxHashMap::default();
            let mut spv_debug_files = FxIndexMap::default();
            let mut insts = self.module.debug_string_source.iter().peekable();
            while let Some(inst) = insts.next() {
                match inst.class.opcode {
                    Op::String => {
                        op_string_by_id.insert(
                            inst.result_id.unwrap(),
                            inst.operands[0].unwrap_literal_string(),
                        );
                    }
                    Op::Source if inst.operands.len() == 4 => {
                        let file_name_id = inst.operands[2].unwrap_id_ref();
                        if let Some(&file_name) = op_string_by_id.get(&file_name_id) {
                            let mut file = SpvDebugFile::default();
                            file.op_source_parts
                                .push(inst.operands[3].unwrap_literal_string());
                            while let Some(&next_inst) = insts.peek() {
                                if next_inst.class.opcode != Op::SourceContinued {
                                    break;
                                }
                                insts.next();

                                file.op_source_parts
                                    .push(next_inst.operands[0].unwrap_literal_string());
                            }

                            // FIXME(eddyb) what if the file is already present,
                            // should it be considered ambiguous overall?
                            spv_debug_files.insert(file_name, file);
                        }
                    }
                    _ => {}
                }
            }
            spv_debug_files
        });
        let spv_debug_file = spv_debug_files.get_mut(file_name)?;

        let file = &mut spv_debug_file.regenerated_rustc_source_file;
        if file.is_none() {
            // FIXME(eddyb) reduce allocations here by checking if the file is
            // already loaded, and not allocating just to compare the source,
            // but at least it's cheap when `OpSourceContinued` isn't used.
            let src = match &spv_debug_file.op_source_parts[..] {
                &[part] => Cow::Borrowed(part),
                parts => parts.concat().into(),
            };

            // HACK(eddyb) in case the file has changed, and because `SourceMap`
            // is strictly monotonic, we need to come up with some other name.
            let mut sm_file_name_candidates = [PathBuf::from(file_name).into()]
                .into_iter()
                .chain((0..).map(|i| FileName::Custom(format!("outdated({i}) {file_name}"))));

            *file = sm_file_name_candidates.find_map(|sm_file_name_candidate| {
                let sf = self
                    .source_map
                    .new_source_file(sm_file_name_candidate, src.clone().into_owned());

                // Only use this `FileName` candidate if we either:
                // 1. reused a `SourceFile` with the right `src`/`external_src`
                // 2. allocated a new `SourceFile` with our choice of `src`
                self.source_map
                    .ensure_source_file_source_present(sf.clone());
                let sf_src_matches = sf
                    .src
                    .as_ref()
                    .map(|sf_src| sf_src[..] == src[..])
                    .or_else(|| {
                        sf.external_src
                            .borrow()
                            .get_source()
                            .map(|sf_src| sf_src[..] == src[..])
                    })
                    .unwrap_or(false);

                if sf_src_matches {
                    Some(sf)
                } else {
                    None
                }
            });
        }
        file.as_deref()
    }

    pub fn serialized_span_to_rustc(&mut self, span: &SerializedSpan) -> Option<Span> {
        let file = self.regenerate_rustc_source_file(&span.file_name[..])?;

        // Sanity check - assuming `SerializedSpan` isn't corrupted, this assert
        // could only ever fail because of the file name being ambiguous.
        assert!(span.lo <= (file.end_pos.0 - file.start_pos.0));

        let lo = file.start_pos + Pos::from_u32(span.lo);
        Some(Span::with_root_ctxt(lo, lo))
    }
}
