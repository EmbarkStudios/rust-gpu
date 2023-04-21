//! SPIR-V decorations specific to `rustc_codegen_spirv`, produced during
//! the original codegen of a crate, and consumed by the `linker`.

use crate::builder_spirv::BuilderSpirv;
use either::Either;
use itertools::Itertools;
use rspirv::dr::{Instruction, Module, Operand};
use rspirv::spirv::{Decoration, Op, Word};
use rustc_data_structures::fx::FxIndexMap;
use rustc_data_structures::sync::Lrc;
use rustc_span::{source_map::SourceMap, Span};
use rustc_span::{FileName, SourceFile};
use smallvec::SmallVec;
use std::borrow::Cow;
use std::marker::PhantomData;
use std::ops::Range;
use std::path::PathBuf;
use std::{fmt, iter, slice, str};

/// Decorations not native to SPIR-V require some form of encoding into existing
/// SPIR-V constructs, for which we use `OpDecorateString` with decoration type
/// `UserTypeGOOGLE` and some encoded Rust value as the decoration string.
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
pub trait CustomDecoration<'a>: Sized {
    const ENCODING_PREFIX: &'static str;

    fn encode(self, w: &mut impl fmt::Write) -> fmt::Result;
    fn decode(s: &'a str) -> Self;

    fn encode_to_inst(self, id: Word) -> Instruction {
        let mut encoded = Self::ENCODING_PREFIX.to_string();
        self.encode(&mut encoded).unwrap();

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

    fn try_decode_from_inst(inst: &Instruction) -> Option<(Word, LazilyDecoded<'_, Self>)> {
        if inst.class.opcode == Op::DecorateString
            && inst.operands[1].unwrap_decoration() == Decoration::UserTypeGOOGLE
        {
            let id = inst.operands[0].unwrap_id_ref();
            let prefixed_encoded = inst.operands[2].unwrap_literal_string();
            let encoded = prefixed_encoded.strip_prefix(Self::ENCODING_PREFIX)?;

            Some((
                id,
                LazilyDecoded {
                    encoded,
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
            .filter_map(Self::try_decode_from_inst as fn(_) -> _)
    }

    fn remove_all(module: &mut Module) {
        module
            .annotations
            .retain(|inst| Self::try_decode_from_inst(inst).is_none());
    }
}

// HACK(eddyb) return type of `CustomDecoration::decode_all`, in lieu of
// `-> impl Iterator<Item = (Word, LazilyDecoded<'_, Self>)` in the trait.
type DecodeAllIter<'a, D> = iter::FilterMap<
    slice::Iter<'a, Instruction>,
    fn(&'a Instruction) -> Option<(Word, LazilyDecoded<'a, D>)>,
>;

/// Helper allowing full decoding to be avoided where possible.
//
// FIXME(eddyb) is this even needed? (decoding impls are now much cheaper)
pub struct LazilyDecoded<'a, D> {
    encoded: &'a str,
    _marker: PhantomData<D>,
}

impl<'a, D: CustomDecoration<'a>> LazilyDecoded<'a, D> {
    pub fn decode(&self) -> D {
        D::decode(self.encoded)
    }
}

pub struct ZombieDecoration<'a> {
    pub reason: Cow<'a, str>,
}

impl<'a> CustomDecoration<'a> for ZombieDecoration<'a> {
    const ENCODING_PREFIX: &'static str = "Z";

    fn encode(self, w: &mut impl fmt::Write) -> fmt::Result {
        let Self { reason } = self;
        w.write_str(&reason)
    }
    fn decode(s: &'a str) -> Self {
        Self { reason: s.into() }
    }
}

/// Equivalent of `OpLine`, for the places where `rspirv` currently doesn't let
/// us actually emit a real `OpLine`, the way generating SPIR-T directly might.
//
// NOTE(eddyb) by keeping `line`+`col`, we mimick `OpLine` limitations
// (which could be lifted in the future using custom SPIR-T debuginfo).
// NOTE(eddyb) `NonSemantic.Shader.DebugInfo`'s `DebugLine` has both start & end,
// might be good to invest in SPIR-T being able to use NonSemantic debuginfo.
#[derive(Copy, Clone)]
pub struct SrcLocDecoration<'a> {
    pub file_name: &'a str,
    pub line: u32,
    pub col: u32,
}

impl<'a> CustomDecoration<'a> for SrcLocDecoration<'a> {
    const ENCODING_PREFIX: &'static str = "L";

    fn encode(self, w: &mut impl fmt::Write) -> fmt::Result {
        let Self {
            file_name,
            line,
            col,
        } = self;
        write!(w, "{file_name}:{line}:{col}")
    }
    fn decode(s: &'a str) -> Self {
        #[derive(Copy, Clone, Debug)]
        struct InvalidSrcLoc<'a>(&'a str);
        let err = InvalidSrcLoc(s);

        let (s, col) = s.rsplit_once(':').ok_or(err).unwrap();
        let (s, line) = s.rsplit_once(':').ok_or(err).unwrap();
        let file_name = s;

        Self {
            file_name,
            line: line.parse().unwrap(),
            col: col.parse().unwrap(),
        }
    }
}

impl<'tcx> SrcLocDecoration<'tcx> {
    pub fn from_rustc_span(span: Span, builder: &BuilderSpirv<'tcx>) -> Option<Self> {
        // We may not always have valid spans.
        // FIXME(eddyb) reduce the sources of this as much as possible.
        if span.is_dummy() {
            return None;
        }

        let (file, line, col) = builder.file_line_col_for_op_line(span);

        Some(Self {
            file_name: file.file_name,
            line,
            col,
        })
    }
}

/// Helper type to delay most of the work necessary to turn a `SrcLocDecoration`
/// back into an usable `Span`, until it's actually needed (i.e. for an error).
pub struct SpanRegenerator<'a> {
    source_map: &'a SourceMap,
    module: Either<&'a Module, &'a spirt::Module>,

    src_loc_decorations: Option<FxIndexMap<Word, LazilyDecoded<'a, SrcLocDecoration<'a>>>>,

    // HACK(eddyb) this has no really good reason to belong here, but it's easier
    // to handle it together with `SrcLocDecoration`, than separately.
    zombie_decorations: Option<FxIndexMap<Word, LazilyDecoded<'a, ZombieDecoration<'a>>>>,

    // HACK(eddyb) this is mostly replicating SPIR-T's module-level debuginfo.
    spv_debug_info: Option<SpvDebugInfo<'a>>,
}

#[derive(Default)]
struct SpvDebugInfo<'a> {
    id_to_op_string: FxIndexMap<Word, &'a str>,
    files: FxIndexMap<&'a str, SpvDebugFile<'a>>,
}

impl<'a> SpvDebugInfo<'a> {
    fn collect(module: Either<&'a Module, &'a spirt::Module>) -> Self {
        let mut this = Self::default();

        let module = match module {
            Either::Left(module) => module,

            // HACK(eddyb) the SPIR-T codepath is simpler, and kind of silly,
            // but we need the `SpvDebugFile`'s `regenerated_rustc_source_file`
            // caching, so for now it reuses `SpvDebugInfo` overall.
            Either::Right(module) => {
                let cx = module.cx_ref();
                match &module.debug_info {
                    spirt::ModuleDebugInfo::Spv(debug_info) => {
                        for sources in debug_info.source_languages.values() {
                            for (&file_name, src) in &sources.file_contents {
                                // FIXME(eddyb) what if the file is already present,
                                // should it be considered ambiguous overall?
                                this.files
                                    .entry(&cx[file_name])
                                    .or_default()
                                    .op_source_parts = [&src[..]].into_iter().collect();
                            }
                        }
                    }
                }
                return this;
            }
        };

        let mut insts = module.debug_string_source.iter().peekable();
        while let Some(inst) = insts.next() {
            match inst.class.opcode {
                Op::String => {
                    this.id_to_op_string.insert(
                        inst.result_id.unwrap(),
                        inst.operands[0].unwrap_literal_string(),
                    );
                }
                Op::Source if inst.operands.len() == 4 => {
                    let file_name_id = inst.operands[2].unwrap_id_ref();
                    if let Some(&file_name) = this.id_to_op_string.get(&file_name_id) {
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
                        this.files.insert(file_name, file);
                    }
                }
                _ => {}
            }
        }
        this
    }
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
            module: Either::Left(module),

            src_loc_decorations: None,
            zombie_decorations: None,

            spv_debug_info: None,
        }
    }

    pub fn new_spirt(source_map: &'a SourceMap, module: &'a spirt::Module) -> Self {
        Self {
            source_map,
            module: Either::Right(module),

            src_loc_decorations: None,
            zombie_decorations: None,

            spv_debug_info: None,
        }
    }

    pub fn src_loc_for_id(&mut self, id: Word) -> Option<SrcLocDecoration<'a>> {
        self.src_loc_decorations
            .get_or_insert_with(|| {
                SrcLocDecoration::decode_all(self.module.left().unwrap()).collect()
            })
            .get(&id)
            .map(|src_loc| src_loc.decode())
    }

    // HACK(eddyb) this has no really good reason to belong here, but it's easier
    // to handle it together with `SrcLocDecoration`, than separately.
    pub(crate) fn zombie_for_id(&mut self, id: Word) -> Option<ZombieDecoration<'a>> {
        self.zombie_decorations
            .get_or_insert_with(|| {
                ZombieDecoration::decode_all(self.module.left().unwrap()).collect()
            })
            .get(&id)
            .map(|zombie| zombie.decode())
    }

    pub fn src_loc_from_op_line(
        &mut self,
        file_id: Word,
        line: u32,
        col: u32,
    ) -> Option<SrcLocDecoration<'a>> {
        self.spv_debug_info
            .get_or_insert_with(|| SpvDebugInfo::collect(self.module))
            .id_to_op_string
            .get(&file_id)
            .map(|&file_name| SrcLocDecoration {
                file_name,
                line,
                col,
            })
    }

    fn regenerate_rustc_source_file(&mut self, file_name: &str) -> Option<&SourceFile> {
        let spv_debug_file = self
            .spv_debug_info
            .get_or_insert_with(|| SpvDebugInfo::collect(self.module))
            .files
            .get_mut(file_name)?;

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

    pub fn src_loc_to_rustc(&mut self, src_loc: SrcLocDecoration<'_>) -> Option<Span> {
        let SrcLocDecoration {
            file_name,
            line,
            col,
        } = src_loc;

        let file = self.regenerate_rustc_source_file(file_name)?;

        let line_bpos_range = file.line_bounds(line.checked_sub(1)? as usize);

        // Find the special cases (`MultiByteChar`s/`NonNarrowChar`s) in the line.
        let multibyte_chars = {
            let find = |bpos| {
                file.multibyte_chars
                    .binary_search_by_key(&bpos, |mbc| mbc.pos)
                    .unwrap_or_else(|x| x)
            };
            let Range { start, end } = line_bpos_range;
            file.multibyte_chars[find(start)..find(end)].iter()
        };
        let non_narrow_chars = {
            let find = |bpos| {
                file.non_narrow_chars
                    .binary_search_by_key(&bpos, |nnc| nnc.pos())
                    .unwrap_or_else(|x| x)
            };
            let Range { start, end } = line_bpos_range;
            file.non_narrow_chars[find(start)..find(end)].iter()
        };
        let mut special_chars = multibyte_chars
            .merge_join_by(non_narrow_chars, |mbc, nnc| mbc.pos.cmp(&nnc.pos()))
            .peekable();

        // Increment the `BytePos` until we reach the right `col_display`, using
        // `MultiByteChar`s/`NonNarrowChar`s to track non-trivial contributions
        // (this may look inefficient, but lines tend to be short, and `rustc`
        // itself is even worse than this, when it comes to `BytePos` lookups).
        let (mut cur_bpos, mut cur_col_display) = (line_bpos_range.start, 0);
        while cur_bpos < line_bpos_range.end && cur_col_display < col {
            let next_special_bpos = special_chars.peek().map(|special| {
                special
                    .as_ref()
                    .map_any(|mbc| mbc.pos, |nnc| nnc.pos())
                    .reduce(|x, _| x)
            });

            // Batch trivial chars (i.e. chars 1:1 wrt `BytePos` vs `col_display`).
            let following_trivial_chars =
                next_special_bpos.unwrap_or(line_bpos_range.end).0 - cur_bpos.0;
            if following_trivial_chars > 0 {
                let wanted_trivial_chars = following_trivial_chars.min(col - cur_col_display);
                cur_bpos.0 += wanted_trivial_chars;
                cur_col_display += wanted_trivial_chars;
                continue;
            }

            // Add a special char's `BytePos` and `col_display` contributions.
            let mbc_nnc = special_chars.next().unwrap();
            cur_bpos.0 += mbc_nnc.as_ref().left().map_or(1, |mbc| mbc.bytes as u32);
            cur_col_display += mbc_nnc.as_ref().right().map_or(1, |nnc| nnc.width() as u32);
        }

        Some(Span::with_root_ctxt(cur_bpos, cur_bpos))
    }
}
