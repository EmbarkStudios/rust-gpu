use crate::builder;
use crate::codegen_cx::CodegenCx;
use crate::spirv_type::SpirvType;
use crate::symbols::Symbols;
use crate::target::SpirvTarget;
use crate::target_feature::TargetFeature;
use rspirv::dr::{Block, Builder, Module, Operand};
use rspirv::spirv::{
    AddressingModel, Capability, MemoryModel, Op, SourceLanguage, StorageClass, Word,
};
use rspirv::{binary::Assemble, binary::Disassemble};
use rustc_arena::DroplessArena;
use rustc_data_structures::fx::{FxHashMap, FxHashSet};
use rustc_data_structures::sync::Lrc;
use rustc_middle::bug;
use rustc_middle::ty::TyCtxt;
use rustc_span::source_map::SourceMap;
use rustc_span::symbol::Symbol;
use rustc_span::{FileName, FileNameDisplayPreference, SourceFile, Span, DUMMY_SP};
use std::assert_matches::assert_matches;
use std::cell::{RefCell, RefMut};
use std::hash::{Hash, Hasher};
use std::iter;
use std::str;
use std::{fs::File, io::Write, path::Path};

#[derive(Copy, Clone, Debug, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub enum SpirvValueKind {
    Def(Word),

    /// The ID of a global instruction matching a `SpirvConst`, but which cannot
    /// pass validation. Used to error (or attach zombie spans), at the usesites
    /// of such constants, instead of where they're generated (and cached).
    IllegalConst(Word),

    /// This can only happen in one specific case - which is as a result of
    /// `codegen_buffer_store_intrinsic`, that function is supposed to return
    /// OpTypeVoid, however because it gets inline by the compiler it can't.
    /// Instead we return this, and trigger an error if we ever end up using the
    /// result of this function call (which we can't).
    IllegalTypeUsed(Word),

    // FIXME(eddyb) this shouldn't be needed, but `rustc_codegen_ssa` still relies
    // on converting `Function`s to `Value`s even for direct calls, the `Builder`
    // should just have direct and indirect `call` variants (or a `Callee` enum).
    FnAddr {
        function: Word,
    },

    /// Deferred pointer cast, for the `Logical` addressing model (which doesn't
    /// really support raw pointers in the way Rust expects to be able to use).
    ///
    /// The cast's target pointer type is the `ty` of the `SpirvValue` that has
    /// `LogicalPtrCast` as its `kind`, as it would be redundant to have it here.
    LogicalPtrCast {
        /// Pointer value being cast.
        original_ptr: Word,

        /// Pointee type of `original_ptr`.
        original_pointee_ty: Word,

        /// Result ID for the `OpBitcast` instruction representing the cast,
        /// to attach zombies to.
        //
        // HACK(eddyb) having an `OpBitcast` only works by being DCE'd away,
        // or by being replaced with a noop in `qptr::lower`.
        bitcast_result_id: Word,
    },
}

#[derive(Copy, Clone, Debug, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct SpirvValue {
    pub kind: SpirvValueKind,
    pub ty: Word,
}

impl SpirvValue {
    pub fn const_fold_load(self, cx: &CodegenCx<'_>) -> Option<Self> {
        match self.kind {
            SpirvValueKind::Def(id) | SpirvValueKind::IllegalConst(id) => {
                let &entry = cx.builder.id_to_const.borrow().get(&id)?;
                match entry.val {
                    SpirvConst::PtrTo { pointee } => {
                        let ty = match cx.lookup_type(self.ty) {
                            SpirvType::Pointer { pointee } => pointee,
                            ty => bug!("load called on value that wasn't a pointer: {:?}", ty),
                        };
                        // FIXME(eddyb) deduplicate this `if`-`else` and its other copies.
                        let kind = if entry.legal.is_ok() {
                            SpirvValueKind::Def(pointee)
                        } else {
                            SpirvValueKind::IllegalConst(pointee)
                        };
                        Some(SpirvValue { kind, ty })
                    }
                    _ => None,
                }
            }

            _ => None,
        }
    }

    // Important: we *cannot* use bx.emit() here, because this is called in
    // contexts where the emitter is already locked. Doing so may cause subtle
    // rare bugs.
    pub fn def(self, bx: &builder::Builder<'_, '_>) -> Word {
        self.def_with_span(bx, bx.span())
    }

    // def and def_cx are separated, because Builder has a span associated with
    // what it's currently emitting.
    pub fn def_cx(self, cx: &CodegenCx<'_>) -> Word {
        self.def_with_span(cx, DUMMY_SP)
    }

    pub fn def_with_span(self, cx: &CodegenCx<'_>, span: Span) -> Word {
        match self.kind {
            SpirvValueKind::Def(id) => id,

            SpirvValueKind::IllegalConst(id) => {
                let entry = &cx.builder.id_to_const.borrow()[&id];
                let msg = match entry.legal.unwrap_err() {
                    IllegalConst::Shallow(cause) => {
                        if let (
                            LeafIllegalConst::CompositeContainsPtrTo,
                            SpirvConst::Composite(_fields),
                        ) = (cause, &entry.val)
                        {
                            // FIXME(eddyb) materialize this at runtime, using
                            // `OpCompositeConstruct` (transitively, i.e. after
                            // putting every field through `SpirvValue::def`),
                            // if we have a `Builder` to do that in.
                            // FIXME(eddyb) this isn't possible right now, as
                            // the builder would be dynamically "locked" anyway
                            // (i.e. attempting to do `bx.emit()` would panic).
                        }

                        cause.message()
                    }

                    IllegalConst::Indirect(cause) => cause.message(),
                };

                cx.zombie_with_span(id, span, msg);

                id
            }

            SpirvValueKind::IllegalTypeUsed(id) => {
                cx.tcx
                    .sess
                    .struct_span_err(span, "Can't use type as a value")
                    .note(&format!("Type: *{}", cx.debug_type(id)))
                    .emit();

                id
            }

            SpirvValueKind::FnAddr { .. } => {
                cx.builder
                    .const_to_id
                    .borrow()
                    .get(&WithType {
                        ty: self.ty,
                        val: SpirvConst::ZombieUndefForFnAddr,
                    })
                    .expect("FnAddr didn't go through proper undef registration")
                    .val
            }

            SpirvValueKind::LogicalPtrCast {
                original_ptr: _,
                original_pointee_ty,
                bitcast_result_id,
            } => {
                cx.zombie_with_span(
                    bitcast_result_id,
                    span,
                    &format!(
                        "cannot cast between pointer types\
                         \nfrom `*{}`\
                         \n  to `{}`",
                        cx.debug_type(original_pointee_ty),
                        cx.debug_type(self.ty)
                    ),
                );

                bitcast_result_id
            }
        }
    }
}

pub trait SpirvValueExt {
    fn with_type(self, ty: Word) -> SpirvValue;
}

impl SpirvValueExt for Word {
    fn with_type(self, ty: Word) -> SpirvValue {
        SpirvValue {
            kind: SpirvValueKind::Def(self),
            ty,
        }
    }
}

#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub enum SpirvConst<'tcx> {
    U32(u32),
    U64(u64),
    /// f32 isn't hash, so store bits
    F32(u32),
    /// f64 isn't hash, so store bits
    F64(u64),
    Bool(bool),

    Null,
    Undef,

    /// Like `Undef`, but cached separately to avoid `FnAddr` zombies accidentally
    /// applying to non-zombie `Undef`s of the same types.
    // FIXME(eddyb) include the function ID so that multiple `fn` pointers to
    // different functions, but of the same type, don't overlap their zombies.
    ZombieUndefForFnAddr,

    Composite(&'tcx [Word]),

    /// Pointer to constant data, i.e. `&pointee`, represented as an `OpVariable`
    /// in the `Private` storage class, and with `pointee` as its initializer.
    PtrTo {
        pointee: Word,
    },
}

impl SpirvConst<'_> {
    /// Replace `&[T]` fields with `&'tcx [T]` ones produced by calling
    /// `tcx.arena.dropless.alloc_slice(...)` - this is done late for two reasons:
    /// 1. it avoids allocating in the arena when the cache would be hit anyway,
    ///    which would create "garbage" (as in, unreachable allocations)
    ///    (ideally these would also be interned, but that's even more refactors)
    /// 2. an empty slice is disallowed (as it's usually handled as a special
    ///    case elsewhere, e.g. `rustc`'s `ty::List` - sadly we can't use that)
    fn tcx_arena_alloc_slices<'tcx>(self, cx: &CodegenCx<'tcx>) -> SpirvConst<'tcx> {
        fn arena_alloc_slice<'tcx, T: Copy>(cx: &CodegenCx<'tcx>, xs: &[T]) -> &'tcx [T] {
            if xs.is_empty() {
                &[]
            } else {
                cx.tcx.arena.dropless.alloc_slice(xs)
            }
        }

        match self {
            // FIXME(eddyb) these are all noop cases, could they be automated?
            SpirvConst::U32(v) => SpirvConst::U32(v),
            SpirvConst::U64(v) => SpirvConst::U64(v),
            SpirvConst::F32(v) => SpirvConst::F32(v),
            SpirvConst::F64(v) => SpirvConst::F64(v),
            SpirvConst::Bool(v) => SpirvConst::Bool(v),
            SpirvConst::Null => SpirvConst::Null,
            SpirvConst::Undef => SpirvConst::Undef,
            SpirvConst::ZombieUndefForFnAddr => SpirvConst::ZombieUndefForFnAddr,
            SpirvConst::PtrTo { pointee } => SpirvConst::PtrTo { pointee },

            SpirvConst::Composite(fields) => SpirvConst::Composite(arena_alloc_slice(cx, fields)),
        }
    }
}

#[derive(Debug, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
struct WithType<V> {
    ty: Word,
    val: V,
}

/// Primary causes for a `SpirvConst` to be deemed illegal.
#[derive(Copy, Clone, Debug)]
enum LeafIllegalConst {
    /// `SpirvConst::Composite` containing a `SpirvConst::PtrTo` as a field.
    /// This is illegal because `OpConstantComposite` must have other constants
    /// as its operands, and `OpVariable`s are never considered constant.
    // FIXME(eddyb) figure out if this is an accidental omission in SPIR-V.
    CompositeContainsPtrTo,
}

impl LeafIllegalConst {
    fn message(&self) -> &'static str {
        match *self {
            Self::CompositeContainsPtrTo => {
                "constant arrays/structs cannot contain pointers to other constants"
            }
        }
    }
}

#[derive(Copy, Clone, Debug)]
enum IllegalConst {
    /// This `SpirvConst` is (or contains) a "leaf" illegal constant. As there
    /// is no indirection, some of these could still be materialized at runtime,
    /// using e.g. `OpCompositeConstruct` instead of `OpConstantComposite`.
    Shallow(LeafIllegalConst),

    /// This `SpirvConst` is (or contains/points to) a `PtrTo` which points to
    /// a "leaf" illegal constant. As the data would have to live for `'static`,
    /// there is no way to materialize it as a pointer in SPIR-V. However, it
    /// could still be legalized during codegen by e.g. folding loads from it.
    Indirect(LeafIllegalConst),
}

#[derive(Copy, Clone, Debug)]
struct WithConstLegality<V> {
    val: V,
    legal: Result<(), IllegalConst>,
}

/// `HashMap` key type (for `debug_file_cache` in `BuilderSpirv`), which is
/// equivalent to a whole `rustc` `SourceFile`, but has O(1) `Eq` and `Hash`
/// implementations (i.e. not involving the path or the contents of the file).
///
/// This is possible because we can compare `Lrc<SourceFile>`s by equality, as
/// `rustc`'s `SourceMap` already ensures that only one `SourceFile` will be
/// allocated for some given file. For hashing, we could hash the address, or
///
struct DebugFileKey(Lrc<SourceFile>);

impl PartialEq for DebugFileKey {
    fn eq(&self, other: &Self) -> bool {
        let (Self(self_sf), Self(other_sf)) = (self, other);
        Lrc::ptr_eq(self_sf, other_sf)
    }
}
impl Eq for DebugFileKey {}

impl Hash for DebugFileKey {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let Self(sf) = self;
        sf.name_hash.hash(state);
        sf.src_hash.hash(state);
    }
}

#[derive(Copy, Clone)]
pub struct DebugFileSpirv<'tcx> {
    pub file_name: &'tcx str,

    /// The SPIR-V ID for the result of the `OpString` instruction containing
    /// `file_name` - this is what e.g. `OpLine` uses to identify the file.
    ///
    /// All other details about the file are also attached to this ID, using
    /// other instructions that don't produce their own IDs (e.g. `OpSource`).
    pub file_name_op_string_id: Word,
}

/// Cursor system:
///
/// The LLVM module builder model (and therefore `codegen_ssa`) assumes that there is a central
/// module object, then, builder objects are created pointing at that central module object (e.g.
/// for adding instructions to a basic block).  Several of these builder objects can be live at the
/// same time, mutating the central module object all at once.  Unfortunately, rspirv doesn't work
/// like that. Instead, there is a single builder object, which owns a module and a "cursor". This
/// cursor indicates to the builder where to append instructions when an instruction is added -
/// e.g. if add() is called, then `OpAdd` is appended to the basic block pointed to by the cursor.
///
/// So! We emulate the LLVM system by treating the rspirv Builder as the "central module object",
/// then, when a "builder object" is created, we store a reference to a `RefCell<rspirv builder>`,
/// *as well as* a copy of the cursor for that particular builder. Whenever the `RefCell` is
/// borrowed, then we stomp over the rspirv cursor with our copy, causing the duration of that
/// `RefCell` borrow to use that cursor.
///
/// So, if you're writing code inside `crate::builder::Builder`, then `self.emit()` will use
/// `self.cursor` (the current basic block) as that "stomp-over" cursor and return a mutable
/// reference to the rspirv builder. If you're writing code elsewhere (`codegen_cx::CodegenCx`),
/// then `self.emit_global()` will use the generic "global cursor" and return a mutable reference
/// to the rspirv builder with no basic block nor function selected, i.e. any instructions emitted
/// will be in the global section.
#[derive(Debug, Default, Copy, Clone)]
#[must_use = "BuilderCursor should usually be assigned to the Builder.cursor field"]
pub struct BuilderCursor {
    pub function: Option<usize>,
    pub block: Option<usize>,
}

pub struct BuilderSpirv<'tcx> {
    source_map: &'tcx SourceMap,
    dropless_arena: &'tcx DroplessArena,

    builder: RefCell<Builder>,

    // Bidirectional maps between `SpirvConst` and the ID of the defined global
    // (e.g. `OpConstant...`) instruction.
    // NOTE(eddyb) both maps have `WithConstLegality` around their keys, which
    // allows getting that legality information without additional lookups.
    const_to_id: RefCell<FxHashMap<WithType<SpirvConst<'tcx>>, WithConstLegality<Word>>>,
    id_to_const: RefCell<FxHashMap<Word, WithConstLegality<SpirvConst<'tcx>>>>,

    debug_file_cache: RefCell<FxHashMap<DebugFileKey, DebugFileSpirv<'tcx>>>,

    enabled_capabilities: FxHashSet<Capability>,
    enabled_extensions: FxHashSet<Symbol>,
}

impl<'tcx> BuilderSpirv<'tcx> {
    pub fn new(
        tcx: TyCtxt<'tcx>,
        sym: &Symbols,
        target: &SpirvTarget,
        features: &[TargetFeature],
    ) -> Self {
        let version = target.spirv_version();
        let memory_model = target.memory_model();

        let mut builder = Builder::new();
        builder.set_version(version.0, version.1);
        builder.module_mut().header.as_mut().unwrap().generator = 0x001B_0000;

        let mut enabled_capabilities = FxHashSet::default();
        let mut enabled_extensions = FxHashSet::default();

        fn add_cap(
            builder: &mut Builder,
            enabled_capabilities: &mut FxHashSet<Capability>,
            cap: Capability,
        ) {
            // This should be the only callsite of Builder::capability (aside from tests), to make
            // sure the hashset stays in sync.
            builder.capability(cap);
            enabled_capabilities.insert(cap);
        }
        fn add_ext(builder: &mut Builder, enabled_extensions: &mut FxHashSet<Symbol>, ext: Symbol) {
            // This should be the only callsite of Builder::extension (aside from tests), to make
            // sure the hashset stays in sync.
            builder.extension(ext.as_str());
            enabled_extensions.insert(ext);
        }

        for feature in features {
            match *feature {
                TargetFeature::Capability(cap) => {
                    add_cap(&mut builder, &mut enabled_capabilities, cap);
                }
                TargetFeature::Extension(ext) => {
                    add_ext(&mut builder, &mut enabled_extensions, ext);
                }
            }
        }

        add_cap(&mut builder, &mut enabled_capabilities, Capability::Shader);
        if memory_model == MemoryModel::Vulkan {
            if version < (1, 5) {
                add_ext(
                    &mut builder,
                    &mut enabled_extensions,
                    sym.spv_khr_vulkan_memory_model,
                );
            }
            add_cap(
                &mut builder,
                &mut enabled_capabilities,
                Capability::VulkanMemoryModel,
            );
        }

        // The linker will always be ran on this module
        add_cap(&mut builder, &mut enabled_capabilities, Capability::Linkage);

        builder.memory_model(AddressingModel::Logical, memory_model);

        Self {
            source_map: tcx.sess.source_map(),
            dropless_arena: &tcx.arena.dropless,
            builder: RefCell::new(builder),
            const_to_id: Default::default(),
            id_to_const: Default::default(),
            debug_file_cache: Default::default(),
            enabled_capabilities,
            enabled_extensions,
        }
    }

    pub fn finalize(self) -> Module {
        self.builder.into_inner().module()
    }

    pub fn dump_module_str(&self) -> String {
        self.builder.borrow().module_ref().disassemble()
    }

    /// Helper function useful to place right before a crash, to debug the module state.
    pub fn dump_module(&self, path: impl AsRef<Path>) {
        let module = self.builder.borrow().module_ref().assemble();
        File::create(path)
            .unwrap()
            .write_all(spirv_tools::binary::from_binary(&module))
            .unwrap();
    }

    /// See comment on `BuilderCursor`
    pub fn builder(&self, cursor: BuilderCursor) -> RefMut<'_, Builder> {
        let mut builder = self.builder.borrow_mut();
        // select_function does bounds checks and other relatively expensive things, so don't just call it
        // unconditionally.
        if builder.selected_function() != cursor.function {
            builder.select_function(cursor.function).unwrap();
        }
        if cursor.function.is_some() && builder.selected_block() != cursor.block {
            builder.select_block(cursor.block).unwrap();
        }
        builder
    }

    pub fn has_capability(&self, capability: Capability) -> bool {
        self.enabled_capabilities.contains(&capability)
    }

    pub fn has_extension(&self, extension: Symbol) -> bool {
        self.enabled_extensions.contains(&extension)
    }

    pub fn select_function_by_id(&self, id: Word) -> BuilderCursor {
        let mut builder = self.builder.borrow_mut();
        for (index, func) in builder.module_ref().functions.iter().enumerate() {
            if func.def.as_ref().and_then(|i| i.result_id) == Some(id) {
                builder.select_function(Some(index)).unwrap();
                return BuilderCursor {
                    function: Some(index),
                    block: None,
                };
            }
        }

        bug!("Function not found: {}", id);
    }

    pub(crate) fn def_constant_cx(
        &self,
        ty: Word,
        val: SpirvConst<'_>,
        cx: &CodegenCx<'tcx>,
    ) -> SpirvValue {
        let val_with_type = WithType { ty, val };
        let mut builder = self.builder(BuilderCursor::default());
        if let Some(entry) = self.const_to_id.borrow().get(&val_with_type) {
            // FIXME(eddyb) deduplicate this `if`-`else` and its other copies.
            let kind = if entry.legal.is_ok() {
                SpirvValueKind::Def(entry.val)
            } else {
                SpirvValueKind::IllegalConst(entry.val)
            };
            return SpirvValue { kind, ty };
        }
        let val = val_with_type.val;
        let id = match val {
            SpirvConst::U32(v) => builder.constant_u32(ty, v),
            SpirvConst::U64(v) => builder.constant_u64(ty, v),
            SpirvConst::F32(v) => builder.constant_f32(ty, f32::from_bits(v)),
            SpirvConst::F64(v) => builder.constant_f64(ty, f64::from_bits(v)),
            SpirvConst::Bool(v) => {
                if v {
                    builder.constant_true(ty)
                } else {
                    builder.constant_false(ty)
                }
            }

            SpirvConst::Null => builder.constant_null(ty),
            SpirvConst::Undef | SpirvConst::ZombieUndefForFnAddr => builder.undef(ty, None),

            SpirvConst::Composite(v) => builder.constant_composite(ty, v.iter().copied()),

            SpirvConst::PtrTo { pointee } => {
                builder.variable(ty, None, StorageClass::Private, Some(pointee))
            }
        };
        #[allow(clippy::match_same_arms)]
        let legal = match val {
            SpirvConst::U32(_)
            | SpirvConst::U64(_)
            | SpirvConst::F32(_)
            | SpirvConst::F64(_)
            | SpirvConst::Bool(_) => Ok(()),

            SpirvConst::Null => {
                // FIXME(eddyb) check that the type supports `OpConstantNull`.
                Ok(())
            }
            SpirvConst::Undef => {
                // FIXME(eddyb) check that the type supports `OpUndef`.
                Ok(())
            }

            SpirvConst::ZombieUndefForFnAddr => {
                // This can be considered legal as it's already marked as zombie.
                // FIXME(eddyb) is it possible for the original zombie to lack a
                // span, and should we go through `IllegalConst` in order to be
                // able to attach a proper usesite span?
                Ok(())
            }

            SpirvConst::Composite(v) => v.iter().fold(Ok(()), |composite_legal, field| {
                let field_entry = &self.id_to_const.borrow()[field];
                let field_legal_in_composite = field_entry.legal.and(
                    // `field` is itself some legal `SpirvConst`, but can we have
                    // it as part of an `OpConstantComposite`?
                    match field_entry.val {
                        SpirvConst::PtrTo { .. } => Err(IllegalConst::Shallow(
                            LeafIllegalConst::CompositeContainsPtrTo,
                        )),
                        _ => Ok(()),
                    },
                );

                match (composite_legal, field_legal_in_composite) {
                    (Ok(()), Ok(())) => Ok(()),
                    (Err(illegal), Ok(())) | (Ok(()), Err(illegal)) => Err(illegal),

                    // Combining two causes of an illegal `SpirvConst` has to
                    // take into account which is "worse", i.e. which imposes
                    // more restrictions on how the resulting value can be used.
                    // `Indirect` is worse than `Shallow` because it cannot be
                    // materialized at runtime in the same way `Shallow` can be.
                    (Err(illegal @ IllegalConst::Indirect(_)), Err(_))
                    | (Err(_), Err(illegal @ IllegalConst::Indirect(_)))
                    | (Err(illegal @ IllegalConst::Shallow(_)), Err(IllegalConst::Shallow(_))) => {
                        Err(illegal)
                    }
                }
            }),

            SpirvConst::PtrTo { pointee } => match self.id_to_const.borrow()[&pointee].legal {
                Ok(()) => Ok(()),

                // `Shallow` becomes `Indirect` when placed behind a pointer.
                Err(IllegalConst::Shallow(cause) | IllegalConst::Indirect(cause)) => {
                    Err(IllegalConst::Indirect(cause))
                }
            },
        };
        let val = val.tcx_arena_alloc_slices(cx);
        assert_matches!(
            self.const_to_id
                .borrow_mut()
                .insert(WithType { ty, val }, WithConstLegality { val: id, legal }),
            None
        );
        assert_matches!(
            self.id_to_const
                .borrow_mut()
                .insert(id, WithConstLegality { val, legal }),
            None
        );
        // FIXME(eddyb) deduplicate this `if`-`else` and its other copies.
        let kind = if legal.is_ok() {
            SpirvValueKind::Def(id)
        } else {
            SpirvValueKind::IllegalConst(id)
        };
        SpirvValue { kind, ty }
    }

    pub fn lookup_const_by_id(&self, id: Word) -> Option<SpirvConst<'tcx>> {
        Some(self.id_to_const.borrow().get(&id)?.val)
    }

    pub fn lookup_const(&self, def: SpirvValue) -> Option<SpirvConst<'tcx>> {
        match def.kind {
            SpirvValueKind::Def(id) | SpirvValueKind::IllegalConst(id) => {
                self.lookup_const_by_id(id)
            }
            _ => None,
        }
    }

    pub fn lookup_const_u64(&self, def: SpirvValue) -> Option<u64> {
        match self.lookup_const(def)? {
            SpirvConst::U32(v) => Some(v as u64),
            SpirvConst::U64(v) => Some(v),
            _ => None,
        }
    }

    pub fn file_line_col_for_op_line(&self, span: Span) -> (DebugFileSpirv<'tcx>, u32, u32) {
        let loc = self.source_map.lookup_char_pos(span.lo());
        (
            self.def_debug_file(loc.file),
            loc.line as u32,
            loc.col_display as u32,
        )
    }

    fn def_debug_file(&self, sf: Lrc<SourceFile>) -> DebugFileSpirv<'tcx> {
        *self
            .debug_file_cache
            .borrow_mut()
            .entry(DebugFileKey(sf))
            .or_insert_with_key(|DebugFileKey(sf)| {
                let mut builder = self.builder(Default::default());

                // FIXME(eddyb) it would be nicer if we could just rely on
                // `RealFileName::to_string_lossy` returning `Cow<'_, str>`,
                // but sadly that `'_` is the lifetime of the temporary `Lrc`,
                // not `'tcx`, so we have to arena-allocate to get `&'tcx str`.
                let file_name = match &sf.name {
                    FileName::Real(name) => {
                        name.to_string_lossy(FileNameDisplayPreference::Remapped)
                    }
                    _ => sf.name.prefer_remapped().to_string().into(),
                };
                let file_name = {
                    // FIXME(eddyb) it should be possible to arena-allocate a
                    // `&str` directly, but it would require upstream changes,
                    // and strings are handled by string interning in `rustc`.
                    fn arena_alloc_slice<'tcx, T: Copy>(
                        dropless_arena: &'tcx DroplessArena,
                        xs: &[T],
                    ) -> &'tcx [T] {
                        if xs.is_empty() {
                            &[]
                        } else {
                            dropless_arena.alloc_slice(xs)
                        }
                    }
                    str::from_utf8(arena_alloc_slice(self.dropless_arena, file_name.as_bytes()))
                        .unwrap()
                };
                let file_name_op_string_id = builder.string(file_name.to_owned());

                let file_contents = self
                    .source_map
                    .span_to_snippet(Span::with_root_ctxt(sf.start_pos, sf.end_pos))
                    .ok();

                // HACK(eddyb) this logic is duplicated from `spirt::spv::lift`.
                let op_source_and_continued_chunks = file_contents.as_ref().map(|contents| {
                    // The maximum word count is `2**16 - 1`, the first word is
                    // taken up by the opcode & word count, and one extra byte is
                    // taken up by the nil byte at the end of the LiteralString.
                    const MAX_OP_SOURCE_CONT_CONTENTS_LEN: usize = (0xffff - 1) * 4 - 1;

                    // `OpSource` has 3 more operands than `OpSourceContinued`,
                    // and each of them take up exactly one word.
                    const MAX_OP_SOURCE_CONTENTS_LEN: usize =
                        MAX_OP_SOURCE_CONT_CONTENTS_LEN - 3 * 4;

                    let (op_source_str, mut all_op_source_continued_str) =
                        contents.split_at(contents.len().min(MAX_OP_SOURCE_CONTENTS_LEN));

                    // FIXME(eddyb) `spirt::spv::lift` should use this.
                    let all_op_source_continued_str_chunks = iter::from_fn(move || {
                        let contents_rest = &mut all_op_source_continued_str;
                        if contents_rest.is_empty() {
                            return None;
                        }

                        // FIXME(eddyb) test with UTF-8! this `split_at` should
                        // actually take *less* than the full possible size, to
                        // avoid cutting a UTF-8 sequence.
                        let (cont_chunk, rest) = contents_rest
                            .split_at(contents_rest.len().min(MAX_OP_SOURCE_CONT_CONTENTS_LEN));
                        *contents_rest = rest;
                        Some(cont_chunk)
                    });
                    (op_source_str, all_op_source_continued_str_chunks)
                });

                if let Some((op_source_str, all_op_source_continued_str_chunks)) =
                    op_source_and_continued_chunks
                {
                    builder.source(
                        SourceLanguage::Unknown,
                        0,
                        Some(file_name_op_string_id),
                        Some(op_source_str),
                    );
                    for cont_chunk in all_op_source_continued_str_chunks {
                        builder.source_continued(cont_chunk);
                    }
                }

                DebugFileSpirv {
                    file_name,
                    file_name_op_string_id,
                }
            })
    }

    pub fn set_global_initializer(&self, global: Word, initializer: Word) {
        let mut builder = self.builder.borrow_mut();
        let module = builder.module_mut();
        let index = module
            .types_global_values
            .iter()
            .enumerate()
            .find_map(|(index, inst)| {
                if inst.result_id == Some(global) {
                    Some(index)
                } else {
                    None
                }
            })
            .expect("set_global_initializer global not found");
        // Remove and push it to the end, to keep spir-v definition order.
        let mut inst = module.types_global_values.remove(index);
        assert_eq!(inst.class.opcode, Op::Variable);
        assert_eq!(
            inst.operands.len(),
            1,
            "global already has initializer defined: {global}"
        );
        inst.operands.push(Operand::IdRef(initializer));
        module.types_global_values.push(inst);
    }

    pub fn select_block_by_id(&self, id: Word) -> BuilderCursor {
        fn block_matches(block: &Block, id: Word) -> bool {
            block.label.as_ref().and_then(|b| b.result_id) == Some(id)
        }

        let mut builder = self.builder.borrow_mut();
        let module = builder.module_ref();

        // The user is probably selecting a block in the current function, so search that first.
        if let Some(selected_function) = builder.selected_function() {
            // make no-ops really fast
            if let Some(selected_block) = builder.selected_block() {
                let block = &module.functions[selected_function].blocks[selected_block];
                if block_matches(block, id) {
                    return BuilderCursor {
                        function: Some(selected_function),
                        block: Some(selected_block),
                    };
                }
            }

            for (index, block) in module.functions[selected_function]
                .blocks
                .iter()
                .enumerate()
            {
                if block_matches(block, id) {
                    builder.select_block(Some(index)).unwrap();
                    return BuilderCursor {
                        function: Some(selected_function),
                        block: Some(index),
                    };
                }
            }
        }

        // Search the whole module.
        for (function_index, function) in module.functions.iter().enumerate() {
            for (block_index, block) in function.blocks.iter().enumerate() {
                if block_matches(block, id) {
                    builder.select_function(Some(function_index)).unwrap();
                    builder.select_block(Some(block_index)).unwrap();
                    return BuilderCursor {
                        function: Some(function_index),
                        block: Some(block_index),
                    };
                }
            }
        }

        bug!("Block not found: {}", id);
    }
}
