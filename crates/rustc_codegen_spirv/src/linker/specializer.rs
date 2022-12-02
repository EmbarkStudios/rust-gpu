//! Specialize globals (types, constants and module-scoped variables) and functions,
//! to legalize a SPIR-V module representing a "family" of types with a single type,
//! by treating some globals and functions as "generic", inferring minimal sets
//! of "generic parameters", and "monomorphizing" them (i.e. expanding them into
//! one specialized copy per distinctly parameterized instance required).
//!
//! For now, this is only used for pointer type storage classes, because
//! Rust's pointer/reference types don't have an "address space" distinction,
//! and we also wouldn't want users to annotate every single type anyway.
//!
//! # Future plans
//!
//! Recursive data types (using `OpTypeForwardPointer`) are not supported, but
//! here is an outline of how that could work:
//! * groups of mutually-recursive `OpTypeForwardPointer`s are computed via SCCs
//! * each mutual-recursive group gets a single "generic" parameter count, that all
//!   pointer types in the group will use, and which is the sum of the "generic"
//!   parameters of all the leaves referenced by the pointer types in the group,
//!   ignoring the pointer types in the group themselves
//! * once the pointer types have been assigned their "g"eneric parameter count,
//!   the non-pointer types in each SCC - i.e. (indirectly) referenced by one of
//!   the pointer types in the group, and which in turn (indirectly) references
//!   a pointer type in the group - can have their "generic" parameters computed
//!   as normal, taking care to record where in the combined lists of "generic"
//!   parameters, any of the pointer types in the group show up
//! * each pointer type in the group will "fan out" a copy of its full set of
//!   "generic" parameters to every (indirect) mention of any pointer type in
//!   the group, using an additional parameter remapping, for which `Generic`:
//!   * requires this extra documentation:
//!     ```
//!     /// The one exception are `OpTypePointer`s involved in recursive data types
//!     /// (i.e. they were declared by `OpTypeForwardPointer`s, and their pointees are
//!     /// `OpTypeStruct`s that have the same pointer type as a leaf).
//!     /// As the pointee `OpTypeStruct` has more parameters than the pointer (each leaf
//!     /// use of the same pointer type requires its own copy of the pointer parameters),
//!     /// a mapping (`expand_params`) indicates how to create the flattened list.
//!     ```
//!   * and this extra field:
//!     ```
//!     /// For every entry in the regular flattened list of parameters expected by
//!     /// operands, this contains the parameter index (i.e. `0..self.param_count`)
//!     /// to use for that parameter.
//!     ///
//!     /// For example, to duplicate `5` parameters into `10`, `expand_params`
//!     /// would be `[0, 1, 2, 3, 4, 0, 1, 2, 3, 4]`.
//!     ///
//!     /// See also `Generic` documentation above for why this is needed
//!     /// (i.e. to replicate parameters for recursive data types).
//!     expand_params: Option<Vec<usize>>,
//!     ```

use crate::linker::ipo::CallGraph;
use crate::spirv_type_constraints::{self, InstSig, StorageClassPat, TyListPat, TyPat};
use indexmap::{IndexMap, IndexSet};
use rspirv::dr::{Builder, Function, Instruction, Module, Operand};
use rspirv::spirv::{Op, StorageClass, Word};
use rustc_data_structures::captures::Captures;
use rustc_data_structures::fx::{FxHashMap, FxHashSet};
use smallvec::SmallVec;
use std::collections::{BTreeMap, VecDeque};
use std::convert::{TryFrom, TryInto};
use std::ops::{Range, RangeTo};
use std::{fmt, io, iter, mem, slice};

// FIXME(eddyb) move this elsewhere.
struct FmtBy<F: Fn(&mut fmt::Formatter<'_>) -> fmt::Result>(F);

impl<F: Fn(&mut fmt::Formatter<'_>) -> fmt::Result> fmt::Debug for FmtBy<F> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0(f)
    }
}

impl<F: Fn(&mut fmt::Formatter<'_>) -> fmt::Result> fmt::Display for FmtBy<F> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0(f)
    }
}

pub trait Specialization {
    /// Return `true` if the specializer should replace every occurence of
    /// `operand` with some other inferred `Operand`.
    fn specialize_operand(&self, operand: &Operand) -> bool;

    /// The operand that should be used to replace unresolved inference variables,
    /// i.e. the uses of operands for which `specialize_operand` returns `true`,
    /// but which none of the instructions in the same SPIR-V function require
    /// any particular concrete value or relate it to the function's signature,
    /// so an arbitrary choice can be made (as long as it's valid SPIR-V etc.).
    fn concrete_fallback(&self) -> Operand;
}

/// Helper to avoid needing an `impl` of `Specialization`, while allowing the rest
/// of this module to use `Specialization` (instead of `Fn(&Operand) -> bool`).
pub struct SimpleSpecialization<SO: Fn(&Operand) -> bool> {
    pub specialize_operand: SO,
    pub concrete_fallback: Operand,
}

impl<SO: Fn(&Operand) -> bool> Specialization for SimpleSpecialization<SO> {
    fn specialize_operand(&self, operand: &Operand) -> bool {
        (self.specialize_operand)(operand)
    }
    fn concrete_fallback(&self) -> Operand {
        self.concrete_fallback.clone()
    }
}

pub fn specialize(
    opts: &super::Options,
    module: Module,
    specialization: impl Specialization,
) -> Module {
    // FIXME(eddyb) use `log`/`tracing` instead.
    let debug = opts.specializer_debug;
    let dump_instances = &opts.specializer_dump_instances;

    let mut debug_names = FxHashMap::default();
    if debug || dump_instances.is_some() {
        debug_names = module
            .debug_names
            .iter()
            .filter(|inst| inst.class.opcode == Op::Name)
            .map(|inst| {
                (
                    inst.operands[0].unwrap_id_ref(),
                    inst.operands[1].unwrap_literal_string().to_string(),
                )
            })
            .collect();
    }

    let mut specializer = Specializer {
        specialization,

        debug,
        debug_names,

        generics: IndexMap::new(),
        int_consts: FxHashMap::default(),
    };

    specializer.collect_generics(&module);

    // "Generic" module-scoped variables can be fully constrained to the point
    // where we could theoretically always add an instance for them, in order
    // to preserve them, even if they would appear to otherwise be unused.
    // We do this here for fully-constrained variables used by `OpEntryPoint`s,
    // in order to avoid a failure in `Expander::expand_module` (see #723).
    let mut interface_concrete_instances = IndexSet::new();
    for inst in &module.entry_points {
        for interface_operand in &inst.operands[3..] {
            let interface_id = interface_operand.unwrap_id_ref();
            if let Some(generic) = specializer.generics.get(&interface_id) {
                if let Some(param_values) = &generic.param_values {
                    if param_values.iter().all(|v| matches!(v, Value::Known(_))) {
                        interface_concrete_instances.insert(Instance {
                            generic_id: interface_id,
                            generic_args: param_values
                                .iter()
                                .copied()
                                .map(|v| match v {
                                    Value::Known(v) => v,
                                    _ => unreachable!(),
                                })
                                .collect(),
                        });
                    }
                }
            }
        }
    }

    let call_graph = CallGraph::collect(&module);
    let mut non_generic_replacements = vec![];
    for func_idx in call_graph.post_order() {
        if let Some(replacements) = specializer.infer_function(&module.functions[func_idx]) {
            non_generic_replacements.push((func_idx, replacements));
        }
    }

    let mut expander = Expander::new(&specializer, module);

    // See comment above on the loop collecting `interface_concrete_instances`.
    for interface_instance in interface_concrete_instances {
        expander.alloc_instance_id(interface_instance);
    }

    // For non-"generic" functions, we can apply `replacements` right away,
    // though not before finishing inference for all functions first
    // (because `expander` needs to borrow `specializer` immutably).
    if debug {
        eprintln!("non-generic replacements:");
    }
    for (func_idx, replacements) in non_generic_replacements {
        let mut func = mem::replace(
            &mut expander.builder.module_mut().functions[func_idx],
            Function::new(),
        );
        if debug {
            let empty = replacements.with_instance.is_empty()
                && replacements.with_concrete_or_param.is_empty();
            if !empty {
                eprintln!("    in %{}:", func.def_id().unwrap());
            }
        }
        for (loc, operand) in
            replacements.to_concrete(&[], |instance| expander.alloc_instance_id(instance))
        {
            if debug {
                eprintln!("        {} -> {:?}", operand, loc);
            }
            func.index_set(loc, operand.into());
        }
        expander.builder.module_mut().functions[func_idx] = func;
    }
    expander.propagate_instances();

    if let Some(path) = dump_instances {
        expander
            .dump_instances(&mut std::fs::File::create(path).unwrap())
            .unwrap();
    }

    expander.expand_module()
}

// HACK(eddyb) `Copy` version of `Operand` that only includes the cases that
// are relevant to the inference algorithm (and is also smaller).
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum CopyOperand {
    IdRef(Word),
    StorageClass(StorageClass),
}

#[derive(Debug)]
struct NotSupportedAsCopyOperand(Operand);

impl TryFrom<&Operand> for CopyOperand {
    type Error = NotSupportedAsCopyOperand;
    fn try_from(operand: &Operand) -> Result<Self, Self::Error> {
        match *operand {
            Operand::IdRef(id) => Ok(Self::IdRef(id)),
            Operand::StorageClass(s) => Ok(Self::StorageClass(s)),
            _ => Err(NotSupportedAsCopyOperand(operand.clone())),
        }
    }
}

impl From<CopyOperand> for Operand {
    fn from(op: CopyOperand) -> Self {
        match op {
            CopyOperand::IdRef(id) => Self::IdRef(id),
            CopyOperand::StorageClass(s) => Self::StorageClass(s),
        }
    }
}

impl fmt::Display for CopyOperand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::IdRef(id) => write!(f, "%{}", id),
            Self::StorageClass(s) => write!(f, "{:?}", s),
        }
    }
}

/// The "value" of a `Param`/`InferVar`, if we know anything about it.
// FIXME(eddyb) find a more specific name.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
enum Value<T> {
    /// The value of this `Param`/`InferVar` is completely known.
    Unknown,

    /// The value of this `Param`/`InferVar` is known to be a specific `Operand`.
    Known(CopyOperand),

    /// The value of this `Param`/`InferVar` is the same as another `Param`/`InferVar`.
    ///
    /// For consistency, and to allow some `Param` <-> `InferVar` mapping,
    /// all cases of `values[y] == Value::SameAs(x)` should have `x < y`,
    /// i.e. "newer" variables must be redirected to "older" ones.
    SameAs(T),
}

impl<T> Value<T> {
    fn map_var<U>(self, f: impl FnOnce(T) -> U) -> Value<U> {
        match self {
            Value::Unknown => Value::Unknown,
            Value::Known(o) => Value::Known(o),
            Value::SameAs(var) => Value::SameAs(f(var)),
        }
    }
}

/// Newtype'd "generic" parameter index.
// FIXME(eddyb) use `rustc_index` for this instead.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Param(u32);

impl fmt::Display for Param {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "${}", self.0)
    }
}

impl Param {
    // HACK(eddyb) this works around `Range<Param>` not being iterable
    // because `Param` doesn't implement the (unstable) `Step` trait.
    fn range_iter(range: &Range<Self>) -> impl Iterator<Item = Self> + Clone {
        (range.start.0..range.end.0).map(Self)
    }
}

/// A specific instance of a "generic" global/function.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Instance<GA> {
    generic_id: Word,
    generic_args: GA,
}

impl<GA> Instance<GA> {
    fn as_ref(&self) -> Instance<&GA> {
        Instance {
            generic_id: self.generic_id,
            generic_args: &self.generic_args,
        }
    }

    fn map_generic_args<T, U, GA2>(self, f: impl FnMut(T) -> U) -> Instance<GA2>
    where
        GA: IntoIterator<Item = T>,
        GA2: std::iter::FromIterator<U>,
    {
        Instance {
            generic_id: self.generic_id,
            generic_args: self.generic_args.into_iter().map(f).collect(),
        }
    }

    // FIXME(eddyb) implement `Step` for `Param` and `InferVar` instead.
    fn display<'a, T: fmt::Display, GAI: Iterator<Item = T> + Clone>(
        &'a self,
        f: impl FnOnce(&'a GA) -> GAI,
    ) -> impl fmt::Display {
        let &Self {
            generic_id,
            ref generic_args,
        } = self;
        let generic_args_iter = f(generic_args);
        FmtBy(move |f| {
            write!(f, "%{}<", generic_id)?;
            for (i, arg) in generic_args_iter.clone().enumerate() {
                if i != 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{}", arg)?;
            }
            write!(f, ">")
        })
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum InstructionLocation {
    Module,
    FnParam(usize),
    FnBody {
        /// Block index within a function.
        block_idx: usize,

        /// Instruction index within the block with index `block_idx`.
        inst_idx: usize,
    },
}

trait OperandIndexGetSet<I> {
    fn index_get(&self, index: I) -> Operand;
    fn index_set(&mut self, index: I, operand: Operand);
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum OperandIdx {
    ResultType,
    Input(usize),
}

impl OperandIndexGetSet<OperandIdx> for Instruction {
    fn index_get(&self, idx: OperandIdx) -> Operand {
        match idx {
            OperandIdx::ResultType => Operand::IdRef(self.result_type.unwrap()),
            OperandIdx::Input(i) => self.operands[i].clone(),
        }
    }
    fn index_set(&mut self, idx: OperandIdx, operand: Operand) {
        match idx {
            OperandIdx::ResultType => self.result_type = Some(operand.unwrap_id_ref()),
            OperandIdx::Input(i) => self.operands[i] = operand,
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
struct OperandLocation {
    inst_loc: InstructionLocation,
    operand_idx: OperandIdx,
}

impl OperandIndexGetSet<OperandLocation> for Instruction {
    fn index_get(&self, loc: OperandLocation) -> Operand {
        assert_eq!(loc.inst_loc, InstructionLocation::Module);
        self.index_get(loc.operand_idx)
    }
    fn index_set(&mut self, loc: OperandLocation, operand: Operand) {
        assert_eq!(loc.inst_loc, InstructionLocation::Module);
        self.index_set(loc.operand_idx, operand);
    }
}

impl OperandIndexGetSet<OperandLocation> for Function {
    fn index_get(&self, loc: OperandLocation) -> Operand {
        let inst = match loc.inst_loc {
            InstructionLocation::Module => self.def.as_ref().unwrap(),
            InstructionLocation::FnParam(i) => &self.parameters[i],
            InstructionLocation::FnBody {
                block_idx,
                inst_idx,
            } => &self.blocks[block_idx].instructions[inst_idx],
        };
        inst.index_get(loc.operand_idx)
    }
    fn index_set(&mut self, loc: OperandLocation, operand: Operand) {
        let inst = match loc.inst_loc {
            InstructionLocation::Module => self.def.as_mut().unwrap(),
            InstructionLocation::FnParam(i) => &mut self.parameters[i],
            InstructionLocation::FnBody {
                block_idx,
                inst_idx,
            } => &mut self.blocks[block_idx].instructions[inst_idx],
        };
        inst.index_set(loc.operand_idx, operand);
    }
}

// FIXME(eddyb) this is a bit like `Value<Param>` but more explicit,
// and the name isn't too nice, but at least it's very clear.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
enum ConcreteOrParam {
    Concrete(CopyOperand),
    Param(Param),
}

impl ConcreteOrParam {
    /// Replace `Param(i)` with `generic_args[i]` while preserving `Concrete`.
    fn apply_generic_args(self, generic_args: &[CopyOperand]) -> CopyOperand {
        match self {
            Self::Concrete(x) => x,
            Self::Param(Param(i)) => generic_args[i as usize],
        }
    }
}

#[derive(Debug)]
struct Replacements {
    /// Operands that need to be replaced with instances of "generic" globals.
    /// Keyed by instance to optimize for few instances used many times.
    // FIXME(eddyb) fine-tune the length of `SmallVec<[_; 4]>` here.
    with_instance: IndexMap<Instance<SmallVec<[ConcreteOrParam; 4]>>, Vec<OperandLocation>>,

    /// Operands that need to be replaced with a concrete operand or a parameter.
    with_concrete_or_param: Vec<(OperandLocation, ConcreteOrParam)>,
}

impl Replacements {
    /// Apply `generic_args` to all the `ConcreteOrParam`s in this `Replacements`
    /// (i.e. replacing `Param(i)` with `generic_args[i]`), producing a stream of
    /// "replace the operand at `OperandLocation` with this concrete `CopyOperand`".
    /// The `concrete_instance_id` closure should look up and/or allocate an ID
    /// for a specific concrete `Instance`.
    fn to_concrete<'a>(
        &'a self,
        generic_args: &'a [CopyOperand],
        mut concrete_instance_id: impl FnMut(Instance<SmallVec<[CopyOperand; 4]>>) -> Word + 'a,
    ) -> impl Iterator<Item = (OperandLocation, CopyOperand)> + 'a {
        self.with_instance
            .iter()
            .flat_map(move |(instance, locations)| {
                let concrete = CopyOperand::IdRef(concrete_instance_id(
                    instance
                        .as_ref()
                        .map_generic_args(|x| x.apply_generic_args(generic_args)),
                ));
                locations.iter().map(move |&loc| (loc, concrete))
            })
            .chain(
                self.with_concrete_or_param
                    .iter()
                    .map(move |&(loc, x)| (loc, x.apply_generic_args(generic_args))),
            )
    }
}

/// Computed "generic" shape for a SPIR-V global/function. In the interest of efficient
/// representation, the parameters of operands that are themselves "generic",
/// are concatenated by default, i.e. parameters come from disjoint leaves.
///
/// As an example, for `%T = OpTypeStruct %A %B`, if `%A` and `%B` have 2 and 3
/// parameters, respectively, `%T` will have `A0, A1, B0, B1, B2` as parameters.
struct Generic {
    param_count: u32,

    /// Defining instruction for this global (`OpType...`, `OpConstant...`, etc.)
    /// or function (`OpFunction`).
    // FIXME(eddyb) consider using `SmallVec` for the operands, or converting
    // the operands into something more like `InferOperand`, but that would
    // complicate `InferOperandList`, which has to be able to iterate them.
    def: Instruction,

    /// `param_values[p]` constrains what "generic" args `Param(p)` could take.
    /// This is only present if any constraints were inferred from the defining
    /// instruction of a global, or the body of a function. Inference performed
    /// after `collect_generics` (e.g. from instructions in function bodies) is
    /// monotonic, i.e. it may only introduce more constraints, not remove any.
    // FIXME(eddyb) use `rustc_index`'s `IndexVec` for this.
    param_values: Option<Vec<Value<Param>>>,

    /// Operand replacements that need to be performed on the defining instruction
    /// of a global, or an entire function (including all instructions in its body),
    /// in order to expand an instance of it.
    replacements: Replacements,
}

struct Specializer<S: Specialization> {
    specialization: S,

    // FIXME(eddyb) use `log`/`tracing` instead.
    debug: bool,

    // HACK(eddyb) if debugging is requested, this is used to quickly get `OpName`s.
    debug_names: FxHashMap<Word, String>,

    // FIXME(eddyb) compact SPIR-V IDs to allow flatter maps.
    generics: IndexMap<Word, Generic>,

    /// Integer `OpConstant`s (i.e. containing a `LiteralInt32`), to be used
    /// for interpreting `TyPat::IndexComposite` (such as for `OpAccessChain`).
    int_consts: FxHashMap<Word, u32>,
}

impl<S: Specialization> Specializer<S> {
    /// Returns the number of "generic" parameters `operand` "takes", either
    /// because it's specialized by, or it refers to a "generic" global/function.
    /// In the latter case, the `&Generic` for that global/function is also returned.
    fn params_needed_by(&self, operand: &Operand) -> (u32, Option<&Generic>) {
        if self.specialization.specialize_operand(operand) {
            // Each operand we specialize by is one leaf "generic" parameter.
            (1, None)
        } else if let Operand::IdRef(id) = operand {
            self.generics
                .get(id)
                .map_or((0, None), |generic| (generic.param_count, Some(generic)))
        } else {
            (0, None)
        }
    }

    fn collect_generics(&mut self, module: &Module) {
        // Process all defining instructions for globals (types, constants,
        // and module-scoped variables), and functions' `OpFunction` instructions,
        // but note that for `OpFunction`s only the signature is considered,
        // actual inference based on bodies happens later, in `infer_function`.
        let types_global_values_and_functions = module
            .types_global_values
            .iter()
            .chain(module.functions.iter().filter_map(|f| f.def.as_ref()));

        let mut forward_declared_pointers = FxHashSet::default();
        for inst in types_global_values_and_functions {
            let result_id = inst.result_id.unwrap_or_else(|| {
                unreachable!(
                    "Op{:?} is in `types_global_values` but not have a result ID",
                    inst.class.opcode
                );
            });

            if inst.class.opcode == Op::TypeForwardPointer {
                forward_declared_pointers.insert(inst.operands[0].unwrap_id_ref());
            }
            if forward_declared_pointers.remove(&result_id) {
                // HACK(eddyb) this is a forward-declared pointer, pretend
                // it's not "generic" at all to avoid breaking the rest of
                // the logic - see module-level docs for how this should be
                // handled in the future to support recursive data types.
                assert_eq!(inst.class.opcode, Op::TypePointer);
                continue;
            }

            // Record all integer `OpConstant`s (used for `IndexComposite`).
            if inst.class.opcode == Op::Constant {
                if let Operand::LiteralInt32(x) = inst.operands[0] {
                    self.int_consts.insert(result_id, x);
                }
            }

            // Instantiate `inst` in a fresh inference context, to determine
            // how many parameters it needs, and how they might be constrained.
            let (param_count, param_values, replacements) = {
                let mut infer_cx = InferCx::new(self);
                infer_cx.instantiate_instruction(inst, InstructionLocation::Module);

                let param_count = infer_cx.infer_var_values.len() as u32;

                // FIXME(eddyb) dedup this with `infer_function`.
                let param_values = infer_cx
                    .infer_var_values
                    .iter()
                    .map(|v| v.map_var(|InferVar(i)| Param(i)));
                // Only allocate `param_values` if they constrain parameters.
                let param_values = if param_values.clone().any(|v| v != Value::Unknown) {
                    Some(param_values.collect())
                } else {
                    None
                };

                (
                    param_count,
                    param_values,
                    infer_cx.into_replacements(..Param(param_count)),
                )
            };

            // Inference variables become "generic" parameters.
            if param_count > 0 {
                self.generics.insert(
                    result_id,
                    Generic {
                        param_count,
                        def: inst.clone(),
                        param_values,
                        replacements,
                    },
                );
            }
        }
    }

    /// Perform inference across the entire definition of `func`, including all
    /// the instructions in its body, and either store the resulting `Replacements`
    /// in its `Generic` (if `func` is "generic"), or return them otherwise.
    fn infer_function(&mut self, func: &Function) -> Option<Replacements> {
        let func_id = func.def_id().unwrap();

        let param_count = self
            .generics
            .get(&func_id)
            .map_or(0, |generic| generic.param_count);

        let (param_values, replacements) = {
            let mut infer_cx = InferCx::new(self);
            infer_cx.instantiate_function(func);

            // FIXME(eddyb) dedup this with `collect_generics`.
            let param_values = infer_cx.infer_var_values[..param_count as usize]
                .iter()
                .map(|v| v.map_var(|InferVar(i)| Param(i)));
            // Only allocate `param_values` if they constrain parameters.
            let param_values = if param_values.clone().any(|v| v != Value::Unknown) {
                Some(param_values.collect())
            } else {
                None
            };

            (
                param_values,
                infer_cx.into_replacements(..Param(param_count)),
            )
        };

        if let Some(generic) = self.generics.get_mut(&func_id) {
            // All constraints `func` could have from `collect_generics`
            // would have to come from its `OpTypeFunction`, but types don't have
            // internal constraints like e.g. `OpConstant*` and `OpVariable` do.
            assert!(generic.param_values.is_none());

            generic.param_values = param_values;
            generic.replacements = replacements;

            None
        } else {
            Some(replacements)
        }
    }
}

/// Newtype'd inference variable index.
// FIXME(eddyb) use `rustc_index` for this instead.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct InferVar(u32);

impl fmt::Display for InferVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "?{}", self.0)
    }
}

impl InferVar {
    // HACK(eddyb) this works around `Range<InferVar>` not being iterable
    // because `InferVar` doesn't implement the (unstable) `Step` trait.
    fn range_iter(range: &Range<Self>) -> impl Iterator<Item = Self> + Clone {
        (range.start.0..range.end.0).map(Self)
    }
}

struct InferCx<'a, S: Specialization> {
    specializer: &'a Specializer<S>,

    /// `infer_var_values[i]` holds the current state of `InferVar(i)`.
    /// Each inference variable starts out as `Unknown`, may become `SameAs`
    /// pointing to another inference variable, but eventually inference must
    /// result in `Known` values (i.e. concrete `Operand`s).
    // FIXME(eddyb) use `rustc_index`'s `IndexVec` for this.
    infer_var_values: Vec<Value<InferVar>>,

    /// Instantiated *Result Type* of each instruction that has any `InferVar`s,
    /// used when an instruction's result is an input to a later instruction.
    ///
    /// Note that for consistency, for `OpFunction` this contains *Function Type*
    /// instead of *Result Type*, which is inexplicably specified as:
    /// > *Result Type* must be the same as the *Return Type* declared in *Function Type*
    type_of_result: IndexMap<Word, InferOperand>,

    /// Operands that need to be replaced with instances of "generic" globals/functions
    /// (taking as "generic" arguments the results of inference).
    instantiated_operands: Vec<(OperandLocation, Instance<Range<InferVar>>)>,

    /// Operands that need to be replaced with results of inference.
    inferred_operands: Vec<(OperandLocation, InferVar)>,
}

impl<'a, S: Specialization> InferCx<'a, S> {
    fn new(specializer: &'a Specializer<S>) -> Self {
        InferCx {
            specializer,

            infer_var_values: vec![],
            type_of_result: IndexMap::new(),
            instantiated_operands: vec![],
            inferred_operands: vec![],
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum InferOperand {
    Unknown,
    Var(InferVar),
    Concrete(CopyOperand),
    Instance(Instance<Range<InferVar>>),
}

impl InferOperand {
    /// Construct an `InferOperand` based on whether `operand` refers to some
    /// "generic" definition, or we're specializing by it.
    /// Also returns the remaining inference variables, not used by this operand.
    fn from_operand_and_generic_args(
        operand: &Operand,
        generic_args: Range<InferVar>,
        cx: &InferCx<'_, impl Specialization>,
    ) -> (Self, Range<InferVar>) {
        let (needed, generic) = cx.specializer.params_needed_by(operand);
        let split = InferVar(generic_args.start.0 + needed);
        let (generic_args, rest) = (generic_args.start..split, split..generic_args.end);
        (
            if generic.is_some() {
                Self::Instance(Instance {
                    generic_id: operand.unwrap_id_ref(),
                    generic_args,
                })
            } else if needed == 0 {
                CopyOperand::try_from(operand).map_or(Self::Unknown, Self::Concrete)
            } else {
                assert_eq!(needed, 1);
                Self::Var(generic_args.start)
            },
            rest,
        )
    }

    fn display_with_infer_var_values<'a>(
        &'a self,
        infer_var_value: impl Fn(InferVar) -> Value<InferVar> + Copy + 'a,
    ) -> impl fmt::Display + '_ {
        FmtBy(move |f| {
            let var_with_value = |v| {
                FmtBy(move |f| {
                    write!(f, "{}", v)?;
                    match infer_var_value(v) {
                        Value::Unknown => Ok(()),
                        Value::Known(o) => write!(f, " = {}", o),
                        Value::SameAs(v) => write!(f, " = {}", v),
                    }
                })
            };
            match self {
                Self::Unknown => write!(f, "_"),
                Self::Var(v) => write!(f, "{}", var_with_value(*v)),
                Self::Concrete(o) => write!(f, "{}", o),
                Self::Instance(instance) => write!(
                    f,
                    "{}",
                    instance.display(|generic_args| {
                        InferVar::range_iter(generic_args).map(var_with_value)
                    })
                ),
            }
        })
    }

    fn display_with_infer_cx<'a>(
        &'a self,
        cx: &'a InferCx<'_, impl Specialization>,
    ) -> impl fmt::Display + '_ {
        self.display_with_infer_var_values(move |v| {
            // HACK(eddyb) can't use `resolve_infer_var` because that mutates
            // `InferCx` (for the "path compression" union-find optimization).
            let get = |v: InferVar| cx.infer_var_values[v.0 as usize];
            let mut value = get(v);
            while let Value::SameAs(v) = value {
                let next = get(v);
                if next == Value::Unknown {
                    break;
                }
                value = next;
            }
            value
        })
    }
}

impl fmt::Display for InferOperand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.display_with_infer_var_values(|_| Value::Unknown)
            .fmt(f)
    }
}

/// How to filter and/or map the operands in an `InferOperandList`, while iterating.
///
/// Having this in `InferOperandList` itself, instead of using iterator combinators,
/// allows storing `InferOperandList`s directly in `Match`, for `TyPatList` matches.
#[derive(Copy, Clone, PartialEq, Eq)]
enum InferOperandListTransform {
    /// The list is the result of keeping only ID operands, and mapping them to
    /// their types (or `InferOperand::Unknown` for non-value operands, or
    /// value operands which don't have a "generic" type).
    ///
    /// This is used to match against the `inputs` `TyListPat` of `InstSig`.
    TypeOfId,
}

#[derive(Clone, PartialEq)]
struct InferOperandList<'a> {
    operands: &'a [Operand],

    /// Joined ranges of all `InferVar`s needed by individual `Operand`s,
    /// either for `InferOperand::Instance` or `InferOperand::Var`.
    all_generic_args: Range<InferVar>,

    transform: Option<InferOperandListTransform>,
}

impl<'a> InferOperandList<'a> {
    fn split_first(
        &self,
        cx: &InferCx<'_, impl Specialization>,
    ) -> Option<(InferOperand, InferOperandList<'a>)> {
        let mut list = self.clone();
        loop {
            let (first_operand, rest) = list.operands.split_first()?;
            list.operands = rest;

            let (first, rest_args) = InferOperand::from_operand_and_generic_args(
                first_operand,
                list.all_generic_args.clone(),
                cx,
            );
            list.all_generic_args = rest_args;

            // Maybe filter this operand, but only *after* consuming the "generic" args for it.
            match self.transform {
                None => {}

                // Skip a non-ID operand.
                Some(InferOperandListTransform::TypeOfId) => {
                    if first_operand.id_ref_any().is_none() {
                        continue;
                    }
                }
            }

            // Maybe replace this operand with a different one.
            let first = match self.transform {
                None => first,

                // Map `first` to its type.
                Some(InferOperandListTransform::TypeOfId) => match first {
                    InferOperand::Concrete(CopyOperand::IdRef(id)) => cx
                        .type_of_result
                        .get(&id)
                        .cloned()
                        .unwrap_or(InferOperand::Unknown),
                    InferOperand::Unknown | InferOperand::Var(_) | InferOperand::Concrete(_) => {
                        InferOperand::Unknown
                    }
                    InferOperand::Instance(instance) => {
                        let generic = &cx.specializer.generics[&instance.generic_id];

                        // HACK(eddyb) work around the inexplicable fact that `OpFunction` is
                        // specified with a *Result Type* that isn't the type of its *Result*:
                        // > *Result Type* must be the same as the *Return Type* declared in *Function Type*
                        // So we use *Function Type* instead as the type of its *Result*, and
                        // we are helped by `instantiate_instruction`, which ensures that the
                        // "generic" args we have are specifically meant for *Function Type*.
                        let type_of_result = match generic.def.class.opcode {
                            Op::Function => Some(generic.def.operands[1].unwrap_id_ref()),
                            _ => generic.def.result_type,
                        };

                        match type_of_result {
                            Some(type_of_result) => {
                                InferOperand::from_operand_and_generic_args(
                                    &Operand::IdRef(type_of_result),
                                    instance.generic_args,
                                    cx,
                                )
                                .0
                            }
                            None => InferOperand::Unknown,
                        }
                    }
                },
            };

            return Some((first, list));
        }
    }

    fn iter<'b>(
        &self,
        cx: &'b InferCx<'_, impl Specialization>,
    ) -> impl Iterator<Item = InferOperand> + 'b
    where
        'a: 'b,
    {
        let mut list = self.clone();
        iter::from_fn(move || {
            let (next, rest) = list.split_first(cx)?;
            list = rest;
            Some(next)
        })
    }

    fn display_with_infer_cx<'b>(
        &'b self,
        cx: &'b InferCx<'a, impl Specialization>,
    ) -> impl fmt::Display + '_ {
        FmtBy(move |f| {
            f.debug_list()
                .entries(self.iter(cx).map(|operand| {
                    FmtBy(move |f| write!(f, "{}", operand.display_with_infer_cx(cx)))
                }))
                .finish()
        })
    }
}

/// `SmallVec<A>` with a map interface.
#[derive(Default)]
struct SmallIntMap<A: smallvec::Array>(SmallVec<A>);

impl<A: smallvec::Array> SmallIntMap<A> {
    fn get(&self, i: usize) -> Option<&A::Item> {
        self.0.get(i)
    }

    fn get_mut_or_default(&mut self, i: usize) -> &mut A::Item
    where
        A::Item: Default,
    {
        let needed = i + 1;
        if self.0.len() < needed {
            self.0.resize_with(needed, Default::default);
        }
        &mut self.0[i]
    }
}

impl<A: smallvec::Array> IntoIterator for SmallIntMap<A> {
    type Item = (usize, A::Item);
    type IntoIter = iter::Enumerate<smallvec::IntoIter<A>>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter().enumerate()
    }
}

impl<'a, A: smallvec::Array> IntoIterator for &'a mut SmallIntMap<A> {
    type Item = (usize, &'a mut A::Item);
    type IntoIter = iter::Enumerate<slice::IterMut<'a, A::Item>>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.iter_mut().enumerate()
    }
}

#[derive(PartialEq)]
struct IndexCompositeMatch<'a> {
    /// *Indexes* `Operand`s (see `TyPat::IndexComposite`'s doc comment for details).
    indices: &'a [Operand],

    /// The result of indexing the composite type with all `indices`.
    leaf: InferOperand,
}

/// Inference success (e.g. type matched type pattern).
#[must_use]
#[derive(Default)]
struct Match<'a> {
    /// Whether this success isn't guaranteed, because of missing information
    /// (such as the defining instructions of non-"generic" types).
    ///
    /// If there are other alternatives, they will be attempted as well,
    /// and merged using `Match::or` (if they don't result in `Unapplicable`).
    ambiguous: bool,

    // FIXME(eddyb) create some type for these that allows providing common methods
    //
    /// `storage_class_var_found[i][..]` holds all the `InferOperand`s matched by
    /// `StorageClassPat::Var(i)` (currently `i` is always `0`, aka `StorageClassPat::S`).
    storage_class_var_found: SmallIntMap<[SmallVec<[InferOperand; 2]>; 1]>,

    /// `ty_var_found[i][..]` holds all the `InferOperand`s matched by
    /// `TyPat::Var(i)` (currently `i` is always `0`, aka `TyPat::T`).
    ty_var_found: SmallIntMap<[SmallVec<[InferOperand; 4]>; 1]>,

    /// `index_composite_found[i][..]` holds all the `InferOperand`s matched by
    /// `TyPat::IndexComposite(TyPat::Var(i))` (currently `i` is always `0`, aka `TyPat::T`).
    index_composite_ty_var_found: SmallIntMap<[SmallVec<[IndexCompositeMatch<'a>; 1]>; 1]>,

    /// `ty_list_var_found[i][..]` holds all the `InferOperandList`s matched by
    /// `TyListPat::Var(i)` (currently `i` is always `0`, aka `TyListPat::TS`).
    ty_list_var_found: SmallIntMap<[SmallVec<[InferOperandList<'a>; 2]>; 1]>,
}

impl<'a> Match<'a> {
    /// Combine two `Match`es such that the result implies both of them apply,
    /// i.e. contains the union of their constraints.
    fn and(mut self, other: Self) -> Self {
        let Match {
            ambiguous,
            storage_class_var_found,
            ty_var_found,
            index_composite_ty_var_found,
            ty_list_var_found,
        } = &mut self;

        *ambiguous |= other.ambiguous;
        for (i, other_found) in other.storage_class_var_found {
            storage_class_var_found
                .get_mut_or_default(i)
                .extend(other_found);
        }
        for (i, other_found) in other.ty_var_found {
            ty_var_found.get_mut_or_default(i).extend(other_found);
        }
        for (i, other_found) in other.index_composite_ty_var_found {
            index_composite_ty_var_found
                .get_mut_or_default(i)
                .extend(other_found);
        }
        for (i, other_found) in other.ty_list_var_found {
            ty_list_var_found.get_mut_or_default(i).extend(other_found);
        }
        self
    }

    /// Combine two `Match`es such that the result allows for either applying,
    /// i.e. contains the intersection of their constraints.
    fn or(mut self, other: Self) -> Self {
        let Match {
            ambiguous,
            storage_class_var_found,
            ty_var_found,
            index_composite_ty_var_found,
            ty_list_var_found,
        } = &mut self;

        *ambiguous |= other.ambiguous;
        for (i, self_found) in storage_class_var_found {
            let other_found = other
                .storage_class_var_found
                .get(i)
                .map_or(&[][..], |xs| &xs[..]);
            self_found.retain(|x| other_found.contains(x));
        }
        for (i, self_found) in ty_var_found {
            let other_found = other.ty_var_found.get(i).map_or(&[][..], |xs| &xs[..]);
            self_found.retain(|x| other_found.contains(x));
        }
        for (i, self_found) in index_composite_ty_var_found {
            let other_found = other
                .index_composite_ty_var_found
                .get(i)
                .map_or(&[][..], |xs| &xs[..]);
            self_found.retain(|x| other_found.contains(x));
        }
        for (i, self_found) in ty_list_var_found {
            let other_found = other.ty_list_var_found.get(i).map_or(&[][..], |xs| &xs[..]);
            self_found.retain(|x| other_found.contains(x));
        }
        self
    }

    fn debug_with_infer_cx<'b>(
        &'b self,
        cx: &'b InferCx<'a, impl Specialization>,
    ) -> impl fmt::Debug + Captures<'a> + '_ {
        fn debug_var_found<'a, A: smallvec::Array<Item = T> + 'a, T: 'a, TD: fmt::Display>(
            var_found: &'a SmallIntMap<impl smallvec::Array<Item = SmallVec<A>>>,
            display: &'a impl Fn(&'a T) -> TD,
        ) -> impl Iterator<Item = impl fmt::Debug + 'a> + 'a {
            var_found
                .0
                .iter()
                .filter(|found| !found.is_empty())
                .map(move |found| {
                    FmtBy(move |f| {
                        let mut found = found.iter().map(display);
                        write!(f, "{}", found.next().unwrap())?;
                        for x in found {
                            write!(f, " = {}", x)?;
                        }
                        Ok(())
                    })
                })
        }
        FmtBy(move |f| {
            let Self {
                ambiguous,
                storage_class_var_found,
                ty_var_found,
                index_composite_ty_var_found,
                ty_list_var_found,
            } = self;
            write!(f, "Match{} ", if *ambiguous { " (ambiguous)" } else { "" })?;
            let mut list = f.debug_list();
            list.entries(debug_var_found(storage_class_var_found, &move |operand| {
                operand.display_with_infer_cx(cx)
            }));
            list.entries(debug_var_found(ty_var_found, &move |operand| {
                operand.display_with_infer_cx(cx)
            }));
            list.entries(
                index_composite_ty_var_found
                    .0
                    .iter()
                    .enumerate()
                    .filter(|(_, found)| !found.is_empty())
                    .flat_map(|(i, found)| found.iter().map(move |x| (i, x)))
                    .map(move |(i, IndexCompositeMatch { indices, leaf })| {
                        FmtBy(move |f| {
                            match ty_var_found.get(i) {
                                Some(found) if found.len() == 1 => {
                                    write!(f, "{}", found[0].display_with_infer_cx(cx))?;
                                }
                                found => {
                                    let found = found.map_or(&[][..], |xs| &xs[..]);
                                    write!(f, "(")?;
                                    for (j, operand) in found.iter().enumerate() {
                                        if j != 0 {
                                            write!(f, " = ")?;
                                        }
                                        write!(f, "{}", operand.display_with_infer_cx(cx))?;
                                    }
                                    write!(f, ")")?;
                                }
                            }
                            for operand in &indices[..] {
                                // Show the value for literals and IDs pointing to
                                // known `OpConstant`s (e.g. struct field indices).
                                let maybe_idx = match operand {
                                    Operand::IdRef(id) => cx.specializer.int_consts.get(id),
                                    Operand::LiteralInt32(idx) => Some(idx),
                                    _ => None,
                                };
                                match maybe_idx {
                                    Some(idx) => write!(f, ".{}", idx)?,
                                    None => write!(f, "[{}]", operand)?,
                                }
                            }
                            write!(f, " = {}", leaf.display_with_infer_cx(cx))
                        })
                    }),
            );
            list.entries(debug_var_found(ty_list_var_found, &move |list| {
                list.display_with_infer_cx(cx)
            }));
            list.finish()
        })
    }
}

/// Pattern-matching failure, returned by `match_*` when the pattern doesn't apply.
struct Unapplicable;

impl<'a, S: Specialization> InferCx<'a, S> {
    /// Match `storage_class` against `pat`, returning a `Match` with found `Var`s.
    #[allow(clippy::unused_self)] // TODO: remove?
    fn match_storage_class_pat(
        &self,
        pat: &StorageClassPat,
        storage_class: InferOperand,
    ) -> Match<'a> {
        match pat {
            StorageClassPat::Any => Match::default(),
            StorageClassPat::Var(i) => {
                let mut m = Match::default();
                m.storage_class_var_found
                    .get_mut_or_default(*i)
                    .push(storage_class);
                m
            }
        }
    }

    /// Match `ty` against `pat`, returning a `Match` with found `Var`s.
    fn match_ty_pat(&self, pat: &TyPat<'_>, ty: InferOperand) -> Result<Match<'a>, Unapplicable> {
        match pat {
            TyPat::Any => Ok(Match::default()),
            TyPat::Var(i) => {
                let mut m = Match::default();
                m.ty_var_found.get_mut_or_default(*i).push(ty);
                Ok(m)
            }
            TyPat::Either(a, b) => match self.match_ty_pat(a, ty.clone()) {
                Ok(m) if !m.ambiguous => Ok(m),
                a_result => match (a_result, self.match_ty_pat(b, ty)) {
                    (Ok(ma), Ok(mb)) => Ok(ma.or(mb)),
                    (Ok(m), _) | (_, Ok(m)) => Ok(m),
                    (Err(Unapplicable), Err(Unapplicable)) => Err(Unapplicable),
                },
            },
            TyPat::IndexComposite(composite_pat) => match composite_pat {
                TyPat::Var(i) => {
                    let mut m = Match::default();
                    m.index_composite_ty_var_found.get_mut_or_default(*i).push(
                        IndexCompositeMatch {
                            // HACK(eddyb) leave empty `indices` in here for
                            // `match_inst_sig` to fill in, as it has access
                            // to the whole `Instruction` but we don't.
                            indices: &[],
                            leaf: ty,
                        },
                    );
                    Ok(m)
                }
                _ => unreachable!(
                    "`IndexComposite({:?})` isn't supported, only type variable
                     patterns are (for the composite type), e.g. `IndexComposite(T)`",
                    composite_pat
                ),
            },
            _ => {
                let instance = match ty {
                    InferOperand::Unknown | InferOperand::Concrete(_) => {
                        return Ok(Match {
                            ambiguous: true,
                            ..Match::default()
                        })
                    }
                    InferOperand::Var(_) => return Err(Unapplicable),
                    InferOperand::Instance(instance) => instance,
                };
                let generic = &self.specializer.generics[&instance.generic_id];

                let ty_operands = InferOperandList {
                    operands: &generic.def.operands,
                    all_generic_args: instance.generic_args,
                    transform: None,
                };
                let simple = |op, inner_pat| {
                    if generic.def.class.opcode == op {
                        self.match_ty_pat(inner_pat, ty_operands.split_first(self).unwrap().0)
                    } else {
                        Err(Unapplicable)
                    }
                };
                match pat {
                    TyPat::Any | TyPat::Var(_) | TyPat::Either(..) | TyPat::IndexComposite(_) => {
                        unreachable!()
                    }

                    // HACK(eddyb) `TyPat::Void` can't be observed because it's
                    // not "generic", so it would return early as ambiguous.
                    TyPat::Void => unreachable!(),

                    TyPat::Pointer(storage_class_pat, pointee_pat) => {
                        let mut ty_operands = ty_operands.iter(self);
                        let (storage_class, pointee_ty) =
                            (ty_operands.next().unwrap(), ty_operands.next().unwrap());
                        Ok(self
                            .match_storage_class_pat(storage_class_pat, storage_class)
                            .and(self.match_ty_pat(pointee_pat, pointee_ty)?))
                    }
                    TyPat::Array(pat) => simple(Op::TypeArray, pat),
                    TyPat::Vector(pat) => simple(Op::TypeVector, pat),
                    TyPat::Vector4(pat) => match ty_operands.operands {
                        [_, Operand::LiteralInt32(4)] => simple(Op::TypeVector, pat),
                        _ => Err(Unapplicable),
                    },
                    TyPat::Matrix(pat) => simple(Op::TypeMatrix, pat),
                    TyPat::Image(pat) => simple(Op::TypeImage, pat),
                    TyPat::Pipe(_pat) => {
                        if generic.def.class.opcode == Op::TypePipe {
                            Ok(Match::default())
                        } else {
                            Err(Unapplicable)
                        }
                    }
                    TyPat::SampledImage(pat) => simple(Op::TypeSampledImage, pat),
                    TyPat::Struct(fields_pat) => self.match_ty_list_pat(fields_pat, ty_operands),
                    TyPat::Function(ret_pat, params_pat) => {
                        let (ret_ty, params_ty_list) = ty_operands.split_first(self).unwrap();
                        Ok(self
                            .match_ty_pat(ret_pat, ret_ty)?
                            .and(self.match_ty_list_pat(params_pat, params_ty_list)?))
                    }
                }
            }
        }
    }

    /// Match `ty_list` against `pat`, returning a `Match` with found `Var`s.
    fn match_ty_list_pat(
        &self,
        mut list_pat: &TyListPat<'_>,
        mut ty_list: InferOperandList<'a>,
    ) -> Result<Match<'a>, Unapplicable> {
        let mut m = Match::default();

        while let TyListPat::Cons { first: pat, suffix } = list_pat {
            list_pat = suffix;

            let (ty, rest) = ty_list.split_first(self).ok_or(Unapplicable)?;
            ty_list = rest;

            m = m.and(self.match_ty_pat(pat, ty)?);
        }

        match list_pat {
            TyListPat::Cons { .. } => unreachable!(),

            TyListPat::Any => {}
            TyListPat::Var(i) => {
                m.ty_list_var_found.get_mut_or_default(*i).push(ty_list);
            }
            TyListPat::Repeat(repeat_list_pat) => {
                let mut tys = ty_list.iter(self).peekable();
                loop {
                    let mut list_pat = repeat_list_pat;
                    while let TyListPat::Cons { first: pat, suffix } = list_pat {
                        m = m.and(self.match_ty_pat(pat, tys.next().ok_or(Unapplicable)?)?);
                        list_pat = suffix;
                    }
                    assert!(matches!(list_pat, TyListPat::Nil));
                    if tys.peek().is_none() {
                        break;
                    }
                }
            }
            TyListPat::Nil => {
                if ty_list.split_first(self).is_some() {
                    return Err(Unapplicable);
                }
            }
        }

        Ok(m)
    }

    /// Match `inst`'s input operands (with `inputs_generic_args` as "generic" args),
    /// and `result_type`, against `sig`, returning a `Match` with found `Var`s.
    fn match_inst_sig(
        &self,
        sig: &InstSig<'_>,
        inst: &'a Instruction,
        inputs_generic_args: Range<InferVar>,
        result_type: Option<InferOperand>,
    ) -> Result<Match<'a>, Unapplicable> {
        let mut m = Match::default();

        if let Some(pat) = sig.storage_class {
            // FIXME(eddyb) going through all the operands to find the one that
            // is a storage class is inefficient, storage classes should be part
            // of a single unified list of operand patterns.
            let all_operands = InferOperandList {
                operands: &inst.operands,
                all_generic_args: inputs_generic_args.clone(),
                transform: None,
            };
            let storage_class = all_operands
                .iter(self)
                .zip(&inst.operands)
                .filter(|(_, original)| matches!(original, Operand::StorageClass(_)))
                .map(|(operand, _)| operand)
                .next()
                .ok_or(Unapplicable)?;
            m = m.and(self.match_storage_class_pat(pat, storage_class));
        }

        let input_ty_list = InferOperandList {
            operands: &inst.operands,
            all_generic_args: inputs_generic_args,
            transform: Some(InferOperandListTransform::TypeOfId),
        };

        m = m.and(self.match_ty_list_pat(sig.input_types, input_ty_list.clone())?);

        match (sig.output_type, result_type) {
            (Some(pat), Some(result_type)) => {
                m = m.and(self.match_ty_pat(pat, result_type)?);
            }
            (None, None) => {}
            _ => return Err(Unapplicable),
        }

        if !m.index_composite_ty_var_found.0.is_empty() {
            let composite_indices = {
                // Drain the `input_types` prefix (everything before `..`).
                let mut ty_list = input_ty_list;
                let mut list_pat = sig.input_types;
                while let TyListPat::Cons { first: _, suffix } = list_pat {
                    list_pat = suffix;
                    ty_list = ty_list.split_first(self).ok_or(Unapplicable)?.1;
                }

                assert_eq!(
                    list_pat,
                    &TyListPat::Any,
                    "`IndexComposite` must have input types end in `..`"
                );

                // Extract the underlying remaining `operands` - while iterating on
                // the `TypeOfId` list would skip over non-ID operands, and replace
                // ID operands with their types, the `operands` slice is still a
                // subslice of `inst.operands` (minus the prefix we drained above).
                ty_list.operands
            };

            // Fill in all the `indices` fields left empty by `match_ty_pat`.
            for (_, found) in &mut m.index_composite_ty_var_found {
                for index_composite_match in found {
                    let empty = mem::replace(&mut index_composite_match.indices, composite_indices);
                    assert_eq!(empty, &[]);
                }
            }
        }

        Ok(m)
    }

    /// Match `inst`'s input operands (with `inputs_generic_args` as "generic" args),
    /// and `result_type`, against `sigs`, returning a `Match` with found `Var`s.
    fn match_inst_sigs(
        &self,
        sigs: &[InstSig<'_>],
        inst: &'a Instruction,
        inputs_generic_args: Range<InferVar>,
        result_type: Option<InferOperand>,
    ) -> Result<Match<'a>, Unapplicable> {
        let mut result = Err(Unapplicable);
        for sig in sigs {
            result = match (
                result,
                self.match_inst_sig(sig, inst, inputs_generic_args.clone(), result_type.clone()),
            ) {
                (Err(Unapplicable), Ok(m)) if !m.ambiguous => return Ok(m),
                (Ok(a), Ok(b)) => Ok(a.or(b)),
                (Ok(m), _) | (_, Ok(m)) => Ok(m),
                (Err(Unapplicable), Err(Unapplicable)) => Err(Unapplicable),
            };
        }
        result
    }
}

enum InferError {
    /// Mismatch between operands, returned by `equate_*(a, b)` when `a != b`.
    // FIXME(eddyb) track where the mismatched operands come from.
    Conflict(InferOperand, InferOperand),
}

impl InferError {
    fn report(self, inst: &Instruction) {
        // FIXME(eddyb) better error reporting than this.
        match self {
            Self::Conflict(a, b) => {
                eprintln!("inference conflict: {:?} vs {:?}", a, b);
            }
        }
        eprint!("    in ");
        // FIXME(eddyb) deduplicate this with other instruction printing logic.
        if let Some(result_id) = inst.result_id {
            eprint!("%{} = ", result_id);
        }
        eprint!("Op{:?}", inst.class.opcode);
        for operand in inst
            .result_type
            .map(Operand::IdRef)
            .iter()
            .chain(inst.operands.iter())
        {
            eprint!(" {}", operand);
        }
        eprintln!();

        std::process::exit(1);
    }
}

impl<'a, S: Specialization> InferCx<'a, S> {
    /// Traverse `SameAs` chains starting at `x` and return the first `InferVar`
    /// that isn't `SameAs` (i.e. that is `Unknown` or `Known`).
    /// This corresponds to `find(v)` from union-find.
    fn resolve_infer_var(&mut self, v: InferVar) -> InferVar {
        match self.infer_var_values[v.0 as usize] {
            Value::Unknown | Value::Known(_) => v,
            Value::SameAs(next) => {
                let resolved = self.resolve_infer_var(next);
                if resolved != next {
                    // Update the `SameAs` entry for faster lookup next time
                    // (also known as "path compression" in union-find).
                    self.infer_var_values[v.0 as usize] = Value::SameAs(resolved);
                }
                resolved
            }
        }
    }

    /// Enforce that `a = b`, returning a combined `InferVar`, if successful.
    /// This corresponds to `union(a, b)` from union-find.
    fn equate_infer_vars(&mut self, a: InferVar, b: InferVar) -> Result<InferVar, InferError> {
        let (a, b) = (self.resolve_infer_var(a), self.resolve_infer_var(b));

        if a == b {
            return Ok(a);
        }

        // Maintain the invariant that "newer" variables are redirected to "older" ones.
        let (older, newer) = (a.min(b), a.max(b));
        let newer_value = mem::replace(
            &mut self.infer_var_values[newer.0 as usize],
            Value::SameAs(older),
        );
        match (self.infer_var_values[older.0 as usize], newer_value) {
            // Guaranteed by `resolve_infer_var`.
            (Value::SameAs(_), _) | (_, Value::SameAs(_)) => unreachable!(),

            // Both `newer` and `older` had a `Known` value, they must match.
            (Value::Known(x), Value::Known(y)) => {
                if x != y {
                    return Err(InferError::Conflict(
                        InferOperand::Concrete(x),
                        InferOperand::Concrete(y),
                    ));
                }
            }

            // Move the `Known` value from `newer` to `older`.
            (Value::Unknown, Value::Known(_)) => {
                self.infer_var_values[older.0 as usize] = newer_value;
            }

            (_, Value::Unknown) => {}
        }

        Ok(older)
    }

    /// Enforce that `a = b`, returning a combined `Range<InferVar>`, if successful.
    fn equate_infer_var_ranges(
        &mut self,
        a: Range<InferVar>,
        b: Range<InferVar>,
    ) -> Result<Range<InferVar>, InferError> {
        if a == b {
            return Ok(a);
        }

        assert_eq!(a.end.0 - a.start.0, b.end.0 - b.start.0);

        for (a, b) in InferVar::range_iter(&a).zip(InferVar::range_iter(&b)) {
            self.equate_infer_vars(a, b)?;
        }

        // Pick the "oldest" range to maintain the invariant that "newer" variables
        // are redirected to "older" ones, while keeping a contiguous range
        // (instead of splitting it into individual variables), for performance.
        Ok(if a.start < b.start { a } else { b })
    }

    /// Enforce that `a = b`, returning a combined `InferOperand`, if successful.
    fn equate_infer_operands(
        &mut self,
        a: InferOperand,
        b: InferOperand,
    ) -> Result<InferOperand, InferError> {
        if a == b {
            return Ok(a);
        }

        #[allow(clippy::match_same_arms)]
        Ok(match (a.clone(), b.clone()) {
            // Instances of "generic" globals/functions must be of the same ID,
            // and their `generic_args` inference variables must be unified.
            (
                InferOperand::Instance(Instance {
                    generic_id: a_id,
                    generic_args: a_args,
                }),
                InferOperand::Instance(Instance {
                    generic_id: b_id,
                    generic_args: b_args,
                }),
            ) => {
                if a_id != b_id {
                    return Err(InferError::Conflict(a, b));
                }
                InferOperand::Instance(Instance {
                    generic_id: a_id,
                    generic_args: self.equate_infer_var_ranges(a_args, b_args)?,
                })
            }

            // Instances of "generic" globals/functions can never equal anything else.
            (InferOperand::Instance(_), _) | (_, InferOperand::Instance(_)) => {
                return Err(InferError::Conflict(a, b));
            }

            // Inference variables must be unified.
            (InferOperand::Var(a), InferOperand::Var(b)) => {
                InferOperand::Var(self.equate_infer_vars(a, b)?)
            }

            // An inference variable can be assigned a concrete value.
            (InferOperand::Var(v), InferOperand::Concrete(new))
            | (InferOperand::Concrete(new), InferOperand::Var(v)) => {
                let v = self.resolve_infer_var(v);
                match &mut self.infer_var_values[v.0 as usize] {
                    // Guaranteed by `resolve_infer_var`.
                    Value::SameAs(_) => unreachable!(),

                    &mut Value::Known(old) => {
                        if new != old {
                            return Err(InferError::Conflict(
                                InferOperand::Concrete(old),
                                InferOperand::Concrete(new),
                            ));
                        }
                    }

                    value @ Value::Unknown => *value = Value::Known(new),
                }
                InferOperand::Var(v)
            }

            // Concrete `Operand`s must simply match.
            (InferOperand::Concrete(_), InferOperand::Concrete(_)) => {
                // Success case is handled by `if a == b` early return above.
                return Err(InferError::Conflict(a, b));
            }

            // Unknowns can be ignored in favor of non-`Unknown`.
            // NOTE(eddyb) `x` cannot be `Instance`, that is handled above.
            (InferOperand::Unknown, x) | (x, InferOperand::Unknown) => x,
        })
    }

    /// Compute the result ("leaf") type for a `TyPat::IndexComposite` pattern,
    /// by applying each index in `indices` to `composite_ty`, extracting the
    /// element type (for `OpType{Array,RuntimeArray,Vector,Matrix}`), or the
    /// field type for `OpTypeStruct`, where `indices` contains the field index.
    fn index_composite(&self, composite_ty: InferOperand, indices: &[Operand]) -> InferOperand {
        let mut ty = composite_ty;
        for idx in indices {
            let instance = match ty {
                InferOperand::Unknown | InferOperand::Concrete(_) | InferOperand::Var(_) => {
                    return InferOperand::Unknown;
                }
                InferOperand::Instance(instance) => instance,
            };
            let generic = &self.specializer.generics[&instance.generic_id];

            let ty_opcode = generic.def.class.opcode;
            let ty_operands = InferOperandList {
                operands: &generic.def.operands,
                all_generic_args: instance.generic_args,
                transform: None,
            };

            let ty_operands_idx = match ty_opcode {
                Op::TypeArray | Op::TypeRuntimeArray | Op::TypeVector | Op::TypeMatrix => 0,
                Op::TypeStruct => match idx {
                    Operand::IdRef(id) => {
                        *self.specializer.int_consts.get(id).unwrap_or_else(|| {
                            unreachable!("non-constant `OpTypeStruct` field index {}", id);
                        })
                    }
                    &Operand::LiteralInt32(i) => i,
                    _ => {
                        unreachable!("invalid `OpTypeStruct` field index operand {:?}", idx);
                    }
                },
                _ => unreachable!("indexing non-composite type `Op{:?}`", ty_opcode),
            };

            ty = ty_operands
                .iter(self)
                .nth(ty_operands_idx as usize)
                .unwrap_or_else(|| {
                    unreachable!(
                        "out of bounds index {} for `Op{:?}`",
                        ty_operands_idx, ty_opcode
                    );
                });
        }
        ty
    }

    /// Enforce that all the `InferOperand`/`InferOperandList`s found for the
    /// same pattern variable (i.e. `*Pat::Var(i)` with the same `i`), are equal.
    fn equate_match_findings(&mut self, m: Match<'_>) -> Result<(), InferError> {
        let Match {
            ambiguous: _,

            storage_class_var_found,
            ty_var_found,
            index_composite_ty_var_found,
            ty_list_var_found,
        } = m;

        for (_, found) in storage_class_var_found {
            let mut found = found.into_iter();
            if let Some(first) = found.next() {
                found.try_fold(first, |a, b| self.equate_infer_operands(a, b))?;
            }
        }

        for (i, found) in ty_var_found {
            let mut found = found.into_iter();
            if let Some(first) = found.next() {
                let equated_ty = found.try_fold(first, |a, b| self.equate_infer_operands(a, b))?;

                // Apply any `IndexComposite(Var(i))`'s indices to `equated_ty`,
                // and equate the resulting "leaf" type with the found "leaf" type.
                let index_composite_found = index_composite_ty_var_found
                    .get(i)
                    .map_or(&[][..], |xs| &xs[..]);
                for IndexCompositeMatch { indices, leaf } in index_composite_found {
                    let indexing_result_ty = self.index_composite(equated_ty.clone(), indices);
                    self.equate_infer_operands(indexing_result_ty, leaf.clone())?;
                }
            }
        }

        for (_, mut found) in ty_list_var_found {
            if let Some((first_list, other_lists)) = found.split_first_mut() {
                // Advance all the lists in lock-step so that we don't have to
                // allocate state proportional to list length and/or `found.len()`.
                while let Some((first, rest)) = first_list.split_first(self) {
                    *first_list = rest;

                    other_lists.iter_mut().try_fold(first, |a, b_list| {
                        let (b, rest) = b_list
                            .split_first(self)
                            .expect("list length mismatch (invalid SPIR-V?)");
                        *b_list = rest;
                        self.equate_infer_operands(a, b)
                    })?;
                }

                for other_list in other_lists {
                    assert!(
                        other_list.split_first(self).is_none(),
                        "list length mismatch (invalid SPIR-V?)"
                    );
                }
            }
        }

        Ok(())
    }

    /// Track an instantiated operand, to be included in the `Replacements`
    /// (produced by `into_replacements`), if it has any `InferVar`s at all.
    fn record_instantiated_operand(&mut self, loc: OperandLocation, operand: InferOperand) {
        match operand {
            InferOperand::Var(v) => {
                self.inferred_operands.push((loc, v));
            }
            InferOperand::Instance(instance) => {
                self.instantiated_operands.push((loc, instance));
            }
            InferOperand::Unknown | InferOperand::Concrete(_) => {}
        }
    }

    /// Instantiate all of `inst`'s operands (and *Result Type*) that refer to
    /// "generic" globals/functions, or we need to specialize by, with fresh
    /// inference variables, and enforce any inference constraints applicable.
    fn instantiate_instruction(&mut self, inst: &'a Instruction, inst_loc: InstructionLocation) {
        let mut all_generic_args = {
            let next_infer_var = InferVar(self.infer_var_values.len().try_into().unwrap());
            next_infer_var..next_infer_var
        };

        // HACK(eddyb) work around the inexplicable fact that `OpFunction` is
        // specified with a *Result Type* that isn't the type of its *Result*:
        // > *Result Type* must be the same as the *Return Type* declared in *Function Type*
        // Specifically, we don't instantiate *Result Type* (to avoid ending
        // up with redundant `InferVar`s), and instead overlap its "generic" args
        // with that of the *Function Type*, for `instantiations.
        let (instantiate_result_type, record_fn_ret_ty, type_of_result) = match inst.class.opcode {
            Op::Function => (
                None,
                inst.result_type,
                Some(inst.operands[1].unwrap_id_ref()),
            ),
            _ => (inst.result_type, None, inst.result_type),
        };

        for (operand_idx, operand) in instantiate_result_type
            .map(Operand::IdRef)
            .iter()
            .map(|o| (OperandIdx::ResultType, o))
            .chain(
                inst.operands
                    .iter()
                    .enumerate()
                    .map(|(i, o)| (OperandIdx::Input(i), o)),
            )
        {
            // HACK(eddyb) use `v..InferVar(u32::MAX)` as an open-ended range of sorts.
            let (operand, rest) = InferOperand::from_operand_and_generic_args(
                operand,
                all_generic_args.end..InferVar(u32::MAX),
                self,
            );
            let generic_args = all_generic_args.end..rest.start;
            all_generic_args.end = generic_args.end;

            let generic = match &operand {
                InferOperand::Instance(instance) => {
                    Some(&self.specializer.generics[&instance.generic_id])
                }
                _ => None,
            };

            // Initialize the new inference variables (for `operand`'s "generic" args)
            // with either `generic.param_values` (if present) or all `Unknown`s.
            match generic {
                Some(Generic {
                    param_values: Some(values),
                    ..
                }) => self.infer_var_values.extend(
                    values
                        .iter()
                        .map(|v| v.map_var(|Param(p)| InferVar(generic_args.start.0 + p))),
                ),

                _ => {
                    self.infer_var_values
                        .extend(InferVar::range_iter(&generic_args).map(|_| Value::Unknown));
                }
            }

            self.record_instantiated_operand(
                OperandLocation {
                    inst_loc,
                    operand_idx,
                },
                operand,
            );
        }

        // HACK(eddyb) workaround for `OpFunction`, see earlier HACK commment.
        if let Some(ret_ty) = record_fn_ret_ty {
            let (ret_ty, _) = InferOperand::from_operand_and_generic_args(
                &Operand::IdRef(ret_ty),
                all_generic_args.clone(),
                self,
            );
            self.record_instantiated_operand(
                OperandLocation {
                    inst_loc,
                    operand_idx: OperandIdx::ResultType,
                },
                ret_ty,
            );
        }

        // *Result Type* comes first in `all_generic_args`, extract it back out.
        let (type_of_result, inputs_generic_args) = match type_of_result {
            Some(type_of_result) => {
                let (type_of_result, rest) = InferOperand::from_operand_and_generic_args(
                    &Operand::IdRef(type_of_result),
                    all_generic_args.clone(),
                    self,
                );
                (
                    Some(type_of_result),
                    // HACK(eddyb) workaround for `OpFunction`, see earlier HACK commment.
                    match inst.class.opcode {
                        Op::Function => all_generic_args,
                        _ => rest,
                    },
                )
            }
            None => (None, all_generic_args),
        };

        let debug_dump_if_enabled = |cx: &Self, prefix| {
            if cx.specializer.debug {
                let result_type = match inst.class.opcode {
                    // HACK(eddyb) workaround for `OpFunction`, see earlier HACK commment.
                    Op::Function => Some(
                        InferOperand::from_operand_and_generic_args(
                            &Operand::IdRef(inst.result_type.unwrap()),
                            inputs_generic_args.clone(),
                            cx,
                        )
                        .0,
                    ),
                    _ => type_of_result.clone(),
                };
                let inputs = InferOperandList {
                    operands: &inst.operands,
                    all_generic_args: inputs_generic_args.clone(),
                    transform: None,
                };

                if inst_loc != InstructionLocation::Module {
                    eprint!("    ");
                }
                eprint!("{}", prefix);
                if let Some(result_id) = inst.result_id {
                    eprint!("%{} = ", result_id);
                }
                eprint!("Op{:?}", inst.class.opcode);
                for operand in result_type.into_iter().chain(inputs.iter(cx)) {
                    eprint!(" {}", operand.display_with_infer_cx(cx));
                }
                eprintln!();
            }
        };

        // If we have some instruction signatures for `inst`, enforce them.
        if let Some(sigs) = spirv_type_constraints::instruction_signatures(inst.class.opcode) {
            // HACK(eddyb) workaround for `OpFunction`, see earlier HACK commment.
            // (specifically, `type_of_result` isn't *Result Type* for `OpFunction`)
            assert_ne!(inst.class.opcode, Op::Function);

            debug_dump_if_enabled(self, " -> ");

            let m = match self.match_inst_sigs(
                sigs,
                inst,
                inputs_generic_args.clone(),
                type_of_result.clone(),
            ) {
                Ok(m) => m,

                // While this could be an user error *in theory*, we haven't really
                // unified any of the `InferOperand`s found by pattern match variables,
                // at this point, so the only the possible error case is that `inst`
                // doesn't match the *shapes* specified in `sigs`, i.e. this is likely
                // a bug in `spirv_type_constraints`, not some kind of inference conflict.
                Err(Unapplicable) => unreachable!(
                    "spirv_type_constraints(Op{:?}) = `{:?}` doesn't match `{:?}`",
                    inst.class.opcode, sigs, inst
                ),
            };

            if self.specializer.debug {
                if inst_loc != InstructionLocation::Module {
                    eprint!("    ");
                }
                eprintln!("    found {:?}", m.debug_with_infer_cx(self));
            }

            if let Err(e) = self.equate_match_findings(m) {
                e.report(inst);
            }

            debug_dump_if_enabled(self, " <- ");
        } else {
            debug_dump_if_enabled(self, "");
        }

        if let Some(type_of_result) = type_of_result {
            // Keep the (instantiated) *Result Type*, for future instructions to use
            // (but only if it has any `InferVar`s at all).
            match type_of_result {
                InferOperand::Var(_) | InferOperand::Instance(_) => {
                    self.type_of_result
                        .insert(inst.result_id.unwrap(), type_of_result);
                }
                InferOperand::Unknown | InferOperand::Concrete(_) => {}
            }
        }
    }

    /// Instantiate `func`'s definition and all instructions in its body,
    /// effectively performing inference across the entire function body.
    fn instantiate_function(&mut self, func: &'a Function) {
        let func_id = func.def_id().unwrap();

        if self.specializer.debug {
            eprintln!();
            eprint!("specializer::instantiate_function(%{}", func_id);
            if let Some(name) = self.specializer.debug_names.get(&func_id) {
                eprint!(" {}", name);
            }
            eprintln!("):");
        }

        // Instantiate the defining `OpFunction` first, so that the first
        // inference variables match the parameters from the `Generic`
        // (if the `OpTypeFunction` is "generic", that is).
        assert!(self.infer_var_values.is_empty());
        self.instantiate_instruction(func.def.as_ref().unwrap(), InstructionLocation::Module);

        if self.specializer.debug {
            eprintln!("infer body {{");
        }

        // If the `OpTypeFunction` is indeed "generic", we have to extract the
        // return / parameter types for `OpReturnValue` and `OpFunctionParameter`.
        let ret_ty = match self.type_of_result.get(&func_id).cloned() {
            Some(InferOperand::Instance(instance)) => {
                let generic = &self.specializer.generics[&instance.generic_id];
                assert_eq!(generic.def.class.opcode, Op::TypeFunction);

                let (ret_ty, mut params_ty_list) = InferOperandList {
                    operands: &generic.def.operands,
                    all_generic_args: instance.generic_args,
                    transform: None,
                }
                .split_first(self)
                .unwrap();

                // HACK(eddyb) manual iteration to avoid borrowing `self`.
                let mut params = func.parameters.iter().enumerate();
                while let Some((param_ty, rest)) = params_ty_list.split_first(self) {
                    params_ty_list = rest;

                    let (i, param) = params.next().unwrap();
                    assert_eq!(param.class.opcode, Op::FunctionParameter);

                    if self.specializer.debug {
                        eprintln!(
                            "    %{} = Op{:?} {}",
                            param.result_id.unwrap(),
                            param.class.opcode,
                            param_ty.display_with_infer_cx(self)
                        );
                    }

                    self.record_instantiated_operand(
                        OperandLocation {
                            inst_loc: InstructionLocation::FnParam(i),
                            operand_idx: OperandIdx::ResultType,
                        },
                        param_ty.clone(),
                    );
                    match param_ty {
                        InferOperand::Var(_) | InferOperand::Instance(_) => {
                            self.type_of_result
                                .insert(param.result_id.unwrap(), param_ty);
                        }
                        InferOperand::Unknown | InferOperand::Concrete(_) => {}
                    }
                }
                assert_eq!(params.next(), None);

                Some(ret_ty)
            }

            _ => None,
        };

        for (block_idx, block) in func.blocks.iter().enumerate() {
            for (inst_idx, inst) in block.instructions.iter().enumerate() {
                // Manually handle `OpReturnValue`/`OpReturn` because there's no
                // way to inject `ret_ty` into `spirv_type_constraints` rules.
                match inst.class.opcode {
                    Op::ReturnValue => {
                        let ret_val_id = inst.operands[0].unwrap_id_ref();
                        if let (Some(expected), Some(found)) = (
                            ret_ty.clone(),
                            self.type_of_result.get(&ret_val_id).cloned(),
                        ) {
                            if let Err(e) = self.equate_infer_operands(expected, found) {
                                e.report(inst);
                            }
                        }
                    }

                    Op::Return => {}

                    _ => self.instantiate_instruction(
                        inst,
                        InstructionLocation::FnBody {
                            block_idx,
                            inst_idx,
                        },
                    ),
                }
            }
        }

        if self.specializer.debug {
            eprint!("}}");
            if let Some(func_ty) = self.type_of_result.get(&func_id) {
                eprint!(" -> %{}: {}", func_id, func_ty.display_with_infer_cx(self));
            }
            eprintln!();
        }
    }

    /// Helper for `into_replacements`, that computes a single `ConcreteOrParam`.
    /// For all `Param(p)` in `generic_params`, inference variables that resolve
    /// to `InferVar(p)` are replaced with `Param(p)`, whereas other inference
    /// variables are considered unconstrained, and are instead replaced with
    /// `S::concrete_fallback()` (which is chosen by the specialization).
    fn resolve_infer_var_to_concrete_or_param(
        &mut self,
        v: InferVar,
        generic_params: RangeTo<Param>,
    ) -> ConcreteOrParam {
        let v = self.resolve_infer_var(v);
        let InferVar(i) = v;
        match self.infer_var_values[i as usize] {
            // Guaranteed by `resolve_infer_var`.
            Value::SameAs(_) => unreachable!(),

            Value::Unknown => {
                if i < generic_params.end.0 {
                    ConcreteOrParam::Param(Param(i))
                } else {
                    ConcreteOrParam::Concrete(
                        CopyOperand::try_from(&self.specializer.specialization.concrete_fallback())
                            .unwrap(),
                    )
                }
            }
            Value::Known(x) => ConcreteOrParam::Concrete(x),
        }
    }

    /// Consume the `InferCx` and return a set of replacements that need to be
    /// performed to instantiate the global/function inferred with this `InferCx`.
    /// See `resolve_infer_var_to_concrete_or_param` for how inference variables
    /// are handled (using `generic_params` and `S::concrete_fallback()`).
    fn into_replacements(mut self, generic_params: RangeTo<Param>) -> Replacements {
        let mut with_instance: IndexMap<_, Vec<_>> = IndexMap::new();
        for (loc, instance) in mem::take(&mut self.instantiated_operands) {
            with_instance
                .entry(Instance {
                    generic_id: instance.generic_id,
                    generic_args: InferVar::range_iter(&instance.generic_args)
                        .map(|v| self.resolve_infer_var_to_concrete_or_param(v, generic_params))
                        .collect(),
                })
                .or_default()
                .push(loc);
        }

        let with_concrete_or_param = mem::take(&mut self.inferred_operands)
            .into_iter()
            .map(|(loc, v)| {
                (
                    loc,
                    self.resolve_infer_var_to_concrete_or_param(v, generic_params),
                )
            })
            .collect();

        Replacements {
            with_instance,
            with_concrete_or_param,
        }
    }
}

// HACK(eddyb) this state could live in `Specializer` except for the fact that
// it's commonly mutated at the same time as parts of `Specializer` are read,
// and in particular this arrangement allows calling `&mut self` methods on
// `Expander` while (immutably) iterating over data inside the `Specializer`.
struct Expander<'a, S: Specialization> {
    specializer: &'a Specializer<S>,

    builder: Builder,

    /// All the instances of "generic" globals/functions that need to be expanded,
    /// and their cached IDs (which are allocated as-needed, before expansion).
    // NOTE(eddyb) this relies on `BTreeMap` so that `all_instances_of` can use
    // `BTreeMap::range` to get all `Instances` that share a certain ID.
    // FIXME(eddyb) fine-tune the length of `SmallVec<[_; 4]>` here.
    instances: BTreeMap<Instance<SmallVec<[CopyOperand; 4]>>, Word>,

    /// Instances of "generic" globals/functions that have yet to have had their
    /// own `replacements` analyzed in order to fully collect all instances.
    // FIXME(eddyb) fine-tune the length of `SmallVec<[_; 4]>` here.
    propagate_instances_queue: VecDeque<Instance<SmallVec<[CopyOperand; 4]>>>,
}

impl<'a, S: Specialization> Expander<'a, S> {
    fn new(specializer: &'a Specializer<S>, module: Module) -> Self {
        Expander {
            specializer,

            builder: Builder::new_from_module(module),

            instances: BTreeMap::new(),
            propagate_instances_queue: VecDeque::new(),
        }
    }

    /// Return the subset of `instances` that have `generic_id`.
    /// This is efficiently implemented via `BTreeMap::range`, taking advantage
    /// of the derived `Ord` on `Instance`, which orders by `generic_id` first,
    /// resulting in `instances` being grouped by `generic_id`.
    fn all_instances_of(
        &self,
        generic_id: Word,
    ) -> std::collections::btree_map::Range<'_, Instance<SmallVec<[CopyOperand; 4]>>, Word> {
        let first_instance_of = |generic_id| Instance {
            generic_id,
            generic_args: SmallVec::new(),
        };
        self.instances
            .range(first_instance_of(generic_id)..first_instance_of(generic_id + 1))
    }

    /// Allocate a new ID for `instance`, or return a cached one if it exists.
    /// If a new ID is created, `instance` is added to `propagate_instances_queue`,
    /// so that `propagate_instances` can later find all transitive dependencies.
    fn alloc_instance_id(&mut self, instance: Instance<SmallVec<[CopyOperand; 4]>>) -> Word {
        use std::collections::btree_map::Entry;

        match self.instances.entry(instance) {
            Entry::Occupied(entry) => *entry.get(),
            Entry::Vacant(entry) => {
                // Get the `Instance` back from the map key, to avoid having to
                // clone it earlier when calling `self.instances.entry(instance)`.
                let instance = entry.key().clone();

                self.propagate_instances_queue.push_back(instance);
                *entry.insert(self.builder.id())
            }
        }
    }

    /// Process all instances seen (by `alloc_instance_id`) up until this point,
    /// to find the full set of instances (transitively) needed by the module.
    ///
    /// **Warning**: calling `alloc_instance_id` later, without another call to
    /// `propagate_instances`, will potentially result in missed instances, i.e.
    /// that are added to `propagate_instances_queue` but never processed.
    fn propagate_instances(&mut self) {
        while let Some(instance) = self.propagate_instances_queue.pop_back() {
            // Drain the iterator to generate all the `alloc_instance_id` calls.
            for _ in self.specializer.generics[&instance.generic_id]
                .replacements
                .to_concrete(&instance.generic_args, |i| self.alloc_instance_id(i))
            {}
        }
    }

    /// Expand every "generic" global/function, and `OpName`/decorations applied
    /// to them, to their respective full set of instances, treating the original
    /// "generic" definition and its inferred `Replacements` as a template.
    fn expand_module(mut self) -> Module {
        // From here on out we assume all instances are known, so ensure there
        // aren't any left unpropagated.
        self.propagate_instances();

        // HACK(eddyb) steal `Vec`s so that we can still call methods on `self` below.
        let module = self.builder.module_mut();
        let mut entry_points = mem::take(&mut module.entry_points);
        let debug_names = mem::take(&mut module.debug_names);
        let annotations = mem::take(&mut module.annotations);
        let types_global_values = mem::take(&mut module.types_global_values);
        let functions = mem::take(&mut module.functions);

        // Adjust `OpEntryPoint ...` in-place to use the new IDs for *Interface*
        // module-scoped `OpVariable`s (which should each have one instance).
        for inst in &mut entry_points {
            let func_id = inst.operands[1].unwrap_id_ref();
            assert!(
                !self.specializer.generics.contains_key(&func_id),
                "entry-point %{} shouldn't be \"generic\"",
                func_id
            );

            for interface_operand in &mut inst.operands[3..] {
                let interface_id = interface_operand.unwrap_id_ref();
                let mut instances = self.all_instances_of(interface_id);
                match (instances.next(), instances.next()) {
                    (None, _) => unreachable!(
                        "entry-point %{} has overly-\"generic\" \
                         interface variable %{}, with no instances",
                        func_id, interface_id
                    ),
                    (Some(_), Some(_)) => unreachable!(
                        "entry-point %{} has overly-\"generic\" \
                         interface variable %{}, with too many instances: {:?}",
                        func_id,
                        interface_id,
                        FmtBy(|f| f
                            .debug_list()
                            .entries(self.all_instances_of(interface_id).map(
                                |(instance, _)| FmtBy(move |f| write!(
                                    f,
                                    "{}",
                                    instance.display(|generic_args| generic_args.iter().copied())
                                ))
                            ))
                            .finish())
                    ),
                    (Some((_, &instance_id)), None) => {
                        *interface_operand = Operand::IdRef(instance_id);
                    }
                }
            }
        }

        // FIXME(eddyb) bucket `instances` into global vs function, and count
        // annotations separately, so that we can know exact capacities below.

        // Expand `Op* %target ...` when `target` is "generic".
        let expand_debug_or_annotation = |insts: Vec<Instruction>| {
            let mut expanded_insts = Vec::with_capacity(insts.len().next_power_of_two());
            for inst in insts {
                if let [Operand::IdRef(target), ..] = inst.operands[..] {
                    if self.specializer.generics.contains_key(&target) {
                        expanded_insts.extend(self.all_instances_of(target).map(
                            |(_, &instance_id)| {
                                let mut expanded_inst = inst.clone();
                                expanded_inst.operands[0] = Operand::IdRef(instance_id);
                                expanded_inst
                            },
                        ));
                        continue;
                    }
                }
                expanded_insts.push(inst);
            }
            expanded_insts
        };

        // Expand `Op(Member)Name %target ...` when `target` is "generic".
        let expanded_debug_names = expand_debug_or_annotation(debug_names);

        // Expand `Op(Member)Decorate* %target ...`, when `target` is "generic".
        let expanded_annotations = expand_debug_or_annotation(annotations);

        // Expand "generic" globals (types, constants and module-scoped variables).
        let mut expanded_types_global_values =
            Vec::with_capacity(types_global_values.len().next_power_of_two());
        for inst in types_global_values {
            if let Some(result_id) = inst.result_id {
                if let Some(generic) = self.specializer.generics.get(&result_id) {
                    expanded_types_global_values.extend(self.all_instances_of(result_id).map(
                        |(instance, &instance_id)| {
                            let mut expanded_inst = inst.clone();
                            expanded_inst.result_id = Some(instance_id);
                            for (loc, operand) in generic
                                .replacements
                                .to_concrete(&instance.generic_args, |i| self.instances[&i])
                            {
                                expanded_inst.index_set(loc, operand.into());
                            }
                            expanded_inst
                        },
                    ));
                    continue;
                }
            }
            expanded_types_global_values.push(inst);
        }

        // Expand "generic" functions.
        let mut expanded_functions = Vec::with_capacity(functions.len().next_power_of_two());
        for func in functions {
            let func_id = func.def_id().unwrap();
            if let Some(generic) = self.specializer.generics.get(&func_id) {
                let old_expanded_functions_len = expanded_functions.len();
                expanded_functions.extend(self.all_instances_of(func_id).map(
                    |(instance, &instance_id)| {
                        let mut expanded_func = func.clone();
                        expanded_func.def.as_mut().unwrap().result_id = Some(instance_id);
                        for (loc, operand) in generic
                            .replacements
                            .to_concrete(&instance.generic_args, |i| self.instances[&i])
                        {
                            expanded_func.index_set(loc, operand.into());
                        }
                        expanded_func
                    },
                ));

                // Renumber all of the IDs defined within the function itself,
                // to avoid conflicts between all the expanded copies.
                // While some passes (such as inlining) may handle IDs reuse
                // between different function bodies (mostly because they do
                // their own renumbering), it's better not to tempt fate here.
                // FIXME(eddyb) use compact IDs for more efficient renumbering.
                let newly_expanded_functions =
                    &mut expanded_functions[old_expanded_functions_len..];
                if newly_expanded_functions.len() > 1 {
                    // NOTE(eddyb) this is defined outside the loop to avoid
                    // allocating it for every expanded copy of the function.
                    let mut rewrite_rules = FxHashMap::default();

                    for func in newly_expanded_functions {
                        rewrite_rules.extend(func.parameters.iter_mut().map(|param| {
                            let old_id = param.result_id.unwrap();
                            let new_id = self.builder.id();

                            // HACK(eddyb) this is only needed because we're using
                            // `apply_rewrite_rules` and that only works on `Block`s,
                            // it should be generalized to handle `Function`s too.
                            param.result_id = Some(new_id);

                            (old_id, new_id)
                        }));
                        rewrite_rules.extend(
                            func.blocks
                                .iter()
                                .flat_map(|b| b.label.iter().chain(b.instructions.iter()))
                                .filter_map(|inst| inst.result_id)
                                .map(|old_id| (old_id, self.builder.id())),
                        );

                        super::apply_rewrite_rules(&rewrite_rules, &mut func.blocks);
                    }
                }

                continue;
            }
            expanded_functions.push(func);
        }

        // No new instances should've been found during expansion - they would've
        // panicked while attempting to get `self.instances[&instance]` anyway.
        assert!(self.propagate_instances_queue.is_empty());

        let module = self.builder.module_mut();
        module.entry_points = entry_points;
        module.debug_names = expanded_debug_names;
        module.annotations = expanded_annotations;
        module.types_global_values = expanded_types_global_values;
        module.functions = expanded_functions;

        self.builder.module()
    }

    fn dump_instances(&self, w: &mut impl io::Write) -> io::Result<()> {
        writeln!(w, "; All specializer \"generic\"s and their instances:")?;
        writeln!(w)?;

        // FIXME(eddyb) maybe dump (transitive) dependencies? could use a def-use graph.
        for (&generic_id, generic) in &self.specializer.generics {
            if let Some(name) = self.specializer.debug_names.get(&generic_id) {
                writeln!(w, "; {}", name)?;
            }

            write!(
                w,
                "{} = Op{:?}",
                Instance {
                    generic_id,
                    generic_args: Param(0)..Param(generic.param_count)
                }
                .display(Param::range_iter),
                generic.def.class.opcode
            )?;
            let mut next_param = Param(0);
            for operand in generic
                .def
                .result_type
                .map(Operand::IdRef)
                .iter()
                .chain(generic.def.operands.iter())
            {
                write!(w, " ")?;
                let (needed, used_generic) = self.specializer.params_needed_by(operand);
                let params = next_param..Param(next_param.0 + needed);

                // NOTE(eddyb) see HACK comment in `instantiate_instruction`.
                if generic.def.class.opcode != Op::Function {
                    next_param = params.end;
                }

                if used_generic.is_some() {
                    write!(
                        w,
                        "{}",
                        Instance {
                            generic_id: operand.unwrap_id_ref(),
                            generic_args: params
                        }
                        .display(Param::range_iter)
                    )?;
                } else if needed == 1 {
                    write!(w, "{}", params.start)?;
                } else {
                    write!(w, "{}", operand)?;
                }
            }
            writeln!(w)?;

            if let Some(param_values) = &generic.param_values {
                write!(w, "        where")?;
                for (i, v) in param_values.iter().enumerate() {
                    let p = Param(i as u32);
                    match v {
                        Value::Unknown => {}
                        Value::Known(o) => write!(w, " {} = {},", p, o)?,
                        Value::SameAs(q) => write!(w, " {} = {},", p, q)?,
                    }
                }
                writeln!(w)?;
            }

            for (instance, instance_id) in self.all_instances_of(generic_id) {
                assert_eq!(instance.generic_id, generic_id);
                writeln!(
                    w,
                    "    %{} = {}",
                    instance_id,
                    instance.display(|generic_args| generic_args.iter().copied())
                )?;
            }

            writeln!(w)?;
        }
        Ok(())
    }
}
