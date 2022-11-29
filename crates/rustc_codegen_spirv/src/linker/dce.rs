//! Dead code elimination
//!
//! This pass removes any instruction that doesn't affect the module. It does so by considering all
//! `OpEntryPoint` instructions to be "rooted", and then everything a rooted instruction touches is
//! also rooted (done transitively). Then, any instruction not rooted is removed. It gets a little
//! weird with things like `OpDecorate`, where the reference is reversed - an `OpDecorate` that
//! *references* a rooted thing is also rooted, not the other way around - but that's the basic
//! concept.

use rspirv::dr::{Function, Instruction, Module, Operand};
use rspirv::spirv::{Decoration, LinkageType, Op, StorageClass, Word};
use rustc_data_structures::fx::FxHashSet;

pub fn dce(module: &mut Module) {
    let mut rooted = collect_roots(module);
    while spread_roots(module, &mut rooted) {}
    kill_unrooted(module, &rooted);
}

pub fn collect_roots(module: &Module) -> FxHashSet<Word> {
    let mut rooted = FxHashSet::default();

    for inst in &module.entry_points {
        root(inst, &mut rooted);
    }

    // NOTE(eddyb) such "link exports" roots are only relevant when `Options`'s
    // `keep_link_export`s field is used to request that `Export`s are left in
    // (primarily for unit testing - see also its doc comment).
    for inst in &module.annotations {
        if inst.class.opcode == Op::Decorate
            && inst.operands[1].unwrap_decoration() == Decoration::LinkageAttributes
            && inst.operands[3].unwrap_linkage_type() == LinkageType::Export
        {
            root(inst, &mut rooted);
        }
    }

    rooted
}

// Exactly the same as Function::all_inst_iter, except return type is `impl DoubleEndedIterator`
// instead of `impl Iterator`
fn all_inst_iter(func: &Function) -> impl DoubleEndedIterator<Item = &Instruction> {
    func.def
        .iter()
        .chain(func.parameters.iter())
        .chain(
            func.blocks
                .iter()
                .flat_map(|b| b.label.iter().chain(b.instructions.iter())),
        )
        .chain(func.end.iter())
}

fn spread_roots(module: &Module, rooted: &mut FxHashSet<Word>) -> bool {
    let mut any = false;
    for inst in module.global_inst_iter() {
        if let Some(id) = inst.result_id {
            if rooted.contains(&id) {
                any |= root(inst, rooted);
            }
        }
    }
    for func in &module.functions {
        if rooted.contains(&func.def_id().unwrap()) {
            // NB (Mobius 2021) - since later insts are much more likely to reference
            // earlier insts, by reversing the iteration order, we're more likely to root the
            // entire relevant function at once.
            // See https://github.com/EmbarkStudios/rust-gpu/pull/691#discussion_r681477091
            for inst in all_inst_iter(func).rev() {
                if !instruction_is_pure(inst) {
                    any |= root(inst, rooted);
                } else if let Some(id) = inst.result_id {
                    if rooted.contains(&id) {
                        any |= root(inst, rooted);
                    }
                }
            }
        }
    }
    any
}

fn root(inst: &Instruction, rooted: &mut FxHashSet<Word>) -> bool {
    let mut any = false;
    if let Some(id) = inst.result_type {
        any |= rooted.insert(id);
    }
    for op in &inst.operands {
        if let Some(id) = op.id_ref_any() {
            any |= rooted.insert(id);
        }
    }
    any
}

fn is_rooted(inst: &Instruction, rooted: &FxHashSet<Word>) -> bool {
    if let Some(result_id) = inst.result_id {
        rooted.contains(&result_id)
    } else {
        // For things like OpDecorate which apply attributes to rooted things, but are not
        // referenced by roots
        inst.operands
            .iter()
            .any(|op| op.id_ref_any().map_or(false, |w| rooted.contains(&w)))
    }
}

fn kill_unrooted(module: &mut Module, rooted: &FxHashSet<Word>) {
    module
        .ext_inst_imports
        .retain(|inst| is_rooted(inst, rooted));
    module
        .execution_modes
        .retain(|inst| is_rooted(inst, rooted));
    module
        .debug_string_source
        .retain(|inst| is_rooted(inst, rooted));
    module.debug_names.retain(|inst| is_rooted(inst, rooted));
    module
        .debug_module_processed
        .retain(|inst| is_rooted(inst, rooted));
    module.annotations.retain(|inst| is_rooted(inst, rooted));
    module
        .types_global_values
        .retain(|inst| is_rooted(inst, rooted));
    module
        .functions
        .retain(|f| is_rooted(f.def.as_ref().unwrap(), rooted));
    for fun in &mut module.functions {
        for block in &mut fun.blocks {
            block
                .instructions
                .retain(|inst| !instruction_is_pure(inst) || is_rooted(inst, rooted));
        }
    }
}

pub fn dce_phi(func: &mut Function) {
    let mut used = FxHashSet::default();
    loop {
        let mut changed = false;
        for inst in func.all_inst_iter() {
            if inst.class.opcode != Op::Phi || used.contains(&inst.result_id.unwrap()) {
                for op in &inst.operands {
                    if let Some(id) = op.id_ref_any() {
                        changed |= used.insert(id);
                    }
                }
            }
        }
        if !changed {
            break;
        }
    }
    for block in &mut func.blocks {
        block
            .instructions
            .retain(|inst| inst.class.opcode != Op::Phi || used.contains(&inst.result_id.unwrap()));
    }
}

fn instruction_is_pure(inst: &Instruction) -> bool {
    use Op::*;
    match inst.class.opcode {
        Nop
        | Undef
        | ConstantTrue
        | ConstantFalse
        | Constant
        | ConstantComposite
        | ConstantSampler
        | ConstantNull
        | AccessChain
        | InBoundsAccessChain
        | PtrAccessChain
        | ArrayLength
        | InBoundsPtrAccessChain
        | CompositeConstruct
        | CompositeExtract
        | CompositeInsert
        | CopyObject
        | Transpose
        | ConvertFToU
        | ConvertFToS
        | ConvertSToF
        | ConvertUToF
        | UConvert
        | SConvert
        | FConvert
        | QuantizeToF16
        | ConvertPtrToU
        | SatConvertSToU
        | SatConvertUToS
        | ConvertUToPtr
        | PtrCastToGeneric
        | GenericCastToPtr
        | GenericCastToPtrExplicit
        | Bitcast
        | SNegate
        | FNegate
        | IAdd
        | FAdd
        | ISub
        | FSub
        | IMul
        | FMul
        | UDiv
        | SDiv
        | FDiv
        | UMod
        | SRem
        | SMod
        | FRem
        | FMod
        | VectorTimesScalar
        | MatrixTimesScalar
        | VectorTimesMatrix
        | MatrixTimesVector
        | MatrixTimesMatrix
        | OuterProduct
        | Dot
        | IAddCarry
        | ISubBorrow
        | UMulExtended
        | SMulExtended
        | Any
        | All
        | IsNan
        | IsInf
        | IsFinite
        | IsNormal
        | SignBitSet
        | LessOrGreater
        | Ordered
        | Unordered
        | LogicalEqual
        | LogicalNotEqual
        | LogicalOr
        | LogicalAnd
        | LogicalNot
        | Select
        | IEqual
        | INotEqual
        | UGreaterThan
        | SGreaterThan
        | UGreaterThanEqual
        | SGreaterThanEqual
        | ULessThan
        | SLessThan
        | ULessThanEqual
        | SLessThanEqual
        | FOrdEqual
        | FUnordEqual
        | FOrdNotEqual
        | FUnordNotEqual
        | FOrdLessThan
        | FUnordLessThan
        | FOrdGreaterThan
        | FUnordGreaterThan
        | FOrdLessThanEqual
        | FUnordLessThanEqual
        | FOrdGreaterThanEqual
        | FUnordGreaterThanEqual
        | ShiftRightLogical
        | ShiftRightArithmetic
        | ShiftLeftLogical
        | BitwiseOr
        | BitwiseXor
        | BitwiseAnd
        | Not
        | BitFieldInsert
        | BitFieldSExtract
        | BitFieldUExtract
        | BitReverse
        | BitCount
        | Phi
        | SizeOf
        | CopyLogical
        | PtrEqual
        | PtrNotEqual
        | PtrDiff => true,
        Variable => inst.operands.get(0) == Some(&Operand::StorageClass(StorageClass::Function)),
        _ => false,
    }
}
