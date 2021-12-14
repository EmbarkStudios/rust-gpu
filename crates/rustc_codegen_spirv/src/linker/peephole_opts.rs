use super::id;
use rspirv::dr::{Function, Instruction, Module, ModuleHeader, Operand};
use rspirv::spirv::{Op, Word};
use rustc_data_structures::fx::{FxHashMap, FxHashSet};
use rustc_middle::bug;

pub fn collect_types(module: &Module) -> FxHashMap<Word, Instruction> {
    module
        .types_global_values
        .iter()
        .filter_map(|inst| Some((inst.result_id?, inst.clone())))
        .collect()
}

fn composite_count(types: &FxHashMap<Word, Instruction>, ty_id: Word) -> Option<usize> {
    let ty = types.get(&ty_id)?;
    match ty.class.opcode {
        Op::TypeStruct => Some(ty.operands.len()),
        Op::TypeVector => Some(ty.operands[1].unwrap_literal_int32() as usize),
        Op::TypeArray => {
            let length_id = ty.operands[1].unwrap_id_ref();
            let const_inst = types.get(&length_id)?;
            if const_inst.class.opcode != Op::Constant {
                return None;
            }
            let const_ty = types.get(&const_inst.result_type.unwrap())?;
            if const_ty.class.opcode != Op::TypeInt {
                return None;
            }
            let const_value = match const_inst.operands[0] {
                Operand::LiteralInt32(v) => v as usize,
                Operand::LiteralInt64(v) => v as usize,
                _ => bug!(),
            };
            Some(const_value)
        }
        _ => None,
    }
}

/// Given a chain of `OpCompositeInsert` instructions where all slots of the composite are
/// assigned, replace the chain with a single `OpCompositeConstruct`.
pub fn composite_construct(types: &FxHashMap<Word, Instruction>, function: &mut Function) {
    let defs = function
        .all_inst_iter()
        .filter_map(|inst| Some((inst.result_id?, inst.clone())))
        .collect::<FxHashMap<Word, Instruction>>();
    for block in &mut function.blocks {
        for inst in &mut block.instructions {
            if inst.class.opcode != Op::CompositeInsert {
                continue;
            }
            // Get the number of components to expect
            let component_count = match composite_count(types, inst.result_type.unwrap()) {
                Some(c) => c,
                None => continue,
            };
            // Remember a map of index -> value for that index. If any index is missing (None)
            // afterwards, then we know not all slots have been filled in, and we should skip
            // optimizing this chain.
            let mut components = vec![None; component_count];
            let mut cur_inst: &Instruction = inst;
            // Start looping from the current instruction, through each instruction in the chain.
            while cur_inst.class.opcode == Op::CompositeInsert {
                if cur_inst.operands.len() != 3 {
                    // If there's more than one index, skip optimizing this chain.
                    break;
                }
                let value = cur_inst.operands[0].unwrap_id_ref();
                let index = cur_inst.operands[2].unwrap_literal_int32() as usize;
                if index >= components.len() {
                    // Theoretically shouldn't happen, as it's invalid SPIR-V if the index is out
                    // of bounds, but just stop optimizing instead of panicing here.
                    break;
                }
                if components[index].is_none() {
                    components[index] = Some(value);
                }
                // Follow back one in the chain of OpCompositeInsert
                cur_inst = match defs.get(&cur_inst.operands[1].unwrap_id_ref()) {
                    Some(i) => i,
                    None => break,
                };
            }
            // If all components are filled in (collect() returns Some), replace it with
            // `OpCompositeConstruct`
            if let Some(composite_construct_operands) = components
                .into_iter()
                .map(|v| v.map(Operand::IdRef))
                .collect::<Option<Vec<_>>>()
            {
                // Leave all the other instructions in the chain as dead code for other passes
                // to clean up.
                *inst = Instruction::new(
                    Op::CompositeConstruct,
                    inst.result_type,
                    inst.result_id,
                    composite_construct_operands,
                );
            }
        }
    }
}

#[derive(Debug)]
enum IdentifiedOperand {
    /// The operand to the vectorized operation is a straight-up vector.
    Vector(Word),
    /// The operand to the vectorized operation is a collection of scalars that need to be packed
    /// together with OpCompositeConstruct before using the vectorized operation.
    Scalars(Vec<Word>),
    /// The operand to the vectorized operation is some non-value: for example, the `instruction`
    /// operand in OpExtInst.
    NonValue(Operand),
}

/// Given an ID ref to a `OpCompositeExtract`, get the vector it's extracting from, and the field
/// index.
fn get_composite_and_index(
    types: &FxHashMap<Word, Instruction>,
    defs: &FxHashMap<Word, Instruction>,
    id: Word,
    vector_width: u32,
) -> Option<(Word, u32)> {
    let inst = defs.get(&id)?;
    if inst.class.opcode != Op::CompositeExtract {
        return None;
    }
    if inst.operands.len() != 2 {
        // If the index is more than one deep, bail.
        return None;
    }
    let composite = inst.operands[0].unwrap_id_ref();
    let index = inst.operands[1].unwrap_literal_int32();

    let composite_def = defs.get(&composite).or_else(|| types.get(&composite))?;
    let vector_def = types.get(&composite_def.result_type.unwrap())?;

    // Make sure it's a vector and has the width we're expecting.
    // Width mismatch would be doing something like `vec2(a.x + b.x, a.y + b.y)` where `a` is a
    // vec4 - if we optimized it to just `a + b`, it'd be incorrect.
    if vector_def.class.opcode != Op::TypeVector
        || vector_width != vector_def.operands[1].unwrap_literal_int32()
    {
        return None;
    }

    Some((composite, index))
}

/// Given a bunch of operands (`results[n].operands[operand_index]`), where all those operands
/// refer to an `OpCompositeExtract` of the same vector (with proper indices, etc.), return that
/// vector.
fn match_vector_operand(
    types: &FxHashMap<Word, Instruction>,
    defs: &FxHashMap<Word, Instruction>,
    results: &[&Instruction],
    operand_index: usize,
    vector_width: u32,
) -> Option<Word> {
    let operand_zero = match results[0].operands[operand_index] {
        Operand::IdRef(id) => id,
        _ => {
            return None;
        }
    };
    // Extract the composite used for the first component.
    let composite_zero = match get_composite_and_index(types, defs, operand_zero, vector_width) {
        Some((composite_zero, 0)) => composite_zero,
        _ => {
            return None;
        }
    };
    // Check the same composite is used for every other component (and indices line up)
    for (expected_index, result) in results.iter().enumerate().skip(1) {
        let operand = match result.operands[operand_index] {
            Operand::IdRef(id) => id,
            _ => {
                return None;
            }
        };
        let (composite, actual_index) =
            match get_composite_and_index(types, defs, operand, vector_width) {
                Some(x) => x,
                None => {
                    return None;
                }
            };
        // If the source composite isn't all from the same composite, or the index
        // isn't the right index, break.
        if composite != composite_zero || expected_index != actual_index as usize {
            return None;
        }
    }
    Some(composite_zero)
}

/// Either extract out the vector behind each scalar component (see `match_vector_operand`), or
/// just return the collection of scalars for this operand (to be constructed into a vector via
/// `OpCompositeConstruct`).
fn match_vector_or_scalars_operand(
    types: &FxHashMap<Word, Instruction>,
    defs: &FxHashMap<Word, Instruction>,
    results: &[&Instruction],
    operand_index: usize,
    vector_width: u32,
) -> Option<IdentifiedOperand> {
    if let Some(composite) = match_vector_operand(types, defs, results, operand_index, vector_width)
    {
        Some(IdentifiedOperand::Vector(composite))
    } else {
        let operands = results
            .iter()
            .map(|inst| match inst.operands[operand_index] {
                Operand::IdRef(id) => Some(id),
                _ => None,
            })
            .collect::<Option<Vec<_>>>()?;
        Some(IdentifiedOperand::Scalars(operands))
    }
}

/// Make sure all the operands are the same at this index, and return that operand. This is used
/// in, for example, the `instruction` operand for `OpExtInst`.
fn match_all_same_operand(results: &[&Instruction], operand_index: usize) -> Option<Operand> {
    let operand_zero = &results[0].operands[operand_index];
    if results
        .iter()
        .skip(1)
        .all(|inst| &inst.operands[operand_index] == operand_zero)
    {
        Some(operand_zero.clone())
    } else {
        None
    }
}

/// Find the proper operands for the vectorized operation. This means finding the backing vector
/// for each scalar component, etc.
fn match_operands(
    types: &FxHashMap<Word, Instruction>,
    defs: &FxHashMap<Word, Instruction>,
    results: &[&Instruction],
    vector_width: u32,
) -> Option<Vec<IdentifiedOperand>> {
    let operation_opcode = results[0].class.opcode;
    // Check to make sure they're all the same opcode, and have the same number of arguments.
    if results.iter().skip(1).any(|r| {
        r.class.opcode != operation_opcode || r.operands.len() != results[0].operands.len()
    }) {
        return None;
    }
    // TODO: There are probably other instructions relevant here.
    match operation_opcode {
        Op::IAdd
        | Op::FAdd
        | Op::ISub
        | Op::FSub
        | Op::IMul
        | Op::FMul
        | Op::UDiv
        | Op::SDiv
        | Op::FDiv
        | Op::UMod
        | Op::SRem
        | Op::FRem
        | Op::FMod
        | Op::ShiftRightLogical
        | Op::ShiftRightArithmetic
        | Op::ShiftLeftLogical
        | Op::BitwiseOr
        | Op::BitwiseXor
        | Op::BitwiseAnd => {
            let left = match_vector_or_scalars_operand(types, defs, results, 0, vector_width)?;
            let right = match_vector_or_scalars_operand(types, defs, results, 1, vector_width)?;
            match (left, right) {
                // Style choice: If all arguments are scalars, don't fuse this operation.
                (IdentifiedOperand::Scalars(_), IdentifiedOperand::Scalars(_)) => None,
                (left, right) => Some(vec![left, right]),
            }
        }
        Op::SNegate | Op::FNegate | Op::Not | Op::BitReverse => {
            let value = match_vector_operand(types, defs, results, 0, vector_width)?;
            Some(vec![IdentifiedOperand::Vector(value)])
        }
        Op::ExtInst => {
            let set = match_all_same_operand(results, 0)?;
            let instruction = match_all_same_operand(results, 1)?;
            let parameters = (2..results[0].operands.len())
                .map(|i| match_vector_or_scalars_operand(types, defs, results, i, vector_width));
            // Do some trickery to reduce allocations.
            let operands = IntoIterator::into_iter([
                Some(IdentifiedOperand::NonValue(set)),
                Some(IdentifiedOperand::NonValue(instruction)),
            ])
            .chain(parameters)
            .collect::<Option<Vec<_>>>()?;
            if operands
                .iter()
                .skip(2)
                .all(|p| matches!(p, &IdentifiedOperand::Scalars(_)))
            {
                // Style choice: If all arguments are scalars, don't fuse this operation.
                return None;
            }
            Some(operands)
        }
        _ => None,
    }
}

fn process_instruction(
    header: &mut ModuleHeader,
    types: &FxHashMap<Word, Instruction>,
    defs: &FxHashMap<Word, Instruction>,
    instructions: &mut Vec<Instruction>,
    instruction_index: &mut usize,
) -> Option<Instruction> {
    let inst = &instructions[*instruction_index];
    // Basic sanity checks
    if inst.class.opcode != Op::CompositeConstruct {
        return None;
    }
    let inst_result_id = inst.result_id.unwrap();
    let vector_ty = inst.result_type.unwrap();
    let vector_ty_inst = match types.get(&vector_ty) {
        Some(inst) => inst,
        _ => return None,
    };
    if vector_ty_inst.class.opcode != Op::TypeVector {
        return None;
    }
    let vector_width = vector_ty_inst.operands[1].unwrap_literal_int32();
    // `results` is the defining instruction for each scalar component of the final result.
    let results = match inst
        .operands
        .iter()
        .map(|op| defs.get(&op.unwrap_id_ref()))
        .collect::<Option<Vec<_>>>()
    {
        Some(r) => r,
        None => return None,
    };

    let operation_opcode = results[0].class.opcode;
    // Figure out the operands for the vectorized instruction.
    let composite_arguments = match_operands(types, defs, &results, vector_width)?;

    // Fun little optimization: SPIR-V has a fancy OpVectorTimesScalar instruction. If we have a
    // vector times a collection of scalars, and the scalars are all the same, reduce it!
    if operation_opcode == Op::FMul && composite_arguments.len() == 2 {
        if let (&IdentifiedOperand::Vector(composite), IdentifiedOperand::Scalars(scalars))
        | (IdentifiedOperand::Scalars(scalars), &IdentifiedOperand::Vector(composite)) =
            (&composite_arguments[0], &composite_arguments[1])
        {
            let scalar = scalars[0];
            if scalars.iter().skip(1).all(|&s| s == scalar) {
                return Some(Instruction::new(
                    Op::VectorTimesScalar,
                    inst.result_type,
                    inst.result_id,
                    vec![Operand::IdRef(composite), Operand::IdRef(scalar)],
                ));
            }
        }
    }

    // Map the operands into their concrete representations: vectors and non-values stay as-is, but
    // we need to emit an OpCompositeConstruct instruction for scalar collections.
    let operands = composite_arguments
        .into_iter()
        .map(|operand| match operand {
            IdentifiedOperand::Vector(composite) => Operand::IdRef(composite),
            IdentifiedOperand::NonValue(operand) => operand,
            IdentifiedOperand::Scalars(scalars) => {
                let id = super::id(header);
                // spirv-opt will transform this into an OpConstantComposite if all arguments are
                // constant, so we don't have to worry about that.
                instructions.insert(
                    *instruction_index,
                    Instruction::new(
                        Op::CompositeConstruct,
                        Some(vector_ty),
                        Some(id),
                        scalars.into_iter().map(Operand::IdRef).collect(),
                    ),
                );
                *instruction_index += 1;
                Operand::IdRef(id)
            }
        })
        .collect();

    Some(Instruction::new(
        operation_opcode,
        Some(vector_ty),
        Some(inst_result_id),
        operands,
    ))
}

/// Fuse a sequence of scalar operations into a single vector operation. For example:
/// ```
/// %x_0 = OpCompositeExtract %x 0
/// %x_1 = OpCompositeExtract %x 1
/// %y_0 = OpCompositeExtract %y 0
/// %y_1 = OpCompositeExtract %y 1
/// %r_0 = OpAdd %x_0 %y_0
/// %r_1 = OpAdd %x_1 %y_1
/// %r = OpCompositeConstruct %r_0 %r_1
/// ```
/// into
/// ```
/// %r = OpAdd %x %y
/// ```
/// (We don't remove the intermediate instructions, however, in case they're used elsewhere - we
/// let spirv-opt remove them if they're actually dead)
pub fn vector_ops(
    header: &mut ModuleHeader,
    types: &FxHashMap<Word, Instruction>,
    function: &mut Function,
) {
    let defs = function
        .all_inst_iter()
        .filter_map(|inst| Some((inst.result_id?, inst.clone())))
        .collect::<FxHashMap<Word, Instruction>>();
    for block in &mut function.blocks {
        // It'd be nice to iterate over &mut block.instructions, but there's a weird case: if we
        // have a vector plus a collection of scalars, we want to pack the collection of scalars
        // into a vector and do a vector+vector op. That means we need to insert an extra
        // OpCompositeConstruct into the block, so, we need to manually keep track of the current
        // index and do a while loop.
        let mut instruction_index = 0;
        while instruction_index < block.instructions.len() {
            if let Some(result) = process_instruction(
                header,
                types,
                &defs,
                &mut block.instructions,
                &mut instruction_index,
            ) {
                // Leave all the other instructions in the chain as dead code for other passes
                // to clean up.
                block.instructions[instruction_index] = result;
            }

            instruction_index += 1;
        }
    }
}

fn can_fuse_bool(
    types: &FxHashMap<Word, Instruction>,
    defs: &FxHashMap<Word, (usize, Instruction)>,
    inst: &Instruction,
) -> bool {
    fn constant_value(types: &FxHashMap<Word, Instruction>, val: Word) -> Option<u32> {
        let inst = match types.get(&val) {
            None => return None,
            Some(inst) => inst,
        };
        if inst.class.opcode != Op::Constant {
            return None;
        }
        match inst.operands[0] {
            Operand::LiteralInt32(v) => Some(v),
            _ => None,
        }
    }

    fn visit(
        types: &FxHashMap<Word, Instruction>,
        defs: &FxHashMap<Word, (usize, Instruction)>,
        visited: &mut FxHashSet<Word>,
        value: Word,
    ) -> bool {
        if visited.insert(value) {
            let inst = match defs.get(&value) {
                Some((_, inst)) => inst,
                None => return false,
            };
            match inst.class.opcode {
                Op::Select => {
                    constant_value(types, inst.operands[1].unwrap_id_ref()) == Some(1)
                        && constant_value(types, inst.operands[2].unwrap_id_ref()) == Some(0)
                }
                Op::Phi => inst
                    .operands
                    .iter()
                    .step_by(2)
                    .all(|op| visit(types, defs, visited, op.unwrap_id_ref())),
                _ => false,
            }
        } else {
            true
        }
    }

    if inst.class.opcode != Op::INotEqual
        || constant_value(types, inst.operands[1].unwrap_id_ref()) != Some(0)
    {
        return false;
    }
    let int_value = inst.operands[0].unwrap_id_ref();

    visit(types, defs, &mut FxHashSet::default(), int_value)
}

fn fuse_bool(
    header: &mut ModuleHeader,
    defs: &FxHashMap<Word, (usize, Instruction)>,
    phis_to_insert: &mut Vec<(usize, Instruction)>,
    already_mapped: &mut FxHashMap<Word, Word>,
    bool_ty: Word,
    int_value: Word,
) -> Word {
    if let Some(&result) = already_mapped.get(&int_value) {
        return result;
    }
    let (block_of_inst, inst) = defs.get(&int_value).unwrap();
    match inst.class.opcode {
        Op::Select => inst.operands[0].unwrap_id_ref(),
        Op::Phi => {
            let result_id = id(header);
            already_mapped.insert(int_value, result_id);
            let new_phi_args = inst
                .operands
                .chunks(2)
                .flat_map(|arr| {
                    let phi_value = &arr[0];
                    let block = &arr[1];
                    [
                        Operand::IdRef(fuse_bool(
                            header,
                            defs,
                            phis_to_insert,
                            already_mapped,
                            bool_ty,
                            phi_value.unwrap_id_ref(),
                        )),
                        block.clone(),
                    ]
                })
                .collect::<Vec<_>>();
            let inst = Instruction::new(Op::Phi, Some(bool_ty), Some(result_id), new_phi_args);
            phis_to_insert.push((*block_of_inst, inst));
            result_id
        }
        _ => bug!("can_fuse_bool should have prevented this case"),
    }
}

// The compiler generates a lot of code that looks like this:
// %v_int = OpSelect %int %v %const_1 %const_0
// %v2 = OpINotEqual %bool %v_int %const_0
// (This is due to rustc/spirv not supporting bools in memory, and needing to convert to u8, but
// then things get inlined/mem2reg'd)
//
// This pass fuses together those two instructions to strip out the intermediate integer variable.
// The purpose is to make simple code that doesn't actually do memory-stuff with bools not require
// the Int8 capability (and so we can't rely on spirv-opt to do this same pass).
//
// Unfortunately, things get complicated because of phis: the majority of actually useful cases to
// do this pass need to track pseudo-bool ints through phi instructions.
//
// The logic goes like:
// 1) Figure out what we *can* fuse. This means finding OpINotEqual instructions (converting back
//    from int->bool) and tracing the value back recursively through any phis, and making sure each
//    one terminates in either a loop back around to something we've already seen, or an OpSelect
//    (converting from bool->int).
// 2) Do the fusion. Trace back through phis, generating a second bool-typed phi alongside the
//    original int-typed phi, and when hitting an OpSelect, taking the bool value directly.
// 3) DCE the dead OpSelects/int-typed OpPhis (done in a later pass). We don't nuke them here,
//    since they might be used elsewhere, and don't want to accidentally leave a dangling
//    reference.
pub fn bool_fusion(
    header: &mut ModuleHeader,
    types: &FxHashMap<Word, Instruction>,
    function: &mut Function,
) {
    let defs: FxHashMap<Word, (usize, Instruction)> = function
        .blocks
        .iter()
        .enumerate()
        .flat_map(|(block_id, block)| {
            block
                .instructions
                .iter()
                .filter_map(move |inst| Some((inst.result_id?, (block_id, inst.clone()))))
        })
        .collect();
    let mut rewrite_rules = FxHashMap::default();
    let mut phis_to_insert = Default::default();
    let mut already_mapped = Default::default();
    for block in &mut function.blocks {
        for inst in &mut block.instructions {
            if can_fuse_bool(types, &defs, inst) {
                let rewrite_to = fuse_bool(
                    header,
                    &defs,
                    &mut phis_to_insert,
                    &mut already_mapped,
                    inst.result_type.unwrap(),
                    inst.operands[0].unwrap_id_ref(),
                );
                rewrite_rules.insert(inst.result_id.unwrap(), rewrite_to);
                *inst = Instruction::new(Op::Nop, None, None, Vec::new());
            }
        }
    }
    for (block, phi) in phis_to_insert {
        function.blocks[block].instructions.insert(0, phi);
    }
    super::apply_rewrite_rules(&rewrite_rules, &mut function.blocks);
}
