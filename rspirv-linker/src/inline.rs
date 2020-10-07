use crate::{operand_idref, operand_idref_mut};
use rspirv::dr::{Block, Function, Instruction, Module, ModuleHeader, Operand};
use rspirv::spirv::{FunctionControl, Op, StorageClass, Word};
use std::collections::{HashMap, HashSet};

type FunctionMap = HashMap<Word, Function>;

pub fn inline(module: &mut Module) {
    let functions = module
        .functions
        .iter()
        .map(|f| (f.def.as_ref().unwrap().result_id.unwrap(), f.clone()))
        .collect();
    let disallowed_argument_types = compute_disallowed_argument_types(module);
    // Drop all the functions we'll be inlining. (This also means we won't waste time processing
    // inlines in functions that will get inlined)
    module
        .functions
        .retain(|f| !should_inline(&disallowed_argument_types, f));
    let mut inliner = Inliner {
        header: &mut module.header.as_mut().unwrap(),
        types_global_values: &mut module.types_global_values,
        functions: &functions,
        disallowed_argument_types: &disallowed_argument_types,
    };
    for function in &mut module.functions {
        inliner.inline_fn(function);
    }
}

fn compute_disallowed_argument_types(module: &Module) -> HashSet<Word> {
    let allowed_argument_storage_classes = &[
        StorageClass::UniformConstant,
        StorageClass::Function,
        StorageClass::Private,
        StorageClass::Workgroup,
        StorageClass::AtomicCounter,
        // TODO: StorageBuffer is allowed if VariablePointers is enabled
    ];
    let pointers = module
        .types_global_values
        .iter()
        .filter(|inst| inst.class.opcode == Op::TypePointer)
        .map(|inst| inst.result_id.unwrap())
        .collect::<HashSet<_>>();
    module
        .types_global_values
        .iter()
        .filter(|inst| {
            inst.class.opcode == Op::TypePointer
                && match inst.operands[0] {
                    Operand::StorageClass(class) => {
                        !allowed_argument_storage_classes.contains(&class)
                            || (class == StorageClass::Function
                                && pointers
                                    .contains(operand_idref(&inst.operands[1]).as_ref().unwrap()))
                    }
                    _ => panic!(),
                }
        })
        .map(|inst| inst.result_id.unwrap())
        .collect()
}

fn should_inline(disallowed_argument_types: &HashSet<Word>, function: &Function) -> bool {
    let def = function.def.as_ref().unwrap();
    let control = match def.operands[0] {
        Operand::FunctionControl(control) => control,
        _ => panic!(),
    };
    control.contains(FunctionControl::INLINE)
        || function
            .parameters
            .iter()
            .any(|inst| disallowed_argument_types.contains(inst.result_type.as_ref().unwrap()))
}

// Steps:
// Move OpVariable decls
// Rewrite return
// Renumber IDs
// Insert blocks

struct Inliner<'m, 'map> {
    header: &'m mut ModuleHeader,
    types_global_values: &'m mut Vec<Instruction>,
    functions: &'map FunctionMap,
    disallowed_argument_types: &'map HashSet<Word>,
    // rewrite_rules: HashMap<Word, Word>,
}

impl Inliner<'_, '_> {
    fn id(&mut self) -> Word {
        let result = self.header.bound;
        self.header.bound += 1;
        result
    }

    fn ptr_ty(&mut self, pointee: Word) -> Word {
        // TODO: This is horribly slow, fix this
        let existing = self.types_global_values.iter().find(|inst| {
            inst.class.opcode == Op::TypePointer
                && inst.operands[0] == Operand::StorageClass(StorageClass::Function)
                && inst.operands[1] == Operand::IdRef(pointee)
        });
        if let Some(existing) = existing {
            return existing.result_id.unwrap();
        }
        let inst_id = self.id();
        self.types_global_values.push(Instruction::new(
            Op::TypePointer,
            None,
            Some(inst_id),
            vec![
                Operand::StorageClass(StorageClass::Function),
                Operand::IdRef(pointee),
            ],
        ));
        inst_id
    }

    fn inline_fn(&mut self, function: &mut Function) {
        let mut block_idx = 0;
        while block_idx < function.blocks.len() {
            // If we successfully inlined a block, then repeat processing on the same block, in
            // case the newly inlined block has more inlined calls.
            // TODO: This is quadratic
            if !self.inline_block(function, block_idx) {
                block_idx += 1;
            }
        }
    }

    fn inline_block(&mut self, caller: &mut Function, block_idx: usize) -> bool {
        // Find the first inlined OpFunctionCall
        let call = caller.blocks[block_idx]
            .instructions
            .iter()
            .enumerate()
            .filter(|(_, inst)| inst.class.opcode == Op::FunctionCall)
            .map(|(index, inst)| {
                (
                    index,
                    inst,
                    self.functions
                        .get(&operand_idref(&inst.operands[0]).unwrap())
                        .unwrap(),
                )
            })
            .find(|(_, _, f)| should_inline(self.disallowed_argument_types, f));
        let (call_index, call_inst, callee) = match call {
            None => return false,
            Some(call) => call,
        };
        let call_result_type = call_inst.result_type.unwrap();
        let call_result_id = call_inst.result_id.unwrap();
        // Rewrite parameters to arguments
        let call_arguments = call_inst
            .operands
            .iter()
            .skip(1)
            .map(|op| operand_idref(op).unwrap());
        let callee_parameters = callee.parameters.iter().map(|inst| {
            assert!(inst.class.opcode == Op::FunctionParameter);
            inst.result_id.unwrap()
        });
        let mut rewrite_rules = callee_parameters.zip(call_arguments).collect();

        let return_variable = self.id();
        let return_jump = self.id();
        // Rewrite OpReturns of the callee.
        let mut inlined_blocks = get_inlined_blocks(callee, return_variable, return_jump);
        // Clone the IDs of the callee, because otherwise they'd be defined multiple times if the
        // fn is inlined multiple times.
        self.add_clone_id_rules(&mut rewrite_rules, &inlined_blocks);
        Self::apply_rewrite_rules(&rewrite_rules, &mut inlined_blocks);

        // Split the block containing the OpFunctionCall into two, around the call.
        let mut post_call_block_insts = caller.blocks[block_idx]
            .instructions
            .split_off(call_index + 1);
        // pop off OpFunctionCall
        let call = caller.blocks[block_idx].instructions.pop().unwrap();
        assert!(call.class.opcode == Op::FunctionCall);

        // Generate the storage space for the return value: Do this *after* the split above,
        // because if block_idx=0, inserting a variable here shifts call_index.
        insert_opvariable(
            &mut caller.blocks[0],
            self.ptr_ty(call_result_type),
            return_variable,
        );

        // Fuse the first block of the callee into the block of the caller. This is okay because
        // it's illegal to branch to the first BB in a function.
        let mut callee_header = inlined_blocks.remove(0).instructions;
        // TODO: OpLine handling
        let num_variables = callee_header
            .iter()
            .position(|inst| inst.class.opcode != Op::Variable)
            .unwrap_or(callee_header.len());
        caller.blocks[block_idx]
            .instructions
            .append(&mut callee_header.split_off(num_variables));
        // Move the OpVariables of the callee to the caller.
        insert_opvariables(&mut caller.blocks[0], callee_header);

        // Add the load of the result value after the inlined function. Note there's guaranteed no
        // OpPhi instructions since we just split this block.
        post_call_block_insts.insert(
            0,
            Instruction::new(
                Op::Load,
                Some(call_result_type),
                Some(call_result_id),
                vec![Operand::IdRef(return_variable)],
            ),
        );
        // Insert the second half of the split block.
        let continue_block = Block {
            label: Some(Instruction::new(Op::Label, None, Some(return_jump), vec![])),
            instructions: post_call_block_insts,
        };
        caller.blocks.insert(block_idx + 1, continue_block);

        // Insert the rest of the blocks (i.e. not the first) between the original block that was
        // split.
        caller
            .blocks
            .splice((block_idx + 1)..(block_idx + 1), inlined_blocks);

        true
    }

    fn add_clone_id_rules(&mut self, rewrite_rules: &mut HashMap<Word, Word>, blocks: &[Block]) {
        for block in blocks {
            for inst in &block.instructions {
                if let Some(result_id) = inst.result_id {
                    let new_id = self.id();
                    let old = rewrite_rules.insert(result_id, new_id);
                    assert!(old.is_none());
                }
            }
        }
    }

    fn apply_rewrite_rules(rewrite_rules: &HashMap<Word, Word>, blocks: &mut [Block]) {
        let apply = |inst: &mut Instruction| {
            if let Some(ref mut id) = &mut inst.result_id {
                if let Some(&rewrite) = rewrite_rules.get(id) {
                    *id = rewrite;
                }
            }

            if let Some(ref mut id) = &mut inst.result_type {
                if let Some(&rewrite) = rewrite_rules.get(id) {
                    *id = rewrite;
                }
            }

            inst.operands.iter_mut().for_each(|op| {
                if let Some(id) = operand_idref_mut(op) {
                    if let Some(&rewrite) = rewrite_rules.get(id) {
                        *id = rewrite;
                    }
                }
            })
        };
        for block in blocks {
            for inst in &mut block.label {
                apply(inst);
            }
            for inst in &mut block.instructions {
                apply(inst);
            }
        }
    }
}

fn get_inlined_blocks(function: &Function, return_variable: Word, return_jump: Word) -> Vec<Block> {
    let mut blocks = function.blocks.clone();
    for block in &mut blocks {
        let last = block.instructions.last().unwrap();
        if let Op::Return | Op::ReturnValue = last.class.opcode {
            if Op::ReturnValue == last.class.opcode {
                let return_value = operand_idref(&last.operands[0]).unwrap();
                block.instructions.insert(
                    block.instructions.len() - 1,
                    Instruction::new(
                        Op::Store,
                        None,
                        None,
                        vec![
                            Operand::IdRef(return_variable),
                            Operand::IdRef(return_value),
                        ],
                    ),
                )
            }
            *block.instructions.last_mut().unwrap() =
                Instruction::new(Op::Branch, None, None, vec![Operand::IdRef(return_jump)]);
        }
    }
    blocks
}

fn insert_opvariable(block: &mut Block, ptr_ty: Word, result_id: Word) {
    let index = block
        .instructions
        .iter()
        .enumerate()
        .find_map(|(index, inst)| {
            if inst.class.opcode != Op::Variable {
                Some(index)
            } else {
                None
            }
        });
    let inst = Instruction::new(
        Op::Variable,
        Some(ptr_ty),
        Some(result_id),
        vec![Operand::StorageClass(StorageClass::Function)],
    );
    match index {
        Some(index) => block.instructions.insert(index, inst),
        None => block.instructions.push(inst),
    }
}

fn insert_opvariables(block: &mut Block, mut insts: Vec<Instruction>) {
    let index = block
        .instructions
        .iter()
        .enumerate()
        .find_map(|(index, inst)| {
            if inst.class.opcode != Op::Variable {
                Some(index)
            } else {
                None
            }
        });
    match index {
        Some(index) => {
            block.instructions.splice(index..index, insts);
        }
        None => block.instructions.append(&mut insts),
    }
}
