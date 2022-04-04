use rspirv::binary::Disassemble;
use rspirv::dr::{Instruction, Module, Operand};
use rspirv::spirv::Op;
use rustc_data_structures::fx::{FxHashMap, FxHashSet};
use rustc_session::Session;

#[derive(Debug, Clone, PartialEq)]
enum FunctionArg {
    Invalid,
    Insts(Vec<Instruction>),
}

pub fn inline_global_varaibles(sess: &Session, module: &mut Module) -> super::Result<()> {
    let mut i = 0;
    let mut cont = true;
    std::fs::write("res0.txt", module.disassemble());
    while cont {
        cont = inline_global_varaibles_rec(sess, module)?;
        i += 1;
        std::fs::write(format!("res{}.txt", i), module.disassemble());
    }
    Ok(())
}

fn inline_global_varaibles_rec(sess: &Session, module: &mut Module) -> super::Result<bool> {
    // first collect global stuff
    let mut variables: FxHashSet<u32> = FxHashSet::default();
    let mut function_types: FxHashMap<u32, Instruction> = FxHashMap::default();
    for global_inst in &module.types_global_values {
        let opcode = global_inst.class.opcode;
        if opcode == Op::Variable || opcode == Op::Constant {
            variables.insert(global_inst.result_id.unwrap());
        } else if opcode == Op::TypeFunction {
            function_types.insert(global_inst.result_id.unwrap(), global_inst.clone());
        }
    }
    // then we keep track of which function parameter are always called with the same expression that only uses global variables
    let mut function_args: FxHashMap<(u32, u32), FunctionArg> = FxHashMap::default();
    for caller in &module.functions {
        let mut insts: FxHashMap<u32, Instruction> = FxHashMap::default();
        for block in &caller.blocks {
            for inst in &block.instructions {
                if inst.result_id.is_some() {
                    insts.insert(inst.result_id.unwrap(), inst.clone());
                }
                if inst.class.opcode == Op::FunctionCall {
                    let function_id = match &inst.operands[0] {
                        &Operand::IdRef(w) => w,
                        _ => panic!(),
                    };
                    for i in 1..inst.operands.len() {
                        let key = (function_id, i as u32 - 1);
                        match &inst.operands[i] {
                            &Operand::IdRef(w) => match &function_args.get(&key) {
                                None => match get_const_arg_insts(&variables, &insts, w) {
                                    Some(insts) => {
                                        function_args.insert(key, FunctionArg::Insts(insts));
                                    }
                                    None => {
                                        function_args.insert(key, FunctionArg::Invalid);
                                    }
                                },
                                Some(FunctionArg::Insts(w2)) => {
                                    let new_insts = get_const_arg_insts(&variables, &insts, w);
                                    match new_insts {
                                        Some(new_insts) => {
                                            if new_insts != *w2 {
                                                function_args.insert(key, FunctionArg::Invalid);
                                            }
                                        }
                                        None => {
                                            function_args.insert(key, FunctionArg::Invalid);
                                        }
                                    }
                                }
                                _ => {
                                    function_args.insert(key, FunctionArg::Invalid);
                                }
                            },
                            _ => {
                                function_args.insert(key, FunctionArg::Invalid);
                            }
                        };
                    }
                }
            }
        }
    }
    function_args.retain(|_, k| match k {
        FunctionArg::Invalid => false,
        FunctionArg::Insts(v) => !v.is_empty(),
    });
    if function_args.is_empty() {
        return Ok(false);
    }
    let mut bound = module.header.as_ref().unwrap().bound;
    for function in &mut module.functions {
        let def = function.def.as_mut().unwrap();
        let fid = def.result_id.unwrap();
        let mut insts: Vec<Instruction> = Vec::new();
        let mut j: u32 = 0;
        let mut i = 0;
        let mut removed_indexes: Vec<u32> = Vec::new();
        // callee side. remove parameters from function def
        while i < function.parameters.len() {
            let mut removed = false;
            match &function_args.get(&(fid, j)) {
                Some(FunctionArg::Insts(arg)) => {
                    let parameter = function.parameters.remove(i);
                    let mut arg = arg.clone();
                    arg.reverse();
                    insts_replacing_captured_ids(&mut arg, &mut bound);
                    let index = arg.len() - 1;
                    arg[index].result_id = parameter.result_id;
                    insts.extend(arg);
                    removed_indexes.push(j);
                    removed = true;
                }
                _ => (),
            }
            if !removed {
                i += 1;
            }
            j += 1;
        }
        // callee side. and add a new function type in global section
        if removed_indexes.len() > 0 {
            if let Operand::IdRef(tid) = def.operands[1] {
                let mut function_type: Instruction = function_types.get(&tid).unwrap().clone();
                let tid: u32 = bound;
                bound += 1;
                for i in removed_indexes.iter().rev() {
                    let i = *i as usize + 1;
                    function_type.operands.remove(i);
                    function_type.result_id = Some(tid);
                }
                def.operands[1] = Operand::IdRef(tid);
                module.types_global_values.push(function_type);
            }
        }
        // callee side. insert initialization instructions, which reuse the ids of the removed parameters
        if !function.blocks.is_empty() {
            let first_block = &mut function.blocks[0];
            // skip some instructions that must be at top of block
            let mut i = 0;
            loop {
                if i >= first_block.instructions.len() {
                    break;
                }
                let inst = &first_block.instructions[i];
                if inst.class.opcode == Op::Label || inst.class.opcode == Op::Variable {
                } else {
                    break;
                }
                i += 1;
            }
            first_block.instructions.splice(i..i, insts);
        }
        // caller side, remove parameters from function call
        for block in &mut function.blocks {
            for inst in &mut block.instructions {
                if inst.class.opcode == Op::FunctionCall {
                    let function_id = match &inst.operands[0] {
                        &Operand::IdRef(w) => w,
                        _ => panic!(),
                    };
                    let mut removed_size = 0;
                    for i in 0..inst.operands.len() - 1 {
                        if function_args.contains_key(&(function_id, i as u32)) {
                            inst.operands.remove(i - removed_size + 1);
                            removed_size += 1;
                        }
                    }
                }
            }
        }
    }
    if let Some(header) = &mut module.header {
        header.bound = bound;
    }
    Ok(true)
}

fn insts_replacing_captured_ids(arg: &mut Vec<Instruction>, bound: &mut u32) {
    let mut id_map: FxHashMap<u32, u32> = FxHashMap::default();
    for ins in arg {
        if let Some(id) = &mut ins.result_id {
            for op in &mut ins.operands {
                match op {
                    Operand::IdRef(id) => match id_map.get(id) {
                        Some(new_id) => {
                            *id = *new_id;
                        }
                        _ => {}
                    },
                    _ => {}
                }
            }
            id_map.insert(*id, *bound);
            *id = *bound;
            *bound += 1;
        }
    }
}

fn get_const_arg_operands(
    variables: &FxHashSet<u32>,
    insts: &FxHashMap<u32, Instruction>,
    operand: &Operand,
) -> Option<Vec<Instruction>> {
    match operand {
        Operand::IdRef(id) => {
            let insts = get_const_arg_insts(variables, insts, *id)?;
            return Some(insts);
        }
        Operand::LiteralInt32(_) => {},
        Operand::LiteralInt64(_) => {},
        Operand::LiteralFloat32(_) => {},
        Operand::LiteralFloat64(_) => {},
        Operand::LiteralExtInstInteger(_) => {},
        Operand::LiteralSpecConstantOpInteger(_) => {},
        Operand::LiteralString(_) => {},
        _ => {
            // TOOD add more cases
            return None;
        }
    }
    return Some(Vec::new());
}

fn get_const_arg_insts(
    variables: &FxHashSet<u32>,
    insts: &FxHashMap<u32, Instruction>,
    id: u32,
) -> Option<Vec<Instruction>> {
    let mut result: Vec<Instruction> = Vec::new();
    if variables.contains(&id) {
        return Some(result);
    }
    let par: &Instruction = insts.get(&id)?;
    if par.class.opcode == Op::AccessChain {
        result.push(par.clone());
        for oprand in &par.operands {
            let insts = get_const_arg_operands(variables, insts, oprand)?;
            result.extend(insts);
        }
    } else if par.class.opcode == Op::FunctionCall {
        result.push(par.clone());
        // skip first, first is function id
        for oprand in &par.operands[1..] {
            let insts = get_const_arg_operands(variables, insts, oprand)?;
            result.extend(insts);
        }
    } else {
        // TOOD add more cases
        return None;
    }
    Some(result)
}
