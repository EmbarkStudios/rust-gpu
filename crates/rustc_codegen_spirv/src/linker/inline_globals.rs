use rspirv::binary::Disassemble;
use rspirv::dr::{Instruction, Module, Operand};
use rspirv::spirv::{Op, StorageClass};
use rustc_data_structures::fx::{FxHashMap, FxHashSet};
use rustc_session::Session;

// bool is if this needs stored
#[derive(Debug, Clone, PartialEq)]
struct NormalizedInstructions {
    vars: Vec<Instruction>,
    insts: Vec<Instruction>,
    root: u32,
}

impl NormalizedInstructions {
    fn new(id: u32) -> Self {
        NormalizedInstructions {
            vars: Vec::new(),
            insts: Vec::new(),
            root: id,
        }
    }

    fn extend(&mut self, o: NormalizedInstructions) {
        self.vars.extend(o.vars);
        self.insts.extend(o.insts);
    }

    fn is_empty(&self) -> bool {
        self.insts.is_empty() && self.vars.is_empty()
    }

    fn fix_ids(&mut self, bound: &mut u32, new_root: u32) {
        let mut id_map: FxHashMap<u32, u32> = FxHashMap::default();
        id_map.insert(self.root, new_root);
        for inst in &mut self.vars {
            Self::fix_instruction(self.root, inst, &mut id_map, bound, new_root);
        }
        for inst in &mut self.insts {
            Self::fix_instruction(self.root, inst, &mut id_map, bound, new_root);
        }
    }

    fn fix_instruction(
        root: u32,
        inst: &mut Instruction,
        id_map: &mut FxHashMap<u32, u32>,
        bound: &mut u32,
        new_root: u32,
    ) {
            for op in &mut inst.operands {
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
        if let Some(id) = &mut inst.result_id {
            if *id != root {
                id_map.insert(*id, *bound);
                *id = *bound;
                *bound += 1;
            } else {
                *id = new_root;
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
enum FunctionArg {
    Invalid,
    Insts(NormalizedInstructions),
}

pub fn inline_global_varaibles(sess: &Session, module: &mut Module) -> super::Result<()> {
    let mut i = 0;
    let mut cont = true;
    let mut has_run = false;
    //std::fs::write("res0.txt", module.disassemble());
    while cont {
        cont = inline_global_varaibles_rec(sess, module)?;
        has_run = has_run || cont;
        i += 1;
        //std::fs::write(format!("res{}.txt", i), module.disassemble());
    }
    // needed because inline global create duplicate types...
    if has_run {
        let _timer = sess.timer("link_remove_duplicate_types_round_2");
        super::duplicates::remove_duplicate_types(&mut module);
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
    let mut bound = module.header.as_ref().unwrap().bound;
    for caller in &module.functions {
        let mut insts: FxHashMap<u32, Instruction> = FxHashMap::default();
        // for variables that only stored once and it's stored as a ref
        let mut ref_stores: FxHashMap<u32, Option<u32>> = FxHashMap::default();
        for block in &caller.blocks {
            for inst in &block.instructions {
                if inst.result_id.is_some() {
                    insts.insert(inst.result_id.unwrap(), inst.clone());
                }
                if inst.class.opcode == Op::Store {
                    if let Operand::IdRef(to) = inst.operands[0] {
                        if let Operand::IdRef(from) = inst.operands[1] {
                            match ref_stores.get(&to) {
                                None => {
                                    ref_stores.insert(to, Some(from));
                                }
                                Some(_) => {
                                    ref_stores.insert(to, None);
                                }
                            }
                        }
                    }
                } else if inst.class.opcode == Op::FunctionCall {
                    let function_id = match &inst.operands[0] {
                        &Operand::IdRef(w) => w,
                        _ => panic!(),
                    };
                    for i in 1..inst.operands.len() {
                        let key = (function_id, i as u32 - 1);
                        match &inst.operands[i] {
                            &Operand::IdRef(w) => match &function_args.get(&key) {
                                None => {
                                    match get_const_arg_insts(bound, &variables, &insts, &ref_stores, w) {
                                        Some(insts) => {
                                            function_args.insert(key, FunctionArg::Insts(insts));
                                        }
                                        None => {
                                            function_args.insert(key, FunctionArg::Invalid);
                                        }
                                    }
                                }
                                Some(FunctionArg::Insts(w2)) => {
                                    let new_insts =
                                        get_const_arg_insts(bound, &variables, &insts, &ref_stores, w);
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
    // retain ones can rewrite
    function_args.retain(|_, k| match k {
        FunctionArg::Invalid => false,
        FunctionArg::Insts(v) => !v.is_empty(),
    });
    if function_args.is_empty() {
        return Ok(false);
    }
    // start rewrite
    for function in &mut module.functions {
        let def = function.def.as_mut().unwrap();
        let fid = def.result_id.unwrap();
        let mut insts = NormalizedInstructions::new(0);
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
                    arg.fix_ids(&mut bound, parameter.result_id.unwrap());
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
                }
                function_type.result_id = Some(tid);
                def.operands[1] = Operand::IdRef(tid);
                module.types_global_values.push(function_type);
            }
        }
        // callee side. insert initialization instructions, which reuse the ids of the removed parameters
        if !function.blocks.is_empty() {
            let first_block = &mut function.blocks[0];
            first_block.instructions.splice(0..0, insts.vars);
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
            first_block.instructions.splice(i..i, insts.insts);
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

fn get_const_arg_operands(
    variables: &FxHashSet<u32>,
    insts: &FxHashMap<u32, Instruction>,
    ref_stores: &FxHashMap<u32, Option<u32>>,
    operand: &Operand,
) -> Option<NormalizedInstructions> {
    match operand {
        Operand::IdRef(id) => {
            let insts = get_const_arg_insts_rec(variables, insts, ref_stores, *id)?;
            return Some(insts);
        }
        Operand::LiteralInt32(_) => {}
        Operand::LiteralInt64(_) => {}
        Operand::LiteralFloat32(_) => {}
        Operand::LiteralFloat64(_) => {}
        Operand::LiteralExtInstInteger(_) => {}
        Operand::LiteralSpecConstantOpInteger(_) => {}
        Operand::LiteralString(_) => {}
        _ => {
            // TOOD add more cases
            return None;
        }
    }
    return Some(NormalizedInstructions::new(0));
}

fn get_const_arg_insts(
    mut bound: u32,
    variables: &FxHashSet<u32>,
    insts: &FxHashMap<u32, Instruction>,
    ref_stores: &FxHashMap<u32, Option<u32>>,
    id: u32,
) -> Option<NormalizedInstructions> {
    let mut res = get_const_arg_insts_rec(variables, insts, ref_stores, id)?;
    res.insts.reverse();
    // the bound passed in is always the same
    // we need to normalize the ids, so they are the same when compared
    let fake_root = bound;
    bound += 1;
    res.fix_ids(&mut bound, fake_root);
    res.root = fake_root;
    Some(res)
}

fn get_const_arg_insts_rec(
    variables: &FxHashSet<u32>,
    insts: &FxHashMap<u32, Instruction>,
    ref_stores: &FxHashMap<u32, Option<u32>>,
    id: u32,
) -> Option<NormalizedInstructions> {
    let mut result = NormalizedInstructions::new(id);
    if variables.contains(&id) {
        return Some(result);
    }
    let par: &Instruction = insts.get(&id)?;
    if par.class.opcode == Op::AccessChain {
        result.insts.push(par.clone());
        for oprand in &par.operands {
            let insts = get_const_arg_operands(variables, insts, ref_stores, oprand)?;
            result.extend(insts);
        }
    } else if par.class.opcode == Op::FunctionCall {
        result.insts.push(par.clone());
        // skip first, first is function id
        for oprand in &par.operands[1..] {
            let insts = get_const_arg_operands(variables, insts, ref_stores, oprand)?;
            result.extend(insts);
        }
    } else if par.class.opcode == Op::Variable {
        result.vars.push(par.clone());
        let stored = ref_stores.get(&id)?;
        let stored = (*stored)?;
        result.insts.push(Instruction::new(
            Op::Store,
            None,
            None,
            vec![Operand::IdRef(id), Operand::IdRef(stored)],
        ));
        let new_insts = get_const_arg_insts_rec(variables, insts, ref_stores, stored)?;
        result.extend(new_insts);
    } else if par.class.opcode == Op::ArrayLength {
        result.insts.push(par.clone());
        let insts = get_const_arg_operands(variables, insts, ref_stores, &par.operands[0])?;
        result.extend(insts);
    } else {
        // TOOD add more cases
        return None;
    }
    Some(result)
}
