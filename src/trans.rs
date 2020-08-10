use crate::spirv_ctx::SpirvContext;
use rspirv::binary::Assemble;
use rustc_middle::mir::{
    BasicBlock, Body, Local, Operand, Rvalue, Statement, StatementKind, Terminator, TerminatorKind,
};
use rustc_middle::ty::{Ty, TyCtxt, TyKind};
use spirv_headers::{FunctionControl, Word};
use std::collections::HashMap;

pub struct Translator<'tcx> {
    spirv: SpirvContext,
    tcx: TyCtxt<'tcx>,
    basic_blocks: HashMap<BasicBlock, Word>,
    locals: HashMap<Local, Word>,
}

impl<'tcx> Translator<'tcx> {
    pub fn new(tcx: TyCtxt<'tcx>) -> Self {
        let spirv = SpirvContext::new();
        Self {
            spirv,
            tcx,
            basic_blocks: HashMap::new(),
            locals: HashMap::new(),
        }
    }

    pub fn assemble(self) -> Vec<u32> {
        self.spirv.builder.module().assemble()
    }

    pub fn get_bb_label(&self, bb: BasicBlock) -> Option<Word> {
        self.basic_blocks.get(&bb).cloned()
    }

    pub fn get_or_gen_bb_label(&mut self, bb: BasicBlock) -> Word {
        let builder = &mut self.spirv.builder;
        *self.basic_blocks.entry(bb).or_insert_with(|| builder.id())
    }

    pub fn def_local(&mut self, local: Local, expr: Word) {
        match self.locals.entry(local) {
            std::collections::hash_map::Entry::Occupied(_) => {
                println!("Non-SSA code not supported yet")
            }
            std::collections::hash_map::Entry::Vacant(entry) => {
                entry.insert(expr);
            }
        }
    }

    pub fn trans_fn(&mut self, instance: rustc_middle::ty::Instance<'tcx>) {
        {
            let mut mir = ::std::io::Cursor::new(Vec::new());

            crate::rustc_mir::util::write_mir_pretty(self.tcx, Some(instance.def_id()), &mut mir)
                .unwrap();

            let s = String::from_utf8(mir.into_inner()).unwrap();

            println!("{}", s);
        }

        let mir = self.tcx.instance_mir(instance.def);

        self.trans_fn_header(mir);

        for (bb, bb_data) in mir.basic_blocks().iter_enumerated() {
            let label_id = self.get_bb_label(bb);
            let result = self.spirv.builder.begin_block(label_id).unwrap();
            if label_id.is_none() {
                self.basic_blocks.insert(bb, result);
            }

            for stmt in &bb_data.statements {
                self.trans_stmt(stmt);
            }
            self.trans_terminator(bb_data.terminator());
        }
        self.spirv.builder.end_function().unwrap();

        self.basic_blocks.clear();
        self.locals.clear();
    }

    // return_type, fn_type
    fn trans_fn_header(&mut self, body: &Body<'tcx>) {
        let return_type = self.trans_type(body.local_decls[0u32.into()].ty);
        let params = (0..body.arg_count)
            .map(|i| self.trans_type(body.local_decls[(i + 1).into()].ty))
            .collect::<Vec<_>>();
        // TODO: this clone is gross
        let function_type = self.spirv.type_function(return_type, params.clone());
        let function_id = None;
        let control = FunctionControl::NONE;
        // TODO: keep track of function IDs
        let _ = self
            .spirv
            .builder
            .begin_function(return_type, function_id, control, function_type)
            .unwrap();

        for (i, &param_type) in params.iter().enumerate() {
            let param_value = self.spirv.builder.function_parameter(param_type).unwrap();
            self.def_local((i + 1).into(), param_value);
        }
    }

    fn trans_type(&mut self, ty: Ty<'tcx>) -> Word {
        match ty.kind {
            TyKind::Bool => self.spirv.type_bool(),
            TyKind::Tuple(fields) if fields.len() == 0 => self.spirv.type_void(),
            TyKind::Int(ty) => {
                let size = ty.bit_width().expect("isize not supported yet");
                self.spirv.type_int(size as u32, 1)
            }
            TyKind::Uint(ty) => {
                let size = ty.bit_width().expect("isize not supported yet");
                self.spirv.type_int(size as u32, 0)
            }
            TyKind::Float(ty) => self.spirv.type_float(ty.bit_width() as u32),
            ref thing => {
                println!("Unknown type: {:?}", thing);
                self.spirv.builder.id()
            }
        }
    }

    fn trans_stmt(&mut self, stmt: &Statement<'tcx>) {
        match &stmt.kind {
            StatementKind::Assign(place_and_rval) => {
                // can't destructure this since it's a Box<(place, rvalue)>
                let place = place_and_rval.0;
                let rvalue = &place_and_rval.1;

                if place.projection.len() != 0 {
                    println!(
                        "Place projections aren't supported yet (assignment): {:?}",
                        place.projection
                    );
                }

                let expr = self.trans_rvalue(rvalue);
                self.def_local(place.local, expr);
            }
            // ignore StorageLive/Dead for now
            StatementKind::StorageLive(_local) => (),
            StatementKind::StorageDead(_local) => (),
            thing => println!("Unknown statement: {:?}", thing),
        }
    }

    fn trans_terminator(&mut self, term: &Terminator<'tcx>) {
        match term.kind {
            TerminatorKind::Return => self.spirv.builder.ret().unwrap(),
            TerminatorKind::Assert { target, .. } => {
                // ignore asserts for now, just do direct goto
                let inst_id = self.get_or_gen_bb_label(target);
                self.spirv.builder.branch(inst_id).unwrap();
            }
            TerminatorKind::Goto { target } => {
                let inst_id = self.get_or_gen_bb_label(target);
                self.spirv.builder.branch(inst_id).unwrap();
            }
            ref thing => println!("Unknown terminator: {:?}", thing),
        }
    }

    fn trans_rvalue(&mut self, expr: &Rvalue<'tcx>) -> Word {
        match expr {
            Rvalue::Use(operand) => self.trans_operand(operand),
            Rvalue::BinaryOp(_op, left, right) => {
                // TODO: properly implement
                let left = self.trans_operand(left);
                let right = self.trans_operand(right);
                let result_type = self.spirv.type_int(32, 1);
                self.spirv
                    .builder
                    .i_add(result_type, None, left, right)
                    .unwrap()
            }
            thing => {
                println!("Unknown rvalue: {:?}", thing);
                self.spirv.builder.id()
            }
        }
    }

    fn trans_operand(&mut self, operand: &Operand<'tcx>) -> Word {
        match operand {
            Operand::Copy(place) | Operand::Move(place) => {
                if place.projection.len() != 0 {
                    println!(
                        "Place projections aren't supported yet (operand): {:?}",
                        place.projection
                    );
                }
                // This probably needs to be fixed, forward-references might be a thing
                *self.locals.get(&place.local).expect("Undefined local")
            }
            Operand::Constant(constant) => {
                println!("Unimplemented Operand::Constant: {:?}", constant);
                self.spirv.builder.id()
            }
        }
    }
}
