use crate::spirv_ctx::SpirvContext;
use rspirv::binary::Assemble;
use rustc_middle::mir::{Rvalue, Statement, StatementKind};
use rustc_middle::ty::TyCtxt;
use spirv_headers::FunctionControl;

pub struct Translator<'tcx> {
    spirv: SpirvContext,
    tcx: TyCtxt<'tcx>,
}

impl<'tcx> Translator<'tcx> {
    pub fn new(tcx: TyCtxt<'tcx>) -> Self {
        let spirv = SpirvContext::new();
        Self { spirv, tcx }
    }

    pub fn assemble(self) -> Vec<u32> {
        self.spirv.builder.module().assemble()
    }

    pub fn trans_fn(&mut self, instance: rustc_middle::ty::Instance<'tcx>) {
        {
            let mut mir = ::std::io::Cursor::new(Vec::new());

            crate::rustc_mir::util::write_mir_pretty(self.tcx, Some(instance.def_id()), &mut mir)
                .unwrap();

            let s = String::from_utf8(mir.into_inner()).unwrap();

            println!("{}", s);
        }

        let void = self.spirv.type_void();
        let void_function = self.spirv.type_function(void, vec![void]);
        let function_id = None;
        let control = FunctionControl::NONE;
        let _ = self
            .spirv
            .builder
            .begin_function(void, function_id, control, void_function)
            .unwrap();
        let mir = self.tcx.instance_mir(instance.def);
        for (_bb, bb_data) in mir.basic_blocks().iter_enumerated() {
            self.spirv.builder.begin_block(None).unwrap();
            for stmt in &bb_data.statements {
                self.trans_stmt(stmt);
            }
        }
        self.spirv.builder.end_function().unwrap();
    }

    fn trans_stmt(&self, stmt: &Statement) {
        match &stmt.kind {
            StatementKind::Assign(place_and_rval) => {
                // can't destructure this since it's a Box<(place, rvalue)>
                let place = place_and_rval.0;
                let rvalue = &place_and_rval.1;

                dbg!(&place);
                for elem in place.projection {
                    match elem {
                        thing => println!("Unknown projection: {:?}", thing),
                    }
                }

                dbg!(&rvalue);
                match &rvalue {
                    Rvalue::Use(operand) => {
                        //let val = trans_operand(fx, operand);
                        //lval.write_cvalue(fx, val);
                        dbg!(operand);
                    }
                    thing => println!("Unknown place: {:?}", thing),
                }
                //let lval = trans_place(fx, to_place_and_rval.0);
                //dbg!(lval);
                //println!("Assign");
            }
            thing => println!("Unknown statement: {:?}", thing),
        }
    }

    fn trans_expr(&self, expr: Rvalue<'tcx>) {
        match &expr {
            Rvalue::Use(operand) => {
                //let val = trans_operand(fx, operand);
                //lval.write_cvalue(fx, val);
                dbg!(operand);
            }
            thing => println!("Unknown place: {:?}", thing),
        }
    }
}
