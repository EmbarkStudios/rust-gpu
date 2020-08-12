use crate::ctx::{Context, FnCtx};
use rspirv::spirv::{FunctionControl, Word};
use rustc_middle::mir::{
    Body, Operand, Rvalue, Statement, StatementKind, Terminator, TerminatorKind,
};
use rustc_middle::ty::{Ty, TyKind};

pub fn trans_fn<'ctx, 'tcx>(
    ctx: &'ctx mut Context<'tcx>,
    instance: rustc_middle::ty::Instance<'tcx>,
) {
    {
        let mut mir = ::std::io::Cursor::new(Vec::new());

        crate::rustc_mir::util::write_mir_pretty(ctx.tcx, Some(instance.def_id()), &mut mir)
            .unwrap();

        let s = String::from_utf8(mir.into_inner()).unwrap();

        println!("{}", s);
    }

    let mir = ctx.tcx.optimized_mir(instance.def_id());

    let mut fnctx = FnCtx::new(ctx, instance.substs);

    trans_fn_header(&mut fnctx, mir);

    for (bb, bb_data) in mir.basic_blocks().iter_enumerated() {
        let label_id = fnctx.get_basic_block(bb);
        fnctx.spirv().begin_block(Some(label_id)).unwrap();

        for stmt in &bb_data.statements {
            trans_stmt(&mut fnctx, stmt);
        }
        trans_terminator(&mut fnctx, bb_data.terminator());
    }
    fnctx.spirv().end_function().unwrap();
}

// return_type, fn_type
fn trans_fn_header<'tcx>(ctx: &mut FnCtx, body: &Body<'tcx>) {
    let mir_return_type = body.local_decls[0u32.into()].ty;
    let return_type = trans_type(ctx, mir_return_type);
    ctx.is_void = if let TyKind::Tuple(fields) = &mir_return_type.kind {
        fields.len() == 0
    } else {
        false
    };
    let params = (0..body.arg_count)
        .map(|i| trans_type(ctx, body.local_decls[(i + 1).into()].ty))
        .collect::<Vec<_>>();
    let mut params_nonzero = params.clone();
    if params_nonzero.is_empty() {
        // spirv says take 1 argument of type void if no arguments
        params_nonzero.push(ctx.spirv().type_void());
    }
    let function_type = ctx.spirv().type_function(return_type, params_nonzero);
    let function_id = None;
    let control = FunctionControl::NONE;
    // TODO: keep track of function IDs
    let _ = ctx
        .spirv()
        .begin_function(return_type, function_id, control, function_type)
        .unwrap();

    for (i, &param_type) in params.iter().enumerate() {
        let param_value = ctx.spirv().function_parameter(param_type).unwrap();
        ctx.locals.def((i + 1).into(), param_value);
    }
}

fn trans_type<'tcx>(ctx: &mut FnCtx, ty: Ty<'tcx>) -> Word {
    match ty.kind {
        TyKind::Bool => ctx.spirv().type_bool(),
        TyKind::Tuple(fields) if fields.len() == 0 => ctx.spirv().type_void(),
        TyKind::Int(ty) => {
            let size = ty.bit_width().expect("isize not supported yet");
            ctx.spirv().type_int(size as u32, 1)
        }
        TyKind::Uint(ty) => {
            let size = ty.bit_width().expect("isize not supported yet");
            ctx.spirv().type_int(size as u32, 0)
        }
        TyKind::Float(ty) => ctx.spirv().type_float(ty.bit_width() as u32),
        TyKind::RawPtr(type_and_mut) => {
            let pointee_type = trans_type(ctx, type_and_mut.ty);
            // note: use custom cache
            ctx.type_pointer(pointee_type)
        }
        ref thing => panic!("Unknown type: {:?}", thing),
    }
}

fn trans_stmt<'tcx>(ctx: &mut FnCtx, stmt: &Statement<'tcx>) {
    match &stmt.kind {
        StatementKind::Assign(place_and_rval) => {
            // can't destructure this since it's a Box<(place, rvalue)>
            let place = place_and_rval.0;
            let rvalue = &place_and_rval.1;

            if place.projection.len() != 0 {
                panic!(
                    "Place projections aren't supported yet (assignment): {:?}",
                    place.projection
                );
            }

            let expr = trans_rvalue(ctx, rvalue);
            ctx.locals.def(place.local, expr);
        }
        // ignore StorageLive/Dead for now
        StatementKind::StorageLive(_local) => (),
        StatementKind::StorageDead(_local) => (),
        thing => panic!("Unknown statement: {:?}", thing),
    }
}

fn trans_terminator<'tcx>(ctx: &mut FnCtx, term: &Terminator<'tcx>) {
    match term.kind {
        TerminatorKind::Return => {
            if ctx.is_void {
                ctx.spirv().ret().unwrap();
            } else {
                // local 0 is return value
                ctx.ctx
                    .spirv
                    .ret_value(ctx.locals.get(0u32.into()))
                    .unwrap();
            }
        }
        TerminatorKind::Assert { target, .. } => {
            // ignore asserts for now, just do direct goto
            let inst_id = ctx.get_basic_block(target);
            ctx.spirv().branch(inst_id).unwrap();
        }
        TerminatorKind::Goto { target } => {
            let inst_id = ctx.get_basic_block(target);
            ctx.spirv().branch(inst_id).unwrap();
        }
        ref thing => panic!("Unknown terminator: {:?}", thing),
    }
}

fn trans_rvalue<'tcx>(ctx: &mut FnCtx, expr: &Rvalue<'tcx>) -> Word {
    match expr {
        Rvalue::Use(operand) => trans_operand(ctx, operand),
        Rvalue::BinaryOp(_op, left, right) => {
            // TODO: properly implement
            let left = trans_operand(ctx, left);
            let right = trans_operand(ctx, right);
            let result_type = ctx.spirv().type_int(32, 0);
            ctx.spirv().i_add(result_type, None, left, right).unwrap()
        }
        thing => panic!("Unknown rvalue: {:?}", thing),
    }
}

fn trans_operand<'tcx>(ctx: &mut FnCtx, operand: &Operand<'tcx>) -> Word {
    match operand {
        Operand::Copy(place) | Operand::Move(place) => {
            if place.projection.len() != 0 {
                panic!(
                    "Place projections aren't supported yet (operand): {:?}",
                    place.projection
                );
            }
            ctx.locals.get(place.local)
        }
        Operand::Constant(constant) => panic!("Unimplemented Operand::Constant: {:?}", constant),
    }
}
