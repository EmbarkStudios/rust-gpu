use crate::ctx::{Context, FnCtx};
use rspirv::spirv::{FunctionControl, Word};
use rustc_middle::mir::{Operand, Rvalue, Statement, StatementKind, Terminator, TerminatorKind};
use rustc_middle::ty::{Instance, ParamEnv, Ty, TyKind};

pub fn trans_fn<'ctx, 'tcx>(ctx: &'ctx mut Context<'tcx>, instance: Instance<'tcx>) {
    {
        let mut mir = ::std::io::Cursor::new(Vec::new());

        crate::rustc_mir::util::write_mir_pretty(ctx.tcx, Some(instance.def_id()), &mut mir)
            .unwrap();

        let s = String::from_utf8(mir.into_inner()).unwrap();

        println!("{}", s);
    }

    let body = ctx.tcx.optimized_mir(instance.def_id());

    let mut fnctx = FnCtx::new(ctx, instance, body);

    trans_fn_header(&mut fnctx);

    for (bb, bb_data) in fnctx.body.basic_blocks().iter_enumerated() {
        let label_id = fnctx.get_basic_block(bb);
        fnctx.spirv().begin_block(Some(label_id)).unwrap();

        for stmt in &bb_data.statements {
            trans_stmt(&mut fnctx, stmt);
        }
        trans_terminator(&mut fnctx, bb_data.terminator());
    }
    fnctx.spirv().end_function().unwrap();
}

fn trans_fn_header<'tcx>(ctx: &mut FnCtx<'_, 'tcx>) {
    let mir_return_type = ctx.body.local_decls[0u32.into()].ty;
    let return_type = trans_type(ctx, mir_return_type);
    ctx.is_void = if let TyKind::Tuple(fields) = &mir_return_type.kind {
        fields.len() == 0
    } else {
        false
    };
    let params = (0..ctx.body.arg_count)
        .map(|i| trans_type(ctx, ctx.body.local_decls[(i + 1).into()].ty))
        .collect::<Vec<_>>();
    let mut params_nonzero = params.clone();
    if params_nonzero.is_empty() {
        // spirv says take 1 argument of type void if no arguments
        params_nonzero.push(ctx.spirv().type_void());
    }
    let function_type = ctx.spirv().type_function(return_type, params_nonzero);
    let function_id = ctx
        .ctx
        .get_func_def(ctx.instance.def_id(), ctx.instance.substs);
    let control = FunctionControl::NONE;
    let _ = ctx
        .spirv()
        .begin_function(return_type, Some(function_id), control, function_type)
        .unwrap();

    for (i, &param_type) in params.iter().enumerate() {
        let param_value = ctx.spirv().function_parameter(param_type).unwrap();
        ctx.locals.def((i + 1).into(), param_value);
    }
}

fn trans_type<'tcx>(ctx: &mut FnCtx<'_, 'tcx>, ty: Ty<'tcx>) -> Word {
    let mono = ctx.monomorphize(&ty);
    match mono.kind {
        TyKind::Param(param) => panic!("TyKind::Param after monomorphize: {:?}", param),
        TyKind::Bool => ctx.spirv().type_bool(),
        TyKind::Tuple(fields) if fields.len() == 0 => ctx.spirv().type_void(),
        TyKind::Int(ty) => {
            let size = ty.bit_width().unwrap_or_else(|| ctx.ctx.pointer_size.val());
            ctx.spirv().type_int(size as u32, 1)
        }
        TyKind::Uint(ty) => {
            let size = ty.bit_width().unwrap_or_else(|| ctx.ctx.pointer_size.val());
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

fn trans_terminator<'tcx>(ctx: &mut FnCtx<'_, 'tcx>, term: &Terminator<'tcx>) {
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
        TerminatorKind::Call {
            ref func,
            ref args,
            ref destination,
            ..
        } => {
            let destination = destination.expect("Divergent function calls not supported yet");
            let fn_ty = ctx.monomorphize(&func.ty(ctx.body, ctx.ctx.tcx));
            let fn_sig = ctx.ctx.tcx.normalize_erasing_late_bound_regions(
                ParamEnv::reveal_all(),
                &fn_ty.fn_sig(ctx.ctx.tcx),
            );
            let fn_return_type = ParamEnv::reveal_all().and(fn_sig.output()).value;
            let result_type = trans_type(ctx, fn_return_type);
            let function = match func {
                // TODO: Can constant.literal.val not be a ZST?
                Operand::Constant(constant) => match constant.literal.ty.kind {
                    TyKind::FnDef(id, substs) => ctx.ctx.get_func_def(id, substs),
                    ref thing => panic!("Unknown type in fn call: {:?}", thing),
                },
                thing => panic!("Dynamic calls not supported yet: {:?}", thing),
            };
            let arguments = args
                .iter()
                .map(|arg| trans_operand(ctx, arg))
                .collect::<Vec<_>>();
            let dest_local = destination.0.local;
            if destination.0.projection.len() != 0 {
                panic!(
                    "Place projections aren't supported yet (fn call): {:?}",
                    destination.0.projection
                );
            }
            let result = ctx
                .spirv()
                .function_call(result_type, None, function, arguments)
                .unwrap();
            ctx.locals.def(dest_local, result);
            let destination_bb = ctx.get_basic_block(destination.1);
            ctx.spirv().branch(destination_bb).unwrap();
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
