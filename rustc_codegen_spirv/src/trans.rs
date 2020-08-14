use crate::ctx::{local_tracker::SpirvLocal, type_tracker::SpirvType, Context, FnCtx};
use rspirv::spirv::{FunctionControl, StorageClass, Word};
use rustc_middle::mir::{
    BinOp, Operand, Place, ProjectionElem, Rvalue, Statement, StatementKind, Terminator,
    TerminatorKind,
};
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

    let mut parameters_spirv = Some(trans_fn_header(&mut fnctx));

    for (bb, bb_data) in fnctx.body.basic_blocks().iter_enumerated() {
        let label_id = fnctx.get_basic_block(bb);
        fnctx.spirv().begin_block(Some(label_id)).unwrap();

        // The first bb is the entry bb.
        if let Some(parameters_spirv) = parameters_spirv.take() {
            trans_locals(&mut fnctx, parameters_spirv);
        }

        for stmt in &bb_data.statements {
            trans_stmt(&mut fnctx, stmt);
        }
        trans_terminator(&mut fnctx, bb_data.terminator());
    }
    fnctx.spirv().end_function().unwrap();
}

// returns list of spirv parameter values
fn trans_fn_header<'tcx>(ctx: &mut FnCtx<'_, 'tcx>) -> Vec<Word> {
    let mir_return_type = ctx.body.local_decls[0u32.into()].ty;
    let return_type = trans_type(ctx, mir_return_type).def();
    ctx.is_void = if let TyKind::Tuple(fields) = &mir_return_type.kind {
        fields.len() == 0
    } else {
        false
    };
    let params = (0..ctx.body.arg_count)
        .map(|i| trans_type(ctx, ctx.body.local_decls[(i + 1).into()].ty).def())
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

    // TODO: ZSTs
    let parameters_spirv = params
        .iter()
        .map(|&p| ctx.spirv().function_parameter(p).unwrap())
        .collect();
    parameters_spirv
}

pub fn trans_locals<'tcx>(ctx: &mut FnCtx<'_, 'tcx>, parameters_spirv: Vec<Word>) {
    for (local, decl) in ctx.body.local_decls.iter_enumerated() {
        // TODO: ZSTs
        let local_type = trans_type(ctx, decl.ty);
        let local_ptr_type_def =
            ctx.spirv()
                .type_pointer(None, StorageClass::Function, local_type.def());
        let local_ptr_type = SpirvType::Pointer {
            def: local_ptr_type_def,
            pointee: Box::new(local_type),
        };
        let initializer = if local != 0u32.into() && local < (ctx.body.arg_count + 1).into() {
            Some(parameters_spirv[local.index() - 1])
        } else {
            None
        };
        let variable_decl = ctx.spirv().variable(
            local_ptr_type_def,
            None,
            StorageClass::Function,
            initializer,
        );
        ctx.locals
            .def(local, SpirvLocal::new(local_ptr_type, variable_decl));
    }
}

fn trans_type<'tcx>(ctx: &mut FnCtx<'_, 'tcx>, ty: Ty<'tcx>) -> SpirvType {
    let mono = ctx.monomorphize(&ty);
    // TODO: Cache the result SpirvType?
    match mono.kind {
        TyKind::Param(param) => panic!("TyKind::Param after monomorphize: {:?}", param),
        TyKind::Bool => SpirvType::Primitive(ctx.spirv().type_bool()),
        TyKind::Tuple(fields) if fields.len() == 0 => SpirvType::Primitive(ctx.spirv().type_void()),
        TyKind::Int(ty) => {
            let size = ty.bit_width().unwrap_or_else(|| ctx.ctx.pointer_size.val());
            SpirvType::Primitive(ctx.spirv().type_int(size as u32, 1))
        }
        TyKind::Uint(ty) => {
            let size = ty.bit_width().unwrap_or_else(|| ctx.ctx.pointer_size.val());
            SpirvType::Primitive(ctx.spirv().type_int(size as u32, 0))
        }
        TyKind::Float(ty) => SpirvType::Primitive(ctx.spirv().type_float(ty.bit_width() as u32)),
        TyKind::RawPtr(type_and_mut) => {
            let pointee_type = trans_type(ctx, type_and_mut.ty);
            // note: use custom cache
            SpirvType::Pointer {
                def: ctx.type_pointer(pointee_type.def()),
                pointee: Box::new(pointee_type),
            }
        }
        TyKind::Ref(_region, pointee_ty, _mutability) => {
            let pointee_type = trans_type(ctx, pointee_ty);
            // note: use custom cache
            SpirvType::Pointer {
                def: ctx.type_pointer(pointee_type.def()),
                pointee: Box::new(pointee_type),
            }
        }
        TyKind::Adt(adt, substs) => {
            if adt.variants.len() != 1 {
                panic!("Enums/unions aren't supported yet: {:?}", adt);
            }
            let variant = &adt.variants[0u32.into()];
            let field_types = variant
                .fields
                .iter()
                .map(|field| {
                    let ty = field.ty(ctx.ctx.tcx, substs);
                    trans_type(ctx, ty)
                })
                .collect::<Vec<_>>();
            let spirv_field_types = field_types.iter().map(|f| f.def());
            let def = ctx
                .ctx
                .spirv
                .type_struct(spirv_field_types.collect::<Vec<_>>());
            SpirvType::Adt { def, field_types }
        }
        ref thing => panic!("Unknown type: {:?}", thing),
    }
}

fn trans_stmt<'tcx>(ctx: &mut FnCtx<'_, 'tcx>, stmt: &Statement<'tcx>) {
    match &stmt.kind {
        StatementKind::Assign(place_and_rval) => {
            // can't destructure this since it's a Box<(place, rvalue)>
            let place = place_and_rval.0;
            let rvalue = &place_and_rval.1;

            let expr = trans_rvalue(ctx, rvalue);
            trans_place_store(ctx, &place, expr);
        }
        // Ignore StorageLive/Dead for now - might be useful for SSA conversion later.
        StatementKind::StorageLive(_) | StatementKind::StorageDead(_) => (),
        thing => panic!("Unknown statement: {:?}", thing),
    }
}

fn trans_terminator<'tcx>(ctx: &mut FnCtx<'_, 'tcx>, term: &Terminator<'tcx>) {
    match term.kind {
        TerminatorKind::Return => {
            if ctx.is_void {
                ctx.spirv().ret().unwrap();
            } else {
                let return_value = trans_place_load(ctx, &Place::return_place()).0;
                ctx.ctx.spirv.ret_value(return_value).unwrap();
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
                .map(|arg| trans_operand(ctx, arg).0)
                .collect::<Vec<_>>();
            let result = ctx
                .spirv()
                .function_call(result_type.def(), None, function, arguments)
                .unwrap();
            trans_place_store(ctx, &destination.0, result);
            let destination_bb = ctx.get_basic_block(destination.1);
            ctx.spirv().branch(destination_bb).unwrap();
        }
        ref thing => panic!("Unknown terminator: {:?}", thing),
    }
}

fn trans_rvalue<'tcx>(ctx: &mut FnCtx<'_, 'tcx>, expr: &Rvalue<'tcx>) -> Word {
    match expr {
        Rvalue::Use(operand) => trans_operand(ctx, operand).0,
        Rvalue::BinaryOp(op, left, right) => trans_binaryop(ctx, op, left, right),
        Rvalue::Ref(_region, _borrow_kind, place) => trans_place(ctx, place).0,
        thing => panic!("Unknown rvalue: {:?}", thing),
    }
}

fn trans_binaryop<'tcx>(
    ctx: &mut FnCtx<'_, 'tcx>,
    op: &BinOp,
    left: &Operand<'tcx>,
    right: &Operand<'tcx>,
) -> Word {
    let (left, left_ty) = trans_operand(ctx, left);
    let (right, right_ty) = trans_operand(ctx, right);
    assert_eq!(left_ty, right_ty);
    let result_type = left_ty.def();
    // TODO: match on type to do correct operation (s_div vs. u_div, etc.)
    match op {
        BinOp::Add => ctx.spirv().i_add(result_type, None, left, right).unwrap(),
        BinOp::Sub => ctx.spirv().i_sub(result_type, None, left, right).unwrap(),
        BinOp::Mul => ctx.spirv().i_mul(result_type, None, left, right).unwrap(),
        BinOp::Div => ctx.spirv().u_div(result_type, None, left, right).unwrap(),
        BinOp::Rem => ctx.spirv().u_mod(result_type, None, left, right).unwrap(),
        BinOp::BitXor => ctx
            .spirv()
            .bitwise_xor(result_type, None, left, right)
            .unwrap(),
        BinOp::BitAnd => ctx
            .spirv()
            .bitwise_and(result_type, None, left, right)
            .unwrap(),
        BinOp::BitOr => ctx
            .spirv()
            .bitwise_or(result_type, None, left, right)
            .unwrap(),
        BinOp::Shl => ctx
            .spirv()
            .shift_left_logical(result_type, None, left, right)
            .unwrap(),
        // TODO: Is this logical or arith?
        BinOp::Shr => ctx
            .spirv()
            .shift_right_logical(result_type, None, left, right)
            .unwrap(),
        BinOp::Eq => {
            let bool = ctx.spirv().type_bool();
            ctx.spirv().i_equal(bool, None, left, right).unwrap()
        }
        BinOp::Ne => {
            let bool = ctx.spirv().type_bool();
            ctx.spirv().i_not_equal(bool, None, left, right).unwrap()
        }
        BinOp::Lt => {
            let bool = ctx.spirv().type_bool();
            ctx.spirv().u_less_than(bool, None, left, right).unwrap()
        }
        BinOp::Le => {
            let bool = ctx.spirv().type_bool();
            ctx.spirv()
                .s_less_than_equal(bool, None, left, right)
                .unwrap()
        }
        BinOp::Ge => {
            let bool = ctx.spirv().type_bool();
            ctx.spirv()
                .u_greater_than_equal(bool, None, left, right)
                .unwrap()
        }
        BinOp::Gt => {
            let bool = ctx.spirv().type_bool();
            ctx.spirv().u_greater_than(bool, None, left, right).unwrap()
        }
        BinOp::Offset => {
            // TODO: Look up what result_type is supposed to be here
            let ptr_size = ctx.ctx.pointer_size.val() as u32;
            let int = ctx.spirv().type_int(ptr_size, 1);
            ctx.spirv().ptr_diff(int, None, left, right).unwrap()
        }
    }
}

fn trans_operand<'tcx>(ctx: &mut FnCtx<'_, 'tcx>, operand: &Operand<'tcx>) -> (Word, SpirvType) {
    match operand {
        Operand::Copy(place) | Operand::Move(place) => trans_place_load(ctx, place),
        Operand::Constant(constant) => panic!("Unimplemented Operand::Constant: {:?}", constant),
    }
}

fn trans_place_load<'tcx>(ctx: &mut FnCtx<'_, 'tcx>, place: &Place<'tcx>) -> (Word, SpirvType) {
    let (pointer, deref_pointer_type) = trans_place(ctx, place);
    let loaded = ctx
        .spirv()
        .load(deref_pointer_type.def(), None, pointer, None, &[])
        .unwrap();
    (loaded, deref_pointer_type)
}

fn trans_place_store<'tcx>(ctx: &mut FnCtx<'_, 'tcx>, place: &Place<'tcx>, expr: Word) {
    let (pointer, _deref_pointer_type) = trans_place(ctx, place);
    ctx.spirv().store(pointer, expr, None, &[]).unwrap();
}

// Returns (pointer, deref_pointer_type)
fn trans_place<'tcx>(ctx: &mut FnCtx<'_, 'tcx>, place: &Place<'tcx>) -> (Word, SpirvType) {
    let local = ctx.locals.get(&place.local);
    let mut access_chain = Vec::new();
    let mut result_type = local.pointee_ty();
    for proj in place.projection {
        match proj {
            ProjectionElem::Deref => {
                let int_ty = ctx.ctx.spirv.type_int(32, 1);
                let indexer_value = ctx.ctx.spirv.constant_u32(int_ty, 0);
                access_chain.push(indexer_value);
                result_type = match result_type {
                    SpirvType::Pointer { pointee, .. } => pointee,
                    ty => panic!("Deref on non-pointer type: {:?}", ty),
                };
            }
            ProjectionElem::Field(field, _field_type) => {
                let int_ty = ctx.ctx.spirv.type_int(32, 1);
                let indexer_value = ctx.ctx.spirv.constant_u32(int_ty, field.as_u32());
                access_chain.push(indexer_value);
                result_type = match result_type {
                    SpirvType::Adt { field_types, .. } => &field_types[field.index()],
                    ty => panic!("Field access on non-ADT type: {:?}", ty),
                };
            }
            ProjectionElem::Index(local) => {
                panic!("Projection::Index not supported yet: {:?}", local)
            }
            ProjectionElem::ConstantIndex { .. } => {
                panic!("Projection::ConstantIndex not supported yet")
            }
            ProjectionElem::Subslice { .. } => panic!("Projection::Subslice not supported yet"),
            ProjectionElem::Downcast { .. } => panic!("Projection::Downcast not supported yet"),
        }
    }

    let pointer = if access_chain.is_empty() {
        local.def
    } else {
        let result_ptr_type =
            ctx.ctx
                .spirv
                .type_pointer(None, StorageClass::Function, result_type.def());
        ctx.ctx
            .spirv
            .access_chain(result_ptr_type, None, local.def, access_chain)
            .unwrap()
    };

    (pointer, result_type.clone())
}
