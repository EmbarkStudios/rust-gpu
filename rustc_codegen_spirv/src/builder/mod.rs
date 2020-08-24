mod builder_methods;

use crate::abi::SpirvType;
use crate::builder_spirv::{BuilderCursor, SpirvValue, SpirvValueExt};
use crate::codegen_cx::CodegenCx;
use rspirv::spirv::StorageClass;
use rustc_ast::ast::{InlineAsmOptions, InlineAsmTemplatePiece};
use rustc_codegen_ssa::coverageinfo::CounterOp;
use rustc_codegen_ssa::mir::operand::{OperandRef, OperandValue};
use rustc_codegen_ssa::mir::place::PlaceRef;
use rustc_codegen_ssa::traits::{
    AbiBuilderMethods, ArgAbiMethods, AsmBuilderMethods, BackendTypes, BuilderMethods,
    CoverageInfoBuilderMethods, DebugInfoBuilderMethods, HasCodegen, InlineAsmOperandRef,
    IntrinsicCallMethods, StaticBuilderMethods,
};
use rustc_hir::LlvmInlineAsmInner;
use rustc_middle::mir::Operand;
use rustc_middle::ty::layout::{HasParamEnv, HasTyCtxt, TyAndLayout};
use rustc_middle::ty::{FnDef, Instance, ParamEnv, Ty, TyCtxt};
use rustc_span::def_id::DefId;
use rustc_span::source_map::Span;
use rustc_span::sym;
use rustc_span::symbol::Symbol;
use rustc_target::abi::call::{ArgAbi, FnAbi, PassMode};
use rustc_target::abi::{HasDataLayout, LayoutOf, Size, TargetDataLayout};
use rustc_target::spec::{HasTargetSpec, Target};
use std::{iter::empty, ops::Deref};

pub struct Builder<'a, 'spv, 'tcx> {
    cx: &'a CodegenCx<'spv, 'tcx>,
    cursor: BuilderCursor,
    current_fn: <Self as BackendTypes>::Function,
    basic_block: <Self as BackendTypes>::BasicBlock,
}

impl<'a, 'spv, 'tcx> Builder<'a, 'spv, 'tcx> {
    pub fn emit(&self) -> std::cell::RefMut<rspirv::dr::Builder> {
        self.emit_with_cursor(self.cursor)
    }

    pub fn gep_help(
        &self,
        ptr: SpirvValue,
        indices: &[SpirvValue],
        is_inbounds: bool,
    ) -> SpirvValue {
        // The first index is an offset to the pointer, the rest are actual members.
        // https://llvm.org/docs/GetElementPtr.html
        // "An OpAccessChain instruction is the equivalent of an LLVM getelementptr instruction where the first index element is zero."
        // https://github.com/gpuweb/gpuweb/issues/33
        let mut result_indices = Vec::with_capacity(indices.len() - 1);
        let /*mut*/ result_pointee_type = match self.lookup_type(ptr.ty) {
            SpirvType::Pointer { pointee } => pointee,
            other_type => panic!("GEP first deref not implemented for type {:?}", other_type),
        };
        for index in indices.iter().cloned().skip(1) {
            result_indices.push(index.def);
            panic!(
                "GEP not implemented for type {:?}",
                self.lookup_type(result_pointee_type)
            );
        }
        let result_type =
            self.emit()
                .type_pointer(None, StorageClass::Generic, result_pointee_type);
        self.def_type(
            result_type,
            SpirvType::Pointer {
                pointee: result_pointee_type,
            },
        );
        if self.builder.lookup_const(indices[0].def) == Some(0) {
            if is_inbounds {
                self.emit()
                    .in_bounds_access_chain(result_type, None, ptr.def, result_indices)
                    .unwrap()
                    .with_type(result_type)
            } else {
                self.emit()
                    .access_chain(result_type, None, ptr.def, result_indices)
                    .unwrap()
                    .with_type(result_type)
            }
        } else if is_inbounds {
            self.emit()
                .in_bounds_ptr_access_chain(
                    result_type,
                    None,
                    ptr.def,
                    indices[0].def,
                    result_indices,
                )
                .unwrap()
                .with_type(result_type)
        } else {
            self.emit()
                .ptr_access_chain(result_type, None, ptr.def, indices[0].def, result_indices)
                .unwrap()
                .with_type(result_type)
        }
    }
}

// Important: This lets us use CodegenCx methods on Builder
impl<'a, 'spv, 'tcx> Deref for Builder<'a, 'spv, 'tcx> {
    type Target = CodegenCx<'spv, 'tcx>;

    fn deref(&self) -> &Self::Target {
        self.cx
    }
}

impl<'a, 'spv, 'tcx> CoverageInfoBuilderMethods<'tcx> for Builder<'a, 'spv, 'tcx> {
    fn add_counter_region(
        &mut self,
        _instance: Instance<'tcx>,
        _function_source_hash: u64,
        _index: u32,
        _start_byte_pos: u32,
        _end_byte_pos: u32,
    ) {
        todo!()
    }

    fn add_counter_expression_region(
        &mut self,
        _instance: Instance<'tcx>,
        _index: u32,
        _lhs: u32,
        _op: CounterOp,
        _rhs: u32,
        _start_byte_pos: u32,
        _end_byte_pos: u32,
    ) {
        todo!()
    }

    fn add_unreachable_region(
        &mut self,
        _instance: Instance<'tcx>,
        _start_byte_pos: u32,
        _end_byte_pos: u32,
    ) {
        todo!()
    }
}

impl<'a, 'spv, 'tcx> DebugInfoBuilderMethods for Builder<'a, 'spv, 'tcx> {
    fn dbg_var_addr(
        &mut self,
        _dbg_var: Self::DIVariable,
        _scope_metadata: Self::DIScope,
        _variable_alloca: Self::Value,
        _direct_offset: Size,
        // NB: each offset implies a deref (i.e. they're steps in a pointer chain).
        _indirect_offsets: &[Size],
        _span: Span,
    ) {
        todo!()
    }

    fn set_source_location(&mut self, _scope: Self::DIScope, _span: Span) {
        todo!()
    }

    fn insert_reference_to_gdb_debug_scripts_section_global(&mut self) {
        todo!()
    }

    fn set_var_name(&mut self, _value: Self::Value, _name: &str) {
        todo!()
    }
}

impl<'a, 'spv, 'tcx> ArgAbiMethods<'tcx> for Builder<'a, 'spv, 'tcx> {
    fn store_fn_arg(
        &mut self,
        arg_abi: &ArgAbi<'tcx, Ty<'tcx>>,
        idx: &mut usize,
        dst: PlaceRef<'tcx, Self::Value>,
    ) {
        fn next<'a, 'spv, 'tcx>(bx: &mut Builder<'a, 'spv, 'tcx>, idx: &mut usize) -> SpirvValue {
            let val = bx.function_parameter_values.borrow()[&bx.current_fn][*idx];
            *idx += 1;
            val
        }
        match arg_abi.mode {
            PassMode::Ignore => (),
            PassMode::Direct(_) => OperandValue::Immediate(next(self, idx)).store(self, dst),
            PassMode::Pair(..) => {
                OperandValue::Pair(next(self, idx), next(self, idx)).store(self, dst)
            }
            PassMode::Indirect(_, Some(_)) => OperandValue::Ref(
                next(self, idx),
                Some(next(self, idx)),
                arg_abi.layout.align.abi,
            )
            .store(self, dst),
            PassMode::Indirect(_, None) => {
                OperandValue::Ref(next(self, idx), None, arg_abi.layout.align.abi).store(self, dst)
            }
            PassMode::Cast(_) => {
                panic!("TODO: store_fn_arg PassMode::Cast not implemented yet");
            }
        }
    }

    fn store_arg(
        &mut self,
        _arg_abi: &ArgAbi<'tcx, Ty<'tcx>>,
        _val: Self::Value,
        _dst: PlaceRef<'tcx, Self::Value>,
    ) {
        todo!()
    }

    fn arg_memory_ty(&self, arg_abi: &ArgAbi<'tcx, Ty<'tcx>>) -> Self::Type {
        self.trans_type(arg_abi.layout)
    }
}

impl<'a, 'spv, 'tcx> AbiBuilderMethods<'tcx> for Builder<'a, 'spv, 'tcx> {
    fn apply_attrs_callsite(&mut self, _fn_abi: &FnAbi<'tcx, Ty<'tcx>>, _callsite: Self::Value) {
        // TODO: Implement this?
    }

    fn get_param(&self, index: usize) -> Self::Value {
        self.function_parameter_values.borrow()[&self.current_fn][index]
    }
}

impl<'a, 'spv, 'tcx> IntrinsicCallMethods<'tcx> for Builder<'a, 'spv, 'tcx> {
    fn codegen_intrinsic_call(
        &mut self,
        instance: Instance<'tcx>,
        fn_abi: &FnAbi<'tcx, Ty<'tcx>>,
        args: &[OperandRef<'tcx, Self::Value>],
        llresult: Self::Value,
        _span: Span,
        _caller_instance: Instance<'tcx>,
    ) {
        let callee_ty = instance.ty(self.tcx, ParamEnv::reveal_all());

        let (def_id, _substs) = match callee_ty.kind {
            FnDef(def_id, substs) => (def_id, substs),
            _ => panic!("expected fn item type, found {}", callee_ty),
        };

        let sig = callee_ty.fn_sig(self.tcx);
        let sig = self
            .tcx
            .normalize_erasing_late_bound_regions(ParamEnv::reveal_all(), &sig);
        // let arg_tys = sig.inputs();
        let ret_ty = sig.output();
        let name = self.tcx.item_name(def_id);
        // let name_str = &*name.as_str();

        // let spirv_ret_ty = self.trans_type(self.layout_of(ret_ty));
        let result = PlaceRef::new_sized(llresult, fn_abi.ret.layout);

        let value = match name {
            sym::size_of
            | sym::pref_align_of
            | sym::min_align_of
            | sym::needs_drop
            | sym::type_id
            | sym::type_name
            | sym::variant_count => {
                let value = self
                    .tcx
                    .const_eval_instance(ParamEnv::reveal_all(), instance, None)
                    .unwrap();
                OperandRef::from_const(self, value, ret_ty).immediate_or_packed_pair(self)
            }
            sym::copy_nonoverlapping
            | sym::copy
            | sym::volatile_copy_nonoverlapping_memory
            | sym::volatile_copy_memory => {
                // let ty = substs.type_at(0);
                let dst = args[0].immediate();
                let src = args[1].immediate();
                let count = args[2].immediate();
                // TODO: rspirv doesn't have copy_memory_sized yet
                self.emit()
                    .copy_memory_sized(dst.def, src.def, count.def, None, None, empty())
                    .unwrap();
                assert!(fn_abi.ret.is_ignore());
                return;
            }
            sym::offset => {
                let ptr = args[0].immediate();
                let offset = args[1].immediate();
                self.inbounds_gep(ptr, &[offset])
            }

            _ => panic!("TODO: Unknown intrinsic '{}'", name),
        };

        if !fn_abi.ret.is_ignore() {
            if let PassMode::Cast(_ty) = fn_abi.ret.mode {
                panic!("TODO: PassMode::Cast not implemented yet in intrinsics");
            } else {
                OperandRef::from_immediate_or_packed_pair(self, value, result.layout)
                    .val
                    .store(self, result);
            }
        }
    }

    fn is_codegen_intrinsic(
        &mut self,
        _intrinsic: Symbol,
        _args: &Vec<Operand<'tcx>>,
        _caller_instance: Instance<'tcx>,
    ) -> bool {
        // TODO
        true
    }

    fn abort(&mut self) {
        self.emit().kill().unwrap();
    }

    fn assume(&mut self, _val: Self::Value) {
        // TODO: llvm.assume
    }

    fn expect(&mut self, cond: Self::Value, _expected: bool) -> Self::Value {
        // TODO: llvm.expect
        cond
    }

    fn sideeffect(&mut self) {
        // TODO: This is currently ignored.
        // It corresponds to the llvm.sideeffect intrinsic - does spir-v have an equivalent?
    }

    fn va_start(&mut self, _val: Self::Value) -> Self::Value {
        todo!()
    }

    fn va_end(&mut self, _val: Self::Value) -> Self::Value {
        todo!()
    }
}

impl<'a, 'spv, 'tcx> AsmBuilderMethods<'tcx> for Builder<'a, 'spv, 'tcx> {
    fn codegen_llvm_inline_asm(
        &mut self,
        _ia: &LlvmInlineAsmInner,
        _outputs: Vec<PlaceRef<'tcx, Self::Value>>,
        _inputs: Vec<Self::Value>,
        _span: Span,
    ) -> bool {
        todo!()
    }

    fn codegen_inline_asm(
        &mut self,
        _template: &[InlineAsmTemplatePiece],
        _operands: &[InlineAsmOperandRef<'tcx, Self>],
        _options: InlineAsmOptions,
        _line_spans: &[Span],
    ) {
        todo!()
    }
}
impl<'a, 'spv, 'tcx> StaticBuilderMethods for Builder<'a, 'spv, 'tcx> {
    fn get_static(&mut self, _def_id: DefId) -> Self::Value {
        todo!()
    }
}

impl<'a, 'spv, 'tcx> BackendTypes for Builder<'a, 'spv, 'tcx> {
    type Value = <CodegenCx<'spv, 'tcx> as BackendTypes>::Value;
    type Function = <CodegenCx<'spv, 'tcx> as BackendTypes>::Function;
    type BasicBlock = <CodegenCx<'spv, 'tcx> as BackendTypes>::BasicBlock;
    type Type = <CodegenCx<'spv, 'tcx> as BackendTypes>::Type;
    type Funclet = <CodegenCx<'spv, 'tcx> as BackendTypes>::Funclet;

    type DIScope = <CodegenCx<'spv, 'tcx> as BackendTypes>::DIScope;
    type DIVariable = <CodegenCx<'spv, 'tcx> as BackendTypes>::DIVariable;
}

impl<'a, 'spv, 'tcx> HasCodegen<'tcx> for Builder<'a, 'spv, 'tcx> {
    type CodegenCx = CodegenCx<'spv, 'tcx>;
}

impl<'a, 'spv, 'tcx> HasParamEnv<'tcx> for Builder<'a, 'spv, 'tcx> {
    fn param_env(&self) -> ParamEnv<'tcx> {
        self.cx.param_env()
    }
}

impl<'a, 'spv, 'tcx> HasTargetSpec for Builder<'a, 'spv, 'tcx> {
    fn target_spec(&self) -> &Target {
        &self.cx.target_spec()
    }
}

impl<'a, 'spv, 'tcx> HasTyCtxt<'tcx> for Builder<'a, 'spv, 'tcx> {
    fn tcx(&self) -> TyCtxt<'tcx> {
        self.cx.tcx
    }
}

impl<'a, 'spv, 'tcx> HasDataLayout for Builder<'a, 'spv, 'tcx> {
    fn data_layout(&self) -> &TargetDataLayout {
        self.cx.data_layout()
    }
}

impl<'a, 'spv, 'tcx> LayoutOf for Builder<'a, 'spv, 'tcx> {
    type Ty = Ty<'tcx>;
    type TyAndLayout = TyAndLayout<'tcx>;

    fn layout_of(&self, ty: Ty<'tcx>) -> Self::TyAndLayout {
        self.cx.layout_of(ty)
    }
}
