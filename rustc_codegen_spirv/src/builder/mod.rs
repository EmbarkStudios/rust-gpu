mod builder_methods;

use crate::{builder_spirv::BuilderCursor, codegen_cx::CodegenCx};
use rustc_ast::ast::{InlineAsmOptions, InlineAsmTemplatePiece};
use rustc_codegen_ssa::coverageinfo::CounterOp;
use rustc_codegen_ssa::mir::operand::OperandRef;
use rustc_codegen_ssa::mir::place::PlaceRef;
use rustc_codegen_ssa::traits::InlineAsmOperandRef;
use rustc_codegen_ssa::traits::{
    AbiBuilderMethods, ArgAbiMethods, AsmBuilderMethods, BackendTypes, CoverageInfoBuilderMethods,
    DebugInfoBuilderMethods, HasCodegen, IntrinsicCallMethods, StaticBuilderMethods,
};
use rustc_hir::LlvmInlineAsmInner;
use rustc_middle::mir::Operand;
use rustc_middle::ty::layout::{HasParamEnv, HasTyCtxt, TyAndLayout};
use rustc_middle::ty::{Instance, ParamEnv, Ty, TyCtxt};
use rustc_span::def_id::DefId;
use rustc_span::source_map::Span;
use rustc_span::symbol::Symbol;
use rustc_target::abi::call::{ArgAbi, FnAbi};
use rustc_target::abi::{HasDataLayout, LayoutOf, Size, TargetDataLayout};
use rustc_target::spec::{HasTargetSpec, Target};
use std::ops::Deref;

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
        _arg_abi: &ArgAbi<'tcx, Ty<'tcx>>,
        _idx: &mut usize,
        _dst: PlaceRef<'tcx, Self::Value>,
    ) {
        todo!()
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
        span: Span,
        caller_instance: Instance<'tcx>,
    ) {
        println!("TODO: codegen_intrinsic_call unimplemented: instance={:?} fn_abi={:?} args={:?} llresult={:?} span={:?} caller_instance={:?}", instance, fn_abi, args, llresult, span, caller_instance);
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
