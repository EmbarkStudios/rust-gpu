mod builder_methods;

use crate::codegen_cx::CodegenCx;
use rustc_ast::ast::InlineAsmOptions;
use rustc_ast::ast::InlineAsmTemplatePiece;
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
use rustc_middle::ty::Instance;
use rustc_middle::ty::{ParamEnv, Ty, TyCtxt};
use rustc_span::def_id::DefId;
use rustc_span::source_map::Span;
use rustc_span::symbol::Symbol;
use rustc_target::abi::call::ArgAbi;
use rustc_target::abi::call::FnAbi;
use rustc_target::abi::{HasDataLayout, LayoutOf, Size, TargetDataLayout};
use rustc_target::spec::{HasTargetSpec, Target};
use std::ops::Deref;

pub struct Builder<'a, 'spv, 'tcx> {
    pub spirv: rspirv::dr::Builder,
    cx: &'a CodegenCx<'spv, 'tcx>,
}

impl<'a, 'spv, 'tcx> Deref for Builder<'a, 'spv, 'tcx> {
    type Target = CodegenCx<'spv, 'tcx>;

    fn deref(&self) -> &<Self as std::ops::Deref>::Target {
        self.cx
    }
}

impl<'a, 'spv, 'tcx> CoverageInfoBuilderMethods<'tcx> for Builder<'a, 'spv, 'tcx> {
    fn add_counter_region(
        &mut self,
        _: rustc_middle::ty::Instance<'tcx>,
        _: u64,
        _: u32,
        _: u32,
        _: u32,
    ) {
        todo!()
    }

    fn add_counter_expression_region(
        &mut self,
        _: rustc_middle::ty::Instance<'tcx>,
        _: u32,
        _: u32,
        _: rustc_codegen_ssa::coverageinfo::CounterOp,
        _: u32,
        _: u32,
        _: u32,
    ) {
        todo!()
    }

    fn add_unreachable_region(&mut self, _: rustc_middle::ty::Instance<'tcx>, _: u32, _: u32) {
        todo!()
    }
}

impl<'a, 'spv, 'tcx> DebugInfoBuilderMethods for Builder<'a, 'spv, 'tcx> {
    fn dbg_var_addr(
        &mut self,
        dbg_var: Self::DIVariable,
        scope_metadata: Self::DIScope,
        variable_alloca: Self::Value,
        direct_offset: Size,
        // NB: each offset implies a deref (i.e. they're steps in a pointer chain).
        indirect_offsets: &[Size],
        span: Span,
    ) {
        todo!()
    }

    fn set_source_location(&mut self, scope: Self::DIScope, span: Span) {
        todo!()
    }

    fn insert_reference_to_gdb_debug_scripts_section_global(&mut self) {
        todo!()
    }

    fn set_var_name(&mut self, value: Self::Value, name: &str) {
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
        todo!()
    }

    fn store_arg(
        &mut self,
        arg_abi: &ArgAbi<'tcx, Ty<'tcx>>,
        val: Self::Value,
        dst: PlaceRef<'tcx, Self::Value>,
    ) {
        todo!()
    }

    fn arg_memory_ty(&self, arg_abi: &ArgAbi<'tcx, Ty<'tcx>>) -> Self::Type {
        todo!()
    }
}

impl<'a, 'spv, 'tcx> AbiBuilderMethods<'tcx> for Builder<'a, 'spv, 'tcx> {
    fn apply_attrs_callsite(&mut self, fn_abi: &FnAbi<'tcx, Ty<'tcx>>, callsite: Self::Value) {
        todo!()
    }

    fn get_param(&self, index: usize) -> Self::Value {
        todo!()
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
        todo!()
    }

    fn is_codegen_intrinsic(
        &mut self,
        intrinsic: Symbol,
        args: &Vec<Operand<'tcx>>,
        caller_instance: Instance<'tcx>,
    ) -> bool {
        todo!()
    }

    fn abort(&mut self) {
        todo!()
    }
    fn assume(&mut self, val: Self::Value) {
        todo!()
    }
    fn expect(&mut self, cond: Self::Value, expected: bool) -> Self::Value {
        todo!()
    }
    fn sideeffect(&mut self) {
        todo!()
    }
    fn va_start(&mut self, val: Self::Value) -> Self::Value {
        todo!()
    }
    fn va_end(&mut self, val: Self::Value) -> Self::Value {
        todo!()
    }
}

impl<'a, 'spv, 'tcx> AsmBuilderMethods<'tcx> for Builder<'a, 'spv, 'tcx> {
    fn codegen_llvm_inline_asm(
        &mut self,
        ia: &LlvmInlineAsmInner,
        outputs: Vec<PlaceRef<'tcx, Self::Value>>,
        inputs: Vec<Self::Value>,
        span: Span,
    ) -> bool {
        todo!()
    }

    fn codegen_inline_asm(
        &mut self,
        template: &[InlineAsmTemplatePiece],
        operands: &[InlineAsmOperandRef<'tcx, Self>],
        options: InlineAsmOptions,
        line_spans: &[Span],
    ) {
        todo!()
    }
}
impl<'a, 'spv, 'tcx> StaticBuilderMethods for Builder<'a, 'spv, 'tcx> {
    fn get_static(&mut self, def_id: DefId) -> Self::Value {
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
