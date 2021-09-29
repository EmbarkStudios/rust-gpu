mod builder_methods;
mod byte_addressable_buffer;
mod ext_inst;
mod intrinsics;
pub mod libm_intrinsics;
mod spirv_asm;

pub use ext_inst::ExtInst;
use rustc_span::DUMMY_SP;
pub use spirv_asm::InstructionTable;

use crate::abi::ConvSpirvType;
use crate::builder_spirv::{BuilderCursor, SpirvValue, SpirvValueExt};
use crate::codegen_cx::CodegenCx;
use crate::spirv_type::SpirvType;
use rspirv::spirv::Word;
use rustc_codegen_ssa::mir::operand::OperandValue;
use rustc_codegen_ssa::mir::place::PlaceRef;
use rustc_codegen_ssa::traits::{
    AbiBuilderMethods, ArgAbiMethods, BackendTypes, BuilderMethods, CoverageInfoBuilderMethods,
    DebugInfoBuilderMethods, HasCodegen, StaticBuilderMethods,
};
use rustc_errors::DiagnosticBuilder;
use rustc_middle::mir::coverage::{
    CodeRegion, CounterValueReference, ExpressionOperandId, InjectedExpressionId, Op,
};
use rustc_middle::ty::layout::{
    FnAbiError, FnAbiOfHelpers, FnAbiRequest, HasParamEnv, HasTyCtxt, LayoutError, LayoutOfHelpers,
    TyAndLayout,
};
use rustc_middle::ty::{Instance, ParamEnv, Ty, TyCtxt};
use rustc_span::def_id::DefId;
use rustc_span::source_map::Span;
use rustc_target::abi::call::{ArgAbi, FnAbi, PassMode};
use rustc_target::abi::{HasDataLayout, Size, TargetDataLayout};
use rustc_target::spec::{HasTargetSpec, Target};
use std::ops::Deref;

pub struct Builder<'a, 'tcx> {
    cx: &'a CodegenCx<'tcx>,
    cursor: BuilderCursor,
    current_fn: <Self as BackendTypes>::Function,
    basic_block: <Self as BackendTypes>::BasicBlock,
    current_span: Option<Span>,
}

impl<'a, 'tcx> Builder<'a, 'tcx> {
    /// See comment on `BuilderCursor`
    pub fn emit(&self) -> std::cell::RefMut<'_, rspirv::dr::Builder> {
        self.emit_with_cursor(self.cursor)
    }

    pub fn zombie(&self, word: Word, reason: &str) {
        if let Some(current_span) = self.current_span {
            self.zombie_with_span(word, current_span, reason);
        } else {
            self.zombie_no_span(word, reason);
        }
    }

    pub fn validate_atomic(&self, ty: Word, to_zombie: Word) {
        if !self.i8_i16_atomics_allowed {
            match self.lookup_type(ty) {
                SpirvType::Integer(width, _) if width < 32 => {
                    self.zombie(to_zombie, "atomic on i8 or i16 when disallowed by runtime");
                }
                _ => (),
            }
        }
    }

    pub fn struct_err(&self, msg: &str) -> DiagnosticBuilder<'_> {
        if let Some(current_span) = self.current_span {
            self.tcx.sess.struct_span_err(current_span, msg)
        } else {
            self.tcx.sess.struct_err(msg)
        }
    }

    pub fn err(&self, msg: &str) {
        if let Some(current_span) = self.current_span {
            self.tcx.sess.span_err(current_span, msg);
        } else {
            self.tcx.sess.err(msg);
        }
    }

    pub fn fatal(&self, msg: &str) -> ! {
        if let Some(current_span) = self.current_span {
            self.tcx.sess.span_fatal(current_span, msg)
        } else {
            self.tcx.sess.fatal(msg)
        }
    }

    pub fn span(&self) -> Span {
        self.current_span.unwrap_or(DUMMY_SP)
    }

    pub fn gep_help(
        &self,
        ty: Word,
        ptr: SpirvValue,
        indices: &[SpirvValue],
        is_inbounds: bool,
    ) -> SpirvValue {
        // The first index is an offset to the pointer, the rest are actual members.
        // https://llvm.org/docs/GetElementPtr.html
        // "An OpAccessChain instruction is the equivalent of an LLVM getelementptr instruction where the first index element is zero."
        // https://github.com/gpuweb/gpuweb/issues/33
        let mut result_indices = Vec::with_capacity(indices.len() - 1);
        let mut result_pointee_type = match self.lookup_type(ptr.ty) {
            SpirvType::Pointer { pointee } => {
                assert_ty_eq!(self, ty, pointee);
                pointee
            }
            other_type => self.fatal(&format!(
                "GEP first deref not implemented for type {:?}",
                other_type
            )),
        };
        for index in indices.iter().cloned().skip(1) {
            result_indices.push(index.def(self));
            result_pointee_type = match self.lookup_type(result_pointee_type) {
                SpirvType::Array { element, .. } | SpirvType::RuntimeArray { element } => element,
                _ => self.fatal(&format!(
                    "GEP not implemented for type {}",
                    self.debug_type(result_pointee_type)
                )),
            };
        }
        let result_type = SpirvType::Pointer {
            pointee: result_pointee_type,
        }
        .def(self.span(), self);
        if self.builder.lookup_const_u64(indices[0]) == Some(0) {
            if is_inbounds {
                self.emit()
                    .in_bounds_access_chain(result_type, None, ptr.def(self), result_indices)
                    .unwrap()
                    .with_type(result_type)
            } else {
                self.emit()
                    .access_chain(result_type, None, ptr.def(self), result_indices)
                    .unwrap()
                    .with_type(result_type)
            }
        } else {
            let result = if is_inbounds {
                self.emit()
                    .in_bounds_ptr_access_chain(
                        result_type,
                        None,
                        ptr.def(self),
                        indices[0].def(self),
                        result_indices,
                    )
                    .unwrap()
                    .with_type(result_type)
            } else {
                self.emit()
                    .ptr_access_chain(
                        result_type,
                        None,
                        ptr.def(self),
                        indices[0].def(self),
                        result_indices,
                    )
                    .unwrap()
                    .with_type(result_type)
            };
            self.zombie(
                result.def(self),
                "Cannot offset a pointer to an arbitrary element",
            );
            result
        }
    }

    // TODO: Definitely add tests to make sure this impl is right.
    fn rotate(&mut self, value: SpirvValue, shift: SpirvValue, is_left: bool) -> SpirvValue {
        let width = match self.lookup_type(shift.ty) {
            SpirvType::Integer(width, _) => width,
            other => self.fatal(&format!(
                "Cannot rotate non-integer type: {}",
                other.debug(shift.ty, self)
            )),
        };
        let int_size = self.constant_int(shift.ty, width as u64);
        let mask = self.constant_int(shift.ty, (width - 1) as u64);
        let zero = self.constant_int(shift.ty, 0);
        let bool = SpirvType::Bool.def(self.span(), self);
        // https://stackoverflow.com/a/10134877
        let mask_shift = self.and(shift, mask);
        let sub = self.sub(int_size, mask_shift);
        let (lhs, rhs) = if is_left {
            (self.shl(value, mask_shift), self.lshr(value, sub))
        } else {
            (self.lshr(value, mask_shift), self.shl(value, sub))
        };
        let or = self.or(lhs, rhs);
        // "The result is undefined if Shift is greater than or equal to the bit width of the components of Base."
        // So we need to check for zero shift, and don't use the shift result if it is.
        let mask_is_zero = self
            .emit()
            .i_not_equal(bool, None, mask_shift.def(self), zero.def(self))
            .unwrap()
            .with_type(bool);
        self.select(mask_is_zero, value, or)
    }
}

// Important: This lets us use CodegenCx methods on Builder
impl<'a, 'tcx> Deref for Builder<'a, 'tcx> {
    type Target = CodegenCx<'tcx>;

    fn deref(&self) -> &Self::Target {
        self.cx
    }
}

impl<'a, 'tcx> CoverageInfoBuilderMethods<'tcx> for Builder<'a, 'tcx> {
    fn set_function_source_hash(&mut self, _: rustc_middle::ty::Instance<'tcx>, _: u64) -> bool {
        todo!()
    }
    fn add_coverage_counter(
        &mut self,
        _: Instance<'tcx>,
        _: CounterValueReference,
        _: CodeRegion,
    ) -> bool {
        todo!()
    }
    fn add_coverage_counter_expression(
        &mut self,
        _: Instance<'tcx>,
        _: InjectedExpressionId,
        _: ExpressionOperandId,
        _: Op,
        _: ExpressionOperandId,
        _: Option<CodeRegion>,
    ) -> bool {
        todo!()
    }
    fn add_coverage_unreachable(&mut self, _: Instance<'tcx>, _: CodeRegion) -> bool {
        todo!()
    }
}

impl<'a, 'tcx> DebugInfoBuilderMethods for Builder<'a, 'tcx> {
    fn dbg_var_addr(
        &mut self,
        _dbg_var: Self::DIVariable,
        _scope_metadata: Self::DILocation,
        _variable_alloca: Self::Value,
        _direct_offset: Size,
        // NB: each offset implies a deref (i.e. they're steps in a pointer chain).
        _indirect_offsets: &[Size],
    ) {
        todo!()
    }

    fn set_dbg_loc(&mut self, _: Self::DILocation) {
        todo!()
    }

    fn insert_reference_to_gdb_debug_scripts_section_global(&mut self) {
        todo!()
    }

    fn set_var_name(&mut self, _value: Self::Value, _name: &str) {
        todo!()
    }
}

impl<'a, 'tcx> ArgAbiMethods<'tcx> for Builder<'a, 'tcx> {
    fn store_fn_arg(
        &mut self,
        arg_abi: &ArgAbi<'tcx, Ty<'tcx>>,
        idx: &mut usize,
        dst: PlaceRef<'tcx, Self::Value>,
    ) {
        fn next<'a, 'tcx>(bx: &mut Builder<'a, 'tcx>, idx: &mut usize) -> SpirvValue {
            let val = bx.function_parameter_values.borrow()[&bx.current_fn.def(bx)][*idx];
            *idx += 1;
            val
        }
        match arg_abi.mode {
            PassMode::Ignore => (),
            PassMode::Pair(..) => {
                OperandValue::Pair(next(self, idx), next(self, idx)).store(self, dst);
            }
            PassMode::Indirect {
                extra_attrs: Some(_),
                ..
            } => OperandValue::Ref(
                next(self, idx),
                Some(next(self, idx)),
                arg_abi.layout.align.abi,
            )
            .store(self, dst),
            PassMode::Direct(_)
            | PassMode::Indirect {
                extra_attrs: None, ..
            }
            | PassMode::Cast(_) => {
                let next_arg = next(self, idx);
                self.store_arg(arg_abi, next_arg, dst);
            }
        }
    }

    fn store_arg(
        &mut self,
        arg_abi: &ArgAbi<'tcx, Ty<'tcx>>,
        val: Self::Value,
        dst: PlaceRef<'tcx, Self::Value>,
    ) {
        if arg_abi.is_ignore() {
            return;
        }
        if arg_abi.is_sized_indirect() {
            OperandValue::Ref(val, None, arg_abi.layout.align.abi).store(self, dst);
        } else if arg_abi.is_unsized_indirect() {
            self.fatal("unsized `ArgAbi` must be handled through `store_fn_arg`");
        } else if let PassMode::Cast(cast) = arg_abi.mode {
            let cast_ty = cast.spirv_type(self.span(), self);
            let cast_ptr_ty = SpirvType::Pointer { pointee: cast_ty }.def(self.span(), self);
            let cast_dst = self.pointercast(dst.llval, cast_ptr_ty);
            self.store(val, cast_dst, arg_abi.layout.align.abi);
        } else {
            OperandValue::Immediate(val).store(self, dst);
        }
    }

    fn arg_memory_ty(&self, arg_abi: &ArgAbi<'tcx, Ty<'tcx>>) -> Self::Type {
        arg_abi.layout.spirv_type(self.span(), self)
    }
}

impl<'a, 'tcx> AbiBuilderMethods<'tcx> for Builder<'a, 'tcx> {
    fn apply_attrs_callsite(&mut self, _fn_abi: &FnAbi<'tcx, Ty<'tcx>>, _callsite: Self::Value) {}

    fn get_param(&self, index: usize) -> Self::Value {
        self.function_parameter_values.borrow()[&self.current_fn.def(self)][index]
    }
}

impl<'a, 'tcx> StaticBuilderMethods for Builder<'a, 'tcx> {
    fn get_static(&mut self, def_id: DefId) -> Self::Value {
        self.cx.get_static(def_id)
    }
}

impl<'a, 'tcx> BackendTypes for Builder<'a, 'tcx> {
    type Value = <CodegenCx<'tcx> as BackendTypes>::Value;
    type Function = <CodegenCx<'tcx> as BackendTypes>::Function;
    type BasicBlock = <CodegenCx<'tcx> as BackendTypes>::BasicBlock;
    type Type = <CodegenCx<'tcx> as BackendTypes>::Type;
    type Funclet = <CodegenCx<'tcx> as BackendTypes>::Funclet;

    type DIScope = <CodegenCx<'tcx> as BackendTypes>::DIScope;
    type DIVariable = <CodegenCx<'tcx> as BackendTypes>::DIVariable;
    type DILocation = <CodegenCx<'tcx> as BackendTypes>::DILocation;
}

impl<'a, 'tcx> HasCodegen<'tcx> for Builder<'a, 'tcx> {
    type CodegenCx = CodegenCx<'tcx>;
}

impl<'a, 'tcx> HasParamEnv<'tcx> for Builder<'a, 'tcx> {
    fn param_env(&self) -> ParamEnv<'tcx> {
        self.cx.param_env()
    }
}

impl<'a, 'tcx> HasTargetSpec for Builder<'a, 'tcx> {
    fn target_spec(&self) -> &Target {
        self.cx.target_spec()
    }
}

impl<'a, 'tcx> HasTyCtxt<'tcx> for Builder<'a, 'tcx> {
    fn tcx(&self) -> TyCtxt<'tcx> {
        self.cx.tcx
    }
}

impl<'a, 'tcx> HasDataLayout for Builder<'a, 'tcx> {
    fn data_layout(&self) -> &TargetDataLayout {
        self.cx.data_layout()
    }
}

impl<'tcx> LayoutOfHelpers<'tcx> for Builder<'_, 'tcx> {
    type LayoutOfResult = TyAndLayout<'tcx>;

    #[inline]
    fn handle_layout_err(&self, err: LayoutError<'tcx>, span: Span, ty: Ty<'tcx>) -> ! {
        self.cx.handle_layout_err(err, span, ty)
    }
}

impl<'tcx> FnAbiOfHelpers<'tcx> for Builder<'_, 'tcx> {
    type FnAbiOfResult = &'tcx FnAbi<'tcx, Ty<'tcx>>;

    #[inline]
    fn handle_fn_abi_err(
        &self,
        err: FnAbiError<'tcx>,
        span: Span,
        fn_abi_request: FnAbiRequest<'tcx>,
    ) -> ! {
        self.cx.handle_fn_abi_err(err, span, fn_abi_request)
    }
}
