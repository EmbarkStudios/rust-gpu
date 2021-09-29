use super::Builder;
use crate::abi::ConvSpirvType;
use crate::builder_spirv::{BuilderCursor, SpirvConst, SpirvValue, SpirvValueExt, SpirvValueKind};
use crate::spirv_type::SpirvType;
use rspirv::dr::{InsertPoint, Instruction, Operand};
use rspirv::spirv::{Capability, MemoryModel, MemorySemantics, Op, Scope, StorageClass, Word};
use rustc_codegen_ssa::common::{
    AtomicOrdering, AtomicRmwBinOp, IntPredicate, RealPredicate, SynchronizationScope,
};
use rustc_codegen_ssa::mir::operand::{OperandRef, OperandValue};
use rustc_codegen_ssa::mir::place::PlaceRef;
use rustc_codegen_ssa::traits::{
    BuilderMethods, ConstMethods, IntrinsicCallMethods, LayoutTypeMethods, OverflowOp,
};
use rustc_codegen_ssa::MemFlags;
use rustc_middle::bug;
use rustc_middle::ty::Ty;
use rustc_span::Span;
use rustc_target::abi::{Abi, Align, Scalar, Size, WrappingRange};
use std::convert::TryInto;
use std::iter::{self, empty};

macro_rules! simple_op {
    (
        $func_name:ident, $inst_name:ident
        $(, fold_const {
            $(int($fold_int_lhs:ident, $fold_int_rhs:ident) => $fold_int:expr)?
        })?
    ) => {
        fn $func_name(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
            assert_ty_eq!(self, lhs.ty, rhs.ty);
            let result_type = lhs.ty;

            $(if let Some(const_lhs) = self.builder.lookup_const(lhs) {
                if let Some(const_rhs) = self.builder.lookup_const(rhs) {
                    match self.lookup_type(result_type) {
                        $(SpirvType::Integer(bits, signed) => {
                            let size = Size::from_bits(bits);
                            let as_u128 = |const_val| {
                                let x = match const_val {
                                    SpirvConst::U32(x) => x as u128,
                                    SpirvConst::U64(x) => x as u128,
                                    _ => return None,
                                };
                                Some(if signed {
                                    size.sign_extend(x)
                                } else {
                                    size.truncate(x)
                                })
                            };
                            if let Some($fold_int_lhs) = as_u128(const_lhs) {
                                if let Some($fold_int_rhs) = as_u128(const_rhs) {
                                    return self.const_uint_big(result_type, $fold_int);
                                }
                            }
                        })?
                        _ => {}
                    }
                }
            })?

            self.emit()
                .$inst_name(result_type, None, lhs.def(self), rhs.def(self))
                .unwrap()
                .with_type(result_type)
        }
    };
}

// shl and shr allow different types as their operands
macro_rules! simple_op_unchecked_type {
    ($func_name:ident, $inst_name:ident) => {
        fn $func_name(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
            self.emit()
                .$inst_name(lhs.ty, None, lhs.def(self), rhs.def(self))
                .unwrap()
                .with_type(lhs.ty)
        }
    };
}

macro_rules! simple_uni_op {
    ($func_name:ident, $inst_name:ident) => {
        fn $func_name(&mut self, val: Self::Value) -> Self::Value {
            self.emit()
                .$inst_name(val.ty, None, val.def(self))
                .unwrap()
                .with_type(val.ty)
        }
    };
}

fn memset_fill_u16(b: u8) -> u16 {
    b as u16 | ((b as u16) << 8)
}

fn memset_fill_u32(b: u8) -> u32 {
    b as u32 | ((b as u32) << 8) | ((b as u32) << 16) | ((b as u32) << 24)
}

fn memset_fill_u64(b: u8) -> u64 {
    b as u64
        | ((b as u64) << 8)
        | ((b as u64) << 16)
        | ((b as u64) << 24)
        | ((b as u64) << 32)
        | ((b as u64) << 40)
        | ((b as u64) << 48)
        | ((b as u64) << 56)
}

fn memset_dynamic_scalar(
    builder: &Builder<'_, '_>,
    fill_var: Word,
    byte_width: usize,
    is_float: bool,
) -> Word {
    let composite_type = SpirvType::Vector {
        element: SpirvType::Integer(8, false).def(builder.span(), builder),
        count: byte_width as u32,
    }
    .def(builder.span(), builder);
    let composite = builder
        .emit()
        .composite_construct(
            composite_type,
            None,
            iter::repeat(fill_var).take(byte_width),
        )
        .unwrap();
    let result_type = if is_float {
        SpirvType::Float(byte_width as u32 * 8)
    } else {
        SpirvType::Integer(byte_width as u32 * 8, false)
    };
    builder
        .emit()
        .bitcast(result_type.def(builder.span(), builder), None, composite)
        .unwrap()
}

impl<'a, 'tcx> Builder<'a, 'tcx> {
    fn ordering_to_semantics_def(&self, ordering: AtomicOrdering) -> SpirvValue {
        let mut invalid_seq_cst = false;
        let semantics = match ordering {
            AtomicOrdering::NotAtomic | AtomicOrdering::Unordered | AtomicOrdering::Monotonic => {
                MemorySemantics::NONE
            }
            // Note: rustc currently has AtomicOrdering::Consume commented out, if it ever becomes
            // uncommented, it should be MakeVisible | Acquire.
            AtomicOrdering::Acquire => MemorySemantics::MAKE_VISIBLE | MemorySemantics::ACQUIRE,
            AtomicOrdering::Release => MemorySemantics::MAKE_AVAILABLE | MemorySemantics::RELEASE,
            AtomicOrdering::AcquireRelease => {
                MemorySemantics::MAKE_AVAILABLE
                    | MemorySemantics::MAKE_VISIBLE
                    | MemorySemantics::ACQUIRE_RELEASE
            }
            AtomicOrdering::SequentiallyConsistent => {
                let emit = self.emit();
                let memory_model = emit.module_ref().memory_model.as_ref().unwrap();
                if memory_model.operands[1].unwrap_memory_model() == MemoryModel::Vulkan {
                    invalid_seq_cst = true;
                }
                MemorySemantics::MAKE_AVAILABLE
                    | MemorySemantics::MAKE_VISIBLE
                    | MemorySemantics::SEQUENTIALLY_CONSISTENT
            }
        };
        let semantics = self.constant_u32(self.span(), semantics.bits());
        if invalid_seq_cst {
            self.zombie(
                semantics.def(self),
                "Cannot use AtomicOrdering=SequentiallyConsistent on Vulkan memory model. Check if AcquireRelease fits your needs.",
            );
        }
        semantics
    }

    fn memset_const_pattern(&self, ty: &SpirvType, fill_byte: u8) -> Word {
        match *ty {
            SpirvType::Void => self.fatal("memset invalid on void pattern"),
            SpirvType::Bool => self.fatal("memset invalid on bool pattern"),
            SpirvType::Integer(width, _signedness) => match width {
                8 => self.constant_u8(self.span(), fill_byte).def(self),
                16 => self
                    .constant_u16(self.span(), memset_fill_u16(fill_byte))
                    .def(self),
                32 => self
                    .constant_u32(self.span(), memset_fill_u32(fill_byte))
                    .def(self),
                64 => self
                    .constant_u64(self.span(), memset_fill_u64(fill_byte))
                    .def(self),
                _ => self.fatal(&format!(
                    "memset on integer width {} not implemented yet",
                    width
                )),
            },
            SpirvType::Float(width) => match width {
                32 => self
                    .constant_f32(self.span(), f32::from_bits(memset_fill_u32(fill_byte)))
                    .def(self),
                64 => self
                    .constant_f64(self.span(), f64::from_bits(memset_fill_u64(fill_byte)))
                    .def(self),
                _ => self.fatal(&format!(
                    "memset on float width {} not implemented yet",
                    width
                )),
            },
            SpirvType::Adt { .. } => self.fatal("memset on structs not implemented yet"),
            SpirvType::Vector { element, count } | SpirvType::Matrix { element, count } => {
                let elem_pat = self.memset_const_pattern(&self.lookup_type(element), fill_byte);
                self.constant_composite(
                    ty.clone().def(self.span(), self),
                    iter::repeat(elem_pat).take(count as usize),
                )
                .def(self)
            }
            SpirvType::Array { element, count } => {
                let elem_pat = self.memset_const_pattern(&self.lookup_type(element), fill_byte);
                let count = self.builder.lookup_const_u64(count).unwrap() as usize;
                self.constant_composite(
                    ty.clone().def(self.span(), self),
                    iter::repeat(elem_pat).take(count),
                )
                .def(self)
            }
            SpirvType::RuntimeArray { .. } => {
                self.fatal("memset on runtime arrays not implemented yet")
            }
            SpirvType::Pointer { .. } => self.fatal("memset on pointers not implemented yet"),
            SpirvType::Function { .. } => self.fatal("memset on functions not implemented yet"),
            SpirvType::Image { .. } => self.fatal("cannot memset image"),
            SpirvType::Sampler => self.fatal("cannot memset sampler"),
            SpirvType::SampledImage { .. } => self.fatal("cannot memset sampled image"),
            SpirvType::InterfaceBlock { .. } => self.fatal("cannot memset interface block"),
            SpirvType::AccelerationStructureKhr => {
                self.fatal("cannot memset acceleration structure")
            }
            SpirvType::RayQueryKhr => self.fatal("cannot memset ray query"),
        }
    }

    fn memset_dynamic_pattern(&self, ty: &SpirvType, fill_var: Word) -> Word {
        match *ty {
            SpirvType::Void => self.fatal("memset invalid on void pattern"),
            SpirvType::Bool => self.fatal("memset invalid on bool pattern"),
            SpirvType::Integer(width, _signedness) => match width {
                8 => fill_var,
                16 => memset_dynamic_scalar(self, fill_var, 2, false),
                32 => memset_dynamic_scalar(self, fill_var, 4, false),
                64 => memset_dynamic_scalar(self, fill_var, 8, false),
                _ => self.fatal(&format!(
                    "memset on integer width {} not implemented yet",
                    width
                )),
            },
            SpirvType::Float(width) => match width {
                32 => memset_dynamic_scalar(self, fill_var, 4, true),
                64 => memset_dynamic_scalar(self, fill_var, 8, true),
                _ => self.fatal(&format!(
                    "memset on float width {} not implemented yet",
                    width
                )),
            },
            SpirvType::Adt { .. } => self.fatal("memset on structs not implemented yet"),
            SpirvType::Array { element, count } => {
                let elem_pat = self.memset_dynamic_pattern(&self.lookup_type(element), fill_var);
                let count = self.builder.lookup_const_u64(count).unwrap() as usize;
                self.emit()
                    .composite_construct(
                        ty.clone().def(self.span(), self),
                        None,
                        iter::repeat(elem_pat).take(count),
                    )
                    .unwrap()
            }
            SpirvType::Vector { element, count } | SpirvType::Matrix { element, count } => {
                let elem_pat = self.memset_dynamic_pattern(&self.lookup_type(element), fill_var);
                self.emit()
                    .composite_construct(
                        ty.clone().def(self.span(), self),
                        None,
                        iter::repeat(elem_pat).take(count as usize),
                    )
                    .unwrap()
            }
            SpirvType::RuntimeArray { .. } => {
                self.fatal("memset on runtime arrays not implemented yet")
            }
            SpirvType::Pointer { .. } => self.fatal("memset on pointers not implemented yet"),
            SpirvType::Function { .. } => self.fatal("memset on functions not implemented yet"),
            SpirvType::Image { .. } => self.fatal("cannot memset image"),
            SpirvType::Sampler => self.fatal("cannot memset sampler"),
            SpirvType::SampledImage { .. } => self.fatal("cannot memset sampled image"),
            SpirvType::InterfaceBlock { .. } => self.fatal("cannot memset interface block"),
            SpirvType::AccelerationStructureKhr => {
                self.fatal("cannot memset acceleration structure")
            }
            SpirvType::RayQueryKhr => self.fatal("cannot memset ray query"),
        }
    }

    fn memset_constant_size(&mut self, ptr: SpirvValue, pat: SpirvValue, size_bytes: u64) {
        let size_elem = self
            .lookup_type(pat.ty)
            .sizeof(self)
            .expect("Memset on unsized values not supported");
        let count = size_bytes / size_elem.bytes();
        if count == 1 {
            self.store(pat, ptr, Align::from_bytes(0).unwrap());
        } else {
            for index in 0..count {
                let const_index = self.constant_u32(self.span(), index as u32);
                let gep_ptr = self.gep(pat.ty, ptr, &[const_index]);
                self.store(pat, gep_ptr, Align::from_bytes(0).unwrap());
            }
        }
    }

    // TODO: Test this is correct
    fn memset_dynamic_size(&mut self, ptr: SpirvValue, pat: SpirvValue, size_bytes: SpirvValue) {
        let size_elem = self
            .lookup_type(pat.ty)
            .sizeof(self)
            .expect("Unable to memset a dynamic sized object");
        let size_elem_const = self.constant_int(size_bytes.ty, size_elem.bytes());
        let zero = self.constant_int(size_bytes.ty, 0);
        let one = self.constant_int(size_bytes.ty, 1);
        let zero_align = Align::from_bytes(0).unwrap();

        let mut header = self.build_sibling_block("memset_header");
        let mut body = self.build_sibling_block("memset_body");
        let exit = self.build_sibling_block("memset_exit");

        let count = self.udiv(size_bytes, size_elem_const);
        let index = self.alloca(count.ty, zero_align);
        self.store(zero, index, zero_align);
        self.br(header.llbb());

        let current_index = header.load(count.ty, index, zero_align);
        let cond = header.icmp(IntPredicate::IntULT, current_index, count);
        header.cond_br(cond, body.llbb(), exit.llbb());

        let gep_ptr = body.gep(pat.ty, ptr, &[current_index]);
        body.store(pat, gep_ptr, zero_align);
        let current_index_plus_1 = body.add(current_index, one);
        body.store(current_index_plus_1, index, zero_align);
        body.br(header.llbb());

        *self = exit;
    }

    fn zombie_convert_ptr_to_u(&self, def: Word) {
        self.zombie(def, "Cannot convert pointers to integers");
    }

    fn zombie_convert_u_to_ptr(&self, def: Word) {
        self.zombie(def, "Cannot convert integers to pointers");
    }

    fn zombie_ptr_equal(&self, def: Word, inst: &str) {
        if !self.builder.has_capability(Capability::VariablePointers) {
            self.zombie(
                def,
                &format!("{} without OpCapability VariablePointers", inst),
            );
        }
    }

    /// If possible, return the appropriate `OpAccessChain` indices for going from
    /// a pointer to `ty`, to a pointer to `leaf_ty`, with an added `offset`.
    ///
    /// That is, try to turn `((_: *T) as *u8).add(offset) as *Leaf` into a series
    /// of struct field and array/vector element accesses.
    fn recover_access_chain_from_offset(
        &self,
        mut ty: Word,
        leaf_ty: Word,
        mut offset: Size,
    ) -> Option<Vec<u32>> {
        assert_ne!(ty, leaf_ty);

        // NOTE(eddyb) `ty` and `ty_kind` should be kept in sync.
        let mut ty_kind = self.lookup_type(ty);

        let mut indices = Vec::new();
        loop {
            match ty_kind {
                SpirvType::Adt {
                    field_types,
                    field_offsets,
                    ..
                } => {
                    let (i, field_ty, field_ty_kind, offset_in_field) = field_offsets
                        .iter()
                        .enumerate()
                        .find_map(|(i, &field_offset)| {
                            if field_offset > offset {
                                return None;
                            }

                            // Grab the actual field type to be able to confirm that
                            // the leaf is somewhere inside the field.
                            let field_ty = field_types[i];
                            let field_ty_kind = self.lookup_type(field_ty);

                            let offset_in_field = offset - field_offset;
                            if field_ty_kind
                                .sizeof(self)
                                .map_or(true, |size| offset_in_field < size)
                            {
                                Some((i, field_ty, field_ty_kind, offset_in_field))
                            } else {
                                None
                            }
                        })?;

                    ty = field_ty;
                    ty_kind = field_ty_kind;

                    indices.push(i as u32);
                    offset = offset_in_field;
                }
                SpirvType::Vector { element, .. }
                | SpirvType::Array { element, .. }
                | SpirvType::RuntimeArray { element }
                | SpirvType::Matrix { element, .. } => {
                    ty = element;
                    ty_kind = self.lookup_type(ty);

                    let stride = ty_kind.sizeof(self)?;
                    indices.push((offset.bytes() / stride.bytes()).try_into().ok()?);
                    offset = Size::from_bytes(offset.bytes() % stride.bytes());
                }
                _ => return None,
            }

            if offset == Size::ZERO && ty == leaf_ty {
                return Some(indices);
            }
        }
    }
}

impl<'a, 'tcx> BuilderMethods<'a, 'tcx> for Builder<'a, 'tcx> {
    fn build(cx: &'a Self::CodegenCx, llbb: Self::BasicBlock) -> Self {
        let cursor = cx.builder.select_block_by_id(llbb);
        // FIXME(eddyb) change `Self::Function` to be more like a function index.
        let current_fn = {
            let emit = cx.emit_with_cursor(cursor);
            let selected_function = emit.selected_function().unwrap();
            let selected_function = &emit.module_ref().functions[selected_function];
            let def_inst = selected_function.def.as_ref().unwrap();
            let def = def_inst.result_id.unwrap();
            let ty = def_inst.operands[1].unwrap_id_ref();
            def.with_type(ty)
        };
        Self {
            cx,
            cursor,
            current_fn,
            basic_block: llbb,
            current_span: Default::default(),
        }
    }

    fn cx(&self) -> &Self::CodegenCx {
        self.cx
    }

    fn llbb(&self) -> Self::BasicBlock {
        self.basic_block
    }

    fn set_span(&mut self, span: Span) {
        self.current_span = Some(span);
        let loc = self.cx.tcx.sess.source_map().lookup_char_pos(span.lo());
        let file = self
            .builder
            .def_string(format!("{}", loc.file.name.prefer_remapped()));
        self.emit()
            .line(file, loc.line as u32, loc.col_display as u32);
    }

    // FIXME(eddyb) change `Self::Function` to be more like a function index.
    fn append_block(
        cx: &'a Self::CodegenCx,
        llfn: Self::Function,
        _name: &str,
    ) -> Self::BasicBlock {
        let cursor_fn = cx.builder.select_function_by_id(llfn.def_cx(cx));
        cx.emit_with_cursor(cursor_fn).begin_block(None).unwrap()
    }

    fn append_sibling_block(&mut self, _name: &str) -> Self::BasicBlock {
        self.emit_with_cursor(BuilderCursor {
            function: self.cursor.function,
            block: None,
        })
        .begin_block(None)
        .unwrap()
    }

    fn build_sibling_block(&mut self, _name: &str) -> Self {
        let mut builder = self.emit_with_cursor(BuilderCursor {
            function: self.cursor.function,
            block: None,
        });
        let new_bb = builder.begin_block(None).unwrap();
        let new_cursor = BuilderCursor {
            function: self.cursor.function,
            block: builder.selected_block(),
        };
        Self {
            cx: self.cx,
            cursor: new_cursor,
            current_fn: self.current_fn,
            basic_block: new_bb,
            current_span: Default::default(),
        }
    }

    fn ret_void(&mut self) {
        self.emit().ret().unwrap();
    }

    fn ret(&mut self, value: Self::Value) {
        self.emit().ret_value(value.def(self)).unwrap();
    }

    fn br(&mut self, dest: Self::BasicBlock) {
        self.emit().branch(dest).unwrap();
    }

    fn cond_br(
        &mut self,
        cond: Self::Value,
        then_llbb: Self::BasicBlock,
        else_llbb: Self::BasicBlock,
    ) {
        self.emit()
            .branch_conditional(cond.def(self), then_llbb, else_llbb, empty())
            .unwrap();
    }

    fn switch(
        &mut self,
        v: Self::Value,
        else_llbb: Self::BasicBlock,
        cases: impl ExactSizeIterator<Item = (u128, Self::BasicBlock)>,
    ) {
        fn construct_8(self_: &Builder<'_, '_>, signed: bool, v: u128) -> Operand {
            if v > u8::MAX as u128 {
                self_.fatal(&format!(
                    "Switches to values above u8::MAX not supported: {:?}",
                    v
                ))
            } else if signed {
                // this cast chain can probably be collapsed, but, whatever, be safe
                Operand::LiteralInt32(v as u8 as i8 as i32 as u32)
            } else {
                Operand::LiteralInt32(v as u8 as u32)
            }
        }
        fn construct_16(self_: &Builder<'_, '_>, signed: bool, v: u128) -> Operand {
            if v > u16::MAX as u128 {
                self_.fatal(&format!(
                    "Switches to values above u16::MAX not supported: {:?}",
                    v
                ))
            } else if signed {
                Operand::LiteralInt32(v as u16 as i16 as i32 as u32)
            } else {
                Operand::LiteralInt32(v as u16 as u32)
            }
        }
        fn construct_32(self_: &Builder<'_, '_>, _signed: bool, v: u128) -> Operand {
            if v > u32::MAX as u128 {
                self_.fatal(&format!(
                    "Switches to values above u32::MAX not supported: {:?}",
                    v
                ))
            } else {
                Operand::LiteralInt32(v as u32)
            }
        }
        fn construct_64(self_: &Builder<'_, '_>, _signed: bool, v: u128) -> Operand {
            if v > u64::MAX as u128 {
                self_.fatal(&format!(
                    "Switches to values above u64::MAX not supported: {:?}",
                    v
                ))
            } else {
                Operand::LiteralInt64(v as u64)
            }
        }
        // pass in signed into the closure to be able to unify closure types
        let (signed, construct_case) = match self.lookup_type(v.ty) {
            SpirvType::Integer(width, signed) => {
                let construct_case = match width {
                    8 => construct_8,
                    16 => construct_16,
                    32 => construct_32,
                    64 => construct_64,
                    other => self.fatal(&format!(
                        "switch selector cannot have width {} (only 8, 16, 32, and 64 bits allowed)",
                        other
                    )),
                };
                (signed, construct_case)
            }
            other => self.fatal(&format!(
                "switch selector cannot have non-integer type {}",
                other.debug(v.ty, self)
            )),
        };
        let cases = cases
            .map(|(i, b)| (construct_case(self, signed, i), b))
            .collect::<Vec<_>>();
        self.emit().switch(v.def(self), else_llbb, cases).unwrap();
    }

    fn invoke(
        &mut self,
        llty: Self::Type,
        llfn: Self::Value,
        args: &[Self::Value],
        then: Self::BasicBlock,
        _catch: Self::BasicBlock,
        funclet: Option<&Self::Funclet>,
    ) -> Self::Value {
        // Exceptions don't exist, jump directly to then block
        let result = self.call(llty, llfn, args, funclet);
        self.emit().branch(then).unwrap();
        result
    }

    fn unreachable(&mut self) {
        self.emit().unreachable().unwrap();
    }

    simple_op! {add, i_add}
    simple_op! {fadd, f_add}
    simple_op! {fadd_fast, f_add} // fast=normal
    simple_op! {sub, i_sub}
    simple_op! {fsub, f_sub}
    simple_op! {fsub_fast, f_sub} // fast=normal
    simple_op! {
        mul, i_mul,
        // HACK(eddyb) `rustc_codegen_ssa` relies on `Builder` methods doing
        // on-the-fly constant-folding, for e.g. intrinsics that copy memory.
        fold_const {
            int(a, b) => a * b
        }
    }
    simple_op! {fmul, f_mul}
    simple_op! {fmul_fast, f_mul} // fast=normal
    simple_op! {udiv, u_div}
    // Note: exactudiv is UB when there's a remainder, so it's valid to implement as a normal div.
    // TODO: Can we take advantage of the UB and emit something else?
    simple_op! {exactudiv, u_div}
    simple_op! {sdiv, s_div}
    // Same note and TODO as exactudiv
    simple_op! {exactsdiv, s_div}
    simple_op! {fdiv, f_div}
    simple_op! {fdiv_fast, f_div} // fast=normal
    simple_op! {urem, u_mod}
    simple_op! {srem, s_rem}
    simple_op! {frem, f_rem}
    simple_op! {frem_fast, f_rem} // fast=normal
    simple_op_unchecked_type! {shl, shift_left_logical}
    simple_op_unchecked_type! {lshr, shift_right_logical}
    simple_op_unchecked_type! {ashr, shift_right_arithmetic}
    simple_op! {unchecked_sadd, i_add} // already unchecked by default
    simple_op! {unchecked_uadd, i_add} // already unchecked by default
    simple_op! {unchecked_ssub, i_sub} // already unchecked by default
    simple_op! {unchecked_usub, i_sub} // already unchecked by default
    simple_op! {unchecked_smul, i_mul} // already unchecked by default
    simple_op! {unchecked_umul, i_mul} // already unchecked by default
    simple_uni_op! {neg, s_negate}
    simple_uni_op! {fneg, f_negate}

    fn and(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        assert_ty_eq!(self, lhs.ty, rhs.ty);
        let ty = lhs.ty;
        match self.lookup_type(ty) {
            SpirvType::Integer(_, _) => {
                self.emit()
                    .bitwise_and(ty, None, lhs.def(self), rhs.def(self))
            }
            SpirvType::Bool => self
                .emit()
                .logical_and(ty, None, lhs.def(self), rhs.def(self)),
            o => self.fatal(&format!(
                "and() not implemented for type {}",
                o.debug(ty, self)
            )),
        }
        .unwrap()
        .with_type(ty)
    }
    fn or(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        assert_ty_eq!(self, lhs.ty, rhs.ty);
        let ty = lhs.ty;
        match self.lookup_type(ty) {
            SpirvType::Integer(_, _) => {
                self.emit()
                    .bitwise_or(ty, None, lhs.def(self), rhs.def(self))
            }
            SpirvType::Bool => self
                .emit()
                .logical_or(ty, None, lhs.def(self), rhs.def(self)),
            o => self.fatal(&format!(
                "or() not implemented for type {}",
                o.debug(ty, self)
            )),
        }
        .unwrap()
        .with_type(ty)
    }
    fn xor(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        assert_ty_eq!(self, lhs.ty, rhs.ty);
        let ty = lhs.ty;
        match self.lookup_type(ty) {
            SpirvType::Integer(_, _) => {
                self.emit()
                    .bitwise_xor(ty, None, lhs.def(self), rhs.def(self))
            }
            SpirvType::Bool => {
                self.emit()
                    .logical_not_equal(ty, None, lhs.def(self), rhs.def(self))
            }
            o => self.fatal(&format!(
                "xor() not implemented for type {}",
                o.debug(ty, self)
            )),
        }
        .unwrap()
        .with_type(ty)
    }
    fn not(&mut self, val: Self::Value) -> Self::Value {
        match self.lookup_type(val.ty) {
            SpirvType::Integer(_, _) => self.emit().not(val.ty, None, val.def(self)),
            SpirvType::Bool => {
                let true_ = self.constant_bool(self.span(), true);
                // intel-compute-runtime doesn't like OpLogicalNot
                self.emit()
                    .logical_not_equal(val.ty, None, val.def(self), true_.def(self))
            }
            o => self.fatal(&format!(
                "not() not implemented for type {}",
                o.debug(val.ty, self)
            )),
        }
        .unwrap()
        .with_type(val.ty)
    }

    fn checked_binop(
        &mut self,
        oop: OverflowOp,
        _ty: Ty<'_>,
        lhs: Self::Value,
        rhs: Self::Value,
    ) -> (Self::Value, Self::Value) {
        let fals = self.constant_bool(self.span(), false);
        let result = match oop {
            OverflowOp::Add => (self.add(lhs, rhs), fals),
            OverflowOp::Sub => (self.sub(lhs, rhs), fals),
            OverflowOp::Mul => (self.mul(lhs, rhs), fals),
        };
        self.zombie(
            result.0.def(self),
            match oop {
                OverflowOp::Add => "checked add is not supported yet",
                OverflowOp::Sub => "checked sub is not supported yet",
                OverflowOp::Mul => "checked mul is not supported yet",
            },
        );
        result
    }

    fn from_immediate(&mut self, val: Self::Value) -> Self::Value {
        if self.lookup_type(val.ty) == SpirvType::Bool {
            let i8 = SpirvType::Integer(8, false).def(self.span(), self);
            self.zext(val, i8)
        } else {
            val
        }
    }

    // silly clippy, we can't rename this!
    #[allow(clippy::wrong_self_convention)]
    fn to_immediate_scalar(&mut self, val: Self::Value, scalar: Scalar) -> Self::Value {
        if scalar.is_bool() {
            let bool = SpirvType::Bool.def(self.span(), self);
            return self.trunc(val, bool);
        }
        val
    }

    fn alloca(&mut self, ty: Self::Type, _align: Align) -> Self::Value {
        let ptr_ty = SpirvType::Pointer { pointee: ty }.def(self.span(), self);
        // "All OpVariable instructions in a function must be the first instructions in the first block."
        let mut builder = self.emit();
        builder.select_block(Some(0)).unwrap();
        let index = {
            let block = &builder.module_ref().functions[builder.selected_function().unwrap()]
                .blocks[builder.selected_block().unwrap()];
            block
                .instructions
                .iter()
                .enumerate()
                .find_map(|(index, inst)| {
                    if inst.class.opcode != Op::Variable {
                        Some(InsertPoint::FromBegin(index))
                    } else {
                        None
                    }
                })
                .unwrap_or(InsertPoint::End)
        };
        // TODO: rspirv doesn't have insert_variable function
        let result_id = builder.id();
        let inst = Instruction::new(
            Op::Variable,
            Some(ptr_ty),
            Some(result_id),
            vec![Operand::StorageClass(StorageClass::Function)],
        );
        builder.insert_into_block(index, inst).unwrap();
        result_id.with_type(ptr_ty)
    }

    fn dynamic_alloca(&mut self, ty: Self::Type, align: Align) -> Self::Value {
        let result = self.alloca(ty, align);
        self.err("dynamic alloca is not supported yet");
        result
    }

    fn array_alloca(&mut self, _ty: Self::Type, _len: Self::Value, _align: Align) -> Self::Value {
        self.fatal("array alloca not supported yet")
    }

    fn load(&mut self, ty: Self::Type, ptr: Self::Value, _align: Align) -> Self::Value {
        if let Some(value) = ptr.const_fold_load(self) {
            return value;
        }
        let ty = match self.lookup_type(ptr.ty) {
            SpirvType::Pointer { pointee } => {
                assert_ty_eq!(self, ty, pointee);
                pointee
            }
            ty => self.fatal(&format!(
                "load called on variable that wasn't a pointer: {:?}",
                ty
            )),
        };
        self.emit()
            .load(ty, None, ptr.def(self), None, empty())
            .unwrap()
            .with_type(ty)
    }

    fn volatile_load(&mut self, ty: Self::Type, ptr: Self::Value) -> Self::Value {
        // TODO: Implement this
        let result = self.load(ty, ptr, Align::from_bytes(0).unwrap());
        self.zombie(result.def(self), "volatile load is not supported yet");
        result
    }

    fn atomic_load(
        &mut self,
        ty: Self::Type,
        ptr: Self::Value,
        order: AtomicOrdering,
        _size: Size,
    ) -> Self::Value {
        let ty = match self.lookup_type(ptr.ty) {
            SpirvType::Pointer { pointee } => {
                assert_ty_eq!(self, ty, pointee);
                pointee
            }
            ty => self.fatal(&format!(
                "atomic_load called on variable that wasn't a pointer: {:?}",
                ty
            )),
        };
        // TODO: Default to device scope
        let memory = self.constant_u32(self.span(), Scope::Device as u32);
        let semantics = self.ordering_to_semantics_def(order);
        let result = self
            .emit()
            .atomic_load(
                ty,
                None,
                ptr.def(self),
                memory.def(self),
                semantics.def(self),
            )
            .unwrap()
            .with_type(ty);
        self.validate_atomic(ty, result.def(self));
        result
    }

    fn load_operand(
        &mut self,
        place: PlaceRef<'tcx, Self::Value>,
    ) -> OperandRef<'tcx, Self::Value> {
        if place.layout.is_zst() {
            return OperandRef::new_zst(self, place.layout);
        }

        let val = if let Some(llextra) = place.llextra {
            OperandValue::Ref(place.llval, Some(llextra), place.align)
        } else if self.cx.is_backend_immediate(place.layout) {
            let llval = self.load(
                place.layout.spirv_type(self.span(), self),
                place.llval,
                place.align,
            );
            OperandValue::Immediate(self.to_immediate(llval, place.layout))
        } else if let Abi::ScalarPair(ref a, ref b) = place.layout.abi {
            let b_offset = a.value.size(self).align_to(b.value.align(self).abi);

            let pair_ty = place.layout.spirv_type(self.span(), self);
            let mut load = |i, scalar: &Scalar, align| {
                let llptr = self.struct_gep(pair_ty, place.llval, i as u64);
                let load = self.load(
                    self.scalar_pair_element_backend_type(place.layout, i, false),
                    llptr,
                    align,
                );
                // WARN! This does not go through to_immediate due to only having a Scalar, not a Ty, but it still does
                // whatever to_immediate does!
                if scalar.is_bool() {
                    self.trunc(load, SpirvType::Bool.def(self.span(), self))
                } else {
                    load
                }
            };

            OperandValue::Pair(
                load(0, a, place.align),
                load(1, b, place.align.restrict_for_offset(b_offset)),
            )
        } else {
            OperandValue::Ref(place.llval, None, place.align)
        };
        OperandRef {
            val,
            layout: place.layout,
        }
    }

    /// Called for `Rvalue::Repeat` when the elem is neither a ZST nor optimizable using memset.
    fn write_operand_repeatedly(
        mut self,
        cg_elem: OperandRef<'tcx, Self::Value>,
        count: u64,
        dest: PlaceRef<'tcx, Self::Value>,
    ) -> Self {
        let zero = self.const_usize(0);
        let start = dest.project_index(&mut self, zero).llval;

        let elem_layout = dest.layout.field(self.cx(), 0);
        let elem_ty = elem_layout.spirv_type(self.span(), &self);
        let align = dest.align.restrict_for_offset(elem_layout.size);

        for i in 0..count {
            let current = self.inbounds_gep(elem_ty, start, &[self.const_usize(i)]);
            cg_elem.val.store(
                &mut self,
                PlaceRef::new_sized_aligned(current, cg_elem.layout, align),
            );
        }

        self
    }

    fn range_metadata(&mut self, _load: Self::Value, _range: WrappingRange) {
        // ignore
    }

    fn nonnull_metadata(&mut self, _load: Self::Value) {
        // ignore
    }

    fn store(&mut self, val: Self::Value, ptr: Self::Value, _align: Align) -> Self::Value {
        let ptr_elem_ty = match self.lookup_type(ptr.ty) {
            SpirvType::Pointer { pointee } => pointee,
            ty => self.fatal(&format!(
                "store called on variable that wasn't a pointer: {:?}",
                ty
            )),
        };
        assert_ty_eq!(self, ptr_elem_ty, val.ty);
        self.emit()
            .store(ptr.def(self), val.def(self), None, empty())
            .unwrap();
        val
    }

    fn store_with_flags(
        &mut self,
        val: Self::Value,
        ptr: Self::Value,
        align: Align,
        flags: MemFlags,
    ) -> Self::Value {
        if flags != MemFlags::empty() {
            self.err(&format!(
                "store_with_flags is not supported yet: {:?}",
                flags
            ));
        }
        self.store(val, ptr, align)
    }

    fn atomic_store(
        &mut self,
        val: Self::Value,
        ptr: Self::Value,
        order: AtomicOrdering,
        _size: Size,
    ) {
        let ptr_elem_ty = match self.lookup_type(ptr.ty) {
            SpirvType::Pointer { pointee } => pointee,
            ty => self.fatal(&format!(
                "atomic_store called on variable that wasn't a pointer: {:?}",
                ty
            )),
        };
        assert_ty_eq!(self, ptr_elem_ty, val.ty);
        // TODO: Default to device scope
        let memory = self.constant_u32(self.span(), Scope::Device as u32);
        let semantics = self.ordering_to_semantics_def(order);
        self.validate_atomic(val.ty, ptr.def(self));
        self.emit()
            .atomic_store(
                ptr.def(self),
                memory.def(self),
                semantics.def(self),
                val.def(self),
            )
            .unwrap();
    }

    fn gep(&mut self, ty: Self::Type, ptr: Self::Value, indices: &[Self::Value]) -> Self::Value {
        self.gep_help(ty, ptr, indices, false)
    }

    fn inbounds_gep(
        &mut self,
        ty: Self::Type,
        ptr: Self::Value,
        indices: &[Self::Value],
    ) -> Self::Value {
        self.gep_help(ty, ptr, indices, true)
    }

    fn struct_gep(&mut self, ty: Self::Type, ptr: Self::Value, idx: u64) -> Self::Value {
        let pointee = match self.lookup_type(ptr.ty) {
            SpirvType::Pointer { pointee } => {
                assert_ty_eq!(self, ty, pointee);
                pointee
            }
            other => self.fatal(&format!(
                "struct_gep not on pointer type: {:?}, index {}",
                other, idx
            )),
        };
        let pointee_kind = self.lookup_type(pointee);
        let result_pointee_type = match pointee_kind {
            SpirvType::Adt {
                ref field_types, ..
            } => field_types[idx as usize],
            SpirvType::Array { element, .. }
            | SpirvType::RuntimeArray { element, .. }
            | SpirvType::Vector { element, .. }
            | SpirvType::Matrix { element, .. } => element,
            SpirvType::InterfaceBlock { inner_type } => {
                assert_eq!(idx, 0);
                inner_type
            }
            other => self.fatal(&format!(
                "struct_gep not on struct, array, or vector type: {:?}, index {}",
                other, idx
            )),
        };
        let result_type = SpirvType::Pointer {
            pointee: result_pointee_type,
        }
        .def(self.span(), self);

        // Special-case field accesses through a `pointercast`, to accesss the
        // right field in the original type, for the `Logical` addressing model.
        if let SpirvValueKind::LogicalPtrCast {
            original_ptr,
            original_pointee_ty,
            zombie_target_undef: _,
        } = ptr.kind
        {
            let offset = match pointee_kind {
                SpirvType::Adt { field_offsets, .. } => field_offsets[idx as usize],
                SpirvType::Array { element, .. }
                | SpirvType::RuntimeArray { element, .. }
                | SpirvType::Vector { element, .. }
                | SpirvType::Matrix { element, .. } => {
                    self.lookup_type(element).sizeof(self).unwrap() * idx
                }
                _ => unreachable!(),
            };
            if let Some(indices) = self.recover_access_chain_from_offset(
                original_pointee_ty,
                result_pointee_type,
                offset,
            ) {
                let indices = indices
                    .into_iter()
                    .map(|idx| self.constant_u32(self.span(), idx).def(self))
                    .collect::<Vec<_>>();
                return self
                    .emit()
                    .access_chain(result_type, None, original_ptr, indices)
                    .unwrap()
                    .with_type(result_type);
            }
        }

        // Important! LLVM, and therefore intel-compute-runtime, require the `getelementptr` instruction (and therefore
        // OpAccessChain) on structs to be a constant i32. Not i64! i32.
        if idx > u32::MAX as u64 {
            self.fatal("struct_gep bigger than u32::MAX");
        }
        let index_const = self.constant_u32(self.span(), idx as u32).def(self);
        self.emit()
            .access_chain(
                result_type,
                None,
                ptr.def(self),
                [index_const].iter().cloned(),
            )
            .unwrap()
            .with_type(result_type)
    }

    // intcast has the logic for dealing with bools, so use that
    fn trunc(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        self.intcast(val, dest_ty, false)
    }
    fn sext(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        self.intcast(val, dest_ty, true)
    }
    fn fptoui_sat(&mut self, _val: Self::Value, _dest_ty: Self::Type) -> Option<Self::Value> {
        None
    }

    fn fptosi_sat(&mut self, _val: Self::Value, _dest_ty: Self::Type) -> Option<Self::Value> {
        None
    }

    fn fptoui(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        if val.ty == dest_ty {
            val
        } else {
            self.emit()
                .convert_f_to_u(dest_ty, None, val.def(self))
                .unwrap()
                .with_type(dest_ty)
        }
    }

    fn fptosi(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        if val.ty == dest_ty {
            val
        } else {
            self.emit()
                .convert_f_to_s(dest_ty, None, val.def(self))
                .unwrap()
                .with_type(dest_ty)
        }
    }

    fn uitofp(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        if val.ty == dest_ty {
            val
        } else {
            self.emit()
                .convert_u_to_f(dest_ty, None, val.def(self))
                .unwrap()
                .with_type(dest_ty)
        }
    }

    fn sitofp(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        if val.ty == dest_ty {
            val
        } else {
            self.emit()
                .convert_s_to_f(dest_ty, None, val.def(self))
                .unwrap()
                .with_type(dest_ty)
        }
    }

    fn fptrunc(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        if val.ty == dest_ty {
            val
        } else {
            self.emit()
                .f_convert(dest_ty, None, val.def(self))
                .unwrap()
                .with_type(dest_ty)
        }
    }

    fn fpext(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        if val.ty == dest_ty {
            val
        } else {
            self.emit()
                .f_convert(dest_ty, None, val.def(self))
                .unwrap()
                .with_type(dest_ty)
        }
    }

    fn ptrtoint(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        match self.lookup_type(val.ty) {
            SpirvType::Pointer { .. } => (),
            other => self.fatal(&format!(
                "ptrtoint called on non-pointer source type: {:?}",
                other
            )),
        }
        if val.ty == dest_ty {
            val
        } else {
            let result = self
                .emit()
                .convert_ptr_to_u(dest_ty, None, val.def(self))
                .unwrap()
                .with_type(dest_ty);
            self.zombie_convert_ptr_to_u(result.def(self));
            result
        }
    }

    fn inttoptr(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        match self.lookup_type(dest_ty) {
            SpirvType::Pointer { .. } => (),
            other => self.fatal(&format!(
                "inttoptr called on non-pointer dest type: {:?}",
                other
            )),
        }
        if val.ty == dest_ty {
            val
        } else {
            let result = self
                .emit()
                .convert_u_to_ptr(dest_ty, None, val.def(self))
                .unwrap()
                .with_type(dest_ty);
            self.zombie_convert_u_to_ptr(result.def(self));
            result
        }
    }

    fn bitcast(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        if val.ty == dest_ty {
            val
        } else {
            let val_is_ptr = matches!(self.lookup_type(val.ty), SpirvType::Pointer { .. });
            let dest_is_ptr = matches!(self.lookup_type(dest_ty), SpirvType::Pointer { .. });

            // Reuse the pointer-specific logic in `pointercast` for `*T -> *U`.
            if val_is_ptr && dest_is_ptr {
                return self.pointercast(val, dest_ty);
            }

            let result = self
                .emit()
                .bitcast(dest_ty, None, val.def(self))
                .unwrap()
                .with_type(dest_ty);

            if val_is_ptr || dest_is_ptr {
                if self.is_system_crate() {
                    self.zombie(
                        result.def(self),
                        &format!(
                            "Cannot cast between pointer and non-pointer types. From: {}. To: {}.",
                            self.debug_type(val.ty),
                            self.debug_type(dest_ty)
                        ),
                    );
                } else {
                    self.struct_err("Cannot cast between pointer and non-pointer types")
                        .note(&format!("from: {}", self.debug_type(val.ty)))
                        .note(&format!("to: {}", self.debug_type(dest_ty)))
                        .emit();
                }
            }

            result
        }
    }

    fn intcast(&mut self, val: Self::Value, dest_ty: Self::Type, is_signed: bool) -> Self::Value {
        if val.ty == dest_ty {
            // I guess?
            return val;
        }
        match (self.lookup_type(val.ty), self.lookup_type(dest_ty)) {
            // sign change
            (
                SpirvType::Integer(val_width, val_signedness),
                SpirvType::Integer(dest_width, dest_signedness),
            ) if val_width == dest_width && val_signedness != dest_signedness => self
                .emit()
                .bitcast(dest_ty, None, val.def(self))
                .unwrap()
                .with_type(dest_ty),
            // width change, and optional sign change
            (SpirvType::Integer(_, _), SpirvType::Integer(_, dest_signedness)) => {
                // spir-v spec doesn't seem to say that signedness needs to match the operands, only that the signedness
                // of the destination type must match the instruction's signedness.
                if dest_signedness {
                    self.emit().s_convert(dest_ty, None, val.def(self))
                } else {
                    self.emit().u_convert(dest_ty, None, val.def(self))
                }
                .unwrap()
                .with_type(dest_ty)
            }
            // bools are ints in llvm, so we have to implement this here
            (SpirvType::Bool, SpirvType::Integer(_, _)) => {
                // spir-v doesn't have a direct conversion instruction
                let if_true = self.constant_int(dest_ty, 1);
                let if_false = self.constant_int(dest_ty, 0);
                self.emit()
                    .select(
                        dest_ty,
                        None,
                        val.def(self),
                        if_true.def(self),
                        if_false.def(self),
                    )
                    .unwrap()
                    .with_type(dest_ty)
            }
            (SpirvType::Integer(_, _), SpirvType::Bool) => {
                // spir-v doesn't have a direct conversion instruction, glslang emits OpINotEqual
                let zero = self.constant_int(val.ty, 0);
                self.emit()
                    .i_not_equal(dest_ty, None, val.def(self), zero.def(self))
                    .unwrap()
                    .with_type(dest_ty)
            }
            (val_ty, dest_ty_spv) => self.fatal(&format!(
                "TODO: intcast not implemented yet: val={:?} val.ty={:?} dest_ty={:?} is_signed={}",
                val, val_ty, dest_ty_spv, is_signed
            )),
        }
    }

    fn pointercast(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        let (val, val_pointee) = match val.kind {
            // Strip a previous `pointercast`, to reveal the original pointer type.
            SpirvValueKind::LogicalPtrCast {
                original_ptr,
                original_pointee_ty,
                zombie_target_undef: _,
            } => (
                original_ptr.with_type(
                    SpirvType::Pointer {
                        pointee: original_pointee_ty,
                    }
                    .def(self.span(), self),
                ),
                original_pointee_ty,
            ),

            _ => match self.lookup_type(val.ty) {
                SpirvType::Pointer { pointee } => (val, pointee),
                other => self.fatal(&format!(
                    "pointercast called on non-pointer source type: {:?}",
                    other
                )),
            },
        };
        let dest_pointee = match self.lookup_type(dest_ty) {
            SpirvType::Pointer { pointee } => pointee,
            other => self.fatal(&format!(
                "pointercast called on non-pointer dest type: {:?}",
                other
            )),
        };
        if val.ty == dest_ty {
            val
        } else if let Some(indices) =
            self.recover_access_chain_from_offset(val_pointee, dest_pointee, Size::ZERO)
        {
            let indices = indices
                .into_iter()
                .map(|idx| self.constant_u32(self.span(), idx).def(self))
                .collect::<Vec<_>>();
            self.emit()
                .access_chain(dest_ty, None, val.def(self), indices)
                .unwrap()
                .with_type(dest_ty)
        } else {
            // Defer the cast so that it has a chance to be avoided.
            SpirvValue {
                kind: SpirvValueKind::LogicalPtrCast {
                    original_ptr: val.def(self),
                    original_pointee_ty: val_pointee,
                    zombie_target_undef: self.undef(dest_ty).def(self),
                },
                ty: dest_ty,
            }
        }
    }

    fn icmp(&mut self, op: IntPredicate, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        // Note: the signedness of the opcode doesn't have to match the signedness of the operands.
        use IntPredicate::*;
        assert_ty_eq!(self, lhs.ty, rhs.ty);
        let b = SpirvType::Bool.def(self.span(), self);
        match self.lookup_type(lhs.ty) {
            SpirvType::Integer(_, _) => match op {
                IntEQ => self.emit().i_equal(b, None, lhs.def(self), rhs.def(self)),
                IntNE => self
                    .emit()
                    .i_not_equal(b, None, lhs.def(self), rhs.def(self)),
                IntUGT => self
                    .emit()
                    .u_greater_than(b, None, lhs.def(self), rhs.def(self)),
                IntUGE => self
                    .emit()
                    .u_greater_than_equal(b, None, lhs.def(self), rhs.def(self)),
                IntULT => self
                    .emit()
                    .u_less_than(b, None, lhs.def(self), rhs.def(self)),
                IntULE => self
                    .emit()
                    .u_less_than_equal(b, None, lhs.def(self), rhs.def(self)),
                IntSGT => self
                    .emit()
                    .s_greater_than(b, None, lhs.def(self), rhs.def(self)),
                IntSGE => self
                    .emit()
                    .s_greater_than_equal(b, None, lhs.def(self), rhs.def(self)),
                IntSLT => self
                    .emit()
                    .s_less_than(b, None, lhs.def(self), rhs.def(self)),
                IntSLE => self
                    .emit()
                    .s_less_than_equal(b, None, lhs.def(self), rhs.def(self)),
            },
            SpirvType::Pointer { .. } => match op {
                IntEQ => {
                    if self.emit().version().unwrap() > (1, 3) {
                        let ptr_equal =
                            self.emit().ptr_equal(b, None, lhs.def(self), rhs.def(self));

                        ptr_equal.map(|result| {
                            self.zombie_ptr_equal(result, "OpPtrEqual");
                            result
                        })
                    } else {
                        let int_ty = self.type_usize();
                        let lhs = self
                            .emit()
                            .convert_ptr_to_u(int_ty, None, lhs.def(self))
                            .unwrap();
                        self.zombie_convert_ptr_to_u(lhs);
                        let rhs = self
                            .emit()
                            .convert_ptr_to_u(int_ty, None, rhs.def(self))
                            .unwrap();
                        self.zombie_convert_ptr_to_u(rhs);
                        self.emit().i_not_equal(b, None, lhs, rhs)
                    }
                }
                IntNE => {
                    if self.emit().version().unwrap() > (1, 3) {
                        self.emit()
                            .ptr_not_equal(b, None, lhs.def(self), rhs.def(self))
                            .map(|result| {
                                self.zombie_ptr_equal(result, "OpPtrNotEqual");
                                result
                            })
                    } else {
                        let int_ty = self.type_usize();
                        let lhs = self
                            .emit()
                            .convert_ptr_to_u(int_ty, None, lhs.def(self))
                            .unwrap();
                        self.zombie_convert_ptr_to_u(lhs);
                        let rhs = self
                            .emit()
                            .convert_ptr_to_u(int_ty, None, rhs.def(self))
                            .unwrap();
                        self.zombie_convert_ptr_to_u(rhs);
                        self.emit().i_not_equal(b, None, lhs, rhs)
                    }
                }
                IntUGT => {
                    let int_ty = self.type_usize();
                    let lhs = self
                        .emit()
                        .convert_ptr_to_u(int_ty, None, lhs.def(self))
                        .unwrap();
                    self.zombie_convert_ptr_to_u(lhs);
                    let rhs = self
                        .emit()
                        .convert_ptr_to_u(int_ty, None, rhs.def(self))
                        .unwrap();
                    self.zombie_convert_ptr_to_u(rhs);
                    self.emit().u_greater_than(b, None, lhs, rhs)
                }
                IntUGE => {
                    let int_ty = self.type_usize();
                    let lhs = self
                        .emit()
                        .convert_ptr_to_u(int_ty, None, lhs.def(self))
                        .unwrap();
                    self.zombie_convert_ptr_to_u(lhs);
                    let rhs = self
                        .emit()
                        .convert_ptr_to_u(int_ty, None, rhs.def(self))
                        .unwrap();
                    self.zombie_convert_ptr_to_u(rhs);
                    self.emit().u_greater_than_equal(b, None, lhs, rhs)
                }
                IntULT => {
                    let int_ty = self.type_usize();
                    let lhs = self
                        .emit()
                        .convert_ptr_to_u(int_ty, None, lhs.def(self))
                        .unwrap();
                    self.zombie_convert_ptr_to_u(lhs);
                    let rhs = self
                        .emit()
                        .convert_ptr_to_u(int_ty, None, rhs.def(self))
                        .unwrap();
                    self.zombie_convert_ptr_to_u(rhs);
                    self.emit().u_less_than(b, None, lhs, rhs)
                }
                IntULE => {
                    let int_ty = self.type_usize();
                    let lhs = self
                        .emit()
                        .convert_ptr_to_u(int_ty, None, lhs.def(self))
                        .unwrap();
                    self.zombie_convert_ptr_to_u(lhs);
                    let rhs = self
                        .emit()
                        .convert_ptr_to_u(int_ty, None, rhs.def(self))
                        .unwrap();
                    self.zombie_convert_ptr_to_u(rhs);
                    self.emit().u_less_than_equal(b, None, lhs, rhs)
                }
                IntSGT => self.fatal("TODO: pointer operator IntSGT not implemented yet"),
                IntSGE => self.fatal("TODO: pointer operator IntSGE not implemented yet"),
                IntSLT => self.fatal("TODO: pointer operator IntSLT not implemented yet"),
                IntSLE => self.fatal("TODO: pointer operator IntSLE not implemented yet"),
            },
            SpirvType::Bool => match op {
                IntEQ => self
                    .emit()
                    .logical_equal(b, None, lhs.def(self), rhs.def(self)),
                IntNE => self
                    .emit()
                    .logical_not_equal(b, None, lhs.def(self), rhs.def(self)),
                // x > y  =>  x && !y
                IntUGT => {
                    // intel-compute-runtime doesn't like OpLogicalNot
                    let true_ = self.constant_bool(self.span(), true);
                    let rhs = self
                        .emit()
                        .logical_not_equal(b, None, rhs.def(self), true_.def(self))
                        .unwrap();
                    self.emit().logical_and(b, None, lhs.def(self), rhs)
                }
                // x >= y  =>  x || !y
                IntUGE => {
                    let true_ = self.constant_bool(self.span(), true);
                    let rhs = self
                        .emit()
                        .logical_not_equal(b, None, rhs.def(self), true_.def(self))
                        .unwrap();
                    self.emit().logical_or(b, None, lhs.def(self), rhs)
                }
                // x < y  =>  !x && y
                IntULE => {
                    let true_ = self.constant_bool(self.span(), true);
                    let lhs = self
                        .emit()
                        .logical_not_equal(b, None, lhs.def(self), true_.def(self))
                        .unwrap();
                    self.emit().logical_and(b, None, lhs, rhs.def(self))
                }
                // x <= y  =>  !x || y
                IntULT => {
                    let true_ = self.constant_bool(self.span(), true);
                    let lhs = self
                        .emit()
                        .logical_not_equal(b, None, lhs.def(self), true_.def(self))
                        .unwrap();
                    self.emit().logical_or(b, None, lhs, rhs.def(self))
                }
                IntSGT => self.fatal("TODO: boolean operator IntSGT not implemented yet"),
                IntSGE => self.fatal("TODO: boolean operator IntSGE not implemented yet"),
                IntSLT => self.fatal("TODO: boolean operator IntSLT not implemented yet"),
                IntSLE => self.fatal("TODO: boolean operator IntSLE not implemented yet"),
            },
            other => self.fatal(&format!(
                "Int comparison not implemented on {}",
                other.debug(lhs.ty, self)
            )),
        }
        .unwrap()
        .with_type(b)
    }

    fn fcmp(&mut self, op: RealPredicate, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        use RealPredicate::*;
        assert_ty_eq!(self, lhs.ty, rhs.ty);
        let b = SpirvType::Bool.def(self.span(), self);
        match op {
            RealPredicateFalse => return self.cx.constant_bool(self.span(), false),
            RealPredicateTrue => return self.cx.constant_bool(self.span(), true),
            RealOEQ => self
                .emit()
                .f_ord_equal(b, None, lhs.def(self), rhs.def(self)),
            RealOGT => self
                .emit()
                .f_ord_greater_than(b, None, lhs.def(self), rhs.def(self)),
            RealOGE => self
                .emit()
                .f_ord_greater_than_equal(b, None, lhs.def(self), rhs.def(self)),
            RealOLT => self
                .emit()
                .f_ord_less_than(b, None, lhs.def(self), rhs.def(self)),
            RealOLE => self
                .emit()
                .f_ord_less_than_equal(b, None, lhs.def(self), rhs.def(self)),
            RealONE => self
                .emit()
                .f_ord_not_equal(b, None, lhs.def(self), rhs.def(self)),
            RealORD => self.emit().ordered(b, None, lhs.def(self), rhs.def(self)),
            RealUNO => self.emit().unordered(b, None, lhs.def(self), rhs.def(self)),
            RealUEQ => self
                .emit()
                .f_unord_equal(b, None, lhs.def(self), rhs.def(self)),
            RealUGT => self
                .emit()
                .f_unord_greater_than(b, None, lhs.def(self), rhs.def(self)),
            RealUGE => {
                self.emit()
                    .f_unord_greater_than_equal(b, None, lhs.def(self), rhs.def(self))
            }
            RealULT => self
                .emit()
                .f_unord_less_than(b, None, lhs.def(self), rhs.def(self)),
            RealULE => self
                .emit()
                .f_unord_less_than_equal(b, None, lhs.def(self), rhs.def(self)),
            RealUNE => self
                .emit()
                .f_unord_not_equal(b, None, lhs.def(self), rhs.def(self)),
        }
        .unwrap()
        .with_type(b)
    }

    fn memcpy(
        &mut self,
        dst: Self::Value,
        _dst_align: Align,
        src: Self::Value,
        _src_align: Align,
        size: Self::Value,
        flags: MemFlags,
    ) {
        if flags != MemFlags::empty() {
            self.err(&format!(
                "memcpy with mem flags is not supported yet: {:?}",
                flags
            ));
        }
        let const_size = self.builder.lookup_const_u64(size);
        if const_size == Some(0) {
            // Nothing to do!
            return;
        }
        let src_pointee = match self.lookup_type(src.ty) {
            SpirvType::Pointer { pointee } => Some(pointee),
            _ => None,
        };
        let src_element_size = src_pointee.and_then(|p| self.lookup_type(p).sizeof(self));
        if src_element_size.is_some() && src_element_size == const_size.map(Size::from_bytes) {
            if let Some(const_value) = src.const_fold_load(self) {
                self.store(const_value, dst, Align::from_bytes(0).unwrap());
            } else {
                self.emit()
                    .copy_memory(dst.def(self), src.def(self), None, None, empty())
                    .unwrap();
            }
        } else {
            self.emit()
                .copy_memory_sized(
                    dst.def(self),
                    src.def(self),
                    size.def(self),
                    None,
                    None,
                    empty(),
                )
                .unwrap();
            self.zombie(dst.def(self), "Cannot memcpy dynamically sized data");
        }
    }

    fn memmove(
        &mut self,
        dst: Self::Value,
        dst_align: Align,
        src: Self::Value,
        src_align: Align,
        size: Self::Value,
        flags: MemFlags,
    ) {
        self.memcpy(dst, dst_align, src, src_align, size, flags);
    }

    fn memset(
        &mut self,
        ptr: Self::Value,
        fill_byte: Self::Value,
        size: Self::Value,
        _align: Align,
        flags: MemFlags,
    ) {
        if flags != MemFlags::empty() {
            self.err(&format!(
                "memset with mem flags is not supported yet: {:?}",
                flags
            ));
        }
        let elem_ty = match self.lookup_type(ptr.ty) {
            SpirvType::Pointer { pointee } => pointee,
            _ => self.fatal(&format!(
                "memset called on non-pointer type: {}",
                self.debug_type(ptr.ty)
            )),
        };
        let elem_ty_spv = self.lookup_type(elem_ty);
        let pat = match self.builder.lookup_const_u64(fill_byte) {
            Some(fill_byte) => self.memset_const_pattern(&elem_ty_spv, fill_byte as u8),
            None => self.memset_dynamic_pattern(&elem_ty_spv, fill_byte.def(self)),
        }
        .with_type(elem_ty);
        match self.builder.lookup_const_u64(size) {
            Some(size) => self.memset_constant_size(ptr, pat, size),
            None => self.memset_dynamic_size(ptr, pat, size),
        }
    }

    fn select(
        &mut self,
        cond: Self::Value,
        then_val: Self::Value,
        else_val: Self::Value,
    ) -> Self::Value {
        assert_ty_eq!(self, then_val.ty, else_val.ty);
        let result_type = then_val.ty;
        self.emit()
            .select(
                result_type,
                None,
                cond.def(self),
                then_val.def(self),
                else_val.def(self),
            )
            .unwrap()
            .with_type(result_type)
    }

    fn va_arg(&mut self, _list: Self::Value, _ty: Self::Type) -> Self::Value {
        todo!()
    }

    fn extract_element(&mut self, vec: Self::Value, idx: Self::Value) -> Self::Value {
        let result_type = match self.lookup_type(vec.ty) {
            SpirvType::Vector { element, .. } => element,
            other => self.fatal(&format!(
                "extract_element not implemented on type {:?}",
                other
            )),
        };
        match self.builder.lookup_const_u64(idx) {
            Some(const_index) => self.emit().composite_extract(
                result_type,
                None,
                vec.def(self),
                [const_index as u32].iter().cloned(),
            ),
            None => {
                self.emit()
                    .vector_extract_dynamic(result_type, None, vec.def(self), idx.def(self))
            }
        }
        .unwrap()
        .with_type(result_type)
    }

    fn vector_splat(&mut self, num_elts: usize, elt: Self::Value) -> Self::Value {
        let result_type = SpirvType::Vector {
            element: elt.ty,
            count: num_elts as u32,
        }
        .def(self.span(), self);
        if self.builder.lookup_const(elt).is_some() {
            self.constant_composite(result_type, iter::repeat(elt.def(self)).take(num_elts))
        } else {
            self.emit()
                .composite_construct(
                    result_type,
                    None,
                    iter::repeat(elt.def(self)).take(num_elts),
                )
                .unwrap()
                .with_type(result_type)
        }
    }

    fn extract_value(&mut self, agg_val: Self::Value, idx: u64) -> Self::Value {
        let result_type = match self.lookup_type(agg_val.ty) {
            SpirvType::Adt { field_types, .. } => field_types[idx as usize],
            SpirvType::Array { element, .. }
            | SpirvType::Vector { element, .. }
            | SpirvType::Matrix { element, .. } => element,
            other => self.fatal(&format!(
                "extract_value not implemented on type {}",
                other.debug(agg_val.ty, self)
            )),
        };
        self.emit()
            .composite_extract(
                result_type,
                None,
                agg_val.def(self),
                [idx as u32].iter().cloned(),
            )
            .unwrap()
            .with_type(result_type)
    }

    fn insert_value(&mut self, agg_val: Self::Value, elt: Self::Value, idx: u64) -> Self::Value {
        match self.lookup_type(agg_val.ty) {
            SpirvType::Adt { field_types, .. } => {
                assert_ty_eq!(self, field_types[idx as usize], elt.ty);
            }
            other => self.fatal(&format!("insert_value not implemented on type {:?}", other)),
        };
        self.emit()
            .composite_insert(
                agg_val.ty,
                None,
                elt.def(self),
                agg_val.def(self),
                [idx as u32].iter().cloned(),
            )
            .unwrap()
            .with_type(agg_val.ty)
    }

    fn landing_pad(
        &mut self,
        _ty: Self::Type,
        _pers_fn: Self::Value,
        _num_clauses: usize,
    ) -> Self::Value {
        todo!()
    }

    fn set_cleanup(&mut self, _landing_pad: Self::Value) {
        todo!()
    }

    fn resume(&mut self, _exn: Self::Value) -> Self::Value {
        todo!()
    }

    fn cleanup_pad(
        &mut self,
        _parent: Option<Self::Value>,
        _args: &[Self::Value],
    ) -> Self::Funclet {
        todo!()
    }

    fn cleanup_ret(
        &mut self,
        _funclet: &Self::Funclet,
        _unwind: Option<Self::BasicBlock>,
    ) -> Self::Value {
        todo!()
    }

    fn catch_pad(&mut self, _parent: Self::Value, _args: &[Self::Value]) -> Self::Funclet {
        todo!()
    }

    fn catch_switch(
        &mut self,
        _parent: Option<Self::Value>,
        _unwind: Option<Self::BasicBlock>,
        _num_handlers: usize,
    ) -> Self::Value {
        todo!()
    }

    fn add_handler(&mut self, _catch_switch: Self::Value, _handler: Self::BasicBlock) {
        todo!()
    }

    fn set_personality_fn(&mut self, _personality: Self::Value) {
        todo!()
    }

    fn atomic_cmpxchg(
        &mut self,
        dst: Self::Value,
        cmp: Self::Value,
        src: Self::Value,
        order: AtomicOrdering,
        failure_order: AtomicOrdering,
        _weak: bool,
    ) -> Self::Value {
        let dst_pointee_ty = match self.lookup_type(dst.ty) {
            SpirvType::Pointer { pointee } => pointee,
            ty => self.fatal(&format!(
                "atomic_cmpxchg called on variable that wasn't a pointer: {:?}",
                ty
            )),
        };
        assert_ty_eq!(self, dst_pointee_ty, cmp.ty);
        assert_ty_eq!(self, dst_pointee_ty, src.ty);
        self.validate_atomic(dst_pointee_ty, dst.def(self));
        // TODO: Default to device scope
        let memory = self.constant_u32(self.span(), Scope::Device as u32);
        let semantics_equal = self.ordering_to_semantics_def(order);
        let semantics_unequal = self.ordering_to_semantics_def(failure_order);
        // Note: OpAtomicCompareExchangeWeak is deprecated, and has the same semantics
        self.emit()
            .atomic_compare_exchange(
                src.ty,
                None,
                dst.def(self),
                memory.def(self),
                semantics_equal.def(self),
                semantics_unequal.def(self),
                src.def(self),
                cmp.def(self),
            )
            .unwrap()
            .with_type(src.ty)
    }

    fn atomic_rmw(
        &mut self,
        op: AtomicRmwBinOp,
        dst: Self::Value,
        src: Self::Value,
        order: AtomicOrdering,
    ) -> Self::Value {
        let dst_pointee_ty = match self.lookup_type(dst.ty) {
            SpirvType::Pointer { pointee } => pointee,
            ty => self.fatal(&format!(
                "atomic_rmw called on variable that wasn't a pointer: {:?}",
                ty
            )),
        };
        assert_ty_eq!(self, dst_pointee_ty, src.ty);
        self.validate_atomic(dst_pointee_ty, dst.def(self));
        // TODO: Default to device scope
        let memory = self
            .constant_u32(self.span(), Scope::Device as u32)
            .def(self);
        let semantics = self.ordering_to_semantics_def(order).def(self);
        let mut emit = self.emit();
        use AtomicRmwBinOp::*;
        match op {
            AtomicXchg => emit.atomic_exchange(
                src.ty,
                None,
                dst.def(self),
                memory,
                semantics,
                src.def(self),
            ),
            AtomicAdd => emit.atomic_i_add(
                src.ty,
                None,
                dst.def(self),
                memory,
                semantics,
                src.def(self),
            ),
            AtomicSub => emit.atomic_i_sub(
                src.ty,
                None,
                dst.def(self),
                memory,
                semantics,
                src.def(self),
            ),
            AtomicAnd => emit.atomic_and(
                src.ty,
                None,
                dst.def(self),
                memory,
                semantics,
                src.def(self),
            ),
            AtomicNand => self.fatal("atomic nand is not supported"),
            AtomicOr => emit.atomic_or(
                src.ty,
                None,
                dst.def(self),
                memory,
                semantics,
                src.def(self),
            ),
            AtomicXor => emit.atomic_xor(
                src.ty,
                None,
                dst.def(self),
                memory,
                semantics,
                src.def(self),
            ),
            AtomicMax => emit.atomic_s_max(
                src.ty,
                None,
                dst.def(self),
                memory,
                semantics,
                src.def(self),
            ),
            AtomicMin => emit.atomic_s_min(
                src.ty,
                None,
                dst.def(self),
                memory,
                semantics,
                src.def(self),
            ),
            AtomicUMax => emit.atomic_u_max(
                src.ty,
                None,
                dst.def(self),
                memory,
                semantics,
                src.def(self),
            ),
            AtomicUMin => emit.atomic_u_min(
                src.ty,
                None,
                dst.def(self),
                memory,
                semantics,
                src.def(self),
            ),
        }
        .unwrap()
        .with_type(src.ty)
    }

    fn atomic_fence(&mut self, order: AtomicOrdering, _scope: SynchronizationScope) {
        // Ignore sync scope (it only has "single thread" and "cross thread")
        // TODO: Default to device scope
        let memory = self
            .constant_u32(self.span(), Scope::Device as u32)
            .def(self);
        let semantics = self.ordering_to_semantics_def(order).def(self);
        self.emit().memory_barrier(memory, semantics).unwrap();
    }

    fn set_invariant_load(&mut self, _load: Self::Value) {
        // ignore
    }

    /// Called for `StorageLive`
    fn lifetime_start(&mut self, _ptr: Self::Value, _size: Size) {
        // ignore
    }

    /// Called for `StorageDead`
    fn lifetime_end(&mut self, _ptr: Self::Value, _size: Size) {
        // ignore
    }

    fn instrprof_increment(
        &mut self,
        _fn_name: Self::Value,
        _hash: Self::Value,
        _num_counters: Self::Value,
        _index: Self::Value,
    ) {
        todo!()
    }

    fn call(
        &mut self,
        callee_ty: Self::Type,
        callee: Self::Value,
        args: &[Self::Value],
        funclet: Option<&Self::Funclet>,
    ) -> Self::Value {
        if funclet.is_some() {
            self.fatal("TODO: Funclets are not supported");
        }

        // NOTE(eddyb) see the comment on `SpirvValueKind::FnAddr`, this should
        // be fixed upstream, so we never see any "function pointer" values being
        // created just to perform direct calls.
        let (callee_val, result_type, argument_types) = match self.lookup_type(callee.ty) {
            // HACK(eddyb) this seems to be needed, but it's not what `get_fn_addr`
            // produces, are these coming from inside `rustc_codegen_spirv`?
            SpirvType::Function {
                return_type,
                arguments,
            } => {
                assert_ty_eq!(self, callee_ty, callee.ty);
                (callee.def(self), return_type, arguments)
            }

            SpirvType::Pointer { pointee } => match self.lookup_type(pointee) {
                SpirvType::Function {
                    return_type,
                    arguments,
                } => (
                    match callee.kind {
                        SpirvValueKind::FnAddr { function } => {
                            assert_ty_eq!(self, callee_ty, pointee);
                            function
                        }

                        // Truly indirect call.
                        _ => {
                            let fn_ptr_val = callee.def(self);
                            self.zombie(fn_ptr_val, "indirect calls are not supported in SPIR-V");
                            fn_ptr_val
                        }
                    },
                    return_type,
                    arguments,
                ),
                _ => bug!(
                    "call expected `fn` pointer to point to function type, got `{}`",
                    self.debug_type(pointee)
                ),
            },

            _ => bug!(
                "call expected function or `fn` pointer type, got `{}`",
                self.debug_type(callee.ty)
            ),
        };

        for (argument, argument_type) in args.iter().zip(argument_types) {
            assert_ty_eq!(self, argument.ty, argument_type);
        }
        let libm_intrinsic = self.libm_intrinsics.borrow().get(&callee_val).cloned();
        if let Some(libm_intrinsic) = libm_intrinsic {
            let result = self.call_libm_intrinsic(libm_intrinsic, result_type, args);
            if result_type != result.ty {
                bug!(
                    "Mismatched libm result type for {:?}: expected {}, got {}",
                    libm_intrinsic,
                    self.debug_type(result_type),
                    self.debug_type(result.ty),
                );
            }
            result
        } else if [self.panic_fn_id.get(), self.panic_bounds_check_fn_id.get()]
            .contains(&Some(callee_val))
        {
            // HACK(eddyb) redirect builtin panic calls to an abort, to avoid
            // needing to materialize `&core::panic::Location` or `format_args!`.
            self.abort();
            self.undef(result_type)
        } else if self
            .buffer_load_intrinsic_fn_id
            .borrow()
            .contains(&callee_val)
        {
            self.codegen_buffer_load_intrinsic(result_type, args)
        } else if self
            .buffer_store_intrinsic_fn_id
            .borrow()
            .contains(&callee_val)
        {
            self.codegen_buffer_store_intrinsic(args);

            let void_ty = SpirvType::Void.def(rustc_span::DUMMY_SP, self);
            SpirvValue {
                kind: SpirvValueKind::IllegalTypeUsed(void_ty),
                ty: void_ty,
            }
        } else {
            let args = args.iter().map(|arg| arg.def(self)).collect::<Vec<_>>();
            self.emit()
                .function_call(result_type, None, callee_val, args)
                .unwrap()
                .with_type(result_type)
        }
    }

    fn zext(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        self.intcast(val, dest_ty, false)
    }

    fn do_not_inline(&mut self, _llret: Self::Value) {
        // Ignore
    }
}
