use super::Builder;
use crate::builder_spirv::{BuilderCursor, SpirvValue};
use crate::codegen_cx::CodegenCx;
use crate::spirv_type::SpirvType;
use rspirv::dr;
use rspirv::grammar::{reflect, LogicalOperand, OperandKind, OperandQuantifier};
use rspirv::spirv::{
    FPFastMathMode, FragmentShadingRate, FunctionControl, ImageOperands, KernelProfilingInfo,
    LoopControl, MemoryAccess, MemorySemantics, Op, RayFlags, SelectionControl, StorageClass, Word,
};
use rustc_ast::ast::{InlineAsmOptions, InlineAsmTemplatePiece};
use rustc_codegen_ssa::mir::place::PlaceRef;
use rustc_codegen_ssa::traits::{AsmBuilderMethods, InlineAsmOperandRef};
use rustc_data_structures::fx::{FxHashMap, FxHashSet};
use rustc_hir::LlvmInlineAsmInner;
use rustc_middle::bug;
use rustc_span::{Span, DUMMY_SP};
use rustc_target::asm::{InlineAsmRegClass, InlineAsmRegOrRegClass, SpirVInlineAsmRegClass};

pub struct InstructionTable {
    table: FxHashMap<&'static str, &'static rspirv::grammar::Instruction<'static>>,
}

impl InstructionTable {
    pub fn new() -> Self {
        let table = rspirv::grammar::CoreInstructionTable::iter()
            .map(|inst| (inst.opname, inst))
            .collect();
        Self { table }
    }
}

impl<'a, 'tcx> AsmBuilderMethods<'tcx> for Builder<'a, 'tcx> {
    fn codegen_llvm_inline_asm(
        &mut self,
        _: &LlvmInlineAsmInner,
        _: Vec<PlaceRef<'tcx, Self::Value>>,
        _: Vec<Self::Value>,
        _: Span,
    ) -> bool {
        self.err("LLVM asm not supported");
        true
    }

    /* Example asm and the template it compiles to:
    asm!(
        "mov {0}, {1}",
        "add {0}, {number}",
        out(reg) o,
        in(reg) i,
        number = const 5,
    );
    [
    String("mov "),
    Placeholder { operand_idx: 0, modifier: None, span: src/lib.rs:19:18: 19:21 (#0) },
    String(", "),
    Placeholder { operand_idx: 1, modifier: None, span: src/lib.rs:19:23: 19:26 (#0) },
    String("\n"),
    String("add "),
    Placeholder { operand_idx: 0, modifier: None, span: src/lib.rs:20:18: 20:21 (#0) },
    String(", "),
    Placeholder { operand_idx: 2, modifier: None, span: src/lib.rs:20:23: 20:31 (#0) }
    ]
     */

    fn codegen_inline_asm(
        &mut self,
        template: &[InlineAsmTemplatePiece],
        operands: &[InlineAsmOperandRef<'tcx, Self>],
        options: InlineAsmOptions,
        _line_spans: &[Span],
    ) {
        const SUPPORTED_OPTIONS: InlineAsmOptions = InlineAsmOptions::NORETURN;
        let unsupported_options = options & !SUPPORTED_OPTIONS;
        if !unsupported_options.is_empty() {
            self.err(&format!(
                "asm flags not supported: {:?}",
                unsupported_options
            ));
        }
        // vec of lines, and each line is vec of tokens
        let mut tokens = vec![vec![]];
        for piece in template {
            match piece {
                InlineAsmTemplatePiece::String(asm) => {
                    // We cannot use str::lines() here because we don't want the behavior of "the
                    // last newline is optional", we want an empty string for the last line if
                    // there is no newline terminator.
                    // Lambda copied from std LinesAnyMap
                    let lines = asm.split('\n').map(|line| {
                        let l = line.len();
                        if l > 0 && line.as_bytes()[l - 1] == b'\r' {
                            &line[0..l - 1]
                        } else {
                            line
                        }
                    });
                    for (index, line) in lines.enumerate() {
                        if index != 0 {
                            // There was a newline, add a new line.
                            tokens.push(vec![]);
                        }
                        let mut chars = line.chars();
                        while let Some(token) = self.lex_word(&mut chars) {
                            tokens.last_mut().unwrap().push(token);
                        }
                    }
                }
                &InlineAsmTemplatePiece::Placeholder {
                    operand_idx,
                    modifier,
                    span,
                } => {
                    if let Some(modifier) = modifier {
                        self.tcx.sess.span_err(
                            span,
                            &format!("asm modifiers are not supported: {}", modifier),
                        );
                    }
                    let line = tokens.last_mut().unwrap();
                    let typeof_kind = line.last().and_then(|prev| match prev {
                        Token::Word("typeof") => Some(TypeofKind::Plain),
                        Token::Word("typeof*") => Some(TypeofKind::Dereference),
                        _ => None,
                    });
                    match typeof_kind {
                        Some(kind) => {
                            *line.last_mut().unwrap() =
                                Token::Typeof(&operands[operand_idx], span, kind);
                        }
                        None => match &operands[operand_idx] {
                            InlineAsmOperandRef::Const { string } => line.push(Token::Word(string)),
                            item => line.push(Token::Placeholder(item, span)),
                        },
                    }
                }
            }
        }

        let mut id_map = FxHashMap::default();
        let mut defined_ids = FxHashSet::default();
        let mut id_to_type_map = FxHashMap::default();
        for operand in operands {
            if let InlineAsmOperandRef::In { reg: _, value } = operand {
                let value = value.immediate();
                id_to_type_map.insert(value.def(self), value.ty);
            }
        }

        let mut asm_block = AsmBlock::Open;
        for line in tokens {
            self.codegen_asm(
                &mut id_map,
                &mut defined_ids,
                &mut id_to_type_map,
                &mut asm_block,
                line.into_iter(),
            );
        }

        match (options.contains(InlineAsmOptions::NORETURN), asm_block) {
            (true, AsmBlock::Open) => {
                self.err("`noreturn` requires a terminator at the end");
            }
            (true, AsmBlock::End(_)) => {
                // `noreturn` appends an `OpUnreachable` after the asm block.
                // This requires starting a new block for this.
                let label = self.emit().id();
                self.emit()
                    .insert_into_block(
                        dr::InsertPoint::End,
                        dr::Instruction::new(Op::Label, None, Some(label), vec![]),
                    )
                    .unwrap();
            }
            (false, AsmBlock::Open) => (),
            (false, AsmBlock::End(terminator)) => {
                self.err(&format!(
                    "trailing terminator {:?} requires `options(noreturn)`",
                    terminator
                ));
            }
        }
        for (id, num) in id_map {
            if !defined_ids.contains(&num) {
                self.err(&format!("%{} is used but not defined", id));
            }
        }
    }
}

enum TypeofKind {
    Plain,
    Dereference,
}

enum Token<'a, 'cx, 'tcx> {
    Word(&'a str),
    String(String),
    Placeholder(&'a InlineAsmOperandRef<'tcx, Builder<'cx, 'tcx>>, Span),
    Typeof(
        &'a InlineAsmOperandRef<'tcx, Builder<'cx, 'tcx>>,
        Span,
        TypeofKind,
    ),
}

enum OutRegister<'a> {
    Regular(Word),
    Place(PlaceRef<'a, SpirvValue>),
}

enum AsmBlock {
    Open,
    End(Op),
}

impl<'cx, 'tcx> Builder<'cx, 'tcx> {
    fn lex_word<'a>(&self, line: &mut std::str::Chars<'a>) -> Option<Token<'a, 'cx, 'tcx>> {
        loop {
            let start = line.as_str();
            match line.next()? {
                // skip over leading whitespace
                ch if ch.is_whitespace() => continue,
                // lex a string
                '"' => {
                    let mut cooked = String::new();
                    loop {
                        match line.next() {
                            None => {
                                self.err("Unterminated string in instruction");
                                return None;
                            }
                            Some('"') => break,
                            Some('\\') => {
                                let escape = match line.next() {
                                    None => {
                                        self.err("Unterminated string in instruction");
                                        return None;
                                    }
                                    Some('n') => '\n',
                                    Some('r') => '\r',
                                    Some('t') => '\t',
                                    Some('0') => '\0',
                                    Some('\\') => '\\',
                                    Some('\'') => '\'',
                                    Some('"') => '"',
                                    Some(escape) => {
                                        self.err(&format!("invalid escape '\\{}'", escape));
                                        return None;
                                    }
                                };
                                cooked.push(escape);
                            }
                            Some(ch) => {
                                cooked.push(ch);
                            }
                        }
                    }
                    break Some(Token::String(cooked));
                }
                // lex a word
                _ => {
                    let end = loop {
                        let end = line.as_str();
                        match line.next() {
                            Some(ch) if !ch.is_whitespace() => continue,
                            _ => break end,
                        }
                    };
                    let word = &start[..(start.len() - end.len())];
                    break Some(Token::Word(word));
                }
            }
        }
    }

    fn insert_inst(
        &mut self,
        id_map: &mut FxHashMap<&str, Word>,
        defined_ids: &mut FxHashSet<Word>,
        asm_block: &mut AsmBlock,
        inst: dr::Instruction,
    ) {
        // Types declared must be registered in our type system.
        let new_result_id = match inst.class.opcode {
            Op::TypeVoid => SpirvType::Void.def(self.span(), self),
            Op::TypeBool => SpirvType::Bool.def(self.span(), self),
            Op::TypeInt => SpirvType::Integer(
                inst.operands[0].unwrap_literal_int32(),
                inst.operands[1].unwrap_literal_int32() != 0,
            )
            .def(self.span(), self),
            Op::TypeFloat => {
                SpirvType::Float(inst.operands[0].unwrap_literal_int32()).def(self.span(), self)
            }
            Op::TypeStruct => {
                self.err("OpTypeStruct in asm! is not supported yet");
                return;
            }
            Op::TypeVector => SpirvType::Vector {
                element: inst.operands[0].unwrap_id_ref(),
                count: inst.operands[1].unwrap_literal_int32(),
            }
            .def(self.span(), self),
            Op::TypeMatrix => SpirvType::Matrix {
                element: inst.operands[0].unwrap_id_ref(),
                count: inst.operands[1].unwrap_literal_int32(),
            }
            .def(self.span(), self),
            Op::TypeArray => {
                self.err("OpTypeArray in asm! is not supported yet");
                return;
            }
            Op::TypeRuntimeArray => SpirvType::RuntimeArray {
                element: inst.operands[0].unwrap_id_ref(),
            }
            .def(self.span(), self),
            Op::TypePointer => {
                let storage_class = inst.operands[0].unwrap_storage_class();
                if storage_class != StorageClass::Generic {
                    self.struct_err("TypePointer in asm! requires `Generic` storage class")
                        .note(&format!(
                            "`{:?}` storage class was specified",
                            storage_class
                        ))
                        .help(&format!(
                            "the storage class will be inferred automatically (e.g. to `{:?}`)",
                            storage_class
                        ))
                        .emit();
                }
                SpirvType::Pointer {
                    pointee: inst.operands[1].unwrap_id_ref(),
                }
                .def(self.span(), self)
            }
            Op::TypeImage => SpirvType::Image {
                sampled_type: inst.operands[0].unwrap_id_ref(),
                dim: inst.operands[1].unwrap_dim(),
                depth: inst.operands[2].unwrap_literal_int32(),
                arrayed: inst.operands[3].unwrap_literal_int32(),
                multisampled: inst.operands[4].unwrap_literal_int32(),
                sampled: inst.operands[5].unwrap_literal_int32(),
                image_format: inst.operands[6].unwrap_image_format(),
            }
            .def(self.span(), self),
            Op::TypeSampledImage => SpirvType::SampledImage {
                image_type: inst.operands[0].unwrap_id_ref(),
            }
            .def(self.span(), self),
            Op::TypeSampler => SpirvType::Sampler.def(self.span(), self),
            Op::TypeAccelerationStructureKHR => {
                SpirvType::AccelerationStructureKhr.def(self.span(), self)
            }
            Op::TypeRayQueryKHR => SpirvType::RayQueryKhr.def(self.span(), self),
            Op::Variable => {
                // OpVariable with Function storage class should be emitted inside the function,
                // however, all other OpVariables should appear in the global scope instead.
                if inst.operands[0].unwrap_storage_class() == StorageClass::Function {
                    self.emit_with_cursor(BuilderCursor {
                        block: Some(0),
                        ..self.cursor
                    })
                    .insert_into_block(dr::InsertPoint::Begin, inst)
                    .unwrap();
                } else {
                    self.emit_global()
                        .insert_types_global_values(dr::InsertPoint::End, inst);
                }
                return;
            }

            op => {
                self.emit()
                    .insert_into_block(dr::InsertPoint::End, inst)
                    .unwrap();

                *asm_block = match *asm_block {
                    AsmBlock::Open => {
                        if reflect::is_block_terminator(op) {
                            AsmBlock::End(op)
                        } else {
                            AsmBlock::Open
                        }
                    }
                    AsmBlock::End(terminator) => {
                        if op != Op::Label {
                            self.err(&format!(
                                "expected OpLabel after terminator {:?}",
                                terminator
                            ));
                        }

                        AsmBlock::Open
                    }
                };

                return;
            }
        };
        for value in id_map.values_mut() {
            if *value == inst.result_id.unwrap() {
                *value = new_result_id;
            }
        }
        if defined_ids.remove(&inst.result_id.unwrap()) {
            // Note this may be a duplicate insert, if the type was deduplicated.
            defined_ids.insert(new_result_id);
        }
    }

    fn codegen_asm<'a>(
        &mut self,
        id_map: &mut FxHashMap<&'a str, Word>,
        defined_ids: &mut FxHashSet<Word>,
        id_to_type_map: &mut FxHashMap<Word, Word>,
        asm_block: &mut AsmBlock,
        mut tokens: impl Iterator<Item = Token<'a, 'cx, 'tcx>>,
    ) where
        'cx: 'a,
        'tcx: 'a,
    {
        let mut first_token = match tokens.next() {
            Some(tok) => tok,
            None => return,
        };
        // Parse result_id in front of instruction:
        // %z = OpAdd %ty %x %y
        let out_register = if match first_token {
            Token::Placeholder(_, _) => true,
            Token::Word(id_str) if id_str.starts_with('%') => true,
            Token::Word(_) | Token::String(_) | Token::Typeof(_, _, _) => false,
        } {
            let result_id = match self.parse_id_out(id_map, defined_ids, first_token) {
                Some(result_id) => result_id,
                None => return,
            };
            match tokens.next() {
                Some(Token::Word("=")) => (),
                _ => {
                    self.err("expected equals after result id specifier");
                    return;
                }
            }
            first_token = match tokens.next() {
                Some(tok) => tok,
                None => {
                    self.err("expected instruction after equals");
                    return;
                }
            };
            Some(result_id)
        } else {
            None
        };
        let inst_name = match first_token {
            Token::Word(inst_name) => inst_name,
            Token::String(_) => {
                self.err("cannot use a string as an instruction");
                return;
            }
            Token::Placeholder(_, span) | Token::Typeof(_, span, _) => {
                self.tcx
                    .sess
                    .span_err(span, "cannot use a dynamic value as an instruction type");
                return;
            }
        };
        let inst_class = inst_name
            .strip_prefix("Op")
            .and_then(|n| self.instruction_table.table.get(n));
        let inst_class = match inst_class {
            Some(inst) => inst,
            None => {
                self.err(&format!("unknown spirv instruction {}", inst_name));
                return;
            }
        };
        let result_id = match out_register {
            Some(OutRegister::Regular(reg)) => Some(reg),
            Some(OutRegister::Place(_)) => Some(self.emit().id()),
            None => None,
        };
        let mut instruction = dr::Instruction {
            class: inst_class,
            result_type: None,
            result_id,
            operands: vec![],
        };
        self.parse_operands(id_map, id_to_type_map, tokens, &mut instruction);
        if let Some(result_type) = instruction.result_type {
            id_to_type_map.insert(instruction.result_id.unwrap(), result_type);
        }
        self.insert_inst(id_map, defined_ids, asm_block, instruction);
        if let Some(OutRegister::Place(place)) = out_register {
            self.emit()
                .store(
                    place.llval.def(self),
                    result_id.unwrap(),
                    None,
                    std::iter::empty(),
                )
                .unwrap();
        }
    }

    fn parse_operands<'a>(
        &mut self,
        id_map: &mut FxHashMap<&'a str, Word>,
        id_to_type_map: &FxHashMap<Word, Word>,
        mut tokens: impl Iterator<Item = Token<'a, 'cx, 'tcx>>,
        instruction: &mut dr::Instruction,
    ) where
        'cx: 'a,
        'tcx: 'a,
    {
        let mut saw_id_result = false;
        let mut need_result_type_infer = false;

        let mut logical_operand_stack = instruction
            .class
            .operands
            .iter()
            .cloned()
            .collect::<std::collections::VecDeque<_>>();

        while let Some(LogicalOperand { kind, quantifier }) = logical_operand_stack.pop_front() {
            if kind == OperandKind::IdResult {
                assert_eq!(quantifier, OperandQuantifier::One);
                if instruction.result_id == None {
                    self.err(&format!(
                        "instruction {} expects a result id",
                        instruction.class.opname
                    ));
                }
                saw_id_result = true;
                continue;
            }

            if kind == OperandKind::IdResultType {
                assert_eq!(quantifier, OperandQuantifier::One);
                if let Some(token) = tokens.next() {
                    if let Token::Word("_") = token {
                        need_result_type_infer = true;
                    } else if let Some(id) = self.parse_id_in(id_map, token) {
                        instruction.result_type = Some(id);
                    }
                } else {
                    self.err(&format!(
                        "instruction {} expects a result type",
                        instruction.class.opname
                    ));
                }
                continue;
            }

            let operands_start = instruction.operands.len();

            match quantifier {
                OperandQuantifier::One => {
                    if !self.parse_one_operand(id_map, instruction, kind, &mut tokens) {
                        self.err(&format!(
                            "expected operand after instruction: {}",
                            instruction.class.opname
                        ));
                        return;
                    }
                }
                OperandQuantifier::ZeroOrOne => {
                    let _ = self.parse_one_operand(id_map, instruction, kind, &mut tokens);
                    // If this return false, well, it's optional, do nothing
                }
                OperandQuantifier::ZeroOrMore => {
                    while self.parse_one_operand(id_map, instruction, kind, &mut tokens) {}
                }
            }

            // Parsed operands can add more optional operands that need to be parsed
            // to an instruction - so push then on the stack here, after parsing
            for op in instruction.operands[operands_start..].iter() {
                logical_operand_stack.extend(op.additional_operands());
            }
        }

        if !saw_id_result && instruction.result_id.is_some() {
            self.err(&format!(
                "instruction {} does not expect a result id",
                instruction.class.opname
            ));
        }
        if tokens.next().is_some() {
            self.tcx.sess.err(&format!(
                "too many operands to instruction: {}",
                instruction.class.opname
            ));
        }

        if need_result_type_infer {
            assert!(instruction.result_type.is_none());

            match self.infer_result_type(id_to_type_map, instruction) {
                Some(result_type) => instruction.result_type = Some(result_type),
                None => self.err(&format!(
                    "instruction {} cannot have its result type inferred",
                    instruction.class.opname
                )),
            }
        }
    }

    fn infer_result_type(
        &self,
        id_to_type_map: &FxHashMap<Word, Word>,
        instruction: &dr::Instruction,
    ) -> Option<Word> {
        use crate::spirv_type_constraints::{instruction_signatures, InstSig, TyListPat, TyPat};

        #[derive(Debug)]
        struct Unapplicable;

        /// Recursively match `ty` against `pat`, returning one of:
        /// * `Ok([None])`: `pat` matched but contained no type variables
        /// * `Ok([Some(var)])`: `pat` matched and `var` is the type variable
        /// * `Err(Mismatch)`: `pat` didn't match or isn't supported right now
        fn match_ty_pat(
            cx: &CodegenCx<'_>,
            pat: &TyPat<'_>,
            ty: Word,
        ) -> Result<[Option<Word>; 1], Unapplicable> {
            match pat {
                TyPat::Any => Ok([None]),
                &TyPat::T => Ok([Some(ty)]),
                TyPat::Either(a, b) => {
                    match_ty_pat(cx, a, ty).or_else(|Unapplicable| match_ty_pat(cx, b, ty))
                }
                _ => match (pat, cx.lookup_type(ty)) {
                    (TyPat::Any | &TyPat::T | TyPat::Either(..), _) => unreachable!(),

                    (TyPat::Void, SpirvType::Void) => Ok([None]),
                    (TyPat::Pointer(_, pat), SpirvType::Pointer { pointee: ty, .. })
                    | (TyPat::Vector(pat), SpirvType::Vector { element: ty, .. })
                    | (
                        TyPat::Vector4(pat),
                        SpirvType::Vector {
                            element: ty,
                            count: 4,
                        },
                    )
                    | (
                        TyPat::Image(pat),
                        SpirvType::Image {
                            sampled_type: ty, ..
                        },
                    )
                    | (TyPat::SampledImage(pat), SpirvType::SampledImage { image_type: ty }) => {
                        match_ty_pat(cx, pat, ty)
                    }
                    _ => Err(Unapplicable),
                },
            }
        }

        #[derive(Debug)]
        struct Ambiguous;

        /// Construct a type from `pat`, replacing `TyPat::Var(i)` with `ty_vars[i]`.
        /// `leftover_operands` is used for `IndexComposite` patterns, if any exist.
        /// If the pattern isn't constraining enough to determine an unique type,
        /// `Err(Ambiguous)` is returned instead.
        fn subst_ty_pat(
            cx: &CodegenCx<'_>,
            pat: &TyPat<'_>,
            ty_vars: &[Option<Word>],
            leftover_operands: &[dr::Operand],
        ) -> Result<Word, Ambiguous> {
            Ok(match pat {
                &TyPat::Var(i) => match ty_vars.get(i) {
                    Some(&Some(ty)) => ty,
                    _ => return Err(Ambiguous),
                },

                TyPat::Pointer(_, pat) => SpirvType::Pointer {
                    pointee: subst_ty_pat(cx, pat, ty_vars, leftover_operands)?,
                }
                .def(DUMMY_SP, cx),

                TyPat::Vector4(pat) => SpirvType::Vector {
                    element: subst_ty_pat(cx, pat, ty_vars, leftover_operands)?,
                    count: 4,
                }
                .def(DUMMY_SP, cx),

                TyPat::SampledImage(pat) => SpirvType::SampledImage {
                    image_type: subst_ty_pat(cx, pat, ty_vars, leftover_operands)?,
                }
                .def(DUMMY_SP, cx),

                TyPat::IndexComposite(pat) => {
                    let mut ty = subst_ty_pat(cx, pat, ty_vars, leftover_operands)?;
                    for _index in leftover_operands {
                        // FIXME(eddyb) support more than just arrays, by looking
                        // up the indices (of struct fields) as constant integers.
                        ty = match cx.lookup_type(ty) {
                            SpirvType::Array { element, .. }
                            | SpirvType::RuntimeArray { element } => element,

                            _ => return Err(Ambiguous),
                        };
                    }
                    ty
                }

                _ => return Err(Ambiguous),
            })
        }

        // FIXME(eddyb) try multiple signatures until one fits.
        let mut sig = match instruction_signatures(instruction.class.opcode)? {
            [sig
            @ InstSig {
                output_type: Some(_),
                ..
            }] => *sig,
            _ => return None,
        };

        let mut combined_ty_vars = [None];

        let mut operands = instruction.operands.iter();
        let mut next_id_operand = || operands.find_map(|o| o.id_ref_any());
        while let TyListPat::Cons { first: pat, suffix } = *sig.input_types {
            sig.input_types = suffix;

            let match_result = match id_to_type_map.get(&next_id_operand()?) {
                Some(&ty) => match_ty_pat(self, pat, ty),

                // Non-value ID operand (or value operand of unknown type),
                // only `TyPat::Any` is valid.
                None => match pat {
                    TyPat::Any => Ok([None]),
                    _ => Err(Unapplicable),
                },
            };

            let ty_vars = match match_result {
                Ok(ty_vars) => ty_vars,
                Err(Unapplicable) => return None,
            };

            for (&var, combined_var) in ty_vars.iter().zip(&mut combined_ty_vars) {
                if let Some(var) = var {
                    match *combined_var {
                        Some(combined_var) => {
                            // FIXME(eddyb) this could use some error reporting
                            // (it's a type mismatch), although we could also
                            // just use the first type and let validation take
                            // care of the mismatch
                            if var != combined_var {
                                return None;
                            }
                        }
                        None => *combined_var = Some(var),
                    }
                }
            }
        }
        match sig.input_types {
            TyListPat::Cons { .. } => unreachable!(),

            TyListPat::Any => {}
            TyListPat::Nil => {
                if next_id_operand().is_some() {
                    return None;
                }
            }
            _ => return None,
        }

        match subst_ty_pat(
            self,
            sig.output_type.unwrap(),
            &combined_ty_vars,
            operands.as_slice(),
        ) {
            Ok(ty) => Some(ty),
            Err(Ambiguous) => None,
        }
    }

    fn check_reg(&mut self, span: Span, reg: &InlineAsmRegOrRegClass) {
        match reg {
            InlineAsmRegOrRegClass::RegClass(InlineAsmRegClass::SpirV(
                SpirVInlineAsmRegClass::reg,
            )) => {}
            _ => self
                .tcx
                .sess
                .span_err(span, &format!("invalid register: {}", reg)),
        }
    }

    fn parse_id_out<'a>(
        &mut self,
        id_map: &mut FxHashMap<&'a str, Word>,
        defined_ids: &mut FxHashSet<Word>,
        token: Token<'a, 'cx, 'tcx>,
    ) -> Option<OutRegister<'a>> {
        match token {
            Token::Word(word) => match word.strip_prefix('%') {
                Some(id) => Some(OutRegister::Regular({
                    let num = *id_map.entry(id).or_insert_with(|| self.emit().id());
                    if !defined_ids.insert(num) {
                        self.err(&format!("%{} is defined more than once", id));
                    }
                    num
                })),
                None => {
                    self.err("expected ID");
                    None
                }
            },
            Token::String(_) => {
                self.err("expected ID, not string");
                None
            }
            Token::Typeof(_, span, _) => {
                self.tcx
                    .sess
                    .span_err(span, "cannot assign to a typeof expression");
                None
            }
            Token::Placeholder(hole, span) => match hole {
                InlineAsmOperandRef::In { reg, value: _ } => {
                    self.check_reg(span, reg);
                    self.tcx
                        .sess
                        .span_err(span, "in register cannot be assigned to");
                    None
                }
                InlineAsmOperandRef::Out {
                    reg,
                    late: _,
                    place,
                } => {
                    self.check_reg(span, reg);
                    match place {
                        Some(place) => Some(OutRegister::Place(*place)),
                        None => {
                            self.tcx.sess.span_err(span, "missing place for register");
                            None
                        }
                    }
                }
                InlineAsmOperandRef::InOut {
                    reg,
                    late: _,
                    in_value: _,
                    out_place,
                } => {
                    self.check_reg(span, reg);
                    match out_place {
                        Some(out_place) => Some(OutRegister::Place(*out_place)),
                        None => {
                            self.tcx.sess.span_err(span, "missing place for register");
                            None
                        }
                    }
                }
                InlineAsmOperandRef::Const { string: _ } => {
                    self.tcx
                        .sess
                        .span_err(span, "cannot write to const asm argument");
                    None
                }
                InlineAsmOperandRef::SymFn { instance: _ } => {
                    self.tcx
                        .sess
                        .span_err(span, "cannot write to function asm argument");
                    None
                }
                InlineAsmOperandRef::SymStatic { def_id: _ } => {
                    self.tcx
                        .sess
                        .span_err(span, "cannot write to static variable asm argument");
                    None
                }
            },
        }
    }

    fn parse_id_in<'a>(
        &mut self,
        id_map: &mut FxHashMap<&'a str, Word>,
        token: Token<'a, 'cx, 'tcx>,
    ) -> Option<Word> {
        match token {
            Token::Word(word) => match word.strip_prefix('%') {
                Some(id) => Some(*id_map.entry(id).or_insert_with(|| self.emit().id())),
                None => {
                    self.err("expected ID");
                    None
                }
            },
            Token::String(_) => {
                self.err("expected ID, not string");
                None
            }
            Token::Typeof(hole, span, kind) => match hole {
                InlineAsmOperandRef::In { reg, value } => {
                    self.check_reg(span, reg);
                    let ty = value.immediate().ty;
                    Some(match kind {
                        TypeofKind::Plain => ty,
                        TypeofKind::Dereference => match self.lookup_type(ty) {
                            SpirvType::Pointer { pointee } => pointee,
                            other => {
                                self.tcx.sess.span_err(
                                    span,
                                    &format!(
                                        "cannot use typeof* on non-pointer type: {}",
                                        other.debug(ty, self)
                                    ),
                                );
                                ty
                            }
                        },
                    })
                }
                InlineAsmOperandRef::Out {
                    reg,
                    late: _,
                    place,
                } => {
                    self.check_reg(span, reg);
                    match place {
                        Some(place) => match self.lookup_type(place.llval.ty) {
                            SpirvType::Pointer { pointee } => Some(pointee),
                            other => {
                                self.tcx.sess.span_err(
                                    span,
                                    &format!(
                                        "out register type not pointer: {}",
                                        other.debug(place.llval.ty, self)
                                    ),
                                );
                                None
                            }
                        },
                        None => {
                            self.tcx
                                .sess
                                .span_err(span, "missing place for out register typeof");
                            None
                        }
                    }
                }
                InlineAsmOperandRef::InOut {
                    reg,
                    late: _,
                    in_value,
                    out_place: _,
                } => {
                    self.check_reg(span, reg);
                    Some(in_value.immediate().ty)
                }
                InlineAsmOperandRef::Const { string: _ } => {
                    self.tcx
                        .sess
                        .span_err(span, "cannot take the type of a const asm argument");
                    None
                }
                InlineAsmOperandRef::SymFn { instance: _ } => {
                    self.tcx
                        .sess
                        .span_err(span, "cannot take the type of a function asm argument");
                    None
                }
                InlineAsmOperandRef::SymStatic { def_id: _ } => {
                    self.tcx.sess.span_err(
                        span,
                        "cannot take the type of a static variable asm argument",
                    );
                    None
                }
            },
            Token::Placeholder(hole, span) => match hole {
                InlineAsmOperandRef::In { reg, value } => {
                    self.check_reg(span, reg);
                    Some(value.immediate().def(self))
                }
                InlineAsmOperandRef::Out {
                    reg,
                    late: _,
                    place: _,
                } => {
                    self.check_reg(span, reg);
                    self.tcx
                        .sess
                        .span_err(span, "out register cannot be used as a value");
                    None
                }
                InlineAsmOperandRef::InOut {
                    reg,
                    late: _,
                    in_value,
                    out_place: _,
                } => {
                    self.check_reg(span, reg);
                    Some(in_value.immediate().def(self))
                }
                InlineAsmOperandRef::Const { string: _ } => {
                    self.tcx
                        .sess
                        .span_err(span, "const asm argument not supported yet");
                    None
                }
                InlineAsmOperandRef::SymFn { instance: _ } => {
                    self.tcx
                        .sess
                        .span_err(span, "function asm argument not supported yet");
                    None
                }
                InlineAsmOperandRef::SymStatic { def_id: _ } => {
                    self.tcx
                        .sess
                        .span_err(span, "static variable asm argument not supported yet");
                    None
                }
            },
        }
    }

    fn parse_one_operand<'a>(
        &mut self,
        id_map: &mut FxHashMap<&'a str, Word>,
        inst: &mut dr::Instruction,
        kind: OperandKind,
        tokens: &mut impl Iterator<Item = Token<'a, 'cx, 'tcx>>,
    ) -> bool
    where
        'cx: 'a,
        'tcx: 'a,
    {
        let token = match tokens.next() {
            Some(tok) => tok,
            None => return false,
        };
        let word = match token {
            Token::Word(word) => Some(word),
            Token::String(_) | Token::Placeholder(_, _) | Token::Typeof(_, _, _) => None,
        };
        match (kind, word) {
            (OperandKind::IdResultType | OperandKind::IdResult, _) => {
                bug!("should be handled by parse_operands");
            }
            (OperandKind::IdMemorySemantics, _) => {
                if let Some(id) = self.parse_id_in(id_map, token) {
                    inst.operands.push(dr::Operand::IdMemorySemantics(id));
                }
            }
            (OperandKind::IdScope, _) => {
                if let Some(id) = self.parse_id_in(id_map, token) {
                    inst.operands.push(dr::Operand::IdScope(id));
                }
            }
            (OperandKind::IdRef, _) => {
                if let Some(id) = self.parse_id_in(id_map, token) {
                    inst.operands.push(dr::Operand::IdRef(id));
                }
            }

            (OperandKind::LiteralInteger, Some(word)) => match word.parse() {
                Ok(v) => inst.operands.push(dr::Operand::LiteralInt32(v)),
                Err(e) => self.err(&format!("invalid integer: {}", e)),
            },
            (OperandKind::LiteralString, _) => {
                if let Token::String(value) = token {
                    inst.operands.push(dr::Operand::LiteralString(value));
                }
            }
            (OperandKind::LiteralContextDependentNumber, Some(word)) => {
                assert!(matches!(inst.class.opcode, Op::Constant | Op::SpecConstant));
                let ty = inst.result_type.unwrap();
                fn parse(ty: SpirvType, w: &str) -> Result<dr::Operand, String> {
                    fn fmt(x: impl ToString) -> String {
                        x.to_string()
                    }
                    Ok(match ty {
                        SpirvType::Integer(8, false) => {
                            dr::Operand::LiteralInt32(w.parse::<u8>().map_err(fmt)? as u32)
                        }
                        SpirvType::Integer(16, false) => {
                            dr::Operand::LiteralInt32(w.parse::<u16>().map_err(fmt)? as u32)
                        }
                        SpirvType::Integer(32, false) => {
                            dr::Operand::LiteralInt32(w.parse::<u32>().map_err(fmt)?)
                        }
                        SpirvType::Integer(64, false) => {
                            dr::Operand::LiteralInt64(w.parse::<u64>().map_err(fmt)?)
                        }
                        SpirvType::Integer(8, true) => {
                            dr::Operand::LiteralInt32(w.parse::<i8>().map_err(fmt)? as i32 as u32)
                        }
                        SpirvType::Integer(16, true) => {
                            dr::Operand::LiteralInt32(w.parse::<i16>().map_err(fmt)? as i32 as u32)
                        }
                        SpirvType::Integer(32, true) => {
                            dr::Operand::LiteralInt32(w.parse::<i32>().map_err(fmt)? as u32)
                        }
                        SpirvType::Integer(64, true) => {
                            dr::Operand::LiteralInt64(w.parse::<i64>().map_err(fmt)? as u64)
                        }
                        SpirvType::Float(32) => {
                            dr::Operand::LiteralFloat32(w.parse::<f32>().map_err(fmt)?)
                        }
                        SpirvType::Float(64) => {
                            dr::Operand::LiteralFloat64(w.parse::<f64>().map_err(fmt)?)
                        }
                        _ => return Err("expected number literal in OpConstant".to_string()),
                    })
                }
                match parse(self.lookup_type(ty), word) {
                    Ok(op) => inst.operands.push(op),
                    Err(err) => self.err(&err),
                }
            }
            (OperandKind::LiteralExtInstInteger, Some(word)) => match word.parse() {
                Ok(v) => inst.operands.push(dr::Operand::LiteralExtInstInteger(v)),
                Err(e) => self.err(&format!("invalid integer: {}", e)),
            },
            (OperandKind::LiteralSpecConstantOpInteger, Some(word)) => {
                match self.instruction_table.table.get(word) {
                    Some(v) => {
                        inst.operands
                            .push(dr::Operand::LiteralSpecConstantOpInteger(v.opcode));
                    }
                    None => self.err("invalid instruction in OpSpecConstantOp"),
                }
            }
            (OperandKind::PairLiteralIntegerIdRef, _) => {
                self.err("PairLiteralIntegerIdRef not supported yet");
            }
            (OperandKind::PairIdRefLiteralInteger, _) => {
                if let Some(id) = self.parse_id_in(id_map, token) {
                    inst.operands.push(dr::Operand::IdRef(id));
                    match tokens.next() {
                        Some(Token::Word(word)) => match word.parse() {
                            Ok(v) => inst.operands.push(dr::Operand::LiteralInt32(v)),
                            Err(e) => self.err(&format!("invalid integer: {}", e)),
                        },
                        Some(Token::String(_)) => self.err(&format!(
                            "expected a literal, not a string for a {:?}",
                            kind
                        )),
                        Some(Token::Placeholder(_, span)) => self.tcx.sess.span_err(
                            span,
                            &format!("expected a literal, not a dynamic value for a {:?}", kind),
                        ),
                        Some(Token::Typeof(_, span, _)) => self.tcx.sess.span_err(
                            span,
                            &format!("expected a literal, not a type for a {:?}", kind),
                        ),
                        None => self.err("expected operand after instruction"),
                    }
                }
            }
            (OperandKind::PairIdRefIdRef, _) => {
                if let Some(id) = self.parse_id_in(id_map, token) {
                    inst.operands.push(dr::Operand::IdRef(id));
                    match tokens.next() {
                        Some(token) => {
                            if let Some(id) = self.parse_id_in(id_map, token) {
                                inst.operands.push(dr::Operand::IdRef(id));
                            }
                        }
                        None => self.err("expected operand after instruction"),
                    }
                }
            }

            (OperandKind::ImageOperands, Some(word)) => {
                match parse_bitflags_operand(IMAGE_OPERANDS, word) {
                    Some(x) => inst.operands.push(dr::Operand::ImageOperands(x)),
                    None => self.err(&format!("Unknown ImageOperands {}", word)),
                }
            }
            (OperandKind::FPFastMathMode, Some(word)) => {
                match parse_bitflags_operand(FP_FAST_MATH_MODE, word) {
                    Some(x) => inst.operands.push(dr::Operand::FPFastMathMode(x)),
                    None => self.err(&format!("Unknown FPFastMathMode {}", word)),
                }
            }
            (OperandKind::SelectionControl, Some(word)) => {
                match parse_bitflags_operand(SELECTION_CONTROL, word) {
                    Some(x) => inst.operands.push(dr::Operand::SelectionControl(x)),
                    None => self.err(&format!("Unknown SelectionControl {}", word)),
                }
            }
            (OperandKind::LoopControl, Some(word)) => {
                match parse_bitflags_operand(LOOP_CONTROL, word) {
                    Some(x) => inst.operands.push(dr::Operand::LoopControl(x)),
                    None => self.err(&format!("Unknown LoopControl {}", word)),
                }
            }
            (OperandKind::FunctionControl, Some(word)) => {
                match parse_bitflags_operand(FUNCTION_CONTROL, word) {
                    Some(x) => inst.operands.push(dr::Operand::FunctionControl(x)),
                    None => self.err(&format!("Unknown FunctionControl {}", word)),
                }
            }
            (OperandKind::MemorySemantics, Some(word)) => {
                match parse_bitflags_operand(MEMORY_SEMANTICS, word) {
                    Some(x) => inst.operands.push(dr::Operand::MemorySemantics(x)),
                    None => self.err(&format!("Unknown MemorySemantics {}", word)),
                }
            }
            (OperandKind::MemoryAccess, Some(word)) => {
                match parse_bitflags_operand(MEMORY_ACCESS, word) {
                    Some(x) => inst.operands.push(dr::Operand::MemoryAccess(x)),
                    None => self.err(&format!("Unknown MemoryAccess {}", word)),
                }
            }
            (OperandKind::KernelProfilingInfo, Some(word)) => {
                match parse_bitflags_operand(KERNEL_PROFILING_INFO, word) {
                    Some(x) => inst.operands.push(dr::Operand::KernelProfilingInfo(x)),
                    None => self.err(&format!("Unknown KernelProfilingInfo {}", word)),
                }
            }
            (OperandKind::RayFlags, Some(word)) => match parse_bitflags_operand(RAY_FLAGS, word) {
                Some(x) => inst.operands.push(dr::Operand::RayFlags(x)),
                None => self.err(&format!("Unknown RayFlags {}", word)),
            },
            (OperandKind::FragmentShadingRate, Some(word)) => {
                match parse_bitflags_operand(FRAGMENT_SHADING_RATE, word) {
                    Some(x) => inst.operands.push(dr::Operand::FragmentShadingRate(x)),
                    None => self.err(&format!("Unknown FragmentShadingRate {}", word)),
                }
            }

            (OperandKind::SourceLanguage, Some(word)) => match word.parse() {
                Ok(x) => inst.operands.push(dr::Operand::SourceLanguage(x)),
                Err(()) => self.err(&format!("Unknown SourceLanguage {}", word)),
            },
            (OperandKind::ExecutionModel, Some(word)) => match word.parse() {
                Ok(x) => inst.operands.push(dr::Operand::ExecutionModel(x)),
                Err(()) => self.err(&format!("unknown ExecutionModel {}", word)),
            },
            (OperandKind::AddressingModel, Some(word)) => match word.parse() {
                Ok(x) => inst.operands.push(dr::Operand::AddressingModel(x)),
                Err(()) => self.err(&format!("unknown AddressingModel {}", word)),
            },
            (OperandKind::MemoryModel, Some(word)) => match word.parse() {
                Ok(x) => inst.operands.push(dr::Operand::MemoryModel(x)),
                Err(()) => self.err(&format!("unknown MemoryModel {}", word)),
            },
            (OperandKind::ExecutionMode, Some(word)) => match word.parse() {
                Ok(x) => inst.operands.push(dr::Operand::ExecutionMode(x)),
                Err(()) => self.err(&format!("unknown ExecutionMode {}", word)),
            },
            (OperandKind::StorageClass, Some(word)) => match word.parse() {
                Ok(x) => inst.operands.push(dr::Operand::StorageClass(x)),
                Err(()) => self.err(&format!("unknown StorageClass {}", word)),
            },
            (OperandKind::Dim, Some(word)) => match word.parse() {
                Ok(x) => inst.operands.push(dr::Operand::Dim(x)),
                Err(()) => self.err(&format!("unknown Dim {}", word)),
            },
            (OperandKind::SamplerAddressingMode, Some(word)) => match word.parse() {
                Ok(x) => inst.operands.push(dr::Operand::SamplerAddressingMode(x)),
                Err(()) => self.err(&format!("unknown SamplerAddressingMode {}", word)),
            },
            (OperandKind::SamplerFilterMode, Some(word)) => match word.parse() {
                Ok(x) => inst.operands.push(dr::Operand::SamplerFilterMode(x)),
                Err(()) => self.err(&format!("unknown SamplerFilterMode {}", word)),
            },
            (OperandKind::ImageFormat, Some(word)) => match word.parse() {
                Ok(x) => inst.operands.push(dr::Operand::ImageFormat(x)),
                Err(()) => self.err(&format!("unknown ImageFormat {}", word)),
            },
            (OperandKind::ImageChannelOrder, Some(word)) => match word.parse() {
                Ok(x) => inst.operands.push(dr::Operand::ImageChannelOrder(x)),
                Err(()) => self.err(&format!("unknown ImageChannelOrder {}", word)),
            },
            (OperandKind::ImageChannelDataType, Some(word)) => match word.parse() {
                Ok(x) => inst.operands.push(dr::Operand::ImageChannelDataType(x)),
                Err(()) => self.err(&format!("unknown ImageChannelDataType {}", word)),
            },
            (OperandKind::FPRoundingMode, Some(word)) => match word.parse() {
                Ok(x) => inst.operands.push(dr::Operand::FPRoundingMode(x)),
                Err(()) => self.err(&format!("unknown FPRoundingMode {}", word)),
            },
            (OperandKind::LinkageType, Some(word)) => match word.parse() {
                Ok(x) => inst.operands.push(dr::Operand::LinkageType(x)),
                Err(()) => self.err(&format!("unknown LinkageType {}", word)),
            },
            (OperandKind::AccessQualifier, Some(word)) => match word.parse() {
                Ok(x) => inst.operands.push(dr::Operand::AccessQualifier(x)),
                Err(()) => self.err(&format!("unknown AccessQualifier {}", word)),
            },
            (OperandKind::FunctionParameterAttribute, Some(word)) => match word.parse() {
                Ok(x) => inst
                    .operands
                    .push(dr::Operand::FunctionParameterAttribute(x)),
                Err(()) => self.err(&format!("unknown FunctionParameterAttribute {}", word)),
            },
            (OperandKind::Decoration, Some(word)) => match word.parse() {
                Ok(x) => inst.operands.push(dr::Operand::Decoration(x)),
                Err(()) => self.err(&format!("unknown Decoration {}", word)),
            },
            (OperandKind::BuiltIn, Some(word)) => match word.parse() {
                Ok(x) => inst.operands.push(dr::Operand::BuiltIn(x)),
                Err(()) => self.err(&format!("unknown BuiltIn {}", word)),
            },
            (OperandKind::Scope, Some(word)) => match word.parse() {
                Ok(x) => inst.operands.push(dr::Operand::Scope(x)),
                Err(()) => self.err(&format!("unknown Scope {}", word)),
            },
            (OperandKind::GroupOperation, Some(word)) => match word.parse() {
                Ok(x) => inst.operands.push(dr::Operand::GroupOperation(x)),
                Err(()) => self.err(&format!("unknown GroupOperation {}", word)),
            },
            (OperandKind::KernelEnqueueFlags, Some(word)) => match word.parse() {
                Ok(x) => inst.operands.push(dr::Operand::KernelEnqueueFlags(x)),
                Err(()) => self.err(&format!("unknown KernelEnqueueFlags {}", word)),
            },
            (OperandKind::Capability, Some(word)) => match word.parse() {
                Ok(x) => inst.operands.push(dr::Operand::Capability(x)),
                Err(()) => self.err(&format!("unknown Capability {}", word)),
            },
            (OperandKind::RayQueryIntersection, Some(word)) => match word.parse() {
                Ok(x) => inst.operands.push(dr::Operand::RayQueryIntersection(x)),
                Err(()) => self.err(&format!("unknown RayQueryIntersection {}", word)),
            },
            (OperandKind::RayQueryCommittedIntersectionType, Some(word)) => match word.parse() {
                Ok(x) => inst
                    .operands
                    .push(dr::Operand::RayQueryCommittedIntersectionType(x)),
                Err(()) => self.err(&format!(
                    "unknown RayQueryCommittedIntersectionType {}",
                    word
                )),
            },
            (OperandKind::RayQueryCandidateIntersectionType, Some(word)) => match word.parse() {
                Ok(x) => inst
                    .operands
                    .push(dr::Operand::RayQueryCandidateIntersectionType(x)),
                Err(()) => self.err(&format!(
                    "unknown RayQueryCandidateIntersectionType {}",
                    word
                )),
            },
            (kind, None) => match token {
                Token::Word(_) => bug!(),
                Token::String(_) => {
                    self.err(&format!(
                        "expected a literal, not a string for a {:?}",
                        kind
                    ));
                }
                Token::Placeholder(_, span) => {
                    self.tcx.sess.span_err(
                        span,
                        &format!("expected a literal, not a dynamic value for a {:?}", kind),
                    );
                }
                Token::Typeof(_, span, _) => {
                    self.tcx.sess.span_err(
                        span,
                        &format!("expected a literal, not a type for a {:?}", kind),
                    );
                }
            },
        }
        true
    }
}

pub const IMAGE_OPERANDS: &[(&str, ImageOperands)] = &[
    ("None", ImageOperands::NONE),
    ("Bias", ImageOperands::BIAS),
    ("Lod", ImageOperands::LOD),
    ("Grad", ImageOperands::GRAD),
    ("ConstOffset", ImageOperands::CONST_OFFSET),
    ("Offset", ImageOperands::OFFSET),
    ("ConstOffsets", ImageOperands::CONST_OFFSETS),
    ("Sample", ImageOperands::SAMPLE),
    ("MinLod", ImageOperands::MIN_LOD),
    ("MakeTexelAvailable", ImageOperands::MAKE_TEXEL_AVAILABLE),
    (
        "MakeTexelAvailableKHR",
        ImageOperands::MAKE_TEXEL_AVAILABLE_KHR,
    ),
    ("MakeTexelVisible", ImageOperands::MAKE_TEXEL_VISIBLE),
    ("MakeTexelVisibleKHR", ImageOperands::MAKE_TEXEL_VISIBLE_KHR),
    ("NonPrivateTexel", ImageOperands::NON_PRIVATE_TEXEL),
    ("NonPrivateTexelKHR", ImageOperands::NON_PRIVATE_TEXEL_KHR),
    ("VolatileTexel", ImageOperands::VOLATILE_TEXEL),
    ("VolatileTexelKHR", ImageOperands::VOLATILE_TEXEL_KHR),
    ("SignExtend", ImageOperands::SIGN_EXTEND),
    ("ZeroExtend", ImageOperands::ZERO_EXTEND),
];
pub const FP_FAST_MATH_MODE: &[(&str, FPFastMathMode)] = &[
    ("None", FPFastMathMode::NONE),
    ("NotNan", FPFastMathMode::NOT_NAN),
    ("NotInf", FPFastMathMode::NOT_INF),
    ("Nsz", FPFastMathMode::NSZ),
    ("AllowRecip", FPFastMathMode::ALLOW_RECIP),
    ("Fast", FPFastMathMode::FAST),
];
pub const SELECTION_CONTROL: &[(&str, SelectionControl)] = &[
    ("None", SelectionControl::NONE),
    ("Flatten", SelectionControl::FLATTEN),
    ("DontFlatten", SelectionControl::DONT_FLATTEN),
];
pub const LOOP_CONTROL: &[(&str, LoopControl)] = &[
    ("None", LoopControl::NONE),
    ("Unroll", LoopControl::UNROLL),
    ("DontUnroll", LoopControl::DONT_UNROLL),
    ("DependencyInfinite", LoopControl::DEPENDENCY_INFINITE),
    ("DependencyLength", LoopControl::DEPENDENCY_LENGTH),
    ("MinIterations", LoopControl::MIN_ITERATIONS),
    ("MaxIterations", LoopControl::MAX_ITERATIONS),
    ("IterationMultiple", LoopControl::ITERATION_MULTIPLE),
    ("PeelCount", LoopControl::PEEL_COUNT),
    ("PartialCount", LoopControl::PARTIAL_COUNT),
];
pub const FUNCTION_CONTROL: &[(&str, FunctionControl)] = &[
    ("None", FunctionControl::NONE),
    ("Inline", FunctionControl::INLINE),
    ("DontInline", FunctionControl::DONT_INLINE),
    ("Pure", FunctionControl::PURE),
    ("Const", FunctionControl::CONST),
];
pub const MEMORY_SEMANTICS: &[(&str, MemorySemantics)] = &[
    ("Relaxed", MemorySemantics::RELAXED),
    ("None", MemorySemantics::NONE),
    ("Acquire", MemorySemantics::ACQUIRE),
    ("Release", MemorySemantics::RELEASE),
    ("AcquireRelease", MemorySemantics::ACQUIRE_RELEASE),
    (
        "SequentiallyConsistent",
        MemorySemantics::SEQUENTIALLY_CONSISTENT,
    ),
    ("UniformMemory", MemorySemantics::UNIFORM_MEMORY),
    ("SubgroupMemory", MemorySemantics::SUBGROUP_MEMORY),
    ("WorkgroupMemory", MemorySemantics::WORKGROUP_MEMORY),
    (
        "CrossWorkgroupMemory",
        MemorySemantics::CROSS_WORKGROUP_MEMORY,
    ),
    (
        "AtomicCounterMemory",
        MemorySemantics::ATOMIC_COUNTER_MEMORY,
    ),
    ("ImageMemory", MemorySemantics::IMAGE_MEMORY),
    ("OutputMemory", MemorySemantics::OUTPUT_MEMORY),
    ("OutputMemoryKHR", MemorySemantics::OUTPUT_MEMORY_KHR),
    ("MakeAvailable", MemorySemantics::MAKE_AVAILABLE),
    ("MakeAvailableKHR", MemorySemantics::MAKE_AVAILABLE_KHR),
    ("MakeVisible", MemorySemantics::MAKE_VISIBLE),
    ("MakeVisibleKHR", MemorySemantics::MAKE_VISIBLE_KHR),
    ("Volatile", MemorySemantics::VOLATILE),
];
pub const MEMORY_ACCESS: &[(&str, MemoryAccess)] = &[
    ("None", MemoryAccess::NONE),
    ("Volatile", MemoryAccess::VOLATILE),
    ("Aligned", MemoryAccess::ALIGNED),
    ("Nontemporal", MemoryAccess::NONTEMPORAL),
    ("MakePointerAvailable", MemoryAccess::MAKE_POINTER_AVAILABLE),
    (
        "MakePointerAvailableKHR",
        MemoryAccess::MAKE_POINTER_AVAILABLE_KHR,
    ),
    ("MakePointerVisible", MemoryAccess::MAKE_POINTER_VISIBLE),
    (
        "MakePointerVisibleKHR",
        MemoryAccess::MAKE_POINTER_VISIBLE_KHR,
    ),
    ("NonPrivatePointer", MemoryAccess::NON_PRIVATE_POINTER),
    (
        "NonPrivatePointerKHR",
        MemoryAccess::NON_PRIVATE_POINTER_KHR,
    ),
];
pub const KERNEL_PROFILING_INFO: &[(&str, KernelProfilingInfo)] = &[
    ("None", KernelProfilingInfo::NONE),
    ("CmdExecTime", KernelProfilingInfo::CMD_EXEC_TIME),
];
pub const RAY_FLAGS: &[(&str, RayFlags)] = &[
    ("NoneKHR", RayFlags::NONE_KHR),
    ("OpaqueKHR", RayFlags::OPAQUE_KHR),
    ("NoOpaqueKHR", RayFlags::NO_OPAQUE_KHR),
    (
        "TerminateOnFirstHitKHR",
        RayFlags::TERMINATE_ON_FIRST_HIT_KHR,
    ),
    (
        "SkipClosestHitShaderKHR",
        RayFlags::SKIP_CLOSEST_HIT_SHADER_KHR,
    ),
    (
        "CullBackFacingTrianglesKHR",
        RayFlags::CULL_BACK_FACING_TRIANGLES_KHR,
    ),
    (
        "CullFrontFacingTrianglesKHR",
        RayFlags::CULL_FRONT_FACING_TRIANGLES_KHR,
    ),
    ("CullOpaqueKHR", RayFlags::CULL_OPAQUE_KHR),
    ("CullNoOpaqueKHR", RayFlags::CULL_NO_OPAQUE_KHR),
    ("SkipTrianglesKHR", RayFlags::SKIP_TRIANGLES_KHR),
    ("SkipAabBsKHR", RayFlags::SKIP_AAB_BS_KHR),
];
pub const FRAGMENT_SHADING_RATE: &[(&str, FragmentShadingRate)] = &[
    ("VERTICAL2_PIXELS", FragmentShadingRate::VERTICAL2_PIXELS),
    ("VERTICAL4_PIXELS", FragmentShadingRate::VERTICAL4_PIXELS),
    (
        "HORIZONTAL2_PIXELS",
        FragmentShadingRate::HORIZONTAL2_PIXELS,
    ),
    (
        "HORIZONTAL4_PIXELS",
        FragmentShadingRate::HORIZONTAL4_PIXELS,
    ),
];

fn parse_bitflags_operand<T: std::ops::BitOr<Output = T> + Copy>(
    values: &'static [(&'static str, T)],
    word: &str,
) -> Option<T> {
    let mut result = None;
    'outer: for item in word.split('|') {
        for &(key, value) in values {
            if item == key {
                result = Some(result.map_or(value, |x| x | value));
                continue 'outer;
            }
        }
        return None;
    }
    result
}
