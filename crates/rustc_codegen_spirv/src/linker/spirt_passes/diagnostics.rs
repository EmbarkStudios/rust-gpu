use crate::decorations::{CustomDecoration, SpanRegenerator, SrcLocDecoration, ZombieDecoration};
use rustc_data_structures::fx::FxIndexSet;
use rustc_errors::DiagnosticBuilder;
use rustc_session::Session;
use rustc_span::{Span, DUMMY_SP};
use smallvec::SmallVec;
use spirt::visit::{InnerVisit, Visitor};
use spirt::{
    spv, Attr, AttrSet, AttrSetDef, Const, Context, DataInstDef, DataInstKind, Diag, DiagLevel,
    ExportKey, Exportee, Func, GlobalVar, Module, Type,
};
use std::marker::PhantomData;
use std::{mem, str};

pub(crate) fn report_diagnostics(
    sess: &Session,
    linker_options: &crate::linker::Options,
    module: &Module,
) -> crate::linker::Result<()> {
    let cx = &module.cx();

    let mut reporter = DiagnosticReporter {
        sess,
        linker_options,

        cx,
        module,

        seen_attrs: FxIndexSet::default(),
        seen_types: FxIndexSet::default(),
        seen_consts: FxIndexSet::default(),
        seen_global_vars: FxIndexSet::default(),
        seen_funcs: FxIndexSet::default(),

        use_stack: SmallVec::new(),
        span_regen: SpanRegenerator::new_spirt(sess.source_map(), module),
        overall_result: Ok(()),
        any_spirt_bugs: false,
    };
    for (export_key, &exportee) in &module.exports {
        assert_eq!(reporter.use_stack.len(), 0);

        if let Exportee::Func(func) = exportee {
            let func_decl = &module.funcs[func];
            reporter.use_stack.push(UseOrigin::IntraFunc {
                func_attrs: func_decl.attrs,
                func_export_key: Some(export_key),
                inst_attrs: AttrSet::default(),
                origin: IntraFuncUseOrigin::Other,
            });
            export_key.inner_visit_with(&mut reporter);
            if reporter.seen_funcs.insert(func) {
                reporter.visit_func_decl(func_decl);
            }
            reporter.use_stack.pop();
        }
        export_key.inner_visit_with(&mut reporter);
        exportee.inner_visit_with(&mut reporter);
    }

    if reporter.any_spirt_bugs {
        let mut note = sess.struct_note_without_error("SPIR-T bugs were reported");
        match &linker_options.dump_spirt_passes {
            Some(dump_dir) => {
                note.help(format!(
                    "pretty-printed SPIR-T will be saved to `{}`, as `.spirt.html` files",
                    dump_dir.display()
                ));
            }
            None => {
                // FIXME(eddyb) maybe just always generate the files in a tmpdir?
                note.help(
                    "re-run with `RUSTGPU_CODEGEN_ARGS=\"--dump-spirt-passes=$PWD\"` to \
                     get pretty-printed SPIR-T (`.spirt.html`)",
                );
            }
        }
        note.note("pretty-printed SPIR-T is preferred when reporting Rust-GPU issues")
            .emit();
    }

    reporter.overall_result
}

// HACK(eddyb) version of `decorations::LazilyDecoded` that works for SPIR-T.
struct LazilyDecoded<D> {
    encoded: String,
    _marker: PhantomData<D>,
}

impl<D> LazilyDecoded<D> {
    fn decode<'a>(&'a self) -> D
    where
        D: CustomDecoration<'a>,
    {
        D::decode(&self.encoded)
    }
}

fn decode_spv_lit_str_with<R>(imms: &[spv::Imm], f: impl FnOnce(&str) -> R) -> R {
    let wk = &super::SpvSpecWithExtras::get().well_known;

    // FIXME(eddyb) deduplicate with `spirt::spv::extract_literal_string`.
    let words = imms.iter().enumerate().map(|(i, &imm)| match (i, imm) {
        (0, spirt::spv::Imm::Short(k, w) | spirt::spv::Imm::LongStart(k, w))
        | (1.., spirt::spv::Imm::LongCont(k, w)) => {
            assert_eq!(k, wk.LiteralString);
            w
        }
        _ => unreachable!(),
    });
    let bytes: SmallVec<[u8; 64]> = words
        .flat_map(u32::to_le_bytes)
        .take_while(|&byte| byte != 0)
        .collect();

    f(str::from_utf8(&bytes).expect("invalid UTF-8 in string literal"))
}

fn try_decode_custom_decoration<'a, D: CustomDecoration<'a>>(
    attrs_def: &AttrSetDef,
) -> Option<LazilyDecoded<D>> {
    let wk = &super::SpvSpecWithExtras::get().well_known;

    attrs_def.attrs.iter().find_map(|attr| {
        let spv_inst = match attr {
            Attr::SpvAnnotation(spv_inst) if spv_inst.opcode == wk.OpDecorateString => spv_inst,
            _ => return None,
        };
        let str_imms = spv_inst
            .imms
            .strip_prefix(&[spv::Imm::Short(wk.Decoration, wk.UserTypeGOOGLE)])?;

        decode_spv_lit_str_with(str_imms, |prefixed_encoded| {
            let encoded = prefixed_encoded.strip_prefix(D::ENCODING_PREFIX)?;

            Some(LazilyDecoded {
                encoded: encoded.to_string(),
                _marker: PhantomData,
            })
        })
    })
}

// FIXME(eddyb) this looks a lot like `ReachableUseCollector`, maybe some
// automation should be built around "deep visitors" in general?
struct DiagnosticReporter<'a> {
    sess: &'a Session,
    linker_options: &'a crate::linker::Options,

    cx: &'a Context,
    module: &'a Module,

    seen_attrs: FxIndexSet<AttrSet>,
    seen_types: FxIndexSet<Type>,
    seen_consts: FxIndexSet<Const>,
    seen_global_vars: FxIndexSet<GlobalVar>,
    seen_funcs: FxIndexSet<Func>,

    use_stack: SmallVec<[UseOrigin<'a>; 8]>,
    span_regen: SpanRegenerator<'a>,
    overall_result: crate::linker::Result<()>,
    any_spirt_bugs: bool,
}

enum UseOrigin<'a> {
    Global {
        kind: &'static &'static str,
        attrs: AttrSet,
    },
    IntraFunc {
        func_attrs: AttrSet,
        func_export_key: Option<&'a ExportKey>,

        inst_attrs: AttrSet,
        origin: IntraFuncUseOrigin,
    },
}

enum IntraFuncUseOrigin {
    CallCallee,
    Other,
}

impl UseOrigin<'_> {
    fn to_rustc_span(&self, cx: &Context, span_regen: &mut SpanRegenerator<'_>) -> Option<Span> {
        let mut from_attrs = |attrs: AttrSet| {
            let attrs_def = &cx[attrs];
            attrs_def
                .attrs
                .iter()
                .find_map(|attr| match attr {
                    &Attr::SpvDebugLine {
                        file_path,
                        line,
                        col,
                    } => span_regen.src_loc_to_rustc(SrcLocDecoration {
                        file_name: &cx[file_path.0],
                        line,
                        col,
                    }),
                    _ => None,
                })
                .or_else(|| {
                    span_regen.src_loc_to_rustc(
                        try_decode_custom_decoration::<SrcLocDecoration<'_>>(attrs_def)?.decode(),
                    )
                })
        };
        match *self {
            Self::Global { attrs, .. } => from_attrs(attrs),
            Self::IntraFunc {
                func_attrs,
                inst_attrs,
                ..
            } => from_attrs(inst_attrs).or_else(|| from_attrs(func_attrs)),
        }
    }

    fn note(
        &self,
        cx: &Context,
        span_regen: &mut SpanRegenerator<'_>,
        err: &mut DiagnosticBuilder<'_, impl rustc_errors::EmissionGuarantee>,
    ) {
        let wk = &super::SpvSpecWithExtras::get().well_known;

        let name_from_attrs = |attrs: AttrSet, kind| {
            cx[attrs]
                .attrs
                .iter()
                .find_map(|attr| match attr {
                    Attr::SpvAnnotation(spv_inst) if spv_inst.opcode == wk.OpName => {
                        Some(decode_spv_lit_str_with(&spv_inst.imms, |name| {
                            format!("`{name}`")
                        }))
                    }
                    _ => None,
                })
                .unwrap_or_else(|| format!("unnamed {kind}"))
        };
        let note = match self {
            &Self::Global { kind, attrs } => {
                format!("used by {}", name_from_attrs(attrs, *kind))
            }
            Self::IntraFunc {
                func_attrs,
                func_export_key,
                inst_attrs: _,
                origin,
            } => {
                let func_desc = func_export_key
                    .map(|export_key| match export_key {
                        &ExportKey::LinkName(name) => format!("function export `{}`", &cx[name]),
                        ExportKey::SpvEntryPoint { imms, .. } => match imms[..] {
                            [em @ spv::Imm::Short(em_kind, _), ref name_imms @ ..] => {
                                assert_eq!(em_kind, wk.ExecutionModel);
                                let em = spv::print::operand_from_imms([em]).concat_to_plain_text();
                                decode_spv_lit_str_with(name_imms, |name| {
                                    format!(
                                        "{} entry-point `{name}`",
                                        em.strip_prefix("ExecutionModel.").unwrap()
                                    )
                                })
                            }
                            _ => unreachable!(),
                        },
                    })
                    .unwrap_or_else(|| name_from_attrs(*func_attrs, "function"));
                match origin {
                    IntraFuncUseOrigin::CallCallee => format!("called by {func_desc}"),
                    IntraFuncUseOrigin::Other => format!("used from within {func_desc}"),
                }
            }
        };

        let span = self.to_rustc_span(cx, span_regen).unwrap_or(DUMMY_SP);
        err.span_note(span, note);
    }
}

impl DiagnosticReporter<'_> {
    fn report_from_attrs(&mut self, attrs: AttrSet) {
        if attrs == AttrSet::default() {
            return;
        }

        // Split off the last entry in `self.use_stack` if it's for the definition
        // that `attrs` come from - this should almost always be the case, except
        // for instructions inside a function body, or visitor bugs.
        let (current_def, use_stack_for_def) = self
            .use_stack
            .split_last()
            .filter(
                |(
                    &UseOrigin::Global {
                        attrs: use_attrs, ..
                    }
                    | &UseOrigin::IntraFunc {
                        func_attrs: use_attrs,
                        ..
                    },
                    _,
                )| { use_attrs == attrs },
            )
            .map_or((None, &self.use_stack[..]), |(current, stack)| {
                (Some(current), stack)
            });

        let attrs_def = &self.cx[attrs];
        if !self.linker_options.early_report_zombies {
            if let Some(zombie) = try_decode_custom_decoration::<ZombieDecoration<'_>>(attrs_def) {
                let ZombieDecoration { reason } = zombie.decode();
                let def_span = current_def
                    .and_then(|def| def.to_rustc_span(self.cx, &mut self.span_regen))
                    .unwrap_or(DUMMY_SP);
                let mut err = self.sess.struct_span_err(def_span, reason);
                for use_origin in use_stack_for_def.iter().rev() {
                    use_origin.note(self.cx, &mut self.span_regen, &mut err);
                }
                self.overall_result = Err(err.emit());
            }
        }

        let diags = attrs_def.attrs.iter().flat_map(|attr| match attr {
            Attr::Diagnostics(diags) => diags.0.iter(),
            _ => [].iter(),
        });
        for diag in diags {
            let Diag { level, message } = diag;

            let prefix = match level {
                DiagLevel::Bug(location) => {
                    let location = location.to_string();
                    let location = match location.rsplit_once("/src/") {
                        Some((_path_prefix, intra_src)) => intra_src,
                        None => &location,
                    };
                    format!("SPIR-T BUG [{location}] ")
                }
                DiagLevel::Error | DiagLevel::Warning => "".to_string(),
            };
            let (deps, msg) = spirt::print::Plan::for_root(self.cx, message)
                .pretty_print_deps_and_root_separately();

            let deps = deps.to_string();
            let suffix = if !deps.is_empty() {
                format!("\n  where\n    {}", deps.replace('\n', "\n    "))
            } else {
                "".to_string()
            };

            let def_span = current_def
                .and_then(|def| def.to_rustc_span(self.cx, &mut self.span_regen))
                .unwrap_or(DUMMY_SP);

            let msg = [prefix, msg.to_string(), suffix].concat();
            match level {
                DiagLevel::Bug(_) | DiagLevel::Error => {
                    let mut err = self.sess.struct_span_err(def_span, msg);
                    for use_origin in use_stack_for_def.iter().rev() {
                        use_origin.note(self.cx, &mut self.span_regen, &mut err);
                    }
                    self.overall_result = Err(err.emit());
                }
                DiagLevel::Warning => {
                    let mut warn = self.sess.struct_span_warn(def_span, msg);
                    for use_origin in use_stack_for_def.iter().rev() {
                        use_origin.note(self.cx, &mut self.span_regen, &mut warn);
                    }
                }
            }
            self.any_spirt_bugs = matches!(level, DiagLevel::Bug(_));
        }
    }
}

impl Visitor<'_> for DiagnosticReporter<'_> {
    fn visit_attr_set_use(&mut self, attrs: AttrSet) {
        // HACK(eddyb) this avoids reporting the same diagnostics more than once.
        if self.seen_attrs.insert(attrs) {
            self.report_from_attrs(attrs);
        }
    }
    fn visit_type_use(&mut self, ty: Type) {
        if self.seen_types.insert(ty) {
            let ty_def = &self.cx[ty];
            self.use_stack.push(UseOrigin::Global {
                kind: &"type",
                attrs: ty_def.attrs,
            });
            self.visit_type_def(ty_def);
            self.use_stack.pop();
        }
    }
    fn visit_const_use(&mut self, ct: Const) {
        if self.seen_consts.insert(ct) {
            let ct_def = &self.cx[ct];
            self.use_stack.push(UseOrigin::Global {
                kind: &"constant",
                attrs: ct_def.attrs,
            });
            self.visit_const_def(ct_def);
            self.use_stack.pop();
        }
    }

    fn visit_global_var_use(&mut self, gv: GlobalVar) {
        if self.seen_global_vars.insert(gv) {
            let gv_decl = &self.module.global_vars[gv];
            self.use_stack.push(UseOrigin::Global {
                // FIXME(eddyb) may be a `&CONST`, or an interface variable,
                // not necessarily an user variable, so this could be confusing.
                kind: &"global variable",
                attrs: gv_decl.attrs,
            });
            self.visit_global_var_decl(gv_decl);
            self.use_stack.pop();
        }
    }
    fn visit_func_use(&mut self, func: Func) {
        if self.seen_funcs.insert(func) {
            let func_decl = &self.module.funcs[func];
            self.use_stack.push(UseOrigin::IntraFunc {
                func_attrs: func_decl.attrs,
                func_export_key: None,
                inst_attrs: AttrSet::default(),
                origin: IntraFuncUseOrigin::Other,
            });
            self.visit_func_decl(func_decl);
            self.use_stack.pop();
        }
    }

    fn visit_data_inst_def(&mut self, data_inst_def: &DataInstDef) {
        match self.use_stack.last_mut() {
            Some(UseOrigin::IntraFunc { inst_attrs, .. }) => *inst_attrs = data_inst_def.attrs,
            _ => unreachable!(),
        }

        if let DataInstKind::FuncCall(func) = data_inst_def.kind {
            let replace_origin = |this: &mut Self, new_origin| match this.use_stack.last_mut() {
                Some(UseOrigin::IntraFunc { origin, .. }) => mem::replace(origin, new_origin),
                _ => unreachable!(),
            };

            // HACK(eddyb) visit `func` early, to control its `use_stack`, with
            // the later visit from `inner_visit_with` ignored as a duplicate.
            let old_origin = replace_origin(self, IntraFuncUseOrigin::CallCallee);
            self.visit_func_use(func);
            replace_origin(self, old_origin);
        }

        data_inst_def.inner_visit_with(self);
    }
}
