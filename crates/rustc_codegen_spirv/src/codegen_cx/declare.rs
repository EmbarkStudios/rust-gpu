use super::CodegenCx;
use crate::abi::ConvSpirvType;
use crate::attr::AggregatedSpirvAttributes;
use crate::builder_spirv::{SpirvConst, SpirvValue, SpirvValueExt};
use crate::custom_decorations::{CustomDecoration, SrcLocDecoration};
use crate::spirv_type::SpirvType;
use itertools::Itertools;
use rspirv::spirv::{FunctionControl, LinkageType, StorageClass, Word};
use rustc_attr::InlineAttr;
use rustc_codegen_ssa::traits::{BaseTypeMethods, PreDefineMethods, StaticMethods};
use rustc_hir::def::DefKind;
use rustc_middle::bug;
use rustc_middle::middle::codegen_fn_attrs::{CodegenFnAttrFlags, CodegenFnAttrs};
use rustc_middle::mir::mono::{Linkage, MonoItem, Visibility};
use rustc_middle::ty::layout::{FnAbiOf, LayoutOf};
use rustc_middle::ty::{self, Instance, ParamEnv, TypeVisitableExt};
use rustc_span::def_id::DefId;
use rustc_span::Span;
use rustc_target::abi::Align;

fn attrs_to_spirv(attrs: &CodegenFnAttrs) -> FunctionControl {
    let mut control = FunctionControl::NONE;
    match attrs.inline {
        InlineAttr::None => (),
        InlineAttr::Hint | InlineAttr::Always => control.insert(FunctionControl::INLINE),
        InlineAttr::Never => control.insert(FunctionControl::DONT_INLINE),
    }
    if attrs.flags.contains(CodegenFnAttrFlags::FFI_PURE) {
        control.insert(FunctionControl::PURE);
    }
    if attrs.flags.contains(CodegenFnAttrFlags::FFI_CONST) {
        control.insert(FunctionControl::CONST);
    }
    control
}

impl<'tcx> CodegenCx<'tcx> {
    /// Returns a function if it already exists, or declares a header if it doesn't.
    pub fn get_fn_ext(&self, instance: Instance<'tcx>) -> SpirvValue {
        assert!(!instance.substs.has_infer());
        assert!(!instance.substs.has_escaping_bound_vars());

        if let Some(&func) = self.instances.borrow().get(&instance) {
            return func;
        }

        // Because we've already declared everything with predefine_fn, if we hit this branch, we're guaranteed to be
        // importing this function from elsewhere. So, slap an extern on it.
        let linkage = Some(LinkageType::Import);
        let llfn = self.declare_fn_ext(instance, linkage);

        self.instances.borrow_mut().insert(instance, llfn);

        llfn
    }

    // The call graph of how this is reachable is a little tangled, so:
    // MiscMethods::get_fn -> get_fn_ext -> declare_fn_ext
    // MiscMethods::get_fn_addr -> get_fn_ext -> declare_fn_ext
    // PreDefineMethods::predefine_fn -> declare_fn_ext
    fn declare_fn_ext(&self, instance: Instance<'tcx>, linkage: Option<LinkageType>) -> SpirvValue {
        let control = attrs_to_spirv(self.tcx.codegen_fn_attrs(instance.def_id()));
        let fn_abi = self.fn_abi_of_instance(instance, ty::List::empty());
        let span = self.tcx.def_span(instance.def_id());
        let function_type = fn_abi.spirv_type(span, self);
        let (return_type, argument_types) = match self.lookup_type(function_type) {
            SpirvType::Function {
                return_type,
                arguments,
            } => (return_type, arguments),
            other => bug!("fn_abi type {}", other.debug(function_type, self)),
        };

        if crate::is_blocklisted_fn(self.tcx, &self.sym, instance) {
            // This can happen if we call a blocklisted function in another crate.
            let result = self.undef(function_type);
            self.zombie_with_span(result.def_cx(self), span, "called blocklisted fn");
            return result;
        }
        let fn_id = {
            let mut emit = self.emit_global();
            let fn_id = emit
                .begin_function(return_type, None, control, function_type)
                .unwrap();
            if linkage != Some(LinkageType::Import) {
                let parameter_values = argument_types
                    .iter()
                    .map(|&ty| emit.function_parameter(ty).unwrap().with_type(ty))
                    .collect::<Vec<_>>();
                self.function_parameter_values
                    .borrow_mut()
                    .insert(fn_id, parameter_values);
            }
            emit.end_function().unwrap();
            fn_id
        };

        // HACK(eddyb) this is a temporary workaround due to our use of `rspirv`,
        // which prevents us from attaching `OpLine`s to `OpFunction` definitions,
        // but we can use our custom `SrcLocDecoration` instead.
        let src_loc_inst = SrcLocDecoration::from_rustc_span(
            self.tcx.def_ident_span(instance.def_id()).unwrap_or(span),
            &self.builder,
        )
        .map(|src_loc| src_loc.encode_to_inst(fn_id));
        self.emit_global()
            .module_mut()
            .annotations
            .extend(src_loc_inst);

        // HACK(eddyb) this is a bit roundabout, but the easiest way to get a
        // fully absolute path that contains at least as much information as
        // `instance.to_string()` (at least with `-C symbol-mangling-version=v0`).
        // While we could use the mangled symbol instead, like we do for linkage,
        // `OpName` is more of a debugging aid, so not having to separately
        // demangle the SPIR-V can help. However, if some tools assume `OpName`
        // is always a valid identifier, we may have to offer the mangled name
        // (as some sort of opt-in, or toggled based on the platform, etc.).
        let symbol_name = self.tcx.symbol_name(instance).name;
        let demangled_symbol_name = format!("{:#}", rustc_demangle::demangle(symbol_name));
        self.emit_global().name(fn_id, &demangled_symbol_name);

        if let Some(linkage) = linkage {
            self.set_linkage(fn_id, symbol_name.to_owned(), linkage);
        }

        let declared = fn_id.with_type(function_type);

        let attrs = AggregatedSpirvAttributes::parse(
            self,
            match self.tcx.def_kind(instance.def_id()) {
                // This was made to ICE cross-crate at some point, but then got
                // reverted in https://github.com/rust-lang/rust/pull/111381.
                // FIXME(eddyb) remove this workaround once we rustup past that.
                DefKind::Closure => &[],
                _ => self.tcx.get_attrs_unchecked(instance.def_id()),
            },
        );
        if let Some(entry) = attrs.entry.map(|attr| attr.value) {
            let entry_name = entry
                .name
                .as_ref()
                .map_or_else(|| instance.to_string(), ToString::to_string);
            self.entry_stub(&instance, fn_abi, declared, entry_name, entry);
        }
        if attrs.buffer_load_intrinsic.is_some() {
            let mode = &fn_abi.ret.mode;
            self.buffer_load_intrinsic_fn_id
                .borrow_mut()
                .insert(fn_id, mode);
        }
        if attrs.buffer_store_intrinsic.is_some() {
            let mode = &fn_abi.args.last().unwrap().mode;
            self.buffer_store_intrinsic_fn_id
                .borrow_mut()
                .insert(fn_id, mode);
        }

        let instance_def_id = instance.def_id();

        if self.tcx.crate_name(instance_def_id.krate) == self.sym.libm {
            let item_name = self.tcx.item_name(instance_def_id);
            let intrinsic = self.sym.libm_intrinsics.get(&item_name);
            if self.tcx.visibility(instance.def_id()) == ty::Visibility::Public {
                match intrinsic {
                    Some(&intrinsic) => {
                        self.libm_intrinsics.borrow_mut().insert(fn_id, intrinsic);
                    }
                    None => {
                        self.tcx.sess.err(format!(
                            "missing libm intrinsic {symbol_name}, which is {instance}"
                        ));
                    }
                }
            }
        }

        if [
            self.tcx.lang_items().panic_fn(),
            self.tcx.lang_items().panic_fmt(),
        ]
        .contains(&Some(instance_def_id))
        {
            self.panic_entry_point_ids.borrow_mut().insert(fn_id);
        }

        // HACK(eddyb) there is no good way to identify these definitions
        // (e.g. no `#[lang = "..."]` attribute), but this works well enough.
        if [
            "<core::fmt::Arguments>::new_v1",
            "<core::fmt::Arguments>::new_const",
        ]
        .contains(&&demangled_symbol_name[..])
        {
            self.fmt_args_new_fn_ids.borrow_mut().insert(fn_id);
        }

        // HACK(eddyb) there is no good way to identify these definitions
        // (e.g. no `#[lang = "..."]` attribute), but this works well enough.
        if let Some(suffix) = demangled_symbol_name.strip_prefix("<core::fmt::rt::Argument>::new_")
        {
            let spec = suffix.split_once("::<").and_then(|(method_suffix, _)| {
                Some(match method_suffix {
                    "display" => ' ',
                    "debug" => '?',
                    "octal" => 'o',
                    "lower_hex" => 'x',
                    "upper_hex" => 'X',
                    "pointer" => 'p',
                    "binary" => 'b',
                    "lower_exp" => 'e',
                    "upper_exp" => 'E',
                    _ => return None,
                })
            });
            if let Some(spec) = spec {
                if let Some((ty,)) = instance.substs.types().collect_tuple() {
                    self.fmt_rt_arg_new_fn_ids_to_ty_and_spec
                        .borrow_mut()
                        .insert(fn_id, (ty, spec));
                }
            }
        }

        declared
    }

    pub fn get_static(&self, def_id: DefId) -> SpirvValue {
        let instance = Instance::mono(self.tcx, def_id);
        if let Some(&g) = self.instances.borrow().get(&instance) {
            return g;
        }

        let defined_in_current_codegen_unit = self
            .codegen_unit
            .items()
            .contains_key(&MonoItem::Static(def_id));
        assert!(
            !defined_in_current_codegen_unit,
            "get_static() should always hit the cache for statics defined in the same CGU, but did not for `{def_id:?}`"
        );

        let ty = instance.ty(self.tcx, ParamEnv::reveal_all());
        let sym = self.tcx.symbol_name(instance).name;
        let span = self.tcx.def_span(def_id);
        let g = self.declare_global(span, self.layout_of(ty).spirv_type(span, self));
        self.instances.borrow_mut().insert(instance, g);
        self.set_linkage(g.def_cx(self), sym.to_string(), LinkageType::Import);
        g
    }

    fn declare_global(&self, span: Span, ty: Word) -> SpirvValue {
        let ptr_ty = SpirvType::Pointer { pointee: ty }.def(span, self);
        // FIXME(eddyb) figure out what the correct storage class is.
        let result = self
            .emit_global()
            .variable(ptr_ty, None, StorageClass::Private, None)
            .with_type(ptr_ty);
        // TODO: These should be StorageClass::Private, so just zombie for now.
        // FIXME(eddyb) why zombie? this looks like it should just work nowadays.
        self.zombie_with_span(result.def_cx(self), span, "globals are not supported yet");
        result
    }
}

impl<'tcx> PreDefineMethods<'tcx> for CodegenCx<'tcx> {
    fn predefine_static(
        &self,
        def_id: DefId,
        linkage: Linkage,
        _visibility: Visibility,
        symbol_name: &str,
    ) {
        let instance = Instance::mono(self.tcx, def_id);
        let ty = instance.ty(self.tcx, ParamEnv::reveal_all());
        let span = self.tcx.def_span(def_id);
        let spvty = self.layout_of(ty).spirv_type(span, self);
        let linkage = match linkage {
            Linkage::External => Some(LinkageType::Export),
            Linkage::Internal => None,
            other => {
                self.tcx.sess.err(format!(
                    "TODO: Linkage type {other:?} not supported yet for static var symbol {symbol_name}"
                ));
                None
            }
        };

        let g = self.declare_global(span, spvty);

        self.instances.borrow_mut().insert(instance, g);
        if let Some(linkage) = linkage {
            self.set_linkage(g.def_cx(self), symbol_name.to_string(), linkage);
        }
    }

    fn predefine_fn(
        &self,
        instance: Instance<'tcx>,
        linkage: Linkage,
        _visibility: Visibility,
        symbol_name: &str,
    ) {
        let linkage2 = match linkage {
            // super sketchy hack: memcpy, memmove, memset, memcmp, and bcmp in the
            // compiler_builtins crate use the WeakAny linkage type. Treat it as actually External
            // linkage because we know there's only one of them.
            Linkage::External | Linkage::WeakAny => Some(LinkageType::Export),
            Linkage::Internal => None,
            other => {
                self.tcx.sess.err(format!(
                    "TODO: Linkage type {other:?} not supported yet for function symbol {symbol_name}"
                ));
                None
            }
        };
        let declared = self.declare_fn_ext(instance, linkage2);

        self.instances.borrow_mut().insert(instance, declared);
    }
}

impl<'tcx> StaticMethods for CodegenCx<'tcx> {
    fn static_addr_of(&self, cv: Self::Value, _align: Align, _kind: Option<&str>) -> Self::Value {
        self.def_constant(
            self.type_ptr_to(cv.ty),
            SpirvConst::PtrTo {
                pointee: cv.def_cx(self),
            },
        )
    }

    fn codegen_static(&self, def_id: DefId, _is_mutable: bool) {
        let g = self.get_static(def_id);

        let alloc = match self.tcx.eval_static_initializer(def_id) {
            Ok(alloc) => alloc,
            // Error has already been reported
            Err(_) => return,
        };
        let value_ty = match self.lookup_type(g.ty) {
            SpirvType::Pointer { pointee } => pointee,
            other => self.tcx.sess.fatal(format!(
                "global had non-pointer type {}",
                other.debug(g.ty, self)
            )),
        };
        let v = self.create_const_alloc(alloc, value_ty);
        assert_ty_eq!(self, value_ty, v.ty);
        self.builder
            .set_global_initializer(g.def_cx(self), v.def_cx(self));
    }

    /// Mark the given global value as "used", to prevent the compiler and linker from potentially
    /// removing a static variable that may otherwise appear unused.
    fn add_used_global(&self, _global: Self::Value) {
        // TODO: Ignore for now.
    }

    /// Same as `add_used_global`, but only prevent the compiler from potentially removing an
    /// otherwise unused symbol. The linker is still permitted to drop it.
    ///
    /// This corresponds to the semantics of the `#[used]` attribute.
    fn add_compiler_used_global(&self, _global: Self::Value) {
        // TODO: Ignore for now.
    }
}
