use super::CodegenCx;
use crate::abi::ConvSpirvType;
use crate::builder_spirv::{SpirvConst, SpirvValue, SpirvValueExt};
use crate::spirv_type::SpirvType;
use crate::symbols::{parse_attrs, SpirvAttribute};
use rspirv::spirv::{FunctionControl, LinkageType, StorageClass, Word};
use rustc_attr::InlineAttr;
use rustc_codegen_ssa::traits::{PreDefineMethods, StaticMethods};
use rustc_middle::bug;
use rustc_middle::middle::codegen_fn_attrs::{CodegenFnAttrFlags, CodegenFnAttrs};
use rustc_middle::mir::mono::{Linkage, MonoItem, Visibility};
use rustc_middle::ty::layout::FnAbiExt;
use rustc_middle::ty::{Instance, ParamEnv, Ty, TypeFoldable};
use rustc_span::def_id::DefId;
use rustc_span::Span;
use rustc_target::abi::call::FnAbi;
use rustc_target::abi::{Align, LayoutOf};

fn attrs_to_spirv(attrs: &CodegenFnAttrs) -> FunctionControl {
    let mut control = FunctionControl::NONE;
    match attrs.inline {
        InlineAttr::None => (),
        InlineAttr::Hint => control.insert(FunctionControl::INLINE),
        InlineAttr::Always => control.insert(FunctionControl::INLINE),
        InlineAttr::Never => control.insert(FunctionControl::DONT_INLINE),
    }
    if attrs.flags.contains(CodegenFnAttrFlags::FFI_PURE) {
        control.insert(FunctionControl::PURE)
    }
    if attrs.flags.contains(CodegenFnAttrFlags::FFI_CONST) {
        control.insert(FunctionControl::CONST)
    }
    control
}

impl<'tcx> CodegenCx<'tcx> {
    /// Returns a function if it already exists, or declares a header if it doesn't.
    pub fn get_fn_ext(&self, instance: Instance<'tcx>) -> SpirvValue {
        assert!(!instance.substs.needs_infer());
        assert!(!instance.substs.has_escaping_bound_vars());

        if let Some(&func) = self.instances.borrow().get(&instance) {
            return func;
        }

        let sym = self.tcx.symbol_name(instance).name;
        // Because we've already declared everything with predefine_fn, if we hit this branch, we're guaranteed to be
        // importing this function from elsewhere. So, slap an extern on it.
        let human_name = format!("{}", instance);
        let linkage = Some(LinkageType::Import);
        let attrs = attrs_to_spirv(self.tcx.codegen_fn_attrs(instance.def_id()));
        let fn_abi = FnAbi::of_instance(self, instance, &[]);
        let llfn = self.declare_fn_ext(sym, Some(&human_name), linkage, attrs, &fn_abi);

        self.instances.borrow_mut().insert(instance, llfn);

        llfn
    }

    // The call graph of how this is reachable is a little tangled, so:
    // MiscMethods::get_fn -> get_fn_ext -> declare_fn_ext
    // MiscMethods::get_fn_addr -> get_fn_ext -> declare_fn_ext
    // PreDefineMethods::predefine_fn -> declare_fn_ext
    fn declare_fn_ext(
        &self,
        name: &str,
        human_name: Option<&str>,
        linkage: Option<LinkageType>,
        control: FunctionControl,
        fn_abi: &FnAbi<'tcx, Ty<'tcx>>,
    ) -> SpirvValue {
        let function_type = fn_abi.spirv_type(self);
        let (return_type, argument_types) = match self.lookup_type(function_type) {
            SpirvType::Function {
                return_type,
                arguments,
            } => (return_type, arguments),
            other => bug!("fn_abi type {}", other.debug(function_type, self)),
        };

        if crate::is_blocklisted_fn(name) {
            // This can happen if we call a blocklisted function in another crate.
            let result = self.undef(function_type);
            // TODO: Span info here
            self.zombie_no_span(result.def, "called blocklisted fn");
            return result;
        }
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
        match human_name {
            Some(human_name) => emit.name(fn_id, human_name),
            None => emit.name(fn_id, name),
        }
        drop(emit); // set_linkage uses emit
        if let Some(linkage) = linkage {
            self.set_linkage(fn_id, name.to_owned(), linkage);
        }

        fn_id.with_type(function_type)
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
            "get_static() should always hit the cache for statics defined in the same CGU, but did not for `{:?}`",
            def_id
        );

        let ty = instance.ty(self.tcx, ParamEnv::reveal_all());
        let sym = self.tcx.symbol_name(instance).name;
        let span = self.tcx.def_span(def_id);
        let g = self.declare_global(span, self.layout_of(ty).spirv_type(self));
        self.instances.borrow_mut().insert(instance, g);
        self.set_linkage(g.def, sym.to_string(), LinkageType::Import);
        g
    }

    fn declare_global(&self, span: Span, ty: Word) -> SpirvValue {
        let ptr_ty = SpirvType::Pointer {
            storage_class: StorageClass::Function,
            pointee: ty,
        }
        .def(self);
        let result = self
            .emit_global()
            .variable(ptr_ty, None, StorageClass::Function, None)
            .with_type(ptr_ty);
        // TODO: These should be StorageClass::Private, so just zombie for now.
        self.zombie_with_span(result.def, span, "Globals are not supported yet");
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
        let spvty = self.layout_of(ty).spirv_type(self);
        let linkage = match linkage {
            Linkage::External => Some(LinkageType::Export),
            Linkage::Internal => None,
            other => self.tcx.sess.fatal(&format!(
                "TODO: Linkage type not supported yet: {:?}",
                other
            )),
        };

        let span = self.tcx.def_span(def_id);
        let g = self.declare_global(span, spvty);

        // unsafe {
        //     llvm::LLVMRustSetLinkage(g, base::linkage_to_llvm(linkage));
        //     llvm::LLVMRustSetVisibility(g, base::visibility_to_llvm(visibility));
        // }

        self.instances.borrow_mut().insert(instance, g);
        if let Some(linkage) = linkage {
            self.set_linkage(g.def, symbol_name.to_string(), linkage);
        }
    }

    fn predefine_fn(
        &self,
        instance: Instance<'tcx>,
        linkage: Linkage,
        _visibility: Visibility,
        symbol_name: &str,
    ) {
        let fn_abi = FnAbi::of_instance(self, instance, &[]);
        let human_name = format!("{}", instance);
        let linkage2 = match linkage {
            Linkage::External => Some(LinkageType::Export),
            Linkage::Internal => None,
            other => self.tcx.sess.fatal(&format!(
                "TODO: Linkage type not supported yet: {:?}",
                other
            )),
        };
        let rust_attrs = self.tcx.codegen_fn_attrs(instance.def_id());
        let spv_attrs = attrs_to_spirv(rust_attrs);

        let declared =
            self.declare_fn_ext(symbol_name, Some(&human_name), linkage2, spv_attrs, &fn_abi);

        for attr in parse_attrs(self, self.tcx.get_attrs(instance.def_id())) {
            match attr {
                SpirvAttribute::Entry(execution_model) => self.entry_stub(
                    &instance,
                    &fn_abi,
                    declared,
                    human_name.clone(),
                    execution_model,
                ),
                SpirvAttribute::ReallyUnsafeIgnoreBitcasts => {
                    self.really_unsafe_ignore_bitcasts
                        .borrow_mut()
                        .insert(declared);
                }
                _ => {}
            }
        }

        self.instances.borrow_mut().insert(instance, declared);
    }
}

impl<'tcx> StaticMethods for CodegenCx<'tcx> {
    fn static_addr_of(&self, cv: Self::Value, _align: Align, _kind: Option<&str>) -> Self::Value {
        self.register_constant_pointer(cv)
    }

    fn codegen_static(&self, def_id: DefId, _is_mutable: bool) {
        let g = self.get_static(def_id);

        let alloc = match self.tcx.eval_static_initializer(def_id) {
            Ok(alloc) => alloc,
            // Error has already been reported
            Err(_) => return,
        };
        let value_ty = match self.lookup_type(g.ty) {
            SpirvType::Pointer { pointee, .. } => pointee,
            other => self.tcx.sess.fatal(&format!(
                "global had non-pointer type {}",
                other.debug(g.ty, self)
            )),
        };
        let mut v = self.create_const_alloc(alloc, value_ty);

        if self.lookup_type(v.ty) == SpirvType::Bool {
            let val = self.builder.lookup_const(v).unwrap();
            let val_int = match val {
                SpirvConst::Bool(_, false) => 0,
                SpirvConst::Bool(_, true) => 0,
                _ => bug!(),
            };
            v = self.constant_u8(val_int);
        }

        assert_ty_eq!(self, value_ty, v.ty);
        self.builder.set_global_initializer(g.def, v.def);
    }

    /// Mark the given global value as "used", to prevent a backend from potentially removing a
    /// static variable that may otherwise appear unused.
    ///
    /// Static variables in Rust can be annotated with the `#[used]` attribute to direct the `rustc`
    /// compiler to mark the variable as a "used global".
    fn add_used_global(&self, _global: Self::Value) {
        // TODO: Ignore for now.
    }
}
