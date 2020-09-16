use super::CodegenCx;
use crate::abi::ConvSpirvType;
use crate::builder_spirv::{SpirvValue, SpirvValueExt};
use crate::spirv_type::SpirvType;
use crate::symbols::{parse_attr, SpirvAttribute};
use rspirv::spirv::{FunctionControl, LinkageType, StorageClass};
use rustc_attr::InlineAttr;
use rustc_codegen_ssa::traits::{DeclareMethods, MiscMethods, PreDefineMethods, StaticMethods};
use rustc_middle::middle::codegen_fn_attrs::CodegenFnAttrFlags;
use rustc_middle::middle::codegen_fn_attrs::CodegenFnAttrs;
use rustc_middle::mir::interpret::ConstValue;
use rustc_middle::mir::mono::MonoItem;
use rustc_middle::mir::mono::{Linkage, Visibility};
use rustc_middle::ty::layout::FnAbiExt;
use rustc_middle::ty::{Instance, ParamEnv, Ty, TypeFoldable};
use rustc_span::def_id::DefId;
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
    pub fn get_fn_ext(&self, instance: Instance<'tcx>) -> SpirvValue {
        assert!(!instance.substs.needs_infer());
        assert!(!instance.substs.has_escaping_bound_vars());

        if let Some(&func) = self.instances.borrow().get(&instance) {
            return func;
        }

        let sym = self.tcx.symbol_name(instance).name;

        let fn_abi = FnAbi::of_instance(self, instance, &[]);

        let llfn = if let Some(llfn) = self.get_declared_value(sym) {
            llfn
        } else {
            // Because we've already declared everything with predefine_fn, if we hit this branch, we're guaranteed to be
            // importing this function from elsewhere. So, slap an extern on it.
            let human_name = format!("{}", instance);
            let rust_attrs = self.tcx.codegen_fn_attrs(instance.def_id());
            let spv_attrs = attrs_to_spirv(rust_attrs);
            self.declare_fn_ext(
                sym,
                Some(&human_name),
                Some(LinkageType::Import),
                spv_attrs,
                &fn_abi,
            )
        };

        self.instances.borrow_mut().insert(instance, llfn);

        llfn
    }

    // The call graph of how this is reachable is a little tangled, so:
    // MiscMethods::get_fn -> get_fn_ext -> declare_fn_ext
    // MiscMethods::get_fn_addr -> get_fn_ext -> declare_fn_ext
    // PreDefineMethods::predefine_fn -> declare_fn_ext
    // DeclareMethods::declare_fn -> declare_fn_ext (as of right now, this is never called)
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
            other => panic!("fn_abi type {}", other.debug(function_type, self)),
        };

        let mut emit = self.emit_global();
        if crate::is_blocklisted_fn(name) {
            // This can happen if we call a blocklisted function in another crate.
            let result = emit.undef(function_type, None);
            self.zombie(result, "called blocklisted fn");
            return result.with_type(function_type);
        }
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

        let result = fn_id.with_type(function_type);
        self.declared_values
            .borrow_mut()
            .insert(name.to_string(), result);
        result
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
        let g = self.declare_global(sym, self.layout_of(ty).spirv_type(self));
        self.instances.borrow_mut().insert(instance, g);
        g
    }
}

impl<'tcx> PreDefineMethods<'tcx> for CodegenCx<'tcx> {
    fn predefine_static(
        &self,
        def_id: DefId,
        _linkage: Linkage,
        _visibility: Visibility,
        symbol_name: &str,
    ) {
        let instance = Instance::mono(self.tcx, def_id);
        let ty = instance.ty(self.tcx, ParamEnv::reveal_all());
        let spvty = self.layout_of(ty).spirv_type(self);

        let g = self.define_global(symbol_name, spvty).unwrap_or_else(|| {
            self.sess().span_fatal(
                self.tcx.def_span(def_id),
                &format!("symbol `{}` is already defined", symbol_name),
            )
        });

        // unsafe {
        //     llvm::LLVMRustSetLinkage(g, base::linkage_to_llvm(linkage));
        //     llvm::LLVMRustSetVisibility(g, base::visibility_to_llvm(visibility));
        // }

        self.instances.borrow_mut().insert(instance, g);
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
        let linkage = match linkage {
            Linkage::External => Some(LinkageType::Export),
            Linkage::Internal => None,
            other => panic!("TODO: Linkage type not supported yet: {:?}", other),
        };
        let rust_attrs = self.tcx.codegen_fn_attrs(instance.def_id());
        let spv_attrs = attrs_to_spirv(rust_attrs);

        let declared =
            self.declare_fn_ext(symbol_name, Some(&human_name), linkage, spv_attrs, &fn_abi);

        for attr in self.tcx.get_attrs(instance.def_id()) {
            if let Some(SpirvAttribute::Entry(execution_model)) = parse_attr(self, attr) {
                let interface = &[];
                self.emit_global().entry_point(
                    execution_model,
                    declared.def,
                    human_name.clone(),
                    interface,
                );
            }
        }

        self.instances.borrow_mut().insert(instance, declared);
    }
}

impl<'tcx> DeclareMethods<'tcx> for CodegenCx<'tcx> {
    fn declare_global(&self, name: &str, ty: Self::Type) -> Self::Value {
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
        self.zombie(result.def, "declare_global");
        self.declared_values
            .borrow_mut()
            .insert(name.to_string(), result);
        result
    }

    fn declare_cfn(&self, _name: &str, _fn_type: Self::Type) -> Self::Function {
        todo!()
    }

    fn declare_fn(&self, name: &str, fn_abi: &FnAbi<'tcx, Ty<'tcx>>) -> Self::Function {
        self.declare_fn_ext(name, None, None, FunctionControl::NONE, fn_abi)
    }

    fn define_global(&self, name: &str, ty: Self::Type) -> Option<Self::Value> {
        if self.get_defined_value(name).is_some() {
            None
        } else {
            Some(self.declare_global(name, ty))
        }
    }

    fn define_private_global(&self, _ty: Self::Type) -> Self::Value {
        todo!()
    }

    fn get_declared_value(&self, name: &str) -> Option<Self::Value> {
        self.declared_values.borrow().get(name).copied()
    }

    fn get_defined_value(&self, name: &str) -> Option<Self::Value> {
        self.get_declared_value(name)
        // self.get_declared_value(name).and_then(|val| {
        //     let declaration = unsafe { llvm::LLVMIsDeclaration(val) != 0 };
        //     if !declaration {
        //         Some(val)
        //     } else {
        //         None
        //     }
        // })
    }
}

impl<'tcx> StaticMethods for CodegenCx<'tcx> {
    fn static_addr_of(&self, cv: Self::Value, _align: Align, _kind: Option<&str>) -> Self::Value {
        // TODO: Integrate this into define_static and whatnot?
        if let Some(already_defined) = self.builder.find_global_constant_variable(cv.def) {
            return already_defined;
        }
        let ty = SpirvType::Pointer {
            storage_class: StorageClass::Function,
            pointee: cv.ty,
        }
        .def(self);
        let result = self
            .emit_global()
            .variable(ty, None, StorageClass::Function, Some(cv.def))
            .with_type(ty);
        // TODO: These should be StorageClass::UniformConstant, so just zombie for now.
        self.zombie(result.def, "static_addr_of");
        result
    }

    fn codegen_static(&self, def_id: DefId, _is_mutable: bool) {
        let g = self.get_static(def_id);

        let alloc = match self.tcx.const_eval_poly(def_id) {
            Ok(ConstValue::ByRef { alloc, offset }) if offset.bytes() == 0 => alloc,
            Ok(val) => panic!("static const eval returned {:#?}", val),
            // Error has already been reported
            Err(_) => return,
        };
        let value_ty = match self.lookup_type(g.ty) {
            SpirvType::Pointer { pointee, .. } => pointee,
            other => panic!("global had non-pointer type {}", other.debug(g.ty, self)),
        };
        let mut v = self.create_const_alloc(alloc, value_ty);

        if self.lookup_type(v.ty) == SpirvType::Bool {
            let val = self.builder.lookup_const_bool(v.def).unwrap();
            let val_int = if val { 1 } else { 0 };
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
