//! Welcome to the API documentation for the `rust-gpu` project, this API is
//! unstable and mainly intended for developing on the project itself. This is
//! the API documentation for `rustc_codegen_spirv` which is not that useful on
//! its own. You might also be interested in the following crates. There's also
//! the [Rust GPU Dev Guide][gpu-dev-guide] which contains more user-level
//! information on how to use and setup `rust-gpu`.
//!
//! - [`spirv-builder`]
//! - [`spirv-std`]
//! - [`spirv-tools`]
//! - [`spirv-tools-sys`]
//!
//! [gpu-dev-guide]: https://embarkstudios.github.io/rust-gpu/book
//! [`spirv-builder`]: https://embarkstudios.github.io/rust-gpu/api/spirv_builder
//! [`spirv-std`]: https://embarkstudios.github.io/rust-gpu/api/spirv_std
//! [`spirv-tools`]: https://embarkstudios.github.io/rust-gpu/api/spirv_tools
//! [`spirv-tools-sys`]: https://embarkstudios.github.io/rust-gpu/api/spirv_tools_sys
#![feature(rustc_private)]
#![feature(assert_matches)]
#![feature(result_flattening)]
#![feature(lint_reasons)]
#![feature(once_cell)]
// crate-specific exceptions:
#![allow(
    unsafe_code,                // rustc_codegen_ssa requires unsafe functions in traits to be impl'd
    clippy::match_on_vec_items, // rustc_codegen_spirv has less strict panic requirements than other embark projects
    clippy::enum_glob_use,      // pretty useful pattern with some codegen'd enums (e.g. rspirv::spirv::Op)
    clippy::todo,               // still lots to implement :)
)]

// Unfortunately, this will not fail fast when compiling, but rather will wait for
// rustc_codegen_spirv to be compiled. Putting this in build.rs will solve that problem, however,
// that creates the much worse problem that then running `cargo check` will cause
// rustc_codegen_spirv to be *compiled* instead of merely checked, something that takes
// significantly longer. So, the trade-off between detecting a bad configuration slower for a
// faster `cargo check` is worth it.
#[cfg(all(feature = "use-compiled-tools", feature = "use-installed-tools"))]
compile_error!(
    "Either \"use-compiled-tools\" (enabled by default) or \"use-installed-tools\" may be enabled."
);

extern crate rustc_apfloat;
extern crate rustc_ast;
extern crate rustc_attr;
extern crate rustc_codegen_ssa;
extern crate rustc_data_structures;
extern crate rustc_driver;
extern crate rustc_errors;
extern crate rustc_hir;
extern crate rustc_index;
extern crate rustc_interface;
extern crate rustc_metadata;
extern crate rustc_middle;
extern crate rustc_session;
extern crate rustc_span;
extern crate rustc_target;

macro_rules! assert_ty_eq {
    ($codegen_cx:expr, $left:expr, $right:expr) => {
        assert!(
            $left == $right,
            "Expected types to be equal:\n{}\n==\n{}",
            $codegen_cx.debug_type($left),
            $codegen_cx.debug_type($right)
        )
    };
}

mod abi;
mod attr;
mod builder;
mod builder_spirv;
mod codegen_cx;
mod decorations;
mod link;
mod linker;
mod spirv_type;
mod spirv_type_constraints;
mod symbols;
mod target;
mod target_feature;

use builder::Builder;
use codegen_cx::CodegenCx;
use rspirv::binary::Assemble;
use rustc_ast::expand::allocator::AllocatorKind;
use rustc_codegen_ssa::back::lto::{LtoModuleCodegen, SerializedModule, ThinModule};
use rustc_codegen_ssa::back::write::{
    CodegenContext, FatLTOInput, ModuleConfig, OngoingCodegen, TargetMachineFactoryConfig,
};
use rustc_codegen_ssa::base::maybe_create_entry_wrapper;
use rustc_codegen_ssa::mono_item::MonoItemExt;
use rustc_codegen_ssa::traits::{
    CodegenBackend, ExtraBackendMethods, ModuleBufferMethods, ThinBufferMethods,
    WriteBackendMethods,
};
use rustc_codegen_ssa::{CodegenResults, CompiledModule, ModuleCodegen, ModuleKind};
use rustc_data_structures::fx::FxHashMap;
use rustc_errors::{ErrorGuaranteed, FatalError, Handler};
use rustc_metadata::EncodedMetadata;
use rustc_middle::dep_graph::{WorkProduct, WorkProductId};
use rustc_middle::mir::mono::{Linkage, MonoItem, Visibility};
use rustc_middle::mir::pretty::write_mir_pretty;
use rustc_middle::ty::print::with_no_trimmed_paths;
use rustc_middle::ty::{self, query, DefIdTree, Instance, InstanceDef, TyCtxt};
use rustc_session::config::{self, OutputFilenames, OutputType};
use rustc_session::Session;
use rustc_span::symbol::{sym, Symbol};
use rustc_target::spec::{Target, TargetTriple};
use std::any::Any;
use std::fs::{create_dir_all, File};
use std::io::Cursor;
use std::io::Write;
use std::path::Path;
use std::sync::Arc;

fn dump_mir<'tcx>(
    tcx: TyCtxt<'tcx>,
    mono_items: &[(MonoItem<'_>, (Linkage, Visibility))],
    path: &Path,
) {
    create_dir_all(path.parent().unwrap()).unwrap();
    let mut file = File::create(path).unwrap();
    for &(mono_item, (_, _)) in mono_items {
        if let MonoItem::Fn(instance) = mono_item {
            if matches!(instance.def, InstanceDef::Item(_)) {
                let mut mir = Cursor::new(Vec::new());
                if write_mir_pretty(tcx, Some(instance.def_id()), &mut mir).is_ok() {
                    writeln!(file, "{}", String::from_utf8(mir.into_inner()).unwrap()).unwrap();
                }
            }
        }
    }
}

fn is_blocklisted_fn<'tcx>(
    tcx: TyCtxt<'tcx>,
    sym: &symbols::Symbols,
    instance: Instance<'tcx>,
) -> bool {
    // TODO: These sometimes have a constant value of an enum variant with a hole
    if let InstanceDef::Item(def) = instance.def {
        if let Some(debug_trait_def_id) = tcx.get_diagnostic_item(sym::Debug) {
            // Helper for detecting `<_ as core::fmt::Debug>::fmt` (in impls).
            let is_debug_fmt_method = |def_id| match tcx.opt_associated_item(def_id) {
                Some(assoc) if assoc.ident(tcx).name == sym::fmt => match assoc.container {
                    ty::ImplContainer => {
                        let impl_def_id = assoc.container_id(tcx);
                        tcx.impl_trait_ref(impl_def_id).map(|tr| tr.def_id)
                            == Some(debug_trait_def_id)
                    }
                    ty::TraitContainer => false,
                },
                _ => false,
            };

            if is_debug_fmt_method(def.did) {
                return true;
            }

            if tcx.opt_item_ident(def.did).map(|i| i.name) == Some(sym.fmt_decimal) {
                if let Some(parent_def_id) = tcx.opt_parent(def.did) {
                    if is_debug_fmt_method(parent_def_id) {
                        return true;
                    }
                }
            }
        }
    }

    false
}

// TODO: Should this store Vec or Module?
struct SpirvModuleBuffer(Vec<u32>);

impl ModuleBufferMethods for SpirvModuleBuffer {
    fn data(&self) -> &[u8] {
        spirv_tools::binary::from_binary(&self.0)
    }
}

// TODO: Should this store Vec or Module?
struct SpirvThinBuffer(Vec<u32>);

impl ThinBufferMethods for SpirvThinBuffer {
    fn data(&self) -> &[u8] {
        spirv_tools::binary::from_binary(&self.0)
    }
}

#[derive(Clone)]
struct SpirvCodegenBackend;

impl CodegenBackend for SpirvCodegenBackend {
    fn target_features(&self, sess: &Session, _allow_unstable: bool) -> Vec<Symbol> {
        let cmdline = sess.opts.cg.target_feature.split(',');
        let cfg = sess.target.options.features.split(',');
        cfg.chain(cmdline)
            .filter(|l| l.starts_with('+'))
            .map(|l| &l[1..])
            .filter(|l| !l.is_empty())
            .map(Symbol::intern)
            .collect()
    }

    fn target_override(&self, opts: &config::Options) -> Option<Target> {
        match opts.target_triple {
            TargetTriple::TargetTriple(ref target) => target
                .parse::<target::SpirvTarget>()
                .map(|target| target.rustc_target())
                .ok(),
            TargetTriple::TargetJson { .. } => None,
        }
    }

    fn provide(&self, providers: &mut query::Providers) {
        // FIXME(eddyb) this is currently only passed back to us, specifically
        // into `target_machine_factory` (which is a noop), but it might make
        // sense to move some of the target feature parsing into here.
        providers.global_backend_features = |_tcx, ()| vec![];

        crate::abi::provide(providers);
        crate::attr::provide(providers);
    }

    fn provide_extern(&self, providers: &mut query::ExternProviders) {
        crate::abi::provide_extern(providers);
    }

    fn codegen_crate(
        &self,
        tcx: TyCtxt<'_>,
        metadata: EncodedMetadata,
        need_metadata_module: bool,
    ) -> Box<dyn Any> {
        Box::new(rustc_codegen_ssa::base::codegen_crate(
            Self,
            tcx,
            tcx.sess
                .opts
                .cg
                .target_cpu
                .clone()
                .unwrap_or_else(|| tcx.sess.target.cpu.to_string()),
            metadata,
            need_metadata_module,
        ))
    }

    fn join_codegen(
        &self,
        ongoing_codegen: Box<dyn Any>,
        sess: &Session,
        _outputs: &OutputFilenames,
    ) -> Result<(CodegenResults, FxHashMap<WorkProductId, WorkProduct>), ErrorGuaranteed> {
        let (codegen_results, work_products) = ongoing_codegen
            .downcast::<OngoingCodegen<Self>>()
            .expect("Expected OngoingCodegen, found Box<Any>")
            .join(sess);

        sess.compile_status()?;

        Ok((codegen_results, work_products))
    }

    fn link(
        &self,
        sess: &Session,
        codegen_results: CodegenResults,
        outputs: &OutputFilenames,
    ) -> Result<(), ErrorGuaranteed> {
        let timer = sess.timer("link_crate");
        link::link(
            sess,
            &codegen_results,
            outputs,
            codegen_results.crate_info.local_crate_name.as_str(),
        );
        drop(timer);

        sess.compile_status()?;
        Ok(())
    }
}

impl WriteBackendMethods for SpirvCodegenBackend {
    type Module = Vec<u32>;
    type TargetMachine = ();
    type ModuleBuffer = SpirvModuleBuffer;
    type ThinData = ();
    type ThinBuffer = SpirvThinBuffer;

    fn run_link(
        _cgcx: &CodegenContext<Self>,
        _diag_handler: &Handler,
        _modules: Vec<ModuleCodegen<Self::Module>>,
    ) -> Result<ModuleCodegen<Self::Module>, FatalError> {
        todo!()
    }

    fn run_fat_lto(
        _: &CodegenContext<Self>,
        _: Vec<FatLTOInput<Self>>,
        _: Vec<(SerializedModule<Self::ModuleBuffer>, WorkProduct)>,
    ) -> Result<LtoModuleCodegen<Self>, FatalError> {
        todo!()
    }

    fn run_thin_lto(
        cgcx: &CodegenContext<Self>,
        modules: Vec<(String, Self::ThinBuffer)>,
        cached_modules: Vec<(SerializedModule<Self::ModuleBuffer>, WorkProduct)>,
    ) -> Result<(Vec<LtoModuleCodegen<Self>>, Vec<WorkProduct>), FatalError> {
        link::run_thin(cgcx, modules, cached_modules)
    }

    fn print_pass_timings(&self) {
        println!("TODO: Implement print_pass_timings");
    }

    unsafe fn optimize(
        _: &CodegenContext<Self>,
        _: &Handler,
        _: &ModuleCodegen<Self::Module>,
        _: &ModuleConfig,
    ) -> Result<(), FatalError> {
        // TODO: Implement
        Ok(())
    }

    unsafe fn optimize_thin(
        _cgcx: &CodegenContext<Self>,
        thin_module: ThinModule<Self>,
    ) -> Result<ModuleCodegen<Self::Module>, FatalError> {
        let module = ModuleCodegen {
            module_llvm: spirv_tools::binary::to_binary(thin_module.data())
                .unwrap()
                .to_vec(),
            name: thin_module.name().to_string(),
            kind: ModuleKind::Regular,
        };
        Ok(module)
    }

    fn optimize_fat(
        _: &CodegenContext<Self>,
        _: &mut ModuleCodegen<Self::Module>,
    ) -> Result<(), FatalError> {
        todo!()
    }

    unsafe fn codegen(
        cgcx: &CodegenContext<Self>,
        _diag_handler: &Handler,
        module: ModuleCodegen<Self::Module>,
        _config: &ModuleConfig,
    ) -> Result<CompiledModule, FatalError> {
        let path = cgcx
            .output_filenames
            .temp_path(OutputType::Object, Some(&module.name));
        // Note: endianness doesn't matter, readers deduce endianness from magic header.
        let spirv_module = spirv_tools::binary::from_binary(&module.module_llvm);
        File::create(&path)
            .unwrap()
            .write_all(spirv_module)
            .unwrap();
        Ok(CompiledModule {
            name: module.name,
            kind: module.kind,
            object: Some(path),
            dwarf_object: None,
            bytecode: None,
        })
    }

    fn prepare_thin(module: ModuleCodegen<Self::Module>) -> (String, Self::ThinBuffer) {
        (module.name, SpirvThinBuffer(module.module_llvm))
    }

    fn serialize_module(module: ModuleCodegen<Self::Module>) -> (String, Self::ModuleBuffer) {
        (module.name, SpirvModuleBuffer(module.module_llvm))
    }
}

impl ExtraBackendMethods for SpirvCodegenBackend {
    fn codegen_allocator<'tcx>(
        &self,
        _: TyCtxt<'tcx>,
        _: &str,
        _: AllocatorKind,
        _: bool,
    ) -> Self::Module {
        todo!()
    }

    fn compile_codegen_unit(
        &self,
        tcx: TyCtxt<'_>,
        cgu_name: Symbol,
    ) -> (ModuleCodegen<Self::Module>, u64) {
        let _timer = tcx
            .prof
            .verbose_generic_activity_with_arg("codegen_module", cgu_name.to_string());

        // TODO: Do dep_graph stuff
        let cgu = tcx.codegen_unit(cgu_name);

        let cx = CodegenCx::new(tcx, cgu);
        let do_codegen = || {
            let mono_items = cx.codegen_unit.items_in_deterministic_order(cx.tcx);

            if let Some(dir) = &cx.codegen_args.dump_mir {
                dump_mir(tcx, &mono_items, &dir.join(cgu_name.to_string()));
            }

            for &(mono_item, (linkage, visibility)) in mono_items.iter() {
                if let MonoItem::Fn(instance) = mono_item {
                    if is_blocklisted_fn(cx.tcx, &cx.sym, instance) {
                        continue;
                    }
                }
                mono_item.predefine::<Builder<'_, '_>>(&cx, linkage, visibility);
            }

            // ... and now that we have everything pre-defined, fill out those definitions.
            for &(mono_item, _) in mono_items.iter() {
                if let MonoItem::Fn(instance) = mono_item {
                    if is_blocklisted_fn(cx.tcx, &cx.sym, instance) {
                        continue;
                    }
                }
                mono_item.define::<Builder<'_, '_>>(&cx);
            }

            if let Some(_entry) = maybe_create_entry_wrapper::<Builder<'_, '_>>(&cx) {
                // attributes::sanitize(&cx, SanitizerSet::empty(), entry);
            }
        };
        if let Some(path) = &cx.codegen_args.dump_module_on_panic {
            let module_dumper = DumpModuleOnPanic { cx: &cx, path };
            with_no_trimmed_paths!(do_codegen());
            drop(module_dumper);
        } else {
            with_no_trimmed_paths!(do_codegen());
        }
        let spirv_module = cx.finalize_module().assemble();

        (
            ModuleCodegen {
                name: cgu_name.to_string(),
                module_llvm: spirv_module,
                kind: ModuleKind::Regular,
            },
            0,
        )
    }

    fn target_machine_factory(
        &self,
        _sess: &Session,
        _opt_level: config::OptLevel,
        _target_features: &[String],
    ) -> Arc<(dyn Fn(TargetMachineFactoryConfig) -> Result<(), String> + Send + Sync + 'static)>
    {
        Arc::new(|_| Ok(()))
    }
}

struct DumpModuleOnPanic<'a, 'cx, 'tcx> {
    cx: &'cx CodegenCx<'tcx>,
    path: &'a Path,
}

impl Drop for DumpModuleOnPanic<'_, '_, '_> {
    fn drop(&mut self) {
        if std::thread::panicking() {
            if self.path.has_root() {
                self.cx.builder.dump_module(self.path);
            } else {
                println!("{}", self.cx.builder.dump_module_str());
            }
        }
    }
}

/// This is the entrypoint for a hot plugged `rustc_codegen_spirv`
#[no_mangle]
pub fn __rustc_codegen_backend() -> Box<dyn CodegenBackend> {
    // Override rustc's panic hook with our own to override the ICE error
    // message, and direct people to `rust-gpu`.
    let _rustc_hook = std::panic::take_hook();
    let default_hook = std::panic::take_hook();
    {
        // NOTE(eddyb) the reason we can get access to the default panic hook,
        // is that `std::panic::take_hook` has this phrase in its documentation:
        //
        // > If no custom hook is registered, the default hook will be returned.
        //
        // But just in case (races with other threads?), we can do it a few more
        // times, and require that we get the same "boxed" ZST every time.
        let more_hooks = [
            std::panic::take_hook(),
            std::panic::take_hook(),
            std::panic::take_hook(),
            std::panic::take_hook(),
        ];
        assert_eq!(
            std::mem::size_of_val(&*default_hook),
            0,
            "failed to acquire default panic hook using `std::panic::take_hook`, \
             or default panic hook not a ZST anymore"
        );
        #[allow(clippy::vtable_address_comparisons)]
        for other_hook in more_hooks {
            assert!(
                std::ptr::eq(&*default_hook, &*other_hook),
                "failed to acquire default panic hook using `std::panic::take_hook`, \
                 or `std::panic::set_hook` was used on another thread"
            );
        }
    }
    std::panic::set_hook(Box::new(move |panic_info| {
        default_hook(panic_info);
        rustc_driver::report_ice(
            panic_info,
            "https://github.com/EmbarkStudios/rust-gpu/issues/new",
        );
        eprintln!("note: `rust-gpu` version {}\n", env!("CARGO_PKG_VERSION"));
    }));

    Box::new(SpirvCodegenBackend)
}
