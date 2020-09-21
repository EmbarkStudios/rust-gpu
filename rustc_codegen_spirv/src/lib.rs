#![feature(rustc_private)]
#![feature(once_cell)]

extern crate rustc_ast;
extern crate rustc_attr;
extern crate rustc_codegen_ssa;
extern crate rustc_data_structures;
extern crate rustc_driver;
extern crate rustc_errors;
extern crate rustc_hir;
extern crate rustc_incremental;
extern crate rustc_interface;
extern crate rustc_middle;
extern crate rustc_mir;
extern crate rustc_serialize;
extern crate rustc_session;
extern crate rustc_span;
extern crate rustc_symbol_mangling;
extern crate rustc_target;

macro_rules! assert_ty_eq {
    ($codegen_cx:expr, $left:expr, $right:expr) => {
        assert_eq!(
            $left,
            $right,
            "Expected types to be equal:\n{}\n==\n{}",
            $codegen_cx.debug_type($left),
            $codegen_cx.debug_type($right)
        )
    };
}

mod abi;
mod builder;
mod builder_spirv;
mod codegen_cx;
mod finalizing_passes;
mod link;
mod spirv_type;
mod symbols;

#[cfg(test)]
#[path = "../test/lib.rs"]
mod test;

use builder::Builder;
use codegen_cx::CodegenCx;
use rspirv::binary::Assemble;
use rustc_ast::expand::allocator::AllocatorKind;
use rustc_codegen_ssa::back::lto::{LtoModuleCodegen, SerializedModule, ThinModule};
use rustc_codegen_ssa::back::write::{CodegenContext, FatLTOInput, ModuleConfig, OngoingCodegen};
use rustc_codegen_ssa::base::maybe_create_entry_wrapper;
use rustc_codegen_ssa::mono_item::MonoItemExt;
use rustc_codegen_ssa::traits::{
    CodegenBackend, ExtraBackendMethods, ModuleBufferMethods, ThinBufferMethods,
    WriteBackendMethods,
};
use rustc_codegen_ssa::{CodegenResults, CompiledModule, ModuleCodegen, ModuleKind};
use rustc_data_structures::sync::MetadataRef;
use rustc_errors::{ErrorReported, FatalError, Handler};
use rustc_middle::dep_graph::{DepGraph, WorkProduct};
use rustc_middle::middle::cstore::{EncodedMetadata, MetadataLoader, MetadataLoaderDyn};
use rustc_middle::mir::mono::MonoItem;
use rustc_middle::ty::print::with_no_trimmed_paths;
use rustc_middle::ty::query::Providers;
use rustc_middle::ty::{Instance, InstanceDef, TyCtxt};
use rustc_serialize::json;
use rustc_session::config::{self, OptLevel, OutputFilenames, OutputType};
use rustc_session::Session;
use rustc_span::Symbol;
use rustc_target::spec::abi::Abi;
use rustc_target::spec::{LinkerFlavor, PanicStrategy, Target, TargetOptions, TargetTriple};
use std::any::Any;
use std::path::Path;
use std::{fs::File, io::Write, sync::Arc};

fn dump_mir<'tcx>(tcx: TyCtxt<'tcx>, instance: Instance<'tcx>) {
    match instance.def {
        InstanceDef::Item(_) => {
            let mut mir = ::std::io::Cursor::new(Vec::new());
            match rustc_mir::util::write_mir_pretty(tcx, Some(instance.def_id()), &mut mir) {
                Ok(()) => println!("{}", String::from_utf8(mir.into_inner()).unwrap()),
                Err(err) => println!("Couldn't dump MIR for {}: {}", instance, err),
            }
        }
        _ => println!(
            "Couldn't dump MIR for {}, not InstanceDef::Item: {:?}",
            instance, instance.def
        ),
    }
}

fn is_blocklisted_fn(symbol_name: &str) -> bool {
    // TODO: These sometimes have a constant value of an enum variant with a hole
    symbol_name.contains("core..fmt..Debug")
}

fn target_options() -> Target {
    Target {
        llvm_target: "no-llvm".to_string(),
        target_endian: "little".to_string(),
        target_pointer_width: "32".to_string(),
        target_c_int_width: "32".to_string(),
        target_os: "unknown".to_string(),
        target_env: String::new(),
        target_vendor: "unknown".to_string(),
        data_layout: "e-m:e-p:32:32:32-i64:64-n8:16:32:64".to_string(),
        arch: "spirv".to_string(),
        linker_flavor: LinkerFlavor::Ld,
        options: TargetOptions {
            dll_prefix: "".to_string(),
            dll_suffix: ".spv".to_string(),
            panic_strategy: PanicStrategy::Abort,
            emit_debug_gdb_scripts: false,
            allows_weak_linkage: false,
            dynamic_linking: true,
            crt_static_allows_dylibs: true,
            // TODO: Investigate if main_needs_argc_argv is useful (for building exes)
            main_needs_argc_argv: false,
            ..Default::default()
        },
    }
}

// TODO: Should this store Vec or Module?
struct SpirvModuleBuffer(Vec<u32>);

impl ModuleBufferMethods for SpirvModuleBuffer {
    fn data(&self) -> &[u8] {
        crate::slice_u32_to_u8(&self.0)
    }
}

// TODO: Should this store Vec or Module?
struct SpirvThinBuffer(Vec<u32>);

impl ThinBufferMethods for SpirvThinBuffer {
    fn data(&self) -> &[u8] {
        crate::slice_u32_to_u8(&self.0)
    }
}

struct SpirvMetadataLoader;

impl MetadataLoader for SpirvMetadataLoader {
    fn get_rlib_metadata(&self, _: &Target, path: &Path) -> Result<MetadataRef, String> {
        Ok(link::read_metadata(path))
    }

    fn get_dylib_metadata(&self, _: &Target, _: &Path) -> Result<MetadataRef, String> {
        panic!("TODO: implement get_dylib_metadata");
    }
}

#[derive(Clone)]
struct SpirvCodegenBackend;

impl CodegenBackend for SpirvCodegenBackend {
    fn metadata_loader(&self) -> Box<MetadataLoaderDyn> {
        Box::new(SpirvMetadataLoader)
    }

    fn target_override(&self, opts: &config::Options) -> Option<Target> {
        match opts.target_triple {
            TargetTriple::TargetTriple(ref target_triple) => match &**target_triple {
                // TODO: Do we want to match *everything* here, since, well, we only support one thing? that will allow
                // folks to not specify --target spirv-unknown-unknown on the commandline.
                "spirv-unknown-unknown" => Some(target_options()),
                _ => None,
            },
            TargetTriple::TargetPath(_) => None,
        }
    }

    fn provide(&self, providers: &mut Providers) {
        // For now, rustc requires this to be provided.
        providers.supported_target_features = |_, _| Default::default();
        // This is a lil weird: so, we obviously don't support C ABIs at all. However, libcore does declare some extern
        // C functions:
        // https://github.com/rust-lang/rust/blob/5fae56971d8487088c0099c82c0a5ce1638b5f62/library/core/src/slice/cmp.rs#L119
        // However, those functions will be implemented by compiler-builtins:
        // https://github.com/rust-lang/rust/blob/5fae56971d8487088c0099c82c0a5ce1638b5f62/library/core/src/lib.rs#L23-L27
        // This theoretically then should be fine to leave as C, but, there's no backend hook for
        // FnAbi::adjust_for_cabi, causing it to panic:
        // https://github.com/rust-lang/rust/blob/5fae56971d8487088c0099c82c0a5ce1638b5f62/compiler/rustc_target/src/abi/call/mod.rs#L603
        // So, treat any extern "C" functions as actually being Rust ABI, to be able to compile libcore with arch=spirv.
        providers.fn_sig = |tcx, def_id| {
            // We can't capture the old fn_sig and just call that, because fn_sig is a `fn`, not a `Fn`, i.e. it can't
            // capture variables. Fortunately, the defaults are exposed (thanks rustdoc), so use that instead.
            let result = (rustc_interface::DEFAULT_QUERY_PROVIDERS.fn_sig)(tcx, def_id);
            result.map_bound(|mut inner| {
                if inner.abi == Abi::C {
                    inner.abi = Abi::Rust;
                }
                inner
            })
        };
    }

    fn provide_extern(&self, providers: &mut Providers) {
        // See comments in provide(), only this time we use the default *extern* provider.
        providers.fn_sig = |tcx, def_id| {
            let result = (rustc_interface::DEFAULT_EXTERN_QUERY_PROVIDERS.fn_sig)(tcx, def_id);
            result.map_bound(|mut inner| {
                if inner.abi == Abi::C {
                    inner.abi = Abi::Rust;
                }
                inner
            })
        };
    }

    fn codegen_crate<'tcx>(
        &self,
        tcx: rustc_middle::ty::TyCtxt<'tcx>,
        metadata: rustc_middle::middle::cstore::EncodedMetadata,
        need_metadata_module: bool,
    ) -> Box<dyn Any> {
        Box::new(rustc_codegen_ssa::base::codegen_crate(
            SpirvCodegenBackend,
            tcx,
            metadata,
            need_metadata_module,
        ))
    }

    fn join_codegen(
        &self,
        ongoing_codegen: Box<dyn Any>,
        sess: &Session,
        dep_graph: &DepGraph,
    ) -> Result<Box<dyn Any>, ErrorReported> {
        let (codegen_results, work_products) = ongoing_codegen
            .downcast::<OngoingCodegen<SpirvCodegenBackend>>()
            .expect("Expected OngoingCodegen, found Box<Any>")
            .join(sess);
        if sess.opts.debugging_opts.incremental_info {
            rustc_codegen_ssa::back::write::dump_incremental_data(&codegen_results);
        }

        sess.time("serialize_work_products", move || {
            rustc_incremental::save_work_product_index(sess, &dep_graph, work_products)
        });

        sess.compile_status()?;

        Ok(Box::new(codegen_results))
    }

    fn link(
        &self,
        sess: &Session,
        codegen_results: Box<dyn Any>,
        outputs: &OutputFilenames,
    ) -> Result<(), ErrorReported> {
        let codegen_results = codegen_results
            .downcast::<CodegenResults>()
            .expect("Expected CodegenResults, found Box<Any>");

        if sess.opts.debugging_opts.no_link {
            // FIXME: use a binary format to encode the `.rlink` file
            let rlink_data = json::encode(&codegen_results).map_err(|err| {
                sess.fatal(&format!("failed to encode rlink: {}", err));
            })?;
            let rlink_file = outputs.with_extension(config::RLINK_EXT);
            std::fs::write(&rlink_file, rlink_data).map_err(|err| {
                sess.fatal(&format!(
                    "failed to write file {}: {}",
                    rlink_file.display(),
                    err
                ));
            })?;
            return Ok(());
        }

        link::link(
            sess,
            &codegen_results,
            outputs,
            &codegen_results.crate_name.as_str(),
        );

        rustc_incremental::finalize_session_directory(sess, codegen_results.crate_hash);

        Ok(())
    }
}

// Note: endianness doesn't matter, readers deduce endianness from magic header.
fn slice_u32_to_u8(slice: &[u32]) -> &[u8] {
    unsafe {
        std::slice::from_raw_parts(
            slice.as_ptr() as *const u8,
            slice.len() * std::mem::size_of::<u32>(),
        )
    }
}

fn slice_u8_to_u32(slice: &[u8]) -> &[u32] {
    unsafe {
        std::slice::from_raw_parts(
            slice.as_ptr() as *const u32,
            slice.len() / std::mem::size_of::<u32>(),
        )
    }
}

impl WriteBackendMethods for SpirvCodegenBackend {
    type Module = Vec<u32>;
    type TargetMachine = ();
    type ModuleBuffer = SpirvModuleBuffer;
    type Context = ();
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
        thin_module: &mut ThinModule<Self>,
    ) -> Result<ModuleCodegen<Self::Module>, FatalError> {
        let module = ModuleCodegen {
            module_llvm: slice_u8_to_u32(thin_module.data()).to_vec(),
            name: thin_module.name().to_string(),
            kind: ModuleKind::Regular,
        };
        Ok(module)
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
        let spirv_module = slice_u32_to_u8(&module.module_llvm);
        File::create(&path)
            .unwrap()
            .write_all(spirv_module)
            .unwrap();
        if let Some(dump_path) = option_env!("SPIRV_VAL") {
            let status = std::process::Command::new("spirv-val").arg(&path).status();
            let status = status.expect("spirv-val failed to execute");
            if !status.success() {
                let dump_path = Path::new(dump_path);
                if dump_path.is_absolute() {
                    let dump_path = dump_path.join(&module.name);
                    println!(
                        "spirv-val failed on module {} with code {}, dumping module to {:?}",
                        module.name, status, dump_path
                    );
                    std::fs::create_dir_all(dump_path.parent().unwrap()).unwrap();
                    std::fs::copy(&path, dump_path).unwrap();
                } else {
                    println!(
                        "spirv-val failed on module {} with code {}",
                        module.name, status
                    );
                }
            }
        }
        Ok(CompiledModule {
            name: module.name,
            kind: module.kind,
            object: Some(path),
            bytecode: None,
        })
    }

    fn prepare_thin(module: ModuleCodegen<Self::Module>) -> (String, Self::ThinBuffer) {
        (module.name, SpirvThinBuffer(module.module_llvm))
    }

    fn serialize_module(module: ModuleCodegen<Self::Module>) -> (String, Self::ModuleBuffer) {
        (module.name, SpirvModuleBuffer(module.module_llvm))
    }

    fn run_lto_pass_manager(
        _: &CodegenContext<Self>,
        _: &ModuleCodegen<Self::Module>,
        _: &ModuleConfig,
        _: bool,
    ) {
        todo!()
    }
}

impl ExtraBackendMethods for SpirvCodegenBackend {
    fn new_metadata(&self, _: TyCtxt<'_>, _: &str) -> Self::Module {
        Self::Module::new()
    }

    fn write_compressed_metadata<'tcx>(
        &self,
        _: TyCtxt<'tcx>,
        _: &EncodedMetadata,
        _: &mut Self::Module,
    ) {
        // Ignore for now.
    }

    fn codegen_allocator<'tcx>(&self, _: TyCtxt<'tcx>, _: &mut Self::Module, _: AllocatorKind) {
        todo!()
    }

    fn compile_codegen_unit(
        &self,
        tcx: TyCtxt<'_>,
        cgu_name: Symbol,
    ) -> (ModuleCodegen<Self::Module>, u64) {
        // TODO: Do dep_graph stuff
        let cgu = tcx.codegen_unit(cgu_name);

        let cx = CodegenCx::new(tcx, cgu);
        let do_codegen = || {
            let mono_items = cx.codegen_unit.items_in_deterministic_order(cx.tcx);

            for &(mono_item, (linkage, visibility)) in mono_items.iter() {
                let name = mono_item.symbol_name(cx.tcx).name;
                if option_env!("DUMP_MIR").is_some() {
                    if let MonoItem::Fn(instance) = mono_item {
                        dump_mir(tcx, instance);
                    }
                }
                if is_blocklisted_fn(name) {
                    continue;
                }
                mono_item.predefine::<Builder<'_, '_>>(&cx, linkage, visibility);
            }

            // ... and now that we have everything pre-defined, fill out those definitions.
            for &(mono_item, _) in mono_items.iter() {
                let name = mono_item.symbol_name(cx.tcx).name;
                if option_env!("DUMP_MIR").is_some() {
                    if let MonoItem::Fn(instance) = mono_item {
                        dump_mir(tcx, instance);
                    }
                }
                if is_blocklisted_fn(name) {
                    continue;
                }
                mono_item.define::<Builder<'_, '_>>(&cx);
            }

            if let Some(_entry) = maybe_create_entry_wrapper::<Builder<'_, '_>>(&cx) {
                // attributes::sanitize(&cx, SanitizerSet::empty(), entry);
            }
        };
        if let Some(path) = option_env!("DUMP_MODULE_ON_PANIC") {
            let module_dumper = DumpModuleOnPanic { cx: &cx, path };
            with_no_trimmed_paths(do_codegen);
            drop(module_dumper)
        } else {
            with_no_trimmed_paths(do_codegen);
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
        _: &Session,
        _: OptLevel,
    ) -> Arc<dyn Fn() -> Result<Self::TargetMachine, String> + Send + Sync + 'static> {
        Arc::new(|| Ok(()))
    }

    fn target_cpu<'b>(&self, _: &'b Session) -> &'b str {
        todo!()
    }
}

struct DumpModuleOnPanic<'a, 'cx, 'tcx> {
    cx: &'cx CodegenCx<'tcx>,
    path: &'a str,
}

impl Drop for DumpModuleOnPanic<'_, '_, '_> {
    fn drop(&mut self) {
        if std::thread::panicking() {
            let path: &std::path::Path = self.path.as_ref();
            if path.has_root() {
                self.cx.builder.dump_module(path);
            } else {
                println!("{}", self.cx.builder.dump_module_str());
            }
        }
    }
}

/// This is the entrypoint for a hot plugged rustc_codegen_spirv
#[no_mangle]
pub fn __rustc_codegen_backend() -> Box<dyn CodegenBackend> {
    Box::new(SpirvCodegenBackend)
}

// https://github.com/bjorn3/rustc_codegen_cranelift/blob/1b8df386aa72bc3dacb803f7d4deb4eadd63b56f/src/base.rs
