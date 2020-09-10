#![feature(rustc_private)]
#![feature(once_cell)]

extern crate rustc_ast;
extern crate rustc_codegen_ssa;
extern crate rustc_data_structures;
extern crate rustc_driver;
extern crate rustc_errors;
extern crate rustc_hir;
extern crate rustc_interface;
extern crate rustc_middle;
extern crate rustc_mir;
extern crate rustc_session;
extern crate rustc_span;
extern crate rustc_symbol_mangling;
extern crate rustc_target;

mod abi;
mod builder;
mod builder_spirv;
mod codegen_cx;
mod finalizing_passes;
mod link;
mod spirv_type;
mod things;

#[cfg(test)]
#[path = "../test/lib.rs"]
mod test;

use builder::Builder;
use codegen_cx::CodegenCx;
use rspirv::binary::Assemble;
use rustc_ast::expand::allocator::AllocatorKind;
use rustc_codegen_ssa::back::lto::{LtoModuleCodegen, SerializedModule, ThinModule};
use rustc_codegen_ssa::back::write::{CodegenContext, FatLTOInput, ModuleConfig};
use rustc_codegen_ssa::base::maybe_create_entry_wrapper;
use rustc_codegen_ssa::mono_item::MonoItemExt;
use rustc_codegen_ssa::traits::{CodegenBackend, ExtraBackendMethods, WriteBackendMethods};
use rustc_codegen_ssa::{CodegenResults, CompiledModule, ModuleCodegen, ModuleKind};
use rustc_data_structures::owning_ref::OwningRef;
use rustc_data_structures::rustc_erase_owner;
use rustc_data_structures::sync::MetadataRef;
use rustc_errors::{ErrorReported, FatalError, Handler};
use rustc_middle::dep_graph::{DepGraph, WorkProduct};
use rustc_middle::middle::cstore::{EncodedMetadata, MetadataLoader, MetadataLoaderDyn};
use rustc_middle::mir::mono::MonoItem;
use rustc_middle::ty::print::with_no_trimmed_paths;
use rustc_middle::ty::query::Providers;
use rustc_middle::ty::{Instance, InstanceDef, TyCtxt};
use rustc_session::config::{OptLevel, OutputFilenames, OutputType};
use rustc_session::Session;
use rustc_span::Symbol;
use rustc_target::spec::Target;
use std::any::Any;
use std::path::Path;
use std::{fs::File, io::Write, sync::Arc};
use things::{SpirvModuleBuffer, SpirvThinBuffer};

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

struct NoLlvmMetadataLoader;

impl MetadataLoader for NoLlvmMetadataLoader {
    fn get_rlib_metadata(&self, _: &Target, filename: &Path) -> Result<MetadataRef, String> {
        let buf =
            std::fs::read(filename).map_err(|e| format!("metadata file open err: {:?}", e))?;
        let buf: OwningRef<Vec<u8>, [u8]> = OwningRef::new(buf);
        Ok(rustc_erase_owner!(buf.map_owner_box()))
    }

    fn get_dylib_metadata(&self, target: &Target, filename: &Path) -> Result<MetadataRef, String> {
        self.get_rlib_metadata(target, filename)
    }
}

#[derive(Clone)]
struct SsaBackend;

impl CodegenBackend for SsaBackend {
    fn metadata_loader(&self) -> Box<MetadataLoaderDyn> {
        Box::new(NoLlvmMetadataLoader)
    }

    fn provide(&self, providers: &mut Providers) {
        rustc_symbol_mangling::provide(providers);

        providers.supported_target_features = |_tcx, _cnum| {
            // Temp hack to make wasm target work
            [("simd128".to_string(), None)].iter().cloned().collect()
        };
        providers.is_reachable_non_generic = |_tcx, _defid| true;
        providers.exported_symbols = |_tcx, _crate| &[];
        // Temp hack to make wasm target work
        providers.wasm_import_module_map = |_tcx, _crate| Default::default();
    }

    fn provide_extern(&self, providers: &mut Providers) {
        providers.is_reachable_non_generic = |_tcx, _defid| true;
    }

    fn codegen_crate<'tcx>(
        &self,
        tcx: rustc_middle::ty::TyCtxt<'tcx>,
        metadata: rustc_middle::middle::cstore::EncodedMetadata,
        need_metadata_module: bool,
    ) -> Box<dyn Any> {
        Box::new(rustc_codegen_ssa::base::codegen_crate(
            SsaBackend,
            tcx,
            metadata,
            need_metadata_module,
        ))
    }

    fn join_codegen(
        &self,
        ongoing_codegen: Box<dyn Any>,
        sess: &Session,
        _dep_graph: &DepGraph,
    ) -> Result<Box<dyn Any>, ErrorReported> {
        let (codegen_results, _work_products) = ongoing_codegen
            .downcast::<rustc_codegen_ssa::back::write::OngoingCodegen<SsaBackend>>()
            .expect("Expected SpirvCodegenBackend's OngoingCodegen, found Box<Any>")
            .join(sess);
        if sess.opts.debugging_opts.incremental_info {
            rustc_codegen_ssa::back::write::dump_incremental_data(&codegen_results);
        }

        // sess.time("serialize_work_products", move || {
        //     rustc_incremental::save_work_product_index(sess, &dep_graph, work_products)
        // });

        sess.compile_status()?;

        Ok(Box::new(codegen_results))
    }

    fn link(
        &self,
        _sess: &Session,
        codegen_results: Box<dyn Any>,
        _outputs: &OutputFilenames,
    ) -> Result<(), ErrorReported> {
        let _codegen_results = codegen_results
            .downcast::<CodegenResults>()
            .expect("Expected CodegenResults, found Box<Any>");

        // TODO: see rustc_codegen_llvm for example of implementation

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

impl WriteBackendMethods for SsaBackend {
    type Module = Vec<u32>;
    type TargetMachine = ();
    type ModuleBuffer = SpirvModuleBuffer;
    type Context = ();
    type ThinData = ();
    type ThinBuffer = SpirvThinBuffer;

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

impl ExtraBackendMethods for SsaBackend {
    fn new_metadata(&self, _: TyCtxt<'_>, _: &str) -> Self::Module {
        todo!()
    }

    fn write_compressed_metadata<'tcx>(
        &self,
        _: TyCtxt<'tcx>,
        _: &EncodedMetadata,
        _: &mut Self::Module,
    ) {
        todo!()
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
    Box::new(SsaBackend)
}

// https://github.com/bjorn3/rustc_codegen_cranelift/blob/1b8df386aa72bc3dacb803f7d4deb4eadd63b56f/src/base.rs
