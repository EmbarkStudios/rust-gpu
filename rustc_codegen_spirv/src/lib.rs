#![feature(rustc_private)]
#![feature(or_insert_with_key)]

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

mod builder;
mod codegen_cx;
mod ctx;
mod things;
mod trans;

use builder::Builder;
use codegen_cx::CodegenCx;
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
use rustc_middle::ty::query::Providers;
use rustc_middle::ty::TyCtxt;
use rustc_session::config::{OptLevel, OutputFilenames};
use rustc_session::Session;
use rustc_span::Symbol;
use rustc_target::spec::Target;
use std::any::Any;
use std::path::Path;
use std::sync::Arc;
use things::{ModuleSpirv, SpirvModuleBuffer, SprivThinBuffer};

#[cfg(test)]
#[path = "../test/lib.rs"]
mod test;

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

struct TheBackend;

impl CodegenBackend for TheBackend {
    fn metadata_loader(&self) -> Box<MetadataLoaderDyn> {
        Box::new(NoLlvmMetadataLoader)
    }

    fn provide(&self, providers: &mut Providers) {
        rustc_symbol_mangling::provide(providers);

        providers.supported_target_features = |_tcx, _cnum| {
            Default::default() // Just a dummy
        };
        providers.is_reachable_non_generic = |_tcx, _defid| true;
        providers.exported_symbols = |_tcx, _crate| &[];
    }

    fn provide_extern(&self, providers: &mut Providers) {
        providers.is_reachable_non_generic = |_tcx, _defid| true;
    }

    fn codegen_crate<'a, 'tcx>(
        &self,
        tcx: TyCtxt<'tcx>,
        _metadata: EncodedMetadata,
        _need_metadata_module: bool,
    ) -> Box<dyn Any> {
        use rustc_hir::def_id::LOCAL_CRATE;

        let cgus = if tcx.sess.opts.output_types.should_codegen() {
            tcx.collect_and_partition_mono_items(LOCAL_CRATE).1
        } else {
            // If only `--emit metadata` is used, we shouldn't perform any codegen.
            // Also `tcx.collect_and_partition_mono_items` may panic in that case.
            &[]
        };

        let mut context = ctx::Context::new(tcx);

        for cgu in cgus {
            let cgu = tcx.codegen_unit(cgu.name());
            let mono_items = cgu.items_in_deterministic_order(tcx);

            for (mono_item, (_linkage, _visibility)) in mono_items {
                match mono_item {
                    MonoItem::Fn(instance) => trans::trans_fn(&mut context, instance),
                    thing => panic!("Unknown MonoItem: {:?}", thing),
                }
            }
        }

        let module = context.assemble();

        Box::new(module)
    }

    fn join_codegen(
        &self,
        ongoing_codegen: Box<dyn Any>,
        _sess: &Session,
        _dep_graph: &DepGraph,
    ) -> Result<Box<dyn Any>, ErrorReported> {
        // I think this function is if `codegen_crate` spawns threads, this is supposed to join those threads?
        Ok(ongoing_codegen)
        // let crate_name = ongoing_codegen
        //     .downcast::<Symbol>()
        //     .expect("in join_codegen: ongoing_codegen is not a Symbol");
        // Ok(crate_name)
    }

    fn link(
        &self,
        sess: &Session,
        codegen_results: Box<dyn Any>,
        outputs: &OutputFilenames,
    ) -> Result<(), ErrorReported> {
        use rustc_session::config::{CrateType, OutputType};
        use std::io::Write;
        let codegen_bytes = codegen_results
            .downcast::<Vec<u32>>()
            .expect("in link: codegen_results is not a Vec<u32>");
        for &crate_type in sess.opts.crate_types.iter() {
            if crate_type != CrateType::Rlib {
                sess.fatal(&format!("Crate type is {:?}", crate_type));
            }
            let mut output_name = outputs.path(OutputType::Exe);
            output_name.set_extension("spv");
            let mut out_file =
                ::std::fs::File::create(output_name).expect("Unable to create output file");
            // Note: endianness doesn't matter, readers deduce endianness from magic header.
            let bytes_u8: &[u8] = unsafe {
                std::slice::from_raw_parts(
                    codegen_bytes.as_ptr() as *const u8,
                    codegen_bytes.len() * std::mem::size_of::<u32>(),
                )
            };
            out_file
                .write_all(bytes_u8)
                .expect("Unable to write output file");
        }
        Ok(())
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
            Default::default() // Just a dummy
        };
        providers.is_reachable_non_generic = |_tcx, _defid| true;
        providers.exported_symbols = |_tcx, _crate| &[];
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

impl WriteBackendMethods for SsaBackend {
    type Module = ModuleSpirv;
    type TargetMachine = ();
    type ModuleBuffer = SpirvModuleBuffer;
    type Context = ();
    type ThinData = ();
    type ThinBuffer = SprivThinBuffer;

    fn run_fat_lto(
        _: &CodegenContext<Self>,
        _: Vec<FatLTOInput<Self>>,
        _: Vec<(SerializedModule<Self::ModuleBuffer>, WorkProduct)>,
    ) -> Result<LtoModuleCodegen<Self>, FatalError> {
        todo!()
    }

    fn run_thin_lto(
        _: &CodegenContext<Self>,
        _: Vec<(String, Self::ThinBuffer)>,
        _: Vec<(SerializedModule<Self::ModuleBuffer>, WorkProduct)>,
    ) -> Result<(Vec<LtoModuleCodegen<Self>>, Vec<WorkProduct>), FatalError> {
        todo!()
    }

    fn print_pass_timings(&self) {
        todo!()
    }

    unsafe fn optimize(
        _: &CodegenContext<Self>,
        _: &Handler,
        _: &ModuleCodegen<Self::Module>,
        _: &ModuleConfig,
    ) -> Result<(), FatalError> {
        todo!()
    }

    unsafe fn optimize_thin(
        _: &CodegenContext<Self>,
        _: &mut ThinModule<Self>,
    ) -> Result<ModuleCodegen<Self::Module>, FatalError> {
        todo!()
    }

    unsafe fn codegen(
        _: &CodegenContext<Self>,
        _: &Handler,
        _: ModuleCodegen<Self::Module>,
        _: &ModuleConfig,
    ) -> Result<CompiledModule, FatalError> {
        todo!()
    }

    fn prepare_thin(_: ModuleCodegen<Self::Module>) -> (String, Self::ThinBuffer) {
        todo!()
    }

    fn serialize_module(_: ModuleCodegen<Self::Module>) -> (String, Self::ModuleBuffer) {
        todo!()
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

        let spirv_module = ModuleSpirv::new(tcx, &cgu_name.as_str());

        {
            let cx = CodegenCx::new(tcx, cgu, &spirv_module);
            let mono_items = cx.codegen_unit.items_in_deterministic_order(cx.tcx);

            for &(mono_item, (linkage, visibility)) in &mono_items {
                mono_item.predefine::<Builder<'_, '_, '_>>(&cx, linkage, visibility);
            }

            // ... and now that we have everything pre-defined, fill out those definitions.
            for &(mono_item, _) in &mono_items {
                mono_item.define::<Builder<'_, '_, '_>>(&cx);
            }

            if let Some(entry) = maybe_create_entry_wrapper::<Builder<'_, '_, '_>>(&cx) {
                // attributes::sanitize(&cx, SanitizerSet::empty(), entry);
            }
        }

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

/// This is the entrypoint for a hot plugged rustc_codegen_spirv
#[no_mangle]
pub fn __rustc_codegen_backend() -> Box<dyn CodegenBackend> {
    Box::new(SsaBackend)
}

// https://github.com/bjorn3/rustc_codegen_cranelift/blob/1b8df386aa72bc3dacb803f7d4deb4eadd63b56f/src/base.rs
