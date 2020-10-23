use crate::{linker, SpirvCodegenBackend, SpirvModuleBuffer, SpirvThinBuffer};
use rustc_codegen_ssa::{
    back::{
        lto::{LtoModuleCodegen, SerializedModule, ThinModule, ThinShared},
        write::CodegenContext,
    },
    CodegenResults,
};
use rustc_data_structures::{owning_ref::OwningRef, rustc_erase_owner, sync::MetadataRef};
use rustc_errors::FatalError;
use rustc_middle::{
    bug, dep_graph::WorkProduct, middle::cstore::NativeLib, middle::dependency_format::Linkage,
};
use rustc_session::{
    config::{CrateType, Lto, OutputFilenames, OutputType},
    output::{check_file_is_writeable, invalid_output_for_target, out_filename},
    utils::NativeLibKind,
    Session,
};
use std::{
    collections::HashSet,
    env,
    ffi::{CString, OsStr},
    fs::File,
    io::{Read, Write},
    path::{Path, PathBuf},
    sync::Arc,
};
use tar::{Archive, Builder, Header};

pub fn link<'a>(
    sess: &'a Session,
    codegen_results: &CodegenResults,
    outputs: &OutputFilenames,
    crate_name: &str,
    legalize: bool,
) {
    let output_metadata = sess.opts.output_types.contains_key(&OutputType::Metadata);
    for &crate_type in sess.crate_types().iter() {
        if (sess.opts.debugging_opts.no_codegen || !sess.opts.output_types.should_codegen())
            && !output_metadata
            && crate_type == CrateType::Executable
        {
            continue;
        }

        if invalid_output_for_target(sess, crate_type) {
            bug!(
                "invalid output type `{:?}` for target os `{}`",
                crate_type,
                sess.opts.target_triple
            );
        }

        for obj in codegen_results
            .modules
            .iter()
            .filter_map(|m| m.object.as_ref())
        {
            check_file_is_writeable(obj, sess);
        }

        if outputs.outputs.should_codegen() {
            let out_filename = out_filename(sess, crate_type, outputs, crate_name);
            match crate_type {
                CrateType::Rlib => {
                    link_rlib(sess, codegen_results, &out_filename);
                }
                CrateType::Executable | CrateType::Cdylib | CrateType::Dylib => {
                    link_exe(sess, crate_type, &out_filename, codegen_results, legalize)
                }
                other => sess.err(&format!("CrateType {:?} not supported yet", other)),
            }
        }
    }
}

fn link_rlib(sess: &Session, codegen_results: &CodegenResults, out_filename: &Path) {
    let mut file_list = Vec::<&Path>::new();
    for obj in codegen_results
        .modules
        .iter()
        .filter_map(|m| m.object.as_ref())
    {
        file_list.push(obj);
    }
    for lib in codegen_results.crate_info.used_libraries.iter() {
        match lib.kind {
            NativeLibKind::StaticBundle => {}
            NativeLibKind::StaticNoBundle
            | NativeLibKind::Dylib
            | NativeLibKind::Framework
            | NativeLibKind::RawDylib
            | NativeLibKind::Unspecified => continue,
        }
        if let Some(name) = lib.name {
            sess.err(&format!(
                "Adding native library to rlib not supported yet: {}",
                name
            ));
        }
    }

    create_archive(&file_list, &codegen_results.metadata.raw_data, out_filename);
}

fn link_exe(
    sess: &Session,
    crate_type: CrateType,
    out_filename: &Path,
    codegen_results: &CodegenResults,
    legalize: bool,
) {
    let mut objects = Vec::new();
    let mut rlibs = Vec::new();
    for obj in codegen_results
        .modules
        .iter()
        .filter_map(|m| m.object.as_ref())
    {
        objects.push(obj.clone());
    }

    link_local_crate_native_libs_and_dependent_crate_libs(
        &mut rlibs,
        sess,
        crate_type,
        codegen_results,
    );

    let spv_binary = do_link(sess, &objects, &rlibs, legalize);

    let spv_binary = if env::var("SPIRV_OPT").is_ok() {
        let _timer = sess.timer("link_spirv_opt");
        do_spirv_opt(sess, spv_binary, out_filename)
    } else {
        spv_binary
    };

    if env::var("NO_SPIRV_VAL").is_err() {
        do_spirv_val(sess, &spv_binary, out_filename);
    }

    {
        let save_modules_timer = sess.timer("link_save_modules");
        if let Err(e) = std::fs::write(out_filename, crate::slice_u32_to_u8(&spv_binary)) {
            let mut err = sess.struct_err("failed to serialize spirv-binary to disk");
            err.note(&format!("module {:?}", out_filename));
            err.note(&format!("I/O error: {:#}", e));
            err.emit();
        }

        drop(save_modules_timer);
    }
}

fn do_spirv_opt(sess: &Session, spv_binary: Vec<u32>, filename: &Path) -> Vec<u32> {
    use spirv_tools::{opt, shared};

    // This is the same default target environment that spirv-opt uses
    let mut optimizer = opt::Optimizer::new(shared::TargetEnv::Universal_1_5);

    optimizer
        .register_size_passes()
        .register_pass(opt::Passes::EliminateDeadConstant)
        .register_pass(opt::Passes::StripDebugInfo);

    let mut opts = opt::Options::new();

    // We run the validator separately
    opts.run_validator(false);

    let mut optimized_binary = None;

    let result = optimizer.optimize(
        &spv_binary,
        &mut |data: &[u32]| {
            optimized_binary = Some(data.to_vec());
        },
        Some(&opts),
    );

    match result {
        Err(opt::RunResult::OptimizerFailed) => {
            let mut err = sess.struct_warn("spirv-opt failed, leaving as unoptimized");
            err.note(&format!("module {:?}", filename));
            spv_binary
        }
        Ok(_) => optimized_binary.unwrap(),
        Err(_) => {
            let mut err =
                sess.struct_warn("invalid arguments supplied to spirv-opt, leaving as unoptimized");
            err.note(&format!("module {:?}", filename));
            spv_binary
        }
    }
}

fn do_spirv_val(sess: &Session, spv_binary: &[u32], filename: &Path) {
    use spirv_tools::{shared, val};

    // This is the same default target environment that spirv-val uses
    let validator = val::Validator::new(shared::TargetEnv::Universal_1_5);
    let opts = val::ValidatorOptions::default();

    if validator.validate(spv_binary, &opts).is_err() {
        let mut err = sess.struct_err("error occurred during validation");
        err.note(&format!("module {:?}", filename));
        err.emit();
    }
}

fn link_local_crate_native_libs_and_dependent_crate_libs<'a>(
    rlibs: &mut Vec<PathBuf>,
    sess: &'a Session,
    crate_type: CrateType,
    codegen_results: &CodegenResults,
) {
    if sess.opts.debugging_opts.link_native_libraries {
        add_local_native_libraries(sess, codegen_results);
    }
    add_upstream_rust_crates(sess, rlibs, codegen_results, crate_type);
    if sess.opts.debugging_opts.link_native_libraries {
        add_upstream_native_libraries(sess, codegen_results, crate_type);
    }
}

fn add_local_native_libraries(sess: &Session, codegen_results: &CodegenResults) {
    let relevant_libs = codegen_results
        .crate_info
        .used_libraries
        .iter()
        .filter(|l| relevant_lib(sess, l));
    assert_eq!(relevant_libs.count(), 0);
}

fn add_upstream_rust_crates(
    sess: &Session,
    rlibs: &mut Vec<PathBuf>,
    codegen_results: &CodegenResults,
    crate_type: CrateType,
) {
    let (_, data) = codegen_results
        .crate_info
        .dependency_formats
        .iter()
        .find(|(ty, _)| *ty == crate_type)
        .expect("failed to find crate type in dependency format list");
    let deps = &codegen_results.crate_info.used_crates_dynamic;
    for &(cnum, _) in deps.iter() {
        let src = &codegen_results.crate_info.used_crate_source[&cnum];
        match data[cnum.as_usize() - 1] {
            Linkage::NotLinked | Linkage::IncludedFromDylib => {}
            Linkage::Static => rlibs.push(src.rlib.as_ref().unwrap().0.clone()),
            //Linkage::Dynamic => rlibs.push(src.dylib.as_ref().unwrap().0.clone()),
            Linkage::Dynamic => sess.err("TODO: Linkage::Dynamic not supported yet"),
        }
    }
}

fn add_upstream_native_libraries(
    sess: &Session,
    codegen_results: &CodegenResults,
    crate_type: CrateType,
) {
    let (_, data) = codegen_results
        .crate_info
        .dependency_formats
        .iter()
        .find(|(ty, _)| *ty == crate_type)
        .expect("failed to find crate type in dependency format list");

    let crates = &codegen_results.crate_info.used_crates_static;
    for &(cnum, _) in crates {
        for lib in codegen_results.crate_info.native_libraries[&cnum].iter() {
            let name = match lib.name {
                Some(l) => l,
                None => continue,
            };
            if !relevant_lib(sess, lib) {
                continue;
            }
            match lib.kind {
                NativeLibKind::Dylib | NativeLibKind::Unspecified => sess.fatal(&format!(
                    "TODO: dylib nativelibkind not supported yet: {}",
                    name
                )),
                NativeLibKind::Framework => sess.fatal(&format!(
                    "TODO: framework nativelibkind not supported yet: {}",
                    name
                )),
                NativeLibKind::StaticNoBundle => {
                    if data[cnum.as_usize() - 1] == Linkage::Static {
                        sess.fatal(&format!(
                            "TODO: staticnobundle nativelibkind not supported yet: {}",
                            name
                        ))
                    }
                }
                NativeLibKind::StaticBundle => {}
                NativeLibKind::RawDylib => {
                    sess.fatal(&format!("raw_dylib feature not yet implemented: {}", name))
                }
            }
        }
    }
}

fn relevant_lib(sess: &Session, lib: &NativeLib) -> bool {
    match lib.cfg {
        Some(ref cfg) => rustc_attr::cfg_matches(cfg, &sess.parse_sess, None),
        None => true,
    }
}

fn create_archive(files: &[&Path], metadata: &[u8], out_filename: &Path) {
    let file = File::create(out_filename).unwrap();
    let mut builder = Builder::new(file);
    {
        let mut header = Header::new_gnu();
        header.set_path(".metadata").unwrap();
        header.set_size(metadata.len() as u64);
        header.set_cksum();
        builder.append(&header, metadata).unwrap();
    }
    let mut filenames = HashSet::new();
    filenames.insert(OsStr::new(".metadata"));
    for file in files {
        assert!(
            filenames.insert(file.file_name().unwrap()),
            "Duplicate filename in archive: {:?}",
            file.file_name().unwrap()
        );
        builder
            .append_path_with_name(file, file.file_name().unwrap())
            .unwrap();
    }
    builder.into_inner().unwrap();
}

pub fn read_metadata(rlib: &Path) -> Result<MetadataRef, String> {
    fn read_metadata_internal(rlib: &Path) -> Result<Option<MetadataRef>, std::io::Error> {
        for entry in Archive::new(File::open(rlib)?).entries()? {
            let mut entry = entry?;
            if entry.path()? == Path::new(".metadata") {
                let mut bytes = Vec::new();
                entry.read_to_end(&mut bytes)?;
                let buf: OwningRef<Vec<u8>, [u8]> = OwningRef::new(bytes);
                return Ok(Some(rustc_erase_owner!(buf.map_owner_box())));
            }
        }
        Ok(None)
    }
    match read_metadata_internal(rlib) {
        Ok(Some(m)) => Ok(m),
        Ok(None) => Err(format!("No .metadata file in rlib: {:?}", rlib)),
        Err(io) => Err(format!("Failed to read rlib at {:?}: {}", rlib, io)),
    }
}

/// This is the actual guts of linking: the rest of the link-related functions are just digging through rustc's
/// shenanigans to collect all the object files we need to link.
fn do_link(sess: &Session, objects: &[PathBuf], rlibs: &[PathBuf], legalize: bool) -> Vec<u32> {
    fn load(bytes: &[u8]) -> rspirv::dr::Module {
        let mut loader = rspirv::dr::Loader::new();
        rspirv::binary::parse_bytes(&bytes, &mut loader).unwrap();
        loader.module()
    }

    let load_modules_timer = sess.timer("link_load_modules");
    let mut modules = Vec::new();
    // `objects` are the plain obj files we need to link - usually produced by the final crate.
    for obj in objects {
        let mut bytes = Vec::new();
        File::open(obj).unwrap().read_to_end(&mut bytes).unwrap();
        modules.push(load(&bytes));
    }
    // `rlibs` are archive files we've created in `create_archive`, usually produced by crates that are being
    // referenced. We need to unpack them and add the modules inside.
    for rlib in rlibs {
        for entry in Archive::new(File::open(rlib).unwrap()).entries().unwrap() {
            let mut entry = entry.unwrap();
            if entry.path().unwrap() != Path::new(".metadata") {
                let mut bytes = Vec::new();
                entry.read_to_end(&mut bytes).unwrap();
                modules.push(load(&bytes));
            }
        }
    }

    let mut module_refs = modules.iter_mut().collect::<Vec<_>>();

    if let Ok(ref path) = env::var("DUMP_PRE_LINK") {
        let path = Path::new(path);
        if path.is_file() {
            std::fs::remove_file(path).unwrap();
        }
        std::fs::create_dir_all(path).unwrap();
        for (num, module) in module_refs.iter().enumerate() {
            File::create(path.join(format!("mod_{}.spv", num)))
                .unwrap()
                .write_all(crate::slice_u32_to_u8(&module.assemble()))
                .unwrap();
        }
    }
    drop(load_modules_timer);

    // Do the link...
    let options = linker::Options {
        dce: env::var("NO_DCE").is_err(),
        compact_ids: env::var("NO_COMPACT_IDS").is_err(),
        inline: legalize,
        mem2reg: legalize,
        structurize: env::var("NO_STRUCTURIZE").is_err(),
    };

    let link_result = linker::link(Some(sess), &mut module_refs, &options);

    let assembled = match link_result {
        Ok(v) => v,
        Err(err) => {
            if let Ok(ref path) = env::var("DUMP_MODULE_ON_PANIC") {
                let path = Path::new(path);
                if path.is_file() {
                    std::fs::remove_file(path).unwrap();
                }
                std::fs::create_dir_all(path).unwrap();
                for (num, module) in modules.iter().enumerate() {
                    File::create(path.join(format!("mod_{}.spv", num)))
                        .unwrap()
                        .write_all(crate::slice_u32_to_u8(&module.assemble()))
                        .unwrap();
                }
            }
            sess.fatal(&format!("Linker error: {}", err))
        }
    };

    // And finally write out the linked binary.
    use rspirv::binary::Assemble;
    assembled.assemble()
}

/// As of right now, this is essentially a no-op, just plumbing through all the files.
// TODO: WorkProduct impl
pub(crate) fn run_thin(
    cgcx: &CodegenContext<SpirvCodegenBackend>,
    modules: Vec<(String, SpirvThinBuffer)>,
    cached_modules: Vec<(SerializedModule<SpirvModuleBuffer>, WorkProduct)>,
) -> Result<(Vec<LtoModuleCodegen<SpirvCodegenBackend>>, Vec<WorkProduct>), FatalError> {
    if cgcx.opts.cg.linker_plugin_lto.enabled() {
        unreachable!("We should never reach this case if the LTO step is deferred to the linker");
    }
    if cgcx.lto != Lto::ThinLocal {
        for _ in cgcx.each_linked_rlib_for_lto.iter() {
            bug!("TODO: Implement whatever the heck this is");
        }
    }
    let mut thin_buffers = Vec::with_capacity(modules.len());
    let mut module_names = Vec::with_capacity(modules.len() + cached_modules.len());

    for (name, buffer) in modules.into_iter() {
        let cname = CString::new(name.clone()).unwrap();
        thin_buffers.push(buffer);
        module_names.push(cname);
    }

    let mut serialized_modules = Vec::with_capacity(cached_modules.len());

    for (sm, wp) in cached_modules {
        let _slice_u8 = sm.data();
        serialized_modules.push(sm);
        module_names.push(CString::new(wp.cgu_name).unwrap());
    }

    let shared = Arc::new(ThinShared {
        data: (),
        thin_buffers,
        serialized_modules,
        module_names,
    });

    let mut opt_jobs = vec![];
    for (module_index, _) in shared.module_names.iter().enumerate() {
        opt_jobs.push(LtoModuleCodegen::Thin(ThinModule {
            shared: shared.clone(),
            idx: module_index,
        }));
    }

    Ok((opt_jobs, vec![]))
}
