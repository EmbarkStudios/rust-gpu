use crate::codegen_cx::{CodegenArgs, SpirvMetadata};
use crate::{linker, SpirvCodegenBackend, SpirvModuleBuffer, SpirvThinBuffer};
use ar::{Archive, GnuBuilder, Header};
use rspirv::binary::Assemble;
use rspirv::dr::Module;
use rustc_ast::CRATE_NODE_ID;
use rustc_codegen_spirv_types::{CompileResult, ModuleResult};
use rustc_codegen_ssa::back::lto::{LtoModuleCodegen, SerializedModule, ThinModule, ThinShared};
use rustc_codegen_ssa::back::write::CodegenContext;
use rustc_codegen_ssa::{CodegenResults, NativeLib};
use rustc_data_structures::fx::FxHashSet;
use rustc_errors::FatalError;
use rustc_metadata::fs::METADATA_FILENAME;
use rustc_middle::bug;
use rustc_middle::dep_graph::WorkProduct;
use rustc_middle::middle::dependency_format::Linkage;
use rustc_session::config::{CrateType, DebugInfo, Lto, OptLevel, OutputFilenames, OutputType};
use rustc_session::output::{check_file_is_writeable, invalid_output_for_target, out_filename};
use rustc_session::utils::NativeLibKind;
use rustc_session::Session;
use std::collections::BTreeMap;
use std::ffi::{CString, OsStr};
use std::fs::File;
use std::io::{BufWriter, Read};
use std::iter;
use std::path::{Path, PathBuf};
use std::sync::Arc;

pub fn link<'a>(
    sess: &'a Session,
    codegen_results: &CodegenResults,
    outputs: &OutputFilenames,
    crate_name: &str,
) {
    let output_metadata = sess.opts.output_types.contains_key(&OutputType::Metadata);
    for &crate_type in sess.crate_types().iter() {
        if (sess.opts.unstable_opts.no_codegen || !sess.opts.output_types.should_codegen())
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
                    // HACK(eddyb) there's no way way to access `outputs.filestem`,
                    // so we pay the cost of building a whole `PathBuf` instead.
                    let disambiguated_crate_name_for_dumps = outputs
                        .with_extension("")
                        .file_name()
                        .unwrap()
                        .to_os_string();

                    link_exe(
                        sess,
                        crate_type,
                        &out_filename,
                        codegen_results,
                        &disambiguated_crate_name_for_dumps,
                    );
                }
                other => {
                    sess.err(format!("CrateType {:?} not supported yet", other));
                }
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
            NativeLibKind::Static {
                bundle: None | Some(true),
                ..
            } => {}
            NativeLibKind::Static {
                bundle: Some(false),
                ..
            }
            | NativeLibKind::Dylib { .. }
            | NativeLibKind::Framework { .. }
            | NativeLibKind::RawDylib
            | NativeLibKind::LinkArg
            | NativeLibKind::Unspecified => continue,
        }
        if let Some(name) = lib.name {
            sess.err(format!(
                "Adding native library to rlib not supported yet: {}",
                name
            ));
        }
    }

    create_archive(
        &file_list,
        codegen_results.metadata.raw_data(),
        out_filename,
    );
}

fn link_exe(
    sess: &Session,
    crate_type: CrateType,
    out_filename: &Path,
    codegen_results: &CodegenResults,
    disambiguated_crate_name_for_dumps: &OsStr,
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

    let cg_args = CodegenArgs::from_session(sess);

    // HACK(eddyb) this removes the `.json` in `.spv.json`, from `out_filename`.
    let out_path_spv = out_filename.with_extension("");

    let link_result = do_link(
        sess,
        &cg_args,
        &objects,
        &rlibs,
        disambiguated_crate_name_for_dumps,
    );
    let compile_result = match link_result {
        linker::LinkResult::SingleModule(module) => {
            let entry_points = entry_points(&module);
            post_link_single_module(sess, &cg_args, *module, &out_path_spv, None);
            CompileResult {
                entry_points,
                module: ModuleResult::SingleModule(out_path_spv),
            }
        }
        linker::LinkResult::MultipleModules {
            file_stem_to_entry_name_and_module,
        } => {
            let out_dir = out_path_spv.with_extension("spvs");
            if !out_dir.is_dir() {
                std::fs::create_dir_all(&out_dir).unwrap();
            }

            let entry_name_to_file_path: BTreeMap<_, _> = file_stem_to_entry_name_and_module
                .into_iter()
                .map(|(file_stem, (entry_name, module))| {
                    let mut out_file_name = file_stem;
                    out_file_name.push(".spv");
                    let out_file_path = out_dir.join(out_file_name);
                    post_link_single_module(
                        sess,
                        &cg_args,
                        module,
                        &out_file_path,
                        Some(disambiguated_crate_name_for_dumps),
                    );
                    (entry_name, out_file_path)
                })
                .collect();
            CompileResult {
                entry_points: entry_name_to_file_path.keys().cloned().collect(),
                module: ModuleResult::MultiModule(entry_name_to_file_path),
            }
        }
    };

    let file = File::create(out_filename).unwrap();
    serde_json::to_writer(BufWriter::new(file), &compile_result).unwrap();
}

fn entry_points(module: &rspirv::dr::Module) -> Vec<String> {
    module
        .entry_points
        .iter()
        .filter(|inst| inst.class.opcode == rspirv::spirv::Op::EntryPoint)
        .map(|inst| inst.operands[2].unwrap_literal_string().to_string())
        .collect()
}

fn post_link_single_module(
    sess: &Session,
    cg_args: &CodegenArgs,
    module: Module,
    out_filename: &Path,
    dump_prefix: Option<&OsStr>,
) {
    cg_args.do_disassemble(&module);
    let spv_binary = module.assemble();

    if let Some(dir) = &cg_args.dump_post_link {
        // FIXME(eddyb) rename `filename` with `file_path` to make this less confusing.
        let out_filename_file_name = out_filename.file_name().unwrap();
        let dump_path = match dump_prefix {
            Some(prefix) => dir.join(prefix).with_extension(out_filename_file_name),
            None => dir.join(out_filename_file_name),
        };
        std::fs::write(dump_path, spirv_tools::binary::from_binary(&spv_binary)).unwrap();
    }

    let val_options = spirv_tools::val::ValidatorOptions {
        relax_struct_store: cg_args.relax_struct_store,
        relax_logical_pointer: cg_args.relax_logical_pointer,
        before_legalization: false,
        relax_block_layout: cg_args.relax_block_layout,
        uniform_buffer_standard_layout: cg_args.uniform_buffer_standard_layout,
        scalar_block_layout: cg_args.scalar_block_layout,
        skip_block_layout: cg_args.skip_block_layout,
        max_limits: vec![],
    };
    let opt_options = spirv_tools::opt::Options {
        validator_options: Some(val_options.clone()),
        max_id_bound: None,
        preserve_bindings: cg_args.preserve_bindings,
        preserve_spec_constants: false,
    };

    let spv_binary = if sess.opts.optimize != OptLevel::No
        || (sess.opts.debuginfo == DebugInfo::None && cg_args.spirv_metadata == SpirvMetadata::None)
    {
        if cg_args.run_spirv_opt {
            let _timer = sess.timer("link_spirv_opt");
            do_spirv_opt(sess, cg_args, spv_binary, out_filename, opt_options)
        } else {
            let reason = match (sess.opts.optimize, sess.opts.debuginfo == DebugInfo::None) {
                (OptLevel::No, true) => "debuginfo=None".to_string(),
                (optlevel, false) => format!("optlevel={:?}", optlevel),
                (optlevel, true) => format!("optlevel={:?}, debuginfo=None", optlevel),
            };
            sess.warn(format!(
                "`spirv-opt` should have ran ({}) but was disabled by `--no-spirv-opt`",
                reason
            ));
            spv_binary
        }
    } else {
        spv_binary
    };

    if cg_args.run_spirv_val {
        do_spirv_val(sess, &spv_binary, out_filename, val_options);
    }

    {
        let save_modules_timer = sess.timer("link_save_modules");
        if let Err(e) = std::fs::write(out_filename, spirv_tools::binary::from_binary(&spv_binary))
        {
            let mut err = sess.struct_err("failed to serialize spirv-binary to disk");
            err.note(&format!("module `{}`", out_filename.display()));
            err.note(&format!("I/O error: {:#}", e));
            err.emit();
        }

        drop(save_modules_timer);
    }
}

fn do_spirv_opt(
    sess: &Session,
    cg_args: &CodegenArgs,
    spv_binary: Vec<u32>,
    filename: &Path,
    options: spirv_tools::opt::Options,
) -> Vec<u32> {
    use spirv_tools::{
        error,
        opt::{self, Optimizer},
    };

    let mut optimizer = opt::create(sess.target.options.env.parse().ok());

    match sess.opts.optimize {
        OptLevel::No => {}
        OptLevel::Less | OptLevel::Default | OptLevel::Aggressive => {
            optimizer.register_performance_passes();
        }
        OptLevel::Size | OptLevel::SizeMin => {
            optimizer.register_size_passes();
        }
    }

    if sess.opts.debuginfo == DebugInfo::None && cg_args.spirv_metadata == SpirvMetadata::None {
        optimizer
            .register_pass(opt::Passes::EliminateDeadConstant)
            .register_pass(opt::Passes::StripDebugInfo);
    }

    let result = optimizer.optimize(
        &spv_binary,
        &mut |msg: error::Message| {
            use error::MessageLevel as Level;

            // TODO: Adds spans here? Not sure how useful with binary, but maybe?

            let mut err = match msg.level {
                Level::Fatal | Level::InternalError => {
                    // FIXME(eddyb) this was `struct_fatal` but that doesn't seem
                    // necessary and also lacks `.forget_guarantee()`.
                    sess.struct_err(&msg.message).forget_guarantee()
                }
                Level::Error => sess.struct_err(&msg.message).forget_guarantee(),
                Level::Warning => sess.struct_warn(&msg.message),
                Level::Info | Level::Debug => sess.struct_note_without_error(&msg.message),
            };

            err.note(&format!("module `{}`", filename.display()));
            err.emit();
        },
        Some(options),
    );

    match result {
        Ok(spirv_tools::binary::Binary::OwnedU32(words)) => words,
        Ok(binary) => binary.as_words().to_vec(),
        Err(e) => {
            let mut err = sess.struct_warn(&e.to_string());
            err.note("spirv-opt failed, leaving as unoptimized");
            err.note(&format!("module `{}`", filename.display()));
            err.emit();
            spv_binary
        }
    }
}

fn do_spirv_val(
    sess: &Session,
    spv_binary: &[u32],
    filename: &Path,
    options: spirv_tools::val::ValidatorOptions,
) {
    use spirv_tools::val::{self, Validator};

    let validator = val::create(sess.target.options.env.parse().ok());

    if let Err(e) = validator.validate(spv_binary, Some(options)) {
        let mut err = sess.struct_err(&e.to_string());
        err.note("spirv-val failed");
        err.note(&format!("module `{}`", filename.display()));
        err.emit();
    }
}

fn link_local_crate_native_libs_and_dependent_crate_libs<'a>(
    rlibs: &mut Vec<PathBuf>,
    sess: &'a Session,
    crate_type: CrateType,
    codegen_results: &CodegenResults,
) {
    if sess.opts.unstable_opts.link_native_libraries {
        add_local_native_libraries(sess, codegen_results);
    }
    add_upstream_rust_crates(sess, rlibs, codegen_results, crate_type);
    if sess.opts.unstable_opts.link_native_libraries {
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
    for &cnum in &codegen_results.crate_info.used_crates {
        let src = &codegen_results.crate_info.used_crate_source[&cnum];
        match data[cnum.as_usize() - 1] {
            Linkage::NotLinked | Linkage::IncludedFromDylib => {}
            Linkage::Static => rlibs.push(src.rlib.as_ref().unwrap().0.clone()),
            //Linkage::Dynamic => rlibs.push(src.dylib.as_ref().unwrap().0.clone()),
            Linkage::Dynamic => {
                sess.err("TODO: Linkage::Dynamic not supported yet");
            }
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

    for &cnum in &codegen_results.crate_info.used_crates {
        for lib in codegen_results.crate_info.native_libraries[&cnum].iter() {
            let name = match lib.name {
                Some(l) => l,
                None => continue,
            };
            if !relevant_lib(sess, lib) {
                continue;
            }
            match lib.kind {
                NativeLibKind::Dylib { .. } | NativeLibKind::Unspecified => sess.fatal(format!(
                    "TODO: dylib nativelibkind not supported yet: {}",
                    name
                )),
                NativeLibKind::Framework { .. } => sess.fatal(format!(
                    "TODO: framework nativelibkind not supported yet: {}",
                    name
                )),
                NativeLibKind::Static {
                    bundle: Some(false),
                    ..
                } => {
                    if data[cnum.as_usize() - 1] == Linkage::Static {
                        sess.fatal(format!(
                            "TODO: staticnobundle nativelibkind not supported yet: {}",
                            name
                        ))
                    }
                }
                NativeLibKind::Static {
                    bundle: None | Some(true),
                    ..
                } => {}
                NativeLibKind::RawDylib => {
                    sess.fatal(format!("raw_dylib feature not yet implemented: {}", name))
                }
                NativeLibKind::LinkArg => sess.fatal(format!(
                    "TODO: linkarg nativelibkind not supported yet: {}",
                    name
                )),
            }
        }
    }
}

// FIXME(eddyb) upstream has code like this already, maybe we can reuse most of it?
// (see `compiler/rustc_codegen_ssa/src/back/link.rs`)
fn relevant_lib(sess: &Session, lib: &NativeLib) -> bool {
    match lib.cfg {
        Some(ref cfg) => rustc_attr::cfg_matches(cfg, &sess.parse_sess, CRATE_NODE_ID, None),
        None => true,
    }
}

fn create_archive(files: &[&Path], metadata: &[u8], out_filename: &Path) {
    let files_with_names = files.iter().map(|file| {
        (
            file,
            file.file_name()
                .unwrap()
                .to_str()
                .expect("archive file names should be valid ASCII/UTF-8"),
        )
    });
    let out_file = File::create(out_filename).unwrap();
    let mut builder = GnuBuilder::new(
        out_file,
        iter::once(METADATA_FILENAME)
            .chain(files_with_names.clone().map(|(_, name)| name))
            .map(|name| name.as_bytes().to_vec())
            .collect(),
    );
    builder
        .append(
            &Header::new(METADATA_FILENAME.as_bytes().to_vec(), metadata.len() as u64),
            metadata,
        )
        .unwrap();

    let mut filenames = FxHashSet::default();
    filenames.insert(METADATA_FILENAME);
    for (file, name) in files_with_names {
        assert!(
            filenames.insert(name),
            "Duplicate filename in archive: {:?}",
            file.file_name().unwrap()
        );

        // NOTE(eddyb) we can't use `append_path` or `append_file`, as they
        // record too much metadata by default (mtime/UID/GID, at least),
        // which is determintal to reproducible build artifacts, but also
        // can misbehave in environments with high UIDs/GIDs (see #889).
        let file = File::open(file).unwrap();
        let header = Header::new(name.as_bytes().to_vec(), file.metadata().unwrap().len());
        // NOTE(eddyb) either `fs::File`, or the result of `fs::read`, could fit
        // here, but `fs::File` has specialized file->file copying on some OSes.
        builder.append(&header, file).unwrap();
    }
    builder.into_inner().unwrap();
}

/// This is the actual guts of linking: the rest of the link-related functions are just digging through rustc's
/// shenanigans to collect all the object files we need to link.
fn do_link(
    sess: &Session,
    cg_args: &CodegenArgs,
    objects: &[PathBuf],
    rlibs: &[PathBuf],
    disambiguated_crate_name_for_dumps: &OsStr,
) -> linker::LinkResult {
    let load_modules_timer = sess.timer("link_load_modules");

    let mut modules = Vec::new();
    let mut add_module = |file_name: &OsStr, bytes: &[u8]| {
        let module = {
            let mut loader = rspirv::dr::Loader::new();
            rspirv::binary::parse_bytes(bytes, &mut loader).unwrap();
            loader.module()
        };
        if let Some(dir) = &cg_args.dump_pre_link {
            // FIXME(eddyb) is it a good idea to re-`assemble` the `rspirv::dr`
            // module, or should this just save the original bytes?
            std::fs::write(
                dir.join(file_name).with_extension("spv"),
                spirv_tools::binary::from_binary(&module.assemble()),
            )
            .unwrap();
        }
        modules.push(module);
    };

    // `objects` are the plain obj files we need to link - usually produced by the final crate.
    for obj in objects {
        add_module(obj.file_name().unwrap(), &std::fs::read(obj).unwrap());
    }

    // `rlibs` are archive files we've created in `create_archive`, usually produced by crates that are being
    // referenced. We need to unpack them and add the modules inside.
    for rlib in rlibs {
        let mut archive = Archive::new(File::open(rlib).unwrap());
        while let Some(entry) = archive.next_entry() {
            let mut entry = entry.unwrap();
            if entry.header().identifier() != METADATA_FILENAME.as_bytes() {
                // std::fs::read adds 1 to the size, so do the same here - see comment:
                // https://github.com/rust-lang/rust/blob/72868e017bdade60603a25889e253f556305f996/library/std/src/fs.rs#L200-L202
                let mut bytes = Vec::with_capacity(entry.header().size() as usize + 1);
                entry.read_to_end(&mut bytes).unwrap();

                let file_name = std::str::from_utf8(entry.header().identifier()).unwrap();
                add_module(OsStr::new(file_name), &bytes);
            }
        }
    }

    drop(load_modules_timer);

    // Do the link...
    let link_result = linker::link(
        sess,
        modules,
        &cg_args.linker_opts,
        disambiguated_crate_name_for_dumps,
    );

    match link_result {
        Ok(v) => v,
        Err(rustc_errors::ErrorGuaranteed { .. }) => {
            sess.abort_if_errors();
            bug!("Linker errored, but no error reported")
        }
    }
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

    for (name, buffer) in modules {
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
