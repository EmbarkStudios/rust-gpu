use cc::Build;
use std::path::Path;

fn add_includes(builder: &mut Build, root: &str, includes: &[&str]) {
    let root = Path::new(root);

    for inc in includes {
        builder.include(root.join(inc));
    }
}

fn add_sources(builder: &mut Build, root: &str, files: &[&str]) {
    let root = Path::new(root);
    builder.files(files.iter().map(|src| {
        let mut p = root.join(src);
        p.set_extension("cpp");
        p
    }));
}

fn shared(build: &mut Build) {
    add_sources(
        build,
        "spirv-tools/source",
        &[
            "util/bit_vector",
            "util/parse_number",
            "util/string_utils",
            "assembly_grammar",
            "binary",
            "diagnostic",
            "disassemble",
            "enum_string_mapping",
            "ext_inst",
            "extensions",
            "libspirv",
            "name_mapper",
            "opcode",
            "operand",
            "parsed_operand",
            "print",
            "software_version",
            "spirv_endian",
            "spirv_fuzzer_options",
            "spirv_optimizer_options",
            "spirv_reducer_options",
            "spirv_target_env",
            "spirv_validator_options",
            "table",
            "text",
            "text_handler",
        ],
    );
}

fn opt(build: &mut Build) {
    build.file("src/c/opt.cpp");

    add_sources(
        build,
        "spirv-tools/source/opt",
        &[
            "aggressive_dead_code_elim_pass",
            "amd_ext_to_khr",
            "basic_block",
            "block_merge_pass",
            "block_merge_util",
            "build_module",
            "ccp_pass",
            "cfg",
            "cfg_cleanup_pass",
            "code_sink",
            "combine_access_chains",
            "compact_ids_pass",
            "composite",
            "const_folding_rules",
            "constants",
            "convert_to_half_pass",
            "copy_prop_arrays",
            "dead_branch_elim_pass",
            "dead_insert_elim_pass",
            "dead_variable_elimination",
            "debug_info_manager",
            "decompose_initialized_variables_pass",
            "decoration_manager",
            "def_use_manager",
            "desc_sroa",
            "dominator_analysis",
            "dominator_tree",
            "eliminate_dead_constant_pass",
            "eliminate_dead_functions_pass",
            "eliminate_dead_functions_util",
            "eliminate_dead_members_pass",
            "feature_manager",
            "fix_storage_class",
            "flatten_decoration_pass",
            "fold",
            "fold_spec_constant_op_and_composite_pass",
            "folding_rules",
            "freeze_spec_constant_value_pass",
            "function",
            "generate_webgpu_initializers_pass",
            "graphics_robust_access_pass",
            "if_conversion",
            "inline_exhaustive_pass",
            "inline_opaque_pass",
            "inline_pass",
            "inst_bindless_check_pass",
            "inst_buff_addr_check_pass",
            "inst_debug_printf_pass",
            "instruction",
            "instruction_list",
            "instrument_pass",
            "ir_context",
            "ir_loader",
            "legalize_vector_shuffle_pass",
            "licm_pass",
            "local_access_chain_convert_pass",
            "local_redundancy_elimination",
            "local_single_block_elim_pass",
            "local_single_store_elim_pass",
            "loop_dependence",
            "loop_dependence_helpers",
            "loop_descriptor",
            "loop_fission",
            "loop_fusion",
            "loop_fusion_pass",
            "loop_peeling",
            "loop_unroller",
            "loop_unswitch_pass",
            "loop_utils",
            "mem_pass",
            "merge_return_pass",
            "module",
            "optimizer",
            "pass",
            "pass_manager",
            "pch_source_opt",
            "private_to_local_pass",
            "process_lines_pass",
            "propagator",
            "reduce_load_size",
            "redundancy_elimination",
            "register_pressure",
            "relax_float_ops_pass",
            "remove_duplicates_pass",
            "replace_invalid_opc",
            "scalar_analysis",
            "scalar_analysis_simplification",
            "scalar_replacement_pass",
            "set_spec_constant_default_value_pass",
            "simplification_pass",
            "split_invalid_unreachable_pass",
            "ssa_rewrite_pass",
            "strength_reduction_pass",
            "strip_atomic_counter_memory_pass",
            "strip_debug_info_pass",
            "strip_reflect_info_pass",
            "struct_cfg_analysis",
            "type_manager",
            "types",
            "unify_const_pass",
            "upgrade_memory_model",
            "value_number_table",
            "vector_dce",
            "workaround1209",
            "wrap_opkill",
        ],
    );
}

fn val(build: &mut Build) {
    add_sources(
        build,
        "spirv-tools/source/val",
        &[
            "validate",
            "validate_adjacency",
            "validate_annotation",
            "validate_arithmetics",
            "validate_atomics",
            "validate_barriers",
            "validate_bitwise",
            "validate_builtins",
            "validate_capability",
            "validate_cfg",
            "validate_composites",
            "validate_constants",
            "validate_conversion",
            "validate_debug",
            "validate_decorations",
            "validate_derivatives",
            "validate_extensions",
            "validate_execution_limitations",
            "validate_function",
            "validate_id",
            "validate_image",
            "validate_interfaces",
            "validate_instruction",
            "validate_layout",
            "validate_literals",
            "validate_logicals",
            "validate_memory",
            "validate_memory_semantics",
            "validate_misc",
            "validate_mode_setting",
            "validate_non_uniform",
            "validate_primitives",
            "validate_scopes",
            "validate_small_type_uses",
            "validate_type",
            "basic_block",
            "construct",
            "function",
            "instruction",
            "validation_state",
        ],
    );
}

fn main() {
    let mut build = Build::new();

    add_includes(&mut build, "spirv-tools", &["", "include"]);
    add_includes(&mut build, "generated", &[""]);
    add_includes(
        &mut build,
        "spirv-headers",
        &["include", "include/spirv/unified1"],
    );

    shared(&mut build);

    // Some opt code requires val as well
    if cfg!(any(feature = "opt", feature = "val")) {
        val(&mut build);
    }

    if cfg!(feature = "opt") {
        opt(&mut build);
    }

    build.define("SPIRV_CHECK_CONTEXT", None);

    let target_def = match std::env::var("CARGO_CFG_TARGET_OS")
        .expect("CARGO_CFG_TARGET_OS not set")
        .as_str()
    {
        "linux" => "SPIRV_LINUX",
        "windows" => "SPIRV_WINDOWS",
        "macos" => "SPIRV_MAC",
        android if android.starts_with("android") => "SPIRV_ANDROID",
        "freebsd" => "SPIRV_FREEBSD",
        other => panic!("unsupported target os '{}'", other),
    };

    build.define(target_def, None);

    let compiler = build.get_compiler();

    if compiler.is_like_gnu() {
        build
            .flag("-Wall")
            .flag("-Wextra")
            .flag("-Wnon-virtual-dtor")
            .flag("-Wno-missing-field-initializers")
            .flag("-Werror")
            .flag("-std=c++11")
            .flag("-fno-exceptions")
            .flag("-fno-rtti")
            .flag("-Wno-long-long")
            .flag("-Wshadow")
            .flag("-Wundef")
            .flag("-Wconversion")
            .flag("-Wno-sign-conversion")
            .flag("-std=gnu++11");
    } else if compiler.is_like_clang() {
        build
            .flag("-Wextra-semi")
            .flag("-Wall")
            .flag("-Wextra")
            .flag("-Wnon-virtual-dtor")
            .flag("-Wno-missing-field-initializers")
            .flag("-Wno-self-assign")
            .flag("-Werror")
            .flag("-std=c++11")
            .flag("-fno-exceptions")
            .flag("-fno-rtti")
            .flag("-Wno-long-long")
            .flag("-Wshadow")
            .flag("-Wundef")
            .flag("-Wconversion")
            .flag("-Wno-sign-conversion")
            .flag("-ftemplate-depth=1024")
            .flag("-std=gnu++11");
    }

    build.cpp(true);
    build.compile("spirv-tools");
}
