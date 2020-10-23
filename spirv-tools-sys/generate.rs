// The spirv tools use generated code, for now we just replicate the minimum
// generation we need here by calling the *shudders* python script(s) we need
// to in a simple script and commit them to source control, as they only need
// to be regenerated when spirv-headers is updated

use std::{fs, process::Command};

fn python<S: AsRef<std::ffi::OsStr>>(args: impl IntoIterator<Item = S>) -> Result<(), i32> {
    Command::new("python")
        .args(args.into_iter())
        .status()
        .map_err(|_| -1)
        .and_then(|es| {
            if es.success() {
                Ok(())
            } else {
                Err(es.code().unwrap_or(-1))
            }
        })
}

fn main() {
    fs::create_dir_all("generated").expect("unable to create 'generated'");

    python(&[
        "spirv-tools/utils/generate_grammar_tables.py",
        "--spirv-core-grammar=spirv-headers/include/spirv/unified1/spirv.core.grammar.json",
        "--extinst-debuginfo-grammar=spirv-headers/include/spirv/unified1/extinst.debuginfo.grammar.json",
        "--extinst-cldebuginfo100-grammar=spirv-headers/include/spirv/unified1/extinst.opencl.debuginfo.100.grammar.json",
        "--extension-enum-output=generated/extension_enum.inc",
        "--enum-string-mapping-output=generated/enum_string_mapping.inc",
    ]).expect("failed to generate enum includes from spirv-headers");

    python(&[
        "spirv-tools/utils/update_build_version.py",
        "spirv-tools",
        "generated/build-version.inc",
    ])
    .expect("failed to generate build version from spirv-headers");

    core_table("unified1");
    glsl_table("unified1");
    opencl_table("unified1");

    vendor_table("spv-amd-shader-explicit-vertex-parameter", None);
    vendor_table("spv-amd-shader-trinary-minmax", None);
    vendor_table("spv-amd-gcn-shader", None);
    vendor_table("spv-amd-shader-ballot", None);
    vendor_table("debuginfo", None);
    vendor_table("nonsemantic.clspvreflection", None);
    vendor_table("opencl.debuginfo.100", Some("CLDEBUG100_"));

    registry_table();
}

fn vendor_table(which: &str, prefix: Option<&str>) {
    python(&[
        "spirv-tools/utils/generate_grammar_tables.py".to_owned(),
        format!(
            "--extinst-vendor-grammar=spirv-headers/include/spirv/unified1/extinst.{}.grammar.json",
            which
        ),
        format!("--vendor-insts-output=generated/{}.insts.inc", which),
        format!(
            "--vendor-operand-kind-prefix={}",
            prefix.unwrap_or_default()
        ),
    ])
    .expect("failed to generate vendor table");
}

fn core_table(which: &str) {
    python(&[
        "spirv-tools/utils/generate_grammar_tables.py".to_owned(),
        "--spirv-core-grammar=spirv-headers/include/spirv/unified1/spirv.core.grammar.json".to_owned(),
        format!("--core-insts-output=generated/core.insts-{}.inc", which),
        "--extinst-debuginfo-grammar=spirv-headers/include/spirv/unified1/extinst.debuginfo.grammar.json".to_owned(),
        "--extinst-cldebuginfo100-grammar=spirv-headers/include/spirv/unified1/extinst.opencl.debuginfo.100.grammar.json".to_owned(),
        format!("--operand-kinds-output=generated/operand.kinds-{}.inc", which),
    ]).expect("failed to generate core table from spirv-headers");
}

fn registry_table() {
    python(&[
        "spirv-tools/utils/generate_registry_tables.py",
        "--xml=spirv-headers/include/spirv/spir-v.xml",
        "--generator=generated/generators.inc",
    ])
    .expect("failed to generate core table from spirv-headers");
}

fn glsl_table(version: &str) {
    python(&[
        "spirv-tools/utils/generate_grammar_tables.py".to_owned(),
        format!("--spirv-core-grammar=spirv-headers/include/spirv/{}/spirv.core.grammar.json", version),
        "--extinst-debuginfo-grammar=spirv-headers/include/spirv/unified1/extinst.debuginfo.grammar.json".to_owned(),
        "--extinst-cldebuginfo100-grammar=spirv-headers/include/spirv/unified1/extinst.opencl.debuginfo.100.grammar.json".to_owned(),
        format!("--extinst-glsl-grammar=spirv-headers/include/spirv/{}/extinst.glsl.std.450.grammar.json", version),
        "--glsl-insts-output=generated/glsl.std.450.insts.inc".to_owned(),
    ]).expect("failed to generate glsl table from spirv-headers");
}

fn opencl_table(version: &str) {
    python(&[
        "spirv-tools/utils/generate_grammar_tables.py".to_owned(),
        format!("--spirv-core-grammar=spirv-headers/include/spirv/{}/spirv.core.grammar.json", version),
        "--extinst-debuginfo-grammar=spirv-headers/include/spirv/unified1/extinst.debuginfo.grammar.json".to_owned(),
        "--extinst-cldebuginfo100-grammar=spirv-headers/include/spirv/unified1/extinst.opencl.debuginfo.100.grammar.json".to_owned(),
        format!("--extinst-opencl-grammar=spirv-headers/include/spirv/{}/extinst.opencl.std.100.grammar.json", version),
        "--opencl-insts-output=generated/opencl.std.insts.inc".to_owned(),
    ]).expect("failed to generate glsl table from spirv-headers");
}
