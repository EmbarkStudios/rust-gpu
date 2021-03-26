use super::{link, LinkResult, Options};
use pipe::pipe;
use rspirv::dr::{Loader, Module};
use rustc_driver::handle_options;
use rustc_errors::registry::Registry;
use rustc_session::config::build_session_options;
use rustc_session::config::Input;
use rustc_session::DiagnosticOutput;
use std::io::Read;
use std::path::PathBuf;

// https://github.com/colin-kiegel/rust-pretty-assertions/issues/24
#[derive(PartialEq, Eq)]
#[doc(hidden)]
pub struct PrettyString<'a>(pub &'a str);
/// Make diff to display string as multi-line string
impl<'a> std::fmt::Debug for PrettyString<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.0)
    }
}

fn assemble_spirv(spirv: &str) -> Vec<u8> {
    use spirv_tools::assembler::{self, Assembler};

    let assembler = assembler::create(None);

    let spv_binary = assembler
        .assemble(spirv, assembler::AssemblerOptions::default())
        .expect("Failed to assemble test spir-v");

    let contents: &[u8] = spv_binary.as_ref();
    contents.to_vec()
}

#[allow(unused)]
fn validate(spirv: &[u32]) {
    use spirv_tools::val::{self, Validator};

    let validator = val::create(None);

    validator
        .validate(spirv, None)
        .expect("validation error occurred");
}

fn load(bytes: &[u8]) -> Module {
    let mut loader = Loader::new();
    rspirv::binary::parse_bytes(&bytes, &mut loader).unwrap();
    loader.module()
}

fn assemble_and_link(binaries: &[&[u8]]) -> Result<Module, String> {
    let modules = binaries.iter().cloned().map(load).collect::<Vec<_>>();

    // need pipe here because Config takes ownership of the writer, and the writer must be 'static.
    let (mut read_diags, write_diags) = pipe();
    let thread = std::thread::spawn(move || {
        // must spawn thread because pipe crate is synchronous
        let mut diags = String::new();
        read_diags.read_to_string(&mut diags).unwrap();
        let suffix = "\n\nerror: aborting due to previous error\n\n";
        if diags.ends_with(suffix) {
            diags.truncate(diags.len() - suffix.len());
        }
        diags
    });
    let matches = handle_options(&["".to_string(), "x.rs".to_string()]).unwrap();
    let sopts = build_session_options(&matches);
    let config = rustc_interface::Config {
        opts: sopts,
        crate_cfg: Default::default(),
        input: Input::File(PathBuf::new()),
        input_path: None,
        output_file: None,
        output_dir: None,
        file_loader: None,
        diagnostic_output: DiagnosticOutput::Raw(Box::new(write_diags)),
        stderr: None,
        lint_caps: Default::default(),
        parse_sess_created: None,
        register_lints: None,
        override_queries: None,
        make_codegen_backend: None,
        registry: Registry::new(&[]),
    };
    rustc_interface::interface::run_compiler(config, |compiler| {
        let res = link(
            compiler.session(),
            modules,
            &Options {
                compact_ids: true,
                dce: false,
                inline: false,
                mem2reg: false,
                structurize: false,
                use_new_structurizer: false,
                emit_multiple_modules: false,
            },
        );
        assert_eq!(compiler.session().has_errors(), res.is_err());
        res.map(|res| match res {
            LinkResult::SingleModule(m) => m,
            LinkResult::MultipleModules(_) => unreachable!(),
        })
    })
    .map_err(|_e| thread.join().unwrap())
}

fn without_header_eq(mut result: Module, expected: &str) {
    use rspirv::binary::Disassemble;
    //use rspirv::binary::Assemble;

    // validate(&result.assemble());

    result.header = None;
    let result = result.disassemble();

    let expected = expected
        .split('\n')
        .map(|l| l.trim())
        .collect::<Vec<_>>()
        .join("\n");

    let result = result
        .split('\n')
        .map(|l| l.trim().replace("  ", " ")) // rspirv outputs multiple spaces between operands
        .collect::<Vec<_>>()
        .join("\n");

    if result != expected {
        println!("{}", &result);
        panic!(
            "assertion failed: `(left.contains(right))`\
            \n\
            \n{}\
            \n",
            pretty_assertions::Comparison::new(&PrettyString(&result), &PrettyString(&expected))
        )
    }
}

#[test]
fn standard() {
    let a = assemble_spirv(
        r#"OpCapability Linkage
        OpDecorate %1 LinkageAttributes "foo" Import
        %2 = OpTypeFloat 32
        %1 = OpVariable %2 Uniform
        %3 = OpVariable %2 Input"#,
    );

    let b = assemble_spirv(
        r#"OpCapability Linkage
        OpDecorate %1 LinkageAttributes "foo" Export
        %2 = OpTypeFloat 32
        %3 = OpConstant %2 42
        %1 = OpVariable %2 Uniform %3
        "#,
    );

    let result = assemble_and_link(&[&a, &b]).unwrap();
    let expect = r#"%1 = OpTypeFloat 32
        %2 = OpVariable %1 Input
        %3 = OpConstant %1 42.0
        %4 = OpVariable %1 Uniform %3"#;

    without_header_eq(result, expect);
}

#[test]
fn not_a_lib_extra_exports() {
    let a = assemble_spirv(
        r#"OpCapability Linkage
            OpDecorate %1 LinkageAttributes "foo" Export
            %2 = OpTypeFloat 32
            %1 = OpVariable %2 Uniform"#,
    );

    let result = assemble_and_link(&[&a]).unwrap();
    let expect = r#"%1 = OpTypeFloat 32
        %2 = OpVariable %1 Uniform"#;
    without_header_eq(result, expect);
}

#[test]
fn unresolved_symbol() {
    let a = assemble_spirv(
        r#"OpCapability Linkage
            OpDecorate %1 LinkageAttributes "foo" Import
            %2 = OpTypeFloat 32
            %1 = OpVariable %2 Uniform"#,
    );

    let b = assemble_spirv("OpCapability Linkage");

    let result = assemble_and_link(&[&a, &b]);

    assert_eq!(
        result.err().as_deref(),
        Some("error: Unresolved symbol \"foo\"")
    );
}

#[test]
fn type_mismatch() {
    let a = assemble_spirv(
        r#"OpCapability Linkage
            OpDecorate %1 LinkageAttributes "foo" Import
            %2 = OpTypeFloat 32
            %1 = OpVariable %2 Uniform
            %3 = OpVariable %2 Input"#,
    );

    let b = assemble_spirv(
        r#"OpCapability Linkage
            OpDecorate %1 LinkageAttributes "foo" Export
            %2 = OpTypeInt 32 0
            %3 = OpConstant %2 42
            %1 = OpVariable %2 Uniform %3
        "#,
    );

    let result = assemble_and_link(&[&a, &b]);
    assert_eq!(
        result.err().as_deref(),
        Some("error: Types mismatch for \"foo\"")
    );
}

#[test]
fn multiple_definitions() {
    let a = assemble_spirv(
        r#"OpCapability Linkage
            OpDecorate %1 LinkageAttributes "foo" Import
            %2 = OpTypeFloat 32
            %1 = OpVariable %2 Uniform
            %3 = OpVariable %2 Input"#,
    );

    let b = assemble_spirv(
        r#"OpCapability Linkage
            OpCapability Linkage
            OpDecorate %1 LinkageAttributes "foo" Export
            %2 = OpTypeFloat 32
            %3 = OpConstant %2 42
            %1 = OpVariable %2 Uniform %3"#,
    );

    let c = assemble_spirv(
        r#"OpCapability Linkage
            OpDecorate %1 LinkageAttributes "foo" Export
            %2 = OpTypeFloat 32
            %3 = OpConstant %2 -1
            %1 = OpVariable %2 Uniform %3"#,
    );

    let result = assemble_and_link(&[&a, &b, &c]);
    assert_eq!(
        result.err().as_deref(),
        Some("error: Multiple exports found for \"foo\"")
    );
}

#[test]
fn multiple_definitions_different_types() {
    let a = assemble_spirv(
        r#"OpCapability Linkage
            OpDecorate %1 LinkageAttributes "foo" Import
            %2 = OpTypeFloat 32
            %1 = OpVariable %2 Uniform
            %3 = OpVariable %2 Input"#,
    );

    let b = assemble_spirv(
        r#"OpCapability Linkage
            OpCapability Linkage
            OpDecorate %1 LinkageAttributes "foo" Export
            %2 = OpTypeInt 32 0
            %3 = OpConstant %2 42
            %1 = OpVariable %2 Uniform %3"#,
    );

    let c = assemble_spirv(
        r#"OpCapability Linkage
            OpDecorate %1 LinkageAttributes "foo" Export
            %2 = OpTypeFloat 32
            %3 = OpConstant %2 12
            %1 = OpVariable %2 Uniform %3"#,
    );

    let result = assemble_and_link(&[&a, &b, &c]);
    assert_eq!(
        result.err().as_deref(),
        Some("error: Multiple exports found for \"foo\"")
    );
}

//jb-todo: this isn't validated yet in the linker (see ensure_matching_import_export_pairs)
/*#[test]
fn decoration_mismatch() {
    let a = assemble_spirv(
        r#"OpCapability Linkage
        OpDecorate %1 LinkageAttributes "foo" Import
        OpDecorate %2 Constant
        %2 = OpTypeFloat 32
        %1 = OpVariable %2 Uniform
        %3 = OpVariable %2 Input"#,
    );

    let b = assemble_spirv(
        r#"OpCapability Linkage
        OpDecorate %1 LinkageAttributes "foo" Export
        %2 = OpTypeFloat 32
        %3 = OpConstant %2 42
        %1 = OpVariable %2 Uniform %3"#,
    );

    let result = assemble_and_link(&[&a, &b]);
    assert_eq!(
        result.err(),
        Some(LinkerError::MultipleExports("foo".to_string()))
    );
    Ok(())
}*/

#[test]
fn func_ctrl() {
    let a = assemble_spirv(
        r#"OpCapability Linkage
            OpDecorate %1 LinkageAttributes "foo" Import
            %2 = OpTypeVoid
            %3 = OpTypeFunction %2
            %4 = OpTypeFloat 32
            %5 = OpVariable %4 Uniform
            %1 = OpFunction %2 None %3
            OpFunctionEnd"#,
    );

    let b = assemble_spirv(
        r#"OpCapability Linkage
            OpDecorate %1 LinkageAttributes "foo" Export
            %2 = OpTypeVoid
            %3 = OpTypeFunction %2
            %1 = OpFunction %2 Inline %3
            %4 = OpLabel
            OpReturn
            OpFunctionEnd"#,
    );

    let result = assemble_and_link(&[&a, &b]).unwrap();

    let expect = r#"%1 = OpTypeVoid
            %2 = OpTypeFunction %1
            %3 = OpTypeFloat 32
            %4 = OpVariable %3 Uniform
            %5 = OpFunction %1 Inline %2
            %6 = OpLabel
            OpReturn
            OpFunctionEnd"#;

    without_header_eq(result, expect);
}

#[test]
fn use_exported_func_param_attr() {
    let a = assemble_spirv(
        r#"OpCapability Kernel
            OpCapability Linkage
            OpDecorate %1 LinkageAttributes "foo" Import
            OpDecorate %2 FuncParamAttr Zext
            %2 = OpDecorationGroup
            OpGroupDecorate %2 %3 %4
            %5 = OpTypeVoid
            %6 = OpTypeInt 32 0
            %7 = OpTypeFunction %5 %6
            %1 = OpFunction %5 None %7
            %3 = OpFunctionParameter %6
            OpFunctionEnd
            %8 = OpFunction %5 None %7
            %4 = OpFunctionParameter %6
            OpFunctionEnd
            "#,
    );

    let b = assemble_spirv(
        r#"OpCapability Kernel
            OpCapability Linkage
            OpDecorate %1 LinkageAttributes "foo" Export
            OpDecorate %2 FuncParamAttr Sext
            %3 = OpTypeVoid
            %4 = OpTypeInt 32 0
            %5 = OpTypeFunction %3 %4
            %1 = OpFunction %3 None %5
            %2 = OpFunctionParameter %4
            %6 = OpLabel
            OpReturn
            OpFunctionEnd
            "#,
    );

    let result = assemble_and_link(&[&a, &b]).unwrap();

    let expect = r#"OpCapability Kernel
        OpDecorate %1 FuncParamAttr Zext
        %1 = OpDecorationGroup
        OpGroupDecorate %1 %2
        OpDecorate %3 FuncParamAttr Sext
        %4 = OpTypeVoid
        %5 = OpTypeInt 32 0
        %6 = OpTypeFunction %4 %5
        %7 = OpFunction %4 None %6
        %2 = OpFunctionParameter %5
        OpFunctionEnd
        %8 = OpFunction %4 None %6
        %3 = OpFunctionParameter %5
        %9 = OpLabel
        OpReturn
        OpFunctionEnd"#;

    without_header_eq(result, expect);
}

#[test]
fn names_and_decorations() {
    let a = assemble_spirv(
        r#"OpCapability Kernel
            OpCapability Linkage
            OpName %1 "foo"
            OpName %3 "param"
            OpDecorate %1 LinkageAttributes "foo" Import
            OpDecorate %2 Restrict
            OpDecorate %4 NonWritable
            %2 = OpDecorationGroup
            OpGroupDecorate %2 %3 %4
            %5 = OpTypeVoid
            %6 = OpTypeInt 32 0
            %9 = OpTypePointer Function %6
            %7 = OpTypeFunction %5 %9
            %1 = OpFunction %5 None %7
            %3 = OpFunctionParameter %9
            OpFunctionEnd
            %8 = OpFunction %5 None %7
            %4 = OpFunctionParameter %9
            OpFunctionEnd
            "#,
    );

    let b = assemble_spirv(
        r#"OpCapability Kernel
            OpCapability Linkage
            OpName %1 "foo"
            OpName %2 "param"
            OpDecorate %1 LinkageAttributes "foo" Export
            OpDecorate %2 Restrict
            %3 = OpTypeVoid
            %4 = OpTypeInt 32 0
            %7 = OpTypePointer Function %4
            %5 = OpTypeFunction %3 %7
            %1 = OpFunction %3 None %5
            %2 = OpFunctionParameter %7
            %6 = OpLabel
            OpReturn
            OpFunctionEnd
            "#,
    );

    let result = assemble_and_link(&[&a, &b]).unwrap();

    let expect = r#"OpCapability Kernel
        OpName %1 "foo"
        OpName %2 "param"
        OpDecorate %3 Restrict
        OpDecorate %4 NonWritable
        %3 = OpDecorationGroup
        OpGroupDecorate %3 %4
        OpDecorate %2 Restrict
        %5 = OpTypeVoid
        %6 = OpTypeInt 32 0
        %7 = OpTypePointer Function %6
        %8 = OpTypeFunction %5 %7
        %9 = OpFunction %5 None %8
        %4 = OpFunctionParameter %7
        OpFunctionEnd
        %1 = OpFunction %5 None %8
        %2 = OpFunctionParameter %7
        %10 = OpLabel
        OpReturn
        OpFunctionEnd"#;

    without_header_eq(result, expect);
}
