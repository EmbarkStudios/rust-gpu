use crate::link;

// https://github.com/colin-kiegel/rust-pretty-assertions/issues/24
#[derive(PartialEq, Eq)]
#[doc(hidden)]
pub struct PrettyString<'a>(pub &'a str);
/// Make diff to display string as multi-line string
impl<'a> std::fmt::Debug for PrettyString<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.write_str(self.0)
    }
}

fn assemble_spirv(spirv: &str) -> Vec<u8> {
    use std::process::Command;
    use tempfile::tempdir;

    let temp = tempdir().expect("Unable to create temp dir");
    let input = temp.path().join("code.txt");
    let output = temp.path().join("code.spv");

    std::fs::write(&input, spirv).unwrap();

    let process = Command::new("spirv-as.exe")
        .arg(input.to_str().unwrap())
        .arg("-o")
        .arg(output.to_str().unwrap())
        .output()
        .expect("failed to execute process");

    println!("status: {}", process.status);
    println!("stdout: {}", String::from_utf8_lossy(&process.stdout));
    println!("stderr: {}", String::from_utf8_lossy(&process.stderr));

    assert!(process.status.success());

    std::fs::read(&output).unwrap()
}

#[allow(unused)]
fn validate(spirv: &[u32]) {
    use std::process::Command;
    use tempfile::tempdir;

    let temp = tempdir().expect("Unable to create temp dir");
    let input = temp.path().join("code.spv");

    let spirv = unsafe { std::slice::from_raw_parts(spirv.as_ptr() as *const u8, spirv.len() * 4) };

    std::fs::write(&input, spirv).unwrap();

    let process = Command::new("spirv-val.exe")
        .arg(input.to_str().unwrap())
        .output()
        .expect("failed to execute process");

    println!("status: {}", process.status);
    println!("stdout: {}", String::from_utf8_lossy(&process.stdout));
    println!("stderr: {}", String::from_utf8_lossy(&process.stderr));

    assert!(process.status.success());
}

fn load(bytes: &[u8]) -> rspirv::dr::Module {
    let mut loader = rspirv::dr::Loader::new();
    rspirv::binary::parse_bytes(&bytes, &mut loader).unwrap();
    let module = loader.module();
    module
}

fn assemble_and_link(binaries: &[&[u8]], opts: &crate::Options) -> rspirv::dr::Module {
    let mut modules = binaries.iter().cloned().map(load).collect::<Vec<_>>();
    let mut modules = modules.iter_mut().map(|m| m).collect::<Vec<_>>();

    link(&mut modules, opts)
}

fn without_header_eq(mut result: rspirv::dr::Module, expected: &str) {
    use rspirv::binary::Disassemble;
    //use rspirv::binary::Assemble;

    // validate(&result.assemble());

    result.header = None;
    let result = result.disassemble();

    let expected = expected
        .split("\n")
        .map(|l| l.trim())
        .collect::<Vec<_>>()
        .join("\n");

    let result = result
        .split("\n")
        .map(|l| l.trim().replace("  ", " ")) // rspirv outputs multiple spaces between operands
        .collect::<Vec<_>>()
        .join("\n");

    if result != expected {
        panic!(
            "assertion failed: `(left.contains(right))`\
            \n\
            \n{}\
            \n",
            pretty_assertions::Comparison::new(&PrettyString(&result), &PrettyString(&expected))
        )
    }
}

mod test {
    use crate::test::assemble_and_link;
    use crate::test::assemble_spirv;
    use crate::test::without_header_eq;
    use crate::Options;

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

        let result = assemble_and_link(&[&a, &b], &Options::default());
        let expect = r#"OpModuleProcessed "Linked by rspirv-linker"
        %1 = OpTypeFloat 32
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

        let result = assemble_and_link(&[&a], &Options::default());
        let expect = r#"OpModuleProcessed "Linked by rspirv-linker"
        %1 = OpTypeFloat 32
        %2 = OpVariable %1 Uniform"#;
        without_header_eq(result, expect);
    }

    #[test]
    fn lib_extra_exports() {
        let a = assemble_spirv(
            r#"OpCapability Linkage
            OpDecorate %1 LinkageAttributes "foo" Export
            %2 = OpTypeFloat 32
            %1 = OpVariable %2 Uniform"#,
        );

        let result = assemble_and_link(
            &[&a],
            &Options {
                lib: true,
                ..Default::default()
            },
        );

        let expect = r#"OpModuleProcessed "Linked by rspirv-linker"
        OpDecorate %1 LinkageAttributes "foo" Export
        %2 = OpTypeFloat 32
        %1 = OpVariable %2 Uniform"#;
        without_header_eq(result, expect);
    }
}
