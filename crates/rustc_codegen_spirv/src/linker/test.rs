use super::{link, LinkResult, Options};
use crate::codegen_cx::SpirvMetadata;
use pipe::pipe;
use rspirv::dr::{Loader, Module};
use rustc_errors::registry::Registry;
use rustc_span::edition::Edition;
use std::{io::Read, thread};

// https://github.com/colin-kiegel/rust-pretty-assertions/issues/24
#[derive(PartialEq, Eq)]
struct PrettyString(String);
impl std::fmt::Debug for PrettyString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // HACK(eddyb) add extra newlines for readability when it shows up
        // in `Result::unwrap` panic messages specifically.
        f.write_str("\n")?;
        f.write_str(self)?;
        f.write_str("\n")
    }
}
impl std::ops::Deref for PrettyString {
    type Target = str;
    fn deref(&self) -> &str {
        &self.0
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
    rspirv::binary::parse_bytes(bytes, &mut loader).unwrap();
    loader.module()
}

// HACK(shesp) This function was taken from `rustc_interface::util` since it's
// no longer public. It's the non-parallel version since it's simpler and good
// enough for our tests.
fn run_in_thread_pool_with_globals<F: FnOnce() -> R + Send, R: Send>(
    edition: Edition,
    _threads: usize,
    f: F,
) -> R {
    // The "thread pool" is a single spawned thread in the non-parallel
    // compiler. We run on a spawned thread instead of the main thread (a) to
    // provide control over the stack size, and (b) to increase similarity with
    // the parallel compiler, in particular to ensure there is no accidental
    // sharing of data between the main thread and the compilation thread
    // (which might cause problems for the parallel compiler).
    let mut builder = thread::Builder::new().name("rustc".to_string());
    const STACK_SIZE: usize = 8 * 1024 * 1024;
    builder = builder.stack_size(STACK_SIZE);

    // We build the session globals and run `f` on the spawned thread, because
    // `SessionGlobals` does not impl `Send` in the non-parallel compiler.
    thread::scope(|s| {
        // `unwrap` is ok here because `spawn_scoped` only panics if the thread
        // name contains null bytes.
        let r = builder
            .spawn_scoped(s, move || {
                rustc_span::create_session_globals_then(edition, f)
            })
            .unwrap()
            .join();

        match r {
            Ok(v) => v,
            Err(e) => std::panic::resume_unwind(e),
        }
    })
}

fn assemble_and_link(binaries: &[&[u8]]) -> Result<Module, PrettyString> {
    let modules = binaries.iter().cloned().map(load).collect::<Vec<_>>();

    // FIXME(eddyb) this seems ridiculous, we should be able to write to
    // some kind of `Arc<Mutex<Vec<u8>>>` without a separate thread.
    //
    // need pipe here because the writer must be 'static.
    let (mut read_diags, write_diags) = pipe();
    let read_diags_thread = std::thread::spawn(move || {
        // must spawn thread because pipe crate is synchronous
        let mut diags = String::new();
        read_diags.read_to_string(&mut diags).unwrap();
        if let Some(diags_without_trailing_newlines) = diags.strip_suffix("\n\n") {
            diags.truncate(diags_without_trailing_newlines.len());
        }
        diags
    });

    // NOTE(eddyb) without `catch_fatal_errors`, you'd get the really strange
    // effect of test failures with no output (because the `FatalError` "panic"
    // is really a silent unwinding device, that should be treated the same as
    // `Err(ErrorGuaranteed)` returns from `link`).
    rustc_driver::catch_fatal_errors(|| {
        let matches = rustc_driver::handle_options(&["".to_string(), "x.rs".to_string()]).unwrap();
        let sopts = rustc_session::config::build_session_options(&matches);

        run_in_thread_pool_with_globals(sopts.edition, sopts.unstable_opts.threads, || {
            let mut sess = rustc_session::build_session(
                sopts,
                None,
                None,
                Registry::new(&[]),
                Default::default(),
                None,
                None,
            );

            // HACK(eddyb) inject `write_diags` into `sess`, to work around
            // the removals in https://github.com/rust-lang/rust/pull/102992.
            sess.parse_sess.span_diagnostic = {
                let fallback_bundle = {
                    extern crate rustc_error_messages;
                    rustc_error_messages::fallback_fluent_bundle(
                        rustc_errors::DEFAULT_LOCALE_RESOURCES,
                        sess.opts.unstable_opts.translate_directionality_markers,
                    )
                };
                let emitter = rustc_errors::emitter::EmitterWriter::new(
                    Box::new(write_diags),
                    Some(sess.parse_sess.clone_source_map()),
                    None,
                    fallback_bundle,
                    false,
                    false,
                    false,
                    None,
                    false,
                );

                rustc_errors::Handler::with_emitter_and_flags(
                    Box::new(emitter),
                    sess.opts.unstable_opts.diagnostic_handler_flags(true),
                )
            };

            let res = link(
                &sess,
                modules,
                &Options {
                    compact_ids: true,
                    dce: false,
                    structurize: false,
                    emit_multiple_modules: false,
                    spirv_metadata: SpirvMetadata::None,
                },
            );
            assert_eq!(sess.has_errors(), res.as_ref().err().copied());
            res.map(|res| match res {
                LinkResult::SingleModule(m) => *m,
                LinkResult::MultipleModules(_) => unreachable!(),
            })
        })
    })
    .flatten()
    .map_err(|_e| read_diags_thread.join().unwrap())
    .map_err(PrettyString)
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
            "assertion failed: `left == right`\
            \n\
            \n{}\
            \n",
            pretty_assertions::Comparison::new(&PrettyString(expected), &PrettyString(result))
        );
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
        Some("error: Types mismatch for \"foo\"\n  |\n  = note: import type: (TypeFloat)\n  = note: export type: (TypeInt)")
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
            %1 = OpFunction %2 DontInline %3
            %4 = OpLabel
            OpReturn
            OpFunctionEnd"#,
    );

    let result = assemble_and_link(&[&a, &b]).unwrap();

    let expect = r#"%1 = OpTypeVoid
            %2 = OpTypeFunction %1
            %3 = OpTypeFloat 32
            %4 = OpVariable %3 Uniform
            %5 = OpFunction %1 DontInline %2
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
            %9 = OpLabel
            %10 = OpLoad %6 %4
            OpReturn
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
            %7 = OpLoad %4 %2
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
        %8 = OpLabel
        %9 = OpLoad %5 %2
        OpReturn
        OpFunctionEnd
        %10 = OpFunction %4 None %6
        %3 = OpFunctionParameter %5
        %11 = OpLabel
        %12 = OpLoad %5 %3
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
            %10 = OpLabel
            %11 = OpLoad %6 %4
            OpReturn
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
            %8 = OpLoad %4 %2
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
        %10 = OpLabel
        %11 = OpLoad %6 %4
        OpReturn
        OpFunctionEnd
        %1 = OpFunction %5 None %8
        %2 = OpFunctionParameter %7
        %12 = OpLabel
        %13 = OpLoad %6 %2
        OpReturn
        OpFunctionEnd"#;

    without_header_eq(result, expect);
}
