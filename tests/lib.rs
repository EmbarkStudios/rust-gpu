use pretty_assertions::assert_eq;
use rspirv::binary::Disassemble;
use std::fs::{remove_file, File};
use std::io::prelude::*;
use std::process::Command;

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

const PREFIX: &str = r#"
#![feature(no_core, lang_items)]
#![no_core]

#[lang = "sized"]
pub trait Sized {}

#[lang = "unsize"]
pub trait Unsize<T: ?Sized> {}

#[lang = "coerce_unsized"]
pub trait CoerceUnsized<T> {}

#[lang = "copy"]
pub unsafe trait Copy {}

unsafe impl Copy for bool {}
unsafe impl Copy for u8 {}
unsafe impl Copy for u16 {}
unsafe impl Copy for u32 {}
unsafe impl Copy for u64 {}
unsafe impl Copy for usize {}
unsafe impl Copy for i8 {}
unsafe impl Copy for i16 {}
unsafe impl Copy for i32 {}
unsafe impl Copy for isize {}
unsafe impl Copy for f32 {}
unsafe impl Copy for char {}
unsafe impl<'a, T: ?Sized> Copy for &'a T {}
unsafe impl<T: ?Sized> Copy for *const T {}
unsafe impl<T: ?Sized> Copy for *mut T {}

#[lang = "add"]
pub trait Add<RHS = Self> {
    type Output;

    fn add(self, rhs: RHS) -> Self::Output;
}

impl Add for u32 {
    type Output = Self;

    fn add(self, rhs: Self) -> Self {
        self + rhs
    }
}
"#;

fn go(code: &str, expected: &str) {
    let temp = std::env::temp_dir();
    let mut input = temp.clone();
    input.push("code.rs");
    let mut output = temp.clone();
    output.push("code.spv");

    let mut input_data = PREFIX.to_string();
    input_data.push_str(code);
    File::create(&input)
        .unwrap()
        .write_all(&input_data.into_bytes())
        .unwrap();

    let cmd = Command::new("rustc")
        .args(&[
            "+nightly",
            "-Zcodegen-backend=target/debug/librustc_codegen_spirv.so",
            "--crate-type",
            "lib",
            "-O",
            "--out-dir",
        ])
        .arg(temp)
        .arg(&input)
        .status()
        .expect("failed to execute process");
    assert!(cmd.success());

    let mut output_data = Vec::new();
    File::open(&output)
        .unwrap()
        .read_to_end(&mut output_data)
        .unwrap();

    let output_disas = rspirv::dr::load_bytes(&output_data)
        .expect("failed to parse spirv")
        .disassemble();

    remove_file(input).expect("Failed to delete input file");
    remove_file(output).expect("Failed to delete output file");

    assert_eq!(PrettyString(&output_disas), PrettyString(expected));
}

#[test]
pub fn it_works() {
    go(
        r"
pub fn add_numbers(x: u32, y: u32) -> u32 {
    x + y
}",
        r"; SPIR-V
; Version: 1.5
; Generator: rspirv
; Bound: 14
OpMemoryModel Logical GLSL450
%1 = OpTypeInt 32 0
%2 = OpTypeFunction %1 %1 %1
%7 = OpTypeInt 32 1
%3 = OpFunction  %1  None %2
%4 = OpFunctionParameter  %1
%5 = OpFunctionParameter  %1
%6 = OpLabel
%8 = OpIAdd  %7  %4 %5
OpReturn
OpFunctionEnd
%9 = OpFunction  %1  None %2
%10 = OpFunctionParameter  %1
%11 = OpFunctionParameter  %1
%12 = OpLabel
%13 = OpIAdd  %7  %10 %11
OpReturn
OpFunctionEnd",
    );
}
