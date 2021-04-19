use super::{dis_entry_fn, dis_fn, dis_globals, val};
use std::ffi::OsStr;

struct SetEnvVar<'a> {
    k: &'a OsStr,
}

impl<'a> SetEnvVar<'a> {
    fn new(k: &'a impl AsRef<OsStr>, v: impl AsRef<OsStr>) -> Self {
        let k = k.as_ref();
        std::env::set_var(k, v);
        Self { k }
    }
}

impl<'a> Drop for SetEnvVar<'a> {
    fn drop(&mut self) {
        std::env::remove_var(self.k)
    }
}

#[test]
fn custom_entry_point() {
    dis_globals(
        r#"
#[spirv(fragment(entry_point_name="hello_world"))]
pub fn main() { }
"#,
        r#"OpCapability Shader
OpMemoryModel Logical Simple
OpEntryPoint Fragment %1 "hello_world"
OpExecutionMode %1 OriginUpperLeft
OpName %2 "test_project::main"
%3 = OpTypeVoid
%4 = OpTypeFunction %3"#,
    );
}

#[test]
// blocked on: https://github.com/EmbarkStudios/rust-gpu/issues/69
#[ignore]
fn no_dce() {
    let _var = SetEnvVar::new(&"NO_DCE", "1");
    val(r#"
#[spirv(fragment)]
pub fn no_dce() {
}
"#);
}

#[test]
fn add_two_ints() {
    dis_fn(
        r#"
fn add_two_ints(x: u32, y: u32) -> u32 {
    x + y
}
#[spirv(fragment)]
pub fn main() {
    add_two_ints(2, 3);
}
"#,
        "add_two_ints",
        r#"%1 = OpFunction %2 None %3
%4 = OpFunctionParameter %2
%5 = OpFunctionParameter %2
%6 = OpLabel
%7 = OpIAdd %2 %4 %5
OpReturnValue %7
OpFunctionEnd"#,
    );
}

#[test]
fn asm() {
    dis_fn(
        r#"
fn asm() {
    unsafe {
        asm!(
            "%int = OpTypeInt 32 0",
            "%scope = OpConstant %int 2",
            "%semantics = OpConstant %int 16",
            "OpMemoryBarrier %scope %semantics",
        );
    }
}
#[spirv(fragment)]
pub fn main() {
    asm();
}
"#,
        "asm",
        // note: the OpConstants get hoisted out to global in the linker merge pass
        r#"%1 = OpFunction %2 None %3
%4 = OpLabel
OpMemoryBarrier %5 %6
OpReturn
OpFunctionEnd"#,
    );
}

#[test]
fn asm_add_two_ints() {
    dis_fn(
        r#"
fn add_two_ints(x: u32, y: u32) -> u32 {
    let result;
    unsafe {
        asm!(
            "{0} = OpIAdd typeof{0} {1} {2}",
            out(reg) result,
            in(reg) x,
            in(reg) y,
        );
    }
    result
}
#[spirv(fragment)]
pub fn main() {
    add_two_ints(2, 3);
}
"#,
        "add_two_ints",
        r#"%1 = OpFunction %2 None %3
%4 = OpFunctionParameter %2
%5 = OpFunctionParameter %2
%6 = OpLabel
%7 = OpIAdd %2 %4 %5
OpReturnValue %7
OpFunctionEnd"#,
    );
}

#[test]
fn asm_op_decorate() {
    // Tests that OpDecorate gets parsed and emitted properly since it's a vararg style instruction
    dis_globals(
        r#"
        fn add_decorate() {
            unsafe {
                let offset = 1u32;
                asm!(
                        "OpExtension \"SPV_EXT_descriptor_indexing\"",
                        "OpCapability RuntimeDescriptorArray",
                        "OpDecorate %image_2d_var DescriptorSet 0",
                        "OpDecorate %image_2d_var Binding 0",
                        "%uint                  = OpTypeInt 32 0",
                        "%float                 = OpTypeFloat 32",
                        "%uint_0                = OpConstant %uint 0",
                        "%image_2d              = OpTypeImage %float Dim2D 0 0 0 1 Unknown",
                        "%sampled_image_2d      = OpTypeSampledImage %image_2d",
                        "%image_array           = OpTypeRuntimeArray %sampled_image_2d",
                        // NOTE(eddyb) `Generic` is used here because it's the placeholder
                        // for storage class inference - both of the two `OpTypePointer`
                        // types below should end up inferring to `UniformConstant`.
                        "%ptr_image_array       = OpTypePointer Generic %image_array",
                        "%image_2d_var          = OpVariable %ptr_image_array UniformConstant",
                        "%ptr_sampled_image_2d  = OpTypePointer Generic %sampled_image_2d",
                        "", // ^^ type preamble
                        "%offset                = OpLoad _ {0}",
                        "%24                    = OpAccessChain %ptr_sampled_image_2d %image_2d_var %offset",
                        "%25                    = OpLoad %sampled_image_2d %24",
                        in(reg) &offset,
                    );
            }
        }
        #[spirv(fragment)]
        pub fn main() {
            add_decorate();
        }"#,
        r#"OpCapability Shader
OpCapability RuntimeDescriptorArray
OpExtension "SPV_EXT_descriptor_indexing"
OpMemoryModel Logical Simple
OpEntryPoint Fragment %1 "main"
OpExecutionMode %1 OriginUpperLeft
OpName %2 "test_project::add_decorate"
OpName %3 "test_project::main"
OpDecorate %4 ArrayStride 4
OpDecorate %5 DescriptorSet 0
OpDecorate %5 Binding 0
%6 = OpTypeVoid
%7 = OpTypeFunction %6
%8 = OpTypeInt 32 0
%9 = OpConstant %8 1
%10 = OpTypeFloat 32
%11 = OpTypeImage %10 2D 0 0 0 1 Unknown
%12 = OpTypeSampledImage %11
%4 = OpTypeRuntimeArray %12
%13 = OpTypePointer UniformConstant %4
%5 = OpVariable %13 UniformConstant
%14 = OpTypePointer UniformConstant %12"#,
    );
}

#[test]
fn unroll_loops() {
    dis_fn(
        // FIXME(eddyb) use `for _ in 0..10` here when that works.
        r#"
#[spirv(unroll_loops)]
fn java_hash_ten_times(mut x: u32, y: u32) -> u32 {
    let mut i = 0;
    while i < 10 {
        x = 31 * x + y;
        i += 1;
    }
    x
}
#[spirv(fragment)]
pub fn main() {
    java_hash_ten_times(7, 42);
}
"#,
        "java_hash_ten_times",
        // NOTE(eddyb) this is very verbose because of the new structurizer
        // producing messier control-flow than necessary, but the important part
        // being tested is `OpLoopMerge` having `Unroll` as its "Loop Control".
        r#"%1 = OpFunction %2 None %3
%4 = OpFunctionParameter %2
%5 = OpFunctionParameter %2
%6 = OpLabel
OpBranch %7
%7 = OpLabel
OpBranch %8
%8 = OpLabel
%9 = OpPhi %10 %11 %7 %12 %13
%14 = OpPhi %2 %4 %7 %15 %13
%16 = OpPhi %17 %18 %7 %19 %13
OpLoopMerge %20 %13 Unroll
OpBranchConditional %16 %21 %20
%21 = OpLabel
%22 = OpSLessThan %17 %9 %23
OpSelectionMerge %24 None
OpBranchConditional %22 %25 %26
%25 = OpLabel
%27 = OpIMul %2 %28 %14
%15 = OpIAdd %2 %27 %5
%12 = OpIAdd %10 %9 %29
OpBranch %24
%26 = OpLabel
OpReturnValue %14
%24 = OpLabel
%19 = OpPhi %17 %18 %25
OpBranch %13
%13 = OpLabel
OpBranch %8
%20 = OpLabel
OpUnreachable
OpFunctionEnd"#,
    );
}

#[test]
fn complex_image_sample_inst() {
    dis_fn(
        r#"
    fn sample_proj_lod(coord: glam::Vec4, ddx: glam::Vec2, ddy: glam::Vec2, offset_x: i32, offset_y: i32) -> glam::Vec4 {
        unsafe {
            let mut result = glam::Vec4::default();
            let index = 0u32;
            asm!(
                    "OpExtension \"SPV_EXT_descriptor_indexing\"",
                    "OpCapability RuntimeDescriptorArray",
                    "OpDecorate %image_2d_var DescriptorSet 0",
                    "OpDecorate %image_2d_var Binding 0",
                    "%uint                  = OpTypeInt 32 0",
                    "%int                   = OpTypeInt 32 1",
                    "%float                 = OpTypeFloat 32",
                    "%v2int                 = OpTypeVector %int 2",
                    "%uint_0                = OpConstant %uint 0",
                    "%int_0                 = OpConstant %int 0",
                    "%image_2d              = OpTypeImage %float Dim2D 0 0 0 1 Unknown",
                    "%sampled_image_2d      = OpTypeSampledImage %image_2d",
                    "%image_array           = OpTypeRuntimeArray %sampled_image_2d",
                    // NOTE(eddyb) `Generic` is used here because it's the placeholder
                    // for storage class inference - both of the two `OpTypePointer`
                    // types below should end up inferring to `UniformConstant`.
                    "%ptr_image_array       = OpTypePointer Generic %image_array",
                    "%image_2d_var          = OpVariable %ptr_image_array UniformConstant",
                    "%ptr_sampled_image_2d  = OpTypePointer Generic %sampled_image_2d",
                    "", // ^^ type preamble
                    "%offset                = OpLoad _ {1}",
                    "%24                    = OpAccessChain %ptr_sampled_image_2d %image_2d_var %offset",
                    "%25                    = OpLoad %sampled_image_2d %24",
                    "%coord                 = OpLoad _ {0}",
                    "%ddx                   = OpLoad _ {3}",
                    "%ddy                   = OpLoad _ {4}",
                    "%offset_x              = OpLoad _ {5}",
                    "%offset_y              = OpLoad _ {6}",
                    "%const_offset          = OpConstantComposite %v2int %int_0 %int_0",
                    "%result                = OpImageSampleProjExplicitLod _ %25 %coord Grad|ConstOffset %ddx %ddy %const_offset",
                    "OpStore {2} %result",
                    in(reg) &coord,
                    in(reg) &index,
                    in(reg) &mut result,
                    in(reg) &ddx,
                    in(reg) &ddy,
                    in(reg) &offset_x,
                    in(reg) &offset_y,
                );
            result
        }
    }
    #[spirv(fragment)]
    pub fn main() {
        sample_proj_lod(glam::Vec4::ZERO, glam::Vec2::ZERO, glam::Vec2::ZERO, 0, 0);
    }"#,
        "sample_proj_lod",
        "%1 = OpFunction %2 None %3
%4 = OpFunctionParameter %2
%5 = OpFunctionParameter %6
%7 = OpFunctionParameter %6
%8 = OpFunctionParameter %9
%10 = OpFunctionParameter %9
%11 = OpLabel
%12 = OpAccessChain %13 %14 %15
%16 = OpLoad %17 %12
%18 = OpImageSampleProjExplicitLod %2 %16 %4 Grad|ConstOffset %5 %7 %19
OpReturnValue %18
OpFunctionEnd",
    );
}

/// Helper to generate all of the `ptr_*` tests below, which test that the various
/// ways to use raw pointer `read`/`write`/`copy`, to copy a single value, work,
/// and that the resulting SPIR-V uses either a pair of `OpLoad` and `OpStore`,
/// and/or the `OpCopyMemory` instruction, but *not* `OpCopyMemorySized`.
macro_rules! test_copy_via_raw_ptr {
    ($copy_expr:literal => $spirv:literal) => {
        dis_fn(
            concat!(
                r#"
        fn copy_via_raw_ptr(src: &f32, dst: &mut f32) {
            unsafe {
                "#,
                $copy_expr,
                r#"
            }
        }
        #[spirv(fragment)]
        pub fn main(i: f32, o: &mut f32) {
            copy_via_raw_ptr(&i, o);
            // FIXME(eddyb) above call results in inlining `copy_via_raw_ptr`,
            // due to the to `Output` storage classe, so to get the disassembled
            // function we also need `Function`-local pointers:
            let (src, mut dst) = (0.0, 0.0);
            copy_via_raw_ptr(&src, &mut dst);
        }
"#
            ),
            "copy_via_raw_ptr",
            concat!(
                r#"%1 = OpFunction %2 None %3
                %4 = OpFunctionParameter %5
                %6 = OpFunctionParameter %5
                %7 = OpLabel"#,
                $spirv,
                r#"OpReturn
                OpFunctionEnd"#
            ),
        );
    };
}

#[test]
fn ptr_read() {
    test_copy_via_raw_ptr!(
        "*dst = core::ptr::read(src)"=> r#"
            %8 = OpVariable %5 Function
            OpStore %8 %9
            OpCopyMemory %8 %4
            %10 = OpLoad %11 %8
            OpStore %6 %10
        "#
    );
}

#[test]
fn ptr_read_method() {
    test_copy_via_raw_ptr!(
        "*dst = (src as *const f32).read()" => r#"
            %8 = OpVariable %5 Function
            OpStore %8 %9
            OpCopyMemory %8 %4
            %10 = OpLoad %11 %8
            OpStore %6 %10
        "#
    );
}

#[test]
fn ptr_write() {
    test_copy_via_raw_ptr!(
        "core::ptr::write(dst, *src)" => r#"
            %8 = OpVariable %5 Function
            %9 = OpLoad %10 %4
            OpStore %8 %9
            OpCopyMemory %6 %8
        "#
    );
}

#[test]
fn ptr_write_method() {
    test_copy_via_raw_ptr!(
        "(dst as *mut f32).write(*src)" => r#"
            %8 = OpVariable %5 Function
            %9 = OpLoad %10 %4
            OpStore %8 %9
            OpCopyMemory %6 %8
        "#
    );
}

#[test]
fn ptr_copy() {
    test_copy_via_raw_ptr!(
        "core::ptr::copy(src, dst, 1)" => r#"
            OpCopyMemory %6 %4
        "#
    );
}

#[test]
// FIXME(eddyb) doesn't work because `<*const T>::copy_to` is a method that wraps
// the actual `core::ptr::copy` intrinsic - this requires either MIR inlining, or
// making the methods themselves intrinsic (via attributes instead of pseudo-ABI).
#[ignore]
fn ptr_copy_to_method() {
    test_copy_via_raw_ptr!(
        "(src as *const f32).copy_to(dst, 1)" => r#"
            OpCopyMemory %6 %4
        "#
    );
}

#[test]
// FIXME(eddyb) doesn't work because `<*mut T>::copy_from` is a method that wraps
// the actual `core::ptr::copy` intrinsic - this requires either MIR inlining, or
// making the methods themselves intrinsic (via attributes instead of pseudo-ABI).
#[ignore]
fn ptr_copy_from_method() {
    test_copy_via_raw_ptr!(
        "(dst as *mut f32).copy_from(src, 1)" => r#"
            OpCopyMemory %6 %4
        "#
    );
}

#[test]
fn index_buffer_slice() {
    dis_entry_fn(
        r#"
#[spirv(fragment)]
pub fn main(
    #[spirv(storage_buffer, descriptor_set = 0, binding = 0)] slice: &mut [f32],
) {
    let float: f32 = slice[0];
    let _ = float;
}
        "#,
        "main",
        r#"%1 = OpFunction %2 None %3
%4 = OpLabel
%5 = OpAccessChain %6 %7 %8
%9 = OpArrayLength %10 %7 0
%11 = OpCompositeInsert %12 %5 %13 0
%14 = OpCompositeInsert %12 %9 %11 1
%15 = OpULessThan %16 %8 %9
OpSelectionMerge %17 None
OpBranchConditional %15 %18 %19
%18 = OpLabel
%20 = OpInBoundsAccessChain %21 %5 %8
%22 = OpLoad %23 %20
OpReturn
%19 = OpLabel
OpBranch %24
%24 = OpLabel
OpBranch %25
%25 = OpLabel
%26 = OpPhi %16 %27 %24 %27 %28
OpLoopMerge %29 %28 None
OpBranchConditional %26 %30 %29
%30 = OpLabel
OpBranch %28
%28 = OpLabel
OpBranch %25
%29 = OpLabel
OpUnreachable
%17 = OpLabel
OpUnreachable
OpFunctionEnd"#,
    )
}

#[test]
fn descriptor_indexing() {
    dis_entry_fn(
        r#"
#[spirv(fragment)]
pub fn main(
    #[spirv(storage_buffer, descriptor_set = 0, binding = 0)] rta_rta: &RuntimeDescriptorArray<[usize]>,
    #[spirv(uniform, descriptor_set = 0, binding = 0)] arr_val: &DescriptorArray<usize, 5>,
    #[spirv(descriptor_set = 0, binding = 0)] rta_res: &RuntimeDescriptorArray<Sampler>,
) {
    unsafe {
        asm! {
            "OpCapability RuntimeDescriptorArray",
            "OpExtension \"SPV_EXT_descriptor_indexing\"",
        }
        let index1: usize = rta_rta.index_unchecked(5)[3];
        let index2: usize = *arr_val.index_unchecked(index1);
        let resource: &Sampler = rta_res.index_unchecked(index2);
        let _ = resource;
    }
}
        "#,
        "main",
        r#"%1 = OpFunction %2 None %3
%4 = OpLabel
%5 = OpAccessChain %6 %7 %8
%9 = OpArrayLength %10 %5 0
%11 = OpInBoundsAccessChain %12 %5 %13
%14 = OpCompositeConstruct %15 %11 %9
%16 = OpCompositeExtract %12 %14 0
%17 = OpCompositeExtract %10 %14 1
%18 = OpULessThan %19 %20 %17
OpSelectionMerge %21 None
OpBranchConditional %18 %22 %23
%22 = OpLabel
%24 = OpInBoundsAccessChain %25 %16 %20
%26 = OpLoad %10 %24
%27 = OpAccessChain %28 %29 %26 %13
%30 = OpLoad %10 %27
%31 = OpAccessChain %32 %33 %30
OpReturn
%23 = OpLabel
OpBranch %34
%34 = OpLabel
OpBranch %35
%35 = OpLabel
%36 = OpPhi %19 %37 %34 %37 %38
OpLoopMerge %39 %38 None
OpBranchConditional %36 %40 %39
%40 = OpLabel
OpBranch %38
%38 = OpLabel
OpBranch %35
%39 = OpLabel
OpUnreachable
%21 = OpLabel
OpUnreachable
OpFunctionEnd"#,
    )
}
