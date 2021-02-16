use super::{dis_fn, dis_globals, val, val_vulkan};
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
fn hello_world() {
    val(r#"
#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main() {
}
"#);
}

#[test]
// blocked on: https://github.com/EmbarkStudios/rust-gpu/issues/69
#[ignore]
fn no_dce() {
    let _var = SetEnvVar::new(&"NO_DCE", "1");
    val(r#"
#[allow(unused_attributes)]
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
#[allow(unused_attributes)]
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
            "%semantics = OpConstant %int 8452",
            "OpMemoryBarrier %scope %semantics",
        );
    }
}
#[allow(unused_attributes)]
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
#[allow(unused_attributes)]
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
                        "%ptr_image_array       = OpTypePointer UniformConstant %image_array",
                        "%image_2d_var          = OpVariable %ptr_image_array UniformConstant",
                        "%ptr_sampled_image_2d  = OpTypePointer UniformConstant %sampled_image_2d",
                        "", // ^^ type preamble
                        "%offset                = OpLoad _ {0}",
                        "%24                    = OpAccessChain %ptr_sampled_image_2d %image_2d_var %offset",
                        "%25                    = OpLoad %sampled_image_2d %24",
                        in(reg) &offset,
                    );
            }
        }
        #[allow(unused_attributes)]
        #[spirv(fragment)]
        pub fn main() {
            add_decorate();
        }"#,
        r#"OpCapability Shader
OpCapability VulkanMemoryModel
OpCapability VariablePointers
OpCapability RuntimeDescriptorArray
OpExtension "SPV_KHR_vulkan_memory_model"
OpExtension "SPV_EXT_descriptor_indexing"
OpMemoryModel Logical Vulkan
OpEntryPoint Fragment %1 "main"
OpExecutionMode %1 OriginUpperLeft
OpName %2 "test_project::add_decorate"
OpName %3 "test_project::main"
OpDecorate %4 DescriptorSet 0
OpDecorate %4 Binding 0
%5 = OpTypeVoid
%6 = OpTypeFunction %5
%7 = OpTypeInt 32 0
%8 = OpTypePointer Function %7
%9 = OpConstant %7 1
%10 = OpTypeFloat 32
%11 = OpTypeImage %10 2D 0 0 0 1 Unknown
%12 = OpTypeSampledImage %11
%13 = OpTypeRuntimeArray %12
%14 = OpTypePointer UniformConstant %13
%4 = OpVariable %14 UniformConstant
%15 = OpTypePointer UniformConstant %12"#,
    );
}

#[test]
fn logical_and() {
    val(r#"
fn f(x: bool, y: bool) -> bool {
    x && y
}
#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main() {
    f(false, true);
}"#);
}

#[test]
fn panic() {
    val(r#"
#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main() {
    panic!("aaa");
}
"#);
}

#[test]
fn panic_builtin() {
    val(r#"
fn int_div(x: usize) -> usize {
    1 / x
}

#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main() {
    int_div(0);
}
"#);
}

#[test]
fn panic_builtin_bounds_check() {
    val(r#"
fn array_bounds_check(x: [u32; 4], i: usize) -> u32 {
    x[i]
}

#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main() {
    array_bounds_check([0, 1, 2, 3], 5);
}
"#);
}

// NOTE(eddyb) this won't pass Vulkan validation (see `push_constant_vulkan`),
// but should still pass the basline SPIR-V validation.
#[test]
fn push_constant() {
    val(r#"
#[derive(Copy, Clone)]
pub struct ShaderConstants {
    pub width: u32,
    pub height: u32,
    pub time: f32,
}

#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main(constants: PushConstant<ShaderConstants>) {
    let _constants = constants.load();
}
"#);
}

// NOTE(eddyb) we specifically run Vulkan validation here, as the default
// validation rules are more lax and don't require a `Block` decoration
// (`#[spirv(block)]` here) on `struct ShaderConstants`.
#[test]
fn push_constant_vulkan() {
    val_vulkan(
        r#"
#[derive(Copy, Clone)]
#[allow(unused_attributes)]
#[spirv(block)]
pub struct ShaderConstants {
    pub width: u32,
    pub height: u32,
    pub time: f32,
}

#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main(constants: PushConstant<ShaderConstants>) {
    let _constants = constants.load();
}
"#,
    );
}

#[test]
fn infinite_loop() {
    val(r#"
#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main() {
    loop {}
}"#);
}

#[test]
fn unroll_loops() {
    dis_fn(
        // FIXME(eddyb) use `for _ in 0..10` here when that works.
        r#"
#[allow(unused_attributes)]
#[spirv(unroll_loops)]
fn java_hash_ten_times(mut x: u32, y: u32) -> u32 {
    let mut i = 0;
    while i < 10 {
        x = 31 * x + y;
        i += 1;
    }
    x
}
#[allow(unused_attributes)]
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
%29 = OpIAdd %2 %27 %5
%30 = OpIAdd %10 %9 %31
OpBranch %24
%26 = OpLabel
OpReturnValue %14
%24 = OpLabel
%12 = OpPhi %10 %30 %25
%15 = OpPhi %2 %29 %25
%19 = OpPhi %17 %32 %25
OpBranch %13
%13 = OpLabel
OpBranch %8
%20 = OpLabel
OpUnreachable
OpFunctionEnd"#,
    );
}

#[test]
fn signum() {
    val(r#"
#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main(i: Input<f32>, mut o: Output<f32>) {
    o.store(i.load().signum());
}"#);
}

#[test]
// Doesn't work, only worked before because I think it got optimized away before hitting the
// backend.
#[ignore]
fn allocate_const_scalar_pointer() {
    val(r#"
use core::ptr::Unique;
const POINTER: Unique<[u8;4]> = Unique::<[u8; 4]>::dangling();

#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main() {
    let _pointer = POINTER;
}"#);
}

#[test]
fn allocate_vec_like_pointer() {
    val(r#"
use core::ptr::Unique;
const VEC_LIKE: (Unique<usize>, usize, usize) = (Unique::<usize>::dangling(), 0, 0);

pub fn assign_vec_like() {
    let _vec_like = VEC_LIKE;
}
#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main() {}"#);
}

#[test]
fn allocate_null_pointer() {
    val(r#"
use core::ptr::null;
const NULL_PTR: *const i32 = null();

#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main() {
    let _null_ptr = NULL_PTR;
}"#);
}

#[test]
fn create_uninitialized_memory() {
    val(r#"
use core::mem::MaybeUninit;
const MAYBEI32: MaybeUninit<&i32> = MaybeUninit::<&i32>::uninit();

pub fn create_uninit_and_write() {
    let mut maybei32 = MAYBEI32;
    unsafe { maybei32.as_mut_ptr().write(&0); }
    let _maybei32 = unsafe { maybei32.assume_init() };
}

#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main() {}"#);
}

#[test]
fn vector_extract_dynamic() {
    val(r#"
#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main() {
    let vector = glam::Vec2::new(1.0, 2.0);
    let element = unsafe { spirv_std::arch::vector_extract_dynamic(vector, 1) };
    assert!(2.0 == element);
}
"#);
}

#[test]
fn mat3_vec3_multiply() {
    val(r#"
#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main(input: Input<glam::Mat3>, mut output: Output<glam::Vec3>) {
    let input = input.load();
    let vector = input * glam::Vec3::new(1.0, 2.0, 3.0);
    output.store(vector);
}
"#);
}
