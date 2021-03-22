// build-pass

#[spirv(geometry(input_lines = 2, output_points = 2))]
pub fn main() {
    unsafe {
        asm!("OpCapability Geometry");
        spirv_std::arch::end_primitive();
    };
}
