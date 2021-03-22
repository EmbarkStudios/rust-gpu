// build-pass

#[spirv(geometry(input_lines = 2, output_points = 2))]
pub fn main() {
    unsafe {
        asm!("OpCapability GeometryStreams");
        spirv_std::arch::end_stream_primitive::<2>();
    };
}
