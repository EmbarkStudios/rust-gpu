// build-pass

#[spirv(geometry(input_lines = 2, output_points = 2))]
pub fn main() {
    unsafe {
        asm!("OpCapability GeometryStreams");
        spirv_std::arch::emit_stream_vertex::<2>();
    };
}
