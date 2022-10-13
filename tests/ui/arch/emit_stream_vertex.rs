// build-pass
// compile-flags: -C target-feature=+GeometryStreams

#[rust_gpu::spirv(geometry(input_lines = 2, output_points = 2))]
pub fn main() {
    unsafe {
        spirv_std::arch::emit_stream_vertex::<2>();
    };
}
