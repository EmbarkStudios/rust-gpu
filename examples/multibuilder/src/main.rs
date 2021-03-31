use spirv_builder::SpirvBuilder;

fn main() {
    let result = SpirvBuilder::new("../shaders/sky-shader")
        .print_metadata(false)
        .spirv_version(1, 0)
        .build_multimodule()
        .unwrap();
    println!("{:#?}", result);
}
