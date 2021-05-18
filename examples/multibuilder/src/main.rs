use spirv_builder::SpirvBuilder;

fn main() {
    let result = SpirvBuilder::new("../shaders/sky-shader", "spirv-unknown-spv1.3")
        .print_metadata(false)
        .multimodule(true)
        .build()
        .unwrap();
    println!("{:#?}", result);
}
