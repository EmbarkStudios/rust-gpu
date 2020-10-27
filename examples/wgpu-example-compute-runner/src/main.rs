
async fn create_device_queue() -> (wgpu::Device, wgpu::Queue) {
    let backend = wgpu::BackendBit::all();
    let instance = wgpu::Instance::new(backend);
        
    let adapter = instance
        .request_adapter(&wgpu::RequestAdapterOptions {
            power_preference: wgpu::PowerPreference::default(),
            // Request an adapter which can render to our surface
            compatible_surface: None,
        })
        .await
        .expect("Failed to find an appropriate adapter");
        
    adapter
        .request_device(
            &wgpu::DeviceDescriptor {
                features: wgpu::Features::empty(),
                limits: wgpu::Limits::empty(),
                shader_validation: true,
            },
            None,
        )
        .await
        .unwrap()
}

fn disassemble_spirv(binary: impl AsRef<[u32]>) -> String {
    use rspirv::binary::Disassemble;
    let mut loader = rspirv::dr::Loader::new();
    rspirv::binary::parse_words(binary, &mut loader).unwrap();
    loader.module()
        .disassemble()
}

fn main() {
    let spirv = wgpu::include_spirv!(env!("wgpu_example_compute_shader.spv"));
    println!("spirv: \n{}\n", disassemble_spirv(&spirv));
    
    /*
    let (device, queue) = futures::executor::block_on(create_device_queue());
    let shader = device.create_shader_module(spirv);*/    
}
