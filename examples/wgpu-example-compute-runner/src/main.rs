
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
                limits: wgpu::Limits::default(),
                shader_validation: true,
            },
            None,
        )
        .await
        .unwrap()
}

macro_rules! include_bytes_align_as {
    ($align_ty:ty, $file:expr) => {{
        #[repr(C)]
        pub struct AlignedAs<Align, Bytes: ?Sized> {
            pub _align: [Align; 0],
            pub bytes: Bytes,
        }

        static ALIGNED: &AlignedAs::<$align_ty, [u8]> = &AlignedAs {
            _align: [],
            bytes: *include_bytes!($file),
        };

        &ALIGNED.bytes
    }};
}

fn disassemble_spirv(binary: impl AsRef<[u32]>) -> String {
    use rspirv::binary::Disassemble;
    let mut loader = rspirv::dr::Loader::new();
    rspirv::binary::parse_words(binary, &mut loader).unwrap();
    loader.module()
        .disassemble()
}

fn main() {
    let spirv = include_bytes_align_as!(u32, env!("wgpu_example_compute_shader.spv"));
    println!("spirv: \n{}\n", disassemble_spirv(bytemuck::cast_slice(spirv)));
    
    let (device, queue) = futures::executor::block_on(create_device_queue());
    let shader = device.create_shader_module(wgpu::util::make_spirv(spirv));    
    
    let bind_group_layout = device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
        label: None,
        entries: &[], /*&[
            wgpu::BindGroupLayoutEntry {
            binding: 0,
            visibility: wgpu::ShaderStage::COMPUTE,
            ty: wgpu::BindingType::StorageBuffer {
                dynamic: false,
                readonly: false,
                min_binding_size: wgpu::BufferSize::new(4),
            },
            count: None,
        }],*/
    });
        
    let pipeline_layout = device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
        label: None,
        bind_group_layouts: &[&bind_group_layout],
        push_constant_ranges: &[],/*&[wgpu::PushConstantRange {
            stages: wgpu::ShaderStage::COMPUTE,
            range: 0..(1 * 4),
        }],*/
    });    
        
    let compute_pipeline = device.create_compute_pipeline(&wgpu::ComputePipelineDescriptor {
        label: None,
        layout: Some(&pipeline_layout),
        compute_stage: wgpu::ProgrammableStageDescriptor {
            module: &shader,
            entry_point: "main_cs",
        },
    });
    
    let bind_group = device.create_bind_group(&wgpu::BindGroupDescriptor {
        label: None,
        layout: &bind_group_layout,
        entries: &[], /*&[
            wgpu::BindGroupEntry {
                binding: 0,
                resource: wgpu::BindingResource::Buffer(x_buffer.slice(..))
            },
            wgpu::BindGroupEntry {
                binding: 1,
                resource: wgpu::BindingResource::Buffer(y_buffer.slice(..))
        }],*/
    });
    
    let mut encoder = device.create_command_encoder(
        &wgpu::CommandEncoderDescriptor { label: None }
    );
    
    {
        let mut cpass = encoder.begin_compute_pass();
        cpass.set_bind_group(0, &bind_group, &[]);
        cpass.set_pipeline(&compute_pipeline);
        //cpass.set_push_constants(0, push_constants);
        cpass.dispatch(1, 1, 1);
    }
    
    //encoder.finish();
    
    queue.submit(Some(encoder.finish()));
    
    println!("Success!");
}
