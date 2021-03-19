use super::{shader_module, Options};
use core::num::NonZeroU64;

fn create_device_queue() -> (wgpu::Device, wgpu::Queue) {
    async fn create_device_queue_async() -> (wgpu::Device, wgpu::Queue) {
        let instance = wgpu::Instance::new(wgpu::BackendBit::PRIMARY);
        let adapter = instance
            .request_adapter(&wgpu::RequestAdapterOptions {
                power_preference: wgpu::PowerPreference::default(),
                compatible_surface: None,
            })
            .await
            .expect("Failed to find an appropriate adapter");

        adapter
            .request_device(
                &wgpu::DeviceDescriptor {
                    label: None,
                    features: wgpu::Features::empty(),
                    limits: wgpu::Limits::default(),
                },
                None,
            )
            .await
            .expect("Failed to create device")
    }
    cfg_if::cfg_if! {
        if #[cfg(target_arch = "wasm32")] {
            wasm_bindgen_futures::spawn_local(create_device_queue_async())
        } else {
            futures::executor::block_on(create_device_queue_async())
        }
    }
}

pub fn start(options: &Options) {
    let (device, queue) = create_device_queue();

    // Load the shaders from disk
    let module = device.create_shader_module(&shader_module(options.shader));

    let bind_group_layout = device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
        label: None,
        entries: &[
            // XXX - some graphics cards do not support empty bind layout groups, so
            // create a dummy entry.
            wgpu::BindGroupLayoutEntry {
                binding: 0,
                count: None,
                visibility: wgpu::ShaderStage::COMPUTE,
                ty: wgpu::BindingType::Buffer {
                    has_dynamic_offset: false,
                    min_binding_size: Some(NonZeroU64::new(1).unwrap()),
                    ty: wgpu::BufferBindingType::Storage { read_only: false },
                },
            },
        ],
    });

    let pipeline_layout = device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
        label: None,
        bind_group_layouts: &[&bind_group_layout],
        push_constant_ranges: &[],
    });

    let compute_pipeline = device.create_compute_pipeline(&wgpu::ComputePipelineDescriptor {
        label: None,
        layout: Some(&pipeline_layout),
        module: &module,
        entry_point: "main_cs",
    });

    let buf = device.create_buffer(&wgpu::BufferDescriptor {
        label: None,
        size: 1,
        usage: wgpu::BufferUsage::STORAGE,
        mapped_at_creation: false,
    });

    let bind_group = device.create_bind_group(&wgpu::BindGroupDescriptor {
        label: None,
        layout: &bind_group_layout,
        entries: &[wgpu::BindGroupEntry {
            binding: 0,
            resource: wgpu::BindingResource::Buffer {
                buffer: &buf,
                offset: 0,
                size: None,
            },
        }],
    });

    let mut encoder =
        device.create_command_encoder(&wgpu::CommandEncoderDescriptor { label: None });

    {
        let mut cpass = encoder.begin_compute_pass(&wgpu::ComputePassDescriptor { label: None });
        cpass.set_bind_group(0, &bind_group, &[]);
        cpass.set_pipeline(&compute_pipeline);
        cpass.dispatch(1, 1, 1);
    }

    queue.submit(Some(encoder.finish()));
}
