use wgpu::util::DeviceExt;

use super::Options;
use futures::future::join;
use std::{convert::TryInto, future::Future, num::NonZeroU64, time::Duration};

fn block_on<T>(future: impl Future<Output = T>) -> T {
    cfg_if::cfg_if! {
        if #[cfg(target_arch = "wasm32")] {
            wasm_bindgen_futures::spawn_local(future)
        } else {
            futures::executor::block_on(future)
        }
    }
}

pub fn start(options: &Options) {
    let shader_binary = crate::maybe_watch(options.shader, None);

    block_on(start_internal(options, shader_binary));
}

pub async fn start_internal(
    _options: &Options,
    shader_binary: wgpu::ShaderModuleDescriptorSpirV<'static>,
) {
    let instance = wgpu::Instance::new(wgpu::Backends::PRIMARY);
    let adapter = instance
        .request_adapter(&wgpu::RequestAdapterOptions {
            power_preference: wgpu::PowerPreference::default(),
            force_fallback_adapter: false,
            compatible_surface: None,
        })
        .await
        .expect("Failed to find an appropriate adapter");

    let (device, queue) = adapter
        .request_device(
            &wgpu::DeviceDescriptor {
                label: None,
                features: wgpu::Features::TIMESTAMP_QUERY
                    | wgpu::Features::SPIRV_SHADER_PASSTHROUGH,
                limits: wgpu::Limits::default(),
            },
            None,
        )
        .await
        .expect("Failed to create device");
    drop(instance);
    drop(adapter);

    let timestamp_period = queue.get_timestamp_period();

    // Load the shaders from disk
    let module = unsafe { device.create_shader_module_spirv(&shader_binary) };

    let top = 2u32.pow(20);
    let src_range = 1..top;

    let src = src_range
        .clone()
        // Not sure which endianness is correct to use here
        .map(u32::to_ne_bytes)
        .flat_map(core::array::IntoIter::new)
        .collect::<Vec<_>>();

    let bind_group_layout = device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
        label: None,
        entries: &[
            // XXX - some graphics cards do not support empty bind layout groups, so
            // create a dummy entry.
            wgpu::BindGroupLayoutEntry {
                binding: 0,
                count: None,
                visibility: wgpu::ShaderStages::COMPUTE,
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

    let readback_buffer = device.create_buffer(&wgpu::BufferDescriptor {
        label: None,
        size: src.len() as wgpu::BufferAddress,
        // Can be read to the CPU, and can be copied from the shader's storage buffer
        usage: wgpu::BufferUsages::MAP_READ | wgpu::BufferUsages::COPY_DST,
        mapped_at_creation: false,
    });

    let storage_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
        label: Some("Collatz Conjecture Input"),
        contents: &src,
        usage: wgpu::BufferUsages::STORAGE
            | wgpu::BufferUsages::COPY_DST
            | wgpu::BufferUsages::COPY_SRC,
    });

    let timestamp_buffer = device.create_buffer(&wgpu::BufferDescriptor {
        label: Some("Timestamps buffer"),
        size: 16,
        usage: wgpu::BufferUsages::MAP_READ | wgpu::BufferUsages::COPY_DST,
        mapped_at_creation: true,
    });
    timestamp_buffer.unmap();

    let bind_group = device.create_bind_group(&wgpu::BindGroupDescriptor {
        label: None,
        layout: &bind_group_layout,
        entries: &[wgpu::BindGroupEntry {
            binding: 0,
            resource: storage_buffer.as_entire_binding(),
        }],
    });

    let queries = device.create_query_set(&wgpu::QuerySetDescriptor {
        label: None,
        count: 2,
        ty: wgpu::QueryType::Timestamp,
    });

    let mut encoder =
        device.create_command_encoder(&wgpu::CommandEncoderDescriptor { label: None });

    {
        let mut cpass = encoder.begin_compute_pass(&wgpu::ComputePassDescriptor { label: None });
        cpass.set_bind_group(0, &bind_group, &[]);
        cpass.set_pipeline(&compute_pipeline);
        cpass.write_timestamp(&queries, 0);
        cpass.dispatch(src_range.len() as u32 / 64, 1, 1);
        cpass.write_timestamp(&queries, 1);
    }

    encoder.copy_buffer_to_buffer(
        &storage_buffer,
        0,
        &readback_buffer,
        0,
        src.len() as wgpu::BufferAddress,
    );
    encoder.resolve_query_set(&queries, 0..2, &timestamp_buffer, 0);

    queue.submit(Some(encoder.finish()));
    let buffer_slice = readback_buffer.slice(..);
    let timestamp_slice = timestamp_buffer.slice(..);
    let timestamp_future = timestamp_slice.map_async(wgpu::MapMode::Read);
    let buffer_future = buffer_slice.map_async(wgpu::MapMode::Read);
    device.poll(wgpu::Maintain::Wait);

    if let (Ok(()), Ok(())) = join(buffer_future, timestamp_future).await {
        let data = buffer_slice.get_mapped_range();
        let timing_data = timestamp_slice.get_mapped_range();
        let result = data
            .chunks_exact(4)
            .map(|b| u32::from_ne_bytes(b.try_into().unwrap()))
            .collect::<Vec<_>>();
        let timings = timing_data
            .chunks_exact(8)
            .map(|b| u64::from_ne_bytes(b.try_into().unwrap()))
            .collect::<Vec<_>>();
        drop(data);
        readback_buffer.unmap();
        drop(timing_data);
        timestamp_buffer.unmap();
        let mut max = 0;
        for (src, out) in src_range.zip(result.iter().copied()) {
            if out == u32::MAX {
                println!("{}: overflowed", src);
                break;
            } else if out > max {
                max = out;
                // Should produce <https://oeis.org/A006877>
                println!("{}: {}", src, out);
            }
        }
        println!(
            "Took: {:?}",
            Duration::from_nanos(
                ((timings[1] - timings[0]) as f64 * f64::from(timestamp_period)) as u64
            )
        );
    }
}
