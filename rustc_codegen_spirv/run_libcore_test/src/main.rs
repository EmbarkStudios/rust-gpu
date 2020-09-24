use ocl::{Buffer, Context, Device, Kernel, Platform, Program, Queue};
use std::ffi::CString;
use std::fs::File;
use std::io::Read;

fn main() {
    let path = "../build_libcore_test/target/spirv-unknown-unknown/release/build_libcore_test.spv";
    let mut spirv = Vec::new();
    File::open(path).unwrap().read_to_end(&mut spirv).unwrap();

    let dims = [1];
    let platform = Platform::list()
        .into_iter()
        .find(|p| {
            for device in Device::list_all(p).unwrap() {
                println!("{:?} = {}", device, device.name().unwrap());
            }
            let version = p.version().unwrap();
            println!("{:?} = {}", p, version);
            version.contains("2.")
        })
        .unwrap();
    let context = Context::builder().platform(platform).build().unwrap();
    let device = context.devices()[0];
    let queue = Queue::new(&context, device, None).unwrap();
    let program = Program::with_il(
        &spirv,
        Some(&[device]),
        &CString::new("").unwrap(),
        &context,
    )
    .unwrap();
    let one = Buffer::<u32>::builder()
        .queue(queue.clone())
        .copy_host_slice(&[1])
        .build()
        .unwrap();
    let two = Buffer::<u32>::builder()
        .queue(queue.clone())
        .copy_host_slice(&[2])
        .build()
        .unwrap();
    let three = Buffer::<u32>::builder()
        .queue(queue.clone())
        .len(dims)
        .build()
        .unwrap();
    let kernel = Kernel::builder()
        .program(&program)
        .name("add_two_numbers")
        .global_work_size(&dims)
        .arg(&one)
        .arg(&two)
        .arg(&three)
        .build()
        .unwrap();

    unsafe {
        kernel.enq().unwrap();
    }

    let mut vec = vec![0u32; three.len()];
    three.read(&mut vec).enq().unwrap();

    println!("The value at index [{}] is now '{}'!", 0, vec[0]);
}
