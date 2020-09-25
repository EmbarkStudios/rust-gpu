use ocl::enums::ProgramInfo;
use ocl::{Buffer, Context, Kernel, Platform, Program, Queue};
use std::ffi::CString;
use std::fs::File;
use std::io::Read;

fn main() {
    let path = "../build_libcore_test/target/spirv-unknown-unknown/release/build_libcore_test.spv";
    let mut spirv = Vec::new();
    File::open(path).unwrap().read_to_end(&mut spirv).unwrap();

    let platform = Platform::list()
        .into_iter()
        .find(|p| p.version().unwrap().contains("2."))
        .unwrap();
    let context = Context::builder().platform(platform).build().unwrap();
    let device = context.devices()[0];
    println!(
        "Using {} -> {}",
        platform.name().unwrap(),
        device.name().unwrap()
    );
    let queue = Queue::new(&context, device, None).unwrap();
    let program = Program::with_il(
        &spirv,
        Some(&[device]),
        &CString::new("").unwrap(),
        &context,
    )
    .unwrap();
    println!(
        "Kernel names: {:?}",
        program.info(ProgramInfo::KernelNames).unwrap()
    );
    let one = Buffer::<u32>::builder()
        .queue(queue.clone())
        .len(1)
        .build()
        .unwrap();
    let two = Buffer::<u32>::builder()
        .queue(queue.clone())
        .len(1)
        .build()
        .unwrap();
    let three = Buffer::<u32>::builder()
        .queue(queue.clone())
        .len(1)
        .build()
        .unwrap();

    one.write(&[1u32] as &[u32]).enq().unwrap();
    two.write(&[2u32] as &[u32]).enq().unwrap();
    three.write(&[5u32] as &[u32]).enq().unwrap();

    let kernel = Kernel::builder()
        .queue(queue.clone())
        .program(&program)
        .name("add_two_ints.1")
        .local_work_size(&[1])
        .global_work_size(&[1])
        .arg(&one)
        .arg(&two)
        .arg(&three)
        .build()
        .unwrap();

    unsafe {
        kernel.enq().unwrap();
    }

    fn read(buf: Buffer<u32>) {
        let mut vec = vec![0; 1];
        buf.read(&mut vec).enq().unwrap();
        println!("{:?}", vec);
    }

    read(one);
    read(two);
    read(three);
}
