use rspirv_linker::*;
use rspirv::binary::Disassemble;

fn main() -> Result<()> {
    let body1 = include_bytes!("../test/1/body_1.spv");
    let body2 = include_bytes!("../test/1/body_2.spv");

    let mut body1 = crate::load(&body1[..]);
    let mut body2 = crate::load(&body2[..]);

    let opts = Options {
        lib: false,
        partial: false,
    };

    let output = link(&mut [&mut body1, &mut body2], &opts)?;
    println!("{}\n\n", output.disassemble());

    Ok(())
}
