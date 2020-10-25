use structopt::StructOpt;

/// Create a SPIR-V binary module from SPIR-V assembly text
#[derive(StructOpt)]
struct Args {
    /// Set the output filename. Use '-' for stdout.
    #[structopt(short, default_value = "out.spv")]
    output: String,
    /// Numeric IDs in the binary will have the same values as in the
    /// source. Non-numeric IDs are allocated by filling in the gaps,
    /// starting with 1 and going up.
    #[structopt(long = "preserve-numeric-ids")]
    preserve_ids: bool,
    /// Use specified environment.
    #[structopt(long = "target-env", parse(try_from_str))]
    target_env: Option<spirv_tools::shared::TargetEnv>,
    /// The input file. Use '-' for stdin.
    #[structopt(name = "FILE")]
    input: String,
}

fn main() {
    use spirv_tools::assembler;

    let args = Args::from_args();

    let contents = if args.input == "-" {
        use std::io::Read;
        let mut v = Vec::with_capacity(1024);
        std::io::stdin()
            .read_to_end(&mut v)
            .expect("failed to read stdin");
        String::from_utf8(v).expect("stdin had invalid utf-8")
    } else {
        std::fs::read_to_string(&args.input).expect("failed to read input file")
    };

    let assembler_opts = assembler::AssemblerOptions {
        preserve_numeric_ids: args.preserve_ids,
    };

    let assembler = assembler::Assembler::new(args.target_env.unwrap_or_default());

    match assembler.assemble(&contents, assembler_opts) {
        Ok(binary) => {
            if args.output == "-" {
                use std::io::Write;
                std::io::stdout()
                    .lock()
                    .write_all(binary.as_ref())
                    .expect("failed to write binary to stdout");
            } else {
                std::fs::write(args.output, &binary).expect("failed to write binary");
            }
        }
        Err(e) => {
            eprintln!("{}", e);
            std::process::exit(1);
        }
    }
}
