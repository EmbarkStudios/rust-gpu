# Compiletests

This folder contains tests known as "compiletests". Each file in the `ui` folder corresponds to a
single compiletest. The way they work is a tool iterates over every file, and tries to compile it.
At the start of the file, there's some meta-comments about the expected result of the compile:
whether it should succeed compilation, or fail. If it is expected to fail, there's a corresponding
.stderr file next to the file that contains the expected compiler error message.

The `src` folder here is the tool that iterates over every file in the `ui` folder. It uses the
`compiletests` library, taken from rustc's own compiletest framework.

You can run compiletests via `cargo compiletests`. This is an alias set up in `.cargo/config` for
`cargo run --release -p compiletests --`. You can filter to run specific tests by passing the
(partial) filenames to `cargo compiletests some_file_name`, and update the `.stderr` files to
contain new output via the `--bless` flag (with `--bless`, make sure you're actually supposed to be
changing the .stderr files due to an intentional change, and hand-validate the output is correct
afterwards).

Keep in mind that tests here here are not executed, merely checked for errors (including validating
the resulting binary with spirv-val). Because of this, there might be some strange code in here -
the point isn't to make a fully functional shader every time (that would take an annoying amount of
effort), but rather validate that specific parts of the compiler are doing their job correctly
(either succeeding as they should, or erroring as they should).
