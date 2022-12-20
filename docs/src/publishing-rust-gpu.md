# Publishing rust-gpu on crates.io

This is a task list for the maintainers of rust-gpu to remember to do when publishing a new version
of rust-gpu (probably not useful for contributors without access to embark's crates.io account 😋)

The published crates and their relative locations are:
1. `spirv-std-types` (`crates/spirv-std/shared`)
2. `spirv-std-macros` (`crates/spirv-std/macros`)
3. `spirv-std` (`crates/spirv-std`)
4. `rustc_codegen_spirv-types` (`crates/rustc_codegen_spirv-types`)
5. `rustc_codegen_spirv` (`crates/rustc_codegen_spirv`)
6. `spirv-builder` (`crates/spirv-builder`)

Publishing the crates in above order prevents dependency issues.
These are the steps: 

1. Bump all the versions to the next one in the workspace's `Cargo.toml`. This project uses workspace
   inheritance, so this is the only place you'll find these actual versions. Make sure to pin the
   rust-gpu dependencies to their *exact* versions using the `=`-notation, such as: `=0.4.0`. All crates
   are built and published in tandem so you're not expected to be able to mix and match between versions.
2. Add this new version to the table in `crates/spirv-builder/README.md` and make sure the correct
   nightly version is listed there as well.
3. Create a PR with that change. Wait for CI and a review, and merge it.
4. Pull the merged `main` branch.
5. Tag `main` with the version: `git tag v0.4.0`
6. Push the tag: `git push origin v0.4.0`
7. Publish the crates: `cd [crate] && cargo publish` in the order of the list above (make sure
   `.cargo/credentials` is set to embark's token). The crates.io index might take some seconds to update
   causing an error if the crates are published in quick succession. Wait a couple of seconds and try
   again 🙂.
