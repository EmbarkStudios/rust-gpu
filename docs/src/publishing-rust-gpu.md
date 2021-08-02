# Publishing rust-gpu on crates.io

This is a task list for the maintainers of rust-gpu to remember to do when publishing a new version
of rust-gpu (probably not useful for contributors without access to embark's crates.io account :P)

1. Bump all the versions in rust-gpu to the next one. I've found this command to be useful:
`rg --files-with-matches alpha | xargs sed -i 's/0.4.0-alpha.10/0.4.0-alpha.11/g'` (replacing with
whatever versions are relevant)
2. Create a PR with that change. Wait for CI and a review, and merge it.
3. Pull the merged `main` branch.
4. Tag `main` with the version: `git tag v0.4.0-alpha.11`
5. Push the tag: `git push origin v0.4.0-alpha.11`
6. Publish the crates: `cd [crate] && cargo publish` (make sure `.cargo/credentials` is set to
embark's token) - crates to be published, in order:
   1. crates/spirv-std/shared
   2. crates/spirv-std/macros
   3. crates/spirv-std
