use cargo_metadata::MetadataCommand;
use std::path::PathBuf;
use std::{env, fs};

fn main() {
    if cfg!(feature = "internal-unstable-trigger-self-build-for-backend") {
        // Nothing to do, Cargo will have built `rustc_codegen_spirv` as a dependency.
        return;
    }

    let propagate_features: Vec<_> = [
        (cfg!(feature = "use-installed-tools"), "use-installed-tools"),
        (cfg!(feature = "use-compiled-tools"), "use-compiled-tools"),
    ]
    .into_iter()
    .filter_map(|(cond, feat)| cond.then_some(feat))
    .collect();

    // HACK(eddyb) allow `docs.rs` to build `spirv-builder` by making building
    // `rustc_codegen_spirv` optional in a way that will always result in it being
    // built normally (as `use-{installed,compiled}-tools` both require it), and
    // produces a compile-time error if it's missing and `cfg(doc)` isn't set.
    if propagate_features.is_empty() {
        return;
    }

    // Clear Cargo environment variables that we don't want to leak into the
    // inner invocations of Cargo (because e.g. build scripts might read them).
    for (key, _) in env::vars_os() {
        let remove = key.to_str().map_or(false, |s| {
            s.starts_with("CARGO_FEATURES_") || s.starts_with("CARGO_CFG_")
        });
        if remove {
            env::remove_var(key);
        }
    }

    let profile = env::var("PROFILE").unwrap();
    let out_dir = PathBuf::from(env::var_os("OUT_DIR").unwrap());

    // Try to obtain the outer target dir, to base the target dir we'll build
    // `rustc_codegen_spirv` in, off of it (but we can use `$OUT_DIR` as fallback).
    let target_dir_base = {
        // Strip `$outer_profile/build/*/out`.
        let outer_target_dir = [&profile, "build", "*", "out"].iter().rev().try_fold(
            out_dir.clone(),
            |mut dir, &filter| {
                if (filter == "*" || dir.ends_with(filter)) && dir.pop() {
                    Some(dir)
                } else {
                    None
                }
            },
        );
        match outer_target_dir {
            Some(target) => target.join("rustc_codegen_spirv-build"),
            None => out_dir.join("build"),
        }
    };

    // Set up a workspace in `$OUT_DIR` to bootstrap building this very crate
    // (`spirv-builder`) with `rust-toolchain.toml` and `Cargo.lock` taken from
    // our `rustc_codegen_spirv` dependency.
    let ws = out_dir.join("nested-self-builder");
    fs::create_dir_all(ws.join("src")).unwrap();
    fs::write(
        ws.join("Cargo.toml"),
        format!(
            r#"
[workspace]

[package]
name = "nested-self-builder"
edition = "2021"
version = "0.0.0"

[dependencies.spirv-builder]
path = {spirv_builder_path:?}
default-features = false
features = [{spirv_builder_features}]

# Enable incremental by default in release mode, as well.
[profile.release]
incremental = true
"#,
            spirv_builder_path = env!("CARGO_MANIFEST_DIR"),
            spirv_builder_features = propagate_features
                .iter()
                .copied()
                .chain(["internal-unstable-trigger-self-build-for-backend"])
                .map(|feat| format!("{feat:?}"))
                .collect::<Vec<_>>()
                .join(", "),
        ),
    )
    .unwrap();
    const NESTED_SELF_BUILDER_MAIN_RS: &str = r#"
fn main() {
    let dylib_path = spirv_builder::codegen_backend::codegen_backend_dylib_path();
    println!("cargo:rerun-if-changed={}", dylib_path.display());
    println!("cargo:rustc-env=SPIRV_BUILDER_CODEGEN_BACKEND_DYLIB_PATH={}", dylib_path.display());
    println!("cargo:rustc-env=SPIRV_BUILDER_RUSTUP_TOOLCHAIN={}", env!("RUSTUP_TOOLCHAIN"));
}
"#;
    let ws_src_main_rs = ws.join("src/main.rs");
    // HACK(eddyb) avoid overwriting the file if it would unchanged, to not update its `mtime`.
    if fs::read_to_string(&ws_src_main_rs).as_deref().ok() != Some(NESTED_SELF_BUILDER_MAIN_RS) {
        fs::write(ws_src_main_rs, NESTED_SELF_BUILDER_MAIN_RS).unwrap();
    }

    // FIXME(eddyb) consider keeping a duplicate of the `rust-toolchain.toml`,
    // to avoid needing
    let find_uniq_dep = |name| {
        let mut packages_by_name = MetadataCommand::new()
            .manifest_path(ws.join("Cargo.toml"))
            .exec()
            .unwrap()
            .packages
            .into_iter()
            .filter(|p| p.name == name);
        match (packages_by_name.next(), packages_by_name.next()) {
            (Some(p), None) => p,
            (None, _) => unreachable!("missing `{name}` in `cargo metadata` output"),
            (Some(_), Some(_)) => unreachable!("more than one `{name}` in `cargo metadata` output"),
        }
    };
    let rustc_codegen_spirv = find_uniq_dep("rustc_codegen_spirv");

    // FIXME(eddyb) can this be more fine-grained?
    println!(
        "cargo:rerun-if-changed={}",
        rustc_codegen_spirv.manifest_path.parent().unwrap()
    );

    let rust_toolchain_toml = fs::read_to_string(
        rustc_codegen_spirv
            .manifest_path
            .with_file_name("rust-toolchain.toml"),
    )
    .unwrap();

    let toolchain_channel = rust_toolchain_toml
        .lines()
        .find_map(|line| line.strip_prefix("channel = \"")?.strip_suffix('"'))
        .unwrap_or_else(|| {
            unreachable!("channel not found in:\n{rust_toolchain_toml}");
        });

    fs::write(ws.join("rust-toolchain.toml"), &rust_toolchain_toml).unwrap();
    fs::copy(
        rustc_codegen_spirv
            .manifest_path
            .with_file_name("Cargo.lock"),
        ws.join("Cargo.lock"),
    )
    .unwrap();

    // HACK(eddyb) this may be necessary to trigger the installation of the
    // toolchain, because `rustup run` itself doesn't actually do that.
    std::process::Command::new("rustup")
        .args(["which", "cargo"])
        .current_dir(&ws)
        .output()
        .unwrap();

    // Clear Cargo environment variables that we don't want to leak into the
    // inner invocation of Cargo (because e.g. build scripts might read them).
    for (key, _) in env::vars_os() {
        let remove = key.to_str().map_or(false, |s| {
            [
                // HACK(eddyb) these env vars can leak the wrong toolchain.
                "CARGO",
                "RUSTC",
                "RUSTC_WORKSPACE_WRAPPER",
                "RUSTC_WRAPPER",
                "CARGO_RUSTC_WRAPPER",
                // Misc variables set by Cargo (w/o a shared prefix like below).
                "CARGO_ENCODED_RUSTFLAGS",
                "PROFILE",
            ]
            .contains(&s)
                || s.starts_with("CARGO_FEATURES_")
                || s.starts_with("CARGO_CFG_")
        });
        if remove {
            std::env::remove_var(key);
        }
    }

    // All commands after this point will take into account `rust-toolchain.toml`,
    // when executing with `ws` as their `current_dir`.
    std::env::remove_var("RUSTUP_TOOLCHAIN");

    // HACK(eddyb) this may be a bit more expensive, but we can't risk it changing
    // just because of the files we just copied over, from the initial "guess".
    assert_eq!(
        rustc_codegen_spirv.manifest_path,
        find_uniq_dep("rustc_codegen_spirv").manifest_path
    );

    // Run the nested build (the helper will print `cargo:rustc-env=...` itself).
    let target_dir = target_dir_base.join(
        // HACK(eddyb) using the features as a suffix will lead to duplicate
        // builds between the different configurations, but at least they won't
        // trample over eachother (sadly Cargo assumes dylibs cannot have suffixes,
        // which would've  otherwise taken care of the conflict problem).
        format!("{toolchain_channel}-{}", propagate_features.join(",")),
    );
    // HACK(eddyb) `rustup run` instead of `cargo` to work around OS-specific
    // `rustup` behavior (https://github.com/rust-lang/rustup/issues/3036).
    let status = std::process::Command::new("rustup")
        .args(["run", toolchain_channel, "cargo", "run", "--release"])
        .arg("--target-dir")
        .arg(&target_dir)
        .current_dir(ws)
        .status()
        .unwrap();
    // FIXME(eddyb) propagate this more directly.
    if !status.success() {
        std::process::exit(1);
    }

    // HACK(eddyb) Cargo already does all the work of coalescing most (all?) of
    // the dependencies into one `.d` file, we can just reuse it.
    let nested_self_builder_depfile =
        fs::read_to_string(target_dir.join("release/nested-self-builder.d")).unwrap();

    for dep in nested_self_builder_depfile.split_ascii_whitespace() {
        if !dep.ends_with(':') {
            println!("cargo:rerun-if-changed={dep}");
        }
    }
}
