// BEGIN - Embark standard lints v0.3
// do not change or add/remove here, but one can add exceptions after this section
// for more info see: <https://github.com/EmbarkStudios/rust-ecosystem/issues/59>
#![deny(unsafe_code)]
#![warn(
    clippy::all,
    clippy::await_holding_lock,
    clippy::dbg_macro,
    clippy::debug_assert_with_mut_call,
    clippy::doc_markdown,
    clippy::empty_enum,
    clippy::enum_glob_use,
    clippy::exit,
    clippy::explicit_into_iter_loop,
    clippy::filter_map_next,
    clippy::fn_params_excessive_bools,
    clippy::if_let_mutex,
    clippy::imprecise_flops,
    clippy::inefficient_to_string,
    clippy::large_types_passed_by_value,
    clippy::let_unit_value,
    clippy::linkedlist,
    clippy::lossy_float_literal,
    clippy::macro_use_imports,
    clippy::map_err_ignore,
    clippy::map_flatten,
    clippy::map_unwrap_or,
    clippy::match_on_vec_items,
    clippy::match_same_arms,
    clippy::match_wildcard_for_single_variants,
    clippy::mem_forget,
    clippy::mismatched_target_os,
    clippy::needless_borrow,
    clippy::needless_continue,
    clippy::option_option,
    clippy::pub_enum_variant_names,
    clippy::ref_option_ref,
    clippy::rest_pat_in_fully_bound_structs,
    clippy::string_add_assign,
    clippy::string_add,
    clippy::string_to_string,
    clippy::suboptimal_flops,
    clippy::todo,
    clippy::unimplemented,
    clippy::unnested_or_patterns,
    clippy::unused_self,
    clippy::verbose_file_reads,
    future_incompatible,
    nonstandard_style,
    rust_2018_idioms
)]
// END - Embark standard lints v0.3
// crate-specific exceptions:
#![allow()]

use std::{env, path::PathBuf};

use proc_macro::TokenStream;
use quote::quote;
use spirv_builder::{MemoryModel, SpirvBuilder};
use syn::{
    parse::{Parse, ParseStream},
    Expr, ExprLit, ExprPath, FieldValue, Lit, Member, Path, Token,
};

#[proc_macro]
pub fn compile_spirv(input: TokenStream) -> TokenStream {
    let cargo_target_dir = env::var_os("CARGO_TARGET_DIR").map(PathBuf::from);
    let spirv_target_dir = if let Some(ref cargo_target_dir) = cargo_target_dir {
        if !(cargo_target_dir.ends_with("spirv-builder")) {
            cargo_target_dir.clone().join("spirv-builder")
        } else {
            cargo_target_dir.clone()
        }
    } else {
        env::current_dir()
            .expect("`compile_spirv!` macro uses the current directory to determine it's target directory.")
            .join("target/spirv-builder")
    };
    env::set_var("CARGO_TARGET_DIR", spirv_target_dir);
    let args = syn::parse_macro_input!(input as SpirvBuilderArgs);
    let spv_path = SpirvBuilder::new(args.path.as_str())
        .memory_model(args.memory_model)
        .spirv_version(args.version.0, args.version.1)
        .release(args.release)
        .print_metadata(false)
        .build()
        .unwrap();
    if let Some(cargo_target_dir) = cargo_target_dir {
        env::set_var("CARGO_TARGET_DIR", cargo_target_dir);
    } else {
        env::remove_var("CARGO_TARGET_DIR");
    };
    let spv_path = spv_path.to_str().unwrap();
    let out = quote! {
        #spv_path
    };
    TokenStream::from(out)
}

struct SpirvBuilderArgs {
    path: String,
    memory_model: MemoryModel,
    version: (u8, u8),
    release: bool,
}

impl Parse for SpirvBuilderArgs {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let mut path = None;
        let mut memory_model = None;
        let mut version = None;
        let mut release = None;
        for field in input.parse_terminated::<FieldValue, Token![,]>(FieldValue::parse)? {
            let arg = if let Member::Named(arg) = field.member {
                arg
            } else {
                return Err(input.error("Unnamed member."));
            };
            if arg == "path" {
                if path.is_some() {
                    return Err(input
                        .error("Duplicate `path` specification, please specify it exactly once."));
                }
                if field.colon_token.is_none() {
                    return Err(input.error(
                        "Please provide the crate path as a string literal: `path: \"path/to/crate\"`."
                    ));
                }
                if let Expr::Lit(ExprLit {
                    lit: Lit::Str(string),
                    ..
                }) = field.expr
                {
                    path = Some(string.value());
                } else {
                    return Err(input.error(
                        "Please provide the crate path as a string literal: `path: \"path/to/crate\"`."
                    ));
                }
            } else if arg == "memory_model" {
                if memory_model.is_some() {
                    return Err(input.error(
                        "Duplicate `memory_model` specification, please specify it at most once. \
                        The default is `memory_model: Vulkan`.",
                    ));
                }
                if field.colon_token.is_none() {
                    return Err(input.error(
                        "Memory model must be specified as `memory_model: MemoryModel` eg. `memory_model: GLSL450`."
                    ));
                }
                match field.expr {
                    Expr::Path(ExprPath {
                        path: Path {
                            segments,
                            ..
                        },
                        ..
                    }) if segments.len() == 1 => {
                        let ident = &segments.first().unwrap().ident;
                        if ident == "Vulkan" {
                            memory_model = Some(MemoryModel::Vulkan);
                        } else if ident == "GLSL450" {
                            memory_model = Some(MemoryModel::GLSL450);
                        } else if ident == "Simple" {
                            memory_model = Some(MemoryModel::Simple);
                        } else {
                            return Err(input.error(
                                "Invalid memory model, please specify one of `Vulkan`, `GLSL450`, or `Simple`."
                            ))
                        }
                    }
                    _ => return Err(input.error(
                        "Memory model must be specified as `memory_model: MemoryModel` eg. `memory_model: GLSL450`."
                    )),
                }
            } else if arg == "version" {
                if version.is_some() {
                    return Err(input.error(
                        "Duplicate `version` specification, please specify it at most once. The default is `version: 1.3`."
                    ));
                }
                if field.colon_token.is_none() {
                    return Err(input.error(
                        "Version must be specified as `version: maj.min`. The default is `version: 1.3`."
                    ));
                }
                if let Expr::Lit(ExprLit {
                    lit: Lit::Float(float),
                    ..
                }) = field.expr
                {
                    let mut radix_split = float.base10_digits().split('.');
                    let (major, minor) = (radix_split.next(), radix_split.next());
                    let major = if major == Some("1") {
                        1
                    } else {
                        return Err(input.error("Major version must be 1."));
                    };
                    let minor = match minor {
                        Some("0") => 0,
                        Some("1") => 1,
                        Some("2") => 2,
                        Some("3") => 3,
                        Some("4") => 4,
                        Some("5") => 5,
                        _ => return Err(input.error("Minor version must be in [0, 5], inclusive.")),
                    };
                    version = Some((major, minor));
                } else {
                    return Err(input.error(
                        "Version must be specified as `version: maj.min`, the default is `version: 1.3`."
                    ));
                }
            } else if arg == "release" || arg == "debug" {
                if release.is_none() {
                    release = Some(arg == "release");
                } else {
                    return Err(input.error(
                        "Duplicate compile mode specification, please specify `release` or `debug` at most once. \
                        If unspecified, the default is `release`."
                    ));
                }
            } else {
                return Err(input.error(format!(
                    "Unknown parameter: `{:?}`, valid parameters are `path`, `memory_model`, \
                    `release` and `debug`.",
                    arg,
                )));
            }
        }
        if path.is_none() {
            return Err(input.error("Please provide a crate path: `path: \"path/to/crate\"`."));
        }
        Ok(Self {
            path: path.unwrap(),
            memory_model: memory_model.unwrap_or(MemoryModel::Vulkan),
            version: version.unwrap_or((1, 3)),
            release: release.unwrap_or(true),
        })
    }
}
