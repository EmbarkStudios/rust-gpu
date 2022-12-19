// BEGIN - Embark standard lints v0.4
// do not change or add/remove here, but one can add exceptions after this section
// for more info see: <https://github.com/EmbarkStudios/rust-ecosystem/issues/59>
#![deny(unsafe_code)]
#![warn(
    clippy::all,
    clippy::await_holding_lock,
    clippy::char_lit_as_u8,
    clippy::checked_conversions,
    clippy::dbg_macro,
    clippy::debug_assert_with_mut_call,
    clippy::doc_markdown,
    clippy::empty_enum,
    clippy::enum_glob_use,
    clippy::exit,
    clippy::expl_impl_clone_on_copy,
    clippy::explicit_deref_methods,
    clippy::explicit_into_iter_loop,
    clippy::fallible_impl_from,
    clippy::filter_map_next,
    clippy::float_cmp_const,
    clippy::fn_params_excessive_bools,
    clippy::if_let_mutex,
    clippy::implicit_clone,
    clippy::imprecise_flops,
    clippy::inefficient_to_string,
    clippy::invalid_upcast_comparisons,
    clippy::large_types_passed_by_value,
    clippy::let_unit_value,
    clippy::linkedlist,
    clippy::lossy_float_literal,
    clippy::macro_use_imports,
    clippy::manual_ok_or,
    clippy::map_err_ignore,
    clippy::map_flatten,
    clippy::map_unwrap_or,
    clippy::match_on_vec_items,
    clippy::match_same_arms,
    clippy::match_wildcard_for_single_variants,
    clippy::mem_forget,
    clippy::mismatched_target_os,
    clippy::mut_mut,
    clippy::mutex_integer,
    clippy::needless_borrow,
    clippy::needless_continue,
    clippy::option_option,
    clippy::path_buf_push_overwrite,
    clippy::ptr_as_ptr,
    clippy::ref_option_ref,
    clippy::rest_pat_in_fully_bound_structs,
    clippy::same_functions_in_if_condition,
    clippy::semicolon_if_nothing_returned,
    clippy::string_add_assign,
    clippy::string_add,
    clippy::string_lit_as_bytes,
    clippy::string_to_string,
    clippy::todo,
    clippy::trait_duplication_in_bounds,
    clippy::unimplemented,
    clippy::unnested_or_patterns,
    clippy::unused_self,
    clippy::useless_transmute,
    clippy::verbose_file_reads,
    clippy::zero_sized_map_values,
    future_incompatible,
    nonstandard_style,
    rust_2018_idioms
)]
// END - Embark standard lints v0.4
// crate-specific exceptions:
// #![allow()]
#![doc = include_str!("../README.md")]

mod image;

use proc_macro::TokenStream;
use proc_macro2::{Delimiter, Group, Ident, Span, TokenTree};

use syn::{punctuated::Punctuated, spanned::Spanned, ItemFn, Token};

use quote::{quote, ToTokens};
use std::fmt::Write;

/// A macro for creating SPIR-V `OpTypeImage` types. Always produces a
/// `spirv_std::image::Image<...>` type.
///
/// The grammar for the macro is as follows:
///
/// ```rust,ignore
/// Image!(
///     <dimensionality>,
///     <type=...|format=...>,
///     [sampled[=<true|false>],]
///     [multisampled[=<true|false>],]
///     [arrayed[=<true|false>],]
///     [depth[=<true|false>],]
/// )
/// ```
///
/// `=true` can be omitted as shorthand - e.g. `sampled` is short for `sampled=true`.
///
/// A basic example looks like this:
/// ```rust,ignore
/// #[spirv(vertex)]
/// fn main(#[spirv(descriptor_set = 0, binding = 0)] image: &Image!(2D, type=f32, sampled)) {}
/// ```
///
/// ## Arguments
///
/// - `dimensionality` — Dimensionality of an image.
///    Accepted values: `1D`, `2D`, `3D`, `rect`, `cube`, `subpass`.
/// - `type` — The sampled type of an image, mutually exclusive with `format`,
///    when set the image format is unknown.
///    Accepted values: `f32`, `f64`, `u8`, `u16`, `u32`, `u64`, `i8`, `i16`, `i32`, `i64`.
/// - `format` — The image format of the image, mutually exclusive with `type`.
///    Accepted values: Snake case versions of [`ImageFormat`].
/// - `sampled` — Whether it is known that the image will be used with a sampler.
///    Accepted values: `true` or `false`. Default: `unknown`.
/// - `multisampled` — Whether the image contains multisampled content.
///    Accepted values: `true` or `false`. Default: `false`.
/// - `arrayed` — Whether the image contains arrayed content.
///    Accepted values: `true` or `false`. Default: `false`.
/// - `depth` — Whether it is known that the image is a depth image.
///    Accepted values: `true` or `false`. Default: `unknown`.
///
/// [`ImageFormat`]: spirv_std_types::image_params::ImageFormat
///
/// Keep in mind that `sampled` here is a different concept than the `SampledImage` type:
/// `sampled=true` means that this image requires a sampler to be able to access, while the
/// `SampledImage` type bundles that sampler together with the image into a single type (e.g.
/// `sampler2D` in GLSL, vs. `texture2D`).
#[proc_macro]
// The `Image` is supposed to be used in the type position, which
// uses `PascalCase`.
#[allow(nonstandard_style)]
pub fn Image(item: TokenStream) -> TokenStream {
    let output = syn::parse_macro_input!(item as image::ImageType).into_token_stream();

    output.into()
}

/// Replaces all (nested) occurrences of the `#[spirv(..)]` attribute with
/// `#[cfg_attr(target_arch="spirv", rust_gpu::spirv(..))]`.
#[proc_macro_attribute]
pub fn spirv(attr: TokenStream, item: TokenStream) -> TokenStream {
    let mut tokens: Vec<TokenTree> = Vec::new();

    // prepend with #[rust_gpu::spirv(..)]
    let attr: proc_macro2::TokenStream = attr.into();
    tokens.extend(quote! { #[cfg_attr(target_arch="spirv", rust_gpu::spirv(#attr))] });

    let item: proc_macro2::TokenStream = item.into();
    for tt in item {
        match tt {
            TokenTree::Group(group) if group.delimiter() == Delimiter::Parenthesis => {
                let mut sub_tokens = Vec::new();
                for tt in group.stream() {
                    match tt {
                        TokenTree::Group(group)
                            if group.delimiter() == Delimiter::Bracket
                                && matches!(group.stream().into_iter().next(), Some(TokenTree::Ident(ident)) if ident == "spirv")
                                && matches!(sub_tokens.last(), Some(TokenTree::Punct(p)) if p.as_char() == '#') =>
                        {
                            // group matches [spirv ...]
                            let inner = group.stream(); // group stream doesn't include the brackets
                            sub_tokens.extend(
                                quote! { [cfg_attr(target_arch="spirv", rust_gpu::#inner)] },
                            );
                        }
                        _ => sub_tokens.push(tt),
                    }
                }
                tokens.push(TokenTree::from(Group::new(
                    Delimiter::Parenthesis,
                    sub_tokens.into_iter().collect(),
                )));
            }
            _ => tokens.push(tt),
        }
    }
    tokens
        .into_iter()
        .collect::<proc_macro2::TokenStream>()
        .into()
}

/// Marks a function as runnable only on the GPU, and will panic on
/// CPU platforms.
#[proc_macro_attribute]
pub fn gpu_only(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let syn::ItemFn {
        attrs,
        vis,
        sig,
        block,
    } = syn::parse_macro_input!(item as syn::ItemFn);

    let fn_name = sig.ident.clone();

    let sig_cpu = syn::Signature {
        abi: None,
        ..sig.clone()
    };

    let output = quote::quote! {
        // Don't warn on unused arguments on the CPU side.
        #[cfg(not(target_arch="spirv"))]
        #[allow(unused_variables)]
        #(#attrs)* #vis #sig_cpu {
            unimplemented!(concat!("`", stringify!(#fn_name), "` is only available on SPIR-V platforms."))
        }

        #[cfg(target_arch="spirv")]
        #(#attrs)* #vis #sig {
            #block
        }
    };

    output.into()
}

/// Accepts a function with an argument named `component`, and outputs the
/// function plus a vectorized version of the function which accepts a vector
/// of `component`. This is mostly useful when you have the same impl body for
/// a scalar and vector versions of the same operation.
#[proc_macro_attribute]
#[doc(hidden)]
pub fn vectorized(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let function = syn::parse_macro_input!(item as syn::ItemFn);
    let vectored_function = match create_vectored_fn(function.clone()) {
        Ok(val) => val,
        Err(err) => return err.to_compile_error().into(),
    };

    let output = quote::quote!(
        #function

        #vectored_function
    );

    output.into()
}

fn create_vectored_fn(
    ItemFn {
        attrs,
        vis,
        mut sig,
        block,
    }: ItemFn,
) -> Result<ItemFn, syn::Error> {
    const COMPONENT_ARG_NAME: &str = "component";
    let trait_bound_name = Ident::new("VECTOR", Span::mixed_site());
    let const_bound_name = Ident::new("LENGTH", Span::mixed_site());

    sig.ident = Ident::new(&format!("{}_vector", sig.ident), Span::mixed_site());
    sig.output = syn::ReturnType::Type(
        Default::default(),
        Box::new(path_from_ident(trait_bound_name.clone())),
    );

    let component_type = sig.inputs.iter_mut().find_map(|x| match x {
        syn::FnArg::Typed(ty) => match &*ty.pat {
            syn::Pat::Ident(pat) if pat.ident == COMPONENT_ARG_NAME => Some(&mut ty.ty),
            _ => None,
        },
        syn::FnArg::Receiver(_) => None,
    });

    if component_type.is_none() {
        return Err(syn::Error::new(
            sig.inputs.span(),
            "#[vectorized] requires an argument named `component`.",
        ));
    }
    let component_type = component_type.unwrap();

    let vector_path = {
        let mut path = syn::Path {
            leading_colon: None,
            segments: Punctuated::new(),
        };

        for segment in &["crate", "vector"] {
            path.segments
                .push(Ident::new(segment, Span::mixed_site()).into());
        }

        path.segments.push(syn::PathSegment {
            ident: Ident::new("Vector", Span::mixed_site()),
            arguments: syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
                colon2_token: None,
                lt_token: Default::default(),
                args: {
                    let mut punct = Punctuated::new();

                    punct.push(syn::GenericArgument::Type(*component_type.clone()));
                    punct.push(syn::GenericArgument::Type(path_from_ident(
                        const_bound_name.clone(),
                    )));

                    punct
                },
                gt_token: Default::default(),
            }),
        });

        path
    };

    // Replace the original component type with vector version.
    **component_type = path_from_ident(trait_bound_name.clone());

    let trait_bounds = {
        let mut punct = Punctuated::new();
        punct.push(syn::TypeParamBound::Trait(syn::TraitBound {
            paren_token: None,
            modifier: syn::TraitBoundModifier::None,
            lifetimes: None,
            path: vector_path,
        }));
        punct
    };

    sig.generics
        .params
        .push(syn::GenericParam::Type(syn::TypeParam {
            attrs: Vec::new(),
            ident: trait_bound_name,
            colon_token: Some(Token![:](Span::mixed_site())),
            bounds: trait_bounds,
            eq_token: None,
            default: None,
        }));

    sig.generics
        .params
        .push(syn::GenericParam::Const(syn::ConstParam {
            attrs: Vec::default(),
            const_token: Default::default(),
            ident: const_bound_name,
            colon_token: Default::default(),
            ty: syn::Type::Path(syn::TypePath {
                qself: None,
                path: Ident::new("usize", Span::mixed_site()).into(),
            }),
            eq_token: None,
            default: None,
        }));

    Ok(ItemFn {
        attrs,
        vis,
        sig,
        block,
    })
}

fn path_from_ident(ident: Ident) -> syn::Type {
    syn::Type::Path(syn::TypePath {
        qself: None,
        path: syn::Path::from(ident),
    })
}

/// Print a formatted string with a newline using the debug printf extension.
///
/// Examples:
///
/// ```rust,ignore
/// debug_printfln!("uv: %v2f", uv);
/// debug_printfln!("pos.x: %f, pos.z: %f, int: %i", pos.x, pos.z, int);
/// ```
///
/// See <https://github.com/KhronosGroup/Vulkan-ValidationLayers/blob/master/docs/debug_printf.md#debug-printf-format-string> for formatting rules.
#[proc_macro]
pub fn debug_printf(input: TokenStream) -> TokenStream {
    debug_printf_inner(syn::parse_macro_input!(input as DebugPrintfInput))
}

/// Similar to `debug_printf` but appends a newline to the format string.
#[proc_macro]
pub fn debug_printfln(input: TokenStream) -> TokenStream {
    let mut input = syn::parse_macro_input!(input as DebugPrintfInput);
    input.format_string.push('\n');
    debug_printf_inner(input)
}

struct DebugPrintfInput {
    span: proc_macro2::Span,
    format_string: String,
    variables: Vec<syn::Expr>,
}

impl syn::parse::Parse for DebugPrintfInput {
    fn parse(input: syn::parse::ParseStream<'_>) -> syn::parse::Result<Self> {
        let span = input.span();

        if input.is_empty() {
            return Ok(Self {
                span,
                format_string: Default::default(),
                variables: Default::default(),
            });
        }

        let format_string = input.parse::<syn::LitStr>()?;
        if !input.is_empty() {
            input.parse::<syn::token::Comma>()?;
        }
        let variables =
            syn::punctuated::Punctuated::<syn::Expr, syn::token::Comma>::parse_terminated(input)?;

        Ok(Self {
            span,
            format_string: format_string.value(),
            variables: variables.into_iter().collect(),
        })
    }
}

fn parsing_error(message: &str, span: proc_macro2::Span) -> TokenStream {
    syn::Error::new(span, message).to_compile_error().into()
}

enum FormatType {
    Scalar {
        ty: proc_macro2::TokenStream,
    },
    Vector {
        ty: proc_macro2::TokenStream,
        width: usize,
    },
}

fn debug_printf_inner(input: DebugPrintfInput) -> TokenStream {
    let DebugPrintfInput {
        format_string,
        variables,
        span,
    } = input;

    fn map_specifier_to_type(
        specifier: char,
        chars: &mut std::str::Chars<'_>,
    ) -> Option<proc_macro2::TokenStream> {
        let mut peekable = chars.peekable();

        Some(match specifier {
            'd' | 'i' => quote::quote! { i32 },
            'o' | 'x' | 'X' => quote::quote! { u32 },
            'a' | 'A' | 'e' | 'E' | 'f' | 'F' | 'g' | 'G' => quote::quote! { f32 },
            'u' => {
                if matches!(peekable.peek(), Some('l')) {
                    chars.next();
                    quote::quote! { u64 }
                } else {
                    quote::quote! { u32 }
                }
            }
            'l' => {
                if matches!(peekable.peek(), Some('u' | 'x')) {
                    chars.next();
                    quote::quote! { u64 }
                } else {
                    return None;
                }
            }
            _ => return None,
        })
    }

    let mut chars = format_string.chars();
    let mut format_arguments = Vec::new();

    while let Some(mut ch) = chars.next() {
        if ch == '%' {
            ch = match chars.next() {
                Some('%') => continue,
                None => return parsing_error("Unterminated format specifier", span),
                Some(ch) => ch,
            };

            let mut has_precision = false;

            while matches!(ch, '0'..='9') {
                ch = match chars.next() {
                    Some(ch) => ch,
                    None => {
                        return parsing_error(
                            "Unterminated format specifier: missing type after precision",
                            span,
                        )
                    }
                };

                has_precision = true;
            }

            if has_precision && ch == '.' {
                ch = match chars.next() {
                    Some(ch) => ch,
                    None => {
                        return parsing_error(
                            "Unterminated format specifier: missing type after decimal point",
                            span,
                        )
                    }
                };

                while matches!(ch, '0'..='9') {
                    ch = match chars.next() {
                        Some(ch) => ch,
                        None => return parsing_error(
                            "Unterminated format specifier: missing type after fraction precision",
                            span,
                        ),
                    };
                }
            }

            if ch == 'v' {
                let width = match chars.next() {
                    Some('2') => 2,
                    Some('3') => 3,
                    Some('4') => 4,
                    Some(ch) => {
                        return parsing_error(&format!("Invalid width for vector: {}", ch), span)
                    }
                    None => return parsing_error("Missing vector dimensions specifier", span),
                };

                ch = match chars.next() {
                    Some(ch) => ch,
                    None => return parsing_error("Missing vector type specifier", span),
                };

                let ty = match map_specifier_to_type(ch, &mut chars) {
                    Some(ty) => ty,
                    _ => {
                        return parsing_error(
                            &format!("Unrecognised vector type specifier: '{}'", ch),
                            span,
                        )
                    }
                };

                format_arguments.push(FormatType::Vector { ty, width });
            } else {
                let ty = match map_specifier_to_type(ch, &mut chars) {
                    Some(ty) => ty,
                    _ => {
                        return parsing_error(
                            &format!("Unrecognised format specifier: '{}'", ch),
                            span,
                        )
                    }
                };

                format_arguments.push(FormatType::Scalar { ty });
            }
        }
    }

    if format_arguments.len() != variables.len() {
        return syn::Error::new(
            span,
            format!(
                "{} % arguments were found, but {} variables were given",
                format_arguments.len(),
                variables.len()
            ),
        )
        .to_compile_error()
        .into();
    }

    let mut variable_idents = String::new();
    let mut input_registers = Vec::new();
    let mut op_loads = Vec::new();

    for (i, (variable, format_argument)) in variables.into_iter().zip(format_arguments).enumerate()
    {
        let ident = quote::format_ident!("_{}", i);

        let _ = write!(variable_idents, "%{} ", ident);

        let assert_fn = match format_argument {
            FormatType::Scalar { ty } => {
                quote::quote! { spirv_std::debug_printf_assert_is_type::<#ty> }
            }
            FormatType::Vector { ty, width } => {
                quote::quote! { spirv_std::debug_printf_assert_is_vector::<#ty, _, #width> }
            }
        };

        input_registers.push(quote::quote! {
            #ident = in(reg) &#assert_fn(#variable),
        });

        let op_load = format!("%{ident} = OpLoad _ {{{ident}}}", ident = ident);

        op_loads.push(quote::quote! {
            #op_load,
        });
    }

    let input_registers = input_registers
        .into_iter()
        .collect::<proc_macro2::TokenStream>();
    let op_loads = op_loads.into_iter().collect::<proc_macro2::TokenStream>();

    let op_string = format!("%string = OpString {:?}", format_string);

    let output = quote::quote! {
        ::core::arch::asm!(
            "%void = OpTypeVoid",
            #op_string,
            "%debug_printf = OpExtInstImport \"NonSemantic.DebugPrintf\"",
            #op_loads
            concat!("%result = OpExtInst %void %debug_printf 1 %string ", #variable_idents),
            #input_registers
        )
    };

    output.into()
}
