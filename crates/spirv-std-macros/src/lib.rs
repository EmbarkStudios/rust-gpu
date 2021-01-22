use proc_macro::{Delimiter, Group, TokenStream, TokenTree};

#[proc_macro_attribute]
pub fn spirv(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let mut tokens = Vec::new();
    for tt in item {
        match tt {
            TokenTree::Group(group) if group.delimiter() == Delimiter::Parenthesis => {
                let mut sub_tokens = Vec::new();
                for tt in group.stream() {
                    match tt {
                        TokenTree::Group(group)
                            if group.delimiter() == Delimiter::Bracket
                                && matches!(group.stream().into_iter().next(), Some(TokenTree::Ident(ident)) if ident.to_string() == "spirv")
                                && matches!(sub_tokens.last(), Some(TokenTree::Punct(p)) if p.as_char() == '#') =>
                        {
                            sub_tokens.pop();
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
    tokens.into_iter().collect()
}

#[proc_macro_attribute]
pub fn gpu_only(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let syn::ItemFn {
        attrs,
        vis,
        sig,
        block,
    } = syn::parse_macro_input!(item as syn::ItemFn);

    let fn_name = sig.ident.clone();

    let output = quote::quote! {
        // Don't warn on unused arguments on the CPU side.
        #[cfg_attr(not(target_arch = "spirv"), allow(unused_variables))]
        #(#attrs)* #vis #sig {
            #[cfg(target_arch="spirv")] { #block }
            #[cfg(not(target_arch="spirv"))] {
                unimplemented!(concat!("`", stringify!(#fn_name), "` is only available on SPIR-V platforms."))
            }
        }
    };

    output.into()
}
