use core::iter::FromIterator;
use core::iter::IntoIterator;
use proc_macro::{Delimiter, Group, TokenStream, TokenTree};

#[proc_macro_attribute]
pub fn spirv(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let mut tokens = Vec::new();
    for tt in item {
        match tt.clone() {
            TokenTree::Group(group) if group.delimiter() == Delimiter::Parenthesis => {
                let mut sub_tokens = Vec::new();
                for tt in group.stream() {
                    match tt.clone() {
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
                    TokenStream::from_iter(sub_tokens),
                )));
            }
            _ => tokens.push(tt),
        }
    }
    TokenStream::from_iter(tokens)
}
