use core::iter::FromIterator;
use core::iter::IntoIterator;
use proc_macro::{Delimiter, Group, TokenStream, TokenTree};

#[proc_macro_attribute]
pub fn spirv(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let mut tokens = Vec::new();
    for tt in item.into_iter() {
        match tt {
            TokenTree::Group(group) => match group.delimiter() {
                Delimiter::Parenthesis => {
                    let mut sub_tokens = Vec::new();
                    for tt in group.stream().into_iter() {
                        match tt {
                            TokenTree::Group(group) => match group.delimiter() {
                                Delimiter::Bracket => {
                                    if group.stream().to_string().starts_with("spirv") {
                                        sub_tokens.pop();
                                    } else {
                                        sub_tokens.push(TokenTree::from(group));
                                    }
                                }
                                _ => sub_tokens.push(TokenTree::from(group)),
                            },
                            _ => sub_tokens.push(tt),
                        }
                    }

                    tokens.push(TokenTree::from(Group::new(
                        Delimiter::Parenthesis,
                        TokenStream::from_iter(sub_tokens.into_iter()),
                    )));
                }
                _ => tokens.push(TokenTree::from(group)),
            },
            _ => tokens.push(tt),
        }
    }

    TokenStream::from_iter(tokens.into_iter())
}
