use core::iter::FromIterator;
use proc_macro::{Delimiter, Group, TokenStream, TokenTree};

#[proc_macro_attribute]
pub fn spirv(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let mut tokens = Vec::new();
    for tt in item {
        if let TokenTree::Group(group) = tt {
            if let Delimiter::Parenthesis = group.delimiter() {
                let mut sub_tokens = Vec::new();
                for tt in group.stream() {
                    if let TokenTree::Group(group) = tt {
                        if let Delimiter::Bracket = group.delimiter() {
                            if group.stream().to_string().starts_with("spirv") {
                                sub_tokens.pop();
                            } else {
                                sub_tokens.push(TokenTree::from(group));
                            }
                        }
                    } else {
                        sub_tokens.push(tt);
                    }
                }
                tokens.push(TokenTree::from(Group::new(
                    Delimiter::Parenthesis,
                    TokenStream::from_iter(sub_tokens),
                )));
            } else {
                tokens.push(TokenTree::from(group));
            }
        } else {
            tokens.push(tt);
        }
    }

    TokenStream::from_iter(tokens)
}
