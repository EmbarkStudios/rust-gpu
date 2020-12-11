#![cfg_attr(target_arch = "spirv", feature(register_attr), register_attr(spirv))]

use proc_macro::TokenStream;
use std::str::FromStr;

#[cfg(not(target_arch = "spirv"))]
#[proc_macro_attribute]
pub fn spirv(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let mut item_str = item.to_string();

    while let Some(start) = item_str.find("#[spirv(") {
        let end = item_str[start..item_str.len()].find(")]");
        item_str.replace_range(start..start + end.unwrap() + 2, "");
    }
    TokenStream::from_str(item_str.as_str()).unwrap()
}
