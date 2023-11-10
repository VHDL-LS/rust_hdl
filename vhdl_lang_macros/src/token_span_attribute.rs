// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use proc_macro::TokenStream;

use quote::quote;
use syn::{
    parse::{Nothing, Parser},
    parse_macro_input, parse_quote,
    punctuated::Punctuated,
    spanned::Spanned,
    token::Brace,
    FieldsNamed, ItemStruct,
};

pub fn add_token_span_fields(args: TokenStream, input: TokenStream) -> TokenStream {
    let _ = parse_macro_input!(args as Nothing);
    let mut item_struct = parse_macro_input!(input as ItemStruct);

    // Add an attribute to the struct for deriving the `HasTokenSpan` implementation
    let token_span_attr_raw: syn::Attribute =
        parse_quote! { #[derive(::vhdl_lang_macros::TokenSpan)] };
    item_struct.attrs.push(token_span_attr_raw);

    // Create (or move from the given struct) an instance of `FieldsNamed` which holds a list of named fields
    let mut fields: FieldsNamed;
    if let syn::Fields::Unit = item_struct.fields {
        fields = FieldsNamed {
            brace_token: Brace::default(), // The token for the '{' braces, with a default span
            named: Punctuated::default(),  // An empty list of named fields
        };
    } else if let syn::Fields::Named(inner_fields) = item_struct.fields {
        fields = inner_fields;
    } else {
        return syn::Error::new(
            item_struct.span(),
            "A token span cannot be added for tuples!",
        )
        .into_compile_error()
        .into();
    }

    // Add the required field
    fields.named.push(
        syn::Field::parse_named
            .parse2(quote! {pub(crate) span: ::vhdl_lang::TokenSpan})
            .unwrap(),
    );

    // Put the modified list back into the struct
    item_struct.fields = syn::Fields::Named(fields);

    quote! {
        #item_struct
    }
    .into()
}
