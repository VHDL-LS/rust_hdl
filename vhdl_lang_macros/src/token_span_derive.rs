// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use proc_macro::TokenStream;

use quote::quote;
use syn::{parse_macro_input, spanned::Spanned, DeriveInput, Item, ItemEnum, ItemStruct};

pub fn add_token_span_impl(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let item = Item::from(input);

    match item {
        syn::Item::Struct(item) => add_token_span_impl_struct(item),
        syn::Item::Enum(item) => add_token_span_impl_enum(item),
        _ => syn::Error::new(
            item.span(),
            "The TokenSpan macro can only be applied to struct and enums!",
        )
        .into_compile_error()
        .into(),
    }
}

fn add_token_span_impl_struct(item: ItemStruct) -> TokenStream {
    let name = item.ident;

    quote! {
        #[automatically_derived]
        impl ::vhdl_lang::TokenSpan for #name {
            fn set_start_token(&mut self, start_token: ::vhdl_lang::TokenId) {
                self.info.set_start_token(start_token);
            }

            fn set_end_token(&mut self, end_token: ::vhdl_lang::TokenId) {
                self.info.set_end_token(end_token);
            }

            fn get_start_token(&self) -> Option<::vhdl_lang::TokenId> {
                self.info.get_start_token()
            }

            fn get_end_token(&self) -> Option<::vhdl_lang::TokenId> {
                self.info.get_end_token()
            }

            fn get_token_slice<'a>(&self, tokens: &'a dyn ::vhdl_lang::TokenAccess) -> &'a[::vhdl_lang::Token] {
                tokens.get_token_slice(self.get_start_token().unwrap(), self.get_end_token().unwrap())
            }

            fn get_pos(&self, tokens: &dyn ::vhdl_lang::TokenAccess) -> ::vhdl_lang::SrcPos {
                self.info.get_pos(tokens).unwrap_or_else(|| panic!("Cannot get position of empty token span!"))
            }
        }

    }
    .into()
}

fn add_token_span_impl_enum(item: ItemEnum) -> TokenStream {
    let enum_name = item.ident;

    // Some validity checks for enums
    for variant in &item.variants {
        // Only allow enums with unnamed fields
        if !matches!(variant.fields, syn::Fields::Unnamed(_)) {
            return syn::Error::new(
                variant.span(),
                "All variants of enumerations using the TokenSpan macro must use unnamed fields!",
            )
            .into_compile_error()
            .into();
        }

        // Only allow enums with exactly 1 field
        if variant.fields.len() != 1 {
            return syn::Error::new(
                variant.span(),
                "All variants of enumerations using the TokenSpan macro must use contain exactly one field!",
            )
            .into_compile_error()
            .into();
        }
    }

    let variant_names: Vec<syn::Ident> = item
        .variants
        .into_iter()
        .map(|variant| variant.ident)
        .collect();

    // Parse the delegating implementations for all `TokenSpan` methods
    // Note that by calling the methods using the trait itself `TokenSpan`, we get nice error messages if
    // a fields' type does not implement the trait!
    quote! {
        impl ::vhdl_lang::TokenSpan for #enum_name {
            fn set_start_token(&mut self, start_token: ::vhdl_lang::TokenId) {
                match self {
                    #( #enum_name::#variant_names(inner) => ::vhdl_lang::TokenSpan::set_start_token(inner, start_token), )*
                }
            }

            fn set_end_token(&mut self, end_token: ::vhdl_lang::TokenId) {
                match self {
                    #( #enum_name::#variant_names(inner) => ::vhdl_lang::TokenSpan::set_end_token(inner, end_token), )*
                }
            }

            fn get_start_token(&self) -> Option<::vhdl_lang::TokenId> {
                match self {
                    #( #enum_name::#variant_names(inner) => ::vhdl_lang::TokenSpan::get_start_token(inner), )*
                }
            }
            fn get_end_token(&self) -> Option<::vhdl_lang::TokenId> {
                match self {
                    #( #enum_name::#variant_names(inner) => ::vhdl_lang::TokenSpan::get_end_token(inner), )*
                }
            }

            fn get_token_slice<'a>(&self, tokens: &'a dyn ::vhdl_lang::TokenAccess) -> &'a [::vhdl_lang::Token] {
                match self {
                    #( #enum_name::#variant_names(inner) => ::vhdl_lang::TokenSpan::get_token_slice(inner, tokens), )*
                }
            }

            fn get_pos(&self, ctx: &dyn ::vhdl_lang::TokenAccess) -> ::vhdl_lang::SrcPos {
                match self {
                    #( #enum_name::#variant_names(inner) => ::vhdl_lang::TokenSpan::get_pos(inner, ctx), )*
                }
            }
        }
    }
    .into()
}
