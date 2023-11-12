// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com
extern crate proc_macro;
use proc_macro::TokenStream;

mod token_span_attribute;
mod token_span_derive;

#[proc_macro_derive(TokenSpan)]
pub fn impl_token_span_trait(input: TokenStream) -> TokenStream {
    token_span_derive::add_token_span_impl(input)
}

#[proc_macro_attribute]
pub fn with_token_span(args: TokenStream, input: TokenStream) -> TokenStream {
    token_span_attribute::add_token_span_fields(args, input)
}
