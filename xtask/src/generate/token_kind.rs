// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2026, Lukas Scheller lukasscheller@icloud.com

use crate::generate::Generator;
use crate::model::{Keyword, Model, TokenKind};
use proc_macro2::{Literal, TokenStream};
use quote::{format_ident, quote};
use strum::IntoEnumIterator;

/// Generates `TokenKind` and `Keyword` from the definitions in `model::token`.
pub struct TokenKindGenerator;

impl Generator for TokenKindGenerator {
    fn name(&self) -> &str {
        "token_kind"
    }

    fn output_dir(&self) -> &str {
        "tokens"
    }

    fn generate_files(&self, _model: &Model) -> Vec<(String, TokenStream)> {
        let token_kind = generate_token_kind();
        let keyword = generate_keyword();
        vec![(
            "generated".to_string(),
            quote! {
                use crate::latin_1::Latin1Str;

                #token_kind
                #keyword
            },
        )]
    }
}

fn latin1(text: &str) -> TokenStream {
    let text = Literal::byte_string(text.as_bytes());
    quote! { Latin1Str::new(#text) }
}

fn generate_token_kind() -> TokenStream {
    let keyword_doc = TokenKind::Keyword(Keyword::Abs).doc();
    let variants = TokenKind::iter().map(|kind| {
        let ident = format_ident!("{}", kind.to_string());
        let doc = kind.doc().map(|doc| quote! { #[doc = #doc] });
        quote! { #doc #ident }
    });

    let (with_text, without_text): (Vec<_>, Vec<_>) =
        TokenKind::iter().partition(|kind| kind.canonical_text().is_some());
    let text_arms = with_text.iter().map(|kind| {
        let ident = format_ident!("{}", kind.to_string());
        let text = latin1(&kind.canonical_text().unwrap());
        quote! { Self::#ident => Some(#text), }
    });
    let without_text = without_text
        .iter()
        .map(|kind| format_ident!("{}", kind.to_string()));

    let keywords: Vec<_> = Keyword::iter()
        .map(|kw| format_ident!("{}", kw.to_string()))
        .collect();
    let others: Vec<_> = TokenKind::iter()
        .map(|kind| format_ident!("{}", kind.to_string()))
        .collect();
    let count = Literal::usize_unsuffixed(keywords.len() + others.len());

    quote! {
        #[allow(clippy::upper_case_acronyms)]
        #[derive(PartialEq, Eq, Copy, Clone, Debug)]
        #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
        pub enum TokenKind {
            #[doc = #keyword_doc]
            Keyword(Keyword),
            #(#variants,)*
        }

        impl TokenKind {
            /// Every token kind, with `Keyword` expanded to one entry per keyword
            pub const ALL: [TokenKind; #count] = [
                #(Self::Keyword(Keyword::#keywords),)*
                #(Self::#others,)*
            ];

            /// Returns the canonical text representation of this token kind, or `None` if the token
            /// kind has no fixed text (e.g. identifiers or literals).
            pub fn canonical_text(&self) -> Option<&'static Latin1Str> {
                match self {
                    Self::Keyword(kw) => Some(kw.canonical_text()),
                    #(#text_arms)*
                    #(Self::#without_text)|* => None,
                }
            }
        }
    }
}

fn generate_keyword() -> TokenStream {
    let idents: Vec<_> = Keyword::iter()
        .map(|kw| format_ident!("{}", kw.to_string()))
        .collect();
    let texts: Vec<_> = Keyword::iter()
        .map(|kw| Literal::byte_string(kw.canonical_text().as_bytes()))
        .collect();
    let count = Literal::usize_unsuffixed(idents.len());

    quote! {
        /// All available keywords in the latest (VHDL 2019) edition of VHDL
        #[derive(PartialEq, Eq, Clone, Copy, Debug)]
        #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
        pub enum Keyword {
            #(#idents,)*
        }

        impl Keyword {
            /// Every keyword, in alphabetical order
            pub const ALL: [Keyword; #count] = [#(Self::#idents,)*];

            /// Returns the canonical (lowercase) text for this keyword.
            pub fn canonical_text(&self) -> &'static Latin1Str {
                match self {
                    #(Self::#idents => Latin1Str::new(#texts),)*
                }
            }

            /// Returns the keyword corresponding to the given Latin-1 string, or `None` if the string
            /// is not a keyword. The comparison is case-insensitive.
            pub fn from_latin1(s: &Latin1Str) -> Option<Self> {
                Some(match s.to_lowercase().as_bytes() {
                    #(#texts => Self::#idents,)*
                    _ => return None,
                })
            }
        }
    }
}
