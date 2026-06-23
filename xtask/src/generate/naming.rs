// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2025, Lukas Scheller lukasscheller@icloud.com

use crate::model::TokenKind;
use convert_case::{Case, Casing};
use proc_macro2::{Ident, Span, TokenStream};
use quote::{format_ident, quote};

/// `FooSyntax`
pub fn syntax_type_ident(name: &str) -> Ident {
    format_ident!("{}Syntax", name.to_case(Case::UpperCamel))
}

/// `FooBuilder`
pub fn builder_ident(name: &str) -> Ident {
    format_ident!("{}Builder", name.to_case(Case::UpperCamel))
}

/// `Foo`  (used for NodeKind variants)
pub fn node_kind_ident(name: &str) -> Ident {
    format_ident!("{}", name.to_case(Case::UpperCamel))
}

/// `FooToken`
pub fn token_type_ident(name: &str) -> Ident {
    format_ident!("{}Token", name.to_case(Case::UpperCamel))
}

/// `CheckedFoo`
pub fn checked_type_ident(name: &str) -> Ident {
    format_ident!("Checked{}", name.to_case(Case::UpperCamel))
}

/// `Foo`  (used for enum variant names — merges the two identical functions)
pub fn variant_ident(name: &str) -> Ident {
    format_ident!("{}", name.to_case(Case::UpperCamel))
}

/// Snake-case method identifier, escaping Rust reserved keywords via `r#name`.
pub fn method_ident(name: &str) -> Ident {
    let snake = name.to_case(Case::Snake);
    match snake.as_str() {
        "as" | "async" | "await" | "break" | "const" | "continue" | "crate" | "dyn" | "else"
        | "enum" | "extern" | "false" | "fn" | "for" | "if" | "impl" | "in" | "let" | "loop"
        | "match" | "mod" | "move" | "mut" | "pub" | "ref" | "return" | "self" | "static"
        | "struct" | "super" | "trait" | "true" | "type" | "unsafe" | "use" | "where" | "while" => {
            Ident::new_raw(&snake, Span::call_site())
        }
        _ => format_ident!("{}", snake),
    }
}

/// Full `TokenKind::Xxx` path expression for use in generated code that has no wildcard import.
///
/// Examples:
/// - `Keyword(kw)` → `TokenKind::Keyword(Kw::Entity)`
/// - `SemiColon`   → `TokenKind::SemiColon`
pub fn token_kind_path(kind: &TokenKind) -> TokenStream {
    match kind {
        TokenKind::Keyword(kw) => {
            let kw_ident = format_ident!("{}", kw.to_string());
            quote! { TokenKind::Keyword(Kw::#kw_ident) }
        }
        _ => {
            let kind_ident = format_ident!("{}", kind.to_string());
            quote! { TokenKind::#kind_ident }
        }
    }
}
