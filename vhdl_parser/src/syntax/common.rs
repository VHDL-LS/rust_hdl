// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use super::tokens::{Kind, TokenStream};
use crate::ast::Ident;
use crate::data::Diagnostic;

/// Parse optional part followed by optional keyword
pub fn parse_optional<F, R>(
    stream: &mut TokenStream,
    keyword: Kind,
    parse_fun: F,
) -> ParseResult<Option<R>>
where
    F: FnOnce(&mut TokenStream) -> ParseResult<R>,
{
    let optional = {
        if stream.skip_if_kind(keyword)? {
            Some(parse_fun(stream)?)
        } else {
            None
        }
    };

    Ok(optional)
}

pub fn error_on_end_identifier_mismatch(
    ident: &Ident,
    end_ident: &Option<Ident>,
) -> Option<Diagnostic> {
    if let Some(end_ident) = end_ident {
        if ident.item != end_ident.item {
            return Some(Diagnostic::error(
                &end_ident.pos,
                format!("End identifier mismatch, expected {}", ident.item.name()),
            ));
        }
    }
    None
}

pub type ParseResult<T> = Result<T, Diagnostic>;
