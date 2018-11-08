// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use ast::Ident;
use message::{warning, Message, ParseResult};
use tokenizer::Kind;
use tokenstream::TokenStream;

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

pub fn warning_on_end_identifier_mismatch(
    ident: &Ident,
    end_ident: &Option<Ident>,
) -> Option<Message> {
    if let Some(end_ident) = end_ident {
        if ident.item != end_ident.item {
            return Some(warning(
                &end_ident.pos,
                &format!("End identifier mismatch, expected {}", ident.item.name()),
            ));
        }
    }
    None
}
