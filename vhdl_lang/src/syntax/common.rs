// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use super::tokens::Kind;
use crate::ast::token_range::WithToken;
use crate::ast::Ident;
use crate::data::Diagnostic;
use crate::syntax::parser::ParsingContext;
use crate::{SrcPos, TokenId};

/// Parse optional part followed by optional keyword
pub fn parse_optional<F, R>(
    ctx: &mut ParsingContext<'_>,
    keyword: Kind,
    parse_fun: F,
) -> ParseResult<Option<R>>
where
    F: FnOnce(&mut ParsingContext) -> ParseResult<R>,
{
    let optional = {
        if ctx.stream.skip_if_kind(keyword) {
            Some(parse_fun(ctx)?)
        } else {
            None
        }
    };

    Ok(optional)
}

pub fn check_end_identifier_mismatch<T: std::fmt::Display + std::cmp::PartialEq>(
    ctx: &mut ParsingContext,
    ident: &WithToken<T>,
    end_ident: Option<WithToken<T>>,
) -> Option<TokenId> {
    if let Some(end_ident) = end_ident {
        if ident.item == end_ident.item {
            return Some(end_ident.token);
        } else {
            ctx.diagnostics.push(Diagnostic::syntax_error(
                end_ident.pos(ctx),
                format!("End identifier mismatch, expected {}", ident.item),
            ));
        }
    }
    None
}

pub fn check_label_identifier_mismatch(
    ctx: &mut ParsingContext,
    label: Option<&Ident>,
    end_ident: Option<Ident>,
) -> Option<SrcPos> {
    if let Some(ident) = label {
        if let Some(end_ident) = end_ident {
            if ident.item == end_ident.item {
                return Some(end_ident.pos(ctx).clone());
            } else {
                ctx.diagnostics.push(Diagnostic::syntax_error(
                    end_ident.pos(ctx),
                    format!("End label mismatch, expected {}", ident.item),
                ));
            }
        }
    } else if let Some(end_ident) = end_ident {
        ctx.diagnostics.push(Diagnostic::syntax_error(
            end_ident.pos(ctx),
            format!(
                "End label '{}' found for unlabeled statement",
                end_ident.item
            ),
        ));
    }
    None
}

pub type ParseResult<T> = Result<T, Diagnostic>;
