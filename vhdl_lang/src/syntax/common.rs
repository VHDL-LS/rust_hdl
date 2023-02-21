// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use super::tokens::{Kind, TokenStream};
use crate::ast::Ident;
use crate::data::Diagnostic;
use crate::data::DiagnosticHandler;
use crate::data::WithPos;
use crate::SrcPos;

/// Parse optional part followed by optional keyword
pub fn parse_optional<F, R>(
    stream: &TokenStream,
    keyword: Kind,
    parse_fun: F,
) -> ParseResult<Option<R>>
where
    F: FnOnce(&TokenStream) -> ParseResult<R>,
{
    let optional = {
        if stream.skip_if_kind(keyword) {
            Some(parse_fun(stream)?)
        } else {
            None
        }
    };

    Ok(optional)
}

pub fn check_end_identifier_mismatch<T: std::fmt::Display + std::cmp::PartialEq>(
    ident: &WithPos<T>,
    end_ident: Option<WithPos<T>>,
    diagnostics: &mut dyn DiagnosticHandler,
) -> Option<SrcPos> {
    if let Some(end_ident) = end_ident {
        if ident.item == end_ident.item {
            return Some(end_ident.pos);
        } else {
            diagnostics.error(
                &end_ident.pos,
                format!("End identifier mismatch, expected {}", ident.item),
            );
        }
    }
    None
}

pub fn check_label_identifier_mismatch(
    label: Option<&Ident>,
    end_ident: Option<Ident>,
    diagnostics: &mut dyn DiagnosticHandler,
) -> Option<SrcPos> {
    if let Some(ident) = label {
        if let Some(end_ident) = end_ident {
            if ident.item == end_ident.item {
                return Some(end_ident.pos);
            } else {
                diagnostics.error(
                    &end_ident.pos,
                    format!("End label mismatch, expected {}", ident.item),
                );
            }
        }
    } else if let Some(end_ident) = end_ident {
        diagnostics.error(
            &end_ident.pos,
            format!(
                "End label '{}' found for unlabeled statement",
                end_ident.item
            ),
        );
    }
    None
}

pub type ParseResult<T> = Result<T, Diagnostic>;
