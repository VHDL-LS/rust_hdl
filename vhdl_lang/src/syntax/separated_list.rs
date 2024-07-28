// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2023, Olof Kraigher olof.kraigher@gmail.com

use crate::ast::token_range::WithTokenSpan;
use crate::ast::{Name, SeparatedList};
use crate::data::DiagnosticResult;
use crate::syntax::common::ParseResult;
use crate::syntax::names::parse_name;
use crate::syntax::Kind::Comma;
use crate::syntax::{kind_str, Kind, TokenAccess};
use crate::Diagnostic;
use vhdl_lang::syntax::parser::ParsingContext;

/// Skip extraneous tokens of kind `separator`.
/// When there are any extra tokens of that kind, mark all the positions of these tokens as erroneous
fn skip_extraneous_tokens(ctx: &mut ParsingContext<'_>, separator: Kind) {
    if let Some(separator_tok) = ctx.stream.pop_if_kind(separator) {
        let start_pos = ctx.stream.get_pos(separator_tok);
        let mut end_pos = start_pos;
        while let Some(separator_tok) = ctx.stream.pop_if_kind(separator) {
            end_pos = ctx.stream.get_pos(separator_tok)
        }
        ctx.diagnostics.push(Diagnostic::syntax_error(
            start_pos.combine(end_pos),
            format!("Extraneous '{}'", kind_str(separator)),
        ));
    }
}

/// Parses a list of the form
///   `element { separator element }`
/// where `element` is an AST element and `separator` is a token of some `ast::Kind`.
/// The returned list retains information of the whereabouts of the separator tokens.
pub fn parse_list_with_separator<F, T>(
    ctx: &mut ParsingContext<'_>,
    separator: Kind,
    parse_fn: F,
) -> DiagnosticResult<SeparatedList<T>>
where
    F: Fn(&mut ParsingContext<'_>) -> ParseResult<T>,
{
    parse_list_with_separator_or_recover(ctx, separator, parse_fn, None)
}

/// Same as `parse_list_with_separator`.
/// However, when supplied with a `recover_token` will skip until either the separator
/// or the recover token is found.
pub fn parse_list_with_separator_or_recover<F, T>(
    ctx: &mut ParsingContext<'_>,
    separator: Kind,
    parse_fn: F,
    recover_token: Option<Kind>,
) -> DiagnosticResult<SeparatedList<T>>
where
    F: Fn(&mut ParsingContext<'_>) -> ParseResult<T>,
{
    let mut items = vec![];
    let mut tokens = vec![];
    loop {
        match parse_fn(ctx) {
            Ok(item) => items.push(item),
            Err(err) => {
                if let Some(tok) = recover_token {
                    ctx.stream
                        .skip_until(|kind| kind == separator || kind == tok)?;
                    ctx.diagnostics.push(err);
                } else {
                    return Err(err);
                }
            }
        }
        if let Some(separator_tok) = ctx.stream.pop_if_kind(separator) {
            skip_extraneous_tokens(ctx, separator);
            tokens.push(separator_tok);
        } else {
            break;
        }
    }
    Ok(SeparatedList { items, tokens })
}

pub fn parse_name_list(ctx: &mut ParsingContext<'_>) -> DiagnosticResult<Vec<WithTokenSpan<Name>>> {
    Ok(parse_list_with_separator(ctx, Comma, parse_name)?.items)
}

#[cfg(test)]
mod test {
    use crate::ast::SeparatedList;
    use crate::syntax::names::parse_association_element;
    use crate::syntax::separated_list::{parse_list_with_separator_or_recover, parse_name_list};
    use crate::syntax::test::Code;
    use crate::syntax::Kind;
    use crate::syntax::Kind::RightPar;
    use crate::Diagnostic;

    #[test]
    pub fn test_error_on_empty_list() {
        let code = Code::new("");
        let (res, diagnostics) = code.with_partial_stream_diagnostics(parse_name_list);
        assert_eq!(
            res,
            Err(Diagnostic::syntax_error(code.eof_pos(), "Unexpected EOF"))
        );
        assert!(diagnostics.is_empty());
    }

    #[test]
    pub fn parse_single_element_list() {
        let code = Code::new("abc");
        assert_eq!(
            code.parse_ok_no_diagnostics(parse_name_list),
            vec![code.s1("abc").name()]
        )
    }

    #[test]
    fn parse_list_with_many_names() {
        let code = Code::new("work.foo, lib.bar.all");
        assert_eq!(
            code.parse_ok_no_diagnostics(parse_name_list),
            vec![code.s1("work.foo").name(), code.s1("lib.bar.all").name()]
        )
    }

    #[test]
    fn parse_extraneous_single_separators() {
        let code = Code::new("a,,b,c");
        let (res, diag) = code.with_stream_diagnostics(parse_name_list);
        assert_eq!(
            res,
            vec![
                code.s1("a").name(),
                code.s1("b").name(),
                code.s1("c").name()
            ]
        );
        assert_eq!(
            diag,
            vec![Diagnostic::syntax_error(
                code.s(",", 2).pos(),
                "Extraneous ','"
            )]
        )
    }

    #[test]
    fn parse_extraneous_multiple_separators() {
        let code = Code::new("a,,,,b,c");
        let (res, diag) = code.with_stream_diagnostics(parse_name_list);
        assert_eq!(
            res,
            vec![
                code.s1("a").name(),
                code.s1("b").name(),
                code.s1("c").name()
            ]
        );
        assert_eq!(
            diag,
            vec![Diagnostic::syntax_error(
                code.s(",,,", 2).pos(),
                "Extraneous ','"
            )]
        )
    }

    #[test]
    fn parse_recoverable_list() {
        let code = Code::new("a => b,c => d, e =>)");
        let (res, diag) = code.with_stream_diagnostics(|ctx| {
            let res = parse_list_with_separator_or_recover(
                ctx,
                Kind::Comma,
                parse_association_element,
                Some(RightPar),
            );
            ctx.stream.skip();
            res
        });
        assert_eq!(
            res,
            SeparatedList {
                items: vec![
                    code.s1("a => b").association_element(),
                    code.s1("c => d").association_element()
                ],
                tokens: vec![code.s(",", 1).token(), code.s(",", 2).token(),],
            }
        );
        assert_eq!(
            diag,
            vec![Diagnostic::syntax_error(
                code.s1(")"),
                "Expected {expression}"
            )]
        );
    }

    #[test]
    fn parse_list_with_erroneous_elements() {
        let code = Code::new("1,c,d");
        let diag = code
            .parse(parse_name_list)
            .0
            .expect_err("Should not parse OK");
        assert_eq!(
            diag,
            Diagnostic::syntax_error(
                code.s1("1"),
                "Expected '{identifier}', '{character}', '{string}' or 'all'"
            )
        );
    }
}
