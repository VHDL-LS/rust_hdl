// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2024, Olof Kraigher olof.kraigher@gmail.com
/// Module for robust parsing
use crate::syntax::parser::ParsingContext;
use crate::syntax::Kind::{Colon, SemiColon};
use crate::syntax::{kinds_error, kinds_str};
use crate::TokenId;

/// Special handling when expecting a semicolon.
/// When the next token is
/// * a semicolon, then consume that token and produce no error
/// * a token that could be confused with a semicolon (i.e., a comma),
///   then consume that token and report an error
/// * none of these choices: do not consume the token and report an error
pub fn expect_semicolon(ctx: &mut ParsingContext<'_>) -> Option<TokenId> {
    let token = match ctx.stream.peek_expect() {
        Ok(token) => token,
        Err(err) => {
            ctx.diagnostics
                .push(err.when(format!("expecting {}", kinds_str(&[SemiColon]))));
            return None;
        }
    };
    match token.kind {
        SemiColon => {
            ctx.stream.skip();
            Some(ctx.stream.get_last_token_id())
        }
        Colon => {
            ctx.stream.skip();
            ctx.diagnostics
                .push(kinds_error(token.pos.clone(), &[SemiColon]));
            Some(ctx.stream.get_last_token_id())
        }
        _ => {
            ctx.diagnostics
                .push(kinds_error(ctx.stream.pos_before(token), &[SemiColon]));
            None
        }
    }
}

/// Expect the next token to be a SemiColon, or return the last token.
/// The behavior is the same as [expect_semicolon].
#[must_use]
pub fn expect_semicolon_or_last(ctx: &mut ParsingContext<'_>) -> TokenId {
    expect_semicolon(ctx).unwrap_or_else(|| ctx.stream.get_last_token_id())
}

#[cfg(test)]
mod tests {
    use crate::analysis::tests::Code;
    use crate::ast::token_range::WithTokenSpan;
    use crate::ast::Declaration;
    use crate::syntax::declarative_part::parse_declarative_part;
    use crate::syntax::test::check_diagnostics;
    use vhdl_lang::ast::{ObjectClass, ObjectDeclaration};
    use vhdl_lang::Diagnostic;

    #[test]
    fn recover_from_semicolon_in_declarative_path() {
        let code = Code::new(
            "\
signal x : std_logic := a.
signal y: bit;
",
        );
        let (declarations, diagnostics) = code.parse_ok(parse_declarative_part);
        assert_eq!(
            declarations,
            vec![
                WithTokenSpan::new(
                    Declaration::Object(ObjectDeclaration {
                        class: ObjectClass::Signal,
                        idents: vec![code.s1("x").decl_ident()],
                        colon_token: code.s1(":").token(),
                        subtype_indication: code.s1("std_logic").subtype_indication(),
                        expression: Some(code.s1("a.").s1("a").expr())
                    }),
                    code.s1("signal x : std_logic := a.").token_span()
                ),
                WithTokenSpan::new(
                    Declaration::Object(ObjectDeclaration {
                        class: ObjectClass::Signal,
                        idents: vec![code.s1("y").decl_ident()],
                        colon_token: code.s(":", 3).token(),
                        subtype_indication: code.s1("bit").subtype_indication(),
                        expression: None
                    }),
                    code.s1("signal y: bit;").token_span()
                ),
            ]
        );
        check_diagnostics(
            diagnostics,
            vec![
                Diagnostic::syntax_error(
                    code.s1(".").pos().pos_at_end(),
                    "Expected '{identifier}', '{character}', '{string}' or 'all'",
                ),
                Diagnostic::syntax_error(code.s1(".").pos().pos_at_end(), "Expected ';'"),
            ],
        );
    }
}
