// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use super::common::ParseResult;
use super::names::{parse_designator, parse_name};
use super::subprogram::parse_signature;
use super::subtype_indication::parse_subtype_indication;
use super::tokens::{Kind::*, TokenSpan};
use crate::ast::token_range::WithTokenSpan;
use crate::ast::{AliasDeclaration, WithDecl};
use crate::syntax::recover::expect_semicolon_or_last;
use vhdl_lang::syntax::parser::ParsingContext;

pub fn parse_alias_declaration(
    ctx: &mut ParsingContext<'_>,
) -> ParseResult<WithTokenSpan<AliasDeclaration>> {
    let start_token = ctx.stream.expect_kind(Alias)?;
    let designator = WithDecl::new(parse_designator(ctx)?);
    let subtype_indication = {
        if ctx.stream.skip_if_kind(Colon) {
            Some(parse_subtype_indication(ctx)?)
        } else {
            None
        }
    };

    ctx.stream.expect_kind(Is)?;
    let name = parse_name(ctx)?;

    let signature = {
        if ctx.stream.peek_kind() == Some(LeftSquare) {
            Some(parse_signature(ctx)?)
        } else {
            None
        }
    };

    let end_token = expect_semicolon_or_last(ctx);

    Ok(WithTokenSpan::new(
        AliasDeclaration {
            designator,
            subtype_indication,
            name,
            signature,
        },
        TokenSpan::new(start_token, end_token),
    ))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::test::Code;

    #[test]
    fn parse_simple_alias() {
        let code = Code::new("alias foo is name;");
        assert_eq!(
            code.with_stream(parse_alias_declaration),
            WithTokenSpan::new(
                AliasDeclaration {
                    designator: code.s1("foo").decl_designator(),
                    subtype_indication: None,
                    name: code.s1("name").name(),
                    signature: None
                },
                code.token_span()
            )
        );
    }

    #[test]
    fn parse_alias_with_subtype_indication() {
        let code = Code::new("alias foo : vector(0 to 1) is name;");
        assert_eq!(
            code.with_stream(parse_alias_declaration),
            WithTokenSpan::new(
                AliasDeclaration {
                    designator: code.s1("foo").decl_designator(),
                    subtype_indication: Some(code.s1("vector(0 to 1)").subtype_indication()),
                    name: code.s1("name").name(),
                    signature: None
                },
                code.token_span()
            )
        );
    }

    #[test]
    fn parse_alias_with_signature() {
        let code = Code::new("alias foo is name [return natural];");
        assert_eq!(
            code.with_stream(parse_alias_declaration),
            WithTokenSpan::new(
                AliasDeclaration {
                    designator: code.s1("foo").decl_designator(),
                    subtype_indication: None,
                    name: code.s1("name").name(),
                    signature: Some(code.s1("[return natural]").signature())
                },
                code.token_span()
            )
        );
    }

    #[test]
    fn parse_alias_with_operator_symbol() {
        let code = Code::new("alias \"and\" is name;");

        let designator = code.s1("\"and\"").decl_designator();

        assert_eq!(
            code.with_stream(parse_alias_declaration),
            WithTokenSpan::new(
                AliasDeclaration {
                    designator,
                    subtype_indication: None,
                    name: code.s1("name").name(),
                    signature: None
                },
                code.token_span()
            )
        );
    }

    #[test]
    fn parse_alias_with_character() {
        let code = Code::new("alias 'c' is 'b';");

        let designator = code.s1("'c'").decl_designator();

        assert_eq!(
            code.with_stream(parse_alias_declaration),
            WithTokenSpan::new(
                AliasDeclaration {
                    designator,
                    subtype_indication: None,
                    name: code.s1("'b'").name(),
                    signature: None
                },
                code.token_span()
            )
        );
    }
}
