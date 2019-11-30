// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use crate::ast::AliasDeclaration;
use crate::diagnostic::ParseResult;
use crate::names::{parse_designator, parse_name};
use crate::subprogram::parse_signature;
use crate::subtype_indication::parse_subtype_indication;
use crate::tokenizer::Kind::*;
use crate::tokenstream::TokenStream;

pub fn parse_alias_declaration(stream: &mut TokenStream) -> ParseResult<AliasDeclaration> {
    stream.expect_kind(Alias)?;
    let designator = parse_designator(stream)?;
    let subtype_indication = {
        if stream.skip_if_kind(Colon)? {
            Some(parse_subtype_indication(stream)?)
        } else {
            None
        }
    };

    stream.expect_kind(Is)?;
    let name = parse_name(stream)?;

    let signature = {
        if stream.peek_kind()? == Some(LeftSquare) {
            Some(parse_signature(stream)?)
        } else {
            None
        }
    };

    stream.expect_kind(SemiColon)?;

    Ok(AliasDeclaration {
        designator,
        subtype_indication,
        name,
        signature,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_util::Code;

    #[test]
    fn parse_simple_alias() {
        let code = Code::new("alias foo is name;");
        assert_eq!(
            code.with_stream(parse_alias_declaration),
            AliasDeclaration {
                designator: code.s1("foo").designator(),
                subtype_indication: None,
                name: code.s1("name").name(),
                signature: None
            }
        );
    }

    #[test]
    fn parse_alias_with_subtype_indication() {
        let code = Code::new("alias foo : vector(0 to 1) is name;");
        assert_eq!(
            code.with_stream(parse_alias_declaration),
            AliasDeclaration {
                designator: code.s1("foo").designator(),
                subtype_indication: Some(code.s1("vector(0 to 1)").subtype_indication()),
                name: code.s1("name").name(),
                signature: None
            }
        );
    }

    #[test]
    fn parse_alias_with_signature() {
        let code = Code::new("alias foo is name [return natural];");
        assert_eq!(
            code.with_stream(parse_alias_declaration),
            AliasDeclaration {
                designator: code.s1("foo").designator(),
                subtype_indication: None,
                name: code.s1("name").name(),
                signature: Some(code.s1("[return natural]").signature())
            }
        );
    }

    #[test]
    fn parse_alias_with_operator_symbol() {
        let code = Code::new("alias \"and\" is name;");

        let designator = code.s1("\"and\"").designator();

        assert_eq!(
            code.with_stream(parse_alias_declaration),
            AliasDeclaration {
                designator,
                subtype_indication: None,
                name: code.s1("name").name(),
                signature: None
            }
        );
    }

    #[test]
    fn parse_alias_with_character() {
        let code = Code::new("alias 'c' is 'b';");

        let designator = code.s1("'c'").designator();

        assert_eq!(
            code.with_stream(parse_alias_declaration),
            AliasDeclaration {
                designator,
                subtype_indication: None,
                name: code.s1("'b'").name(),
                signature: None
            }
        );
    }
}
