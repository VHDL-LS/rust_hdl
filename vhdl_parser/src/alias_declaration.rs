// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use ast::{AliasDeclaration, AliasDesignator};
use message::ParseResult;
use names::parse_name;
use subprogram::parse_signature;
use subtype_indication::parse_subtype_indication;
use tokenizer::Kind::*;
use tokenstream::TokenStream;

pub fn parse_alias_declaration(stream: &mut TokenStream) -> ParseResult<AliasDeclaration> {
    stream.expect_kind(Alias)?;

    let token = stream.expect()?;
    let designator = try_token_kind!(
        token,
        Identifier => AliasDesignator::Identifier(token.expect_ident()?),
        StringLiteral => AliasDesignator::OperatorSymbol(token.expect_string()?),
        Character => AliasDesignator::Character(token.expect_character()?)
    );

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
        subtype_indication: subtype_indication,
        name: name,
        signature: signature,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_util::Code;

    #[test]
    fn parse_simple_alias() {
        let code = Code::new("alias foo is name;");
        assert_eq!(
            code.with_stream(parse_alias_declaration),
            AliasDeclaration {
                designator: AliasDesignator::Identifier(code.s1("foo").ident()),
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
                designator: AliasDesignator::Identifier(code.s1("foo").ident()),
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
                designator: AliasDesignator::Identifier(code.s1("foo").ident()),
                subtype_indication: None,
                name: code.s1("name").name(),
                signature: Some(code.s1("[return natural]").signature())
            }
        );
    }

    #[test]
    fn parse_alias_with_operator_symbol() {
        let code = Code::new("alias \"and\" is name;");

        let designator = AliasDesignator::OperatorSymbol(code.s1("\"and\"").operator_symbol());

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

        let designator = AliasDesignator::Character(code.s1("'c'").character());

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
