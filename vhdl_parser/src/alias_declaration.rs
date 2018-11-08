// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use ast::{AliasDeclaration, Designator};
use message::ParseResult;
use names::parse_name;
use subprogram::parse_signature;
use subtype_indication::parse_subtype_indication;
use tokenizer::Kind::*;
use tokenstream::TokenStream;

pub fn parse_alias_declaration(stream: &mut TokenStream) -> ParseResult<AliasDeclaration> {
    stream.expect_kind(Alias)?;
    let designator = stream.expect_ident()?.map_into(Designator::Identifier);

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
    use test_util::with_stream;

    #[test]
    fn parse_simple_alias() {
        let (util, result) = with_stream(parse_alias_declaration, "alias foo is name;");
        assert_eq!(
            result,
            AliasDeclaration {
                designator: util.ident("foo").map_into(Designator::Identifier),
                subtype_indication: None,
                name: util.name("name"),
                signature: None
            }
        );
    }

    #[test]
    fn parse_alias_with_subtype_indication() {
        let (util, result) = with_stream(
            parse_alias_declaration,
            "alias foo : vector(0 to 1) is name;",
        );
        assert_eq!(
            result,
            AliasDeclaration {
                designator: util.ident("foo").map_into(Designator::Identifier),
                subtype_indication: Some(util.subtype_indication("vector(0 to 1)")),
                name: util.name("name"),
                signature: None
            }
        );
    }

    #[test]
    fn parse_alias_with_signature() {
        let (util, result) = with_stream(
            parse_alias_declaration,
            "alias foo is name [return natural];",
        );
        assert_eq!(
            result,
            AliasDeclaration {
                designator: util.ident("foo").map_into(Designator::Identifier),
                subtype_indication: None,
                name: util.name("name"),
                signature: Some(util.signature("[return natural]"))
            }
        );
    }

}
