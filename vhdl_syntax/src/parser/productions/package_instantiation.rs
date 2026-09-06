// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com

use crate::parser::marker::CompletedMarker;
use crate::parser::Parser;
use crate::syntax::node_kind::NodeKind::*;
use crate::tokens::Keyword as Kw;
use crate::tokens::TokenKind::{SemiColon, *};

impl Parser {
    pub(crate) fn package_instantiation_declaration(&mut self) -> CompletedMarker {
        self.node(
            PackageInstantiationDeclarationItem,
            Parser::package_instantiation,
        )
    }

    pub(crate) fn package_instantiation(&mut self) {
        self.node(PackageInstantiationDeclaration, |p| {
            p.package_instantiation_preamble();
            p.opt_generic_map_aspect();
            p.expect_token(SemiColon);
        });
    }

    pub(crate) fn package_instantiation_preamble(&mut self) {
        self.node(PackageInstantiationPreamble, |p| {
            p.expect_kw(Kw::Package);
            p.identifier();
            p.expect_tokens([Keyword(Kw::Is), Keyword(Kw::New)]);
            p.name();
        });
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::{test_utils::to_test_text, Parser};

    #[test]
    fn package_instantiation() {
        insta::assert_snapshot!(to_test_text(
            Parser::package_instantiation_declaration,
            "package ident is new lib.foo.bar;"
        ));
    }

    #[test]
    fn package_instantiation_generic_map() {
        insta::assert_snapshot!(to_test_text(
            Parser::package_instantiation_declaration,
            "\
package ident is new lib.foo.bar
  generic map (
    foo => bar
  );"
        ));
    }
}
