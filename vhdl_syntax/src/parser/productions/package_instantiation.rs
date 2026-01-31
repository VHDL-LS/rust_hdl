// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com

use crate::parser::Parser;
use crate::syntax::node_kind::NodeKind::*;
use crate::tokens::Keyword as Kw;
use crate::tokens::TokenKind::{SemiColon, *};

impl Parser {
    pub fn package_instantiation_declaration(&mut self) {
        self.start_node(PackageInstantiationDeclaration);
        self.start_node(PackageInstantiation);
        self.package_instantiation_preamble();
        self.opt_generic_map_aspect();
        self.end_node();
        self.expect_token(SemiColon);
        self.end_node();
    }

    pub fn package_instantiation_preamble(&mut self) {
        self.start_node(PackageInstantiationPreamble);
        self.expect_kw(Kw::Package);
        self.identifier();
        self.expect_tokens([Keyword(Kw::Is), Keyword(Kw::New)]);
        self.name();

        self.end_node();
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
