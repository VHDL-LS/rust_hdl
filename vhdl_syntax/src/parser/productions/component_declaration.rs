// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com

use crate::parser::Parser;
use crate::standard::VHDLStandard;
use crate::syntax::node_kind::NodeKind::*;
use crate::tokens::token_kind::Keyword as Kw;
use crate::tokens::TokenKind::*;

impl Parser {
    pub fn component_declaration(&mut self) {
        self.start_node(ComponentDeclaration);
        self.component_declaration_preamble();
        self.start_node(ComponentDeclarationItems);
        self.opt_generic_clause();
        self.opt_port_clause();
        self.end_node();
        self.component_declaration_epilogue();
        self.end_node();
    }

    pub fn component_declaration_preamble(&mut self) {
        self.start_node(ComponentDeclarationPreamble);
        self.expect_token(Keyword(Kw::Component));
        self.identifier();
        self.opt_token(Keyword(Kw::Is));
        self.end_node();
    }

    pub fn component_declaration_epilogue(&mut self) {
        self.start_node(ComponentDeclarationEpilogue);
        self.expect_token(Keyword(Kw::End));
        if self.standard().is_at_least(VHDLStandard::VHDL2019) {
            self.opt_token(Keyword(Kw::Component));
        } else {
            self.expect_token(Keyword(Kw::Component));
        }
        self.opt_identifier();
        self.expect_token(SemiColon);
        self.end_node();
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils::{to_test_text, to_test_text_with_standard};
    use crate::parser::{parse_syntax_with_standard, Parser};
    use crate::standard::VHDLStandard;

    #[test]
    fn simple_components() {
        insta::assert_snapshot!(to_test_text(
            Parser::component_declaration,
            "\
component foo
end component;
"
        ));
        insta::assert_snapshot!(to_test_text(
            Parser::component_declaration,
            "\
component foo is
end component;
"
        ));
        insta::assert_snapshot!(to_test_text(
            Parser::component_declaration,
            "\
component foo is
end component foo;
",
        ));
    }

    #[test]
    fn components_with_generics() {
        insta::assert_snapshot!(to_test_text(
            Parser::component_declaration,
            "\
component foo is
  generic (
    foo : natural
  );
end component;
",
        ));
    }

    #[test]
    fn components_with_port() {
        insta::assert_snapshot!(to_test_text(
            Parser::component_declaration,
            "\
component foo is
  port (
    foo : natural
  );
end component;
",
        ));
    }

    #[test]
    fn component_end_without_keyword_vhdl2019() {
        // `end;` without the trailing `component` keyword is valid from VHDL-2019 onwards.
        insta::assert_snapshot!(to_test_text_with_standard(
            VHDLStandard::VHDL2019,
            Parser::component_declaration,
            "\
component foo is
end;
"
        ));
        // Optional identifier after `end` is also accepted.
        insta::assert_snapshot!(to_test_text_with_standard(
            VHDLStandard::VHDL2019,
            Parser::component_declaration,
            "\
component foo is
end foo;
"
        ));
    }

    #[test]
    fn component_end_without_keyword_is_error_before_vhdl2019() {
        // Omitting the trailing `component` keyword must produce a diagnostic under VHDL-2008.
        let (_, diagnostics) = parse_syntax_with_standard(
            VHDLStandard::VHDL2008,
            "component foo is\nend;\n".bytes(),
            Parser::component_declaration,
        );
        assert!(
            !diagnostics.is_empty(),
            "expected a diagnostic for missing 'component' keyword under VHDL-2008"
        );
    }
}
