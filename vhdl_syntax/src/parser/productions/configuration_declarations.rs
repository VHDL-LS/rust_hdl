// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com

use crate::parser::builder::Checkpoint;
use crate::parser::Parser;
use crate::syntax::NodeKind;
use crate::tokens::TokenKind::*;
use crate::tokens::{Keyword as Kw, TokenKind};

impl Parser {
    pub fn configuration_declaration(&mut self) {
        self.start_node(NodeKind::ConfigurationDeclaration);
        self.expect_kw(Kw::Configuration);
        self.identifier();
        self.expect_kw(Kw::Of);
        self.name();
        self.expect_kw(Kw::Is);
        self.configuration_declarative_part();
        if self.next_is(Keyword(Kw::Use)) && self.next_nth_is(Keyword(Kw::Vunit), 1) {
            self.start_node(NodeKind::SemiColonTerminatedVerificationUnitBindingIndication);
            self.verification_unit_binding_indication();
            self.expect_token(SemiColon);
            self.end_node();
        }
        self.block_configuration();
        self.expect_kw(Kw::End);
        self.opt_token(Keyword(Kw::Configuration));
        self.opt_identifier();
        self.expect_token(SemiColon);
        self.end_node();
    }

    pub fn configuration_declarative_part(&mut self) {
        loop {
            if self.next_is(Keyword(Kw::Use)) && !self.next_nth_is(Keyword(Kw::Vunit), 1) {
                self.use_clause_declaration();
            } else if self.next_is(Keyword(Kw::Group)) {
                unimplemented!("Group declarations");
            } else if self.next_is(Keyword(Kw::Attribute)) {
                self.attribute_specification();
            } else {
                break;
            }
        }
    }

    pub fn configuration_item(&mut self) {
        let checkpoint = self.checkpoint();
        self.expect_kw(Kw::For);
        self.configuration_item_known_keyword(checkpoint);
    }

    pub fn block_configuration(&mut self) {
        self.start_node(NodeKind::BlockConfiguration);
        self.expect_kw(Kw::For);
        self.name();
        self.block_configuration_known_spec();
        self.end_node();
    }

    fn block_configuration_known_spec(&mut self) {
        while self.next_is(Keyword(Kw::Use)) {
            self.use_clause();
        }
        while self.next_is(Keyword(Kw::For)) {
            self.configuration_item();
        }
        self.expect_tokens([Keyword(Kw::End), Keyword(Kw::For), SemiColon]);
    }

    fn configuration_item_known_keyword(&mut self, item_checkpoint: Checkpoint) {
        match self.peek_token() {
            Some(tok @ Keyword(Kw::All | Kw::Others)) => {
                self.start_node_at(item_checkpoint, NodeKind::ComponentConfigurationItem);
                self.start_node(NodeKind::ComponentSpecification);
                if tok == Keyword(Kw::All) {
                    self.start_node(NodeKind::InstantiationListAll);
                } else {
                    self.start_node(NodeKind::InstantiationListOthers);
                }
                self.skip();
                self.end_node();
                self.expect_token(Colon);
                self.name();
                self.end_node();
                self.component_configuration_known_spec();
                self.end_node();
            }
            Some(Identifier) => {
                if self.next_nth_is(Comma, 1) {
                    self.start_node_at(item_checkpoint, NodeKind::ComponentConfigurationItem);
                    self.start_node(NodeKind::ComponentSpecification);
                    self.start_node(NodeKind::InstantiationListList);
                    self.separated_list(Parser::identifier, Comma);
                    self.end_node();
                    self.expect_token(Colon);
                    self.name();
                    self.end_node();
                    self.component_configuration_known_spec();
                    self.end_node();
                } else {
                    let checkpoint = self.checkpoint();
                    self.name();
                    match self.peek_token() {
                        Some(Colon) => {
                            self.start_node_at(
                                item_checkpoint,
                                NodeKind::ComponentConfigurationItem,
                            );
                            self.start_node_at(checkpoint, NodeKind::ComponentSpecification);
                            self.start_node_at(checkpoint, NodeKind::InstantiationListList);
                            self.end_node();
                            self.skip();
                            self.name();
                            self.end_node();
                            self.component_configuration_known_spec();
                            self.end_node();
                        }
                        _ => {
                            self.start_node_at(item_checkpoint, NodeKind::BlockConfigurationItem);
                            self.block_configuration_known_spec();
                            self.end_node();
                        }
                    }
                }
            }
            _ => self.expect_tokens_err([Keyword(Kw::All), Keyword(Kw::Others), Identifier]),
        }
    }

    fn component_configuration_known_spec(&mut self) {
        if self.next_is_one_of([Keyword(Kw::Use), Keyword(Kw::Generic), Keyword(Kw::Port)])
            && !self.next_nth_is(Keyword(Kw::Vunit), 1)
        {
            self.start_node(NodeKind::SemiColonTerminatedBindingIndication);
            self.binding_indication();
            self.expect_token(TokenKind::SemiColon);
            self.end_node();
        }
        if self.next_is(Keyword(Kw::Use)) && self.next_nth_is(Keyword(Kw::Vunit), 1) {
            self.start_node(NodeKind::SemiColonTerminatedVerificationUnitBindingIndication);
            self.verification_unit_binding_indication();
            self.expect_token(SemiColon);
            self.end_node();
        }
        if self.next_is(Keyword(Kw::For)) {
            self.block_configuration();
        }
        self.expect_tokens([Keyword(Kw::End), Keyword(Kw::For), SemiColon]);
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::{test_utils::to_test_text, Parser};

    #[test]
    fn empty_configuration() {
        insta::assert_snapshot!(to_test_text(
            Parser::configuration_declaration,
            "\
configuration cfg of entity_name is
  for rtl(0)
  end for;
end;",
        ));

        insta::assert_snapshot!(to_test_text(
            Parser::configuration_declaration,
            "\
configuration cfg of entity_name is
  for rtl(0)
  end for;
end configuration cfg;",
        ));
    }

    #[test]
    fn configuration_use_clause() {
        insta::assert_snapshot!(to_test_text(
            Parser::configuration_declaration,
            "\
configuration cfg of entity_name is
  use lib.foo.bar;
  use lib2.foo.bar;
  for rtl(0)
  end for;
end configuration cfg;",
        ));
    }

    #[test]
    fn configuration_vunit_binding_indication() {
        insta::assert_snapshot!(to_test_text(
            Parser::configuration_declaration,
            "\
configuration cfg of entity_name is
  use lib.foo.bar;
  use vunit baz.foobar;
  for rtl(0)
  end for;
end configuration cfg;",
        ));
    }

    #[test]
    fn configuration_block_configuration() {
        insta::assert_snapshot!(to_test_text(
            Parser::configuration_declaration,
            "\
configuration cfg of entity_name is
  for rtl(0)
  end for;
end configuration cfg;",
        ));
    }

    #[test]
    fn configuration_nested_block_configuration() {
        insta::assert_snapshot!(to_test_text(
            Parser::configuration_declaration,
            "\
configuration cfg of entity_name is
  for rtl(0)
    for name(0 to 3)
    end for;
    for other_name
    end for;
  end for;
end configuration cfg;",
        ));
    }

    #[test]
    fn configuration_component_configuration_nested() {
        insta::assert_snapshot!(to_test_text(
            Parser::configuration_declaration,
            "\
configuration cfg of entity_name is
  for rtl(0)
    for inst : lib.pkg.comp
      for arch
      end for;
    end for;
  end for;
end configuration cfg;",
        ));
    }

    #[test]
    fn configuration_component_configuration_vunit_binding_indication() {
        insta::assert_snapshot!(to_test_text(
            Parser::configuration_declaration,
            "\
configuration cfg of entity_name is
  for rtl(0)
    for inst : lib.pkg.comp
      use entity work.bar;
      use vunit baz;
      for arch
      end for;
    end for;
  end for;
end configuration cfg;",
        ));
    }

    #[test]
    fn configuration_component_configuration_binding_indication() {
        insta::assert_snapshot!(to_test_text(
            Parser::configuration_declaration,
            "\
configuration cfg of entity_name is
  for rtl(0)
    for inst : lib.pkg.comp
      use entity lib.use_name;
    end for;
  end for;
end configuration cfg;",
        ));
    }

    #[test]
    fn configuration_component_configuration() {
        insta::assert_snapshot!(to_test_text(
            Parser::configuration_declaration,
            "\
configuration cfg of entity_name is
  for rtl(0)
    for inst : lib.pkg.comp
    end for;
    for inst1, inst2, inst3 : lib2.pkg.comp
    end for;
    for all : lib3.pkg.comp
    end for;
    for others : lib4.pkg.comp
    end for;
  end for;
end configuration cfg;",
        ));
    }

    #[test]
    fn entity_entity_aspect_entity() {
        insta::assert_snapshot!(to_test_text(Parser::entity_aspect, "entity lib.foo.name",));
    }

    #[test]
    fn entity_entity_aspect_entity_arch() {
        insta::assert_snapshot!(to_test_text(
            Parser::entity_aspect,
            "entity lib.foo.name(arch)",
        ));
    }

    #[test]
    fn entity_entity_aspect_configuration() {
        insta::assert_snapshot!(to_test_text(
            Parser::entity_aspect,
            "configuration lib.foo.name",
        ));
    }

    #[test]
    fn entity_entity_aspect_open() {
        insta::assert_snapshot!(to_test_text(Parser::entity_aspect, "open",));
    }

    #[test]
    fn simple_configuration_specification() {
        insta::assert_snapshot!(to_test_text(
            Parser::configuration_specification,
            "for all : lib.pkg.comp use entity work.foo(rtl);",
        ));
    }

    #[test]
    fn simple_configuration_specification_end_for() {
        insta::assert_snapshot!(to_test_text(
            Parser::configuration_specification,
            "for all : lib.pkg.comp use entity work.foo(rtl); end for;",
        ));
    }

    #[test]
    fn compound_configuration_specification() {
        insta::assert_snapshot!(to_test_text(
            Parser::configuration_specification,
            "for all : lib.pkg.comp use entity work.foo(rtl); use vunit bar, baz; end for;",
        ));
    }
}
