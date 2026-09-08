// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com

use crate::parser::marker::CompletedMarker;
use crate::parser::Parser;
use crate::syntax::NodeKind::{self, ConfigurationDeclarativePart};
use crate::tokens::TokenKind::*;
use crate::tokens::{Keyword as Kw, TokenKind};

impl Parser {
    pub(crate) fn configuration_declaration(&mut self) {
        self.node(NodeKind::ConfigurationDeclaration, |p| {
            p.configuration_declaration_preamble();
            p.configuration_declarative_part();
            while p.next_is(Keyword(Kw::Use)) && p.next_nth_is(Keyword(Kw::Vunit), 1) {
                p.node(NodeKind::VerificationUnitBinding, |p| {
                    p.verification_unit_binding_indication();
                    p.expect_token(SemiColon);
                });
            }
            p.block_configuration();
            p.configuration_declaration_epilogue();
        });
    }

    pub(crate) fn configuration_declaration_preamble(&mut self) {
        self.node(NodeKind::ConfigurationDeclarationPreamble, |p| {
            p.expect_kw(Kw::Configuration);
            p.identifier();
            p.expect_kw(Kw::Of);
            p.name();
            p.expect_kw(Kw::Is);
        });
    }

    pub(crate) fn configuration_declaration_epilogue(&mut self) {
        self.node(NodeKind::ConfigurationDeclarationEpilogue, |p| {
            p.expect_kw(Kw::End);
            p.opt_token(Keyword(Kw::Configuration));
            p.opt_identifier();
            p.expect_token(SemiColon);
        });
    }

    pub(crate) fn group_declaration_or_template_declaration(&mut self) -> CompletedMarker {
        if self.next_nth_is(Keyword(Kw::Is), 2) {
            self.group_template_declaration()
        } else {
            self.group_declaration()
        }
    }

    pub(crate) fn group_template_declaration(&mut self) -> CompletedMarker {
        self.node(NodeKind::GroupTemplateDeclaration, |p| {
            p.expect_kw(Kw::Group);
            p.identifier();
            p.expect_kw(Kw::Is);
            p.expect_token(LeftPar);
            p.entity_class_entry_list();
            p.expect_token(RightPar);
            p.expect_token(SemiColon);
        })
    }

    pub(crate) fn group_declaration(&mut self) -> CompletedMarker {
        self.node(NodeKind::GroupDeclaration, |p| {
            p.expect_kw(Kw::Group);
            p.identifier();
            p.expect_token(Colon);
            p.name();
            p.expect_token(SemiColon);
        })
    }

    pub(crate) fn entity_class_entry(&mut self) {
        self.node(NodeKind::EntityClassEntry, |p| {
            p.entity_class();
            p.opt_token(BOX);
        });
    }

    pub(crate) fn entity_class_entry_list(&mut self) {
        self.separated_list(
            NodeKind::EntityClassEntryList,
            Parser::entity_class_entry,
            Comma,
        );
    }

    pub(crate) fn configuration_declarative_part(&mut self) {
        self.node(ConfigurationDeclarativePart, |p| loop {
            if p.next_is(Keyword(Kw::Use)) && !p.next_nth_is(Keyword(Kw::Vunit), 1) {
                p.use_clause_declaration();
            } else if p.next_is(Keyword(Kw::Group)) {
                p.group_declaration();
            } else if p.next_is(Keyword(Kw::Attribute)) {
                p.attribute_specification();
            } else {
                break;
            }
        });
    }

    pub(crate) fn configuration_item(&mut self) {
        match self.peek_nth_token(1) {
            Keyword(Kw::All | Kw::Others) => self.component_configuration(),
            Identifier if self.next_nth_is(Comma, 2) || self.next_nth_is(Colon, 2) => {
                self.component_configuration()
            }
            Identifier => {
                self.node(NodeKind::BlockConfigurationItem, |p| {
                    p.block_configuration();
                });
            }
            _ => {
                self.expect_kw(Kw::For);
                self.expect_tokens_recover([Keyword(Kw::All), Keyword(Kw::Others), Identifier]);
            }
        }
    }

    pub(crate) fn block_configuration(&mut self) {
        self.node(NodeKind::BlockConfiguration, |p| {
            p.block_configuration_preamble();
            p.block_configuration_known_spec();
        });
    }

    pub(crate) fn block_configuration_preamble(&mut self) {
        self.node(NodeKind::BlockConfigurationPreamble, |p| {
            p.expect_kw(Kw::For);
            p.name();
        });
    }

    fn block_configuration_known_spec(&mut self) {
        while self.next_is(Keyword(Kw::Use)) {
            self.use_clause();
        }
        while self.next_is(Keyword(Kw::For)) {
            self.configuration_item();
        }
        self.block_configuration_epilogue();
    }

    pub(crate) fn block_configuration_epilogue(&mut self) {
        self.node(NodeKind::BlockConfigurationEpilogue, |p| {
            p.expect_tokens([Keyword(Kw::End), Keyword(Kw::For), SemiColon]);
        });
    }

    fn component_configuration(&mut self) {
        self.node(NodeKind::ComponentConfiguration, |p| {
            p.node(NodeKind::ComponentConfigurationPreamble, |p| {
                p.expect_kw(Kw::For);
                p.node(NodeKind::ComponentSpecification, |p| {
                    match p.peek_token() {
                        Keyword(Kw::All) => p.skip_into_node(NodeKind::InstantiationListAll),
                        Keyword(Kw::Others) => p.skip_into_node(NodeKind::InstantiationListOthers),
                        _ => p.separated_list(
                            NodeKind::InstantiationListList,
                            Parser::identifier,
                            Comma,
                        ),
                    };
                    p.expect_token(Colon);
                    p.name();
                });
            });
            p.component_configuration_known_spec();
        });
    }

    pub(crate) fn component_configuration_epilogue(&mut self) {
        self.node(NodeKind::ComponentConfigurationEpilogue, |p| {
            p.expect_tokens([Keyword(Kw::End), Keyword(Kw::For), SemiColon]);
        });
    }

    fn component_configuration_known_spec(&mut self) {
        // surprisingly, a `;` is a legal `binding`
        if self.next_is_one_of([
            Keyword(Kw::Use),
            Keyword(Kw::Generic),
            Keyword(Kw::Port),
            SemiColon,
        ]) && !self.next_nth_is(Keyword(Kw::Vunit), 1)
        {
            self.node(NodeKind::Binding, |p| {
                p.binding_indication();
                p.expect_token(TokenKind::SemiColon);
            });
        }
        while self.next_is(Keyword(Kw::Use)) && self.next_nth_is(Keyword(Kw::Vunit), 1) {
            self.node(NodeKind::VerificationUnitBinding, |p| {
                p.verification_unit_binding_indication();
                p.expect_token(SemiColon);
            });
        }
        if self.next_is(Keyword(Kw::For)) {
            self.block_configuration();
        }
        self.component_configuration_epilogue();
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

    #[test]
    fn configuration_declaration_multiple_vunit_binding_indications() {
        insta::assert_snapshot!(to_test_text(
            Parser::configuration_declaration,
            "\
configuration cfg of ent is
    use vunit foo;
    use vunit bar;
    for baz
    end for;
end configuration cfg ;",
        ));
    }

    #[test]
    fn configuration_with_empty_binding() {
        insta::assert_snapshot!(to_test_text(
            Parser::component_configuration,
            "\
for i, i : name;
end for;
"
        ));
    }

    #[test]
    fn component_configuration_with_multiple_vunit_bindings() {
        insta::assert_snapshot!(to_test_text(
            Parser::component_configuration,
            "\
for others: name
    use vunit name;
    use vunit name;
end for;
"
        ));
    }

    #[test]
    fn simple_group_template_declaration() {
        insta::assert_snapshot!(to_test_text(
            Parser::group_declaration_or_template_declaration,
            "group resource is (label);",
        ));
    }

    #[test]
    fn group_template_declaration_with_multiple_entries() {
        insta::assert_snapshot!(to_test_text(
            Parser::group_declaration_or_template_declaration,
            "group pin2pin is (signal, signal);",
        ));
    }

    #[test]
    fn group_template_declaration_with_infinite_entry() {
        insta::assert_snapshot!(to_test_text(
            Parser::group_declaration_or_template_declaration,
            "group dependency is (label, signal <>);",
        ));
    }

    #[test]
    fn simple_group_declaration() {
        insta::assert_snapshot!(to_test_text(
            Parser::group_declaration_or_template_declaration,
            "group g1 : resource (l1);",
        ));
    }

    #[test]
    fn group_declaration_with_multiple_constituents() {
        insta::assert_snapshot!(to_test_text(
            Parser::group_declaration_or_template_declaration,
            "group g2 : pin2pin (sig_a, sig_b);",
        ));
    }
}
