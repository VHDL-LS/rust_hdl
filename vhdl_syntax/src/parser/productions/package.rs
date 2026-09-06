use crate::parser::Parser;
use crate::syntax::node_kind::NodeKind::*;
use crate::syntax::{AstNode, PackageBodyDeclarativeItemSyntax, PackageDeclarativeItemSyntax};
use crate::tokens::token_kind::Keyword as Kw;
use crate::tokens::token_kind::TokenKind::*;

impl Parser {
    pub(crate) fn package(&mut self) {
        self.node(PackageDeclaration, |p| {
            p.package_preamble();
            p.package_header();
            p.package_declarative_part();
            p.package_epilogue();
        });
    }

    pub(crate) fn package_declarative_part(&mut self) {
        self.declarations(PackageDeclarativePart, PackageDeclarativeItemSyntax::META);
    }

    pub(crate) fn package_preamble(&mut self) {
        self.node(PackagePreamble, |p| {
            p.expect_kw(Kw::Package);
            p.identifier();
            p.expect_kw(Kw::Is);
        });
    }

    pub(crate) fn package_epilogue(&mut self) {
        self.node(PackageEpilogue, |p| {
            p.expect_kw(Kw::End);
            p.opt_token(Keyword(Kw::Package));
            p.opt_identifier();
            p.expect_token(SemiColon);
        });
    }

    pub(crate) fn package_header(&mut self) {
        if !self.next_is(Keyword(Kw::Generic)) {
            return;
        }
        self.node(PackageHeader, |p| {
            p.generic_clause();
            if p.next_is(Keyword(Kw::Generic)) {
                p.node(GenericMap, |p| {
                    p.opt_generic_map_aspect();
                    p.expect_token(SemiColon);
                });
            }
        });
    }

    pub(crate) fn package_body(&mut self) {
        self.node(PackageBody, |p| {
            p.package_body_preamble();
            p.package_body_declarative_part();
            p.package_body_epilogue();
        });
    }

    pub(crate) fn package_body_declarative_part(&mut self) {
        self.declarations(
            PackageBodyDeclarativePart,
            PackageBodyDeclarativeItemSyntax::META,
        );
    }

    pub(crate) fn package_body_preamble(&mut self) {
        self.node(PackageBodyPreamble, |p| {
            p.expect_kw(Kw::Package);
            p.expect_kw(Kw::Body);
            p.identifier();
            p.expect_kw(Kw::Is);
        });
    }

    pub(crate) fn package_body_epilogue(&mut self) {
        self.node(PackageBodyEpilogue, |p| {
            p.expect_kw(Kw::End);
            if p.next_is(Keyword(Kw::Package)) {
                p.node(EndPackageBody, |p| {
                    p.skip(); // Kw::Package
                    p.expect_kw(Kw::Body);
                });
            }
            p.opt_identifier();
            p.expect_token(SemiColon);
        });
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::{test_utils::to_test_text, Parser};

    #[test]
    fn test_package_declaration() {
        insta::assert_snapshot!(to_test_text(
            Parser::package,
            "\
package pkg_name is
end package;"
        ));
    }

    #[test]
    fn test_package_declaration_with_declarations() {
        insta::assert_snapshot!(to_test_text(
            Parser::package,
            "\
package pkg_name is
  type foo;
  constant bar : natural := 0;
end package;"
        ));
    }

    #[test]
    fn test_package_declaration_generics_clause() {
        insta::assert_snapshot!(to_test_text(
            Parser::package,
            "\
package pkg_name is
  generic (
    type foo;
    type bar
  );
end package;"
        ));
    }

    #[test]
    fn test_package_declaration_generic_map_aspect() {
        insta::assert_snapshot!(to_test_text(
            Parser::package,
            "\
package pkg_name is
  generic (
    type foo
  );
  generic map (
    foo => bar
  );
end package;"
        ));
    }

    #[test]
    fn test_package_body_declaration() {
        insta::assert_snapshot!(to_test_text(
            Parser::package_body,
            "\
package body pkg_name is
end package body;"
        ));
    }

    #[test]
    fn test_package_body_declaration_with_function() {
        insta::assert_snapshot!(to_test_text(
            Parser::package_body,
            "\
package body pkg_name is
    procedure foo is
    begin
    end foo;
end package body;"
        ));
    }

    // MARK: Error recovery

    #[test]
    fn package_missing_is() {
        assert_recovery_snapshot!(
            "\
package math_pkg
  constant pi : real := 3.14;
end package;",
            Parser::package
        );
    }

    #[test]
    fn package_missing_end() {
        assert_recovery_snapshot!(
            "\
package math_pkg is
  constant pi : real := 3.14;",
            Parser::package
        );
    }

    #[test]
    fn package_body_missing_body_keyword() {
        assert_recovery_snapshot!(
            "\
package math_pkg is
  constant pi : real := 3.14;
end package body;",
            Parser::package_body
        );
    }

    #[test]
    fn package_body_end_package_without_body_keyword() {
        assert_recovery_snapshot!(
            "\
package body math_pkg is
end package;",
            Parser::package_body
        );
    }
}
