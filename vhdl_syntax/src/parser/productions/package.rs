use crate::parser::Parser;
use crate::syntax::node_kind::NodeKind::*;
use crate::tokens::token_kind::Keyword as Kw;
use crate::tokens::token_kind::TokenKind::*;

impl Parser {
    pub fn package(&mut self) {
        self.start_node(Package);
        self.package_preamble();
        self.package_header();
        self.declarations();
        self.package_epilogue();
        self.end_node();
    }

    pub fn package_preamble(&mut self) {
        self.start_node(PackagePreamble);
        self.expect_kw(Kw::Package);
        self.identifier();
        self.expect_kw(Kw::Is);
        self.end_node();
    }

    pub fn package_epilogue(&mut self) {
        self.start_node(PackageEpilogue);
        self.expect_kw(Kw::End);
        self.opt_token(Keyword(Kw::Package));
        self.opt_identifier();
        self.expect_token(SemiColon);
        self.end_node();
    }

    pub fn package_header(&mut self) {
        self.start_node(PackageHeader);
        self.opt_generic_clause();
        self.opt_generic_map_aspect();
        self.opt_token(SemiColon);
        self.end_node();
    }

    pub fn package_body(&mut self) {
        self.start_node(PackageBody);
        self.package_body_preamble();
        self.declarations();
        self.package_body_epilogue();
        self.end_node();
    }

    pub fn package_body_preamble(&mut self) {
        self.start_node(PackageBodyPreamble);
        self.expect_kw(Kw::Package);
        self.expect_kw(Kw::Body);
        self.identifier();
        self.expect_kw(Kw::Is);
        self.end_node();
    }

    pub fn package_body_epilogue(&mut self) {
        self.start_node(PackageBodyEpilogue);
        self.expect_kw(Kw::End);
        self.opt_token(Keyword(Kw::Package));
        self.opt_token(Keyword(Kw::Body));
        self.opt_identifier();
        self.expect_token(SemiColon);
        self.end_node();
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
}
