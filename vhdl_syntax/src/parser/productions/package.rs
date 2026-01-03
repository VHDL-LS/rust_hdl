use crate::parser::Parser;
use crate::syntax::node_kind::NodeKind::*;
use crate::tokens::token_kind::Keyword as Kw;
use crate::tokens::token_kind::TokenKind::*;
use crate::tokens::TokenStream;

impl<T: TokenStream> Parser<T> {
    pub fn package(&mut self) {
        self.start_node(Package);
        self.expect_kw(Kw::Package);
        self.identifier();
        self.expect_kw(Kw::Is);
        self.package_header();
        self.declarative_part();
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
        self.expect_kw(Kw::Package);
        self.expect_kw(Kw::Body);
        self.identifier();
        self.expect_kw(Kw::Is);
        self.declarative_part();
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
}
