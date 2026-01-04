use crate::parser::Parser;
use crate::syntax::NodeKind::*;
use crate::tokens::token_kind::Keyword as Kw;
use crate::tokens::token_kind::TokenKind::*;

impl Parser {
    pub fn architecture(&mut self) {
        self.start_node(ArchitectureBody);
        self.expect_kw(Kw::Architecture);
        self.identifier();
        self.expect_kw(Kw::Of);
        self.name();
        self.expect_kw(Kw::Is);
        self.declarative_part();
        self.expect_kw(Kw::Begin);
        self.concurrent_statements();
        self.opt_token(Keyword(Kw::End));
        self.opt_token(Keyword(Kw::Architecture));
        self.opt_identifier();
        self.expect_token(SemiColon);
        self.end_node();
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::{test_utils::to_test_text, Parser};

    #[test]
    fn parse_architecture_body() {
        insta::assert_snapshot!(to_test_text(
            Parser::architecture,
            "\
architecture arch_name of myent is
begin
end architecture;"
        ));
    }

    #[test]
    fn parse_architecture_body_end_identifier() {
        insta::assert_snapshot!(to_test_text(
            Parser::architecture,
            "\
architecture arch_name of myent is
begin
end architecture arch_name;"
        ));
    }

    #[test]
    fn parse_architecture_body_end() {
        insta::assert_snapshot!(to_test_text(
            Parser::architecture,
            "\
architecture arch_name of myent is
begin
end;"
        ));
    }
}
