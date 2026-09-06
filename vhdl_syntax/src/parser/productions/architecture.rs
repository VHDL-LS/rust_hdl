use crate::parser::Parser;
use crate::syntax::AstNode;
use crate::syntax::NodeKind::*;
use crate::syntax::{BlockDeclarativeItemSyntax, ConcurrentStatementSyntax};
use crate::tokens::token_kind::Keyword as Kw;
use crate::tokens::token_kind::TokenKind::*;

impl Parser {
    pub(crate) fn architecture(&mut self) {
        self.node(ArchitectureBody, |p| {
            p.architecture_preamble();
            p.architecture_declarative_part();
            p.node(DeclarationStatementSeparator, |p| {
                p.expect_kw(Kw::Begin);
            });
            p.architecture_statement_part();
            p.architecture_epilogue();
        });
    }

    pub(crate) fn architecture_preamble(&mut self) {
        self.node(ArchitecturePreamble, |p| {
            p.expect_kw(Kw::Architecture);
            p.identifier();
            p.expect_kw(Kw::Of);
            p.name();
            p.expect_kw(Kw::Is);
        });
    }

    pub(crate) fn architecture_epilogue(&mut self) {
        self.node(ArchitectureEpilogue, |p| {
            p.expect_kw(Kw::End);
            p.opt_token(Keyword(Kw::Architecture));
            p.opt_identifier();
            p.expect_token(SemiColon);
        });
    }

    pub(crate) fn architecture_declarative_part(&mut self) {
        self.declarations(
            ArchitectureDeclarativePart,
            BlockDeclarativeItemSyntax::META,
        );
    }

    pub(crate) fn architecture_statement_part(&mut self) {
        self.concurrent_statements(ArchitectureStatementPart, ConcurrentStatementSyntax::META);
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

    // MARK: Error recovery

    #[test]
    fn architecture_missing_of() {
        assert_recovery_snapshot!(
            "\
architecture rtl myent is
begin
end;",
            Parser::architecture
        );
    }

    #[test]
    fn architecture_missing_is() {
        assert_recovery_snapshot!(
            "\
architecture rtl of myent
begin
end;",
            Parser::architecture
        );
    }

    #[test]
    fn architecture_missing_begin() {
        assert_recovery_snapshot!(
            "\
architecture rtl of myent is
end;",
            Parser::architecture
        );
    }
}
