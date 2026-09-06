use crate::parser::Parser;
use crate::syntax::node_kind::NodeKind;
use crate::syntax::NodeKind::LogicalNameList;
use crate::tokens::token_kind::Keyword as Kw;
use crate::tokens::token_kind::TokenKind::*;

impl Parser {
    pub(crate) fn library_clause(&mut self) {
        self.node(NodeKind::LibraryClause, |p| {
            p.expect_kw(Kw::Library);
            p.separated_list(LogicalNameList, Parser::identifier, Comma);
            p.expect_token(SemiColon);
        });
    }

    pub(crate) fn use_clause(&mut self) {
        self.node(NodeKind::UseClause, |p| {
            p.expect_kw(Kw::Use);
            p.name_list();
            p.expect_token(SemiColon);
        });
    }

    pub(crate) fn context_reference(&mut self) {
        self.node(NodeKind::ContextReference, |p| {
            p.expect_kw(Kw::Context);
            p.name_list();
            p.expect_token(SemiColon);
        });
    }

    pub(crate) fn context_clause(&mut self) {
        self.node(NodeKind::ContextClause, |p| loop {
            match p.peek_token() {
                Keyword(Kw::Use) => {
                    p.node(NodeKind::UseClauseContextItem, |p| {
                        p.use_clause();
                    });
                }
                Keyword(Kw::Library) => p.library_clause(),
                Keyword(Kw::Context) => {
                    if !p.next_nth_is(Keyword(Kw::Is), 2) {
                        p.context_reference()
                    } else {
                        break;
                    }
                }
                _ => break,
            }
        });
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::{test_utils::to_test_text, Parser};

    #[test]
    fn test_library_clause_single_name() {
        insta::assert_snapshot!(to_test_text(Parser::library_clause, "library foo;",));
    }

    #[test]
    fn test_library_clause_multiple_names() {
        insta::assert_snapshot!(to_test_text(Parser::library_clause, "library foo, bar;",));
    }

    #[test]
    fn test_use_clause_single_name() {
        insta::assert_snapshot!(to_test_text(Parser::use_clause, "use lib.foo;",));
    }

    #[test]
    fn test_use_clause_multiple_names() {
        insta::assert_snapshot!(to_test_text(
            Parser::use_clause,
            "use foo.'a', lib.bar.all;",
        ));
    }

    #[test]
    fn test_context_reference_single_name() {
        insta::assert_snapshot!(to_test_text(Parser::context_reference, "context lib.foo;",));
    }

    // MARK: Error recovery

    #[test]
    fn context_reference_missing_semicolon() {
        assert_recovery_snapshot!("context lib.foo", Parser::context_reference);
    }

    #[test]
    fn library_clause_missing_name() {
        assert_recovery_snapshot!("library ;", Parser::library_clause);
    }
}
