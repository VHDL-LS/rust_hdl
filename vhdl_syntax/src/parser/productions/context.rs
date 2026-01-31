use crate::parser::Parser;
use crate::syntax::node_kind::NodeKind;
use crate::tokens::token_kind::Keyword as Kw;
use crate::tokens::token_kind::TokenKind::*;

impl Parser {
    pub fn library_clause(&mut self) {
        self.start_node(NodeKind::LibraryClause);
        self.expect_kw(Kw::Library);
        self.identifier_list();
        self.expect_token(SemiColon);
        self.end_node();
    }

    pub fn use_clause(&mut self) {
        self.start_node(NodeKind::UseClause);
        self.expect_kw(Kw::Use);
        self.name_list();
        self.expect_token(SemiColon);
        self.end_node();
    }

    pub fn context_reference(&mut self) {
        self.start_node(NodeKind::ContextReference);
        self.expect_kw(Kw::Context);
        self.name_list();
        self.expect_token(SemiColon);
        self.end_node();
    }

    pub fn context_clause(&mut self) {
        self.start_node(NodeKind::ContextClause);
        loop {
            match self.peek_token() {
                Some(Keyword(Kw::Use)) => self.use_clause(),
                Some(Keyword(Kw::Library)) => self.library_clause(),
                Some(Keyword(Kw::Context)) => {
                    if !self.next_nth_is(Keyword(Kw::Is), 2) {
                        self.context_reference()
                    } else {
                        break;
                    }
                }
                _ => break,
            }
        }
        self.end_node();
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
}
