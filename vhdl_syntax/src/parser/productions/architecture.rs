use crate::parser::Parser;
use crate::syntax::NodeKind::*;
use crate::tokens::token_kind::Keyword as Kw;
use crate::tokens::token_kind::TokenKind::*;
use crate::tokens::TokenStream;

impl<T: TokenStream> Parser<T> {
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
