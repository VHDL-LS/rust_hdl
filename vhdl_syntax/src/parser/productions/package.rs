use crate::tokens::TokenStream;
use crate::parser::Parser;
use crate::syntax::node_kind::NodeKind::*;
use crate::tokens::token_kind::Keyword as Kw;
use crate::tokens::token_kind::TokenKind::*;


impl<T: TokenStream> Parser<T> {
    pub fn package(&mut self) {
        self.start_node(Package);
        self.expect_kw(Kw::Package);
        self.identifier();
        self.package_header();
        self.declarative_part();
        self.expect_kw(Kw::End);
        self.opt_token(Keyword(Kw::Package));
        self.identifier();
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
}