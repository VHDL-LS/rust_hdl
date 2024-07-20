use crate::ast::WithDecl;
use crate::formatting::buffer::Buffer;
use crate::formatting::VHDLFormatter;
use crate::{TokenAccess, TokenId};
use vhdl_lang::ast::Ident;
use vhdl_lang::TokenSpan;

impl VHDLFormatter<'_> {
    pub(crate) fn format_token_id(&self, id: TokenId, buffer: &mut Buffer) {
        buffer.push_token(self.tokens.get_token(id));
    }

    pub(crate) fn format_token_span(&self, span: TokenSpan, buffer: &mut Buffer) {
        for (index, id) in span.iter().enumerate() {
            self.format_token_id(id, buffer);
            if index < span.len() - 1 {
                buffer.push_whitespace();
            }
        }
    }

    pub(crate) fn join_token_span(&self, span: TokenSpan, buffer: &mut Buffer) {
        for id in span.iter() {
            self.format_token_id(id, buffer);
        }
    }

    pub(crate) fn format_ident(&self, ident: &WithDecl<Ident>, buffer: &mut Buffer) {
        self.format_token_id(ident.tree.token, buffer)
    }
}
