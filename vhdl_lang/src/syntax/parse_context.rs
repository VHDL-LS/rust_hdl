use crate::ast::{ContextItem, ContextReference, LibraryClause, UseClause};
use crate::SrcPos;
use crate::syntax::{TokenAccess};

/// holds references to information obtained while parsing
/// that are not directly available in the AST.
/// Currently, only holds access to the tokens of the stream
pub struct ParseContext<'a> {
    pub tokens: &'a dyn TokenAccess
}

impl UseClause {
    pub fn pos(&self, ctx: &dyn TokenAccess) -> SrcPos {
        ctx.get_span(self.use_token, self.semi_token)
    }
}

impl ContextReference {
    pub fn pos(&self, ctx: &dyn TokenAccess) -> SrcPos {
        ctx.get_span(self.context_token, self.semi_token)
    }
}

impl LibraryClause {
    pub fn pos(&self, ctx: &dyn TokenAccess) -> SrcPos {
        ctx.get_span(self.library_token, self.semi_token)
    }
}

impl ContextItem {
    pub fn pos(&self, ctx: &dyn TokenAccess) -> SrcPos {
        match self {
            ContextItem::Use(use_clause) => use_clause.pos(ctx),
            ContextItem::Library(lib_clause) => lib_clause.pos(ctx),
            ContextItem::Context(ctx_clause) => ctx_clause.pos(ctx),
        }
    }
}
