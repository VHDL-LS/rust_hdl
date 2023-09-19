use crate::ast::{ContextItem, ContextReference, LibraryClause, UseClause};
use crate::syntax::TokenAccess;
use crate::SrcPos;

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
