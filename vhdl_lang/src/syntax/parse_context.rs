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

#[cfg(test)]
mod tests {
    use crate::syntax::test::Code;

    #[test]
    pub fn pos_of_context_elements() {
        let code = Code::new(
            "\
context my_context is
  library ieee, env;
  context my_context;
  use ieee.std_logic_1164.all, std.env.xyz;
end my_context;
",
        );
        let ctx = code.tokenize();
        let lib = code.context_declaration();
        assert_eq!(lib.items[0].pos(&ctx), code.s1("library ieee, env;").pos());
        assert_eq!(lib.items[1].pos(&ctx), code.s1("context my_context;").pos());
        assert_eq!(
            lib.items[2].pos(&ctx),
            code.s1("use ieee.std_logic_1164.all, std.env.xyz;").pos()
        );
    }
}
