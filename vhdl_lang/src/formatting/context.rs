use crate::ast::{ContextClause, ContextDeclaration, ContextItem};
use crate::formatting::VHDLFormatter;
use crate::TokenSpan;

impl VHDLFormatter<'_> {
    pub fn format_context(&self, context: &ContextDeclaration, buffer: &mut String) {
        // context <name> is
        self.format_token_span(
            TokenSpan::new(context.span.start_token, context.span.start_token + 2),
            buffer,
        );
        self.increase_indentation();
        if !context.items.is_empty() {
            self.newline(buffer);
        }
        self.format_context_clause(&context.items, buffer);
        self.decrease_indentation();
        self.newline(buffer);
        self.format_token_span(
            TokenSpan::new(context.end_token, context.span.end_token - 1),
            buffer,
        );
        self.format_token_id(context.span.end_token, buffer);
    }

    pub fn format_context_clause(&self, clause: &ContextClause, buffer: &mut String) {
        for (i, item) in clause.iter().enumerate() {
            match item {
                ContextItem::Use(use_clause) => self.format_use_clause(use_clause, buffer),
                ContextItem::Library(library_clause) => {
                    self.format_library_clause(library_clause, buffer)
                }
                ContextItem::Context(context_reference) => {
                    self.format_context_reference(context_reference, buffer)
                }
            }
            if i < clause.len() - 1 {
                self.newline(buffer);
            }
        }
    }
}
