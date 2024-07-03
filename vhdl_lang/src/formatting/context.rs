use crate::ast::ContextClause;
use crate::formatting::DesignUnitFormatter;

impl DesignUnitFormatter<'_> {
    pub fn format_context_clause(&self, clause: &ContextClause, buffer: &mut String) {
        if !clause.is_empty() {
            unimplemented!()
        }
    }
}
