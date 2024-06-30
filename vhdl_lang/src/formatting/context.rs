use crate::ast::ContextClause;
use crate::formatting::DesignUnitFormatter;

impl DesignUnitFormatter<'_, '_> {
    pub fn format_context_clause(&self, clause: &ContextClause) -> String {
        if clause.is_empty() {
            String::default()
        } else {
            unimplemented!()
        }
    }
}
