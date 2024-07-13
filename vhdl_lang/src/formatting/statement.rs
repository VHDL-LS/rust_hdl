use crate::ast::LabeledConcurrentStatement;

impl crate::formatting::DesignUnitFormatter<'_> {
    pub fn format_concurrent_statements(
        &self,
        statements: &[LabeledConcurrentStatement],
        buffer: &mut String,
    ) {
        for (i, statement) in statements.iter().enumerate() {
            self.format_concurrent_statement(statement);
            if i < statements.len() - 1 {
                self.newline(buffer);
            }
        }
    }

    pub fn format_concurrent_statement(&self, statement: &LabeledConcurrentStatement) -> String {
        unimplemented!()
    }
}
