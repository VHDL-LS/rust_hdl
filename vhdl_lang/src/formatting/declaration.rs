use crate::formatting::DesignUnitFormatter;
use vhdl_lang::ast::token_range::WithTokenSpan;
use vhdl_lang::ast::Declaration;

impl DesignUnitFormatter<'_> {
    pub(crate) fn format_declarations(
        &self,
        declarations: &[WithTokenSpan<Declaration>],
        buffer: &mut String,
    ) {
        for (i, decl) in declarations.iter().enumerate() {
            self.format_declaration(decl);
            if i < declarations.len() - 1 {
                self.newline(buffer);
            }
        }
    }

    pub fn format_declaration(&self, declaration: &WithTokenSpan<Declaration>) -> String {
        unimplemented!();
    }
}
