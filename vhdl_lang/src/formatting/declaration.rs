use crate::formatting::DesignUnitFormatter;
use vhdl_lang::ast::token_range::WithTokenSpan;
use vhdl_lang::ast::Declaration;

impl DesignUnitFormatter<'_, '_> {
    pub fn format_declaration(&self, declaration: &WithTokenSpan<Declaration>) -> String {
        unimplemented!();
    }
}
