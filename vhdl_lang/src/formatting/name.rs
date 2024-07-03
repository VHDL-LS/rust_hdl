use crate::ast::token_range::WithTokenSpan;
use crate::ast::Designator;
use crate::formatting::DesignUnitFormatter;
use vhdl_lang::ast::Name;

impl DesignUnitFormatter<'_> {
    pub fn format_name(&self, name: &WithTokenSpan<Name>, buffer: &mut String) {
        match &name.item {
            Name::Designator(designator) => self.format_designator(&designator.item, buffer),
            _ => unimplemented!(),
        }
    }

    pub fn format_designator(&self, designator: &Designator, buffer: &mut String) {
        match designator {
            Designator::Identifier(identifier) => buffer.push_str(&identifier.to_string()),
            _ => unimplemented!(),
        }
    }
}
