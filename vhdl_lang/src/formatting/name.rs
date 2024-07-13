use crate::formatting::DesignUnitFormatter;
use crate::TokenSpan;
use vhdl_lang::ast::{AttributeName, Name};

impl DesignUnitFormatter<'_> {
    pub fn format_name(&self, name: &Name, span: TokenSpan, buffer: &mut String) {
        use Name::*;
        match name {
            Designator(_) => self.join_token_span(span, buffer),
            Attribute(attr_name) => self.format_attribute_name(attr_name, span, buffer),
            _ => unimplemented!(),
        }
    }

    pub fn format_attribute_name(
        &self,
        _name: &AttributeName,
        span: TokenSpan,
        buffer: &mut String,
    ) {
        unimplemented!()
    }
}
