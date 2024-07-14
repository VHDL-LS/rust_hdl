use crate::ast::token_range::WithTokenSpan;
use crate::ast::CallOrIndexed;
use crate::formatting::DesignUnitFormatter;
use crate::syntax::Kind;
use crate::{TokenAccess, TokenSpan};
use vhdl_lang::ast::{AttributeName, Name};

impl DesignUnitFormatter<'_> {
    pub fn format_name(&self, name: &Name, span: TokenSpan, buffer: &mut String) {
        use Name::*;
        match name {
            Designator(_) => self.join_token_span(span, buffer),
            Attribute(attr_name) => self.format_attribute_name(attr_name, span, buffer),
            CallOrIndexed(call_or_indexed) => {
                self.format_call_or_indexed(call_or_indexed, span, buffer)
            }
            _ => unimplemented!(),
        }
    }

    pub fn format_call_or_indexed(
        &self,
        call: &CallOrIndexed,
        span: TokenSpan,
        buffer: &mut String,
    ) {
        self.format_name(&call.name.item, call.name.span, buffer);
        let open_paren = call.name.span.end_token + 1;
        if self.tokens.get_token(open_paren).kind == Kind::LeftPar {
            self.format_token_id(open_paren, buffer);
        }
        for (i, parameter) in call.parameters.items.iter().enumerate() {
            self.format_association_element(parameter, buffer);
            if let Some(token) = call.parameters.tokens.get(i) {
                self.format_token_id(*token, buffer);
                buffer.push(' ');
            }
        }
        let close_paren = span.end_token;
        if self.tokens.get_token(close_paren).kind == Kind::RightPar {
            self.format_token_id(close_paren, buffer);
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
