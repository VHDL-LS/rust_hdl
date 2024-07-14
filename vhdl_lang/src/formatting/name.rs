use crate::ast::CallOrIndexed;
use crate::formatting::VHDLFormatter;
use crate::syntax::Kind;
use crate::{TokenAccess, TokenSpan};
use vhdl_lang::ast::{AttributeName, Name};

impl VHDLFormatter<'_> {
    pub fn format_name(&self, name: &Name, span: TokenSpan, buffer: &mut String) {
        use Name::*;
        match name {
            Designator(_) => self.join_token_span(span, buffer),
            Selected(name, designator) => {
                self.format_name(&name.item, name.span, buffer);
                self.join_token_span(
                    TokenSpan::new(designator.token - 1, designator.token),
                    buffer,
                );
            }
            SelectedAll(name) => {
                self.format_name(&name.item, name.span, buffer);
                self.join_token_span(TokenSpan::new(span.end_token - 1, span.end_token), buffer);
            }
            Slice(name, range) => {
                self.format_name(&name.item, name.span, buffer);
                self.format_token_id(name.span.end_token + 1, buffer);
                self.format_discrete_range(range, buffer);
                self.format_token_id(span.end_token, buffer);
            }
            Attribute(attr_name) => self.format_attribute_name(attr_name, buffer),
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

    pub fn format_attribute_name(&self, name: &AttributeName, buffer: &mut String) {
        self.format_name(&name.name.item, name.name.span, buffer);
        if let Some(signature) = &name.signature {
            self.format_signature(signature, buffer);
        }
        // '
        self.format_token_id(name.attr.token - 1, buffer);
        self.format_token_id(name.attr.token, buffer);
        if let Some(expr) = &name.expr {
            self.format_token_id(expr.span.start_token - 1, buffer);
            self.format_expression(&expr.item, expr.span, buffer);
            self.format_token_id(expr.span.end_token + 1, buffer);
        }
    }
}

#[cfg(test)]
pub mod tests {
    use crate::syntax::test::Code;
    use vhdl_lang::formatting::test_utils::check_formatted;

    pub fn check_name(input: &str) {
        check_formatted(input, input, Code::name, |formatter, ast, buffer| {
            formatter.format_name(&ast.item, ast.span, buffer)
        })
    }

    #[test]
    fn simple_names() {
        check_name("\"+\"");
        check_name("\"AND\"");
        check_name("\"and\"");
    }

    #[test]
    fn selected_names() {
        check_name("foo.bar.baz");
        check_name("foo.all");
    }

    #[test]
    fn slice_names() {
        check_name("prefix(0 to 3)");
        check_name("prefix(3 downto 0)");
    }

    #[test]
    fn attribute_name() {
        check_name("prefix'subtype");
        check_name("prefix'element");
        check_name("prefix'foo(expr + 1)");
        check_name("prefix[return natural]'foo(expr + 1)");
    }

    #[test]
    fn complex_names() {
        check_name("prefix(foo(0)'range)");
    }
}
