use crate::ast::token_range::WithTokenSpan;
use crate::ast::{CallOrIndexed, ExternalName, ExternalPath};
use crate::formatting::VHDLFormatter;
use crate::syntax::Kind;
use crate::{TokenAccess, TokenSpan};
use vhdl_lang::ast::{AttributeName, Name};

impl VHDLFormatter<'_> {
    pub fn format_name(&self, name: WithTokenSpan<&Name>, buffer: &mut String) {
        use Name::*;
        let span = name.span;
        match &name.item {
            Designator(_) => self.join_token_span(span, buffer),
            Selected(name, designator) => {
                self.format_name(name.as_ref().as_ref(), buffer);
                self.join_token_span(
                    TokenSpan::new(designator.token - 1, designator.token),
                    buffer,
                );
            }
            SelectedAll(name) => {
                self.format_name(name.as_ref().as_ref(), buffer);
                self.join_token_span(TokenSpan::new(span.end_token - 1, span.end_token), buffer);
            }
            Slice(name, range) => {
                self.format_name(name.as_ref().as_ref(), buffer);
                self.format_token_id(name.span.end_token + 1, buffer);
                self.format_discrete_range(range, buffer);
                self.format_token_id(span.end_token, buffer);
            }
            Attribute(attr_name) => self.format_attribute_name(attr_name, buffer),
            CallOrIndexed(call_or_indexed) => {
                self.format_call_or_indexed(call_or_indexed, span, buffer)
            }
            External(external) => {
                self.format_external_name(WithTokenSpan::new(external, span), buffer)
            }
        }
    }

    pub fn format_call_or_indexed(
        &self,
        call: &CallOrIndexed,
        span: TokenSpan,
        buffer: &mut String,
    ) {
        self.format_name(call.name.as_ref(), buffer);
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
        self.format_name(name.name.as_ref(), buffer);
        if let Some(signature) = &name.signature {
            self.format_signature(signature, buffer);
        }
        // '
        self.format_token_id(name.attr.token - 1, buffer);
        self.format_token_id(name.attr.token, buffer);
        if let Some(expr) = &name.expr {
            self.format_token_id(expr.span.start_token - 1, buffer);
            self.format_expression(expr.as_ref().as_ref(), buffer);
            self.format_token_id(expr.span.end_token + 1, buffer);
        }
    }

    pub fn format_external_name(&self, name: WithTokenSpan<&ExternalName>, buffer: &mut String) {
        // <<
        self.format_token_id(name.span.start_token, buffer);
        buffer.push(' ');
        // entity class
        self.format_token_id(name.span.start_token + 1, buffer);
        buffer.push(' ');
        let path = &name.item.path;
        match &path.item {
            ExternalPath::Package(name) => {
                // @
                self.format_token_id(name.span.start_token - 1, buffer);
                self.format_name(name.as_ref(), buffer)
            }
            ExternalPath::Absolute(name) => {
                // .
                self.format_token_id(name.span.start_token - 1, buffer);
                self.format_name(name.as_ref(), buffer);
            }
            ExternalPath::Relative(name, up_levels) => {
                for i in (1..=*up_levels).rev() {
                    // ^
                    self.format_token_id(name.span.start_token - (2 * i), buffer);
                    // .
                    self.format_token_id(name.span.start_token - (2 * i - 1), buffer);
                }
                self.format_name(name.as_ref(), buffer)
            }
        }
        buffer.push(' ');
        self.format_token_id(name.item.colon_token, buffer);
        buffer.push(' ');
        self.format_subtype_indication(&name.item.subtype, buffer);
        buffer.push(' ');
        // >>
        self.format_token_id(name.span.end_token, buffer);
    }
}

#[cfg(test)]
pub mod tests {
    use crate::syntax::test::Code;
    use vhdl_lang::formatting::test_utils::check_formatted;

    pub fn check_name(input: &str) {
        check_formatted(input, input, Code::name, |formatter, ast, buffer| {
            formatter.format_name(ast.as_ref(), buffer)
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

    #[test]
    fn external_names() {
        check_name("<< signal dut.gen(0) : std_logic >>");
        check_name("<< signal .dut.gen(0) : std_logic >>");
        check_name("<< signal @dut.gen(0) : std_logic >>");
        check_name("<< signal ^.dut.gen(0) : std_logic >>");
        check_name("<< signal ^.^.^.dut.gen(0) : std_logic >>");
    }
}
