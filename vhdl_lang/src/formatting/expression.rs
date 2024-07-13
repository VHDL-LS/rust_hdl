use crate::ast::token_range::WithTokenSpan;
use crate::ast::{Expression, ResolutionIndication, SubtypeIndication};
use crate::formatting::DesignUnitFormatter;
use crate::syntax::Kind;
use crate::TokenAccess;
use vhdl_lang::TokenSpan;

impl DesignUnitFormatter<'_> {
    pub fn format_expression(&self, expression: &Expression, span: TokenSpan, buffer: &mut String) {
        use Expression::*;
        let reduced_span: TokenSpan = if self.tokens.get_token(span.start_token).kind
            == Kind::LeftPar
            && self.tokens.get_token(span.end_token).kind == Kind::RightPar
        {
            self.format_token_id(span.start_token, buffer);
            TokenSpan::new(span.start_token + 1, span.end_token - 1)
        } else {
            span
        };
        match &expression {
            Name(name) => self.format_name(name, reduced_span, buffer),
            Literal(_) => self.format_token_span(reduced_span, buffer),
            Binary(op, lhs, rhs) => {
                self.format_expression(&lhs.item, lhs.span, buffer);
                buffer.push(' ');
                self.format_token_id(op.token, buffer);
                buffer.push(' ');
                self.format_expression(&rhs.item, rhs.span, buffer);
            }
            Unary(op, rhs) => {
                self.format_token_id(op.token, buffer);
                self.format_expression(&rhs.item, rhs.span, buffer);
            }
            _ => unimplemented!(),
        }
        if self.tokens.get_token(span.end_token).kind == Kind::RightPar {
            self.format_token_id(span.end_token, buffer);
        }
    }

    pub fn format_subtype(&self, subtype: &SubtypeIndication, buffer: &mut String) {
        match &subtype.resolution {
            ResolutionIndication::Unresolved => {}
            _ => unimplemented!(),
        }
        self.format_name(&subtype.type_mark.item, subtype.type_mark.span, buffer);
        if let Some(constraint) = &subtype.constraint {
            unimplemented!();
        }
    }

    // Helper to format ` := <expression>`
    pub(crate) fn format_default_expression(
        &self,
        expression: Option<&WithTokenSpan<Expression>>,
        buffer: &mut String,
    ) {
        if let Some(expr) = expression {
            buffer.push(' ');
            self.format_token_id(expr.span.start_token - 1, buffer);
            buffer.push(' ');
            self.format_expression(&expr.item, expr.span, buffer);
        }
    }
}

#[cfg(test)]
mod test {
    use crate::analysis::tests::Code;
    use crate::formatting::DesignUnitFormatter;

    fn check_expression(input: &str, expected: &str) {
        let code = Code::new(input);
        let expression = code.expr();
        let tokens = code.tokenize();
        let formatter = DesignUnitFormatter::new(&tokens);
        let mut buffer = String::new();
        formatter.format_expression(&expression.item, code.token_span(), &mut buffer);
        assert_eq!(&buffer, expected);
    }

    #[test]
    fn test_simple_expression() {
        check_expression("name", "name")
    }

    #[test]
    fn test_parenthesized_expression() {
        check_expression("(name)", "(name)")
    }

    #[test]
    fn formal_literal() {
        check_expression("12387.44e7", "12387.44e7");
        check_expression("7", "7");
    }

    #[test]
    fn binary_expressions() {
        check_expression("1 + B", "1 + B");
        check_expression("2 sll 2", "2 sll 2");
    }

    #[test]
    fn unary_expressions() {
        check_expression("+ B", "+B");
        check_expression("-2", "-2");
    }

    #[test]
    fn complex_expression() {
        check_expression("A + B - C", "A + B - C");
        check_expression("(A * B) + C", "(A * B) + C");
        check_expression("((A * B) + C)", "((A * B) + C)");
    }
}
