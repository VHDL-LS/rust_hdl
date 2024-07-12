use crate::ast::{Expression, ResolutionIndication, SubtypeIndication};
use crate::formatting::DesignUnitFormatter;
use crate::syntax::Kind;
use crate::TokenAccess;
use vhdl_lang::ast::token_range::WithTokenSpan;
use vhdl_lang::TokenSpan;

impl DesignUnitFormatter<'_> {
    pub fn format_expression(&self, expression: &WithTokenSpan<Expression>, buffer: &mut String) {
        use Expression::*;
        let span = expression.span;
        let reduced_span: TokenSpan =
            if self.tokens.get_token(span.start_token).kind == Kind::LeftPar {
                self.format_token_id(span.start_token, buffer);
                TokenSpan::new(span.start_token + 1, span.end_token - 1)
            } else {
                span
            };
        match &expression.item {
            Name(name) => self.format_name(name, reduced_span, buffer),
            Literal(_) => self.format_token_span(reduced_span, buffer),
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
        formatter.format_expression(&expression, &mut buffer);
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
}
