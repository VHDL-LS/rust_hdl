use crate::ast::token_range::WithTokenSpan;
use crate::ast::{
    ElementAssociation, Expression, Operator, ResolutionIndication, SubtypeConstraint,
    SubtypeIndication,
};
use crate::formatting::buffer::Buffer;
use crate::formatting::VHDLFormatter;
use crate::syntax::Kind;
use crate::{HasTokenSpan, Token, TokenAccess};
use vhdl_lang::ast::{Allocator, QualifiedExpression};
use vhdl_lang::TokenSpan;

impl VHDLFormatter<'_> {
    pub fn format_expression(&self, expression: WithTokenSpan<&Expression>, buffer: &mut Buffer) {
        let span = expression.span;
        use Expression::*;
        let is_parenthesized = self.tokens.get_token(span.start_token).kind == Kind::LeftPar
            && self.tokens.get_token(span.end_token).kind == Kind::RightPar;
        let reduced_span: TokenSpan = if is_parenthesized {
            self.format_token_id(span.start_token, buffer);
            TokenSpan::new(span.start_token + 1, span.end_token - 1)
        } else {
            span
        };
        match &expression.item {
            Binary(op, lhs, rhs) => {
                self.format_expression(lhs.as_ref().as_ref(), buffer);
                buffer.push_whitespace();
                self.format_token_id(op.token, buffer);
                buffer.push_whitespace();
                self.format_expression(rhs.as_ref().as_ref(), buffer);
            }
            Unary(op, rhs) => {
                self.format_token_id(op.token, buffer);
                match op.item.item {
                    Operator::Minus | Operator::Plus | Operator::QueQue => {
                        // Leave as unary operator without whitespace
                    }
                    _ => buffer.push_whitespace(),
                }
                self.format_expression(rhs.as_ref().as_ref(), buffer);
            }
            Aggregate(aggregate) => self.format_element_associations(aggregate, buffer),
            Qualified(qualified_expr) => self.format_qualified_expression(qualified_expr, buffer),
            Name(name) => self.format_name(WithTokenSpan::new(name, reduced_span), buffer),
            Literal(_) => self.format_token_span(reduced_span, buffer),
            New(allocator) => self.format_allocator(allocator, buffer),
        }
        if is_parenthesized {
            self.format_token_id(span.end_token, buffer);
        }
    }

    pub fn format_element_associations(
        &self,
        associations: &[WithTokenSpan<ElementAssociation>],
        buffer: &mut Buffer,
    ) {
        for (i, association) in associations.iter().enumerate() {
            match &association.item {
                ElementAssociation::Positional(expression) => {
                    self.format_expression(expression.as_ref(), buffer)
                }
                ElementAssociation::Named(choices, expression) => {
                    for (j, choice) in choices.iter().enumerate() {
                        self.format_choice(choice, buffer);
                        if j < choices.len() - 1 {
                            buffer.push_whitespace();
                            self.format_token_id(choice.span.end_token + 1, buffer);
                            buffer.push_whitespace();
                        }
                    }
                    buffer.push_whitespace();
                    self.format_token_id(expression.span.start_token - 1, buffer);
                    buffer.push_whitespace();
                    self.format_expression(expression.as_ref(), buffer);
                }
            }
            if i < associations.len() - 1 {
                self.format_token_id(association.span.end_token + 1, buffer);
                buffer.push_whitespace();
            }
        }
    }

    pub fn format_subtype_indication(&self, indication: &SubtypeIndication, buffer: &mut Buffer) {
        if let Some(resolution) = &indication.resolution {
            self.format_resolution_indication(resolution, buffer);
            buffer.push_whitespace();
        }
        self.format_name(indication.type_mark.as_ref(), buffer);
        if let Some(constraint) = &indication.constraint {
            if matches!(constraint.item, SubtypeConstraint::Range(_)) {
                buffer.push_whitespace();
            }
            self.format_subtype_constraint(constraint, buffer)
        }
    }

    pub fn format_resolution_indication(
        &self,
        indication: &ResolutionIndication,
        buffer: &mut Buffer,
    ) {
        match &indication {
            ResolutionIndication::FunctionName(name) => self.format_name(name.as_ref(), buffer),
            ResolutionIndication::ArrayElement(element) => {
                self.format_token_id(element.span.start_token - 1, buffer);
                self.format_name(element.as_ref(), buffer);
                self.format_token_id(element.span.end_token + 1, buffer);
            }
            ResolutionIndication::Record(record) => {
                let span = record.span;
                self.format_token_id(span.start_token, buffer);
                for (i, element_resolution) in record.item.iter().enumerate() {
                    self.format_token_id(element_resolution.ident.token, buffer);
                    buffer.push_whitespace();
                    self.format_resolution_indication(&element_resolution.resolution, buffer);
                    if i < record.item.len() - 1 {
                        // ,
                        self.format_token_id(
                            element_resolution.resolution.get_end_token() + 1,
                            buffer,
                        );
                        buffer.push_whitespace();
                    }
                }
                self.format_token_id(span.end_token, buffer);
            }
        }
    }

    // Helper to format ` := <expression>`
    pub(crate) fn format_default_expression(
        &self,
        expression: Option<&WithTokenSpan<Expression>>,
        buffer: &mut Buffer,
    ) {
        if let Some(expr) = expression {
            buffer.push_whitespace();
            self.format_token_id(expr.span.start_token - 1, buffer);
            buffer.push_whitespace();
            self.format_expression(expr.as_ref(), buffer);
        }
    }

    pub fn format_qualified_expression(
        &self,
        expression: &QualifiedExpression,
        buffer: &mut Buffer,
    ) {
        self.format_name(expression.type_mark.as_ref(), buffer);
        // '
        self.format_token_id(expression.type_mark.span.end_token + 1, buffer);
        self.format_expression(expression.expr.as_ref(), buffer);
    }

    pub fn format_allocator(&self, allocator: &WithTokenSpan<Allocator>, buffer: &mut Buffer) {
        // new
        self.format_token_id(allocator.span.start_token - 1, buffer);
        buffer.push_whitespace();
        match &allocator.item {
            Allocator::Qualified(expr) => self.format_qualified_expression(expr, buffer),
            Allocator::Subtype(subtype) => self.format_subtype_indication(subtype, buffer),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::analysis::tests::Code;
    use crate::ast::token_range::WithTokenSpan;
    use crate::formatting::VHDLFormatter;
    use vhdl_lang::formatting::buffer::Buffer;
    use vhdl_lang::formatting::test_utils::check_formatted;

    fn check_expression(input: &str) {
        let code = Code::new(input);
        let expression = code.expr();
        let tokens = code.tokenize();
        let formatter = VHDLFormatter::new(&tokens);
        let mut buffer = Buffer::new();
        formatter.format_expression(
            WithTokenSpan::new(&expression.item, code.token_span()),
            &mut buffer,
        );
        assert_eq!(buffer.as_str(), input);
    }

    #[test]
    fn test_simple_expression() {
        check_expression("name")
    }

    #[test]
    fn test_parenthesized_expression() {
        check_expression("(name)")
    }

    #[test]
    fn formal_literal() {
        check_expression("12387.44e7");
        check_expression("7");
    }

    #[test]
    fn binary_expressions() {
        check_expression("1 + B");
        check_expression("2 sll 2");
    }

    #[test]
    fn unary_expressions() {
        check_expression("+B");
        check_expression("-2");
        check_expression("not A")
    }

    #[test]
    fn complex_expression() {
        check_expression("A + B - C");
        check_expression("(A * B) + C");
        check_expression("((A * B) + C)");
    }

    #[test]
    fn aggregate() {
        check_expression("(1, 2)");
        check_expression("(1 => 2, 3)");
        check_expression("(others => 1, others => 2)");
        check_expression("(1 downto 0 => 2)");
        check_expression("(0 to 1 => 2)");
        check_expression("(1 | 2 => 3)");
    }

    #[test]
    fn qualified_expressions() {
        check_expression("integer_vector'(0, 1)");
        check_expression("foo'(1 + 2)");
        check_expression("foo'(others => '1')");
    }

    #[test]
    fn allocator_expressions() {
        check_expression("new integer_vector'(0, 1)");
        check_expression("new integer_vector");
        check_expression("new integer_vector(0 to 1)");
        check_expression("new integer_vector(foo'range)");
    }

    fn check_subtype_indication(input: &str) {
        check_formatted(
            input,
            input,
            Code::subtype_indication,
            |formatter, subtype_indication, buffer| {
                formatter.format_subtype_indication(subtype_indication, buffer)
            },
        );
    }

    #[test]
    fn resolution_indication() {
        check_subtype_indication("resolve std_logic");
        check_subtype_indication("(resolve) integer_vector");
        check_subtype_indication("(elem resolve) rec_t");
        check_subtype_indication(
            "(elem1 (resolve1), elem2 resolve2, elem3 (sub_elem sub_resolve)) rec_t",
        );
    }

    #[test]
    fn expression_with_comments() {
        check_expression(
            "\
-- Some comment
A & -- And
B & -- as well as
C",
        )
    }
}
