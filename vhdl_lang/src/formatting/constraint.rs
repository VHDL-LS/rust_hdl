use crate::ast::token_range::WithTokenSpan;
use crate::ast::SubtypeConstraint;
use crate::formatting::DesignUnitFormatter;
use crate::TokenSpan;
use vhdl_lang::ast::{Range, RangeConstraint};

impl DesignUnitFormatter<'_> {
    pub fn format_subtype_constraint(
        &self,
        constraint: &WithTokenSpan<SubtypeConstraint>,
        buffer: &mut String,
    ) {
        unimplemented!()
    }

    pub fn format_range_constraint(&self, constraint: &RangeConstraint, buffer: &mut String) {
        self.format_expression(
            &constraint.left_expr.item,
            constraint.left_expr.span,
            buffer,
        );
        buffer.push(' ');
        self.format_token_id(constraint.direction_token(), buffer);
        buffer.push(' ');
        self.format_expression(
            &constraint.right_expr.item,
            constraint.right_expr.span,
            buffer,
        );
    }

    pub fn format_range(&self, range: &Range, span: TokenSpan, buffer: &mut String) {
        match range {
            Range::Range(constraint) => self.format_range_constraint(constraint, buffer),
            Range::Attribute(attribute) => self.format_attribute_name(attribute, span, buffer),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::formatting::DesignUnitFormatter;
    use crate::syntax::test::Code;

    fn check_range(input: &str, expected: &str) {
        let code = Code::new(input);
        let range = code.range();
        let tokens = code.tokenize();
        let formatter = DesignUnitFormatter::new(&tokens);
        let mut buffer = String::new();
        formatter.format_range(&range, code.token_span(), &mut buffer);
        assert_eq!(&buffer, expected);
    }

    #[test]
    fn check_simple_range() {
        check_range("0 to 5", "0 to 5");
        check_range("0 downto 5 - C_OFFSET", "0 downto 5 - C_OFFSET");
    }
}
