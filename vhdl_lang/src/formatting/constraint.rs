use crate::ast::token_range::WithTokenSpan;
use crate::ast::{DiscreteRange, SubtypeConstraint};
use crate::formatting::VHDLFormatter;
use crate::TokenSpan;
use vhdl_lang::ast::{Range, RangeConstraint};

impl VHDLFormatter<'_> {
    // TODO: has bugs + not tested properly
    pub fn format_subtype_constraint(
        &self,
        constraint: &WithTokenSpan<SubtypeConstraint>,
        buffer: &mut String,
    ) {
        self.format_token_id(constraint.span.start_token, buffer);
        let span = TokenSpan::new(
            constraint.span.start_token + 1,
            constraint.span.end_token - 1,
        );
        match &constraint.item {
            SubtypeConstraint::Range(range) => self.format_range(range, buffer),
            SubtypeConstraint::Array(ranges, opt_constraint) => {
                for range in ranges {
                    self.format_discrete_range(range, buffer);
                }
            }
            SubtypeConstraint::Record(_) => unimplemented!(),
        }
        self.format_token_id(constraint.span.end_token, buffer);
    }

    pub fn format_range_constraint(&self, constraint: &RangeConstraint, buffer: &mut String) {
        self.format_expression(constraint.left_expr.as_ref().as_ref(), buffer);
        buffer.push(' ');
        self.format_token_id(constraint.direction_token(), buffer);
        buffer.push(' ');
        self.format_expression(constraint.right_expr.as_ref().as_ref(), buffer);
    }

    pub fn format_range(&self, range: &Range, buffer: &mut String) {
        match range {
            Range::Range(constraint) => self.format_range_constraint(constraint, buffer),
            Range::Attribute(attribute) => self.format_attribute_name(attribute, buffer),
        }
    }

    pub fn format_discrete_range(&self, range: &DiscreteRange, buffer: &mut String) {
        match range {
            DiscreteRange::Discrete(name, range) => {
                self.format_name(name.as_ref(), buffer);
                buffer.push(' ');
                if let Some(range) = range {
                    // range
                    self.format_token_id(name.span.end_token + 1, buffer);
                    buffer.push(' ');
                    self.format_range(range, buffer);
                }
            }
            DiscreteRange::Range(range) => self.format_range(range, buffer),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::formatting::VHDLFormatter;
    use crate::syntax::test::Code;

    fn check_range(input: &str) {
        let code = Code::new(input);
        let range = code.range();
        let tokens = code.tokenize();
        let formatter = VHDLFormatter::new(&tokens);
        let mut buffer = String::new();
        formatter.format_range(&range, &mut buffer);
        assert_eq!(&buffer, input);
    }

    #[test]
    fn check_simple_range() {
        check_range("0 to 5");
        check_range("0 downto 5 - C_OFFSET");
    }
}
