use crate::ast::token_range::WithTokenSpan;
use crate::ast::{DiscreteRange, SubtypeConstraint};
use crate::formatting::VHDLFormatter;
use crate::syntax::Kind;
use crate::TokenAccess;
use vhdl_lang::ast::{ElementConstraint, Range, RangeConstraint};

impl VHDLFormatter<'_> {
    pub fn format_subtype_constraint(
        &self,
        constraint: &WithTokenSpan<SubtypeConstraint>,
        buffer: &mut String,
    ) {
        match &constraint.item {
            SubtypeConstraint::Range(range) => {
                self.format_token_id(constraint.span.start_token, buffer);
                buffer.push(' ');
                self.format_range(range, buffer)
            }
            SubtypeConstraint::Array(ranges, opt_constraint) => {
                self.format_token_id(constraint.span.start_token, buffer);
                if ranges.is_empty() {
                    // open
                    self.format_token_id(constraint.span.start_token + 1, buffer);
                }
                for range in ranges {
                    self.format_discrete_range(&range.item, buffer);
                    if self.tokens.get_token(range.span.end_token + 1).kind == Kind::Comma {
                        self.format_token_id(range.span.end_token + 1, buffer);
                        buffer.push(' ');
                    }
                }
                if let Some(constraint) = opt_constraint {
                    self.format_token_id(constraint.span.start_token - 1, buffer);
                    self.format_subtype_constraint(constraint, buffer);
                } else {
                    self.format_token_id(constraint.span.end_token, buffer);
                }
            }
            SubtypeConstraint::Record(records) => {
                self.format_token_id(constraint.span.start_token, buffer);
                for record in records {
                    self.format_element_constraint(record, buffer);
                    if self
                        .tokens
                        .get_token(record.constraint.span.end_token + 1)
                        .kind
                        == Kind::Comma
                    {
                        self.format_token_id(record.constraint.span.end_token + 1, buffer);
                        buffer.push(' ');
                    }
                }
                self.format_token_id(constraint.span.end_token, buffer);
            }
        }
    }

    pub fn format_element_constraint(&self, constraint: &ElementConstraint, buffer: &mut String) {
        self.format_token_id(constraint.ident.token, buffer);
        self.format_subtype_constraint(&constraint.constraint, buffer);
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
                if let Some(range) = range {
                    buffer.push(' ');
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
    use crate::formatting::test_utils::check_formatted;
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

    fn check_subtype_indications(inputs: &[&str]) {
        for input in inputs {
            check_formatted(
                input,
                input,
                Code::subtype_indication,
                |formatter, subtype_indication, buffer| {
                    formatter.format_subtype_indication(subtype_indication, buffer)
                },
            )
        }
    }

    #[test]
    fn format_range_subtype_constraint() {
        check_subtype_indications(&[
            "integer range 0 to 2 - 1",
            "integer range lib.foo.bar'range",
        ]);
    }

    #[test]
    fn format_array_subtype_constraint() {
        check_subtype_indications(&[
            "integer_vector(2 - 1 downto 0)",
            "integer_vector(lib.foo.bar)",
            "integer_vector(lib.pkg.bar'range)",
            "integer_vector(open)",
            "integer_vector(2 - 1 downto 0, 11 to 14)",
            "integer_vector(2 - 1 downto 0, 11 to 14)(foo to bar)",
        ]);
    }

    #[test]
    fn format_record_subtype_constraint() {
        check_subtype_indications(&["axi_m2s_t(tdata(2 - 1 downto 0), tuser(3 to 5))"]);
    }
}
