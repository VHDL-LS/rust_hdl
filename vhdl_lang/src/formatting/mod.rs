use crate::Token;
use std::cell::Cell;
use std::iter;

mod architecture;
mod concurrent_statement;
mod constraint;
mod context;
mod declaration;
mod design;
mod entity;
mod expression;
mod interface;
mod name;
mod sequential_statement;
mod subprogram;
mod token;

struct Formatter {}

struct FormatterConfig {
    indent_char: char,
    indent_size: usize,
}

pub(crate) struct DesignUnitFormatter<'b> {
    tokens: &'b Vec<Token>,
    indentation: Cell<usize>,
    config: FormatterConfig,
}

impl<'b> DesignUnitFormatter<'b> {
    pub fn new(tokens: &'b Vec<Token>) -> DesignUnitFormatter<'b> {
        DesignUnitFormatter {
            tokens,
            indentation: Cell::new(0),
            config: FormatterConfig {
                indent_char: ' ',
                indent_size: 4,
            },
        }
    }
}

impl DesignUnitFormatter<'_> {
    pub fn newline(&self, buffer: &mut String) {
        buffer.push('\n');
        buffer.extend(
            iter::repeat(self.config.indent_char)
                .take(self.config.indent_size * self.indentation.get()),
        );
    }

    pub fn increase_indentation(&self) {
        self.indentation.replace(self.indentation.get() + 1);
    }

    pub fn decrease_indentation(&self) {
        self.indentation.replace(self.indentation.get() - 1);
    }
}

#[cfg(test)]
pub mod test_utils {
    use crate::formatting::DesignUnitFormatter;
    use crate::syntax::test::Code;
    use vhdl_lang::VHDLStandard;

    pub(crate) fn check_formatted<T>(
        input: &str,
        expected: &str,
        to_ast: impl FnOnce(&Code) -> T,
        format: impl FnOnce(&DesignUnitFormatter, &T, &mut String),
    ) {
        check_formatted_std(input, expected, VHDLStandard::default(), to_ast, format)
    }

    pub(crate) fn check_formatted_std<T>(
        input: &str,
        expected: &str,
        std: VHDLStandard,
        to_ast: impl FnOnce(&Code) -> T,
        format: impl FnOnce(&DesignUnitFormatter, &T, &mut String),
    ) {
        let code = Code::with_standard(input, std);
        let ast_element = to_ast(&code);
        let tokens = code.tokenize();
        let formatter = DesignUnitFormatter::new(&tokens);
        let mut buffer = String::new();
        format(&formatter, &ast_element, &mut buffer);
        assert_eq!(&buffer, expected);
    }
}
