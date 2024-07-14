use crate::ast::DesignFile;
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

struct FormatterConfig {
    indent_char: char,
    indent_size: usize,
}

pub struct VHDLFormatter<'b> {
    tokens: &'b Vec<Token>,
    indentation: Cell<usize>,
    config: FormatterConfig,
}

pub fn format_design_file(file: &DesignFile) -> String {
    let mut result = String::new();
    for (i, (tokens, design_unit)) in file.design_units.iter().enumerate() {
        let formatter = VHDLFormatter::new(tokens);
        formatter.format_any_design_unit(
            design_unit,
            &mut result,
            i == file.design_units.len() - 1,
        );
    }
    result
}

impl<'b> VHDLFormatter<'b> {
    pub fn new(tokens: &'b Vec<Token>) -> VHDLFormatter<'b> {
        VHDLFormatter {
            tokens,
            indentation: Cell::new(0),
            config: FormatterConfig {
                indent_char: ' ',
                indent_size: 4,
            },
        }
    }
}

impl VHDLFormatter<'_> {
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
    use crate::formatting::VHDLFormatter;
    use crate::syntax::test::Code;
    use vhdl_lang::VHDLStandard;

    pub(crate) fn check_formatted<T>(
        input: &str,
        expected: &str,
        to_ast: impl FnOnce(&Code) -> T,
        format: impl FnOnce(&VHDLFormatter, &T, &mut String),
    ) {
        check_formatted_std(input, expected, VHDLStandard::default(), to_ast, format)
    }

    pub(crate) fn check_formatted_std<T>(
        input: &str,
        expected: &str,
        std: VHDLStandard,
        to_ast: impl FnOnce(&Code) -> T,
        format: impl FnOnce(&VHDLFormatter, &T, &mut String),
    ) {
        let code = Code::with_standard(input, std);
        let ast_element = to_ast(&code);
        let tokens = code.tokenize();
        let formatter = VHDLFormatter::new(&tokens);
        let mut buffer = String::new();
        format(&formatter, &ast_element, &mut buffer);
        assert_eq!(&buffer, expected);
    }
}
