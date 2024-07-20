use crate::ast::DesignFile;
use crate::formatting::buffer::Buffer;
use crate::syntax::Kind;
use crate::{Token, TokenAccess};
use std::cell::Cell;
use vhdl_lang::ast::HasIdent;

mod architecture;
mod buffer;
mod concurrent_statement;
mod configuration;
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
    let mut result = Buffer::new();
    for (i, (tokens, design_unit)) in file.design_units.iter().enumerate() {
        let formatter = VHDLFormatter::new(tokens);
        formatter.format_any_design_unit(
            design_unit,
            &mut result,
            i == file.design_units.len() - 1,
        );
    }
    result.into_string()
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
    pub fn increase_indentation(&self) {
        self.indentation.replace(self.indentation.get() + 1);
    }

    pub fn decrease_indentation(&self) {
        self.indentation.replace(self.indentation.get() - 1);
    }

    pub fn format_ident_list<T: HasIdent>(&self, idents: &[T], buffer: &mut Buffer) {
        for ident in idents {
            let token = ident.ident().token;
            self.format_token_id(token, buffer);
            if self.tokens.get_token(token + 1).kind == Kind::Comma {
                self.format_token_id(token + 1, buffer);
                buffer.push_whitespace();
            }
        }
    }
}

#[cfg(test)]
pub mod test_utils {
    use crate::formatting::buffer::Buffer;
    use crate::formatting::VHDLFormatter;
    use crate::syntax::test::Code;
    use vhdl_lang::VHDLStandard;

    pub(crate) fn check_formatted<T>(
        input: &str,
        expected: &str,
        to_ast: impl FnOnce(&Code) -> T,
        format: impl FnOnce(&VHDLFormatter, &T, &mut Buffer),
    ) {
        check_formatted_std(input, expected, VHDLStandard::default(), to_ast, format)
    }

    pub(crate) fn check_formatted_std<T>(
        input: &str,
        expected: &str,
        std: VHDLStandard,
        to_ast: impl FnOnce(&Code) -> T,
        format: impl FnOnce(&VHDLFormatter, &T, &mut Buffer),
    ) {
        let code = Code::with_standard(input, std);
        let ast_element = to_ast(&code);
        let tokens = code.tokenize();
        let formatter = VHDLFormatter::new(&tokens);
        let mut buffer = Buffer::new();
        format(&formatter, &ast_element, &mut buffer);
        assert_eq!(buffer.as_str(), expected);
    }
}
