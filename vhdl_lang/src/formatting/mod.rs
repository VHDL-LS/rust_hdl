use crate::Token;
use std::cell::Cell;
use std::iter;

mod architecture;
mod context;
mod declaration;
mod design;
mod entity;
mod expression;
mod interface;
mod name;
mod statement;
mod token;

struct Formatter {}

struct FormatterConfig {
    indent_char: char,
    indent_size: usize,
}

struct DesignUnitFormatter<'b> {
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
