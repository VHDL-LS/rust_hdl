// This Source Code Form is subject to the terms of the Mozilla Public
// Lic// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// This Source Code Form is subject to the terms of the Mozilla Public
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2024, Olof Kraigher olof.kraigher@gmail.com

use crate::ast::DesignFile;
use crate::formatting::buffer::Buffer;
use crate::syntax::Kind;
use crate::{Token, TokenAccess};
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
mod statement;
mod subprogram;
mod token;

/// The formatter is the main entry point used for formatting a single
/// Design Unit from AST representation to string representation. In that sense,
/// the Formatter is the inverse to the Parser.
///
/// Most methods herein are called `format_<node>` where `node` is the AST node to format.
/// Rather than returning a string, the methods accept a mutable [Buffer] object that they
/// use to format.
///
/// The formatter is capable of retaining comment information as well as preserving newlines.
pub struct VHDLFormatter<'b> {
    tokens: &'b Vec<Token>,
}

impl<'b> VHDLFormatter<'b> {
    pub fn new(tokens: &'b Vec<Token>) -> VHDLFormatter<'b> {
        VHDLFormatter { tokens }
    }

    /// Format a whole design file.
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
        result.into()
    }
}

impl VHDLFormatter<'_> {
    pub fn format_ident_list<T: HasIdent>(&self, idents: &[T], buffer: &mut Buffer) {
        for ident in idents {
            let token = ident.ident().token;
            self.format_token_id(token, buffer);
            if self
                .tokens
                .get_token(token + 1)
                .is_some_and(|token| token.kind == Kind::Comma)
            {
                self.format_token_id(token + 1, buffer);
                buffer.push_whitespace();
            }
        }
    }
}

/// indents the provided block and de-indents at the end.
#[macro_export]
macro_rules! indented {
    ($buffer:ident, $block:block) => {
        $buffer.increase_indent();
        $block
        $buffer.decrease_indent();
    };
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
