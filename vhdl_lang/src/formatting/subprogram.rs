use crate::ast::token_range::WithTokenSpan;
use crate::ast::{Signature, SubprogramDeclaration, SubprogramSpecification};
use crate::formatting::DesignUnitFormatter;
use crate::{HasTokenSpan, TokenSpan};
use vhdl_lang::ast::{FunctionSpecification, ProcedureSpecification, SubprogramBody};

impl DesignUnitFormatter<'_> {
    pub fn format_subprogram_declaration(
        &self,
        declaration: &SubprogramDeclaration,
        buffer: &mut String,
    ) {
        self.format_subprogram_specification(&declaration.specification, buffer);
        // ;
        self.format_token_id(declaration.span.end_token, buffer);
    }

    pub fn format_subprogram_specification(
        &self,
        specification: &SubprogramSpecification,
        buffer: &mut String,
    ) {
        use SubprogramSpecification::*;
        match specification {
            Procedure(procedure_spec) => {
                self.format_procedure_specification(procedure_spec, buffer)
            }
            Function(function_spec) => self.format_function_specification(function_spec, buffer),
        }
    }

    pub fn format_procedure_specification(
        &self,
        specification: &ProcedureSpecification,
        buffer: &mut String,
    ) {
        // procedure <name>
        self.format_token_span(
            TokenSpan::new(
                specification.span.start_token,
                specification.designator.tree.token,
            ),
            buffer,
        );
        if specification.header.is_some() {
            unimplemented!()
        }
        for parameter in &specification.parameter_list {
            self.format_interface_list(parameter, buffer);
        }
    }

    pub fn format_function_specification(
        &self,
        specification: &FunctionSpecification,
        buffer: &mut String,
    ) {
        // function
        self.format_token_span(
            TokenSpan::new(
                specification.span.start_token,
                specification.designator.tree.token,
            ),
            buffer,
        );
        if specification.header.is_some() {
            unimplemented!()
        }
        for parameter in &specification.parameter_list {
            self.format_interface_list(parameter, buffer);
        }
        buffer.push(' ');
        // return
        self.format_token_id(specification.return_type.span.start_token - 1, buffer);
        buffer.push(' ');
        self.format_name(
            &specification.return_type.item,
            specification.return_type.span,
            buffer,
        );
    }

    pub fn format_subprogram_body(&self, body: &SubprogramBody, buffer: &mut String) {
        self.format_subprogram_specification(&body.specification, buffer);
        buffer.push(' ');
        // is
        self.format_token_id(body.specification.span().end_token + 1, buffer);
        self.newline(buffer);
        self.increase_indentation();
        self.format_declarations(&body.declarations, buffer);
        self.decrease_indentation();
        self.format_token_id(body.begin_token, buffer);
        self.increase_indentation();
        if !body.statements.is_empty() {
            unimplemented!()
        }
        self.decrease_indentation();
        self.newline(buffer);
        // end
        self.format_token_id(body.span.end_token - 1, buffer);
        // ;
        self.format_token_id(body.span.end_token, buffer);
    }

    pub fn format_signature(&self, signature: &WithTokenSpan<Signature>, buffer: &mut String) {
        self.format_token_id(signature.span.start_token, buffer);
        match &signature.item {
            Signature::Function(functions, return_type) => {
                for (i, function) in functions.iter().enumerate() {
                    self.format_name(&function.item, function.span, buffer);
                    if i < functions.len() - 1 {
                        // ,
                        self.format_token_id(function.span.end_token + 1, buffer);
                    }
                    buffer.push(' ');
                }
                // return
                self.format_token_id(return_type.span.start_token - 1, buffer);
                buffer.push(' ');
                self.format_name(&return_type.item, return_type.span, buffer);
            }
            Signature::Procedure(procedures) => {
                for (i, procedure) in procedures.iter().enumerate() {
                    self.format_name(&procedure.item, procedure.span, buffer);
                    if i < procedures.len() - 1 {
                        // ,
                        self.format_token_id(procedure.span.end_token + 1, buffer);
                        buffer.push(' ');
                    }
                }
            }
        }
        self.format_token_id(signature.span.end_token, buffer);
    }
}

#[cfg(test)]
mod test {
    use crate::formatting::test_utils::check_formatted;

    fn check_signature(input: &str) {
        check_formatted(
            input,
            input,
            |code| code.signature(),
            |formatter, ast, buffer| formatter.format_signature(ast, buffer),
        );
    }

    #[test]
    fn test_signature() {
        check_signature("[return type_mark]");
        check_signature("[foo return bar]");
        check_signature("[type_mark]");
        check_signature("[foo, foo2 return bar]");
    }

    fn check_subprogram_declaration(input: &str) {
        check_formatted(
            input,
            input,
            |code| code.subprogram_decl(),
            |formatter, ast, buffer| formatter.format_subprogram_declaration(ast, buffer),
        );
    }

    #[test]
    fn test_subprogram_declaration_without_parameters() {
        check_subprogram_declaration("procedure foo;");
        check_subprogram_declaration("function foo return natural;");
        check_subprogram_declaration("function \"+\" return natural;");
        check_subprogram_declaration("impure function foo return natural;");
        check_subprogram_declaration("pure function foo return natural;");
    }
}
