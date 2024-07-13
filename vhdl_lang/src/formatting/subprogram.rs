use crate::ast::{SubprogramDeclaration, SubprogramSpecification};
use crate::formatting::DesignUnitFormatter;
use crate::HasTokenSpan;
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
        // procedure
        self.format_token_id(specification.span.start_token, buffer);
        buffer.push(' ');
        self.format_token_id(specification.designator.tree.token, buffer);
        if specification.header.is_some() {
            unimplemented!()
        }
        if let Some(tok) = specification.param_tok {
            self.format_token_id(tok, buffer);
        }
        for parameter in &specification.parameter_list {
            unimplemented!()
        }
    }

    pub fn format_function_specification(
        &self,
        specification: &FunctionSpecification,
        buffer: &mut String,
    ) {
        // function
        self.format_token_id(specification.span.start_token, buffer);
        buffer.push(' ');
        self.format_token_id(specification.designator.tree.token, buffer);
        if specification.header.is_some() {
            unimplemented!()
        }
        if let Some(tok) = specification.param_tok {
            self.format_token_id(tok, buffer);
        }
        for parameter in &specification.parameter_list {
            unimplemented!()
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
}
