use crate::ast::token_range::WithTokenSpan;
use crate::ast::{
    Signature, SubprogramDeclaration, SubprogramHeader, SubprogramInstantiation,
    SubprogramSpecification,
};
use crate::formatting::buffer::Buffer;
use crate::formatting::VHDLFormatter;
use crate::{HasTokenSpan, TokenSpan};
use vhdl_lang::ast::{FunctionSpecification, ProcedureSpecification, SubprogramBody};
use vhdl_lang::indented;

impl VHDLFormatter<'_> {
    pub fn format_subprogram_declaration(
        &self,
        declaration: &SubprogramDeclaration,
        buffer: &mut Buffer,
    ) {
        self.format_subprogram_specification(&declaration.specification, buffer);
        // ;
        self.format_token_id(declaration.span.end_token, buffer);
    }

    pub fn format_subprogram_specification(
        &self,
        specification: &SubprogramSpecification,
        buffer: &mut Buffer,
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
        buffer: &mut Buffer,
    ) {
        // procedure <name>
        self.format_token_span(
            TokenSpan::new(
                specification.span.start_token,
                specification.designator.tree.token,
            ),
            buffer,
        );
        if let Some(header) = &specification.header {
            self.format_subprogram_header(header, buffer);
        }
        if let Some(parameter) = &specification.parameter_list {
            if specification.header.is_some() {
                buffer.increase_indent();
                buffer.line_break();
            }
            self.format_interface_list(parameter, buffer);
            if specification.header.is_some() {
                buffer.decrease_indent();
            }
        }
    }

    pub fn format_function_specification(
        &self,
        specification: &FunctionSpecification,
        buffer: &mut Buffer,
    ) {
        // function <name>
        self.format_token_span(
            TokenSpan::new(
                specification.span.start_token,
                specification.designator.tree.token,
            ),
            buffer,
        );
        if let Some(header) = &specification.header {
            self.format_subprogram_header(header, buffer);
        }
        if let Some(parameter) = &specification.parameter_list {
            self.format_interface_list(parameter, buffer);
        }
        buffer.push_whitespace();
        // return
        self.format_token_id(specification.return_type.span.start_token - 1, buffer);
        buffer.push_whitespace();
        self.format_name(specification.return_type.as_ref(), buffer);
    }

    pub fn format_subprogram_header(&self, header: &SubprogramHeader, buffer: &mut Buffer) {
        indented!(buffer, {
            buffer.line_break();
            self.format_interface_list(&header.generic_list, buffer);
        });
        if let Some(map_aspect) = &header.map_aspect {
            buffer.push_whitespace();
            self.format_map_aspect(map_aspect, buffer);
        }
    }

    pub fn format_subprogram_body(&self, body: &SubprogramBody, buffer: &mut Buffer) {
        self.format_subprogram_specification(&body.specification, buffer);
        buffer.push_whitespace();
        // is
        self.format_token_id(body.specification.span().end_token + 1, buffer);
        buffer.line_break();
        indented!(buffer, {
            self.format_declarations(&body.declarations, buffer);
        });
        self.format_token_id(body.begin_token, buffer);
        self.format_sequential_statements(&body.statements, buffer);
        buffer.line_break();
        // end
        self.format_token_span(
            TokenSpan::new(body.end_token, body.span.end_token - 1),
            buffer,
        );
        // ;
        self.format_token_id(body.span.end_token, buffer);
    }

    pub fn format_signature(&self, signature: &WithTokenSpan<Signature>, buffer: &mut Buffer) {
        self.format_token_id(signature.span.start_token, buffer);
        match &signature.item {
            Signature::Function(functions, return_type) => {
                for (i, function) in functions.iter().enumerate() {
                    self.format_name(function.as_ref(), buffer);
                    if i < functions.len() - 1 {
                        // ,
                        self.format_token_id(function.span.end_token + 1, buffer);
                    }
                    buffer.push_whitespace();
                }
                // return
                self.format_token_id(return_type.span.start_token - 1, buffer);
                buffer.push_whitespace();
                self.format_name(return_type.as_ref(), buffer);
            }
            Signature::Procedure(procedures) => {
                self.format_name_list(buffer, procedures);
            }
        }
        self.format_token_id(signature.span.end_token, buffer);
    }

    pub fn format_subprogram_instantiation(
        &self,
        instantiation: &SubprogramInstantiation,
        buffer: &mut Buffer,
    ) {
        // function <name> is new
        self.format_token_span(
            TokenSpan::new(
                instantiation.span.start_token,
                instantiation.span.start_token + 3,
            ),
            buffer,
        );
        buffer.push_whitespace();
        self.format_name(instantiation.subprogram_name.as_ref(), buffer);
        if let Some(signature) = &instantiation.signature {
            self.format_signature(signature, buffer);
        }
        if let Some(generic_map) = &instantiation.generic_map {
            buffer.push_whitespace();
            self.format_map_aspect(generic_map, buffer);
        }
        self.format_token_id(instantiation.span.end_token, buffer);
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

    #[test]
    fn test_subprogram_declaration_one_parameter() {
        check_subprogram_declaration(
            "\
procedure foo(
    a: std_logic
);",
        );
        check_subprogram_declaration(
            "\
function foo(
    a: std_logic
) return std_logic;",
        );
    }

    #[test]
    fn test_subprogram_declaration_multiple_parameters() {
        check_subprogram_declaration(
            "\
procedure foo(
    arg0: std_logic;
    arg1: std_logic
);",
        );
    }

    #[test]
    fn test_subprogram_declaration_with_generics() {
        check_subprogram_declaration(
            "\
procedure foo
    generic (
        x: natural
    );",
        );
        check_subprogram_declaration(
            "\
procedure foo
    generic (
        x: natural
    )
    parameter (
        a: std_logic
    );",
        );
        check_subprogram_declaration(
            "\
procedure foo
    generic (
        x: natural
    )
    (
        a: std_logic
    );",
        );
    }

    fn check_declaration(input: &str) {
        check_formatted(
            input,
            input,
            |code| code.declarative_part().into_iter().next().unwrap(),
            |formatter, ast, buffer| formatter.format_declaration(ast, buffer),
        );
    }

    #[test]
    fn test_subprogram_body() {
        check_declaration(
            "\
function \"+\"(
    arg: natural
) return natural is
begin
end function \"+\";",
        );
        check_declaration(
            "\
function foo(
    arg: natural
) return natural is
begin
end function foo;",
        );
    }

    #[test]
    fn test_subprogram_instantiation() {
        check_declaration("procedure my_proc is new proc;");
        check_declaration("function my_func is new func;");
        check_declaration("function my_func is new func[bit return bit_vector];");
        check_declaration(
            "\
function my_func is new func[bit return bit_vector] generic map (
    x => x
);",
        );
    }
}
