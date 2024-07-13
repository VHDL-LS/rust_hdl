use crate::ast::token_range::WithTokenSpan;
use crate::ast::{
    InterfaceDeclaration, InterfaceObjectDeclaration, ModeIndication, SimpleModeIndication,
};
use crate::formatting::DesignUnitFormatter;
use vhdl_lang::TokenSpan;

impl DesignUnitFormatter<'_> {
    pub(crate) fn format_port_or_generic(
        &self,
        clause: &WithTokenSpan<Vec<InterfaceDeclaration>>,
        buffer: &mut String,
    ) {
        self.increase_indentation();
        self.newline(buffer);
        let span = clause.span;
        // port (
        // generic (
        self.format_token_span(
            TokenSpan::new(span.start_token, span.start_token + 1),
            buffer,
        );
        self.increase_indentation();
        for item in &clause.item {
            self.newline(buffer);
            self.format_interface_declaration(item, buffer);
        }
        self.decrease_indentation();
        if !clause.item.is_empty() {
            self.newline(buffer);
        }
        // );
        self.format_token_id(span.end_token - 1, buffer);
        self.format_token_id(span.end_token, buffer);
        self.decrease_indentation();
    }

    pub fn format_interface_declaration(
        &self,
        declaration: &InterfaceDeclaration,
        buffer: &mut String,
    ) {
        match declaration {
            InterfaceDeclaration::Object(object) => self.format_interface_object(object, buffer),
            InterfaceDeclaration::File(_) => unimplemented!(),
            InterfaceDeclaration::Type(_) => unimplemented!(),
            InterfaceDeclaration::Subprogram(_) => unimplemented!(),
            InterfaceDeclaration::Package(_) => unimplemented!(),
        }
    }

    pub fn format_interface_object(
        &self,
        object: &InterfaceObjectDeclaration,
        buffer: &mut String,
    ) {
        // [signal] my_signal :
        self.format_token_span(
            TokenSpan::new(object.span.start_token, object.ident.tree.token),
            buffer,
        );
        self.format_token_id(object.colon_token(), buffer);
        buffer.push(' ');
        self.format_mode(&object.mode, buffer);
    }

    pub fn format_mode(&self, mode: &ModeIndication, buffer: &mut String) {
        match mode {
            ModeIndication::Simple(simple) => self.format_simple_mode(simple, buffer),
            ModeIndication::View(_) => unimplemented!(),
        }
    }

    pub fn format_simple_mode(&self, mode: &SimpleModeIndication, buffer: &mut String) {
        if let Some(mode) = &mode.mode {
            self.format_token_id(mode.token, buffer);
            buffer.push(' ');
        }
        self.format_subtype(&mode.subtype_indication, buffer);
        if let Some(expression) = &mode.expression {
            buffer.push(' ');
            self.format_token_id(expression.span.start_token - 1, buffer);
            buffer.push(' ');
            self.format_expression(&expression.item, expression.span, buffer);
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::analysis::tests::Code;
    use crate::formatting::DesignUnitFormatter;

    fn check_generic(input: &str, expected: &str) {
        let code = Code::new(input);
        let interface = code.generic();
        let tokens = code.tokenize();
        let formatter = DesignUnitFormatter::new(&tokens);
        let mut buffer = String::new();
        formatter.format_interface_declaration(&interface, &mut buffer);
        assert_eq!(&buffer, expected);
    }

    #[test]
    fn format_simple_object() {
        check_generic("my_generic: natural", "my_generic: natural");
    }

    #[test]
    fn format_simple_object_with_default() {
        check_generic("my_generic: natural := 7", "my_generic: natural := 7");
    }

    #[test]
    fn format_simple_object_with_explicit_mode() {
        check_generic("my_generic: in natural", "my_generic: in natural");
    }

    #[test]
    fn format_object_with_class() {
        check_generic(
            "constant my_generic: in natural",
            "constant my_generic: in natural",
        );
        check_generic(
            "constant my_generic: natural",
            "constant my_generic: natural",
        );
    }
}
