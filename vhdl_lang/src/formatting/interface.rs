use crate::ast::{
    ActualPart, AssociationElement, ElementMode, IdentList, InterfaceDeclaration, InterfaceList,
    InterfaceObjectDeclaration, MapAspect, ModeIndication, ModeViewElement, SimpleModeIndication,
};
use crate::formatting::DesignUnitFormatter;
use crate::syntax::Kind;
use crate::{HasTokenSpan, TokenAccess};
use vhdl_lang::ast::token_range::WithTokenSpan;
use vhdl_lang::TokenSpan;

impl DesignUnitFormatter<'_> {
    pub(crate) fn format_interface_list(&self, clause: &InterfaceList, buffer: &mut String) {
        let span = clause.span;
        let end_token = if self.tokens.get_token(span.start_token).kind == Kind::LeftPar {
            // We start with a `(` immediately
            // applicable for parameters (though VHDL 2008 allows an optional `parameter` keyword)
            span.start_token
        } else {
            // We start with a `generic`, `port` or `parameter` keyword
            span.start_token + 1
        };
        // port (
        // generic (
        // parameter (
        // (
        self.format_token_span(TokenSpan::new(span.start_token, end_token), buffer);
        self.increase_indentation();
        for (i, item) in clause.items.iter().enumerate() {
            self.newline(buffer);
            self.format_interface_declaration(item, buffer);
            if i < clause.items.len() - 1 {
                self.format_token_id(item.get_end_token() + 1, buffer);
            }
        }
        self.decrease_indentation();
        if !clause.items.is_empty() {
            self.newline(buffer);
        }
        if self.tokens.get_token(span.end_token).kind == Kind::SemiColon {
            // );
            self.format_token_id(span.end_token - 1, buffer);
            self.format_token_id(span.end_token, buffer);
        } else {
            self.format_token_id(span.end_token, buffer);
        }
    }

    pub fn format_map_aspect(&self, aspect: &MapAspect, buffer: &mut String) {
        // port map (
        // generic map (
        self.format_token_span(
            TokenSpan::new(aspect.span.start_token, aspect.span.start_token + 2),
            buffer,
        );
        self.increase_indentation();
        for (i, item) in aspect.list.items.iter().enumerate() {
            self.newline(buffer);
            self.format_association_element(item, buffer);
            if let Some(token) = aspect.list.tokens.get(i) {
                self.format_token_id(*token, buffer);
            }
        }
        self.decrease_indentation();
        if !aspect.list.items.is_empty() {
            self.newline(buffer);
        }
        // )
        self.format_token_id(aspect.span.end_token, buffer);
    }

    pub fn format_association_element(&self, element: &AssociationElement, buffer: &mut String) {
        if let Some(formal) = &element.formal {
            self.format_name(&formal.item, formal.span, buffer);
            buffer.push(' ');
            self.format_token_id(formal.span.end_token + 1, buffer);
            buffer.push(' ');
        }
        self.format_actual_part(&element.actual, buffer)
    }

    pub fn format_actual_part(&self, actual_part: &WithTokenSpan<ActualPart>, buffer: &mut String) {
        match &actual_part.item {
            ActualPart::Expression(expression) => {
                self.format_expression(expression, actual_part.span, buffer)
            }
            ActualPart::Open => self.format_token_span(actual_part.span, buffer),
        }
    }

    pub fn format_interface_declaration(
        &self,
        declaration: &InterfaceDeclaration,
        buffer: &mut String,
    ) {
        match declaration {
            InterfaceDeclaration::Object(object) => self.format_interface_object(object, buffer),
            InterfaceDeclaration::File(_) => unimplemented!(),
            InterfaceDeclaration::Type(type_decl) => {
                self.format_token_id(type_decl.tree.token - 1, buffer);
                buffer.push(' ');
                self.format_ident(type_decl, buffer);
            }
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

    pub fn format_element_mode(&self, mode: &ElementMode, buffer: &mut String) {
        match mode {
            ElementMode::Simple(simple) => self.format_token_id(simple.token, buffer),
            ElementMode::Record(name) => {
                // view
                self.format_token_id(name.get_start_token() - 1, buffer);
                buffer.push(' ');
                self.format_name(&name.item, name.span, buffer)
            }
            ElementMode::Array(name) => {
                //view (
                self.format_token_span(
                    TokenSpan::new(name.get_start_token() - 2, name.get_start_token() - 1),
                    buffer,
                );
                self.format_name(&name.item, name.span, buffer);
                // )
                self.format_token_id(name.get_end_token() + 1, buffer);
            }
        }
    }

    pub fn format_ident_list(&self, ident_list: &IdentList, buffer: &mut String) {
        for (i, item) in ident_list.items.iter().enumerate() {
            self.format_token_id(item.item.token, buffer);
            if let Some(token) = ident_list.tokens.get(i) {
                self.format_token_id(*token, buffer);
            }
        }
    }

    pub fn format_mode_view_element(&self, mode: &ModeViewElement, buffer: &mut String) {
        self.format_ident_list(&mode.names, buffer);
        self.format_token_id(mode.colon_token, buffer);
        buffer.push(' ');
        self.format_element_mode(&mode.mode, buffer);
        // ;
        self.format_token_id(mode.span.end_token, buffer);
    }

    pub fn format_simple_mode(&self, mode: &SimpleModeIndication, buffer: &mut String) {
        if let Some(mode) = &mode.mode {
            self.format_token_id(mode.token, buffer);
            buffer.push(' ');
        }
        self.format_subtype_indication(&mode.subtype_indication, buffer);
        self.format_default_expression(mode.expression.as_ref(), buffer);
    }
}

#[cfg(test)]
mod tests {
    use crate::analysis::tests::Code;
    use crate::formatting::test_utils::{check_formatted, check_formatted_std};
    use crate::VHDLStandard::VHDL2019;

    fn check_generic(input: &str) {
        check_formatted(
            input,
            input,
            Code::generic,
            |formatter, interface, buffer| {
                formatter.format_interface_declaration(interface, buffer)
            },
        );
    }

    #[test]
    fn format_simple_object() {
        check_generic("my_generic: natural");
    }

    #[test]
    fn format_simple_object_with_default() {
        check_generic("my_generic: natural := 7");
    }

    #[test]
    fn format_simple_object_with_explicit_mode() {
        check_generic("my_generic: in natural");
    }

    #[test]
    fn format_object_with_class() {
        check_generic("constant my_generic: in natural");
        check_generic("constant my_generic: natural");
    }

    fn check_element_mode(input: &str) {
        check_formatted_std(
            input,
            input,
            VHDL2019,
            Code::element_mode,
            |formatter, mode, buffer| formatter.format_element_mode(mode, buffer),
        );
    }

    #[test]
    fn format_element_mode() {
        check_element_mode("in");
        check_element_mode("out");
        check_element_mode("inout");
        check_element_mode("buffer");
        check_element_mode("linkage");
        check_element_mode("view foo");
        check_element_mode("view (foo)");
    }
}
