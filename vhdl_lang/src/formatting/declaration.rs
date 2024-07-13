use crate::ast::{
    ArrayIndex, ComponentDeclaration, ElementDeclaration, EntityName, FileDeclaration,
    ObjectDeclaration, ProtectedTypeDeclarativeItem, SubtypeIndication, TypeDeclaration,
    TypeDefinition,
};
use crate::formatting::DesignUnitFormatter;
use crate::syntax::Kind;
use crate::{TokenAccess, TokenId, TokenSpan};
use vhdl_lang::ast::token_range::WithTokenSpan;
use vhdl_lang::ast::{
    AliasDeclaration, Attribute, AttributeDeclaration, AttributeSpecification, Declaration,
    ObjectClass, PhysicalTypeDeclaration, ProtectedTypeBody, ProtectedTypeDeclaration,
};

impl DesignUnitFormatter<'_> {
    pub(crate) fn format_declarations(
        &self,
        declarations: &[WithTokenSpan<Declaration>],
        buffer: &mut String,
    ) {
        if !declarations.is_empty() {
            self.newline(buffer);
        }
        for (i, decl) in declarations.iter().enumerate() {
            self.format_declaration(decl, buffer);
            if i < declarations.len() - 1 {
                self.newline(buffer);
            }
        }
    }

    pub fn format_declaration(
        &self,
        declaration: &WithTokenSpan<Declaration>,
        buffer: &mut String,
    ) {
        use Declaration::*;
        match &declaration.item {
            Object(object_decl) => {
                self.format_object_declaration(object_decl, declaration.span, buffer)
            }
            File(file_decl) => self.format_file_declaration(file_decl, declaration.span, buffer),
            Type(type_decl) => self.format_type_declaration(type_decl, declaration.span, buffer),
            Component(component) => self.format_component_declaration(component, buffer),
            Attribute(attribute) => self.format_attribute(attribute, declaration.span, buffer),
            Alias(alias) => self.format_alias_declaration(alias, declaration.span, buffer),
            SubprogramDeclaration(subprogram_declaration) => {
                self.format_subprogram_declaration(subprogram_declaration, buffer)
            }
            SubprogramBody(subprogram_body) => self.format_subprogram_body(subprogram_body, buffer),
            _ => unimplemented!(),
        }
    }

    pub fn format_component_declaration(
        &self,
        component: &ComponentDeclaration,
        buffer: &mut String,
    ) {
        self.format_token_span(
            TokenSpan::new(
                component.span.start_token,
                component.is_token.unwrap_or(component.span.start_token + 1),
            ),
            buffer,
        );
        if let Some(generic_clause) = &component.generic_list {
            self.format_interface_list(generic_clause, buffer);
        }
        if let Some(port_clause) = &component.port_list {
            self.format_interface_list(port_clause, buffer);
        }
        self.newline(buffer);
        self.format_token_span(
            TokenSpan::new(component.end_token, component.span.end_token - 1),
            buffer,
        );
        self.format_token_id(component.span.end_token, buffer);
    }

    pub fn format_object_declaration(
        &self,
        object_decl: &ObjectDeclaration,
        span: TokenSpan,
        buffer: &mut String,
    ) {
        self.format_token_id(span.start_token, buffer);
        if object_decl.class == ObjectClass::SharedVariable {
            buffer.push(' ');
            self.format_token_id(span.start_token + 1, buffer);
        }
        buffer.push(' ');
        self.format_token_id(object_decl.ident.tree.token, buffer);
        self.format_token_id(object_decl.colon_token(), buffer);
        buffer.push(' ');
        self.format_subtype_indication(&object_decl.subtype_indication, buffer);
        self.format_default_expression(object_decl.expression.as_ref(), buffer);

        self.format_token_id(span.end_token, buffer);
    }

    pub fn format_file_declaration(
        &self,
        file_decl: &FileDeclaration,
        span: TokenSpan,
        buffer: &mut String,
    ) {
        self.format_token_id(span.start_token, buffer);
        buffer.push(' ');
        self.format_ident(&file_decl.ident, buffer);
        self.format_token_id(file_decl.colon_token(), buffer);
        buffer.push(' ');
        self.format_subtype_indication(&file_decl.subtype_indication, buffer);
        if let Some((token, open_information)) = &file_decl.open_info {
            buffer.push(' ');
            self.format_token_id(*token, buffer);
            buffer.push(' ');
            self.format_expression(&open_information.item, open_information.span, buffer);
        }
        if let Some((token, file_name)) = &file_decl.file_name {
            buffer.push(' ');
            self.format_token_id(*token, buffer);
            buffer.push(' ');
            self.format_expression(&file_name.item, file_name.span, buffer);
        }
        self.format_token_id(span.end_token, buffer);
    }

    pub fn format_type_declaration(
        &self,
        type_decl: &TypeDeclaration,
        span: TokenSpan,
        buffer: &mut String,
    ) {
        self.format_token_span(
            TokenSpan::new(span.start_token, type_decl.ident.tree.token),
            buffer,
        );
        if let Some(is_token) = type_decl.is_token() {
            buffer.push(' ');
            self.format_token_id(is_token, buffer);
        }
        if let Some(is_token) = type_decl.is_token() {
            buffer.push(' ');

            self.format_type_definition(
                &type_decl.def,
                TokenSpan::new(
                    is_token + 1,
                    type_decl.end_ident_pos.unwrap_or(span.end_token) - 1,
                ),
                buffer,
            );
        }
        if let Some(end_ident) = type_decl.end_ident_pos {
            buffer.push(' ');
            self.format_token_id(end_ident, buffer);
        }
        self.format_token_id(span.end_token, buffer);
    }

    pub fn format_type_definition(
        &self,
        definition: &TypeDefinition,
        span: TokenSpan,
        buffer: &mut String,
    ) {
        use TypeDefinition::*;
        match definition {
            Enumeration(literals) => {
                self.format_token_id(span.start_token, buffer);
                for literal in literals {
                    self.format_token_id(literal.tree.token, buffer);
                    if self.tokens.get_token(literal.tree.token + 1).kind == Kind::Comma {
                        self.format_token_id(literal.tree.token + 1, buffer);
                        buffer.push(' ');
                    }
                }
                self.format_token_id(span.end_token, buffer);
            }
            Numeric(range) => {
                self.format_token_id(span.start_token, buffer);
                buffer.push(' ');
                self.format_range(range, span.start_with(span.start_token + 1), buffer)
            }
            Physical(physical_type) => {
                self.format_physical_type_declaration(physical_type, span, buffer)
            }
            Array(indices, of_token, subtype) => {
                self.format_array_type_declaration(indices, *of_token, subtype, span, buffer)
            }
            Record(elements) => self.format_record_declaration(elements, span, buffer),
            Access(subtype_indication) => {
                // access
                self.format_token_id(span.start_token, buffer);
                buffer.push(' ');
                self.format_subtype_indication(subtype_indication, buffer);
            }
            Incomplete(_) => {
                // nothing to do
            }
            File(name) => {
                // file of
                self.format_token_span(
                    TokenSpan::new(span.start_token, span.start_token + 1),
                    buffer,
                );
                buffer.push(' ');
                self.format_name(&name.item, name.span, buffer);
            }
            Protected(protected) => self.format_protected_type_declaration(protected, span, buffer),
            ProtectedBody(protected_body) => {
                self.format_protected_body_type_declaration(protected_body, span, buffer)
            }
            Subtype(subtype) => self.format_subtype_indication(subtype, buffer),
        }
    }

    pub fn format_physical_type_declaration(
        &self,
        declaration: &PhysicalTypeDeclaration,
        span: TokenSpan,
        buffer: &mut String,
    ) {
        // range
        self.format_token_id(span.start_token, buffer);
        buffer.push(' ');
        self.format_range(
            &declaration.range,
            TokenSpan::new(span.start_token + 1, declaration.units_token - 1),
            buffer,
        );
        self.increase_indentation();
        self.newline(buffer);
        self.format_token_id(declaration.units_token, buffer);
        self.increase_indentation();
        self.newline(buffer);
        // primary_unit;
        self.format_ident(&declaration.primary_unit, buffer);
        self.format_token_id(declaration.primary_unit.tree.token + 1, buffer);
        for (ident, literal) in &declaration.secondary_units {
            self.newline(buffer);
            self.format_ident(ident, buffer);
            buffer.push(' ');
            // =
            self.format_token_id(ident.tree.token + 1, buffer);
            buffer.push(' ');
            self.format_token_span(literal.span, buffer);
            // ;
            self.format_token_id(literal.span.end_token + 1, buffer);
        }
        self.decrease_indentation();
        self.newline(buffer);
        // end units
        self.format_token_span(TokenSpan::new(span.end_token - 1, span.end_token), buffer);
        self.decrease_indentation();
    }

    pub fn format_array_type_declaration(
        &self,
        indices: &[ArrayIndex],
        of_token: TokenId,
        subtype: &SubtypeIndication,
        span: TokenSpan,
        buffer: &mut String,
    ) {
        // array
        self.format_token_id(span.start_token, buffer);
        buffer.push(' ');
        // (
        self.format_token_id(span.start_token + 1, buffer);
        for (i, index) in indices.iter().enumerate() {
            let end_token = match index {
                ArrayIndex::IndexSubtypeDefintion(name) => {
                    self.format_name(&name.item, name.span, buffer);
                    buffer.push(' ');
                    self.format_token_span(
                        TokenSpan::new(name.span.end_token + 1, name.span.end_token + 2),
                        buffer,
                    );
                    name.span.end_token + 3
                }
                ArrayIndex::Discrete(discrete_range) => {
                    self.format_discrete_range(&discrete_range.item, discrete_range.span, buffer);
                    discrete_range.span.end_token + 1
                }
            };
            if i < indices.len() - 1 {
                self.format_token_id(end_token, buffer);
                buffer.push(' ');
            }
        }
        // )
        self.format_token_id(of_token - 1, buffer);
        buffer.push(' ');
        // of
        self.format_token_id(of_token, buffer);
        buffer.push(' ');
        self.format_subtype_indication(subtype, buffer);
    }

    pub fn format_record_declaration(
        &self,
        elements: &[ElementDeclaration],
        span: TokenSpan,
        buffer: &mut String,
    ) {
        // record
        self.format_token_id(span.start_token, buffer);
        let mut last_token = span.start_token;
        self.increase_indentation();
        for element in elements {
            self.newline(buffer);
            self.format_element_declaration(element, buffer);
            last_token = element.span.end_token;
        }
        self.decrease_indentation();
        self.newline(buffer);
        // end record
        self.format_token_span(TokenSpan::new(last_token + 1, span.end_token), buffer)
    }

    pub fn format_element_declaration(
        &self,
        declaration: &ElementDeclaration,
        buffer: &mut String,
    ) {
        self.format_ident(&declaration.ident, buffer);
        // :
        self.format_token_id(declaration.ident.tree.token + 1, buffer);
        buffer.push(' ');
        self.format_subtype_indication(&declaration.subtype, buffer);
        // ;
        self.format_token_id(declaration.span.end_token, buffer);
    }

    pub fn format_protected_type_declaration(
        &self,
        declaration: &ProtectedTypeDeclaration,
        span: TokenSpan,
        buffer: &mut String,
    ) {
        // protected
        self.format_token_id(span.start_token, buffer);
        let mut last_token = span.start_token;
        self.increase_indentation();
        for element in &declaration.items {
            self.newline(buffer);
            match element {
                ProtectedTypeDeclarativeItem::Subprogram(subprogram) => {
                    self.format_subprogram_declaration(subprogram, buffer);
                    last_token = subprogram.span.end_token;
                }
            }
        }
        self.decrease_indentation();
        self.newline(buffer);
        // end protected
        self.format_token_span(TokenSpan::new(last_token + 1, span.end_token), buffer)
    }

    pub fn format_protected_body_type_declaration(
        &self,
        declaration: &ProtectedTypeBody,
        span: TokenSpan,
        buffer: &mut String,
    ) {
        // protected body
        self.format_token_span(
            TokenSpan::new(span.start_token, span.start_token + 1),
            buffer,
        );
        self.increase_indentation();
        self.format_declarations(&declaration.decl, buffer);
        self.decrease_indentation();
        self.newline(buffer);
        let last_token = declaration
            .decl
            .last()
            .map(|last| last.span.end_token)
            .unwrap_or(span.start_token + 1);
        // end protected body
        self.format_token_span(TokenSpan::new(last_token + 1, span.end_token), buffer)
    }

    pub fn format_attribute(&self, attribute: &Attribute, span: TokenSpan, buffer: &mut String) {
        use Attribute::*;
        match attribute {
            Specification(spec) => self.format_attribute_specification(spec, span, buffer),
            Declaration(dec) => self.format_attribute_declaration(dec, span, buffer),
        }
    }

    pub fn format_attribute_declaration(
        &self,
        attribute: &AttributeDeclaration,
        span: TokenSpan,
        buffer: &mut String,
    ) {
        self.format_token_span(
            TokenSpan::new(span.start_token, attribute.ident.tree.token),
            buffer,
        );
        // :
        self.format_token_id(attribute.ident.tree.token + 1, buffer);
        buffer.push(' ');
        self.format_name(&attribute.type_mark.item, attribute.type_mark.span, buffer);
        self.format_token_id(span.end_token, buffer);
    }

    pub fn format_attribute_specification(
        &self,
        attribute: &AttributeSpecification,
        span: TokenSpan,
        buffer: &mut String,
    ) {
        // attribute <name> of
        self.format_token_span(
            TokenSpan::new(span.start_token, attribute.ident.item.token + 1),
            buffer,
        );
        buffer.push(' ');
        match &attribute.entity_name {
            EntityName::Name(name) => {
                self.format_token_id(name.designator.token, buffer);
                if let Some(signature) = &name.signature {
                    self.format_signature(signature, buffer);
                }
            }
            EntityName::All | EntityName::Others => {
                self.format_token_id(attribute.ident.item.token + 2, buffer)
            }
        }
        // : <entity_class> is
        self.format_token_span(
            TokenSpan::new(attribute.colon_token, attribute.colon_token + 2),
            buffer,
        );
        buffer.push(' ');
        self.format_expression(&attribute.expr.item, attribute.expr.span, buffer);
        self.format_token_id(span.end_token, buffer);
    }

    pub fn format_alias_declaration(
        &self,
        alias: &AliasDeclaration,
        span: TokenSpan,
        buffer: &mut String,
    ) {
        // alias <name>
        self.format_token_span(
            TokenSpan::new(span.start_token, span.start_token + 1),
            buffer,
        );
        if let Some(subtype) = &alias.subtype_indication {
            // :
            self.format_token_id(span.start_token + 2, buffer);
            buffer.push(' ');
            self.format_subtype_indication(subtype, buffer);
        }
        buffer.push(' ');
        self.format_token_id(alias.is_token, buffer);
        buffer.push(' ');
        self.format_name(&alias.name.item, alias.name.span, buffer);
        if let Some(signature) = &alias.signature {
            self.format_signature(signature, buffer);
        }
        self.format_token_id(span.end_token, buffer);
    }
}

#[cfg(test)]
mod tests {
    use crate::formatting::test_utils::check_formatted;

    fn check_declaration(input: &str) {
        check_formatted(
            input,
            input,
            |code| code.declarative_part().into_iter().next().unwrap(),
            |formatter, ast, buffer| formatter.format_declaration(ast, buffer),
        );
    }

    #[test]
    fn object_declarations() {
        check_declaration("constant my_const: std_logic;");
        check_declaration("variable my_var: std_logic;");
        check_declaration("signal foo: std_logic;");
        check_declaration("shared variable bar: std_logic;");

        check_declaration("shared variable bar: std_logic := '0';");
    }

    #[test]
    fn file_declarations() {
        check_declaration("file my_file: text;");
        check_declaration("file my_file: text is \"my_file.txt\";");
        check_declaration("file my_file: text open mode is \"my_file.txt\";");
    }

    #[test]
    fn enum_declaration() {
        check_declaration("type my_enum is (A);");
        check_declaration("type my_enum is (A, B);");
        check_declaration("type my_enum is ('0', '1', 'U', 'X');");
    }

    #[test]
    fn numeric_type_declaration() {
        check_declaration("type my_enum is range 0 to 5;");
    }

    #[test]
    fn physical_types() {
        check_declaration(
            "\
type TIME is range -9223372036854775807 to 9223372036854775807
    units
        fs; -- femtosecond
    end units;",
        );

        check_declaration(
            "\
type TIME is range -9223372036854775807 to 9223372036854775807
    units
        fs; -- femtosecond
        ps = 1000 fs; -- picosecond
        ns = 1000 ps; -- nanosecond
        us = 1000 ns; -- microsecond
        ms = 1000 us; -- millisecond
        sec = 1000 ms; -- second
        min = 60 sec; -- minute
        hr = 60 min; -- hour
    end units;",
        );

        check_declaration(
            "\
type TIME is range -9223372036854775807 to 9223372036854775807
    units
        fs; -- femtosecond
        ps = fs; -- picosecond
    end units;",
        );
    }

    #[test]
    fn array_type_definition() {
        check_declaration("type my_array is array (natural range <>) of std_logic_vector;");
        check_declaration("type foo is array (2 - 1 downto 0, integer range <>) of boolean;");
        check_declaration("type foo is array (2 - 1 downto 0) of boolean;");
    }

    #[test]
    fn record_type_definition() {
        check_declaration(
            "\
type x is record
end record;",
        );
        check_declaration(
            "\
type foo is record
    element: boolean;
end record;",
        );
        check_declaration(
            "\
type foo is record
    element: boolean;
    other_element: std_logic_vector;
end foo;",
        );
        check_declaration(
            "\
type dummy_rec is record
    dummy: bit;
end record;",
        );
    }

    #[test]
    fn access_definition() {
        check_declaration("type dummy_rec is access bit;");
    }

    #[test]
    fn incomplete_type_definition() {
        check_declaration("type incomplete;");
    }

    #[test]
    fn file_definitions() {
        check_declaration("type foo is file of character;");
    }

    #[test]
    fn protected_declaration() {
        check_declaration(
            "type foo is protected
end protected;",
        );

        check_declaration(
            "type foo is protected
end protected foo;",
        );

        check_declaration(
            "type foo is protected
    procedure proc;
    function fun return ret;
end protected;",
        );
    }

    #[test]
    fn protected_body_declaration() {
        check_declaration(
            "type foo is protected body
end protected body;",
        );

        check_declaration(
            "\
type foo is protected body
    variable foo: natural;
    procedure proc is
    begin
    end;
end protected body;",
        );
    }

    #[test]
    fn protected_subtype_declaration() {
        check_declaration("subtype vec_t is integer_vector(2 - 1 downto 0);");
    }

    #[test]
    fn component_declaration() {
        check_declaration(
            "\
component foo
end component;",
        );
        check_declaration(
            "\
component foo is
end component;",
        );
        check_declaration(
            "\
component foo is
end component foo;",
        );
        check_declaration(
            "\
component foo is
    generic (
        foo: natural
    );
end component;",
        );
        check_declaration(
            "\
component foo is
    port (
        foo: natural
    );
end component;",
        );
    }

    #[test]
    fn check_attribute_declaration() {
        check_declaration("attribute foo: name;");
        check_declaration("attribute attr_name of foo: signal is 0 + 1;");
        check_declaration("attribute attr_name of \"**\": function is 0 + 1;");
        check_declaration("attribute attr_name of all: signal is 0 + 1;");
        check_declaration("attribute attr_name of foo[return natural]: function is 0 + 1;");
    }

    #[test]
    fn check_alias_declaration() {
        check_declaration("alias foo is name;");
        check_declaration("alias foo: vector(0 to 1) is name;");
        check_declaration("alias foo is name[return natural];");
        check_declaration("alias \"and\" is name;");
        check_declaration("alias 'c' is 'b';");
    }
}
