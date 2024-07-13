use crate::ast::{
    ArrayIndex, FileDeclaration, ObjectDeclaration, SubtypeIndication, TypeDeclaration,
    TypeDefinition,
};
use crate::formatting::DesignUnitFormatter;
use crate::syntax::Kind;
use crate::{TokenAccess, TokenId, TokenSpan};
use vhdl_lang::ast::token_range::WithTokenSpan;
use vhdl_lang::ast::{Declaration, ObjectClass, PhysicalTypeDeclaration};

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
            _ => unimplemented!(),
        }
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
        self.format_subtype(&object_decl.subtype_indication, buffer);
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
        self.format_subtype(&file_decl.subtype_indication, buffer);
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
            TokenSpan::new(span.start_token, type_decl.is_token()),
            buffer,
        );
        buffer.push(' ');
        self.format_type_definition(
            &type_decl.def,
            TokenSpan::new(
                type_decl.is_token() + 1,
                type_decl.end_ident_pos.unwrap_or(span.end_token) - 1,
            ),
            buffer,
        );
        if let Some(end_ident) = type_decl.end_ident_pos {
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
            _ => unimplemented!(),
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
        self.format_subtype(subtype, buffer);
    }
}

#[cfg(test)]
mod tests {
    use crate::formatting::DesignUnitFormatter;
    use crate::syntax::test::Code;

    fn check_declaration(input: &str, expected: &str) {
        let code = Code::new(input);
        let declaration = code.declarative_part().into_iter().next().unwrap();
        let tokens = code.tokenize();
        let formatter = DesignUnitFormatter::new(&tokens);
        let mut buffer = String::new();
        formatter.format_declaration(&declaration, &mut buffer);
        assert_eq!(&buffer, expected);
    }

    #[test]
    fn object_declarations() {
        check_declaration(
            "constant my_const : std_logic;",
            "constant my_const: std_logic;",
        );
        check_declaration(
            "variable my_var : std_logic;",
            "variable my_var: std_logic;",
        );
        check_declaration("signal foo : std_logic;", "signal foo: std_logic;");
        check_declaration(
            "shared variable bar : std_logic;",
            "shared variable bar: std_logic;",
        );

        check_declaration(
            "shared variable bar : std_logic := '0';",
            "shared variable bar: std_logic := '0';",
        );
    }

    #[test]
    fn file_declarations() {
        check_declaration("file my_file : text;", "file my_file: text;");
        check_declaration(
            "file my_file : text is \"my_file.txt\";",
            "file my_file: text is \"my_file.txt\";",
        );
        check_declaration(
            "file my_file : text open mode is \"my_file.txt\";",
            "file my_file: text open mode is \"my_file.txt\";",
        );
    }

    #[test]
    fn enum_declaration() {
        check_declaration("type my_enum is (A);", "type my_enum is (A);");
        check_declaration("type my_enum is (A,B);", "type my_enum is (A, B);");
        check_declaration(
            "type my_enum is ('0', '1', 'U', 'X');",
            "type my_enum is ('0', '1', 'U', 'X');",
        );
    }

    #[test]
    fn numeric_type_declaration() {
        check_declaration(
            "type my_enum is range 0 to 5;",
            "type my_enum is range 0 to 5;",
        );
    }

    #[test]
    fn physical_types() {
        check_declaration(
            "type TIME is range -9223372036854775807 to 9223372036854775807
    units
      fs; -- femtosecond
    end units;",
            "\
type TIME is range -9223372036854775807 to 9223372036854775807
    units
        fs; -- femtosecond
    end units;",
        );

        check_declaration(
            "type TIME is range -9223372036854775807 to 9223372036854775807
    units
      fs; -- femtosecond
      ps = 1000 fs; -- picosecond
      ns = 1000 ps; -- nanosecond
      us = 1000 ns; -- microsecond
      ms = 1000 us; -- millisecond
      sec = 1000 ms; -- second
      min = 60 sec; -- minute
      hr= 60 min; -- hour
    end units;",
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
            "type TIME is range -9223372036854775807 to 9223372036854775807
    units
      fs; -- femtosecond
      ps = fs; -- picosecond
    end units;",
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
        check_declaration(
            "type my_array is array (natural range <>) of std_logic_vector;",
            "type my_array is array (natural range <>) of std_logic_vector;",
        );
        check_declaration(
            "type foo is array (2-1 downto 0, integer range <>) of boolean;",
            "type foo is array (2 - 1 downto 0, integer range <>) of boolean;",
        );
        check_declaration(
            "type foo is array (2-1 downto 0) of boolean;",
            "type foo is array (2 - 1 downto 0) of boolean;",
        );
    }
}
