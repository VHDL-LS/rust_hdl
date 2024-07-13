use crate::ast::{FileDeclaration, ObjectDeclaration};
use crate::formatting::DesignUnitFormatter;
use crate::TokenSpan;
use vhdl_lang::ast::token_range::WithTokenSpan;
use vhdl_lang::ast::{Declaration, ObjectClass};

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
        self.format_token_id(file_decl.ident.tree.token, buffer);
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
}
