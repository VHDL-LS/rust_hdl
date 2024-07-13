use crate::ast::ObjectDeclaration;
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
        match &declaration.item {
            Declaration::Object(object_decl) => {
                self.format_object_declaration(object_decl, declaration.span, buffer)
            }
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
}

#[cfg(test)]
mod tests {
    use crate::formatting::DesignUnitFormatter;
    use crate::syntax::test::Code;

    fn check_object_declaration(input: &str, expected: &str) {
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
        check_object_declaration(
            "constant my_const : std_logic;",
            "constant my_const: std_logic;",
        );
        check_object_declaration(
            "variable my_var : std_logic;",
            "variable my_var: std_logic;",
        );
        check_object_declaration("signal foo : std_logic;", "signal foo: std_logic;");
        check_object_declaration(
            "shared variable bar : std_logic;",
            "shared variable bar: std_logic;",
        );

        check_object_declaration(
            "shared variable bar : std_logic := '0';",
            "shared variable bar: std_logic := '0';",
        );
    }
}
