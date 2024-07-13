use crate::ast::EntityDeclaration;
use crate::formatting::DesignUnitFormatter;
use crate::{HasTokenSpan, TokenSpan};

impl DesignUnitFormatter<'_> {
    pub fn format_entity(&self, entity: &EntityDeclaration, buffer: &mut String) {
        self.format_context_clause(&entity.context_clause, buffer);
        let span = entity.span();
        // entity <ident> is
        self.format_token_span(TokenSpan::new(span.start_token, entity.is_token()), buffer);
        if let Some(generic_clause) = &entity.generic_clause {
            self.increase_indentation();
            self.newline(buffer);
            let span = generic_clause.span;
            // generic (
            self.format_token_span(
                TokenSpan::new(span.start_token, span.start_token + 1),
                buffer,
            );
            self.increase_indentation();
            for item in &generic_clause.item {
                self.newline(buffer);
                self.format_interface_declaration(item, buffer);
            }
            self.decrease_indentation();
            if !generic_clause.item.is_empty() {
                self.newline(buffer);
            }
            // );
            self.format_token_id(span.end_token - 1, buffer);
            self.format_token_id(span.end_token, buffer);
            self.decrease_indentation();
        }
        if entity.port_clause.is_some() {
            unimplemented!();
        }
        for (i, decl) in entity.decl.iter().enumerate() {
            self.format_declaration(decl);
            if i < entity.decl.len() - 1 {
                self.newline(buffer);
            }
        }
        for (i, statement) in entity.statements.iter().enumerate() {
            self.format_concurrent_statement(statement);
            if i < entity.decl.len() - 1 {
                self.newline(buffer);
            }
        }
        self.newline(buffer);
        self.format_token_span(TokenSpan::new(entity.end_token, span.end_token - 1), buffer);
        self.format_token_id(span.end_token, buffer);
    }
}

#[cfg(test)]
mod test {
    use crate::analysis::tests::Code;
    use crate::formatting::DesignUnitFormatter;

    fn check_entity_formatted(input: &str, expected: &str) {
        let code = Code::new(input);
        let ent = code.entity_decl();
        let tokens = code.tokenize();
        let formatter = DesignUnitFormatter::new(&tokens);
        let mut buffer = String::new();
        formatter.format_entity(&ent, &mut buffer);
        assert_eq!(&buffer, expected);
    }

    #[test]
    fn test_format_simple_entity() {
        check_entity_formatted(
            "entity my_ent is end entity my_ent;",
            "\
entity my_ent is
end entity my_ent;",
        );
        check_entity_formatted(
            "entity my_ent is end my_ent;",
            "\
entity my_ent is
end my_ent;",
        );

        check_entity_formatted(
            "entity my_ent is end;",
            "\
entity my_ent is
end;",
        );

        check_entity_formatted(
            "entity my_ent is end entity;",
            "\
entity my_ent is
end entity;",
        );
    }

    #[test]
    fn test_entity_with_comments() {
        check_entity_formatted(
            "\
-- Some comment about the entity
entity my_ent is end entity;",
            "\
-- Some comment about the entity
entity my_ent is
end entity;",
        );

        check_entity_formatted(
            "\
entity my_ent is -- trailing comment
end entity;",
            "\
entity my_ent is -- trailing comment
end entity;",
        );

        check_entity_formatted(
            "\
entity /* Why would you put a comment here? */ my_ent is
end entity;",
            "\
entity /* Why would you put a comment here? */ my_ent is
end entity;",
        );

        check_entity_formatted(
            "\
entity /* Why would you put a comment here? */ my_ent is -- this is an entity
end entity;",
            "\
entity /* Why would you put a comment here? */ my_ent is -- this is an entity
end entity;",
        );
    }

    #[test]
    fn test_entity_with_generic() {
        check_entity_formatted(
            "\
entity foo is
    generic ();
end foo;",
            "\
entity foo is
    generic ();
end foo;",
        );
        check_entity_formatted(
            "\
entity foo is
    -- Generics come here
    generic (); --<This is it
end foo;",
            "\
entity foo is
    -- Generics come here
    generic (); --<This is it
end foo;",
        );
    }

    #[test]
    fn test_entity_with_simple_generic() {
        check_entity_formatted(
            "\
entity foo is
    generic (foo : in std_logic);
end foo;",
            "\
entity foo is
    generic (
        foo: in std_logic
    );
end foo;",
        );
    }

    #[test]
    fn test_entity_generic_default_value() {
        check_entity_formatted(
            "\
entity foo is
    generic (foo : in std_logic := '1');
end foo;",
            "\
entity foo is
    generic (
        foo: in std_logic := '1'
    );
end foo;",
        );
    }

    #[test]
    fn test_entity_with_ports() {
        check_entity_formatted(
            "\
entity foo is
    port (foo : in std_logic := '1');
end foo;",
            "\
entity foo is
    port (
        foo: in std_logic := '1'
    );
end foo;",
        );
    }
}
