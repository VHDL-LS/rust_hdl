use crate::ast::EntityDeclaration;
use crate::formatting::DesignUnitFormatter;
use crate::{HasTokenSpan, TokenSpan};

impl DesignUnitFormatter<'_> {
    pub fn format_entity(&self, entity: &EntityDeclaration, buffer: &mut String) {
        self.format_context_clause(&entity.context_clause, buffer);
        if !entity.context_clause.is_empty() {
            self.newline(buffer);
            self.newline(buffer);
        }
        let span = entity.span();
        // entity <ident> is
        self.format_token_span(TokenSpan::new(span.start_token, entity.is_token()), buffer);
        if let Some(generic_clause) = &entity.generic_clause {
            self.increase_indentation();
            self.newline(buffer);
            self.format_interface_list(generic_clause, buffer);
            self.decrease_indentation();
        }
        if let Some(port_clause) = &entity.port_clause {
            self.increase_indentation();
            self.newline(buffer);
            self.format_interface_list(port_clause, buffer);
            self.decrease_indentation();
        }

        self.increase_indentation();
        self.format_declarations(&entity.decl, buffer);
        self.decrease_indentation();
        if let Some(token) = entity.begin_token {
            self.newline(buffer);
            self.format_token_id(token, buffer);
        }
        self.increase_indentation();
        self.format_concurrent_statements(&entity.statements, buffer);
        self.decrease_indentation();
        self.newline(buffer);
        // end [entity] [name];
        self.format_token_span(TokenSpan::new(entity.end_token, span.end_token - 1), buffer);
        self.format_token_id(span.end_token, buffer);
    }
}

#[cfg(test)]
mod test {
    use crate::analysis::tests::Code;
    use vhdl_lang::formatting::test_utils::check_formatted;

    fn check_entity_formatted(input: &str) {
        check_formatted(
            input,
            input,
            Code::entity_decl,
            |formatter, entity, buffer| formatter.format_entity(entity, buffer),
        );
    }

    #[test]
    fn test_format_simple_entity() {
        check_entity_formatted(
            "\
entity my_ent is
end entity my_ent;",
        );
        check_entity_formatted(
            "\
entity my_ent is
end my_ent;",
        );
        check_entity_formatted(
            "\
entity my_ent is
end;",
        );

        check_entity_formatted(
            "\
entity my_ent is
end entity;",
        );
        check_entity_formatted(
            "\
entity my_ent is
begin
end entity;",
        );
    }

    #[test]
    fn test_entity_with_comments() {
        check_entity_formatted(
            "\
-- Some comment about the entity
entity my_ent is
end entity;",
        );

        check_entity_formatted(
            "\
entity my_ent is -- trailing comment
end entity;",
        );

        check_entity_formatted(
            "\
entity /* Why would you put a comment here? */ my_ent is
end entity;",
        );

        check_entity_formatted(
            "\
entity /* Why would you put a comment here? */ my_ent is -- this is an entity
end entity;",
        );
    }

    #[test]
    fn test_entity_with_simple_generic() {
        check_entity_formatted(
            "\
entity foo is
    -- Generics come here
    generic (
        foo: in std_logic --<This is it
    );
end foo;",
        );
    }

    #[test]
    fn test_entity_generic_default_value() {
        check_entity_formatted(
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
    port (
        foo: in std_logic := '1'
    );
end foo;",
        );
    }

    #[test]
    fn test_entity_with_generics_and_ports() {
        check_entity_formatted(
            "\
entity foo is
    generic (
        a: in std_logic := '1'
    );
    port (
        B: in std_logic := '1'
    );
end foo;",
        );
    }

    #[test]
    fn test_entity_with_declarations() {
        check_entity_formatted(
            "\
entity foo is
    port (
        foo: in std_logic := '1'
    );
    constant x: foo := bar;
    signal y: bar := foobar;
end foo;",
        );
    }
}
