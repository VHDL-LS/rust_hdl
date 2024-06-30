use crate::ast::EntityDeclaration;
use crate::formatting::DesignUnitFormatter;
use crate::{HasTokenSpan, TokenSpan};

impl DesignUnitFormatter<'_, '_> {
    pub fn format_entity(&self, entity: &EntityDeclaration) -> String {
        let mut result = String::new();
        result.push_str(&self.format_context_clause(&entity.context_clause));
        let span = entity.span();
        // entity <ident> is
        result
            .push_str(&self.format_token_span(TokenSpan::new(span.start_token, entity.is_token())));
        result.push('\n');
        if let Some(_) = &entity.generic_clause {
            unimplemented!();
        }
        if entity.port_clause.is_some() {
            unimplemented!();
        }
        if !entity.decl.is_empty() {
            unimplemented!();
        }
        if !entity.statements.is_empty() {
            unimplemented!();
        }
        result.push_str(
            &self.format_token_span(TokenSpan::new(entity.end_token, span.end_token - 1)),
        );
        result.push_str(&self.format_token_id(span.end_token));
        result
    }
}

#[cfg(test)]
mod test {
    use crate::analysis::tests::Code;
    use crate::formatting::{DesignUnitFormatter, Formatter};

    fn check_entity_formatted(input: &str, expected: &str) {
        let code = Code::new(input);
        let ent = code.entity_decl();
        let parent_formatter = Formatter {};
        let formatter = DesignUnitFormatter {
            formatter: &parent_formatter,
            tokens: &code.tokenize(),
        };
        assert_eq!(&formatter.format_entity(&ent), expected);
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
}
