use crate::ast::{BlockConfiguration, ConfigurationDeclaration, ConfigurationItem};
use crate::{TokenSpan, VHDLFormatter};

impl VHDLFormatter<'_> {
    pub fn format_configuration(
        &self,
        configuration: &ConfigurationDeclaration,
        buffer: &mut String,
    ) {
        self.format_context_clause(&configuration.context_clause, buffer);
        if !configuration.context_clause.is_empty() {
            self.newline(buffer);
            self.newline(buffer);
        }
        // configuration cfg of entity_name is
        self.format_token_span(
            TokenSpan::new(
                configuration.span.start_token,
                configuration.span.start_token + 4,
            ),
            buffer,
        );
        self.increase_indentation();
        self.format_declarations(&configuration.decl, buffer);
        if !configuration.vunit_bind_inds.is_empty() {
            unimplemented!()
        }
        self.newline(buffer);
        self.format_block_configuration(&configuration.block_config, buffer);
        self.decrease_indentation();
        self.newline(buffer);
        self.format_token_span(
            TokenSpan::new(configuration.end_token, configuration.span.end_token - 1),
            buffer,
        );
        self.format_token_id(configuration.span.end_token, buffer);
    }

    pub fn format_block_configuration(&self, config: &BlockConfiguration, buffer: &mut String) {
        if !config.use_clauses.is_empty() {
            unreachable!("Not implemented on AST side")
        }
        // for
        self.format_token_id(config.span.start_token, buffer);
        buffer.push(' ');
        self.format_name(config.block_spec.as_ref(), buffer);
        self.increase_indentation();
        for item in &config.items {
            self.newline(buffer);
            match item {
                ConfigurationItem::Block(block_configuration) => {
                    self.format_block_configuration(block_configuration, buffer)
                }
                ConfigurationItem::Component(_) => unimplemented!(),
            }
        }
        self.decrease_indentation();
        self.newline(buffer);
        // end
        self.format_token_id(config.span.end_token - 2, buffer);
        buffer.push(' ');
        // for
        self.format_token_id(config.span.end_token - 1, buffer);
        // ;
        self.format_token_id(config.span.end_token, buffer);
    }
}

#[cfg(test)]
mod test {
    use crate::analysis::tests::Code;
    use crate::formatting::test_utils::check_formatted;

    fn check_design_unit_formatted(input: &str) {
        check_formatted(
            input,
            input,
            Code::design_file,
            |formatter, file, buffer| {
                formatter.format_any_design_unit(&file.design_units[0].1, buffer, true)
            },
        );
    }

    #[test]
    fn check_configuration() {
        check_design_unit_formatted(
            "\
configuration cfg of entity_name is
    for rtl(0)
    end for;
end;",
        );
        check_design_unit_formatted(
            "\
configuration cfg of entity_name is
    for rtl(0)
    end for;
end configuration cfg;",
        );
        check_design_unit_formatted(
            "\
configuration cfg of entity_name is
    use lib.foo.bar;
    use lib2.foo.bar;
    for rtl(0)
    end for;
end configuration cfg;",
        );
        check_design_unit_formatted(
            "\
configuration cfg of entity_name is
    for rtl(0)
        for name(0 to 3)
        end for;
        for other_name
        end for;
    end for;
end configuration cfg;",
        );
        check_design_unit_formatted(
            "\
configuration cfg of entity_name is
    for rtl(0)
        for name(0 to 3)
            for name(7 to 8)
            end for;
        end for;
        for other_name
        end for;
    end for;
end configuration cfg;",
        );
    }
}
