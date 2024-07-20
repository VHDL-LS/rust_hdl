use crate::ast::{
    BindingIndication, BlockConfiguration, ComponentSpecification, ConfigurationDeclaration,
    ConfigurationItem, ConfigurationSpecification, EntityAspect, VUnitBindingIndication,
};
use crate::formatting::buffer::Buffer;
use crate::syntax::Kind;
use crate::{TokenAccess, TokenSpan, VHDLFormatter};
use vhdl_lang::ast::{ComponentConfiguration, InstantiationList};

impl VHDLFormatter<'_> {
    pub fn format_configuration(
        &self,
        configuration: &ConfigurationDeclaration,
        buffer: &mut Buffer,
    ) {
        self.format_context_clause(&configuration.context_clause, buffer);
        if !configuration.context_clause.is_empty() {
            buffer.line_breaks(2);
        }
        // configuration cfg of entity_name is
        self.format_token_span(
            TokenSpan::new(
                configuration.span.start_token,
                configuration.span.start_token + 4,
            ),
            buffer,
        );
        buffer.increase_indent();
        self.format_declarations(&configuration.decl, buffer);
        self.format_v_unit_binding_indications(&configuration.vunit_bind_inds, buffer);
        buffer.line_break();
        self.format_block_configuration(&configuration.block_config, buffer);
        buffer.decrease_indent();
        buffer.line_break();
        self.format_token_span(
            TokenSpan::new(configuration.end_token, configuration.span.end_token - 1),
            buffer,
        );
        self.format_token_id(configuration.span.end_token, buffer);
    }

    pub fn format_v_unit_binding_indications(
        &self,
        vunits: &[VUnitBindingIndication],
        buffer: &mut Buffer,
    ) {
        for vunit_bind_ind in vunits {
            buffer.line_break();
            self.format_v_unit_indication(vunit_bind_ind, buffer);
        }
    }

    pub fn format_block_configuration(&self, config: &BlockConfiguration, buffer: &mut Buffer) {
        if !config.use_clauses.is_empty() {
            unreachable!("Not implemented on AST side")
        }
        // for
        self.format_token_id(config.span.start_token, buffer);
        buffer.push_whitespace();
        self.format_name(config.block_spec.as_ref(), buffer);
        buffer.increase_indent();
        for item in &config.items {
            buffer.line_break();
            match item {
                ConfigurationItem::Block(block_configuration) => {
                    self.format_block_configuration(block_configuration, buffer)
                }
                ConfigurationItem::Component(component_configuration) => {
                    self.format_component_configuration(component_configuration, buffer)
                }
            }
        }
        buffer.decrease_indent();
        buffer.line_break();
        // end
        self.format_token_id(config.span.end_token - 2, buffer);
        buffer.push_whitespace();
        // for
        self.format_token_id(config.span.end_token - 1, buffer);
        // ;
        self.format_token_id(config.span.end_token, buffer);
    }

    pub fn format_component_configuration(
        &self,
        config: &ComponentConfiguration,
        buffer: &mut Buffer,
    ) {
        self.format_component_specification(&config.spec, buffer);
        buffer.increase_indent();
        if let Some(binding_indication) = &config.bind_ind {
            buffer.line_break();
            self.format_binding_indication(binding_indication, buffer)
        }
        self.format_v_unit_binding_indications(&config.vunit_bind_inds, buffer);
        if let Some(block_configuration) = &config.block_config {
            buffer.line_break();
            self.format_block_configuration(block_configuration, buffer);
        }
        buffer.decrease_indent();
        buffer.line_break();
        // end
        self.format_token_id(config.span.end_token - 2, buffer);
        buffer.push_whitespace();
        // for
        self.format_token_id(config.span.end_token - 1, buffer);
        // ;
        self.format_token_id(config.span.end_token, buffer);
    }

    pub fn format_binding_indication(&self, indication: &BindingIndication, buffer: &mut Buffer) {
        // use
        self.format_token_id(indication.span.start_token, buffer);
        if let Some(aspect) = &indication.entity_aspect {
            buffer.push_whitespace();
            self.format_token_id(indication.span.start_token + 1, buffer);
            buffer.push_whitespace();
            match aspect {
                EntityAspect::Entity(entity, architecture) => {
                    self.format_name(entity.as_ref(), buffer);
                    if let Some(arch) = architecture {
                        self.format_token_id(arch.token - 1, buffer);
                        self.format_token_id(arch.token, buffer);
                        self.format_token_id(arch.token + 1, buffer);
                    }
                }
                EntityAspect::Configuration(config) => {
                    self.format_name(config.as_ref(), buffer);
                }
                EntityAspect::Open => {}
            }
        }
        if let Some(map_aspect) = &indication.generic_map {
            buffer.increase_indent();
            buffer.line_break();
            self.format_map_aspect(map_aspect, buffer);
            buffer.decrease_indent();
        }
        if let Some(map_aspect) = &indication.port_map {
            buffer.increase_indent();
            buffer.line_break();
            self.format_map_aspect(map_aspect, buffer);
            buffer.decrease_indent();
        }
        self.format_token_id(indication.span.end_token, buffer);
    }

    pub fn format_configuration_specification(
        &self,
        configuration: &ConfigurationSpecification,
        buffer: &mut Buffer,
    ) {
        self.format_component_specification(&configuration.spec, buffer);
        buffer.increase_indent();
        buffer.line_break();
        self.format_binding_indication(&configuration.bind_ind, buffer);
        self.format_v_unit_binding_indications(&configuration.vunit_bind_inds, buffer);
        buffer.decrease_indent();

        if let Some(end_token) = configuration.end_token {
            buffer.line_break();
            self.format_token_id(end_token, buffer);
            buffer.push_whitespace();
            self.format_token_id(end_token + 1, buffer);
            self.format_token_id(configuration.span.end_token, buffer);
        }
    }

    pub fn format_component_specification(
        &self,
        spec: &ComponentSpecification,
        buffer: &mut Buffer,
    ) {
        // for
        self.format_token_id(spec.span.start_token, buffer);
        buffer.push_whitespace();
        match &spec.instantiation_list {
            InstantiationList::Labels(labels) => self.format_ident_list(labels, buffer),
            InstantiationList::Others => self.format_token_id(spec.span.start_token + 1, buffer),
            InstantiationList::All => self.format_token_id(spec.span.start_token + 1, buffer),
        }
        // :
        self.format_token_id(spec.colon_token, buffer);
        buffer.push_whitespace();
        self.format_name(spec.component_name.as_ref(), buffer);
    }

    pub fn format_v_unit_indication(
        &self,
        vunit_binding_indication: &VUnitBindingIndication,
        buffer: &mut Buffer,
    ) {
        // use
        self.format_token_id(vunit_binding_indication.span.start_token, buffer);
        buffer.push_whitespace();
        // vunit
        self.format_token_id(vunit_binding_indication.span.start_token + 1, buffer);
        buffer.push_whitespace();
        for vunit in &vunit_binding_indication.vunit_list {
            self.format_name(vunit.as_ref(), buffer);
            if self.tokens.get_token(vunit.span.end_token + 1).kind == Kind::Comma {
                self.format_token_id(vunit.span.end_token + 1, buffer);
                buffer.push_whitespace();
            }
        }
        self.format_token_id(vunit_binding_indication.span.end_token, buffer);
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
        check_design_unit_formatted(
            "\
configuration cfg of entity_name is
    use lib.foo.bar;
    use vunit baz.foobar;
    for rtl(0)
    end for;
end configuration cfg;",
        );
        check_design_unit_formatted(
            "\
configuration cfg of entity_name is
    for rtl(0)
        for inst: lib.pkg.comp
            for arch
            end for;
        end for;
    end for;
end configuration cfg;",
        );
        check_design_unit_formatted(
            "\
configuration cfg of entity_name is
    for rtl(0)
        for inst: lib.pkg.comp
            use entity work.bar;
            use vunit baz;
            for arch
            end for;
        end for;
    end for;
end configuration cfg;",
        );
        check_design_unit_formatted(
            "\
configuration cfg of entity_name is
    for rtl(0)
        for inst: lib.pkg.comp
            use entity lib.use_name;
        end for;
    end for;
end configuration cfg;",
        );
        check_design_unit_formatted(
            "\
configuration cfg of entity_name is
    for rtl(0)
        for inst: lib.pkg.comp
        end for;
        for inst1, inst2, inst3: lib2.pkg.comp
        end for;
        for all: lib3.pkg.comp
        end for;
        for others: lib4.pkg.comp
        end for;
    end for;
end configuration cfg;",
        );
    }

    #[test]
    fn check_entity_aspect() {
        check_design_unit_formatted(
            "\
configuration cfg of entity_name is
    for foo
        for inst: lib.pkg.comp
            use entity lib.use_name;
        end for;
    end for;
end configuration cfg;",
        );
        check_design_unit_formatted(
            "\
configuration cfg of entity_name is
    for foo
        for inst: lib.pkg.comp
            use entity lib.foo.name(arch);
        end for;
    end for;
end configuration cfg;",
        );
        check_design_unit_formatted(
            "\
configuration cfg of entity_name is
    for foo
        for inst: lib.pkg.comp
            use configuration lib.foo.name;
        end for;
    end for;
end configuration cfg;",
        );
    }
}
