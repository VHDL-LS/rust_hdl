// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use super::common::check_end_identifier_mismatch;
use super::common::ParseResult;
use super::concurrent_statement::parse_generic_and_port_map;
use super::context::parse_use_clause;
use super::names::{parse_name, parse_selected_name};
use super::tokens::{Kind::*, TokenSpan, TokenStream};
use crate::ast::*;
use crate::data::*;

/// LRM 7.3.2.2
fn parse_entity_aspect(stream: &TokenStream) -> ParseResult<EntityAspect> {
    let entity_aspect = expect_token!(
        stream,
        token,
        Open => EntityAspect::Open,
        Configuration => EntityAspect::Configuration(parse_selected_name(stream)?),
        Entity => {
            let entity_name = parse_selected_name(stream)?;
            let arch_name = {
                if stream.skip_if_kind(LeftPar) {
                    let ident = stream.expect_ident()?;
                    stream.expect_kind(RightPar)?;
                    Some(ident)
                } else {
                    None
                }
            };
            EntityAspect::Entity(entity_name, arch_name)
        }
    );
    Ok(entity_aspect)
}

fn parse_binding_indication_known_entity_aspect(
    entity_aspect: Option<EntityAspect>,
    stream: &TokenStream,
    diagnostics: &mut dyn DiagnosticHandler,
) -> ParseResult<BindingIndication> {
    let (generic_map, port_map) = parse_generic_and_port_map(stream, diagnostics)?;

    stream.expect_kind(SemiColon)?;
    Ok(BindingIndication {
        entity_aspect,
        generic_map,
        port_map,
    })
}

/// LRM 7.3.2
fn parse_binding_indication(
    stream: &TokenStream,
    diagnostics: &mut dyn DiagnosticHandler,
) -> ParseResult<BindingIndication> {
    let entity_aspect = if stream.skip_if_kind(Use) {
        Some(parse_entity_aspect(stream)?)
    } else {
        None
    };
    parse_binding_indication_known_entity_aspect(entity_aspect, stream, diagnostics)
}

fn parse_component_configuration_known_spec(
    stream: &TokenStream,
    spec: ComponentSpecification,
    diagnostics: &mut dyn DiagnosticHandler,
) -> ParseResult<ComponentConfiguration> {
    let (bind_ind, vunit_bind_inds) = peek_token!(
        stream,
        token,
        End => (None, Vec::new()),
        For => (None, Vec::new()),
        Use => {
            stream.skip();
            if stream.peek_kind() == Some(Vunit) {
                let vunit_bind_inds = parse_vunit_binding_indication_list_known_keyword(stream)?;
                (None, vunit_bind_inds)
            } else {
                let aspect = parse_entity_aspect(stream)?;
                let bind_ind = parse_binding_indication_known_entity_aspect(Some(aspect), stream, diagnostics)?;

                if stream.skip_if_kind(Use) {
                    (Some(bind_ind), parse_vunit_binding_indication_list_known_keyword(stream)?)
                } else {
                    (Some(bind_ind), Vec::new())
                }
            }
        }
    );

    let block_config = expect_token!(
        stream,
        token,
        End => None,
        For => {
            let block_config = parse_block_configuration_known_keyword(stream, diagnostics)?;
            stream.expect_kind(End)?;
            Some(block_config)
        }
    );

    stream.expect_kind(For)?;
    stream.expect_kind(SemiColon)?;
    Ok(ComponentConfiguration {
        spec,
        bind_ind,
        vunit_bind_inds,
        block_config,
    })
}

enum ComponentSpecificationOrName {
    ComponentSpec(ComponentSpecification),
    Name(WithPos<Name>),
}

fn parse_component_specification_or_name(
    stream: &TokenStream,
) -> ParseResult<ComponentSpecificationOrName> {
    peek_token!(
        stream, token,
        All => {
            stream.skip();
            stream.expect_kind(Colon)?;
            let component_name = parse_selected_name(stream)?;
            Ok(ComponentSpecificationOrName::ComponentSpec(ComponentSpecification {
                instantiation_list: InstantiationList::All,
                component_name,
            }))

        },
        Others => {
            stream.skip();
            stream.expect_kind(Colon)?;
            let component_name = parse_selected_name(stream)?;
            Ok(ComponentSpecificationOrName::ComponentSpec(ComponentSpecification {
                instantiation_list: InstantiationList::Others,
                component_name,
            }))
        },
        Identifier => {
            let name = parse_name(stream)?;
            let sep_token = stream.peek_expect()?;
            match sep_token.kind {
                Colon => {
                    stream.skip();
                    let ident = to_simple_name(name)?;
                    let component_name = parse_selected_name(stream)?;
                    Ok(ComponentSpecificationOrName::ComponentSpec(ComponentSpecification {
                        instantiation_list: InstantiationList::Labels(vec![ident]),
                        component_name,
                    }))
                }
                Comma => {
                    stream.skip();
                    let mut idents = vec![to_simple_name(name)?];
                    loop {
                        idents.push(stream.expect_ident()?);
                        expect_token!(
                            stream,
                            next_token,
                            Comma => {},
                            Colon => break
                        );
                    }
                    let component_name = parse_selected_name(stream)?;
                    Ok(ComponentSpecificationOrName::ComponentSpec(ComponentSpecification {
                        instantiation_list: InstantiationList::Labels(idents),
                        component_name,
                    }))
                }
                _ => Ok(ComponentSpecificationOrName::Name(name))
            }
        }
    )
}

fn parse_configuration_item_known_keyword(
    stream: &TokenStream,
    diagnostics: &mut dyn DiagnosticHandler,
) -> ParseResult<ConfigurationItem> {
    match parse_component_specification_or_name(stream)? {
        ComponentSpecificationOrName::ComponentSpec(component_spec) => {
            Ok(ConfigurationItem::Component(
                parse_component_configuration_known_spec(stream, component_spec, diagnostics)?,
            ))
        }
        ComponentSpecificationOrName::Name(name) => Ok(ConfigurationItem::Block(
            parse_block_configuration_known_name(stream, name, diagnostics)?,
        )),
    }
}

fn parse_block_configuration_known_name(
    stream: &TokenStream,
    name: WithPos<Name>,
    diagnostics: &mut dyn DiagnosticHandler,
) -> ParseResult<BlockConfiguration> {
    let block_spec = name;
    // @TODO use clauses
    let use_clauses = Vec::new();
    let mut items = Vec::new();

    loop {
        expect_token!(
            stream,
            token,
            End => {
                break;
            },
            For => {
                items.push(parse_configuration_item_known_keyword(stream, diagnostics)?);
            }
        );
    }
    stream.expect_kind(For)?;
    stream.expect_kind(SemiColon)?;
    Ok(BlockConfiguration {
        block_spec,
        use_clauses,
        items,
    })
}

fn parse_block_configuration_known_keyword(
    stream: &TokenStream,
    diagnostics: &mut dyn DiagnosticHandler,
) -> ParseResult<BlockConfiguration> {
    let name = parse_name(stream)?;
    parse_block_configuration_known_name(stream, name, diagnostics)
}

fn parse_vunit_binding_indication_list_known_keyword(
    stream: &TokenStream,
) -> ParseResult<Vec<VUnitBindingIndication>> {
    let mut indications = Vec::new();
    loop {
        stream.expect_kind(Vunit)?;

        let mut vunit_list = Vec::new();

        let vunit_bind_ind = loop {
            vunit_list.push(parse_name(stream)?);
            peek_token!(
                stream, token,
                Comma => {
                    stream.skip();
                },
                SemiColon => {
                    stream.skip();
                    break VUnitBindingIndication { vunit_list };
                }
            );
        };

        indications.push(vunit_bind_ind);

        if !stream.skip_if_kind(Use) {
            break;
        }
    }
    Ok(indications)
}

/// LRM 3.4 Configuration declaration
pub fn parse_configuration_declaration(
    stream: &TokenStream,
    diagnostics: &mut dyn DiagnosticHandler,
) -> ParseResult<ConfigurationDeclaration> {
    let start_token = stream.expect_kind(Configuration)?;
    let ident = WithDecl::new(stream.expect_ident()?);
    stream.expect_kind(Of)?;
    let entity_name = parse_selected_name(stream)?;
    stream.expect_kind(Is)?;
    let mut decl = Vec::new();

    let vunit_bind_inds = loop {
        let token = stream.peek_expect()?;
        match token.kind {
            Use => {
                if stream.nth_kind_is(1, Vunit) {
                    stream.skip();
                    break parse_vunit_binding_indication_list_known_keyword(stream)?;
                }

                decl.push(ConfigurationDeclarativeItem::Use(parse_use_clause(
                    stream,
                    diagnostics,
                )?));
            }
            _ => break Vec::new(),
        }
    };

    stream.expect_kind(For)?;
    let block_config = parse_block_configuration_known_keyword(stream, diagnostics)?;

    stream.expect_kind(End)?;
    stream.pop_if_kind(Configuration);
    let end_ident = stream.pop_optional_ident();
    let end_token = stream.expect_kind(SemiColon)?;

    Ok(ConfigurationDeclaration {
        span: TokenSpan::new(start_token, end_token),
        context_clause: ContextClause::default(),
        end_ident_pos: check_end_identifier_mismatch(&ident.tree, end_ident, diagnostics),
        ident,
        entity_name,
        decl,
        vunit_bind_inds,
        block_config,
    })
}

/// LRM 7.3 Configuration Specification
pub fn parse_configuration_specification(
    stream: &TokenStream,
    diagnsotics: &mut dyn DiagnosticHandler,
) -> ParseResult<ConfigurationSpecification> {
    let start_token = stream.expect_kind(For)?;
    match parse_component_specification_or_name(stream)? {
        ComponentSpecificationOrName::ComponentSpec(spec) => {
            let bind_ind = parse_binding_indication(stream, diagnsotics)?;
            if stream.skip_if_kind(Use) {
                let vunit_bind_inds = parse_vunit_binding_indication_list_known_keyword(stream)?;
                stream.expect_kind(End)?;
                stream.expect_kind(For)?;
                let end_token = stream.expect_kind(SemiColon)?;
                Ok(ConfigurationSpecification {
                    span: TokenSpan::new(start_token, end_token),
                    spec,
                    bind_ind,
                    vunit_bind_inds,
                })
            } else {
                if stream.skip_if_kind(End) {
                    stream.expect_kind(For)?;
                    stream.expect_kind(SemiColon)?;
                }
                let end_token = stream.get_last_token_id();
                Ok(ConfigurationSpecification {
                    span: TokenSpan::new(start_token, end_token),
                    spec,
                    bind_ind,
                    vunit_bind_inds: Vec::new(),
                })
            }
        }
        ComponentSpecificationOrName::Name(name) => {
            Err(Diagnostic::error(name, "Expected component specification"))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::test::Code;

    #[test]
    fn empty_configuration() {
        let code = Code::new(
            "\
configuration cfg of entity_name is
  for rtl(0)
  end for;
end;
",
        );
        assert_eq!(
            code.with_stream_no_diagnostics(parse_configuration_declaration),
            ConfigurationDeclaration {
                span: code.token_span(),
                context_clause: ContextClause::default(),
                ident: code.s1("cfg").decl_ident(),
                entity_name: code.s1("entity_name").selected_name(),
                decl: vec![],
                vunit_bind_inds: Vec::new(),
                block_config: BlockConfiguration {
                    block_spec: code.s1("rtl(0)").name(),
                    use_clauses: vec![],
                    items: vec![],
                },
                end_ident_pos: None,
            }
        );
    }

    #[test]
    fn empty_configuration_variant() {
        let code = Code::new(
            "\
configuration cfg of entity_name is
  for rtl(0)
  end for;
end configuration cfg;
",
        );
        assert_eq!(
            code.with_stream_no_diagnostics(parse_configuration_declaration),
            ConfigurationDeclaration {
                span: code.token_span(),
                context_clause: ContextClause::default(),
                ident: code.s1("cfg").decl_ident(),
                entity_name: code.s1("entity_name").selected_name(),
                decl: vec![],
                vunit_bind_inds: Vec::new(),
                block_config: BlockConfiguration {
                    block_spec: code.s1("rtl(0)").name(),
                    use_clauses: vec![],
                    items: vec![],
                },
                end_ident_pos: Some(code.s("cfg", 2).pos())
            }
        );
    }
    #[test]
    fn configuration_use_clause() {
        let code = Code::new(
            "\
configuration cfg of entity_name is
  use lib.foo.bar;
  use lib2.foo.bar;
  for rtl(0)
  end for;
end configuration cfg;
",
        );
        assert_eq!(
            code.with_stream_no_diagnostics(parse_configuration_declaration),
            ConfigurationDeclaration {
                span: code.token_span(),
                context_clause: ContextClause::default(),
                ident: code.s1("cfg").decl_ident(),
                entity_name: code.s1("entity_name").selected_name(),
                decl: vec![
                    ConfigurationDeclarativeItem::Use(code.s1("use lib.foo.bar;").use_clause()),
                    ConfigurationDeclarativeItem::Use(code.s1("use lib2.foo.bar;").use_clause())
                ],
                vunit_bind_inds: Vec::new(),
                block_config: BlockConfiguration {
                    block_spec: code.s1("rtl(0)").name(),
                    use_clauses: vec![],
                    items: vec![],
                },
                end_ident_pos: Some(code.s("cfg", 2).pos())
            }
        );
    }

    #[test]
    fn configuration_vunit_binding_indication() {
        let code = Code::new(
            "\
configuration cfg of entity_name is
  use lib.foo.bar;
  use vunit baz.foobar;
  for rtl(0)
  end for;
end configuration cfg;
",
        );
        assert_eq!(
            code.with_stream_no_diagnostics(parse_configuration_declaration),
            ConfigurationDeclaration {
                span: code.token_span(),
                context_clause: ContextClause::default(),
                ident: code.s1("cfg").decl_ident(),
                entity_name: code.s1("entity_name").selected_name(),
                decl: vec![ConfigurationDeclarativeItem::Use(
                    code.s1("use lib.foo.bar;").use_clause()
                ),],
                vunit_bind_inds: vec![VUnitBindingIndication {
                    vunit_list: vec![code.s1("baz.foobar").name()]
                }],
                block_config: BlockConfiguration {
                    block_spec: code.s1("rtl(0)").name(),
                    use_clauses: vec![],
                    items: vec![],
                },
                end_ident_pos: Some(code.s("cfg", 2).pos())
            }
        );
    }

    #[test]
    fn configuration_block_configuration() {
        let code = Code::new(
            "\
configuration cfg of entity_name is
  for rtl(0)
  end for;
end configuration cfg;
",
        );
        assert_eq!(
            code.with_stream_no_diagnostics(parse_configuration_declaration),
            ConfigurationDeclaration {
                span: code.token_span(),
                context_clause: ContextClause::default(),
                ident: code.s1("cfg").decl_ident(),
                entity_name: code.s1("entity_name").selected_name(),
                decl: vec![],
                vunit_bind_inds: Vec::new(),
                block_config: BlockConfiguration {
                    block_spec: code.s1("rtl(0)").name(),
                    use_clauses: vec![],
                    items: vec![],
                },
                end_ident_pos: Some(code.s("cfg", 2).pos())
            }
        );
    }

    #[test]
    fn configuration_nested_block_configuration() {
        let code = Code::new(
            "\
configuration cfg of entity_name is
  for rtl(0)
    for name(0 to 3)
    end for;
    for other_name
    end for;
  end for;
end configuration cfg;
",
        );
        assert_eq!(
            code.with_stream_no_diagnostics(parse_configuration_declaration),
            ConfigurationDeclaration {
                span: code.token_span(),
                context_clause: ContextClause::default(),
                ident: code.s1("cfg").decl_ident(),
                entity_name: code.s1("entity_name").selected_name(),
                decl: vec![],
                vunit_bind_inds: Vec::new(),
                block_config: BlockConfiguration {
                    block_spec: code.s1("rtl(0)").name(),
                    use_clauses: vec![],
                    items: vec![
                        ConfigurationItem::Block(BlockConfiguration {
                            block_spec: code.s1("name(0 to 3)").name(),
                            use_clauses: vec![],
                            items: vec![],
                        }),
                        ConfigurationItem::Block(BlockConfiguration {
                            block_spec: code.s1("other_name").name(),
                            use_clauses: vec![],
                            items: vec![],
                        })
                    ],
                },
                end_ident_pos: Some(code.s("cfg", 2).pos())
            }
        );
    }

    #[test]
    fn configuration_component_configuration_nested() {
        let code = Code::new(
            "\
configuration cfg of entity_name is
  for rtl(0)
    for inst : lib.pkg.comp
      for arch
      end for;
    end for;
  end for;
end configuration cfg;
",
        );
        assert_eq!(
            code.with_stream_no_diagnostics(parse_configuration_declaration),
            ConfigurationDeclaration {
                span: code.token_span(),
                context_clause: ContextClause::default(),
                ident: code.s1("cfg").decl_ident(),
                entity_name: code.s1("entity_name").selected_name(),
                decl: vec![],
                vunit_bind_inds: Vec::new(),
                block_config: BlockConfiguration {
                    block_spec: code.s1("rtl(0)").name(),
                    use_clauses: vec![],
                    items: vec![ConfigurationItem::Component(ComponentConfiguration {
                        spec: ComponentSpecification {
                            instantiation_list: InstantiationList::Labels(vec![code
                                .s1("inst")
                                .ident()]),
                            component_name: code.s1("lib.pkg.comp").selected_name()
                        },
                        bind_ind: None,
                        vunit_bind_inds: Vec::new(),
                        block_config: Some(BlockConfiguration {
                            block_spec: code.s1("arch").name(),
                            use_clauses: vec![],
                            items: vec![],
                        }),
                    }),],
                },
                end_ident_pos: Some(code.s("cfg", 2).pos())
            }
        );
    }

    #[test]
    fn configuration_component_configuration_vunit_binding_indication() {
        let code = Code::new(
            "\
configuration cfg of entity_name is
  for rtl(0)
    for inst : lib.pkg.comp
      use entity work.bar;
      use vunit baz;
      for arch
      end for;
    end for;
  end for;
end configuration cfg;
",
        );
        assert_eq!(
            code.with_stream_no_diagnostics(parse_configuration_declaration),
            ConfigurationDeclaration {
                span: code.token_span(),
                context_clause: ContextClause::default(),
                ident: code.s1("cfg").decl_ident(),
                entity_name: code.s1("entity_name").selected_name(),
                decl: vec![],
                vunit_bind_inds: Vec::new(),
                block_config: BlockConfiguration {
                    block_spec: code.s1("rtl(0)").name(),
                    use_clauses: vec![],
                    items: vec![ConfigurationItem::Component(ComponentConfiguration {
                        spec: ComponentSpecification {
                            instantiation_list: InstantiationList::Labels(vec![code
                                .s1("inst")
                                .ident()]),
                            component_name: code.s1("lib.pkg.comp").selected_name()
                        },
                        bind_ind: Some(BindingIndication {
                            entity_aspect: Some(EntityAspect::Entity(
                                code.s1("work.bar").selected_name(),
                                None
                            )),
                            generic_map: None,
                            port_map: None
                        }),
                        vunit_bind_inds: vec![VUnitBindingIndication {
                            vunit_list: vec![code.s1("baz").name()]
                        },],
                        block_config: Some(BlockConfiguration {
                            block_spec: code.s1("arch").name(),
                            use_clauses: vec![],
                            items: vec![],
                        }),
                    }),],
                },
                end_ident_pos: Some(code.s("cfg", 2).pos())
            }
        );
    }

    #[test]
    fn configuration_component_configuration_binding_indication() {
        let code = Code::new(
            "\
configuration cfg of entity_name is
  for rtl(0)
    for inst : lib.pkg.comp
      use entity lib.use_name;
    end for;
  end for;
end configuration cfg;
",
        );
        assert_eq!(
            code.with_stream_no_diagnostics(parse_configuration_declaration),
            ConfigurationDeclaration {
                span: code.token_span(),
                context_clause: ContextClause::default(),
                ident: code.s1("cfg").decl_ident(),
                entity_name: code.s1("entity_name").selected_name(),
                decl: vec![],
                vunit_bind_inds: Vec::new(),
                block_config: BlockConfiguration {
                    block_spec: code.s1("rtl(0)").name(),
                    use_clauses: vec![],
                    items: vec![ConfigurationItem::Component(ComponentConfiguration {
                        spec: ComponentSpecification {
                            instantiation_list: InstantiationList::Labels(vec![code
                                .s1("inst")
                                .ident()]),
                            component_name: code.s1("lib.pkg.comp").selected_name()
                        },
                        bind_ind: Some(BindingIndication {
                            entity_aspect: Some(EntityAspect::Entity(
                                code.s1("lib.use_name").selected_name(),
                                None
                            )),
                            generic_map: None,
                            port_map: None,
                        }),
                        vunit_bind_inds: Vec::new(),
                        block_config: None,
                    }),],
                },
                end_ident_pos: Some(code.s("cfg", 2).pos())
            }
        );
    }

    #[test]
    fn configuration_component_configuration() {
        let code = Code::new(
            "\
configuration cfg of entity_name is
  for rtl(0)
    for inst : lib.pkg.comp
    end for;
    for inst1, inst2, inst3 : lib2.pkg.comp
    end for;
    for all : lib3.pkg.comp
    end for;
    for others : lib4.pkg.comp
    end for;
  end for;
end configuration cfg;
",
        );
        assert_eq!(
            code.with_stream_no_diagnostics(parse_configuration_declaration),
            ConfigurationDeclaration {
                span: code.token_span(),
                context_clause: ContextClause::default(),
                ident: code.s1("cfg").decl_ident(),
                entity_name: code.s1("entity_name").selected_name(),
                decl: vec![],
                vunit_bind_inds: Vec::new(),
                block_config: BlockConfiguration {
                    block_spec: code.s1("rtl(0)").name(),
                    use_clauses: vec![],
                    items: vec![
                        ConfigurationItem::Component(ComponentConfiguration {
                            spec: ComponentSpecification {
                                instantiation_list: InstantiationList::Labels(vec![code
                                    .s1("inst")
                                    .ident()]),
                                component_name: code.s1("lib.pkg.comp").selected_name()
                            },
                            bind_ind: None,
                            vunit_bind_inds: Vec::new(),
                            block_config: None,
                        }),
                        ConfigurationItem::Component(ComponentConfiguration {
                            spec: ComponentSpecification {
                                instantiation_list: InstantiationList::Labels(vec![
                                    code.s1("inst1").ident(),
                                    code.s1("inst2").ident(),
                                    code.s1("inst3").ident()
                                ]),
                                component_name: code.s1("lib2.pkg.comp").selected_name()
                            },
                            bind_ind: None,
                            vunit_bind_inds: Vec::new(),
                            block_config: None,
                        }),
                        ConfigurationItem::Component(ComponentConfiguration {
                            spec: ComponentSpecification {
                                instantiation_list: InstantiationList::All,
                                component_name: code.s1("lib3.pkg.comp").selected_name()
                            },
                            bind_ind: None,
                            vunit_bind_inds: Vec::new(),
                            block_config: None,
                        }),
                        ConfigurationItem::Component(ComponentConfiguration {
                            spec: ComponentSpecification {
                                instantiation_list: InstantiationList::Others,
                                component_name: code.s1("lib4.pkg.comp").selected_name()
                            },
                            bind_ind: None,
                            vunit_bind_inds: Vec::new(),
                            block_config: None,
                        })
                    ],
                },
                end_ident_pos: Some(code.s("cfg", 2).pos())
            }
        );
    }

    #[test]
    fn entity_entity_aspect_entity() {
        let code = Code::new("entity lib.foo.name");
        assert_eq!(
            code.with_stream(parse_entity_aspect),
            EntityAspect::Entity(code.s1("lib.foo.name").selected_name(), None)
        );
    }

    #[test]
    fn entity_entity_aspect_entity_arch() {
        let code = Code::new("entity lib.foo.name(arch)");
        assert_eq!(
            code.with_stream(parse_entity_aspect),
            EntityAspect::Entity(
                code.s1("lib.foo.name").selected_name(),
                Some(code.s1("arch").ident())
            )
        );
    }

    #[test]
    fn entity_entity_aspect_configuration() {
        let code = Code::new("configuration lib.foo.name");
        assert_eq!(
            code.with_stream(parse_entity_aspect),
            EntityAspect::Configuration(code.s1("lib.foo.name").selected_name())
        );
    }

    #[test]
    fn entity_entity_aspect_open() {
        let code = Code::new("open");
        assert_eq!(code.with_stream(parse_entity_aspect), EntityAspect::Open);
    }

    #[test]
    fn simple_configuration_specification() {
        let code = Code::new("for all : lib.pkg.comp use entity work.foo(rtl);");

        assert_eq!(
            code.with_stream_no_diagnostics(parse_configuration_specification),
            ConfigurationSpecification {
                span: code.token_span(),
                spec: ComponentSpecification {
                    instantiation_list: InstantiationList::All,
                    component_name: code.s1("lib.pkg.comp").selected_name(),
                },
                bind_ind: BindingIndication {
                    entity_aspect: Some(EntityAspect::Entity(
                        code.s1("work.foo").selected_name(),
                        Some(code.s1("rtl").ident())
                    )),
                    generic_map: None,
                    port_map: None
                },
                vunit_bind_inds: Vec::new()
            }
        );
    }

    #[test]
    fn simple_configuration_specification_end_for() {
        let code = Code::new("for all : lib.pkg.comp use entity work.foo(rtl); end for;");

        assert_eq!(
            code.with_stream_no_diagnostics(parse_configuration_specification),
            ConfigurationSpecification {
                span: code.token_span(),
                spec: ComponentSpecification {
                    instantiation_list: InstantiationList::All,
                    component_name: code.s1("lib.pkg.comp").selected_name(),
                },
                bind_ind: BindingIndication {
                    entity_aspect: Some(EntityAspect::Entity(
                        code.s1("work.foo").selected_name(),
                        Some(code.s1("rtl").ident())
                    )),
                    generic_map: None,
                    port_map: None
                },
                vunit_bind_inds: Vec::new()
            }
        );
    }

    #[test]
    fn compound_configuration_specification() {
        let code = Code::new(
            "for all : lib.pkg.comp use entity work.foo(rtl); use vunit bar, baz; end for;",
        );

        assert_eq!(
            code.with_stream_no_diagnostics(parse_configuration_specification),
            ConfigurationSpecification {
                span: code.token_span(),
                spec: ComponentSpecification {
                    instantiation_list: InstantiationList::All,
                    component_name: code.s1("lib.pkg.comp").selected_name(),
                },
                bind_ind: BindingIndication {
                    entity_aspect: Some(EntityAspect::Entity(
                        code.s1("work.foo").selected_name(),
                        Some(code.s1("rtl").ident())
                    )),
                    generic_map: None,
                    port_map: None
                },
                vunit_bind_inds: vec![VUnitBindingIndication {
                    vunit_list: vec![code.s1("bar").name(), code.s1("baz").name()]
                }],
            }
        );
    }
}
