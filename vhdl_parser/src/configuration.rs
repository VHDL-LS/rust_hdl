// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use ast::{
    BindingIndication, BlockConfiguration, ComponentConfiguration, ComponentSpecification,
    ConfigurationDeclaration, ConfigurationDeclarativeItem, ConfigurationItem, EntityAspect,
    InstantiationList, Name,
};
use common::error_on_end_identifier_mismatch;
use context::parse_use_clause;
use message::{MessageHandler, ParseResult};
use names::{parse_name, parse_name_initial_token, parse_selected_name, to_simple_name};
use source::WithPos;
use tokenizer::Kind::*;
use tokenstream::TokenStream;

fn parse_entity_aspect(stream: &mut TokenStream) -> ParseResult<EntityAspect> {
    stream.expect_kind(Use)?;
    let token = stream.expect()?;
    let entity_aspect = try_token_kind!(
        token,
        Open => EntityAspect::Open,
        Configuration => EntityAspect::Configuration(parse_selected_name(stream)?),
        Entity => {
            let entity_name = parse_selected_name(stream)?;
            let arch_name = {
                if stream.skip_if_kind(LeftPar)? {
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
    stream.expect_kind(SemiColon)?;
    Ok(entity_aspect)
}

fn parse_component_configuration_known_instance(
    stream: &mut TokenStream,
    instantiation_list: InstantiationList,
    messages: &mut MessageHandler,
) -> ParseResult<ComponentConfiguration> {
    let component_name = parse_selected_name(stream)?;
    let spec = ComponentSpecification {
        instantiation_list,
        component_name,
    };

    let token = stream.peek_expect()?;
    let bind_ind = try_token_kind!(
        token,
        End => None,
        For => None,
        Use => {
            let entity_aspect = Some(parse_entity_aspect(stream)?);
            // @TODO generic map
            let generic_map = None;
            // @TODO port  map
            let port_map = None;
            Some(BindingIndication {
                entity_aspect,
                generic_map,
                port_map
            })
        }
    );

    let token = stream.expect()?;
    let block_config = try_token_kind!(
        token,
        End => None,
        For => {
            let block_config = parse_block_configuration_known_keyword(stream, messages)?;
            stream.expect_kind(End)?;
            Some(block_config)
        }
    );

    stream.expect_kind(For)?;
    stream.expect_kind(SemiColon)?;
    Ok(ComponentConfiguration {
        spec,
        bind_ind,
        block_config,
    })
}

fn parse_block_configuration_known_name(
    stream: &mut TokenStream,
    name: WithPos<Name>,
    messages: &mut MessageHandler,
) -> ParseResult<BlockConfiguration> {
    let block_spec = name;
    // @TODO use clauses
    let use_clauses = Vec::new();
    let mut items = Vec::new();

    loop {
        let token = stream.expect()?;
        try_token_kind!(
            token,
            End => {
                break;
            },
            For => {
                let name_token = stream.expect()?;
                try_token_kind!(
                    name_token,
                    All => {
                        stream.expect_kind(Colon)?;
                        items.push(ConfigurationItem::Component(
                            parse_component_configuration_known_instance(
                                stream, InstantiationList::All, messages)?));
                    },
                    Others => {
                        stream.expect_kind(Colon)?;
                        items.push(ConfigurationItem::Component(
                            parse_component_configuration_known_instance(
                                stream, InstantiationList::Others, messages)?));
                    },
                    Identifier => {
                        let name = parse_name_initial_token(stream, name_token)?;
                        let sep_token = stream.peek_expect()?;
                        match sep_token.kind {
                            Colon => {
                                stream.move_after(&sep_token);
                                let ident = to_simple_name(name)?;
                                items.push(ConfigurationItem::Component(
                                    parse_component_configuration_known_instance(
                                        stream, InstantiationList::Labels(vec![ident]), messages)?));
                            }
                            Comma => {
                                stream.move_after(&sep_token);
                                let mut idents = vec![to_simple_name(name)?];
                                loop {
                                    idents.push(stream.expect_ident()?);
                                    let next_token = stream.expect()?;
                                    try_token_kind!(
                                        next_token,
                                        Comma => {},
                                        Colon => break
                                    );
                                }
                                items.push(ConfigurationItem::Component(
                                    parse_component_configuration_known_instance(
                                        stream, InstantiationList::Labels(idents), messages)?));
                            }
                            _ => {
                                items.push(ConfigurationItem::Block(
                                    parse_block_configuration_known_name(stream, name, messages)?));
                            }
                        }
                    }
                );
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
    stream: &mut TokenStream,
    messages: &mut MessageHandler,
) -> ParseResult<BlockConfiguration> {
    let name = parse_name(stream)?;
    parse_block_configuration_known_name(stream, name, messages)
}

pub fn parse_configuration_declaration(
    stream: &mut TokenStream,
    messages: &mut MessageHandler,
) -> ParseResult<ConfigurationDeclaration> {
    stream.expect_kind(Configuration)?;
    let ident = stream.expect_ident()?;
    stream.expect_kind(Of)?;
    let entity_name = parse_selected_name(stream)?;
    stream.expect_kind(Is)?;
    let mut decl = Vec::new();

    let block_config = loop {
        let token = stream.peek_expect()?;
        try_token_kind!(
            token,
            End => {
                stream.move_after(&token);
                break None;
            },
            Use => {
                decl.push(ConfigurationDeclarativeItem::Use(parse_use_clause(stream)?));
            },
            For => {
                stream.move_after(&token);
                let block_config = parse_block_configuration_known_keyword(stream, messages)?;
                stream.expect_kind(End)?;
                break Some(block_config);
            }
        );
    };

    stream.pop_if_kind(Configuration)?;
    let end_ident = stream.pop_optional_ident()?;
    if let Some(msg) = error_on_end_identifier_mismatch(&ident, &end_ident) {
        messages.push(msg)
    }
    stream.expect_kind(SemiColon)?;
    Ok(ConfigurationDeclaration {
        ident,
        entity_name,
        decl,
        block_config,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_util::{with_stream, with_stream_no_messages};

    #[test]
    fn empty_configuration() {
        let (util, cfg) = with_stream_no_messages(
            parse_configuration_declaration,
            "\
configuration cfg of entity_name is
end;
",
        );
        assert_eq!(
            cfg,
            ConfigurationDeclaration {
                ident: util.ident("cfg"),
                entity_name: util.selected_name("entity_name"),
                decl: vec![],
                block_config: None
            }
        );
    }

    #[test]
    fn empty_configuration_variant() {
        let (util, cfg) = with_stream_no_messages(
            parse_configuration_declaration,
            "\
configuration cfg of entity_name is
end configuration cfg;
",
        );
        assert_eq!(
            cfg,
            ConfigurationDeclaration {
                ident: util.ident("cfg"),
                entity_name: util.selected_name("entity_name"),
                decl: vec![],
                block_config: None
            }
        );
    }
    #[test]
    fn configuration_use_clause() {
        let (util, cfg) = with_stream_no_messages(
            parse_configuration_declaration,
            "\
configuration cfg of entity_name is
  use lib.foo.bar;
  use lib2.foo.bar;
end configuration cfg;
",
        );
        assert_eq!(
            cfg,
            ConfigurationDeclaration {
                ident: util.ident("cfg"),
                entity_name: util.selected_name("entity_name"),
                decl: vec![
                    ConfigurationDeclarativeItem::Use(util.use_clause("use lib.foo.bar;")),
                    ConfigurationDeclarativeItem::Use(util.use_clause("use lib2.foo.bar;"))
                ],
                block_config: None
            }
        );
    }

    #[test]
    fn configuration_block_configuration() {
        let (util, cfg) = with_stream_no_messages(
            parse_configuration_declaration,
            "\
configuration cfg of entity_name is
  for rtl(0)
  end for;
end configuration cfg;
",
        );
        assert_eq!(
            cfg,
            ConfigurationDeclaration {
                ident: util.ident("cfg"),
                entity_name: util.selected_name("entity_name"),
                decl: vec![],
                block_config: Some(BlockConfiguration {
                    block_spec: util.name("rtl(0)"),
                    use_clauses: vec![],
                    items: vec![],
                })
            }
        );
    }

    #[test]
    fn configuration_nested_block_configuration() {
        let (util, cfg) = with_stream_no_messages(
            parse_configuration_declaration,
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
            cfg,
            ConfigurationDeclaration {
                ident: util.ident("cfg"),
                entity_name: util.selected_name("entity_name"),
                decl: vec![],
                block_config: Some(BlockConfiguration {
                    block_spec: util.name("rtl(0)"),
                    use_clauses: vec![],
                    items: vec![
                        ConfigurationItem::Block(BlockConfiguration {
                            block_spec: util.name("name(0 to 3)"),
                            use_clauses: vec![],
                            items: vec![],
                        }),
                        ConfigurationItem::Block(BlockConfiguration {
                            block_spec: util.name("other_name"),
                            use_clauses: vec![],
                            items: vec![],
                        })
                    ],
                })
            }
        );
    }

    #[test]
    fn configuration_component_configuration_nested() {
        let (util, cfg) = with_stream_no_messages(
            parse_configuration_declaration,
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
            cfg,
            ConfigurationDeclaration {
                ident: util.ident("cfg"),
                entity_name: util.selected_name("entity_name"),
                decl: vec![],
                block_config: Some(BlockConfiguration {
                    block_spec: util.name("rtl(0)"),
                    use_clauses: vec![],
                    items: vec![ConfigurationItem::Component(ComponentConfiguration {
                        spec: ComponentSpecification {
                            instantiation_list: InstantiationList::Labels(vec![util.ident("inst")]),
                            component_name: util.selected_name("lib.pkg.comp")
                        },
                        bind_ind: None,
                        block_config: Some(BlockConfiguration {
                            block_spec: util.name("arch"),
                            use_clauses: vec![],
                            items: vec![],
                        }),
                    }),],
                })
            }
        );
    }

    #[test]
    fn configuration_component_configuration_binding_indication() {
        let (util, cfg) = with_stream_no_messages(
            parse_configuration_declaration,
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
            cfg,
            ConfigurationDeclaration {
                ident: util.ident("cfg"),
                entity_name: util.selected_name("entity_name"),
                decl: vec![],
                block_config: Some(BlockConfiguration {
                    block_spec: util.name("rtl(0)"),
                    use_clauses: vec![],
                    items: vec![ConfigurationItem::Component(ComponentConfiguration {
                        spec: ComponentSpecification {
                            instantiation_list: InstantiationList::Labels(vec![util.ident("inst")]),
                            component_name: util.selected_name("lib.pkg.comp")
                        },
                        bind_ind: Some(BindingIndication {
                            entity_aspect: Some(EntityAspect::Entity(
                                util.selected_name("lib.use_name"),
                                None
                            )),
                            generic_map: None,
                            port_map: None,
                        }),
                        block_config: None,
                    }),],
                })
            }
        );
    }

    #[test]
    fn configuration_component_configuration() {
        let (util, cfg) = with_stream_no_messages(
            parse_configuration_declaration,
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
            cfg,
            ConfigurationDeclaration {
                ident: util.ident("cfg"),
                entity_name: util.selected_name("entity_name"),
                decl: vec![],
                block_config: Some(BlockConfiguration {
                    block_spec: util.name("rtl(0)"),
                    use_clauses: vec![],
                    items: vec![
                        ConfigurationItem::Component(ComponentConfiguration {
                            spec: ComponentSpecification {
                                instantiation_list: InstantiationList::Labels(vec![
                                    util.ident("inst")
                                ]),
                                component_name: util.selected_name("lib.pkg.comp")
                            },
                            bind_ind: None,
                            block_config: None,
                        }),
                        ConfigurationItem::Component(ComponentConfiguration {
                            spec: ComponentSpecification {
                                instantiation_list: InstantiationList::Labels(vec![
                                    util.ident("inst1"),
                                    util.ident("inst2"),
                                    util.ident("inst3")
                                ]),
                                component_name: util.selected_name("lib2.pkg.comp")
                            },
                            bind_ind: None,
                            block_config: None,
                        }),
                        ConfigurationItem::Component(ComponentConfiguration {
                            spec: ComponentSpecification {
                                instantiation_list: InstantiationList::All,
                                component_name: util.selected_name("lib3.pkg.comp")
                            },
                            bind_ind: None,
                            block_config: None,
                        }),
                        ConfigurationItem::Component(ComponentConfiguration {
                            spec: ComponentSpecification {
                                instantiation_list: InstantiationList::Others,
                                component_name: util.selected_name("lib4.pkg.comp")
                            },
                            bind_ind: None,
                            block_config: None,
                        })
                    ],
                })
            }
        );
    }

    #[test]
    fn entity_entity_aspect_entity() {
        let (util, binding) = with_stream(parse_entity_aspect, "use entity lib.foo.name;");
        assert_eq!(
            binding,
            EntityAspect::Entity(util.selected_name("lib.foo.name"), None)
        );
    }

    #[test]
    fn entity_entity_aspect_entity_arch() {
        let (util, binding) = with_stream(parse_entity_aspect, "use entity lib.foo.name(arch);");
        assert_eq!(
            binding,
            EntityAspect::Entity(util.selected_name("lib.foo.name"), Some(util.ident("arch")))
        );
    }

    #[test]
    fn entity_entity_aspect_configuration() {
        let (util, binding) = with_stream(parse_entity_aspect, "use configuration lib.foo.name;");
        assert_eq!(
            binding,
            EntityAspect::Configuration(util.selected_name("lib.foo.name"))
        );
    }

    #[test]
    fn entity_entity_aspect_open() {
        let (_, binding) = with_stream(parse_entity_aspect, "use open;");
        assert_eq!(binding, EntityAspect::Open);
    }

}
