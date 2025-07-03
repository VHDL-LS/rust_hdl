use crate::vhdl_server::{from_lsp_pos, uri_to_file_name, VHDLServer};
use lsp_types::{
    CompletionItem, CompletionItemKind, CompletionList, CompletionParams, Documentation,
    InsertTextFormat, MarkupContent, MarkupKind,
};
use vhdl_lang::ast::{Designator, ObjectClass};
use vhdl_lang::{kind_str, AnyEntKind, Design, EntRef, InterfaceEnt, Overloaded};

impl VHDLServer {
    fn completion_item_to_lsp_item(
        &self,
        item: vhdl_lang::CompletionItem,
    ) -> lsp_types::CompletionItem {
        match item {
            vhdl_lang::CompletionItem::Simple(ent) => entity_to_completion_item(ent),
            vhdl_lang::CompletionItem::Work => CompletionItem {
                label: "work".to_string(),
                detail: Some("work library".to_string()),
                kind: Some(CompletionItemKind::MODULE),
                insert_text: Some("work".to_string()),
                ..Default::default()
            },
            vhdl_lang::CompletionItem::Formal(ent) => {
                let mut item = entity_to_completion_item(ent);
                if self.client_supports_snippets() {
                    item.insert_text_format = Some(InsertTextFormat::SNIPPET);
                    item.insert_text = Some(format!("{} => $1,", item.insert_text.unwrap()));
                }
                item
            }
            vhdl_lang::CompletionItem::Overloaded(desi, count) => CompletionItem {
                label: desi.to_string(),
                detail: Some(format!("+{count} overloaded")),
                kind: match desi {
                    Designator::Identifier(_) => Some(CompletionItemKind::FUNCTION),
                    Designator::OperatorSymbol(_) => Some(CompletionItemKind::OPERATOR),
                    _ => None,
                },
                insert_text: Some(desi.to_string()),
                ..Default::default()
            },
            vhdl_lang::CompletionItem::Keyword(kind) => CompletionItem {
                label: kind_str(kind).to_string(),
                detail: Some(kind_str(kind).to_string()),
                insert_text: Some(kind_str(kind).to_string()),
                kind: Some(CompletionItemKind::KEYWORD),
                ..Default::default()
            },
            vhdl_lang::CompletionItem::Instantiation(ent, architectures) => {
                let work_name = "work";

                let library_names = if let Some(lib_name) = ent.library_name() {
                    vec![work_name.to_string(), lib_name.name().to_string()]
                } else {
                    vec![work_name.to_string()]
                };
                let (region, is_component_instantiation) = match ent.kind() {
                    AnyEntKind::Design(Design::Entity(_, region)) => (region, false),
                    AnyEntKind::Component(region) => (region, true),
                    // should never happen but better return some value instead of crashing
                    _ => return entity_to_completion_item(ent),
                };
                let template = if self.client_supports_snippets() {
                    let mut line = if is_component_instantiation {
                        format!("${{1:{}_inst}}: {}", ent.designator, ent.designator)
                    } else {
                        format!(
                            "${{1:{}_inst}}: entity ${{2|{}|}}.{}",
                            ent.designator,
                            library_names.join(","),
                            ent.designator
                        )
                    };
                    if architectures.len() > 1 {
                        line.push_str("(${3|");
                        for (i, architecture) in architectures.iter().enumerate() {
                            line.push_str(&architecture.designator().to_string());
                            if i != architectures.len() - 1 {
                                line.push(',')
                            }
                        }
                        line.push_str("|})");
                    }
                    let (ports, generics) = region.ports_and_generics();
                    let mut idx = 4;
                    let mut interface_ent = |elements: Vec<InterfaceEnt>, purpose: &str| {
                        line += &*format!("\n {purpose} map(\n");
                        for (i, generic) in elements.iter().enumerate() {
                            line += &*format!(
                                "    {} => ${{{}:{}}}",
                                generic.designator, idx, generic.designator
                            );
                            idx += 1;
                            if i != elements.len() - 1 {
                                line += ","
                            }
                            line += "\n";
                        }
                        line += ")";
                    };
                    if !generics.is_empty() {
                        interface_ent(generics, "generic");
                    }
                    if !ports.is_empty() {
                        interface_ent(ports, "port");
                    }
                    line += ";";
                    line
                } else {
                    format!("{}", ent.designator)
                };
                CompletionItem {
                    label: format!("{} instantiation", ent.designator),
                    insert_text: Some(template),
                    insert_text_format: Some(InsertTextFormat::SNIPPET),
                    kind: Some(CompletionItemKind::MODULE),
                    ..Default::default()
                }
            }
            vhdl_lang::CompletionItem::Attribute(attribute) => CompletionItem {
                label: format!("{attribute}"),
                detail: Some(format!("{attribute}")),
                insert_text: Some(format!("{attribute}")),
                kind: Some(CompletionItemKind::REFERENCE),
                ..Default::default()
            },
        }
    }

    /// Called when the client requests a completion.
    /// This function looks in the source code to find suitable options and then returns them
    pub fn request_completion(&mut self, params: &CompletionParams) -> CompletionList {
        let binding = uri_to_file_name(&params.text_document_position.text_document.uri);
        let file = binding.as_path();
        // 1) get source position, and source file
        let Some(source) = self.project.get_source(file) else {
            // Do not enable completions for files that are not part of the project
            return CompletionList {
                ..Default::default()
            };
        };
        let cursor = from_lsp_pos(params.text_document_position.position);
        // 2) Optimization chance: go to last recognizable token before the cursor. For example:
        //    - Any primary unit (e.g. entity declaration, package declaration, ...)
        //      => keyword `entity`, `package`, ...
        //    - Any secondary unit (e.g. package body, architecture)
        //      => keyword `architecture`, ...

        // 3) Run the parser until the point of the cursor. Then exit with possible completions
        let options = self
            .project
            .list_completion_options(&source, cursor)
            .into_iter()
            .map(|item| self.completion_item_to_lsp_item(item))
            .collect();

        CompletionList {
            items: options,
            is_incomplete: true,
        }
    }

    pub fn resolve_completion_item(&mut self, params: &CompletionItem) -> CompletionItem {
        let mut params = params.clone();
        let eid = params
            .data
            .clone()
            .and_then(|val| serde_json::from_value::<usize>(val).ok())
            .and_then(|raw| self.project.entity_id_from_raw(raw));
        if let Some(id) = eid {
            if let Some(text) = self.project.format_entity(id) {
                params.documentation = Some(Documentation::MarkupContent(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: format!("```vhdl\n{text}\n```"),
                }));
            }
        }
        params
    }
}

fn entity_to_completion_item(ent: EntRef) -> CompletionItem {
    CompletionItem {
        label: ent.designator.to_string(),
        detail: Some(ent.describe()),
        kind: Some(entity_kind_to_completion_kind(ent.kind())),
        data: serde_json::to_value(ent.id.to_raw()).ok(),
        insert_text: Some(ent.designator.to_string()),
        ..Default::default()
    }
}

fn entity_kind_to_completion_kind(kind: &AnyEntKind) -> CompletionItemKind {
    match kind {
        AnyEntKind::ExternalAlias { .. } | AnyEntKind::ObjectAlias { .. } => {
            CompletionItemKind::FIELD
        }
        AnyEntKind::File(_) | AnyEntKind::InterfaceFile(_) => CompletionItemKind::FILE,
        AnyEntKind::Component(_) => CompletionItemKind::MODULE,
        AnyEntKind::Attribute(_) => CompletionItemKind::REFERENCE,
        AnyEntKind::Overloaded(overloaded) => match overloaded {
            Overloaded::SubprogramDecl(_)
            | Overloaded::Subprogram(_)
            | Overloaded::UninstSubprogramDecl(..)
            | Overloaded::UninstSubprogram(..)
            | Overloaded::InterfaceSubprogram(_) => CompletionItemKind::FUNCTION,
            Overloaded::EnumLiteral(_) => CompletionItemKind::ENUM_MEMBER,
            Overloaded::Alias(_) => CompletionItemKind::FIELD,
        },
        AnyEntKind::Type(_) => CompletionItemKind::TYPE_PARAMETER,
        AnyEntKind::ElementDeclaration(_) => CompletionItemKind::FIELD,
        AnyEntKind::Concurrent(..) => CompletionItemKind::MODULE,
        AnyEntKind::Sequential(_) => CompletionItemKind::MODULE,
        AnyEntKind::Object(object) => match object.class {
            ObjectClass::Signal => CompletionItemKind::EVENT,
            ObjectClass::Constant => CompletionItemKind::CONSTANT,
            ObjectClass::Variable | ObjectClass::SharedVariable => CompletionItemKind::VARIABLE,
        },
        AnyEntKind::LoopParameter(_) => CompletionItemKind::MODULE,
        AnyEntKind::PhysicalLiteral(_) => CompletionItemKind::UNIT,
        AnyEntKind::DeferredConstant(_) => CompletionItemKind::CONSTANT,
        AnyEntKind::Library => CompletionItemKind::MODULE,
        AnyEntKind::Design(_) => CompletionItemKind::MODULE,
        AnyEntKind::View(_) => CompletionItemKind::INTERFACE,
    }
}
