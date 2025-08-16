use crate::vhdl_server::{from_lsp_pos, uri_to_file_name, VHDLServer};
use lsp_types::{
    CompletionItem, CompletionItemKind, CompletionList, CompletionParams, Documentation,
    InsertTextFormat, MarkupContent, MarkupKind,
};
use vhdl_lang::ast::{Designator, ObjectClass};
use vhdl_lang::{kind_str, AnyEntKind, Design, EntRef, InterfaceEnt, Overloaded};

impl VHDLServer {
    fn insert_text(&self, val: impl ToString) -> String {
        let mut val = val.to_string();
        if let Some(case) = &self.case_transform {
            case.convert(&mut val)
        }
        val
    }

    fn completion_item_to_lsp_item(&self, item: vhdl_lang::CompletionItem) -> CompletionItem {
        match item {
            vhdl_lang::CompletionItem::Simple(ent) => self.entity_to_completion_item(ent),
            vhdl_lang::CompletionItem::Work => CompletionItem {
                label: self.insert_text("work"),
                detail: Some("work library".to_string()),
                kind: Some(CompletionItemKind::MODULE),
                ..Default::default()
            },
            vhdl_lang::CompletionItem::Formal(ent) => {
                let mut item = self.entity_to_completion_item(ent);
                if self.client_supports_snippets() {
                    item.insert_text_format = Some(InsertTextFormat::SNIPPET);
                    item.insert_text = Some(format!("{} => $1,", item.insert_text.unwrap()));
                }
                item
            }
            vhdl_lang::CompletionItem::Overloaded(desi, count) => {
                let kind = match desi {
                    Designator::Identifier(_) => Some(CompletionItemKind::FUNCTION),
                    Designator::OperatorSymbol(_) => Some(CompletionItemKind::OPERATOR),
                    _ => None,
                };
                CompletionItem {
                    label: self.insert_text(desi),
                    detail: Some(format!("+{count} overloaded")),
                    kind,
                    ..Default::default()
                }
            }
            vhdl_lang::CompletionItem::Keyword(kind) => CompletionItem {
                label: self.insert_text(kind_str(kind)),
                detail: Some(kind_str(kind).to_string()),
                kind: Some(CompletionItemKind::KEYWORD),
                ..Default::default()
            },
            vhdl_lang::CompletionItem::Instantiation(ent, architectures) => {
                let work_name = self.insert_text("work");

                let library_names = if let Some(lib_name) = ent.library_name() {
                    vec![work_name, self.insert_text(lib_name.name())]
                } else {
                    vec![work_name]
                };
                let (region, is_component_instantiation) = match ent.kind() {
                    AnyEntKind::Design(Design::Entity(_, region)) => (region, false),
                    AnyEntKind::Component(region) => (region, true),
                    // should never happen but better return some value instead of crashing
                    _ => return self.entity_to_completion_item(ent),
                };
                let designator = self.insert_text(&ent.designator);
                let template = if self.client_supports_snippets() {
                    let mut line = if is_component_instantiation {
                        format!("${{1:{designator}_inst}}: {designator}",)
                    } else {
                        format!(
                            "${{1:{designator}_inst}}: entity ${{2|{}|}}.{designator}",
                            library_names.join(","),
                        )
                    };
                    if architectures.len() > 1 {
                        line.push_str("(${3|");
                        for (i, architecture) in architectures.iter().enumerate() {
                            line.push_str(&self.insert_text(architecture.designator()));
                            if i != architectures.len() - 1 {
                                line.push(',')
                            }
                        }
                        line.push_str("|})");
                    }
                    let (ports, generics) = region.ports_and_generics();
                    let mut idx = 4;
                    let mut interface_ent = |elements: Vec<InterfaceEnt>, purpose: &str| {
                        line += &*format!("\n {purpose} {}(\n", self.insert_text("map"));
                        for (i, generic) in elements.iter().enumerate() {
                            let generic_designator = self.insert_text(&generic.designator);
                            line += &*format!(
                                "    {generic_designator} => ${{{idx}:{generic_designator}}}",
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
                        interface_ent(generics, &self.insert_text("generic"));
                    }
                    if !ports.is_empty() {
                        interface_ent(ports, &self.insert_text("port"));
                    }
                    line += ";";
                    line
                } else {
                    designator.clone()
                };
                CompletionItem {
                    label: format!("{designator} instantiation"),
                    insert_text: Some(template),
                    insert_text_format: Some(InsertTextFormat::SNIPPET),
                    kind: Some(CompletionItemKind::MODULE),
                    ..Default::default()
                }
            }
            vhdl_lang::CompletionItem::Attribute(attribute) => CompletionItem {
                label: self.insert_text(attribute),
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

    fn entity_to_completion_item(&self, ent: EntRef) -> CompletionItem {
        CompletionItem {
            label: self.insert_text(&ent.designator),
            detail: Some(ent.describe()),
            kind: Some(entity_kind_to_completion_kind(ent.kind())),
            data: serde_json::to_value(ent.id.to_raw()).ok(),
            ..Default::default()
        }
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
