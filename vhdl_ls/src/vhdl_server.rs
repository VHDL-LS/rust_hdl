// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use lsp_types::*;

use fnv::FnvHashMap;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use vhdl_lang::ast::{Designator, ObjectClass};

use crate::rpc_channel::SharedRpcChannel;
use serde_json::Value;
use std::io;
use std::io::ErrorKind;
use std::path::{Path, PathBuf};
use vhdl_lang::{
    kind_str, AnyEntKind, Concurrent, Config, Design, Diagnostic, EntHierarchy, EntRef,
    InterfaceEnt, Message, MessageHandler, Object, Overloaded, Project, Severity, SeverityMap,
    Source, SrcPos, Token, Type, VHDLStandard,
};

/// Defines how the language server handles files
/// that are not part of the `vhdl_ls.toml` project settings file.
#[derive(Default, Clone, Eq, PartialEq)]
pub enum NonProjectFileHandling {
    /// Ignore any non-project files
    Ignore,
    /// Add non-project files to an anonymous library and analyze them
    #[default]
    Analyze,
}

impl NonProjectFileHandling {
    pub fn from_string(value: &str) -> Option<NonProjectFileHandling> {
        use NonProjectFileHandling::*;
        Some(match value {
            "ignore" => Ignore,
            "analyze" => Analyze,
            _ => return None,
        })
    }
}

#[derive(Default, Clone)]
pub struct VHDLServerSettings {
    pub no_lint: bool,
    pub silent: bool,
    pub non_project_file_handling: NonProjectFileHandling,
}

pub struct VHDLServer {
    rpc: SharedRpcChannel,
    settings: VHDLServerSettings,
    // To have well defined unit tests that are not affected by environment
    use_external_config: bool,
    project: Project,
    files_with_notifications: FnvHashMap<Url, ()>,
    init_params: Option<InitializeParams>,
    config_file: Option<PathBuf>,
    severity_map: SeverityMap,
}

impl VHDLServer {
    pub fn new_settings(rpc: SharedRpcChannel, settings: VHDLServerSettings) -> VHDLServer {
        VHDLServer {
            rpc,
            settings,
            use_external_config: true,
            project: Project::new(VHDLStandard::default()),
            files_with_notifications: FnvHashMap::default(),
            init_params: None,
            config_file: None,
            severity_map: SeverityMap::default(),
        }
    }

    #[cfg(test)]
    fn new_external_config(rpc: SharedRpcChannel, use_external_config: bool) -> VHDLServer {
        VHDLServer {
            rpc,
            settings: Default::default(),
            use_external_config,
            project: Project::new(VHDLStandard::default()),
            files_with_notifications: FnvHashMap::default(),
            init_params: None,
            config_file: None,
            severity_map: SeverityMap::default(),
        }
    }

    /// Load the workspace root configuration file
    fn load_root_uri_config(&self) -> io::Result<Config> {
        let config_file = self.config_file.as_ref().ok_or_else(|| {
            io::Error::new(
                io::ErrorKind::Other,
                "Workspace root configuration file not set",
            )
        })?;
        let config = Config::read_file_path(config_file)?;

        // Log which file was loaded
        self.message(Message::log(format!(
            "Loaded workspace root configuration file: {}",
            config_file.to_str().unwrap()
        )));

        Ok(config)
    }

    /// Load the configuration or use a default configuration if unsuccessful
    /// Log info/error messages to the client
    fn load_config(&self) -> Config {
        let mut config = Config::default();

        if self.use_external_config {
            config.load_external_config(&mut self.message_filter(), None);
        }

        match self.load_root_uri_config() {
            Ok(root_config) => {
                config.append(&root_config, &mut self.message_filter());
            }
            Err(ref err) => {
                if matches!(err.kind(), ErrorKind::NotFound) {
                    self.message(Message::error(format!(
                        "Library mapping is unknown due to missing vhdl_ls.toml config file in the workspace root path: {err}"
                    )));
                    self.message(Message::warning(
                        "Without library mapping semantic analysis might be incorrect",
                    ));
                } else {
                    self.message(Message::error(format!("Error loading vhdl_ls.toml: {err}")));
                }
            }
        };

        config
    }

    fn apply_initial_options(&mut self, options: &Value) {
        let Some(non_project_file_handling) = options.get("nonProjectFiles") else {
            return;
        };
        match non_project_file_handling {
            Value::String(handling) => match NonProjectFileHandling::from_string(handling) {
                None => self.message(Message::error(format!(
                    "Illegal setting {handling} for nonProjectFiles setting"
                ))),
                Some(handling) => self.settings.non_project_file_handling = handling,
            },
            _ => self.message(Message::error("nonProjectFiles must be a string")),
        }
    }

    pub fn initialize_request(&mut self, init_params: InitializeParams) -> InitializeResult {
        self.config_file = self.root_uri_config_file(&init_params);
        let config = self.load_config();
        self.severity_map = *config.severities();
        self.project = Project::from_config(config, &mut self.message_filter());
        self.project.enable_unused_declaration_detection();
        if let Some(options) = &init_params.initialization_options {
            self.apply_initial_options(options)
        }
        self.init_params = Some(init_params);
        let trigger_chars: Vec<String> = r".".chars().map(|ch| ch.to_string()).collect();

        let capabilities = ServerCapabilities {
            text_document_sync: Some(TextDocumentSyncCapability::Kind(
                TextDocumentSyncKind::INCREMENTAL,
            )),
            declaration_provider: Some(DeclarationCapability::Simple(true)),
            definition_provider: Some(OneOf::Left(true)),
            hover_provider: Some(HoverProviderCapability::Simple(true)),
            references_provider: Some(OneOf::Left(true)),
            implementation_provider: Some(ImplementationProviderCapability::Simple(true)),
            rename_provider: Some(OneOf::Right(RenameOptions {
                prepare_provider: Some(true),
                work_done_progress_options: Default::default(),
            })),
            workspace_symbol_provider: Some(OneOf::Left(true)),
            document_symbol_provider: Some(OneOf::Left(true)),
            document_highlight_provider: Some(OneOf::Left(true)),
            completion_provider: Some(CompletionOptions {
                resolve_provider: Some(true),
                trigger_characters: Some(trigger_chars),
                completion_item: Some(CompletionOptionsCompletionItem {
                    label_details_support: Some(true),
                }),
                ..Default::default()
            }),
            ..Default::default()
        };

        InitializeResult {
            capabilities,
            server_info: None,
        }
    }

    /// Extract path of workspace root configuration file from InitializeParams
    fn root_uri_config_file(&self, params: &InitializeParams) -> Option<PathBuf> {
        #[allow(deprecated)]
        match params.root_uri.clone() {
            Some(root_uri) => root_uri
                .to_file_path()
                .map(|root_path| root_path.join("vhdl_ls.toml"))
                .map_err(|_| {
                    self.message(Message::error(format!(
                        "{} {} {:?} ",
                        "Cannot load workspace:",
                        "initializeParams.rootUri is not a valid file path:",
                        root_uri,
                    )))
                })
                .ok(),
            None => {
                self.message(Message::error(
                    "Cannot load workspace: Initialize request is missing rootUri parameter.",
                ));
                None
            }
        }
    }

    pub fn shutdown_server(&mut self) {
        self.init_params = None;
    }

    pub fn exit_notification(&mut self) {
        match self.init_params {
            Some(_) => ::std::process::exit(1),
            None => ::std::process::exit(0),
        }
    }

    /// Register capabilities on the client side:
    /// - watch workspace config file for changes
    fn register_capabilities(&mut self) {
        if self.client_supports_did_change_watched_files() {
            let register_options = DidChangeWatchedFilesRegistrationOptions {
                watchers: vec![FileSystemWatcher {
                    glob_pattern: GlobPattern::String("**/vhdl_ls.toml".to_owned()),
                    kind: None,
                }],
            };
            let params = RegistrationParams {
                registrations: vec![Registration {
                    id: "workspace/didChangeWatchedFiles".to_owned(),
                    method: "workspace/didChangeWatchedFiles".to_owned(),
                    register_options: serde_json::to_value(register_options).ok(),
                }],
            };
            self.rpc.send_request("client/registerCapability", params);
        }
    }

    pub fn initialized_notification(&mut self) {
        self.register_capabilities();
        self.publish_diagnostics();
    }

    pub fn text_document_did_change_notification(&mut self, params: &DidChangeTextDocumentParams) {
        let file_name = uri_to_file_name(&params.text_document.uri);
        if let Some(source) = self.project.get_source(&file_name) {
            for content_change in params.content_changes.iter() {
                let range = content_change.range.map(from_lsp_range);
                source.change(range.as_ref(), &content_change.text);
            }
            self.project.update_source(&source);
            self.publish_diagnostics();
        } else if self.settings.non_project_file_handling != NonProjectFileHandling::Ignore {
            self.message(Message::error(format!(
                "Changing file {} that is not part of the project",
                file_name.to_string_lossy()
            )));
        }
    }

    pub fn text_document_did_open_notification(&mut self, params: &DidOpenTextDocumentParams) {
        let TextDocumentItem { uri, text, .. } = &params.text_document;
        let file_name = uri_to_file_name(uri);
        if let Some(source) = self.project.get_source(&file_name) {
            source.change(None, text);
            self.project.update_source(&source);
            self.publish_diagnostics();
        } else {
            match self.settings.non_project_file_handling {
                NonProjectFileHandling::Ignore => {}
                NonProjectFileHandling::Analyze => {
                    self.message(Message::warning(format!(
                        "Opening file {} that is not part of the project",
                        file_name.to_string_lossy()
                    )));
                    self.project
                        .update_source(&Source::inline(&file_name, text));
                    self.publish_diagnostics();
                }
            }
        }
    }

    pub fn workspace_did_change_watched_files(&mut self, params: &DidChangeWatchedFilesParams) {
        if let Some(config_file) = &self.config_file {
            let config_file_has_changed = params
                .changes
                .iter()
                .any(|change| uri_to_file_name(&change.uri).as_path() == config_file);
            if config_file_has_changed {
                self.message(Message::log(
                    "Configuration file has changed, reloading project...",
                ));
                let config = self.load_config();
                self.severity_map = *config.severities();

                self.project
                    .update_config(config, &mut self.message_filter());
                self.publish_diagnostics();
            }
        }
    }

    fn completion_item_to_lsp_item(
        &self,
        item: vhdl_lang::CompletionItem,
    ) -> lsp_types::CompletionItem {
        match item {
            vhdl_lang::CompletionItem::Simple(ent) => entity_to_completion_item(ent),
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
            vhdl_lang::CompletionItem::EntityInstantiation(ent, architectures) => {
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
                        line += &*format!("\n {} map(\n", purpose);
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

    fn client_supports_related_information(&self) -> bool {
        let try_fun = || {
            self.init_params
                .as_ref()?
                .capabilities
                .text_document
                .as_ref()?
                .publish_diagnostics
                .as_ref()?
                .related_information
        };
        try_fun().unwrap_or(false)
    }

    fn client_supports_did_change_watched_files(&self) -> bool {
        let try_fun = || {
            self.init_params
                .as_ref()?
                .capabilities
                .workspace
                .as_ref()?
                .did_change_watched_files
                .as_ref()?
                .dynamic_registration
        };
        try_fun().unwrap_or(false)
    }

    fn client_supports_snippets(&self) -> bool {
        let try_fun = || {
            self.init_params
                .as_ref()?
                .capabilities
                .text_document
                .as_ref()?
                .completion
                .as_ref()?
                .completion_item
                .as_ref()?
                .snippet_support
        };
        try_fun().unwrap_or(false)
    }

    fn client_has_hierarchical_document_symbol_support(&self) -> bool {
        let try_fun = || {
            self.init_params
                .as_ref()?
                .capabilities
                .text_document
                .as_ref()?
                .document_symbol
                .as_ref()?
                .hierarchical_document_symbol_support
        };
        try_fun().unwrap_or(false)
    }

    fn publish_diagnostics(&mut self) {
        let diagnostics = self.project.analyse();

        if self.settings.no_lint {
            return;
        }

        let supports_related_information = self.client_supports_related_information();
        let diagnostics = {
            if supports_related_information {
                diagnostics
            } else {
                flatten_related(diagnostics)
            }
        };

        let mut files_with_notifications = std::mem::take(&mut self.files_with_notifications);
        for (file_uri, diagnostics) in diagnostics_by_uri(diagnostics).into_iter() {
            let lsp_diagnostics = diagnostics
                .into_iter()
                .filter_map(|diag| to_lsp_diagnostic(diag, &self.severity_map))
                .collect();

            let publish_diagnostics = PublishDiagnosticsParams {
                uri: file_uri.clone(),
                diagnostics: lsp_diagnostics,
                version: None,
            };

            self.rpc
                .send_notification("textDocument/publishDiagnostics", publish_diagnostics);

            self.files_with_notifications.insert(file_uri.clone(), ());
        }

        for (file_uri, _) in files_with_notifications.drain() {
            // File has no longer any diagnostics, publish empty notification to clear them
            if !self.files_with_notifications.contains_key(&file_uri) {
                let publish_diagnostics = PublishDiagnosticsParams {
                    uri: file_uri.clone(),
                    diagnostics: vec![],
                    version: None,
                };

                self.rpc
                    .send_notification("textDocument/publishDiagnostics", publish_diagnostics);
            }
        }
    }

    pub fn text_document_declaration(
        &mut self,
        params: &TextDocumentPositionParams,
    ) -> Option<Location> {
        let source = self
            .project
            .get_source(&uri_to_file_name(&params.text_document.uri))?;

        let ent = self
            .project
            .find_declaration(&source, from_lsp_pos(params.position))?;
        Some(srcpos_to_location(ent.decl_pos()?))
    }

    pub fn text_document_definition(
        &mut self,
        params: &TextDocumentPositionParams,
    ) -> Option<Location> {
        let source = self
            .project
            .get_source(&uri_to_file_name(&params.text_document.uri))?;

        let ent = self
            .project
            .find_definition(&source, from_lsp_pos(params.position))?;
        Some(srcpos_to_location(ent.decl_pos()?))
    }

    pub fn text_document_implementation(
        &mut self,
        params: &TextDocumentPositionParams,
    ) -> Option<GotoDefinitionResponse> {
        let source = self
            .project
            .get_source(&uri_to_file_name(&params.text_document.uri))?;

        let ents = self
            .project
            .find_implementation(&source, from_lsp_pos(params.position));

        Some(GotoDefinitionResponse::Array(
            ents.into_iter()
                .filter_map(|ent| ent.decl_pos().map(srcpos_to_location))
                .collect(),
        ))
    }

    pub fn prepare_rename(
        &mut self,
        params: &TextDocumentPositionParams,
    ) -> Option<PrepareRenameResponse> {
        let source = self
            .project
            .get_source(&uri_to_file_name(&params.text_document.uri))?;

        let (pos, ent) = self
            .project
            .item_at_cursor(&source, from_lsp_pos(params.position))?;

        if let Designator::Identifier(_) = ent.designator() {
            Some(PrepareRenameResponse::Range(to_lsp_range(pos.range)))
        } else {
            // It does not make sense to rename operator symbols and character literals
            // Also they have different representations that would not be handled consistently
            // Such as function "+"(arg1, arg2 : integer) but used as foo + bar
            None
        }
    }

    pub fn rename(&mut self, params: &RenameParams) -> Option<WorkspaceEdit> {
        let source = self.project.get_source(&uri_to_file_name(
            &params.text_document_position.text_document.uri,
        ))?;

        let ent = self.project.find_declaration(
            &source,
            from_lsp_pos(params.text_document_position.position),
        )?;

        let mut changes: HashMap<Url, Vec<TextEdit>> = Default::default();

        for srcpos in self.project.find_all_references(ent) {
            let loc = srcpos_to_location(&srcpos);
            changes.entry(loc.uri).or_default().push(TextEdit {
                range: loc.range,
                new_text: params.new_name.clone(),
            });
        }

        Some(WorkspaceEdit {
            changes: Some(changes),
            ..Default::default()
        })
    }

    pub fn document_highlight(
        &mut self,
        params: &TextDocumentPositionParams,
    ) -> Option<Vec<DocumentHighlight>> {
        let source = self
            .project
            .get_source(&uri_to_file_name(&params.text_document.uri))?;

        let ent = self
            .project
            .find_declaration(&source, from_lsp_pos(params.position))?;

        Some(
            self.project
                .find_all_references_in_source(&source, ent)
                .iter()
                .map(|pos| DocumentHighlight {
                    range: to_lsp_range(pos.range()),
                    kind: Some(DocumentHighlightKind::TEXT),
                })
                .collect(),
        )
    }

    pub fn workspace_symbol(
        &self,
        params: &WorkspaceSymbolParams,
    ) -> Option<WorkspaceSymbolResponse> {
        let trunc_limit = 200;
        let query = params.query.to_ascii_lowercase();
        let mut symbols: Vec<_> = self
            .project
            .public_symbols()
            .filter_map(|ent| match ent.designator() {
                Designator::Identifier(_) | Designator::Character(_) => {
                    Some((ent, ent.designator().to_string().to_ascii_lowercase()))
                }
                Designator::OperatorSymbol(op) => Some((ent, op.to_string().to_ascii_lowercase())),
                Designator::Anonymous(_) => None,
            })
            .collect();
        symbols.sort_by(|(_, n1), (_, n2)| n1.cmp(n2));
        Some(WorkspaceSymbolResponse::Nested(
            symbols
                .into_iter()
                .filter_map(|(ent, name)| {
                    let decl_pos = ent.decl_pos()?;
                    if name.starts_with(&query) {
                        Some(WorkspaceSymbol {
                            name: ent.describe(),
                            kind: to_symbol_kind(ent.kind()),
                            tags: None,
                            container_name: ent.parent.map(|ent| ent.path_name()),
                            location: OneOf::Left(srcpos_to_location(decl_pos)),
                            data: None,
                        })
                    } else {
                        None
                    }
                })
                .take(trunc_limit)
                .collect(),
        ))
    }

    pub fn document_symbol(&self, params: &DocumentSymbolParams) -> Option<DocumentSymbolResponse> {
        let source = self
            .project
            .get_source(&uri_to_file_name(&params.text_document.uri))?;

        // Some files are mapped to multiple libraries, only use the first library for document symbols
        let library_name = self
            .project
            .library_mapping_of(&source)
            .into_iter()
            .next()?;

        if self.client_has_hierarchical_document_symbol_support() {
            fn to_document_symbol(
                EntHierarchy { ent, children }: EntHierarchy,
                ctx: &Vec<Token>,
            ) -> DocumentSymbol {
                // Use the declaration position, if it exists,
                // else the position of the first source range token.
                // The latter is applicable for unnamed elements, e.g., processes or loops.
                let selection_pos = ent.decl_pos().unwrap_or(ent.src_span.start_token.pos(ctx));
                let src_range = ent.src_span.pos(ctx).range();
                #[allow(deprecated)]
                DocumentSymbol {
                    name: ent.describe(),
                    kind: to_symbol_kind(ent.kind()),
                    tags: None,
                    detail: None,
                    selection_range: to_lsp_range(selection_pos.range),
                    range: to_lsp_range(src_range),
                    children: if !children.is_empty() {
                        Some(
                            children
                                .into_iter()
                                .map(|hierarchy| to_document_symbol(hierarchy, ctx))
                                .collect(),
                        )
                    } else {
                        None
                    },
                    deprecated: None,
                }
            }

            Some(DocumentSymbolResponse::Nested(
                self.project
                    .document_symbols(&library_name, &source)
                    .into_iter()
                    .map(|(hierarchy, tokens)| to_document_symbol(hierarchy, tokens))
                    .collect(),
            ))
        } else {
            #[allow(clippy::ptr_arg)]
            fn to_symbol_information(ent: EntRef, ctx: &Vec<Token>) -> SymbolInformation {
                let selection_pos = ent.decl_pos().unwrap_or(ent.src_span.start_token.pos(ctx));
                #[allow(deprecated)]
                SymbolInformation {
                    name: ent.describe(),
                    kind: to_symbol_kind(ent.kind()),
                    tags: None,
                    location: srcpos_to_location(selection_pos),
                    deprecated: None,
                    container_name: ent.parent_in_same_source().map(|ent| ent.describe()),
                }
            }

            Some(DocumentSymbolResponse::Flat(
                self.project
                    .document_symbols(&library_name, &source)
                    .into_iter()
                    .flat_map(|(a, ctx)| {
                        a.into_flat()
                            .into_iter()
                            .map(|hierarchy| to_symbol_information(hierarchy, ctx))
                    })
                    .collect(),
            ))
        }
    }

    pub fn text_document_hover(&mut self, params: &TextDocumentPositionParams) -> Option<Hover> {
        let source = self
            .project
            .get_source(&uri_to_file_name(&params.text_document.uri))?;
        let ent = self
            .project
            .find_declaration(&source, from_lsp_pos(params.position))?;

        let value = self.project.format_declaration(ent)?;

        Some(Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value: format!("```vhdl\n{value}\n```"),
            }),
            range: None,
        })
    }

    pub fn text_document_references(&mut self, params: &ReferenceParams) -> Vec<Location> {
        let ent = self
            .project
            .get_source(&uri_to_file_name(
                &params.text_document_position.text_document.uri,
            ))
            .and_then(|source| {
                self.project.find_declaration(
                    &source,
                    from_lsp_pos(params.text_document_position.position),
                )
            });

        if let Some(ent) = ent {
            self.project
                .find_all_references(ent)
                .iter()
                .map(srcpos_to_location)
                .collect()
        } else {
            Vec::new()
        }
    }

    fn message_filter(&self) -> MessageFilter {
        MessageFilter {
            silent: self.settings.silent,
            rpc: self.rpc.clone(),
        }
    }

    fn message(&self, msg: Message) {
        self.message_filter().push(msg);
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
        AnyEntKind::Concurrent(_) => CompletionItemKind::MODULE,
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

struct MessageFilter {
    silent: bool,
    rpc: SharedRpcChannel,
}

impl MessageHandler for MessageFilter {
    fn push(&mut self, msg: Message) {
        if !self.silent
            && matches!(
                msg.message_type,
                vhdl_lang::MessageType::Warning | vhdl_lang::MessageType::Error
            )
        {
            self.rpc.send_notification(
                "window/showMessage",
                ShowMessageParams {
                    typ: to_lsp_message_type(&msg.message_type),
                    message: msg.message.clone(),
                },
            );
        }

        self.rpc.send_notification(
            "window/logMessage",
            LogMessageParams {
                typ: to_lsp_message_type(&msg.message_type),
                message: msg.message,
            },
        );
    }
}

fn to_lsp_message_type(message_type: &vhdl_lang::MessageType) -> MessageType {
    match message_type {
        vhdl_lang::MessageType::Error => MessageType::ERROR,
        vhdl_lang::MessageType::Warning => MessageType::WARNING,
        vhdl_lang::MessageType::Info => MessageType::INFO,
        vhdl_lang::MessageType::Log => MessageType::LOG,
    }
}

fn srcpos_to_location(pos: &SrcPos) -> Location {
    let uri = file_name_to_uri(pos.source.file_name());
    Location {
        uri,
        range: to_lsp_range(pos.range()),
    }
}

fn from_lsp_pos(position: lsp_types::Position) -> vhdl_lang::Position {
    vhdl_lang::Position {
        line: position.line,
        character: position.character,
    }
}

fn to_lsp_pos(position: vhdl_lang::Position) -> lsp_types::Position {
    lsp_types::Position {
        line: position.line,
        character: position.character,
    }
}

fn to_lsp_range(range: vhdl_lang::Range) -> lsp_types::Range {
    lsp_types::Range {
        start: to_lsp_pos(range.start),
        end: to_lsp_pos(range.end),
    }
}

fn from_lsp_range(range: lsp_types::Range) -> vhdl_lang::Range {
    vhdl_lang::Range {
        start: from_lsp_pos(range.start),
        end: from_lsp_pos(range.end),
    }
}

fn diagnostics_by_uri(diagnostics: Vec<Diagnostic>) -> FnvHashMap<Url, Vec<Diagnostic>> {
    let mut map: FnvHashMap<Url, Vec<Diagnostic>> = FnvHashMap::default();

    for diagnostic in diagnostics {
        let uri = file_name_to_uri(diagnostic.pos.source.file_name());
        match map.entry(uri) {
            Entry::Occupied(mut entry) => entry.get_mut().push(diagnostic),
            Entry::Vacant(entry) => {
                let vec = vec![diagnostic];
                entry.insert(vec);
            }
        }
    }

    map
}

fn flatten_related(diagnostics: Vec<Diagnostic>) -> Vec<Diagnostic> {
    let mut flat_diagnostics = Vec::new();
    for mut diagnostic in diagnostics {
        flat_diagnostics.extend(diagnostic.drain_related());
        flat_diagnostics.push(diagnostic);
    }
    flat_diagnostics
}

fn file_name_to_uri(file_name: &Path) -> Url {
    // @TODO return error to client
    Url::from_file_path(file_name).unwrap()
}

fn uri_to_file_name(uri: &Url) -> PathBuf {
    // @TODO return error to client
    uri.to_file_path().unwrap()
}

fn to_lsp_diagnostic(
    diagnostic: Diagnostic,
    severity_map: &SeverityMap,
) -> Option<lsp_types::Diagnostic> {
    let severity = match severity_map[diagnostic.code]? {
        Severity::Error => DiagnosticSeverity::ERROR,
        Severity::Warning => DiagnosticSeverity::WARNING,
        Severity::Info => DiagnosticSeverity::INFORMATION,
        Severity::Hint => DiagnosticSeverity::HINT,
    };

    let related_information = if !diagnostic.related.is_empty() {
        let mut related_information = Vec::new();
        for (pos, msg) in diagnostic.related {
            let uri = file_name_to_uri(pos.source.file_name());
            related_information.push(DiagnosticRelatedInformation {
                location: Location {
                    uri: uri.to_owned(),
                    range: to_lsp_range(pos.range()),
                },
                message: msg,
            })
        }
        Some(related_information)
    } else {
        None
    };

    Some(lsp_types::Diagnostic {
        range: to_lsp_range(diagnostic.pos.range()),
        severity: Some(severity),
        code: Some(NumberOrString::String(format!("{}", diagnostic.code))),
        source: Some("vhdl ls".to_owned()),
        message: diagnostic.message,
        related_information,
        ..Default::default()
    })
}

fn overloaded_kind(overloaded: &Overloaded) -> SymbolKind {
    match overloaded {
        Overloaded::SubprogramDecl(_) => SymbolKind::FUNCTION,
        Overloaded::Subprogram(_) => SymbolKind::FUNCTION,
        Overloaded::UninstSubprogramDecl(..) => SymbolKind::FUNCTION,
        Overloaded::UninstSubprogram(..) => SymbolKind::FUNCTION,
        Overloaded::InterfaceSubprogram(_) => SymbolKind::FUNCTION,
        Overloaded::EnumLiteral(_) => SymbolKind::ENUM_MEMBER,
        Overloaded::Alias(o) => overloaded_kind(o.kind()),
    }
}

fn object_kind(object: &Object) -> SymbolKind {
    if matches!(object.subtype.type_mark().kind(), Type::Protected(..)) {
        SymbolKind::OBJECT
    } else if object.iface.is_some() {
        SymbolKind::INTERFACE
    } else {
        object_class_kind(object.class)
    }
}

fn object_class_kind(class: ObjectClass) -> SymbolKind {
    match class {
        ObjectClass::Signal => SymbolKind::EVENT,
        ObjectClass::Constant => SymbolKind::CONSTANT,
        ObjectClass::Variable => SymbolKind::VARIABLE,
        ObjectClass::SharedVariable => SymbolKind::VARIABLE,
    }
}

fn type_kind(t: &Type) -> SymbolKind {
    match t {
        vhdl_lang::Type::Array { .. } => SymbolKind::ARRAY,
        vhdl_lang::Type::Enum(_) => SymbolKind::ENUM,
        vhdl_lang::Type::Integer => SymbolKind::NUMBER,
        vhdl_lang::Type::Real => SymbolKind::NUMBER,
        vhdl_lang::Type::Physical => SymbolKind::NUMBER,
        vhdl_lang::Type::Access(_) => SymbolKind::ENUM,
        vhdl_lang::Type::Record(_) => SymbolKind::STRUCT,
        vhdl_lang::Type::Incomplete => SymbolKind::NULL,
        vhdl_lang::Type::Subtype(t) => type_kind(t.type_mark().kind()),
        vhdl_lang::Type::Protected(_, _) => SymbolKind::CLASS,
        vhdl_lang::Type::File => SymbolKind::FILE,
        vhdl_lang::Type::Interface => SymbolKind::TYPE_PARAMETER,
        vhdl_lang::Type::Alias(t) => type_kind(t.kind()),
        vhdl_lang::Type::Universal(_) => SymbolKind::NUMBER,
    }
}

fn to_symbol_kind(kind: &AnyEntKind) -> SymbolKind {
    match kind {
        AnyEntKind::ExternalAlias { class, .. } => object_class_kind(ObjectClass::from(*class)),
        AnyEntKind::ObjectAlias { base_object, .. } => object_kind(base_object.object()),
        AnyEntKind::Object(o) => object_kind(o),
        AnyEntKind::LoopParameter(_) => SymbolKind::CONSTANT,
        AnyEntKind::PhysicalLiteral(_) => SymbolKind::CONSTANT,
        AnyEntKind::DeferredConstant(_) => SymbolKind::CONSTANT,
        AnyEntKind::File { .. } => SymbolKind::FILE,
        AnyEntKind::InterfaceFile { .. } => SymbolKind::INTERFACE,
        AnyEntKind::Component(_) => SymbolKind::CLASS,
        AnyEntKind::Attribute(_) => SymbolKind::PROPERTY,
        AnyEntKind::Overloaded(o) => overloaded_kind(o),
        AnyEntKind::Type(t) => type_kind(t),
        AnyEntKind::ElementDeclaration(_) => SymbolKind::FIELD,
        AnyEntKind::Sequential(_) => SymbolKind::NAMESPACE,
        AnyEntKind::Concurrent(Some(Concurrent::Instance)) => SymbolKind::MODULE,
        AnyEntKind::Concurrent(_) => SymbolKind::NAMESPACE,
        AnyEntKind::Library => SymbolKind::NAMESPACE,
        AnyEntKind::View(_) => SymbolKind::INTERFACE,
        AnyEntKind::Design(d) => match d {
            vhdl_lang::Design::Entity(_, _) => SymbolKind::MODULE,
            vhdl_lang::Design::Architecture(_) => SymbolKind::MODULE,
            vhdl_lang::Design::Configuration => SymbolKind::MODULE,
            vhdl_lang::Design::Package(_, _) => SymbolKind::PACKAGE,
            vhdl_lang::Design::PackageBody => SymbolKind::PACKAGE,
            vhdl_lang::Design::UninstPackage(_, _) => SymbolKind::PACKAGE,
            vhdl_lang::Design::PackageInstance(_) => SymbolKind::PACKAGE,
            vhdl_lang::Design::InterfacePackageInstance(_) => SymbolKind::PACKAGE,
            vhdl_lang::Design::Context(_) => SymbolKind::NAMESPACE,
        },
    }
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use super::*;
    use crate::rpc_channel::test_support::*;

    fn initialize_server(server: &mut VHDLServer, root_uri: Url) {
        let capabilities = ClientCapabilities::default();

        #[allow(deprecated)]
        let initialize_params = InitializeParams {
            process_id: None,
            root_path: None,
            root_uri: Some(root_uri),
            initialization_options: None,
            capabilities,
            trace: None,
            workspace_folders: None,
            client_info: None,
            locale: None,
            work_done_progress_params: WorkDoneProgressParams::default(),
        };

        server.initialize_request(initialize_params);
        server.initialized_notification();
    }

    fn temp_root_uri() -> (tempfile::TempDir, Url) {
        let tempdir = tempfile::tempdir().unwrap();
        let root_uri = Url::from_file_path(tempdir.path().canonicalize().unwrap()).unwrap();
        (tempdir, root_uri)
    }

    fn expect_loaded_config_messages(mock: &RpcMock, config_uri: &Url) {
        let file_name = config_uri
            .to_file_path()
            .unwrap()
            .to_str()
            .unwrap()
            .to_owned();
        mock.expect_message_contains(format!(
            "Loaded workspace root configuration file: {file_name}"
        ));
    }

    fn expect_missing_config_messages(mock: &RpcMock) {
        mock.expect_error_contains("Library mapping is unknown due to missing vhdl_ls.toml config file in the workspace root path");
        mock.expect_warning_contains(
            "Without library mapping semantic analysis might be incorrect",
        );
    }

    fn expect_erroneous_config(mock: &RpcMock) {
        mock.expect_error_contains("Error loading vhdl_ls.toml");
    }

    /// Create RpcMock and VHDLServer
    fn setup_server() -> (Rc<RpcMock>, VHDLServer) {
        let mock = Rc::new(RpcMock::new());
        let server = VHDLServer::new_external_config(SharedRpcChannel::new(mock.clone()), false);
        (mock, server)
    }

    #[test]
    fn initialize() {
        let (mock, mut server) = setup_server();
        let (_tempdir, root_uri) = temp_root_uri();
        expect_missing_config_messages(&mock);
        initialize_server(&mut server, root_uri);
    }

    #[test]
    fn did_open_no_diagnostics() {
        let (mock, mut server) = setup_server();
        let (_tempdir, root_uri) = temp_root_uri();
        expect_missing_config_messages(&mock);
        initialize_server(&mut server, root_uri.clone());

        let file_url = root_uri.join("ent.vhd").unwrap();
        let code = "
entity ent is
end entity ent;
"
        .to_owned();

        let did_open = DidOpenTextDocumentParams {
            text_document: TextDocumentItem {
                uri: file_url,
                language_id: "vhdl".to_owned(),
                version: 0,
                text: code,
            },
        };

        mock.expect_warning_contains("is not part of the project");

        server.text_document_did_open_notification(&did_open);
    }

    #[test]
    fn did_open_with_diagnostics_and_change_without() {
        let (mock, mut server) = setup_server();

        let (_tempdir, root_uri) = temp_root_uri();
        expect_missing_config_messages(&mock);
        initialize_server(&mut server, root_uri.clone());

        let file_url = root_uri.join("ent.vhd").unwrap();
        let code = "
entity ent is
end entity ent2;
"
        .to_owned();

        let did_open = DidOpenTextDocumentParams {
            text_document: TextDocumentItem {
                uri: file_url.clone(),
                language_id: "vhdl".to_owned(),
                version: 0,
                text: code,
            },
        };

        let publish_diagnostics = PublishDiagnosticsParams {
            uri: file_url.clone(),
            diagnostics: vec![lsp_types::Diagnostic {
                range: Range {
                    start: lsp_types::Position {
                        line: 2,
                        character: "end entity ".len() as u32,
                    },
                    end: lsp_types::Position {
                        line: 2,
                        character: "end entity ent2".len() as u32,
                    },
                },
                code: Some(NumberOrString::String("syntax_error".to_owned())),
                severity: Some(DiagnosticSeverity::ERROR),
                source: Some("vhdl ls".to_owned()),
                message: "End identifier mismatch, expected ent".to_owned(),
                ..Default::default()
            }],
            version: None,
        };

        mock.expect_warning_contains("is not part of the project");

        mock.expect_notification("textDocument/publishDiagnostics", publish_diagnostics);
        server.text_document_did_open_notification(&did_open);

        let code = "
entity ent is
end entity ent;
"
        .to_owned();

        let did_change = DidChangeTextDocumentParams {
            text_document: VersionedTextDocumentIdentifier {
                uri: file_url.clone(),
                version: 1,
            },
            content_changes: vec![TextDocumentContentChangeEvent {
                range: None,
                range_length: None,
                text: code,
            }],
        };

        let publish_diagnostics = PublishDiagnosticsParams {
            uri: file_url,
            diagnostics: vec![],
            version: None,
        };

        mock.expect_notification("textDocument/publishDiagnostics", publish_diagnostics);
        server.text_document_did_change_notification(&did_change);
    }

    fn write_file(root_uri: &Url, file_name: impl AsRef<str>, contents: impl AsRef<str>) -> Url {
        let path = root_uri.to_file_path().unwrap().join(file_name.as_ref());
        std::fs::write(&path, contents.as_ref()).unwrap();
        Url::from_file_path(path).unwrap()
    }

    fn write_config(root_uri: &Url, contents: impl AsRef<str>) -> Url {
        write_file(root_uri, "vhdl_ls.toml", contents)
    }

    #[test]
    fn initialize_with_config() {
        let (mock, mut server) = setup_server();
        let (_tempdir, root_uri) = temp_root_uri();
        let file_uri = write_file(
            &root_uri,
            "file.vhd",
            "\
entity ent is
end entity;

architecture rtl of ent2 is
begin
end;
",
        );

        let config_uri = write_config(
            &root_uri,
            "
[libraries]
lib.files = [
  'file.vhd'
]
",
        );

        let publish_diagnostics = PublishDiagnosticsParams {
            uri: file_uri,
            diagnostics: vec![lsp_types::Diagnostic {
                range: Range {
                    start: lsp_types::Position {
                        line: 3,
                        character: "architecture rtl of ".len() as u32,
                    },
                    end: lsp_types::Position {
                        line: 3,
                        character: "architecture rtl of ent2".len() as u32,
                    },
                },
                code: Some(NumberOrString::String("unresolved".to_owned())),
                severity: Some(DiagnosticSeverity::ERROR),
                source: Some("vhdl ls".to_owned()),
                message: "No primary unit \'ent2\' within library \'lib\'".to_owned(),
                ..Default::default()
            }],
            version: None,
        };

        expect_loaded_config_messages(&mock, &config_uri);
        mock.expect_notification("textDocument/publishDiagnostics", publish_diagnostics);

        initialize_server(&mut server, root_uri);
    }

    #[test]
    fn initialize_with_bad_config() {
        let (mock, mut server) = setup_server();
        let (_tempdir, root_uri) = temp_root_uri();

        write_config(
            &root_uri,
            "
[libraries
",
        );

        expect_erroneous_config(&mock);
        initialize_server(&mut server, root_uri);
    }

    #[test]
    fn initialize_with_config_missing_files() {
        let (mock, mut server) = setup_server();
        let (_tempdir, root_uri) = temp_root_uri();

        let config_uri = write_config(
            &root_uri,
            "
[libraries]
lib.files = [
'missing_file.vhd',
]
",
        );

        expect_loaded_config_messages(&mock, &config_uri);
        mock.expect_warning_contains("missing_file.vhd");
        initialize_server(&mut server, root_uri);
    }

    #[test]
    fn text_document_declaration() {
        let (mock, mut server) = setup_server();
        let (_tempdir, root_uri) = temp_root_uri();

        let file_url1 = write_file(
            &root_uri,
            "pkg1.vhd",
            "\
package pkg1 is
  type typ_t is (foo, bar);
end package;
",
        );

        let code2 = "\
use work.pkg1.all;
package pkg2 is
  constant c : typ_t := bar;
end package;
        "
        .to_owned();
        let file_url2 = write_file(&root_uri, "pkg2.vhd", &code2);

        let config_uri = write_config(
            &root_uri,
            format!(
                "
[libraries]
std.files = [
'{}/../vhdl_libraries/std/*.vhd',
]
lib.files = [
  '*.vhd'
]
",
                std::env::var("CARGO_MANIFEST_DIR").unwrap()
            ),
        );

        expect_loaded_config_messages(&mock, &config_uri);
        initialize_server(&mut server, root_uri);

        let did_open = DidOpenTextDocumentParams {
            text_document: TextDocumentItem {
                uri: file_url2.clone(),
                language_id: "vhdl".to_owned(),
                version: 0,
                text: code2,
            },
        };

        server.text_document_did_open_notification(&did_open);

        let response = server.text_document_declaration(&TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri: file_url2 },
            position: lsp_types::Position {
                line: 2,
                character: "  constant c : t".len() as u32,
            },
        });

        let expected = Location {
            uri: file_url1,
            range: Range {
                start: lsp_types::Position {
                    line: 1,
                    character: "  type ".len() as u32,
                },
                end: lsp_types::Position {
                    line: 1,
                    character: "  type tpe_t".len() as u32,
                },
            },
        };

        assert_eq!(response, Some(expected));
    }

    #[test]
    fn client_register_capability() {
        let (mock, mut server) = setup_server();
        let (_tempdir, root_uri) = temp_root_uri();

        let config_uri = write_config(
            &root_uri,
            "
[libraries]
        ",
        );

        let register_options = DidChangeWatchedFilesRegistrationOptions {
            watchers: vec![FileSystemWatcher {
                glob_pattern: GlobPattern::String("**/vhdl_ls.toml".to_owned()),
                kind: None,
            }],
        };
        let register_capability = RegistrationParams {
            registrations: vec![Registration {
                id: "workspace/didChangeWatchedFiles".to_owned(),
                method: "workspace/didChangeWatchedFiles".to_owned(),
                register_options: serde_json::to_value(register_options).ok(),
            }],
        };

        expect_loaded_config_messages(&mock, &config_uri);
        mock.expect_request("client/registerCapability", register_capability);

        let capabilities = ClientCapabilities {
            workspace: Some(WorkspaceClientCapabilities {
                did_change_watched_files: Some(DidChangeWatchedFilesClientCapabilities {
                    dynamic_registration: Some(true),
                    relative_pattern_support: Some(false),
                }),
                ..WorkspaceClientCapabilities::default()
            }),
            ..ClientCapabilities::default()
        };
        #[allow(deprecated)]
        let initialize_params = InitializeParams {
            root_uri: Some(root_uri),
            capabilities,
            ..Default::default()
        };

        server.initialize_request(initialize_params);
        server.initialized_notification();
    }

    #[test]
    fn update_config_file() {
        let (mock, mut server) = setup_server();
        let (_tempdir, root_uri) = temp_root_uri();
        let file1_uri = write_file(
            &root_uri,
            "file1.vhd",
            "\
architecture rtl of ent is
begin
end;
",
        );
        let file2_uri = write_file(
            &root_uri,
            "file2.vhd",
            "\
architecture rtl of ent is
begin
end;
",
        );
        let config_uri = write_config(
            &root_uri,
            "
[libraries]
lib.files = [
  'file1.vhd'
]
",
        );

        let publish_diagnostics1 = PublishDiagnosticsParams {
            uri: file1_uri.clone(),
            diagnostics: vec![lsp_types::Diagnostic {
                range: Range {
                    start: lsp_types::Position {
                        line: 0,
                        character: "architecture rtl of ".len() as u32,
                    },
                    end: lsp_types::Position {
                        line: 0,
                        character: "architecture rtl of ent".len() as u32,
                    },
                },
                code: Some(NumberOrString::String("unresolved".to_owned())),
                severity: Some(DiagnosticSeverity::ERROR),
                source: Some("vhdl ls".to_owned()),
                message: "No primary unit \'ent\' within library \'lib\'".to_owned(),
                ..Default::default()
            }],
            version: None,
        };

        // after config change
        let publish_diagnostics2a = PublishDiagnosticsParams {
            uri: file2_uri,
            ..publish_diagnostics1.clone()
        };
        let publish_diagnostics2b = PublishDiagnosticsParams {
            uri: file1_uri,
            diagnostics: vec![],
            version: None,
        };

        expect_loaded_config_messages(&mock, &config_uri);
        mock.expect_notification("textDocument/publishDiagnostics", publish_diagnostics1);
        mock.expect_message_contains("Configuration file has changed, reloading project...");
        expect_loaded_config_messages(&mock, &config_uri);
        mock.expect_notification("textDocument/publishDiagnostics", publish_diagnostics2a);
        mock.expect_notification("textDocument/publishDiagnostics", publish_diagnostics2b);

        initialize_server(&mut server, root_uri.clone());

        let config_uri = write_config(
            &root_uri,
            "
[libraries]
lib.files = [
  'file2.vhd'
]
",
        );
        server.workspace_did_change_watched_files(&DidChangeWatchedFilesParams {
            changes: vec![FileEvent {
                typ: FileChangeType::CHANGED,
                uri: config_uri,
            }],
        });
    }
}
