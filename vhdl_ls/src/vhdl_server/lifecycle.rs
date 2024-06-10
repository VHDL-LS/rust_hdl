use crate::vhdl_server::{NonProjectFileHandling, VHDLServer};
use lsp_types::*;
use serde_json::Value;
use vhdl_lang::{Message, Project};

impl VHDLServer {
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

    pub fn shutdown_server(&mut self) {
        self.init_params = None;
    }

    pub fn exit_notification(&mut self) {
        match self.init_params {
            Some(_) => ::std::process::exit(1),
            None => ::std::process::exit(0),
        }
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
}
