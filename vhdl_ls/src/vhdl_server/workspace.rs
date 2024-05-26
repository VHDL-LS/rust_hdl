use crate::vhdl_server::{srcpos_to_location, to_symbol_kind, uri_to_file_name, VHDLServer};
use lsp_types::{
    DidChangeWatchedFilesParams, OneOf, WorkspaceSymbol, WorkspaceSymbolParams,
    WorkspaceSymbolResponse,
};
use vhdl_lang::ast::Designator;
use vhdl_lang::Message;

impl VHDLServer {
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
}
