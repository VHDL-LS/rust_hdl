use crate::vhdl_server::{srcpos_to_location, to_symbol_kind, uri_to_file_name, VHDLServer};
use fuzzy_matcher::FuzzyMatcher;
use lsp_types::{
    DidChangeWatchedFilesParams, OneOf, WorkspaceSymbol, WorkspaceSymbolParams,
    WorkspaceSymbolResponse,
};
use std::cmp::Ordering;
use std::collections::BinaryHeap;
use vhdl_lang::ast::Designator;
use vhdl_lang::{EntRef, Message};

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
                self.case_transform = config.preferred_case();

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
        let query = params.query.clone();
        let symbols = self
            .project
            .public_symbols()
            .filter_map(|ent| match ent.designator() {
                Designator::Identifier(_) | Designator::Character(_) => {
                    Some((ent, ent.designator().to_string()))
                }
                Designator::OperatorSymbol(op) => Some((ent, op.to_string())),
                Designator::Anonymous(_) => None,
            });

        Some(WorkspaceSymbolResponse::Nested(
            self.filter_workspace_symbols(symbols.into_iter(), &query, trunc_limit),
        ))
    }

    /// Filters found workspace symbols according to a given query.
    /// This uses a fuzzy matcher internally to improve the results.
    /// Queries 'close' to the target string will score high and be included in the
    /// returned vec, while queries 'not close' to the target string will be omitted.
    /// The returned vec is sorted according to the score of the fuzzy matcher.
    fn filter_workspace_symbols<'a>(
        &self,
        symbols: impl Iterator<Item = (EntRef<'a>, String)>,
        query: &str,
        trunc_limit: usize,
    ) -> Vec<WorkspaceSymbol> {
        #[derive(Eq, PartialEq)]
        struct WorkspaceSymbolWithScore {
            symbol: WorkspaceSymbol,
            score: i64,
        }

        impl PartialOrd<Self> for WorkspaceSymbolWithScore {
            fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
                Some(self.cmp(other))
            }
        }

        impl Ord for WorkspaceSymbolWithScore {
            fn cmp(&self, other: &Self) -> Ordering {
                self.score.cmp(&other.score)
            }
        }

        let symbols_with_scores: BinaryHeap<_> = symbols
            .into_iter()
            .filter_map(|(ent, name)| {
                let decl_pos = ent.decl_pos()?;
                self.string_matcher.fuzzy_match(&name, query).map(|score| {
                    WorkspaceSymbolWithScore {
                        symbol: WorkspaceSymbol {
                            name: ent.describe(),
                            kind: to_symbol_kind(ent.kind()),
                            tags: None,
                            container_name: ent.parent.map(|ent| ent.path_name()),
                            location: OneOf::Left(srcpos_to_location(decl_pos)),
                            data: None,
                        },
                        score,
                    }
                })
            })
            .take(trunc_limit)
            .collect();
        symbols_with_scores
            .into_sorted_vec()
            .into_iter()
            .rev()
            .map(|wsws| wsws.symbol)
            .collect()
    }
}
