use crate::vhdl_server::{file_name_to_uri, to_lsp_range, VHDLServer};
use fnv::FnvHashMap;
use lsp_types::{
    DiagnosticRelatedInformation, DiagnosticSeverity, Location, NumberOrString,
    PublishDiagnosticsParams, Url,
};
use std::collections::hash_map::Entry;
use vhdl_lang::{Diagnostic, Severity, SeverityMap};

impl VHDLServer {
    pub fn publish_diagnostics(&mut self) {
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

        let mut by_uri = diagnostics_by_uri(diagnostics);
        for (file_uri, cached_diagnostics) in self.diagnostic_cache.iter_mut() {
            let Some(new_diagnostics) = by_uri.remove(file_uri) else {
                // Diagnostics are in the cache, but not in the newly created diagnostics.
                // This means that there are no longer any diagnostics in the given file.
                // As a consequence, the client needs to be updated
                let publish_diagnostics = PublishDiagnosticsParams {
                    uri: file_uri.clone(),
                    diagnostics: vec![],
                    version: None,
                };
                self.rpc
                    .send_notification("textDocument/publishDiagnostics", publish_diagnostics);
                cached_diagnostics.clear();
                continue;
            };
            // Diagnostics are in the cache, but they are not equivalent to the old diagnostics.
            // This means that we need to update the client, i.e., send a notification.
            if &new_diagnostics != cached_diagnostics {
                let lsp_diagnostics = new_diagnostics
                    .iter()
                    .filter_map(|diag| to_lsp_diagnostic(diag.clone(), &self.severity_map))
                    .collect();
                let publish_diagnostics = PublishDiagnosticsParams {
                    uri: file_uri.clone(),
                    diagnostics: lsp_diagnostics,
                    version: None,
                };
                self.rpc
                    .send_notification("textDocument/publishDiagnostics", publish_diagnostics);
            }
            // else: diagnostics are the same in the cache and the new analysis state.
            // No need to update.
        }

        // These are new diagnostics that weren't in the cache before.
        for (file_uri, diagnostics) in by_uri.into_iter() {
            let lsp_diagnostics = diagnostics
                .iter()
                .filter_map(|diag| to_lsp_diagnostic(diag.clone(), &self.severity_map))
                .collect();
            let publish_diagnostics = PublishDiagnosticsParams {
                uri: file_uri.clone(),
                diagnostics: lsp_diagnostics,
                version: None,
            };
            self.rpc
                .send_notification("textDocument/publishDiagnostics", publish_diagnostics);
            self.diagnostic_cache.insert(file_uri, diagnostics);
        }
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
