use crate::vhdl_server::{file_name_to_uri, to_lsp_range, VHDLServer};
use fnv::FnvHashMap;
use lsp_types::{
    DiagnosticRelatedInformation, DiagnosticSeverity, Location, NumberOrString,
    PublishDiagnosticsParams, Url,
};
use std::collections::hash_map::Entry;
use vhdl_lang::{Diagnostic, Severity, SeverityMap};

impl VHDLServer {
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
