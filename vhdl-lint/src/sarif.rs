//! SARIF 2.1.0 output: the interchange format GitHub code scanning reads.

use serde::Serialize;
use std::path::Path;

use crate::{
    diagnostic::Diagnostic, error_code::ErrorCode, fix::Applicability,
    rule::registry::RuleRegistry, severity::Severity, FileId, FileStore,
};

/// The schema every SARIF file declares.
const SCHEMA: &str = "https://json.schemastore.org/sarif-2.1.0.json";

/// The version of the format this module produces.
const VERSION: &str = "2.1.0";

/// The name of the tool, as it appears in `tool.driver.name`.
const TOOL_NAME: &str = "vhdl-lint";

/// Where the tool's documentation lives.
const INFORMATION_URI: &str = "https://vhdl-ls.github.io/rust_hdl/vhdl-lint/";

/// vhdl-lint counts columns in UTF-32 code points; SARIF defaults to UTF-16.
const COLUMN_KIND: &str = "unicodeCodePoints";

/// A severity as a SARIF level.
///
/// SARIF has no `info`, and `none` is its lowest.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "lowercase")]
pub enum SarifLevel {
    Error,
    Warning,
    Note,
    None,
}

/// What the run did, beyond the diagnostics themselves.
#[derive(Debug, Clone)]
pub struct Invocation {
    /// The exit code the process reports.
    pub exit_code: u8,
    /// Whether the tool ran to completion. Findings are not a failure.
    pub execution_successful: bool,
    /// Files that could not be analyzed.
    pub skipped: Vec<String>,
    /// The command line the run was started with.
    pub command_line: String,
}

/// A SARIF log, the whole file.
#[derive(Debug, Serialize)]
pub struct SarifLog {
    #[serde(rename = "$schema")]
    pub schema: &'static str,
    pub version: &'static str,
    pub runs: Vec<SarifRun>,
}

/// One run of one tool.
#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SarifRun {
    pub tool: SarifTool,
    pub column_kind: &'static str,
    pub invocations: Vec<SarifInvocation>,
    pub results: Vec<SarifResult>,
}

/// The tool that produced the run.
#[derive(Debug, Serialize)]
pub struct SarifTool {
    pub driver: SarifDriver,
}

/// The tool component, which for this tool is the linter itself.
#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SarifDriver {
    pub name: &'static str,
    pub version: &'static str,
    pub information_uri: &'static str,
    /// The rules that produced a result.
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub rules: Vec<SarifRule>,
}

/// A rule that can produce results.
#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SarifRule {
    /// The rule's code, e.g. `IDM002`.
    pub id: String,
    /// The rule's name, e.g. `explicit-port-mode`.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub name: Option<String>,
    pub short_description: SarifMessage,
    pub full_description: SarifMessage,
    pub default_configuration: SarifRuleConfiguration,
}

/// How a rule is configured by default.
#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SarifRuleConfiguration {
    pub level: SarifLevel,
    /// Whether the rule is enabled by default.
    pub enabled: bool,
}

/// A message. SARIF requires `text`; `markdown` is the formatted variant.
#[derive(Debug, Serialize)]
pub struct SarifMessage {
    pub text: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub markdown: Option<String>,
}

/// One finding.
#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SarifResult {
    /// The code of the rule that produced this result.
    pub rule_id: String,
    pub level: SarifLevel,
    pub message: SarifMessage,
    pub locations: Vec<SarifLocation>,
    /// The fixes that resolve this result, when the rule knows any.
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub fixes: Vec<SarifFix>,
    /// Rule-specific detail that has no SARIF counterpart.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub properties: Option<SarifProperties>,
}

/// Rule-specific detail that has no SARIF counterpart.
#[derive(Debug, Serialize)]
pub struct SarifProperties {
    /// The note the rule attached to the finding.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub note: Option<String>,
    /// Whether the fix is safe to apply, unsafe, or only a suggestion.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub applicability: Option<String>,
}

/// Where a result was found.
#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SarifLocation {
    pub physical_location: SarifPhysicalLocation,
}

/// A location in a file.
#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SarifPhysicalLocation {
    pub artifact_location: SarifArtifactLocation,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub region: Option<SarifRegion>,
}

/// The file a result was found in.
#[derive(Debug, Serialize)]
pub struct SarifArtifactLocation {
    pub uri: String,
}

/// A range in a file. Columns are 1-based and the end is exclusive.
#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SarifRegion {
    pub start_line: usize,
    pub start_column: usize,
    pub end_line: usize,
    pub end_column: usize,
}

/// A way to resolve a result.
#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SarifFix {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<SarifMessage>,
    pub artifact_changes: Vec<SarifArtifactChange>,
}

/// A change to a file.
#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SarifArtifactChange {
    pub artifact_location: SarifArtifactLocation,
    pub replacements: Vec<SarifReplacement>,
}

/// A single replacement within a file.
#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SarifReplacement {
    pub deleted_region: SarifRegion,
    pub inserted_content: SarifContent,
}

/// The text that replaces a region.
#[derive(Debug, Serialize)]
pub struct SarifContent {
    pub text: String,
}

/// How the run was invoked.
#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SarifInvocation {
    pub execution_successful: bool,
    pub exit_code: i32,
    pub command_line: String,
    /// Files that could not be analyzed.
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub tool_execution_notifications: Vec<SarifNotification>,
}

/// A note about the run rather than about the code.
#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SarifNotification {
    pub level: SarifLevel,
    pub message: SarifMessage,
}

impl SarifLog {
    /// Build a SARIF log from the run's diagnostics.
    ///
    /// `base` is the directory the run was started in; paths under it become
    /// relative URIs.
    pub fn new(
        diagnostics: &[Diagnostic],
        files: &FileStore,
        registry: &RuleRegistry,
        invocation: Invocation,
        base: Option<&Path>,
    ) -> Self {
        let results = diagnostics
            .iter()
            .map(|diagnostic| SarifResult::of(diagnostic, files, base))
            .collect();
        SarifLog {
            schema: SCHEMA,
            version: VERSION,
            runs: vec![SarifRun {
                tool: SarifTool {
                    driver: SarifDriver {
                        name: TOOL_NAME,
                        version: env!("CARGO_PKG_VERSION"),
                        information_uri: INFORMATION_URI,
                        rules: SarifRule::of_used(diagnostics, registry),
                    },
                },
                column_kind: COLUMN_KIND,
                invocations: vec![SarifInvocation {
                    execution_successful: invocation.execution_successful,
                    exit_code: i32::from(invocation.exit_code),
                    command_line: invocation.command_line,
                    tool_execution_notifications: invocation
                        .skipped
                        .iter()
                        .map(|skipped| SarifNotification {
                            level: SarifLevel::Warning,
                            message: SarifMessage {
                                text: skipped.clone(),
                                markdown: None,
                            },
                        })
                        .collect(),
                }],
                results,
            }],
        }
    }
}

impl SarifResult {
    fn of(diagnostic: &Diagnostic, files: &FileStore, base: Option<&Path>) -> Self {
        let loc = diagnostic.loc();
        let file = files.get(loc.file());
        let uri = artifact_uri(file.path(), base);

        let mut properties = SarifProperties {
            note: diagnostic.note().map(str::to_string),
            applicability: None,
        };
        let mut fixes = Vec::new();
        if let Some(fix) = diagnostic.fix() {
            properties.applicability = Some(applicability(fix.applicability()).to_string());
            fixes.push(SarifFix {
                description: Some(SarifMessage {
                    text: fix.title().to_string(),
                    markdown: None,
                }),
                artifact_changes: vec![SarifArtifactChange {
                    artifact_location: SarifArtifactLocation { uri: uri.clone() },
                    replacements: fix
                        .edits()
                        .iter()
                        .map(|edit| SarifReplacement {
                            deleted_region: region(files, loc.file(), edit.span()),
                            inserted_content: SarifContent {
                                text: edit.replacement().to_str().to_string(),
                            },
                        })
                        .collect(),
                }],
            });
        }

        SarifResult {
            rule_id: diagnostic.code().to_string(),
            level: level(diagnostic.severity()),
            message: SarifMessage {
                text: diagnostic.message().to_string(),
                markdown: None,
            },
            locations: vec![SarifLocation {
                physical_location: SarifPhysicalLocation {
                    artifact_location: SarifArtifactLocation { uri },
                    region: Some(region(files, loc.file(), loc.span())),
                },
            }],
            fixes,
            properties: if properties.note.is_some() || properties.applicability.is_some() {
                Some(properties)
            } else {
                None
            },
        }
    }
}

impl SarifRule {
    /// The rules that produced at least one result.
    fn of_used(diagnostics: &[Diagnostic], registry: &RuleRegistry) -> Vec<Self> {
        let mut seen: Vec<ErrorCode> = Vec::new();
        for diagnostic in diagnostics {
            let code = *diagnostic.code();
            if !seen.contains(&code) {
                seen.push(code);
            }
        }
        seen.into_iter()
            .filter_map(|code| {
                let rule = registry.by_code(&code)?;
                let docs = rule.docs();
                Some(SarifRule {
                    id: code.to_string(),
                    name: Some(docs.name.to_string()),
                    short_description: SarifMessage {
                        text: docs.summary.to_string(),
                        markdown: None,
                    },
                    // The summary is the first paragraph, so it is the plain
                    // text rendering of the markdown.
                    full_description: SarifMessage {
                        text: docs.summary.to_string(),
                        markdown: Some(docs.text.to_string()),
                    },
                    default_configuration: SarifRuleConfiguration {
                        level: level(rule.severity()),
                        enabled: rule.is_enabled_by_default(),
                    },
                })
            })
            .collect()
    }
}

/// A path as a SARIF artifact URI: forward slashes, percent-encoded, relative.
fn artifact_uri(path: &Path, base: Option<&Path>) -> String {
    let relative = base
        .and_then(|base| path.strip_prefix(base).ok())
        .unwrap_or(path);
    let text = relative.to_string_lossy().replace('\\', "/");
    let text = text.strip_prefix("./").unwrap_or(&text);
    let is_absolute = text.starts_with('/');
    let mut uri = url::Url::parse("https://sarif.invalid/").expect("valid base URL");
    uri.set_path(text);
    let path = uri.path();
    if is_absolute {
        path.to_string()
    } else {
        path.strip_prefix('/').unwrap_or(path).to_string()
    }
}

/// The region a span covers, in SARIF's 1-based, end-exclusive columns.
fn region(files: &FileStore, file: FileId, span: &std::ops::Range<usize>) -> SarifRegion {
    let converter = files.get(file).source_mapping_utf32();
    let start = converter.source_loc(span.start);
    let end = converter.source_loc(span.end);
    SarifRegion {
        start_line: start.line + 1,
        start_column: start.col + 1,
        end_line: end.line + 1,
        end_column: end.col + 1,
    }
}

/// A severity as a SARIF level.
fn level(severity: Severity) -> SarifLevel {
    match severity {
        Severity::Error => SarifLevel::Error,
        Severity::Warning => SarifLevel::Warning,
        Severity::Info => SarifLevel::Note,
        Severity::Note => SarifLevel::None,
    }
}

/// An applicability as it is spelled in the JSON output.
fn applicability(applicability: Applicability) -> &'static str {
    match applicability {
        Applicability::Safe => "safe",
        Applicability::Unsafe => "unsafe",
        Applicability::DisplayOnly => "display-only",
    }
}

#[cfg(test)]
mod tests {
    use insta::assert_snapshot;

    use super::*;
    use crate::{
        rule::{register_builtin_rules, registry::ActiveRules},
        Encoding, FileId, FileSettings,
    };

    /// A file with a port that has no mode.
    const SOURCE: &[u8] = b"entity demo is\n  port (clk : std_logic);\nend entity demo;\n";

    fn files(source: &[u8]) -> FileStore {
        let mut files = FileStore::new();
        files.insert(
            "demo.vhd",
            source.to_vec(),
            FileSettings {
                encoding: Encoding::Utf8,
                ..FileSettings::default()
            },
        );
        files
    }

    fn registry() -> RuleRegistry {
        let mut registry = RuleRegistry::new();
        register_builtin_rules(&mut registry);
        registry
    }

    fn invocation() -> Invocation {
        Invocation {
            exit_code: 1,
            execution_successful: true,
            skipped: Vec::new(),
            command_line: "vhdl-lint".to_string(),
        }
    }

    fn diagnose(source: &[u8]) -> (FileStore, RuleRegistry, Vec<Diagnostic>) {
        let files = files(source);
        let registry = registry();
        let rules = ActiveRules::new(registry.rules());
        let diagnostics = crate::parse_and_analyze_file(files.get(FileId(0)), FileId(0), &rules)
            .into_diagnostics();
        (files, registry, diagnostics)
    }

    #[test]
    fn a_finding_becomes_a_result() {
        let (files, registry, diagnostics) = diagnose(SOURCE);
        let log = SarifLog::new(&diagnostics, &files, &registry, invocation(), None);
        assert_snapshot!(serde_json::to_string_pretty(&log).unwrap(), @r#"
        {
          "$schema": "https://json.schemastore.org/sarif-2.1.0.json",
          "version": "2.1.0",
          "runs": [
            {
              "tool": {
                "driver": {
                  "name": "vhdl-lint",
                  "version": "0.1.0",
                  "informationUri": "https://vhdl-ls.github.io/rust_hdl/vhdl-lint/",
                  "rules": [
                    {
                      "id": "IDM002",
                      "name": "explicit-port-mode",
                      "shortDescription": {
                        "text": "Checks that port clauses have an explicit mode set."
                      },
                      "fullDescription": {
                        "text": "Checks that port clauses have an explicit mode set.",
                        "markdown": "Checks that port clauses have an explicit mode set.\n\n# Non-compliant example\n\n```vhdl\nentity foo is\n    port (\n        clk : std_logic\n    );\nend foo;\n```\n\n# Compliant example\n\n```vhdl\nentity foo is\n    port (\n        clk : in std_logic\n    );\nend foo;\n```"
                      },
                      "defaultConfiguration": {
                        "level": "warning",
                        "enabled": true
                      }
                    }
                  ]
                }
              },
              "columnKind": "unicodeCodePoints",
              "invocations": [
                {
                  "executionSuccessful": true,
                  "exitCode": 1,
                  "commandLine": "vhdl-lint"
                }
              ],
              "results": [
                {
                  "ruleId": "IDM002",
                  "level": "warning",
                  "message": {
                    "text": "missing port mode"
                  },
                  "locations": [
                    {
                      "physicalLocation": {
                        "artifactLocation": {
                          "uri": "demo.vhd"
                        },
                        "region": {
                          "startLine": 2,
                          "startColumn": 9,
                          "endLine": 2,
                          "endColumn": 12
                        }
                      }
                    }
                  ],
                  "fixes": [
                    {
                      "description": {
                        "text": "spell out 'in' if that is intended"
                      },
                      "artifactChanges": [
                        {
                          "artifactLocation": {
                            "uri": "demo.vhd"
                          },
                          "replacements": [
                            {
                              "deletedRegion": {
                                "startLine": 2,
                                "startColumn": 14,
                                "endLine": 2,
                                "endColumn": 14
                              },
                              "insertedContent": {
                                "text": " in"
                              }
                            }
                          ]
                        }
                      ]
                    }
                  ],
                  "properties": {
                    "note": "a port without a mode is an input",
                    "applicability": "display-only"
                  }
                }
              ]
            }
          ]
        }
        "#);
    }

    #[test]
    fn the_region_is_one_based_and_end_exclusive() {
        let (files, registry, diagnostics) = diagnose(SOURCE);
        let log = SarifLog::new(&diagnostics, &files, &registry, invocation(), None);
        let result = &log.runs[0].results[0];
        let region = result.locations[0]
            .physical_location
            .region
            .as_ref()
            .expect("a region");
        // The port identifier `clk` on the second line, columns 9 to 12.
        assert_eq!(region.start_line, 2);
        assert_eq!(region.start_column, 9);
        assert_eq!(region.end_column, 12);
    }

    #[test]
    fn a_clean_run_has_no_results_but_still_reports_the_invocation() {
        let (files, registry, diagnostics) = diagnose(b"entity demo is\nend entity demo;\n");
        let mut invocation = invocation();
        invocation.exit_code = 0;
        let log = SarifLog::new(&diagnostics, &files, &registry, invocation, None);
        assert!(log.runs[0].results.is_empty());
        // An empty result set and a linter that never ran must not look alike.
        assert!(log.runs[0].invocations[0].execution_successful);
        assert_eq!(log.runs[0].invocations[0].exit_code, 0);
    }

    #[test]
    fn a_skipped_file_is_reported_as_a_notification() {
        let (files, registry, diagnostics) = diagnose(SOURCE);
        let mut invocation = invocation();
        invocation.skipped = vec!["could not read foo.vhd".to_string()];
        invocation.execution_successful = false;
        let log = SarifLog::new(&diagnostics, &files, &registry, invocation, None);
        let notifications = &log.runs[0].invocations[0].tool_execution_notifications;
        assert_eq!(notifications.len(), 1);
        assert_eq!(notifications[0].message.text, "could not read foo.vhd");
        assert!(!log.runs[0].invocations[0].execution_successful);
    }

    #[test]
    fn a_path_becomes_a_uri() {
        // SARIF wants a URI, not a filesystem path.
        assert_eq!(
            artifact_uri(Path::new("rtl/demo.vhd"), None),
            "rtl/demo.vhd"
        );
        assert_eq!(
            artifact_uri(Path::new("./rtl/demo.vhd"), None),
            "rtl/demo.vhd"
        );
        assert_eq!(
            artifact_uri(Path::new("rtl\\demo.vhd"), None),
            "rtl/demo.vhd"
        );
        assert_eq!(
            artifact_uri(Path::new("rtl/my file.vhd"), None),
            "rtl/my%20file.vhd"
        );
        // A path under the working directory is relative to it.
        assert_eq!(
            artifact_uri(
                Path::new("/project/rtl/demo.vhd"),
                Some(Path::new("/project"))
            ),
            "rtl/demo.vhd"
        );
        // A path outside it stays absolute.
        assert_eq!(
            artifact_uri(
                Path::new("/elsewhere/demo.vhd"),
                Some(Path::new("/project"))
            ),
            "/elsewhere/demo.vhd"
        );
    }

    #[test]
    fn every_severity_has_a_level() {
        // SARIF has no `info`, and `none` is the lowest level still reported.
        assert_eq!(level(Severity::Error), SarifLevel::Error);
        assert_eq!(level(Severity::Warning), SarifLevel::Warning);
        assert_eq!(level(Severity::Info), SarifLevel::Note);
        assert_eq!(level(Severity::Note), SarifLevel::None);
        // The levels are spelled the way SARIF spells them.
        assert_eq!(
            serde_json::to_string(&[
                SarifLevel::Error,
                SarifLevel::Warning,
                SarifLevel::Note,
                SarifLevel::None
            ])
            .unwrap(),
            r#"["error","warning","note","none"]"#
        );
    }

    #[test]
    fn only_the_rules_that_fired_are_reported() {
        let (files, registry, diagnostics) = diagnose(SOURCE);
        let log = SarifLog::new(&diagnostics, &files, &registry, invocation(), None);
        let rules = &log.runs[0].tool.driver.rules;
        assert_eq!(rules.len(), 1);
        assert_eq!(rules[0].id, "IDM002");
        assert_eq!(rules[0].name.as_deref(), Some("explicit-port-mode"));
        assert_eq!(rules[0].default_configuration.level, SarifLevel::Warning);
        assert!(rules[0].default_configuration.enabled);
    }
}
