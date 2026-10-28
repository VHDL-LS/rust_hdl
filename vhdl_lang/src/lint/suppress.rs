// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.

//! Comment-based suppression of diagnostics.
//!
//! A VHDL comment of the form `-- vhdl_ls(rule1, rule2)` (or
//! `-- vhdl_ls rule1, rule2`) on the same line as a declaration, or on the
//! line directly above it, suppresses the listed diagnostics for that
//! declaration. Use `*` or `all` to suppress all diagnostics.
//!
//! Examples:
//! ```vhdl
//! signal debug_sig : std_logic; -- vhdl_ls(never_read)
//! -- vhdl_ls(unused, never_read)
//! signal another_sig : std_logic;
//! signal any_sig : std_logic; -- vhdl_ls(*)
//! ```

use crate::data::error_codes::ErrorCode;
use crate::data::Diagnostic;

/// The marker that identifies a suppression comment.
const SUPPRESS_MARKER: &str = "vhdl_ls";

/// Apply comment-based suppressions to a list of diagnostics.
///
/// Diagnostics whose position (or the line above it) has a suppression
/// comment matching the diagnostic's error code are removed.
pub fn apply_suppressions(diagnostics: &mut Vec<Diagnostic>) {
    diagnostics.retain(|diag| !is_suppressed(diag));
}

/// Check if a diagnostic is suppressed by a comment on the same line
/// or the line directly above.
fn is_suppressed(diag: &Diagnostic) -> bool {
    let line = diag.pos.range.start.line as usize;
    let contents = diag.pos.source.contents();

    // Check the same line (trailing comment)
    if let Some(line_text) = contents.get_line(line) {
        if let Some(rules) = parse_suppression_comment(line_text) {
            if rules.matches(diag.code) {
                return true;
            }
        }
    }

    // Check the line above (leading comment)
    if line > 0 {
        if let Some(prev_line_text) = contents.get_line(line - 1) {
            if let Some(rules) = parse_suppression_comment(prev_line_text) {
                // Only apply if the previous line is ONLY a comment
                // (not code followed by a comment)
                let trimmed = prev_line_text.trim_start();
                if trimmed.starts_with("--") && rules.matches(diag.code) {
                    return true;
                }
            }
        }
    }

    false
}

/// Parsed suppression rules from a comment.
struct SuppressionRules {
    /// Specific rule names (lowercase) to suppress.
    rules: Vec<String>,
    /// If true, suppress all rules.
    all: bool,
}

impl SuppressionRules {
    fn matches(&self, code: ErrorCode) -> bool {
        if self.all {
            return true;
        }
        let code_str = code.as_str();
        self.rules.iter().any(|r| r == code_str)
    }
}

/// Parse a suppression comment from a line of VHDL code.
///
/// Returns `Some(SuppressionRules)` if the line contains a suppression comment,
/// `None` otherwise.
///
/// Supported formats (case-insensitive):
/// - `-- vhdl_ls(never_read)`
/// - `-- vhdl_ls(never_read, unused)`
/// - `-- vhdl_ls never_read`
/// - `-- vhdl_ls never_read, unused`
/// - `-- vhdl_ls(*)` or `-- vhdl_ls(all)` — suppress all
fn parse_suppression_comment(line: &str) -> Option<SuppressionRules> {
    // Find the comment start
    let comment_start = line.find("--")?;
    let comment = &line[comment_start + 2..];

    // Find the marker (case-insensitive)
    let lower = comment.to_lowercase();
    let marker_pos = lower.find(SUPPRESS_MARKER)?;
    let after_marker = &comment[marker_pos + SUPPRESS_MARKER.len()..];

    // Skip optional whitespace and opening parenthesis
    let after_marker = after_marker.trim_start();
    let after_marker = after_marker.strip_prefix('(').unwrap_or(after_marker);

    // Extract rule names until closing paren or end of meaningful text
    let rules_text = if after_marker.contains(')') {
        after_marker.split(')').next().unwrap_or("")
    } else {
        // No parens: take until end of line, but stop at another comment marker
        after_marker.split("--").next().unwrap_or("").trim_end()
    };

    let rules_text = rules_text.trim();
    if rules_text.is_empty() {
        return None;
    }

    // Parse comma-separated rule names
    let rules: Vec<String> = rules_text
        .split(',')
        .map(|s| s.trim().to_lowercase())
        .filter(|s| !s.is_empty())
        .collect();

    if rules.is_empty() {
        return None;
    }

    let all = rules.iter().any(|r| r == "*" || r == "all");

    Some(SuppressionRules { rules, all })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::analysis::tests::LibraryBuilder;
    use crate::data::{Diagnostic, Position, Source};
    use std::path::Path;

    #[test]
    fn parses_parenthesized_single_rule() {
        let rules = parse_suppression_comment("signal x : bit; -- vhdl_ls(never_read)").unwrap();
        assert!(!rules.all);
        assert_eq!(rules.rules, vec!["never_read"]);
        assert!(rules.matches(ErrorCode::NeverRead));
        assert!(!rules.matches(ErrorCode::Unused));
    }

    #[test]
    fn parses_parenthesized_multiple_rules() {
        let rules = parse_suppression_comment("-- vhdl_ls(never_read, unused)").unwrap();
        assert!(!rules.all);
        assert!(rules.matches(ErrorCode::NeverRead));
        assert!(rules.matches(ErrorCode::Unused));
        assert!(!rules.matches(ErrorCode::Uninitialized));
    }

    #[test]
    fn parses_space_separated() {
        let rules = parse_suppression_comment("-- vhdl_ls never_read").unwrap();
        assert!(rules.matches(ErrorCode::NeverRead));
    }

    #[test]
    fn parses_wildcard() {
        let rules = parse_suppression_comment("-- vhdl_ls(*)").unwrap();
        assert!(rules.all);
        assert!(rules.matches(ErrorCode::NeverRead));
        assert!(rules.matches(ErrorCode::Unused));
        assert!(rules.matches(ErrorCode::Uninitialized));
    }

    #[test]
    fn parses_all_keyword() {
        let rules = parse_suppression_comment("-- vhdl_ls(all)").unwrap();
        assert!(rules.all);
    }

    #[test]
    fn ignores_non_suppression_comments() {
        assert!(parse_suppression_comment("-- just a comment").is_none());
        assert!(parse_suppression_comment("signal x : bit;").is_none());
        assert!(parse_suppression_comment("-- vhdl_ls").is_none());
    }

    #[test]
    fn case_insensitive() {
        let rules = parse_suppression_comment("-- VHDL_LS(Never_Read)").unwrap();
        assert!(rules.matches(ErrorCode::NeverRead));
    }

    #[test]
    fn with_trailing_text() {
        let rules = parse_suppression_comment("-- vhdl_ls(never_read) for debug purposes").unwrap();
        assert!(rules.matches(ErrorCode::NeverRead));
    }

    /// Helper: create a diagnostic at a given line in a source.
    fn make_diag(source: &Source, line: u32, code: ErrorCode) -> Diagnostic {
        Diagnostic::new(
            source.pos(Position::new(line, 0), Position::new(line, 1)),
            "test diagnostic",
            code,
        )
    }

    #[test]
    fn suppresses_trailing_comment_same_line() {
        let source = Source::inline(
            Path::new("test.vhd"),
            "signal x : bit; -- vhdl_ls(never_read)\nsignal y : bit;\n",
        );
        let mut diags = vec![make_diag(&source, 0, ErrorCode::NeverRead)];
        apply_suppressions(&mut diags);
        assert!(diags.is_empty(), "diagnostic should be suppressed");
    }

    #[test]
    fn suppresses_leading_comment_above() {
        let source = Source::inline(
            Path::new("test.vhd"),
            "-- vhdl_ls(never_read)\nsignal x : bit;\n",
        );
        let mut diags = vec![make_diag(&source, 1, ErrorCode::NeverRead)];
        apply_suppressions(&mut diags);
        assert!(diags.is_empty(), "diagnostic should be suppressed");
    }

    #[test]
    fn does_not_suppress_wrong_rule() {
        let source = Source::inline(
            Path::new("test.vhd"),
            "signal x : bit; -- vhdl_ls(unused)\n",
        );
        let mut diags = vec![make_diag(&source, 0, ErrorCode::NeverRead)];
        apply_suppressions(&mut diags);
        assert_eq!(diags.len(), 1, "diagnostic should NOT be suppressed");
    }

    #[test]
    fn does_not_suppress_different_line() {
        let source = Source::inline(
            Path::new("test.vhd"),
            "signal x : bit;\nsignal y : bit; -- vhdl_ls(never_read)\n",
        );
        let mut diags = vec![make_diag(&source, 0, ErrorCode::NeverRead)];
        apply_suppressions(&mut diags);
        assert_eq!(
            diags.len(),
            1,
            "diagnostic should NOT be suppressed (different line)"
        );
    }

    #[test]
    fn wildcard_suppresses_any_rule() {
        let source = Source::inline(Path::new("test.vhd"), "signal x : bit; -- vhdl_ls(*)\n");
        let mut diags = vec![
            make_diag(&source, 0, ErrorCode::NeverRead),
            make_diag(&source, 0, ErrorCode::Unused),
            make_diag(&source, 0, ErrorCode::Uninitialized),
        ];
        apply_suppressions(&mut diags);
        assert!(
            diags.is_empty(),
            "all diagnostics should be suppressed by wildcard"
        );
    }

    #[test]
    fn leading_comment_not_applied_if_code_before_comment() {
        // A trailing comment on a code line above should NOT suppress
        // the diagnostic on the next line (only pure-comment lines count)
        let source = Source::inline(
            Path::new("test.vhd"),
            "signal z : bit; -- vhdl_ls(never_read)\nsignal x : bit;\n",
        );
        let mut diags = vec![make_diag(&source, 1, ErrorCode::NeverRead)];
        apply_suppressions(&mut diags);
        assert_eq!(
            diags.len(),
            1,
            "trailing comment on code line should not suppress next line"
        );
    }

    #[test]
    fn end_to_end_with_linter() {
        // Full end-to-end test: analyze code with a suppression comment
        // and verify the diagnostic is filtered out.
        use crate::analysis::tests::add_standard_library;
        use crate::analysis::DesignRoot;

        let mut builder = LibraryBuilder::new();
        builder.code(
            "libname",
            "
entity ent is
  port (
    o : out bit
  );
end entity;

architecture a of ent is
  signal debug_sig : bit; -- vhdl_ls(never_read)
  signal unused_sig : bit;
begin
  debug_sig <= '1';
  unused_sig <= '0';
  o <= '0';
end architecture;",
        );

        // Build root manually so we can capture analyzed units in one analyze() call
        let symbols = builder.symbols();
        let mut root = DesignRoot::new(symbols.clone());
        let mut parse_diags = Vec::new();
        add_standard_library(symbols, &mut root);
        for (library_name, code) in builder.take_code() {
            root.add_design_file(library_name, code.design_file_diagnostics(&mut parse_diags));
        }
        let analyzed_units = root.analyze(&mut parse_diags);

        // Create a config that includes the 'libname' library
        let config = crate::Config::from_str(
            "[libraries]\nlibname.files = ['test.vhd']\n",
            Path::new("."),
        )
        .unwrap();

        // Run the linter
        let mut linter = crate::lint::dead_code::UnusedDeclarationsLinter::default();
        let mut diagnostics = Vec::new();
        linter.lint(&root, &config, &analyzed_units, &mut diagnostics);

        // Before suppression: should have diagnostics for debug_sig (NeverRead)
        // and unused_sig (NeverRead)
        assert!(
            !diagnostics.is_empty(),
            "should have diagnostics before suppression"
        );

        // Apply suppressions
        apply_suppressions(&mut diagnostics);

        // After suppression: debug_sig's NeverRead should be gone
        let has_debug_sig_never_read = diagnostics
            .iter()
            .any(|d| d.code == ErrorCode::NeverRead && d.message.contains("debug_sig"));
        assert!(
            !has_debug_sig_never_read,
            "debug_sig NeverRead should be suppressed, got: {:?}",
            diagnostics
        );

        // unused_sig's NeverRead should still be there
        let has_unused_sig = diagnostics
            .iter()
            .any(|d| d.code == ErrorCode::NeverRead && d.message.contains("unused_sig"));
        assert!(
            has_unused_sig,
            "unused_sig NeverRead should NOT be suppressed, got: {:?}",
            diagnostics
        );
    }
}
