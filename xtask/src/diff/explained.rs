// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2025, Lukas Scheller lukasscheller@icloud.com

//! Reads the developer book's write-ups of the differences between the LRM grammar
//! and the grammar `vhdl_syntax` models.
//!
//! Every markdown file below `book/src/lrm-differences/` except the chapter's
//! `README.md` may quote a difference in a fenced `diff` block, headed by the
//! production it is about:
//!
//! ```text
//! ```diff
//! DesignFile =
//!    DesignUnit
//!    DesignUnit*
//! +  '#eof'
//! ```
//! ```
//!
//! The quoted diff has to be exactly what [`super::diff_grammar`] renders for that
//! production, so a difference counts as *explained* only for as long as the prose
//! still describes the grammar. Once the grammar moves on, the fence stops matching
//! and is reported as stale rather than quietly going on being counted.

use anyhow::{bail, Context, Result};
use std::path::Path;

/// One fenced `diff` block from the book: a difference somebody wrote the reason for.
#[derive(Debug)]
pub struct Explanation {
    /// The production named by the fence's header line (`DesignFile =`).
    pub production: String,
    /// The diff below the header, [`normalize`]d so it can be compared line by line
    /// with the diff the tool renders.
    pub diff: Vec<String>,
    /// The file the fence came from, for reporting.
    file: String,
    /// 1-based line of the opening fence, for reporting.
    line: usize,
}

impl Explanation {
    /// `design.md:3`, the way an editor jumps to it.
    pub fn location(&self) -> String {
        format!("{}:{}", self.file, self.line)
    }
}

/// Every explanation in `dir`, in file then document order.
pub fn load(dir: &Path) -> Result<Vec<Explanation>> {
    let entries =
        std::fs::read_dir(dir).with_context(|| format!("cannot read {}", dir.display()))?;

    let mut files = Vec::new();
    for entry in entries {
        let path = entry
            .with_context(|| format!("cannot read {}", dir.display()))?
            .path();
        // The chapter's own introduction explains the idea, not a single difference.
        let is_chapter_intro = path.file_name().is_some_and(|name| name == "README.md");
        if path.extension().is_some_and(|ext| ext == "md") && !is_chapter_intro {
            files.push(path);
        }
    }
    files.sort();

    let mut explanations = Vec::new();
    for path in &files {
        let text = std::fs::read_to_string(path)
            .with_context(|| format!("cannot read {}", path.display()))?;
        let name = path.file_name().unwrap_or_default().to_string_lossy();
        explanations.extend(parse(&text, &name)?);
    }
    Ok(explanations)
}

/// Trims the trailing whitespace of every line and drops the blank lines around the
/// block, so that a fence indented or padded by an editor still compares equal.
/// Leading whitespace is left alone -- it is what tells a diff's `-`, `+` and context
/// lines apart from one another.
pub fn normalize(lines: Vec<String>) -> Vec<String> {
    let mut lines: Vec<String> = lines
        .into_iter()
        .map(|line| line.trim_end().to_owned())
        .collect();
    while lines.last().is_some_and(String::is_empty) {
        lines.pop();
    }
    let leading = lines.iter().take_while(|line| line.is_empty()).count();
    lines.drain(..leading);
    lines
}

/// The fenced `diff` blocks of one markdown file.
///
/// Fences carrying any other info string are skipped whole, so a `diff` fence is never
/// found inside one.
pub(super) fn parse(text: &str, file: &str) -> Result<Vec<Explanation>> {
    let mut explanations = Vec::new();
    let mut lines = text.lines().enumerate();

    while let Some((index, line)) = lines.next() {
        let Some(info) = line.trim().strip_prefix("```") else {
            continue;
        };
        let mut body = Vec::new();
        let mut terminated = false;
        for (_, line) in lines.by_ref() {
            if line.trim() == "```" {
                terminated = true;
                break;
            }
            body.push(line.to_owned());
        }

        if info.trim() != "diff" {
            continue;
        }
        let line = index + 1;
        if !terminated {
            bail!("{file}:{line}: unterminated diff fence");
        }
        explanations.push(explanation(body, file, line)?);
    }

    Ok(explanations)
}

/// Splits a fence into the production it heads and the diff it quotes.
fn explanation(mut body: Vec<String>, file: &str, line: usize) -> Result<Explanation> {
    if body.is_empty() {
        bail!("{file}:{line}: empty diff fence");
    }
    let header = body.remove(0);
    let production = header.trim().trim_end_matches('=').trim();
    let is_production_name = !production.is_empty()
        && production
            .chars()
            .all(|c| c.is_ascii_alphanumeric() || c == '_');
    if !is_production_name {
        bail!(
            "{file}:{line}: a diff fence starts with the production it explains, \
             written like `DesignFile =`; found `{header}`"
        );
    }

    Ok(Explanation {
        production: production.to_owned(),
        diff: normalize(body),
        file: file.to_owned(),
        line,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parsed(text: &str) -> Vec<Explanation> {
        parse(text, "test.md").expect("fence does not parse")
    }

    #[test]
    fn a_fence_is_the_production_it_heads_and_the_diff_it_quotes() {
        let explanations = parsed(
            "# Design\n\
             \n\
             ```diff\n\
             DesignFile =\n\
             \x20  DesignUnit\n\
             +  '#eof'\n\
             ```\n\
             \n\
             prose\n",
        );
        assert_eq!(explanations.len(), 1);
        assert_eq!(explanations[0].production, "DesignFile");
        assert_eq!(explanations[0].diff, ["   DesignUnit", "+  '#eof'"]);
        assert_eq!(explanations[0].location(), "test.md:3");
    }

    #[test]
    fn a_header_may_trail_whitespace_after_its_equals_sign() {
        let explanations = parsed("```diff\nDesignFile = \n+  '#eof'\n```\n");
        assert_eq!(explanations[0].production, "DesignFile");
        assert_eq!(explanations[0].diff, ["+  '#eof'"]);
    }

    #[test]
    fn several_fences_in_one_file_are_all_read() {
        let explanations = parsed("```diff\nA =\n+  'a'\n```\n\n```diff\nB =\n+  'b'\n```\n");
        let names: Vec<_> = explanations.iter().map(|e| e.production.as_str()).collect();
        assert_eq!(names, ["A", "B"]);
    }

    #[test]
    fn a_fence_of_another_language_is_skipped_whole() {
        let explanations = parsed("```rust\n```diff\nA =\n```\n```\n");
        assert!(explanations.is_empty());
    }

    #[test]
    fn a_fence_without_a_production_header_is_an_error() {
        let err = parse("```diff\n-  Prefix\n+  Name\n```\n", "test.md").unwrap_err();
        assert!(err.to_string().contains("starts with the production"));
    }

    #[test]
    fn an_unterminated_fence_is_an_error() {
        let err = parse("```diff\nA =\n+  'a'\n", "test.md").unwrap_err();
        assert!(err.to_string().contains("unterminated"));
    }

    #[test]
    fn normalize_drops_trailing_whitespace_but_keeps_the_diff_column() {
        assert_eq!(
            normalize(vec![
                String::new(),
                "-  A  ".to_owned(),
                "   B".to_owned(),
                "  ".to_owned(),
            ]),
            ["-  A", "   B"]
        );
    }
}
