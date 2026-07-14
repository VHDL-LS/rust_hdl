// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2025, Lukas Scheller lukasscheller@icloud.com

pub mod builders;
pub use builders::BuilderGenerator;
pub mod checked_nodes;
pub use checked_nodes::CheckedNodeGenerator;
pub mod meta;
pub use meta::MetaGenerator;
pub mod naming;
pub mod syntax_nodes;
pub use syntax_nodes::SyntaxNodeGenerator;

use crate::model::Model;
use anyhow::Result;
use proc_macro2::TokenStream;
use similar::{ChangeTag, TextDiff};
use std::io::Write as _;
use std::path::Path;
use std::process::{Command, Stdio};

/// A code generator that transforms a [`Model`] into one or more Rust source files.
pub trait Generator {
    fn name(&self) -> &str;
    /// Returns `(file_stem, token_stream)` pairs — one entry per output file.
    fn generate_files(&self, model: &Model) -> Vec<(String, TokenStream)>;
}

// Copyright header prepended to every generated file.
// Year is hardcoded to match the committed generated files.
const COPYRIGHT_HEADER: &str = "\
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2026, Lukas Scheller lukasscheller@icloud.com
";

/// Format a Rust source string using rustfmt (via stdin/stdout).
fn rustfmt_code(code: &str) -> Result<String> {
    let mut child = Command::new("rustfmt")
        .arg("--edition")
        .arg("2021")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .map_err(|e| anyhow::anyhow!("Failed to spawn rustfmt: {e}"))?;

    if let Some(mut stdin) = child.stdin.take() {
        stdin.write_all(code.as_bytes())?;
    }

    let output = child.wait_with_output()?;
    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(anyhow::anyhow!("rustfmt failed: {stderr}"));
    }

    Ok(String::from_utf8(output.stdout)?)
}

/// Combine copyright header + token stream content and format with rustfmt.
fn generate_file_content(token_stream: &TokenStream) -> Result<String> {
    let code = format!("{}{}", COPYRIGHT_HEADER, token_stream);
    rustfmt_code(&code)
}

/// Run all generators, write output to `output_dir`, format each file with rustfmt.
pub fn run_generators(
    generators: &[&dyn Generator],
    model: &Model,
    output_dir: &Path,
) -> Result<()> {
    std::fs::create_dir_all(output_dir)?;

    for generator in generators {
        println!("Running generator: {}", generator.name());
        for (stem, token_stream) in generator.generate_files(model) {
            let content = generate_file_content(&token_stream)?;
            let path = output_dir.join(format!("{stem}.rs"));
            std::fs::write(&path, &content)?;
            println!("  Wrote {}", path.display());
        }
    }

    Ok(())
}

/// Same as `run_generators` but compare to existing files instead of writing.
/// Returns a list of file stems that would change.
pub fn check_generators(
    generators: &[&dyn Generator],
    model: &Model,
    output_dir: &Path,
) -> Result<Vec<String>> {
    let mut stale = Vec::new();

    for generator in generators {
        for (stem, token_stream) in generator.generate_files(model) {
            let generated_content = generate_file_content(&token_stream)
                .map_err(|e| anyhow::anyhow!("[{}] {e}", generator.name()))?;
            let path = output_dir.join(format!("{stem}.rs"));

            let existing_content = std::fs::read_to_string(&path).unwrap_or_default();

            if existing_content != generated_content {
                let diff = TextDiff::from_lines(&existing_content, &generated_content);
                eprintln!("--- {stem}.rs (on disk)");
                eprintln!("+++ {stem}.rs (would be generated)");
                for change in diff.iter_all_changes() {
                    let sign = match change.tag() {
                        ChangeTag::Delete => "-",
                        ChangeTag::Insert => "+",
                        ChangeTag::Equal => " ",
                    };
                    eprint!("{sign}{change}");
                }
                stale.push(stem);
            }
        }
    }

    Ok(stale)
}
