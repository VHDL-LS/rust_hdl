use std::{path::PathBuf, process::ExitCode};

use annotate_snippets::{renderer::DecorStyle, Renderer};
use clap::Parser;
use vhdl_syntax::{
    fmt::encoding::LossyUtf8Encoder,
    parser,
    syntax::AstNode,
    text::{char_encoding, source_loc::SourceLocConverter},
};

use crate::reporting::parser_diagnostic_to_report;

mod reporting;

#[derive(Parser, Debug)]
struct Args {
    /// The file to lint.
    file: PathBuf,
}

fn main() -> ExitCode {
    let args = Args::parse();

    let content = match std::fs::read(&args.file) {
        Ok(content) => content,
        Err(io_err) => {
            anstream::eprintln!("couldn't read `{}`: {}", args.file.display(), io_err);
            return ExitCode::from(2);
        }
    };

    let (ast, diagnostics) = parser::parse(content);
    if diagnostics.is_empty() {
        return ExitCode::SUCCESS;
    }

    let cache = SourceLocConverter::new_lossy::<LossyUtf8Encoder, char_encoding::Utf8>(&ast.raw());

    let fname = args.file.file_name().map(|path| path.to_string_lossy());

    let report = diagnostics
        .into_iter()
        .map(|diag| parser_diagnostic_to_report(&diag, fname.clone(), &ast.raw(), &cache))
        .collect::<Vec<_>>();

    let renderer = Renderer::styled().decor_style(DecorStyle::Unicode);
    anstream::eprintln!("{}", renderer.render(&report));

    let count = report.len();
    if count == 1 {
        anstream::eprintln!("1 error emitted");
    } else {
        anstream::eprintln!("{count} errors emitted");
    }

    ExitCode::from(1)
}
