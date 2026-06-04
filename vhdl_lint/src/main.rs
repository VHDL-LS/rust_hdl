use std::{path::PathBuf, process::ExitCode};

use clap::Parser;
use vhdl_syntax::{
    fmt::encoding::LossyUtf8Encoder,
    parser,
    syntax::AstNode,
    text::{char_encoding, source_loc::SourceLocConverter},
};

use crate::diagnostic::{Diagnostic, SourceId};
use crate::reporting::Report;
use crate::source::SourceMap;

pub mod diagnostic;
mod reporting;
mod source;
mod syntax_err;

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

    let (ast, errors) = parser::parse(content);
    if errors.is_empty() {
        return ExitCode::SUCCESS;
    }

    let tree = ast.raw();
    let converter = SourceLocConverter::new_lossy::<LossyUtf8Encoder, char_encoding::Utf8>(&tree);
    let name = args
        .file
        .file_name()
        .map(|name| name.to_string_lossy().into_owned());

    let source_id = SourceId::new(0);
    let mut sources = SourceMap::new();
    sources.insert(source_id, name, &tree, converter);

    let diagnostics: Vec<Diagnostic> = errors
        .iter()
        .map(|error| Diagnostic::from_syntax_err(error, &tree, source_id))
        .collect();

    let report = Report::new(&sources, &diagnostics);
    anstream::eprintln!("{}", report);

    let count = diagnostics.len();
    if count == 1 {
        anstream::eprintln!("1 error emitted");
    } else {
        anstream::eprintln!("{count} errors emitted");
    }

    ExitCode::from(1)
}
