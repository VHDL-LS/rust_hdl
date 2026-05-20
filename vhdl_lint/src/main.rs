use std::{io::IsTerminal, path::PathBuf, process::ExitCode};

use anstream::ColorChoice;
use ariadne::Source;
use clap::{Parser, ValueEnum};
use vhdl_syntax::parser;

use crate::reporting::parser_diagnostic_to_report;

mod reporting;

#[derive(Copy, Clone, Debug, PartialEq, Eq, ValueEnum)]
enum ColorWhen {
    Auto,
    Always,
    Never,
}

impl From<ColorWhen> for ColorChoice {
    fn from(c: ColorWhen) -> Self {
        match c {
            ColorWhen::Auto => ColorChoice::Auto,
            ColorWhen::Always => ColorChoice::Always,
            ColorWhen::Never => ColorChoice::Never,
        }
    }
}

#[derive(Parser, Debug)]
struct Args {
    file: PathBuf,

    /// Control color output.
    #[arg(long, value_name = "WHEN", default_value = "auto")]
    color: ColorWhen,
}

fn main() -> ExitCode {
    let args = Args::parse();

    let choice: ColorChoice = args.color.into();
    choice.write_global();
    let use_color = match choice {
        ColorChoice::Always | ColorChoice::AlwaysAnsi => true,
        ColorChoice::Never => false,
        ColorChoice::Auto => std::io::stderr().is_terminal(),
    };

    let content = match std::fs::read(&args.file) {
        Ok(content) => content,
        Err(io_err) => {
            // TODO: Nicer error message
            println!(
                "Cannot open file {}: {}",
                args.file.display(),
                io_err.kind()
            );
            println!("{}", io_err);
            return ExitCode::FAILURE;
        }
    };

    let (_, diagnostics) = parser::parse(content);
    if diagnostics.is_empty() {
        return ExitCode::SUCCESS;
    }

    let fname = args.file.file_name().unwrap().to_string_lossy().to_string();
    let config = ariadne::Config::new().with_color(use_color);
    let mut stderr = anstream::AutoStream::new(std::io::stderr(), choice);

    for diagnostic in diagnostics {
        let report = parser_diagnostic_to_report(&diagnostic, fname.as_str(), config);
        report
            .write(
                (
                    fname.as_str(),
                    Source::from(std::fs::read_to_string(&args.file).unwrap()),
                ),
                &mut stderr,
            )
            .unwrap();
    }

    ExitCode::FAILURE
}
