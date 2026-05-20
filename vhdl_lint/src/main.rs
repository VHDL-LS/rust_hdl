use std::{path::PathBuf, process::ExitCode};

use ariadne::{Label, Report, ReportKind, Source};
use clap::Parser;
use vhdl_syntax::{
    parser::{self, diagnostics::ParserDiagnostic},
    tokens::TokenKind,
};

#[derive(Parser, Debug)]
struct Args {
    file: PathBuf,
}

fn main() -> ExitCode {
    let args = Args::parse();

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

    for diagnostic in diagnostics {
        let report = match diagnostic {
            ParserDiagnostic::ExpectedToken {
                expected: (insertion_pos, expected),
                found: (found_pos, found),
            } => Report::build(ReportKind::Error, (&fname, insertion_pos..insertion_pos))
                .with_message(expected_token_message(&expected, found))
                .with_label(
                    Label::new((&fname, insertion_pos..insertion_pos))
                        .with_message(format!("{} expected here", expected_message(&expected))),
                )
                .with_label(Label::new((&fname, found_pos)).with_message("unexpected token"))
                .finish(),
            ParserDiagnostic::UnexpectedInput { span } => {
                Report::build(ReportKind::Error, (&fname, span.clone()))
                    .with_message("UnexpectedInput")
                    .with_label(Label::new((&fname, span)).with_message("This input is unexpected"))
                    .finish()
            }
        };
        report
            .eprint((
                &fname,
                Source::from(std::fs::read_to_string(&args.file).unwrap()),
            ))
            .unwrap();
    }

    return ExitCode::FAILURE;
}

fn expected_message(expected: &[TokenKind]) -> String {
    if expected.len() == 1 {
        format!("{:?}", expected[0])
    } else {
        format!(
            "one of {}",
            expected
                .iter()
                .map(|f| format!("{f:?}"))
                .collect::<Vec<_>>()
                .join(",")
        )
    }
}

fn expected_token_message(expected: &[TokenKind], found: TokenKind) -> String {
    let exp_message = expected_message(expected);

    if found == TokenKind::Eof {
        format!("Expected {exp_message}")
    } else {
        format!("Expected {exp_message}, found {found:?}")
    }
}
