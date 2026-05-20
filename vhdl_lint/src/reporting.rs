use core::fmt;
use std::ops::Range;

use vhdl_syntax::{parser::diagnostics::ParserDiagnostic, tokens::TokenKind};

pub fn parser_diagnostic_to_report<'a>(
    diagnostic: &ParserDiagnostic,
    file_name: &'a str,
    config: ariadne::Config,
) -> ariadne::Report<'a, (&'a str, Range<usize>)> {
    use ariadne::{Label, ReportKind};
    match diagnostic {
        ParserDiagnostic::ExpectedToken {
            expected: (insertion_pos, expected),
            found: (found_pos, found),
        } => ariadne::Report::build(
            ReportKind::Error,
            (file_name, *insertion_pos..*insertion_pos),
        )
        .with_config(config)
        .with_message(expected_token_message(expected, *found))
        .with_label(
            Label::new((file_name, *insertion_pos..*insertion_pos))
                .with_message(format!("{} expected here", expected_message(expected))),
        )
        .with_label(Label::new((file_name, found_pos.clone())).with_message("unexpected token"))
        .finish(),
        ParserDiagnostic::UnexpectedInput { span } => {
            ariadne::Report::build(ReportKind::Error, (file_name, span.clone()))
                .with_config(config)
                .with_message("UnexpectedInput")
                .with_label(
                    Label::new((file_name, span.clone())).with_message("This input is unexpected"),
                )
                .finish()
        }
    }
}

fn expected_message(expected: &[TokenKind]) -> String {
    use std::fmt::Write;
    match expected {
        [] => String::new(),
        [only] => format!("{}", Expected(*only)),
        [head @ .., last] => {
            let mut out = String::new();
            for (i, tok) in head.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                write!(out, "{}", Expected(*tok)).unwrap();
            }
            write!(out, ", or {}", Expected(*last)).unwrap();
            out
        }
    }
}

fn expected_token_message(expected: &[TokenKind], found: TokenKind) -> String {
    let exp_message = expected_message(expected);

    if found == TokenKind::Eof {
        format!("Expected {exp_message}")
    } else {
        format!("Expected {exp_message}, found {}", Expected(found))
    }
}

/// Newtype wrapper to properly format token kinds when printing them to the user
struct Expected(TokenKind);

impl fmt::Display for Expected {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0 {
            TokenKind::Keyword(kw)
             => {
                write!(f, "`{}`", kw.canonical_text())
            }
            TokenKind::Plus => write!(f, "`+`"),
            TokenKind::Minus => write!(f, "`-`"),
            TokenKind::EQ => write!(f, "`=`"),
            TokenKind::NE => write!(f, "`/=`"),
            TokenKind::LT => write!(f, "`<`"),
            TokenKind::LTE => write!(f, "`<=`"),
            TokenKind::GT => write!(f, "`>`"),
            TokenKind::GTE => write!(f, "`>=`"),
            TokenKind::QueEQ => write!(f, "`?=`"),
            TokenKind::QueNE => write!(f, "`?/=`"),
            TokenKind::QueLT => write!(f, "`?<`"),
            TokenKind::QueLTE => write!(f, "`?<=`"),
            TokenKind::QueGT => write!(f, "`?>`"),
            TokenKind::QueGTE => write!(f, "`?>=`"),
            TokenKind::Que => write!(f, "`?`"),
            TokenKind::QueQue => write!(f, "`??`"),
            TokenKind::Times => write!(f, "`*`"),
            TokenKind::Pow => write!(f, "`**`"),
            TokenKind::Div => write!(f, "`/`"),
            TokenKind::Tick => write!(f, "`'`"),
            TokenKind::LeftPar => write!(f, "`(`"),
            TokenKind::RightPar => write!(f, "`)`"),
            TokenKind::LeftSquare => write!(f, "`[`"),
            TokenKind::RightSquare => write!(f, "`]`"),
            TokenKind::SemiColon => write!(f, "`;`"),
            TokenKind::Colon => write!(f, "`:`"),
            TokenKind::Bar => write!(f, "`|`"),
            TokenKind::Dot => write!(f, "`.`"),
            TokenKind::BOX => write!(f, "`<>`"),
            TokenKind::LtLt => write!(f, "`<<`"),
            TokenKind::GtGt => write!(f, "`>>`"),
            TokenKind::Circ => write!(f, "`^`"),
            TokenKind::CommAt => write!(f, "`@`"),
            TokenKind::Concat => write!(f, "`&`"),
            TokenKind::Comma => write!(f, "`,`"),
            TokenKind::ColonEq => write!(f, "`:=`"),
            TokenKind::RightArrow => write!(f, "`=>`"),
            TokenKind::Eof => write!(f, "end of input"),
            TokenKind::Identifier => write!(f, "identifier"),
            TokenKind::AbstractLiteral => write!(f, "abstract literal"),
            TokenKind::StringLiteral => write!(f, "string literal"),
            TokenKind::BitStringLiteral => write!(f, "bitstring literal"),
            TokenKind::CharacterLiteral => write!(f, "character literal"),
            TokenKind::ToolDirective => write!(f, "tool directive"),
            TokenKind::Unterminated | TokenKind::Unknown => {
                unreachable!("should never be called by 'expected'")
            }
        }
    }
}

