//! Conversion from a parser [`SyntaxErr`] into the frontend [`Diagnostic`].
//!
//! This is the seam where the tree is consulted: an `Expected` error stores
//! only the zero-width insertion locus, so the token actually *found* there is
//! recovered from the syntax tree and resolved into a concrete span. The
//! resulting [`Diagnostic`] holds only raw source spans and no longer depends on
//! the tree.
//!
//! All human-readable message wording for syntax errors lives here, since it is
//! part of producing the diagnostic — not of rendering it.

use std::fmt;
use std::ops::Range;

use vhdl_syntax::{
    parser::error::{SyntaxErr, SyntaxErrKind},
    syntax::node::{SyntaxNode, SyntaxToken},
    tokens::{tokenizer::UnterminatedKind, TokenKind},
};

use crate::diagnostic::{Diagnostic, DiagnosticCode, Label, SourceId};

impl Diagnostic {
    /// Build a [`Diagnostic`] from a parser [`SyntaxErr`].
    ///
    /// `tree` is the syntax tree the error was produced from (used to resolve the
    /// found token of an `Expected` error); `source` identifies the file the
    /// error's spans refer to.
    pub fn from_syntax_err(err: &SyntaxErr, tree: &SyntaxNode, source: SourceId) -> Diagnostic {
        let span = err.span_raw().clone();
        match err.err() {
            SyntaxErrKind::Expected { kinds } => {
                let found = token_after(tree, &span);
                let diag = Diagnostic::new(
                    DiagnosticCode::Syntax,
                    expected_token_message(kinds, found.kind()),
                )
                .with_label(Label::primary(
                    source,
                    span,
                    format!("{} expected here", expected_message(kinds)),
                ));

                if found.kind().is_eof() {
                    diag
                } else {
                    diag.with_label(Label::context(
                        source,
                        found.text_range(),
                        "unexpected token",
                    ))
                }
            }
            SyntaxErrKind::Unexpected { .. } => {
                Diagnostic::new(DiagnosticCode::Syntax, "Unexpected input").with_label(
                    Label::primary(source, span, "This input is unexpected"),
                )
            }
            SyntaxErrKind::Illegal { .. } => Diagnostic::new(DiagnosticCode::Syntax, "Illegal input")
                .with_label(Label::primary(source, span, "This input is unexpected")),
            SyntaxErrKind::Unterminated { kind } => Diagnostic::new(
                DiagnosticCode::Syntax,
                format!("Unterminated {}", describe_unterminated(kind)),
            )
            .with_label(Label::primary(
                source,
                span,
                "Opening delimiter was never closed",
            )),
        }
    }
}

/// The token covering the end of `span` — i.e. the token found where one of the
/// expected tokens should have appeared.
pub(crate) fn token_after(tree: &SyntaxNode, span: &Range<usize>) -> SyntaxToken {
    tree.covering_token_at_offset(span.end)
}

pub(crate) fn describe_unterminated(kind: &UnterminatedKind) -> &'static str {
    match kind {
        UnterminatedKind::StringLiteral => "string literal",
        UnterminatedKind::BasedLiteral => "based literal",
        UnterminatedKind::ExtendedIdentifier => "extended identifier",
        UnterminatedKind::BlockComment => "comment",
    }
}

pub(crate) fn expected_message(expected: &[TokenKind]) -> String {
    use std::fmt::Write;
    match expected {
        [] => unreachable!("At least one expected must be present"),
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

pub(crate) fn expected_token_message(expected: &[TokenKind], found: TokenKind) -> String {
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
            TokenKind::Keyword(kw) => {
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
            TokenKind::Unknown => {
                unreachable!("should never be called by 'expected'")
            }
        }
    }
}
