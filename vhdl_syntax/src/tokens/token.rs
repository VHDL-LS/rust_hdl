// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2024, Lukas Scheller lukasscheller@icloud.com

use crate::interning::{Interned, Interner};
use crate::latin_1::Latin1Str;
use crate::standard::VHDLStandard;
use crate::tokens::{TokenKind, TokenStream, Tokenizer, Trivia};
use std::fmt::Debug;
use std::io::{self, Write};
use std::sync::RwLock;

static STR_INTERNER: RwLock<Interner<Latin1Str>> = RwLock::new(Interner::new());

/// A source-code token.
#[derive(Clone, Eq, PartialEq)]
pub struct Token {
    pub(crate) leading_trivia: Trivia,
    kind: TokenKind,
    text: Interned<Latin1Str>,
}

impl Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Token")
            .field("leading_trivia", &self.leading_trivia)
            .field("kind", &self.kind)
            .field("text", &self.text())
            .finish()
    }
}

impl Token {
    pub fn new(kind: TokenKind, text: impl AsRef<Latin1Str>, leading_trivia: Trivia) -> Token {
        Token {
            leading_trivia,
            kind,
            text: Interned::get(&STR_INTERNER, text.as_ref()),
        }
    }

    pub(crate) fn eof(leading_trivia: Trivia) -> Token {
        Token::new(TokenKind::Eof, b"", leading_trivia)
    }

    #[cfg(test)]
    pub fn simple(kind: TokenKind, text: impl AsRef<Latin1Str>) -> Token {
        Token::new(kind, text, Trivia::default())
    }

    pub fn kind(&self) -> TokenKind {
        self.kind
    }

    pub fn leading_trivia(&self) -> &Trivia {
        &self.leading_trivia
    }

    pub fn set_leading_trivia(&mut self, trivia: Trivia) {
        self.leading_trivia = trivia;
    }

    pub fn text(&self) -> &Latin1Str {
        self.text.value(&STR_INTERNER)
    }

    /// The length of the main content of this token in bytes without any trivia
    pub fn text_len(&self) -> usize {
        self.text().len()
    }

    /// The length of this token including trivia
    pub fn byte_len(&self) -> usize {
        self.leading_trivia.byte_len() + self.text_len()
    }

    pub fn write_to(&self, writer: &mut impl Write) -> io::Result<()> {
        self.leading_trivia().write_to(writer)?;
        writer.write_all(self.text().as_bytes())?;
        Ok(())
    }
}

/// Returns whether `t1` and `t2` require a separator to be distinguished
/// as two separate tokens when `t1` is directly followed by `t2`.
/// If this function returns `false`, the two tokens may be printed without separator.
///
/// This function is order dependent. For example, `? =` are two tokens whereas `?=` is one token.
/// This is different from the other way round: `= ?` are two tokens and so are `=?`
///
/// # Special cases
///
/// - `ToolDirective`s always return `true`. Note that its separator must be a newline.
/// - Identifiers, keywords and abstract literals followed by an identifier, keyword,
///   abstract literal or bit-string literal always return `true` (LRM §15.3).
pub fn requires_separator(t1: &Token, t2: &Token, standard: VHDLStandard) -> bool {
    use TokenKind::*;

    if t1.kind() == ToolDirective || t2.kind() == ToolDirective {
        return true;
    }
    // LRM §15.3: 'At least one separator is required between an identifier
    // or an abstract literal and an adjacent identifier or abstract literal'
    if matches!(t1.kind(), Identifier | Keyword(_) | AbstractLiteral)
        && matches!(
            t2.kind(),
            Identifier | Keyword(_) | AbstractLiteral | BitStringLiteral
        )
    {
        return true;
    }
    // Fast path for single-character tokens that never combine with adjacent characters.
    let is_isolated = |kind| {
        matches!(
            kind,
            LeftPar | RightPar | SemiColon | Comma | Concat | Bar | LeftSquare | RightSquare
        )
    };
    // `t1` cannot absorb `t2`. Ticks and character literals depend on the preceding token.
    if is_isolated(t1.kind()) && !matches!(t2.kind(), Tick | CharacterLiteral) {
        return false;
    }
    // Keywords and basic identifiers cannot absorb `t2`.
    // Extended identifiers are excluded as they are potentially unterminated.
    if is_isolated(t2.kind())
        && (matches!(t1.kind(), Keyword(_))
            || (t1.kind() == Identifier && t1.text().as_bytes().first() != Some(&b'\\')))
    {
        return false;
    }

    // Collecting into a `TokenStream` to merge bit-string literals
    let mut tokens: TokenStream =
        Tokenizer::with_standard(standard, t1.text().into_iter().chain(t2.text()).copied())
            .collect();
    let Some((first_tokenized, None)) = tokens.next() else {
        return true;
    };
    let Some((second_tokenized, None)) = tokens.next() else {
        return true;
    };
    !(t1.text() == first_tokenized.text()
        && t1.kind() == first_tokenized.kind()
        && t2.text() == second_tokenized.text()
        && t2.kind() == second_tokenized.kind())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tokens::Keyword as Kw;
    use TokenKind::*;

    fn requires(t1: (TokenKind, &str), t2: (TokenKind, &str)) -> bool {
        requires_separator(
            &Token::simple(t1.0, t1.1.as_bytes()),
            &Token::simple(t2.0, t2.1.as_bytes()),
            VHDLStandard::default(),
        )
    }

    #[test]
    fn operators() {
        assert!(requires((Que, "?"), (EQ, "=")));
        assert!(!requires((EQ, "="), (Que, "?")));
        assert!(!requires((Que, "?"), (Div, "/")));
        assert!(requires((Div, "/"), (EQ, "=")));
        assert!(requires((LT, "<"), (LT, "<")));
        assert!(!requires((Plus, "+"), (Minus, "-")));
    }

    #[test]
    fn comment_starts() {
        assert!(requires((Minus, "-"), (Minus, "-")));
        assert!(requires((Div, "/"), (Times, "*")));
    }

    #[test]
    fn identifiers_keywords_and_abstract_literals() {
        assert!(requires((Identifier, "a"), (Identifier, "b")));
        assert!(requires((Identifier, "a"), (AbstractLiteral, "1")));
        assert!(requires((AbstractLiteral, "10"), (Identifier, "ns")));
        assert!(requires((AbstractLiteral, "0"), (Keyword(Kw::To), "to")));
        assert!(requires((Keyword(Kw::To), "to"), (AbstractLiteral, "7")));
        assert!(requires((Identifier, "\\a\\"), (Identifier, "\\b\\")));
        assert!(!requires((Identifier, "a"), (LeftPar, "(")));
        assert!(!requires((Identifier, "\\a\\"), (LeftPar, "(")));
        assert!(!requires((RightPar, ")"), (Keyword(Kw::Is), "is")));
        assert!(!requires((AbstractLiteral, "1"), (SemiColon, ";")));
    }

    #[test]
    fn bit_string_literals() {
        assert!(requires(
            (BitStringLiteral, "b\"01\""),
            (StringLiteral, "\"x\"")
        ));
        assert!(!requires((BitStringLiteral, "x\"FF\""), (Comma, ",")));
        assert!(!requires((LeftPar, "("), (BitStringLiteral, "x\"FF\"")));
        assert!(!requires((EQ, "="), (BitStringLiteral, "x\"FF\"")));
        assert!(requires((Identifier, "a"), (BitStringLiteral, "x\"FF\"")));
        assert!(requires(
            (AbstractLiteral, "10"),
            (BitStringLiteral, "ub\"01\"")
        ));
        assert!(requires((Identifier, "b"), (StringLiteral, "\"01\"")));
        assert!(requires((Identifier, "ub"), (StringLiteral, "\"01\"")));
        assert!(!requires((Identifier, "foo"), (StringLiteral, "\"01\"")));
    }

    #[test]
    fn string_literals() {
        assert!(requires((StringLiteral, "\"a\""), (StringLiteral, "\"b\"")));
        assert!(!requires((StringLiteral, "\"a\""), (Concat, "&")));
    }

    #[test]
    fn tool_directives() {
        assert!(requires((Identifier, "a"), (ToolDirective, "`if")));
        assert!(requires((ToolDirective, "`end"), (Identifier, "a")));
        assert!(requires((ToolDirective, "`end"), (SemiColon, ";")));
    }

    #[test]
    fn character_literals() {
        assert!(!requires((Comma, ","), (CharacterLiteral, "'a'")));
        assert!(requires((RightPar, ")"), (CharacterLiteral, "'a'")));
    }
}
