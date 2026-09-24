// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2025, Lukas Scheller lukasscheller@icloud.com

use crate::latin_1::{Latin1Str, Latin1String};
use crate::standard::VHDLStandard;
use crate::tokens::comment::Comment;
use crate::tokens::TokenKind::*;
use crate::tokens::{Keyword as Kw, TriviaBuf, TriviaPiece};
use crate::tokens::{Token, TokenKind};
use std::slice::{self, SliceIndex};

/// describes the kind of a token was unterminated
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnterminatedKind {
    StringLiteral,
    BasedLiteral,
    ExtendedIdentifier,
    BlockComment,
}

impl std::fmt::Display for UnterminatedKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = match self {
            UnterminatedKind::StringLiteral => "string literal",
            UnterminatedKind::BasedLiteral => "based literal",
            UnterminatedKind::ExtendedIdentifier => "extended identifier",
            UnterminatedKind::BlockComment => "block comment",
        };
        f.write_str(name)
    }
}

/// Error kind that occurs when lexing
#[derive(Clone, Copy, Debug)]
pub enum LexErrKind {
    /// A token (string, comment, e.t.c.) was not terminated properly
    Unterminated(UnterminatedKind),
    IllegalInput,
}

/// Token errors are always attached to raw tokens.
/// Given that each token may include additional trivia, this enum
/// defines whether the error refers to the token itself, or leading trivia
/// of that token
#[derive(Copy, Clone, Debug)]
pub enum LexErrPos {
    /// Refers to the token
    Token,
    /// Refers to the trivia attached to the token at the given index
    Trivia(usize),
}

#[derive(Debug)]
pub struct LexErr {
    pub err: LexErrKind,
    pub pos: LexErrPos,
}

impl LexErr {
    pub fn token(err: LexErrKind) -> LexErr {
        LexErr {
            err,
            pos: LexErrPos::Token,
        }
    }

    pub fn trivia(index: usize, err: LexErrKind) -> LexErr {
        LexErr {
            err,
            pos: LexErrPos::Trivia(index),
        }
    }
}

pub trait Tokenize {
    fn tokenize(&self) -> impl Iterator<Item = (Token, Option<LexErr>)>;
}

impl Tokenize for &str {
    fn tokenize(&self) -> impl Iterator<Item = (Token, Option<LexErr>)> {
        Tokenizer::new(self.as_bytes().iter())
    }
}

impl Tokenize for String {
    fn tokenize(&self) -> impl Iterator<Item = (Token, Option<LexErr>)> {
        Tokenizer::new(self.as_bytes().iter())
    }
}

impl Tokenize for &Latin1Str {
    fn tokenize(&self) -> impl Iterator<Item = (Token, Option<LexErr>)> {
        Tokenizer::new(self.as_bytes().iter())
    }
}

impl Tokenize for Latin1String {
    fn tokenize(&self) -> impl Iterator<Item = (Token, Option<LexErr>)> {
        Tokenizer::new(self.as_bytes().iter())
    }
}

impl Tokenize for &[u8] {
    fn tokenize(&self) -> impl Iterator<Item = (Token, Option<LexErr>)> {
        Tokenizer::new(self.iter())
    }
}

impl Tokenize for Vec<u8> {
    fn tokenize(&self) -> impl Iterator<Item = (Token, Option<LexErr>)> {
        Tokenizer::new(self.iter())
    }
}

/// The Tokenizer is an iterator that consumes some char iterator (i.e., an iterator that
/// produces u8 items) and generates tokens and optionally an error associated to that token.
/// The tokenizer is constructed via a slice-iterator, e.g., from a `Vec`, or a `str` literal:
/// ```
/// use vhdl_syntax::tokens::{Keyword, TokenKind, Tokenizer};
/// let mut tokenizer = Tokenizer::new("entity foo".as_bytes().iter());
/// assert_eq!(tokenizer.next().unwrap().0.kind(), TokenKind::Keyword(Keyword::Entity));
/// assert_eq!(tokenizer.next().unwrap().0.kind(), TokenKind::Identifier);
/// assert_eq!(tokenizer.next().unwrap().0.kind(), TokenKind::Eof);
/// assert!(tokenizer.next().is_none());
/// ```
///
/// The [Tokenize] trait enables syntactic sugar to work with tokenizers using iterators:
/// ```
/// use vhdl_syntax::tokens::tokenizer::Tokenize;
/// use vhdl_syntax::tokens::TokenKind;
///
/// let tokens = "a ?> b".tokenize().map(|(tok, _)| tok.kind()).collect::<Vec<_>>();
/// assert_eq!(tokens, vec![TokenKind::Identifier, TokenKind::QueGT, TokenKind::Identifier, TokenKind::Eof])
/// ```
pub struct Tokenizer<'a> {
    /// The text, i.e., an iterator over chars.
    text: slice::Iter<'a, u8>,
    /// The last token kind observed, used to disambiguate ticks (i.e., whether ticks are
    /// used as attributes or character literals)
    last_token_kind: Option<TokenKind>,
    /// flag indicating whether the `EOF` token was already emitted.
    eof_emitted: bool,
    /// Under what standard to tokenize this.
    standard: VHDLStandard,
}

impl<'a> Tokenizer<'a> {
    /// Creates a new tokenizer from some iterator.
    pub fn new(text: slice::Iter<'a, u8>) -> Self {
        Self::with_standard(VHDLStandard::default(), text)
    }

    pub fn with_standard(standard: VHDLStandard, text: slice::Iter<'a, u8>) -> Self {
        Tokenizer {
            text,
            last_token_kind: None,
            eof_emitted: false,
            standard,
        }
    }

    pub fn current(&self) -> Option<u8> {
        self.text.as_slice().first().copied()
    }
}

#[derive(Copy, Clone)]
enum QuoteKind {
    QuotationMark,      // "
    ExtendedIdentifier, // \
}

impl QuoteKind {
    pub fn unterminated_kind(&self) -> UnterminatedKind {
        match self {
            QuoteKind::QuotationMark => UnterminatedKind::StringLiteral,
            QuoteKind::ExtendedIdentifier => UnterminatedKind::ExtendedIdentifier,
        }
    }

    pub fn token_kind(&self) -> TokenKind {
        match self {
            QuoteKind::QuotationMark => StringLiteral,
            QuoteKind::ExtendedIdentifier => Identifier,
        }
    }
}

fn split_while(slice: &[u8], predicate: impl Fn(&u8) -> bool) -> &[u8] {
    let Some(pos) = slice.iter().position(|ch| !predicate(ch)) else {
        return slice;
    };
    &slice[..pos]
}

fn integer(slice: &[u8]) -> &[u8] {
    split_while(slice, |ch| matches!(ch, b'0'..=b'9' | b'_'))
}

fn based_integer(slice: &[u8]) -> &[u8] {
    split_while(
        slice,
        |ch| matches!(ch, b'0'..=b'9' | b'a'..=b'z' | b'A'..=b'Z' | b'_'),
    )
}

fn opt_exponent(slice: &[u8]) -> &[u8] {
    let mut offset = 0usize;
    if matches!(slice.first(), Some(b'e' | b'E')) {
        offset += 1;
        if matches!(slice.get(1), Some(b'+' | b'-')) {
            offset += 1;
        }
        offset += integer(&slice[offset..]).len();
    }
    &slice[..offset]
}

impl<'a> Tokenizer<'a> {
    /// Peek the next character
    fn peek(&self) -> Option<u8> {
        self.peek_n(0)
    }

    fn peek_n(&self, n: usize) -> Option<u8> {
        self.text.as_slice().get(n).copied()
    }

    /// tokenize a tool directive
    fn tool_directive(&self) -> &'a [u8] {
        let slice = self.text.as_slice();

        let pos = slice
            .iter()
            .position(|ch| matches!(ch, b'\n' | b'\r'))
            .unwrap_or(slice.len());
        &slice[..pos]
    }

    fn get_while(&self, condition: impl Fn(&u8) -> bool) -> &'a [u8] {
        let slice = self.text.as_slice();
        let pos = slice
            .iter()
            .position(|ch| !condition(ch))
            .unwrap_or(slice.len());
        &slice[..pos]
    }

    /// Tokenize an identifier or keyword.
    fn identifier_or_keyword(&self) -> (TokenKind, &'a [u8]) {
        let slice =
            self.get_while(|ch| matches!(ch, b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_'));
        if let Some(kw) =
            Kw::from_latin1(Latin1Str::new(slice)).filter(|kw| kw.introduced_in() <= self.standard)
        {
            (Keyword(kw), slice)
        } else {
            (Identifier, slice)
        }
    }

    /// Tokenize an abstract literal or bit-string literal where the productions
    /// are given by the following:
    /// ```ebnf
    /// abstract_literal ::= decimal_literal | based_literal
    /// bit_string_literal ::= [ integer ] base_specifier " [ bit_value ] "
    ///
    /// decimal_literal ::= integer [ . integer ] [ exponent ]
    /// based_literal ::= integer # based_integer [ . based_integer ] # [ exponent ]
    ///
    /// integer ::= digit { [ underline ] digit }
    /// exponent ::= E [ + ] integer | E – integer
    /// based_integer ::= extended_digit { [ underline ] extended_digit }
    ///
    /// extended_digit ::= digit | letter
    /// bit_value ::= graphic_character { [ underline ] graphic_character }
    /// ```
    fn abstract_literal(&self) -> (TokenKind, &'a [u8], Option<LexErr>) {
        let mut diag = None;
        let slice = self.text.as_slice();
        let mut offset = integer(slice).len();
        match self.peek_n(offset) {
            Some(b'.') => {
                offset += 1;
                offset += integer(&slice[offset..]).len();
                offset += opt_exponent(&slice[offset..]).len()
            }
            Some(ch @ b'#' | ch @ b':') => {
                offset += 1;
                offset += based_integer(&slice[offset..]).len();
                if self.peek_n(offset) == Some(b'.') {
                    offset += 1;
                    offset += based_integer(&slice[offset..]).len()
                }
                if self.peek_n(offset) == Some(ch) {
                    offset += 1;
                } else {
                    diag = Some(LexErr::token(LexErrKind::Unterminated(
                        UnterminatedKind::BasedLiteral,
                    )));
                }
                offset += opt_exponent(&slice[offset..]).len();
            }
            Some(b'e' | b'E') => offset += opt_exponent(&slice[offset..]).len(),
            _ => {}
        }
        (AbstractLiteral, self.slice(..offset), diag)
    }

    /// Parse a quoted string.
    /// Returns [LexErr::Unterminated], if the quote string was not seen at the end.
    /// In VHDL, escaping the quotation mark is performed by repeating it.
    fn quoted(&self, quote_kind: QuoteKind) -> (&[u8], TokenKind, Option<LexErr>) {
        let quote = self.peek().expect("Input empty while tokenizing quoted");
        let mut itr = self.slice(1..).iter();
        let mut count = 1usize;
        while let Some(next) = itr.next() {
            if next == &quote {
                // Escaped
                if itr.as_slice().first() == Some(&quote) {
                    itr.next();
                    count += 2;
                    continue;
                } else {
                    count += 1;
                    return (self.slice(..count), quote_kind.token_kind(), None);
                }
            }
            count += 1;
        }
        (
            self.slice(..),
            quote_kind.token_kind(),
            Some(LexErr::token(LexErrKind::Unterminated(
                quote_kind.unterminated_kind(),
            ))),
        )
    }

    /// Consume a trivia piece (i.e., whitespace, newline, comments, ...)
    fn consume_trivia_piece(&mut self) -> Option<(TriviaPiece, Option<LexErrKind>)> {
        let count_chars = |this: &mut Self, ch| {
            let mut count = 0;
            while this.peek() == Some(ch) {
                this.text.next();
                count += 1;
            }
            count
        };
        Some(match self.peek()? {
            b'\t' => (TriviaPiece::HorizontalTabs(count_chars(self, b'\t')), None),
            /*vertical tab*/
            0x0B => (TriviaPiece::VerticalTabs(count_chars(self, 0x0B)), None),
            b'\r' => {
                if self.peek_n(1) == Some(b'\n') {
                    let mut count = 0;
                    while self.peek() == Some(b'\r') && self.peek_n(1) == Some(b'\n') {
                        self.text.nth(1);
                        count += 1;
                    }
                    (TriviaPiece::CarriageReturnLineFeeds(count), None)
                } else {
                    (TriviaPiece::CarriageReturns(count_chars(self, b'\r')), None)
                }
            }
            0x0C => (TriviaPiece::FormFeeds(count_chars(self, 0x0C)), None),
            b'\n' => (TriviaPiece::LineFeeds(count_chars(self, b'\n')), None),
            b' ' => (TriviaPiece::Spaces(count_chars(self, b' ')), None),
            b'-' if self.peek_n(1) == Some(b'-') => {
                let bytes = self.get_while(|ch| !matches!(ch, b'\r' | b'\n'));
                self.text.nth(bytes.len() - 1);
                (TriviaPiece::LineComment(Comment::from_raw(bytes)), None)
            }
            b'/' if self.peek_n(1) == Some(b'*') => {
                let mut bytes = vec![b'/', b'*'];
                self.text.nth(1);
                loop {
                    if self.peek() == Some(b'*') && self.peek_n(1) == Some(b'/') {
                        bytes.push(b'*');
                        bytes.push(b'/');
                        self.text.nth(1);
                        break;
                    }
                    let Some(ch) = self.text.next() else {
                        return Some((
                            TriviaPiece::BlockComment(Comment::from_raw(bytes)),
                            Some(LexErrKind::Unterminated(UnterminatedKind::BlockComment)),
                        ));
                    };
                    bytes.push(*ch)
                }
                (TriviaPiece::BlockComment(Comment::from_raw(bytes)), None)
            }
            /*non breaking spaces*/
            0xA0 => (
                TriviaPiece::NonBreakingSpaces(count_chars(self, 0xA0)),
                None,
            ),
            _ => return None,
        })
    }

    /// Consumes all trivia.
    fn consume_trivia(&mut self) -> (TriviaBuf, Option<LexErr>) {
        let mut trivia = TriviaBuf::default();
        // Note: we currently only allow one error. This is fine because an unterminated input will consume everything.
        // If we ever decide against this, the design must change.
        while let Some((piece, err)) = self.consume_trivia_piece() {
            trivia.push(piece);
            if let Some(err) = err {
                let idx = trivia.len() - 1;
                return (trivia, Some(LexErr::trivia(idx, err)));
            }
        }
        (trivia, None)
    }

    fn slice<I: SliceIndex<[u8], Output = [u8]>>(&self, range: I) -> &'a [u8] {
        &self.text.as_slice()[range]
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = (Token, Option<LexErr>);

    fn next(&mut self) -> Option<Self::Item> {
        let (trivia, trivia_diag) = self.consume_trivia();
        let mut token_diag = None;
        let Some(current) = self.peek() else {
            if self.eof_emitted {
                return None;
            }
            self.eof_emitted = true;
            return Some((Token::eof(trivia), trivia_diag));
        };
        let (kind, text) = match current {
            b'a'..=b'z' | b'A'..=b'Z' => self.identifier_or_keyword(),
            b'0'..=b'9' => {
                let (kind, text, diag) = self.abstract_literal();
                token_diag = diag;
                (kind, text)
            }
            b':' => {
                if self.peek_n(1) == Some(b'=') {
                    (ColonEq, self.slice(..2))
                } else {
                    (Colon, self.slice(..1))
                }
            }
            b'\'' => {
                if can_be_char(self.last_token_kind) {
                    if self.peek_n(2) == Some(b'\'') {
                        (CharacterLiteral, self.slice(..3))
                    } else {
                        (Tick, self.slice(..1))
                    }
                } else {
                    (Tick, self.slice(..1))
                }
            }
            b'-' => (Minus, self.slice(..1)),
            b'"' => {
                let (slice, kind, diag) = self.quoted(QuoteKind::QuotationMark);
                token_diag = diag;
                (kind, slice)
            }
            b';' => (SemiColon, self.slice(..1)),
            b'(' => (LeftPar, self.slice(..1)),
            b')' => (RightPar, self.slice(..1)),
            b'+' => (Plus, self.slice(..1)),
            b'.' => (Dot, self.slice(..1)),
            b'&' => (Concat, self.slice(..1)),
            b',' => (Comma, self.slice(..1)),
            b'=' => {
                if self.peek_n(1) == Some(b'>') {
                    (RightArrow, self.slice(..2))
                } else {
                    (EQ, self.slice(..1))
                }
            }
            b'<' => match self.peek_n(1) {
                Some(b'=') => (LTE, self.slice(..2)),
                Some(b'>') => (BOX, self.slice(..2)),
                Some(b'<') => (LtLt, self.slice(..2)),
                _ => (LT, self.slice(..1)),
            },
            b'>' => match self.peek_n(1) {
                Some(b'=') => (GTE, self.slice(..2)),
                Some(b'>') => (GtGt, self.slice(..2)),
                _ => (GT, self.slice(..1)),
            },
            b'/' => {
                if self.peek_n(1) == Some(b'=') {
                    (NE, self.slice(..2))
                } else {
                    (Div, self.slice(..1))
                }
            }
            b'*' => {
                if self.peek_n(1) == Some(b'*') {
                    (Pow, self.slice(..2))
                } else {
                    (Times, self.slice(..1))
                }
            }
            b'?' => match self.peek_n(1) {
                Some(b'?') => (QueQue, self.slice(..2)),
                Some(b'=') => (QueEQ, self.slice(..2)),
                Some(b'/') => {
                    if self.peek_n(2) == Some(b'=') {
                        (QueNE, self.slice(..3))
                    } else {
                        (Que, self.slice(..1))
                    }
                }
                Some(b'<') => {
                    if self.peek_n(2) == Some(b'=') {
                        (QueLTE, self.slice(..3))
                    } else {
                        (QueLT, self.slice(..2))
                    }
                }
                Some(b'>') => {
                    if self.peek_n(2) == Some(b'=') {
                        (QueGTE, self.slice(..3))
                    } else {
                        (QueGT, self.slice(..2))
                    }
                }
                _ => (Que, self.slice(..1)),
            },
            b'^' => (Circ, self.slice(..1)),
            b'@' => (CommAt, self.slice(..1)),
            b'|' => (Bar, self.slice(..1)),
            b'[' => (LeftSquare, self.slice(..1)),
            b']' => (RightSquare, self.slice(..1)),
            b'\\' => {
                let (text, kind, diag) = self.quoted(QuoteKind::ExtendedIdentifier);
                token_diag = diag;
                (kind, text)
            }
            b'`' => (ToolDirective, self.tool_directive()),
            _ => {
                token_diag = Some(LexErr::token(LexErrKind::IllegalInput));
                (Unknown, self.slice(..1))
            }
        };
        let diag = match (trivia_diag, token_diag) {
            (Some(triv_diag), None) => Some(triv_diag),
            (None, Some(token_diag)) => Some(token_diag),
            (None, None) => None,
            _ => {
                unreachable!("Trivia diagnostics and token diagnostics should never occur together")
            }
        };
        let tok = (Token::new(kind, text, trivia), diag);
        // Consume the parsed text
        let _ = self.text.nth(text.len() - 1);
        self.last_token_kind = Some(kind);
        Some(tok)
    }
}

/// Resolves ir1045
/// http://www.eda-stds.org/isac/IRs-VHDL-93/IR1045.txt
/// char may not come after ], ), all, or identifier
fn can_be_char(last_token_kind: Option<TokenKind>) -> bool {
    if let Some(kind) = last_token_kind {
        !matches!(kind, RightSquare | RightPar | Keyword(Kw::All) | Identifier)
    } else {
        true
    }
}

#[cfg(test)]
mod tests {

    use crate::tokens::comment::Comment;
    use crate::tokens::tokenizer::Tokenize;
    use crate::tokens::TokenKind;
    use crate::tokens::TokenKind::*;
    use crate::tokens::{Keyword as Kw, Token, TriviaBuf, TriviaPiece};
    use pretty_assertions::assert_eq;

    fn kinds_tokenize_remove_eof(code: &str) -> Vec<TokenKind> {
        let mut val = code
            .tokenize()
            .map(|(tok, _)| tok.kind())
            .collect::<Vec<_>>();
        assert_eq!(val.pop(), Some(TokenKind::Eof));
        val
    }

    trait TokenizeVec {
        fn tokenize_vec(&self) -> Vec<Token>;

        fn tokenize_kind_value(&self) -> Vec<(TokenKind, String)>;

        fn tokenize_kind_value_one(&self) -> (TokenKind, String) {
            self.tokenize_kind_value().first().unwrap().clone()
        }

        fn tokenize_kinds(&self) -> Vec<TokenKind>;

        fn tokenize_one(&self) -> Token {
            self.tokenize_vec().first().unwrap().clone()
        }
    }

    impl<T> TokenizeVec for T
    where
        T: Tokenize,
    {
        fn tokenize_vec(&self) -> Vec<Token> {
            self.tokenize().map(|(tok, _)| tok).collect()
        }

        fn tokenize_kind_value(&self) -> Vec<(TokenKind, String)> {
            self.tokenize()
                .map(|(tok, _)| (tok.kind(), tok.text().to_string()))
                .collect()
        }

        fn tokenize_kinds(&self) -> Vec<TokenKind> {
            self.tokenize().map(|(tok, _)| tok.kind()).collect()
        }
    }

    #[test]
    fn tokenize_empty_input() {
        assert_eq!("".tokenize_vec(), vec![Token::eof(TriviaBuf::default())]);
    }

    #[test]
    fn tokenize_input_only_trivia() {
        assert_eq!(
            "  ".tokenize_vec(),
            vec![Token::eof(TriviaBuf::from([TriviaPiece::Spaces(2)]))]
        );
    }

    #[test]
    fn tokenize_keywords() {
        assert_eq!(
            kinds_tokenize_remove_eof("architecture"),
            vec![Keyword(Kw::Architecture)]
        );
        assert_eq!(
            kinds_tokenize_remove_eof("entity"),
            vec![Keyword(Kw::Entity)]
        );
        assert_eq!(kinds_tokenize_remove_eof("is"), vec![Keyword(Kw::Is)]);
        assert_eq!(
            kinds_tokenize_remove_eof("generic"),
            vec![Keyword(Kw::Generic)]
        );
        assert_eq!(kinds_tokenize_remove_eof("port"), vec![Keyword(Kw::Port)]);
        assert_eq!(kinds_tokenize_remove_eof("begin"), vec![Keyword(Kw::Begin)]);
        assert_eq!(kinds_tokenize_remove_eof("end"), vec![Keyword(Kw::End)]);
        assert_eq!(kinds_tokenize_remove_eof("all"), vec![Keyword(Kw::All)]);
        assert_eq!(kinds_tokenize_remove_eof("abs"), vec![Keyword(Kw::Abs)]);
        assert_eq!(kinds_tokenize_remove_eof("not"), vec![Keyword(Kw::Not)]);
    }

    #[test]
    fn tokenize_newline() {
        assert_eq!(
            "
entity is
end entity"
                .tokenize_kinds(),
            vec![
                Keyword(Kw::Entity),
                Keyword(Kw::Is),
                Keyword(Kw::End),
                Keyword(Kw::Entity),
                Eof
            ]
        );
    }

    #[test]
    fn tokenize_trivia() {
        assert_eq!(
            "

entity foo"
                .tokenize_vec(),
            vec![
                Token::new(
                    Keyword(Kw::Entity),
                    b"entity",
                    TriviaBuf::from([TriviaPiece::LineFeeds(2)]),
                ),
                Token::new(
                    Identifier,
                    b"foo",
                    TriviaBuf::from([TriviaPiece::Spaces(1)])
                ),
                Token::eof(TriviaBuf::default())
            ]
        );
    }

    #[test]
    fn tokenize_keywords_case_insensitive() {
        assert_eq!(
            kinds_tokenize_remove_eof("entity"),
            vec![Keyword(Kw::Entity)]
        );
        assert_eq!(
            kinds_tokenize_remove_eof("Entity"),
            vec![Keyword(Kw::Entity)]
        );
        assert_eq!(
            kinds_tokenize_remove_eof("arCHitecture"),
            vec![Keyword(Kw::Architecture)]
        );
    }

    #[test]
    fn tokenize_identifier() {
        assert_eq!(
            "my_ident".tokenize_one(),
            Token::simple(Identifier, b"my_ident")
        );
    }

    #[test]
    fn tokenize_extended_identifier() {
        assert_eq!(
            "\\1$my_ident\\".tokenize_one(),
            Token::simple(Identifier, b"\\1$my_ident\\")
        );
        assert_eq!(
            "\\my\\\\_ident\\".tokenize_one(),
            Token::simple(Identifier, b"\\my\\\\_ident\\")
        );
    }

    #[test]
    fn tokenize_many_identifiers() {
        assert_eq!(
            "my_ident

my_other_ident"
                .tokenize_vec(),
            vec![
                Token::new(Identifier, b"my_ident", TriviaBuf::default(),),
                Token::new(
                    Identifier,
                    b"my_other_ident",
                    TriviaBuf::from([TriviaPiece::LineFeeds(2)])
                ),
                Token::eof(TriviaBuf::default())
            ]
        );
    }

    #[test]
    fn tokenize_integer() {
        assert_eq!(
            "100 -123 1_6_2 1e3 2E4 1e-1".tokenize_kind_value(),
            vec![
                (AbstractLiteral, "100".to_string()),
                (Minus, "-".to_string()),
                (AbstractLiteral, "123".to_string()),
                (AbstractLiteral, "1_6_2".to_string()),
                (AbstractLiteral, "1e3".to_string()),
                (AbstractLiteral, "2E4".to_string()),
                (AbstractLiteral, "1e-1".to_string()),
                (Eof, "".to_string())
            ]
        );
    }

    #[test]
    fn tokenize_real() {
        assert_eq!(
            "0.1 -2_2.3_3 2.0e3 3.33E2 2.1e-2 4.4e+1 2.5E+3"
                .tokenize()
                .map(|(tok, _)| (tok.kind(), tok.text().to_string()))
                .collect::<Vec<_>>(),
            vec![
                (AbstractLiteral, "0.1".to_string()),
                (Minus, "-".to_string()),
                (AbstractLiteral, "2_2.3_3".to_string()),
                (AbstractLiteral, "2.0e3".to_string()),
                (AbstractLiteral, "3.33E2".to_string()),
                (AbstractLiteral, "2.1e-2".to_string()),
                (AbstractLiteral, "4.4e+1".to_string()),
                (AbstractLiteral, "2.5E+3".to_string()),
                (Eof, "".to_string())
            ]
        );
    }

    #[test]
    fn tokenize_real_many_fractional_digits() {
        assert_eq!(
            "0.1000_0000_0000_0000_0000_0000_0000_0000".tokenize_kind_value_one(),
            (
                AbstractLiteral,
                "0.1000_0000_0000_0000_0000_0000_0000_0000".to_string()
            )
        );
    }

    #[test]
    fn tokenize_real_many_integer_digits() {
        assert_eq!(
            "1000_0000_0000_0000_0000_0000_0000_0000.0".tokenize_kind_value_one(),
            (
                AbstractLiteral,
                "1000_0000_0000_0000_0000_0000_0000_0000.0".to_string()
            )
        );
    }

    #[test]
    fn tokenize_string_literal() {
        assert_eq!(
            "\"string\"".tokenize_one(),
            Token::simple(StringLiteral, b"\"string\"")
        );
    }

    #[test]
    fn tokenize_string_literal_quote() {
        assert_eq!(
            "\"str\"\"ing\"".tokenize_one(),
            Token::simple(StringLiteral, b"\"str\"\"ing\"")
        );
    }

    #[test]
    fn tokenize_string_literal_quote_separated() {
        assert_eq!(
            "\"str\" \"ing\"".tokenize_vec(),
            vec![
                Token::new(StringLiteral, b"\"str\"", TriviaBuf::default(),),
                Token::new(
                    StringLiteral,
                    b"\"ing\"",
                    TriviaBuf::from([TriviaPiece::Spaces(1)]),
                ),
                Token::eof(TriviaBuf::default())
            ]
        );
    }

    #[test]
    fn tokenize_string_literal_multiline() {
        assert_eq!(
            "\"str\ning\"".tokenize_one(),
            Token::simple(StringLiteral, b"\"str\ning\"")
        );
    }

    #[test]
    fn tokenize_string_literal_error_on_early_eof() {
        assert_eq!(
            "\"string".tokenize_one(),
            Token::simple(StringLiteral, b"\"string")
        );
    }

    #[test]
    fn tokenize_base_specifier_then_string() {
        // The tokenizer emits these as separate tokens; merging into
        // BitStringLiteral happens in TokenStream.
        assert_eq!(
            "b\"0101\"".tokenize_kinds(),
            vec![Identifier, StringLiteral, Eof]
        );
        assert_eq!(
            "sx\"FF\"".tokenize_kinds(),
            vec![Identifier, StringLiteral, Eof]
        );
        assert_eq!(
            "10ub\"0101\"".tokenize_kinds(),
            vec![AbstractLiteral, Identifier, StringLiteral, Eof]
        );
    }

    #[test]
    fn tokenize_number_followed_by_identifier() {
        // Previously these were swallowed as Unknown; now the tokenizer emits
        // separate tokens, allowing the parser/user to interpret `10s` etc.
        assert_eq!(
            "10x".tokenize_kinds(),
            vec![AbstractLiteral, Identifier, Eof]
        );
        assert_eq!(
            "10ux".tokenize_kinds(),
            vec![AbstractLiteral, Identifier, Eof]
        );
        assert_eq!(
            "10s".tokenize_kinds(),
            vec![AbstractLiteral, Identifier, Eof]
        );
    }

    #[test]
    fn tokenize_based_integer() {
        assert_eq!(
            "2#101#".tokenize_kind_value_one(),
            (AbstractLiteral, "2#101#".to_string())
        );
        assert_eq!(
            "8#321#".tokenize_kind_value_one(),
            (AbstractLiteral, "8#321#".to_string())
        );
        assert_eq!(
            "16#eEFfa#".tokenize_kind_value_one(),
            (AbstractLiteral, "16#eEFfa#".to_string())
        );
        // This is illegal, but the checking happens at a later stage
        assert_eq!(
            "3#3#".tokenize_kind_value_one(),
            (AbstractLiteral, "3#3#".to_string())
        );
    }

    #[test]
    fn tokenize_based_real() {
        // The fractional part may contain extended digits (letters), not only 0-9
        assert_eq!(
            "16#ff.ff#E1".tokenize_kind_value_one(),
            (AbstractLiteral, "16#ff.ff#E1".to_string())
        );
        assert_eq!(
            "2#1.1#".tokenize_kind_value_one(),
            (AbstractLiteral, "2#1.1#".to_string())
        );
        assert_eq!(
            "16#F.8#e-1".tokenize_kind_value_one(),
            (AbstractLiteral, "16#F.8#e-1".to_string())
        );
    }

    #[test]
    fn tokenize_que_div() {
        assert_eq!(
            "?/".tokenize_kinds(),
            [TokenKind::Que, TokenKind::Div, TokenKind::Eof]
        )
    }

    macro_rules! check_tokenize {
        ($tokens:literal, $kind:expr) => {
            assert_eq!(
                $tokens.tokenize_kind_value_one(),
                ($kind, $tokens.to_string())
            )
        };
    }

    #[test]
    fn tokenize_char_literal() {
        check_tokenize!("'c'", CharacterLiteral);
    }

    #[test]
    fn tokenize_tick() {
        check_tokenize!("'", Tick);
    }

    #[test]
    fn tokenize_plus() {
        check_tokenize!("+", Plus);
    }

    #[test]
    fn tokenize_minus() {
        check_tokenize!("-", Minus);
    }

    #[test]
    fn tokenize_semi_colon() {
        check_tokenize!(";", SemiColon);
    }

    #[test]
    fn tokenize_colon() {
        check_tokenize!(":", Colon);
    }

    #[test]
    fn tokenize_bar() {
        check_tokenize!("|", Bar);
    }

    #[test]
    fn tokenize_dot() {
        check_tokenize!(".", Dot);
    }

    #[test]
    fn tokenize_concat() {
        check_tokenize!("&", Concat);
    }

    #[test]
    fn tokenize_eq() {
        check_tokenize!("=", EQ);
    }

    #[test]
    fn tokenize_colon_eq() {
        check_tokenize!(":=", ColonEq);
    }

    #[test]
    fn tokenize_right_arrow() {
        check_tokenize!("=>", RightArrow);
    }

    #[test]
    fn tokenize_cmp() {
        check_tokenize!("<", LT);
        check_tokenize!("<=", LTE);
        check_tokenize!(">", GT);
        check_tokenize!(">=", GTE);
    }

    #[test]
    fn tokenize_box() {
        check_tokenize!("<>", BOX);
    }

    #[test]
    fn tokenize_external_name() {
        check_tokenize!("<<", LtLt);
        check_tokenize!(">>", GtGt);
    }

    #[test]
    fn tokenize_questionmark_cmp() {
        check_tokenize!("?", Que);
        check_tokenize!("?<", QueLT);
        check_tokenize!("?<=", QueLTE);
        check_tokenize!("?=", QueEQ);
        check_tokenize!("?>", QueGT);
        check_tokenize!("?>=", QueGTE);
        check_tokenize!("??", QueQue);
    }

    #[test]
    fn tokenize_ne() {
        check_tokenize!("/=", NE);
    }

    #[test]
    fn tokenize_times() {
        check_tokenize!("*", Times);
    }

    #[test]
    fn tokenize_pow() {
        check_tokenize!("**", Pow);
    }

    #[test]
    fn tokenize_div() {
        check_tokenize!("/", Div);
    }

    #[test]
    fn tokenize_comma() {
        check_tokenize!(",", Comma);
    }

    #[test]
    fn tokenize_pars() {
        check_tokenize!("(", LeftPar);
        check_tokenize!(")", RightPar);
    }

    #[test]
    fn tokenize_squares() {
        check_tokenize!("[", LeftSquare);
        check_tokenize!("]", RightSquare);
    }

    #[test]
    fn tokenize_comments() {
        assert_eq!(
            "
1
--comment
-2
"
            .tokenize_vec(),
            vec![
                Token::new(
                    AbstractLiteral,
                    b"1",
                    TriviaBuf::from([TriviaPiece::LineFeeds(1)]),
                ),
                Token::new(
                    Minus,
                    b"-",
                    TriviaBuf::from([
                        TriviaPiece::LineFeeds(1),
                        TriviaPiece::LineComment(Comment::from_raw(b"--comment")),
                        TriviaPiece::LineFeeds(1)
                    ])
                ),
                Token::new(AbstractLiteral, b"2", TriviaBuf::default(),),
                Token::eof(TriviaBuf::from([TriviaPiece::LineFeeds(1)]))
            ]
        )
    }

    #[test]
    fn tokenize_multi_line_comments() {
        assert_eq!(
            "
1

/*
comment
*/

-2 /*
comment
*/

"
            .tokenize_vec(),
            vec![
                Token::new(
                    AbstractLiteral,
                    b"1",
                    TriviaBuf::from([TriviaPiece::LineFeeds(1)]),
                ),
                Token::new(
                    Minus,
                    b"-",
                    TriviaBuf::from([
                        TriviaPiece::LineFeeds(2),
                        TriviaPiece::BlockComment(Comment::from_raw(b"/*\ncomment\n*/")),
                        TriviaPiece::LineFeeds(2),
                    ])
                ),
                Token::new(AbstractLiteral, b"2", TriviaBuf::default(),),
                Token::eof(TriviaBuf::from([
                    TriviaPiece::Spaces(1),
                    TriviaPiece::BlockComment(Comment::from_raw("/*\ncomment\n*/")),
                    TriviaPiece::LineFeeds(2),
                ]),)
            ]
        )
    }

    #[test]
    fn unterminated_block_comment_roundtrips() {
        for input in ["1 /* unterminated", "1 /* unterminated *", "1 /*"] {
            let comment = &input[2..];
            assert_eq!(
                input.tokenize_vec(),
                vec![
                    Token::new(AbstractLiteral, b"1", TriviaBuf::default()),
                    Token::eof(TriviaBuf::from([
                        TriviaPiece::Spaces(1),
                        TriviaPiece::BlockComment(Comment::from_raw(comment)),
                    ])),
                ],
                "input: {input:?}"
            );

            let mut buf = Vec::new();
            for token in input.tokenize_vec() {
                token.write_to(&mut buf).unwrap();
            }
            assert_eq!(buf, input.as_bytes(), "input: {input:?}");

            let byte_len: usize = input
                .tokenize_vec()
                .iter()
                .map(|token| token.byte_len())
                .sum();
            assert_eq!(byte_len, input.len(), "input: {input:?}");
        }
    }

    #[test]
    fn tokenize_ir1045() {
        // http://www.eda-stds.org/isac/IRs-VHDL-93/IR1045.txt
        assert_eq!(
            "string'('a')".tokenize_kinds(),
            vec![Identifier, Tick, LeftPar, CharacterLiteral, RightPar, Eof]
        );
    }

    #[test]
    fn tokenize_illegal() {
        assert_eq!(
            "begin!end".tokenize_kinds(),
            vec![Keyword(Kw::Begin), Unknown, Keyword(Kw::End), Eof]
        );
    }

    // ---- standard-aware keyword tests ----

    fn tokenize_first_kind_with_standard(
        standard: crate::standard::VHDLStandard,
        input: &str,
    ) -> TokenKind {
        use super::Tokenizer;
        Tokenizer::with_standard(standard, input.as_bytes().iter())
            .next()
            .unwrap()
            .0
            .kind()
    }

    #[test]
    fn xnor_is_identifier_before_vhdl1993() {
        use crate::standard::VHDLStandard::*;
        assert_eq!(
            tokenize_first_kind_with_standard(VHDL1987, "xnor"),
            Identifier
        );
        assert_eq!(
            tokenize_first_kind_with_standard(VHDL1993, "xnor"),
            Keyword(Kw::Xnor)
        );
        assert_eq!(
            tokenize_first_kind_with_standard(VHDL2008, "xnor"),
            Keyword(Kw::Xnor)
        );
    }

    #[test]
    fn protected_is_identifier_before_vhdl2000() {
        use crate::standard::VHDLStandard::*;
        assert_eq!(
            tokenize_first_kind_with_standard(VHDL1993, "protected"),
            Identifier
        );
        assert_eq!(
            tokenize_first_kind_with_standard(VHDL2000, "protected"),
            Keyword(Kw::Protected)
        );
        assert_eq!(
            tokenize_first_kind_with_standard(VHDL2008, "protected"),
            Keyword(Kw::Protected)
        );
    }

    #[test]
    fn context_is_identifier_before_vhdl2008() {
        use crate::standard::VHDLStandard::*;
        assert_eq!(
            tokenize_first_kind_with_standard(VHDL2002, "context"),
            Identifier
        );
        assert_eq!(
            tokenize_first_kind_with_standard(VHDL2008, "context"),
            Keyword(Kw::Context)
        );
        assert_eq!(
            tokenize_first_kind_with_standard(VHDL2019, "context"),
            Keyword(Kw::Context)
        );
    }

    #[test]
    fn view_is_identifier_before_vhdl2019() {
        use crate::standard::VHDLStandard::*;
        assert_eq!(
            tokenize_first_kind_with_standard(VHDL2008, "view"),
            Identifier
        );
        assert_eq!(
            tokenize_first_kind_with_standard(VHDL2019, "view"),
            Keyword(Kw::View)
        );
    }
}
