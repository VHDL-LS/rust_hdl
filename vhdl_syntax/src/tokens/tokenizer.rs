// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2025, Lukas Scheller lukasscheller@icloud.com

use crate::latin_1::{Latin1Str, Latin1String};
use crate::tokens::trivia_piece::Comment;
use crate::tokens::TokenKind::*;
use crate::tokens::{Keyword as Kw, Trivia, TriviaPiece};
use crate::tokens::{Token, TokenKind};
use std::iter::Peekable;
use std::mem::replace;

pub trait Tokenize {
    fn tokenize(&self) -> impl Iterator<Item = Token>;
}

impl Tokenize for &str {
    fn tokenize(&self) -> impl Iterator<Item = Token> {
        Tokenizer::from(self.bytes())
    }
}

impl Tokenize for String {
    fn tokenize(&self) -> impl Iterator<Item = Token> {
        Tokenizer::from(self.bytes())
    }
}

impl Tokenize for &Latin1Str {
    fn tokenize(&self) -> impl Iterator<Item = Token> {
        Tokenizer::from(self.as_bytes().iter().copied())
    }
}

impl Tokenize for Latin1String {
    fn tokenize(&self) -> impl Iterator<Item = Token> {
        Tokenizer::from(self.as_bytes().iter().copied())
    }
}

impl Tokenize for &[u8] {
    fn tokenize(&self) -> impl Iterator<Item = Token> {
        Tokenizer::from(self.iter().copied())
    }
}

impl Tokenize for Vec<u8> {
    fn tokenize(&self) -> impl Iterator<Item = Token> {
        Tokenizer::from(self.iter().copied())
    }
}

/// The Tokenizer is an iterator that consumes some char iterator (i.e., an iterator that
/// produces u8 items) and generates tokens.
/// All iterators that implement `IntoIter<Item = u8>` can be used for this purpose. For example.
/// ```
/// use vhdl_syntax::tokens::{Keyword, TokenKind, Tokenizer};
/// // &str implements IntoIter<u8>, therefore it can be converted to a tokenizer
/// let mut tokenizer = Tokenizer::from("entity foo".bytes());
/// assert_eq!(tokenizer.next().unwrap().kind(), TokenKind::Keyword(Keyword::Entity));
/// assert_eq!(tokenizer.next().unwrap().kind(), TokenKind::Identifier);
/// assert_eq!(tokenizer.next().unwrap().kind(), TokenKind::Eof);
/// assert_eq!(tokenizer.next(), None);
/// ```
///
/// The [Tokenize] trait enables syntactic sugar to work with tokenizers using iterators:
/// ```
/// use vhdl_syntax::tokens::tokenizer::Tokenize;
/// use vhdl_syntax::tokens::TokenKind;
///
/// let tokens = "a ?> b".tokenize().map(|tok| tok.kind()).collect::<Vec<_>>();
/// assert_eq!(tokens, vec![TokenKind::Identifier, TokenKind::QueGT, TokenKind::Identifier, TokenKind::Eof])
/// ```
pub struct Tokenizer<I: Iterator<Item = u8>> {
    /// The text, i.e., an iterator over chars.
    text: Peekable<I>,
    /// The current char that is being investigated.
    /// The method [Tokenizer::skip] should be used to update this char.
    current: Option<u8>,
    /// The last token kind observed, used to disambiguate ticks (i.e., whether ticks are
    /// used as attributes or character literals)
    last_token_kind: Option<TokenKind>,
    /// flag indicating whether the `EOF` token was already emitted.
    eof_emitted : bool,
}

impl<I: Iterator<Item = u8>> Tokenizer<I> {
    /// Creates a new tokenizer from some iterator.
    pub fn new(mut text: I) -> Self {
        let current = text.next();
        Tokenizer {
            text: text.peekable(),
            current,
            last_token_kind: None,
            eof_emitted: false,
        }
    }
}

impl<T> From<T> for Tokenizer<T::IntoIter>
where
    T: IntoIterator<Item = u8>,
{
    fn from(value: T) -> Self {
        Tokenizer::new(value.into_iter())
    }
}

impl<T: Iterator<Item = u8>> Tokenizer<T> {
    /// Peek the next character
    fn peek(&mut self) -> Option<u8> {
        self.text.peek().copied()
    }

    /// Place the next character in the current one, returning the current
    fn skip(&mut self) -> Option<u8> {
        replace(&mut self.current, self.text.next())
    }

    /// Skips the current character, if the given precondition is met.
    fn skip_if(&mut self, cond: impl Fn(u8) -> bool) -> Option<u8> {
        if self.current.is_some_and(cond) {
            self.skip()
        } else {
            None
        }
    }

    /// Skips the current character if it is equal to `value`
    fn skip_if_eq(&mut self, value: u8) -> bool {
        if self.current.is_some_and(|peeked| peeked == value) {
            self.skip();
            true
        } else {
            false
        }
    }

    /// Fill the buffer with subsequent characters while a precondition holds.
    fn fill_buffer_while(&mut self, buf: &mut Latin1String, cond: impl Fn(u8) -> bool) {
        while let Some(ch) = self.skip_if(&cond) {
            buf.push(ch);
        }
    }

    /// tokenize a tool directive
    fn tool_directive(&mut self, buf: &mut Latin1String) {
        self.fill_buffer_while(buf, |ch| !matches!(ch, b'\n' | b'\r'))
    }

    /// Tokenize an identifier, keyword or Bit String Literal.
    fn identifier_keyword_or_bistring_literal(
        &mut self,
        buf: &mut Latin1String,
    ) -> Option<TokenKind> {
        self.fill_buffer_while(
            buf,
            |ch| matches!(ch, b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_'),
        );
        if self.current == Some(b'"') {
            if self.quoted(buf) == Unterminated {
                Some(Unterminated)
            } else {
                Some(BitStringLiteral)
            }
        } else {
            None
        }
    }

    /// Tokenize a simple integer
    fn integer(&mut self, buf: &mut Latin1String) {
        self.fill_buffer_while(buf, |ch| matches!(ch, b'0'..=b'9' | b'_'))
    }

    /// Tokenize a based integer. As opposed to a simple integer,
    /// this can contain letters and digits
    fn based_integer(&mut self, buf: &mut Latin1String) {
        self.fill_buffer_while(
            buf,
            |ch| matches!(ch, b'0'..=b'9' | b'a'..=b'z' | b'A'..=b'Z' | b'_'),
        )
    }

    /// Tokenize an optional exponent.
    fn opt_exponent(&mut self, buf: &mut Latin1String) {
        if let Some(ch) = self.skip_if(|ch| matches!(ch, b'e' | b'E')) {
            buf.push(ch);
            if let Some(ch) = self.skip_if(|ch| matches!(ch, b'+' | b'-')) {
                buf.push(ch)
            }
            self.integer(buf)
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
    fn abstract_literal(&mut self, buf: &mut Latin1String) -> TokenKind {
        self.integer(buf);
        match self.current {
            Some(b'.') => {
                self.skip();
                buf.push(b'.');
                self.integer(buf);
                self.opt_exponent(buf);
            }
            Some(ch @ b'#' | ch @ b':') => {
                self.skip();
                buf.push(ch);
                self.based_integer(buf);
                if self.skip_if_eq(b'.') {
                    buf.push(b'.');
                    self.integer(buf);
                }
                if self.skip_if_eq(ch) {
                    buf.push(ch);
                } else {
                    return Unterminated;
                }
                self.opt_exponent(buf);
            }
            Some(b'e' | b'E') => self.opt_exponent(buf),
            _ => {
                return self.opt_bit_string_literal(buf).unwrap_or(AbstractLiteral);
            }
        }
        AbstractLiteral
    }

    fn opt_bit_string_literal(&mut self, buf: &mut Latin1String) -> Option<TokenKind> {
        match self.current?.to_ascii_lowercase() {
            b's' | b'u' | b'b' | b'o' | b'x' | b'd' => {
                match self.identifier_keyword_or_bistring_literal(buf) {
                    Some(kind @ Unterminated | kind @ BitStringLiteral) => Some(kind),
                    _ => Some(Unknown),
                }
            }
            _ => None,
        }
    }

    /// Parse a quoted string.
    /// Returns [Unterminated], if the quote string was not seen at the end.
    /// In VHDL, escaping the quotation mark is performed by repeating it.
    fn quoted(&mut self, buf: &mut Latin1String) -> TokenKind {
        let quote = self.skip().expect("Input empty while tokenizing quoted");
        buf.push(quote);
        let mut found_end = false;
        while let Some(chr) = self.skip() {
            buf.push(chr);
            if chr == quote {
                if self.current == Some(quote) {
                    buf.push(chr);
                    self.skip();
                } else {
                    found_end = true;
                    break;
                }
            }
        }
        if !found_end {
            Unterminated
        } else if quote == b'"' {
            StringLiteral
        } else if quote == b'\\' {
            Identifier
        } else {
            Unknown
        }
    }

    /// Consume a trivia piece (i.e., whitespace, newline, comments, ...)
    // TODO: Currently, comment encoding is UTF-8 (and will panic if this is not the case)
    // Instead, the comment encoding should be user-configurable to avoid
    // panicking and support different use-cases.
    // VHDL allows comments to have a different encoding (NOTE 2 in 15.9) and we should respect that.
    fn consume_trivia_piece(&mut self) -> Option<TriviaPiece> {
        macro_rules! count_chars {
            ($ch:literal) => {{
                let mut count = 0;
                while self.skip_if_eq($ch) {
                    count += 1;
                }
                count
            }};
        }
        Some(match self.current? {
            b'\t' => TriviaPiece::HorizontalTabs(count_chars!(b'\t')),
            /*vertical tab*/
            0x0B => TriviaPiece::VerticalTabs(count_chars!(0x0B)),
            b'\r' => {
                if self.peek() == Some(b'\n') {
                    let mut count = 0;
                    while self.current == Some(b'\r') && self.peek() == Some(b'\n') {
                        self.skip();
                        self.skip();
                        count += 1;
                    }
                    TriviaPiece::CarriageReturnLineFeeds(count)
                } else {
                    TriviaPiece::CarriageReturns(count_chars!(b'\r'))
                }
            }
            0x0C => TriviaPiece::FormFeeds(count_chars!(0x0C)),
            b'\n' => TriviaPiece::LineFeeds(count_chars!(b'\n')),
            b' ' => TriviaPiece::Spaces(count_chars!(b' ')),
            b'-' => {
                if self.peek() == Some(b'-') {
                    self.skip();
                    self.skip();
                    let mut bytes = Vec::default();
                    while let Some(ch) = self.skip_if(|ch| !matches!(ch, b'\r' | b'\n')) {
                        bytes.push(ch);
                    }
                    TriviaPiece::LineComment(Comment::new(bytes))
                } else {
                    return None;
                }
            }
            b'/' => {
                if self.peek() == Some(b'*') {
                    self.skip();
                    self.skip();
                    let mut bytes = Vec::default();
                    loop {
                        if self.current == Some(b'*') && self.peek() == Some(b'/') {
                            self.skip();
                            self.skip();
                            break;
                        }
                        let Some(ch) = self.skip() else { break };
                        bytes.push(ch)
                    }
                    TriviaPiece::BlockComment(Comment::new(bytes))
                } else {
                    return None;
                }
            }
            /*non breaking spaces*/
            0xA0 => TriviaPiece::NonBreakingSpaces(count_chars!(0xA0)),
            _ => return None,
        })
    }

    /// Consumes all trivia.
    fn consume_trivia(&mut self) -> Trivia {
        let mut trivia = Trivia::default();
        while let Some(piece) = self.consume_trivia_piece() {
            trivia.push(piece);
        }
        trivia
    }
}

impl<T: Iterator<Item = u8>> Iterator for Tokenizer<T> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let leading_trivia = self.consume_trivia();
        let Some(current) = self.current else {
            if self.eof_emitted {
                return None
            }
            self.eof_emitted = true;
            return Some(Token::eof(leading_trivia));
        };
        let (kind, text) = match current {
            b'a'..=b'z' | b'A'..=b'Z' => {
                let mut ident_str = Latin1String::new();
                let kind = self.identifier_keyword_or_bistring_literal(&mut ident_str);
                if let Some(kind) = kind {
                    (kind, ident_str)
                } else if let Some(kw) = str_to_keyword(&ident_str) {
                    (Keyword(kw), ident_str)
                } else {
                    (Identifier, ident_str)
                }
            }
            b'0'..=b'9' => {
                let mut buf = Latin1String::new();
                let kind = self.abstract_literal(&mut buf);
                (kind, buf)
            }
            b':' => {
                self.skip();
                if self.current == Some(b'=') {
                    self.skip();
                    (ColonEq, Latin1String::from(b":="))
                } else {
                    (Colon, Latin1String::from(":"))
                }
            }
            b'\'' => {
                self.skip();
                if can_be_char(self.last_token_kind) {
                    if self.peek() == Some(b'\'') {
                        let mut ret = Latin1String::new();
                        ret.push(b'\'');
                        ret.push(self.skip().unwrap());
                        ret.push(b'\'');
                        let ret = (CharacterLiteral, ret);
                        self.skip();
                        ret
                    } else {
                        (Tick, Latin1String::from(b"'"))
                    }
                } else {
                    (Tick, Latin1String::from(b"'"))
                }
            }
            b'-' => {
                self.skip();
                (Minus, Latin1String::from(b"-"))
            }
            b'"' => {
                let mut buf = Latin1String::new();
                let kind = self.quoted(&mut buf);
                (kind, buf)
            }
            b';' => {
                self.skip();
                (SemiColon, Latin1String::from(b";"))
            }
            b'(' => {
                self.skip();
                (LeftPar, Latin1String::from(b"("))
            }
            b')' => {
                self.skip();
                (RightPar, Latin1String::from(b")"))
            }
            b'+' => {
                self.skip();
                (Plus, Latin1String::from(b"+"))
            }
            b'.' => {
                self.skip();
                (Dot, Latin1String::from(b"."))
            }
            b'&' => {
                self.skip();
                (Concat, Latin1String::from(b"&"))
            }
            b',' => {
                self.skip();
                (Comma, Latin1String::from(b","))
            }
            b'=' => {
                self.skip();
                if self.current == Some(b'>') {
                    self.skip();
                    (RightArrow, Latin1String::from(b"=>"))
                } else {
                    (EQ, Latin1String::from(b"="))
                }
            }
            b'<' => {
                self.skip();
                match self.current {
                    Some(b'=') => {
                        self.skip();
                        (LTE, Latin1String::from(b"<="))
                    }
                    Some(b'>') => {
                        self.skip();
                        (BOX, Latin1String::from(b"<>"))
                    }
                    Some(b'<') => {
                        self.skip();
                        (LtLt, Latin1String::from(b"<<"))
                    }
                    _ => (LT, Latin1String::from(b"<")),
                }
            }
            b'>' => {
                self.skip();
                match self.current {
                    Some(b'=') => {
                        self.skip();
                        (GTE, Latin1String::from(b">="))
                    }
                    Some(b'>') => {
                        self.skip();
                        (GtGt, Latin1String::from(b">>"))
                    }
                    _ => (GT, Latin1String::from(b">")),
                }
            }
            b'/' => {
                self.skip();
                if self.current == Some(b'=') {
                    self.skip();
                    (NE, Latin1String::from(b"/="))
                } else {
                    (Div, Latin1String::from(b"/"))
                }
            }
            b'*' => {
                self.skip();
                if self.current == Some(b'*') {
                    self.skip();
                    (Pow, Latin1String::from(b"**"))
                } else {
                    (Times, Latin1String::from(b"*"))
                }
            }
            b'?' => {
                self.skip();
                match self.current {
                    Some(b'?') => {
                        self.skip();
                        (QueQue, Latin1String::from(b"??"))
                    }
                    Some(b'=') => {
                        self.skip();
                        (QueEQ, Latin1String::from(b"?="))
                    }
                    Some(b'/') => {
                        if self.peek() == Some(b'=') {
                            self.skip();
                            self.skip();
                            (QueNE, Latin1String::from(b"?/="))
                        } else {
                            (Que, Latin1String::from(b"?"))
                        }
                    }
                    Some(b'<') => {
                        self.skip();
                        if self.current == Some(b'=') {
                            self.skip();
                            (QueLTE, Latin1String::from(b"?<="))
                        } else {
                            (QueLT, Latin1String::from(b"?<"))
                        }
                    }
                    Some(b'>') => {
                        self.skip();
                        if self.current == Some(b'=') {
                            self.skip();
                            (QueGTE, Latin1String::from(b"?>="))
                        } else {
                            (QueGT, Latin1String::from(b"?>"))
                        }
                    }
                    _ => (Que, Latin1String::from(b"?")),
                }
            }
            b'^' => {
                self.skip();
                (Circ, Latin1String::from(b"^"))
            }
            b'@' => {
                self.skip();
                (CommAt, Latin1String::from(b"@"))
            }
            b'|' => {
                self.skip();
                (Bar, Latin1String::from(b"|"))
            }
            b'[' => {
                self.skip();
                (LeftSquare, Latin1String::from(b"["))
            }
            b']' => {
                self.skip();
                (RightSquare, Latin1String::from(b"]"))
            }
            b'\\' => {
                let mut buf = Latin1String::new();
                let kind = self.quoted(&mut buf);
                (kind, buf)
            }
            b'`' => {
                let mut buf = Latin1String::new();
                self.tool_directive(&mut buf);
                (ToolDirective, buf)
            }
            ch => {
                self.skip();
                (Unknown, Latin1String::from(ch))
            }
        };
        self.last_token_kind = Some(kind);
        Some(Token::new(kind, text, leading_trivia))
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

fn str_to_keyword(inp: &Latin1Str) -> Option<Kw> {
    Some(match inp.to_lowercase().as_bytes() {
        b"abs" => Kw::Abs,
        b"access" => Kw::Access,
        b"after" => Kw::After,
        b"alias" => Kw::Alias,
        b"all" => Kw::All,
        b"and" => Kw::And,
        b"architecture" => Kw::Architecture,
        b"array" => Kw::Array,
        b"assert" => Kw::Assert,
        b"assume" => Kw::Assume,
        b"attribute" => Kw::Attribute,
        b"begin" => Kw::Begin,
        b"block" => Kw::Block,
        b"body" => Kw::Body,
        b"buffer" => Kw::Buffer,
        b"bus" => Kw::Bus,
        b"case" => Kw::Case,
        b"component" => Kw::Component,
        b"configuration" => Kw::Configuration,
        b"constant" => Kw::Constant,
        b"context" => Kw::Context,
        b"cover" => Kw::Cover,
        b"default" => Kw::Default,
        b"disconnect" => Kw::Disconnect,
        b"downto" => Kw::Downto,
        b"else" => Kw::Else,
        b"elsif" => Kw::Elsif,
        b"end" => Kw::End,
        b"entity" => Kw::Entity,
        b"exit" => Kw::Exit,
        b"fairness" => Kw::Fairness,
        b"file" => Kw::File,
        b"for" => Kw::For,
        b"force" => Kw::Force,
        b"function" => Kw::Function,
        b"generate" => Kw::Generate,
        b"generic" => Kw::Generic,
        b"group" => Kw::Group,
        b"guarded" => Kw::Guarded,
        b"if" => Kw::If,
        b"impure" => Kw::Impure,
        b"in" => Kw::In,
        b"inertial" => Kw::Inertial,
        b"inout" => Kw::Inout,
        b"is" => Kw::Is,
        b"label" => Kw::Label,
        b"library" => Kw::Library,
        b"linkage" => Kw::Linkage,
        b"literal" => Kw::Literal,
        b"loop" => Kw::Loop,
        b"map" => Kw::Map,
        b"mod" => Kw::Mod,
        b"nand" => Kw::Nand,
        b"new" => Kw::New,
        b"next" => Kw::Next,
        b"nor" => Kw::Nor,
        b"not" => Kw::Not,
        b"null" => Kw::Null,
        b"of" => Kw::Of,
        b"on" => Kw::On,
        b"open" => Kw::Open,
        b"or" => Kw::Or,
        b"others" => Kw::Others,
        b"out" => Kw::Out,
        b"package" => Kw::Package,
        b"parameter" => Kw::Parameter,
        b"port" => Kw::Port,
        b"postponed" => Kw::Postponed,
        b"procedure" => Kw::Procedure,
        b"process" => Kw::Process,
        b"property" => Kw::Property,
        b"protected" => Kw::Protected,
        b"private" => Kw::Private,
        b"pure" => Kw::Pure,
        b"range" => Kw::Range,
        b"record" => Kw::Record,
        b"register" => Kw::Register,
        b"reject" => Kw::Reject,
        b"release" => Kw::Release,
        b"rem" => Kw::Rem,
        b"report" => Kw::Report,
        b"restrict" => Kw::Restrict,
        b"return" => Kw::Return,
        b"rol" => Kw::Rol,
        b"ror" => Kw::Ror,
        b"select" => Kw::Select,
        b"sequence" => Kw::Sequence,
        b"severity" => Kw::Severity,
        b"signal" => Kw::Signal,
        b"shared" => Kw::Shared,
        b"sla" => Kw::Sla,
        b"sll" => Kw::Sll,
        b"sra" => Kw::Sra,
        b"srl" => Kw::Srl,
        b"strong" => Kw::Strong,
        b"subtype" => Kw::Subtype,
        b"then" => Kw::Then,
        b"to" => Kw::To,
        b"transport" => Kw::Transport,
        b"type" => Kw::Type,
        b"unaffected" => Kw::Unaffected,
        b"units" => Kw::Units,
        b"until" => Kw::Until,
        b"use" => Kw::Use,
        b"variable" => Kw::Variable,
        b"view" => Kw::View,
        b"vpgk" => Kw::Vpgk,
        b"vmode" => Kw::Vmode,
        b"vprop" => Kw::Vprop,
        b"vunit" => Kw::Vunit,
        b"wait" => Kw::Wait,
        b"when" => Kw::When,
        b"while" => Kw::While,
        b"with" => Kw::With,
        b"xnor" => Kw::Xnor,
        b"xor" => Kw::Xor,
        _ => return None,
    })
}

#[cfg(test)]
mod tests {

    use crate::latin_1::Latin1String;
    use crate::tokens::tokenizer::Tokenize;
    use crate::tokens::TokenKind;
    use crate::tokens::TokenKind::*;
    use crate::tokens::trivia_piece::Comment;
    use crate::tokens::{Keyword as Kw, Token, Trivia, TriviaPiece};
    use pretty_assertions::assert_eq;

    fn kinds_tokenize_remove_eof(code: &str) -> Vec<TokenKind> {
        let mut val = code.tokenize().map(|tok| tok.kind()).collect::<Vec<_>>();
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
            self.tokenize().collect()
        }

        fn tokenize_kind_value(&self) -> Vec<(TokenKind, String)> {
            self.tokenize()
                .map(|tok| (tok.kind(), tok.text().to_string()))
                .collect()
        }

        fn tokenize_kinds(&self) -> Vec<TokenKind> {
            self.tokenize().map(|tok| tok.kind()).collect()
        }
    }

    #[test]
    fn tokenize_empty_input() {
        assert_eq!("".tokenize_vec(), vec![Token::eof(Trivia::default())]);
    }

    #[test]
    fn tokenize_input_only_trivia() {
        assert_eq!("  ".tokenize_vec(), vec![Token::eof(Trivia::from([TriviaPiece::Spaces(2)]))]);
    }

    #[test]
    fn tokenize_keywords() {
        assert_eq!(
            kinds_tokenize_remove_eof("architecture"),
            vec![Keyword(Kw::Architecture)]
        );
        assert_eq!(kinds_tokenize_remove_eof("entity"), vec![Keyword(Kw::Entity)]);
        assert_eq!(kinds_tokenize_remove_eof("is"), vec![Keyword(Kw::Is)]);
        assert_eq!(kinds_tokenize_remove_eof("generic"), vec![Keyword(Kw::Generic)]);
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
                    Trivia::from([TriviaPiece::LineFeeds(2)]),
                ),
                Token::new(Identifier, b"foo", Trivia::from([TriviaPiece::Spaces(1)])),
                Token::eof(Trivia::default())
                
            ]
        );
    }

    #[test]
    fn tokenize_keywords_case_insensitive() {
        assert_eq!(kinds_tokenize_remove_eof("entity"), vec![Keyword(Kw::Entity)]);
        assert_eq!(kinds_tokenize_remove_eof("Entity"), vec![Keyword(Kw::Entity)]);
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
                Token::new(
                    Identifier,
                    b"my_ident",
                    Trivia::default(),
                ),
                Token::new(
                    Identifier,
                    b"my_other_ident",
                    Trivia::from([TriviaPiece::LineFeeds(2)])
                ),
                Token::eof(Trivia::default())
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
                .map(|tok| (tok.kind(), tok.text().to_string()))
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
                Token::new(
                    StringLiteral,
                    b"\"str\"",
                    Trivia::default(),
                ),
                Token::new(
                    StringLiteral,
                    b"\"ing\"",
                    Trivia::from([TriviaPiece::Spaces(1)]),
                ),
                Token::eof(Trivia::default())
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
            Token::simple(Unterminated, b"\"string")
        );
    }

    #[test]
    fn tokenize_bit_string_literal() {
        // Test all base specifiers
        for &base in ["b", "o", "x", "d", "sb", "so", "sx", "ub", "uo", "ux"].iter() {
            let (base_spec, value, length) = match base {
                "b" => ("b", "10", 2),
                "o" => ("o", "76543210", 8 * 3),
                "x" => ("x", "fedcba987654321", 16 * 4),
                "d" => ("d", "9876543210", 34),
                "sb" => ("sb", "10", 2),
                "so" => ("so", "76543210", 8 * 3),
                "sx" => ("sx", "fedcba987654321", 16 * 4),
                "ub" => ("ub", "10", 2),
                "uo" => ("uo", "76543210", 8 * 3),
                "ux" => ("ux", "fedcba987654321", 16 * 4),
                _ => unreachable!(),
            };

            // Test with upper and lower case base specifier
            for &upper_case in [true, false].iter() {
                // Test with and without length prefix
                for &use_length in [true, false].iter() {
                    let length_str = if use_length {
                        length.to_string()
                    } else {
                        "".to_owned()
                    };

                    let mut code =
                        Latin1String::from_utf8(&format!("{length_str}{base_spec}\"{value}\""))
                            .unwrap();

                    if upper_case {
                        code.make_uppercase()
                    }

                    let token = code.tokenize_one();
                    assert_eq!(token, Token::simple(BitStringLiteral, code));
                }
            }
        }
    }

    #[test]
    fn tokenize_illegal_bit_string() {
        assert_eq!("10x".tokenize_one(), Token::simple(Unknown, b"10x"));
        assert_eq!("10ux".tokenize_one(), Token::simple(Unknown, b"10ux"));
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
                    Trivia::from([TriviaPiece::LineFeeds(1)]),
                ),
                Token::new(Minus, b"-", Trivia::from([
                        TriviaPiece::LineFeeds(1),
                        TriviaPiece::LineComment(Comment::new(b"comment")),
                        TriviaPiece::LineFeeds(1)
                    ])),
                Token::new(
                    AbstractLiteral,
                    b"2",
                    Trivia::default(),
                ),
                Token::eof(
                    Trivia::from([TriviaPiece::LineFeeds(1)])
                )
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
                    Trivia::from([TriviaPiece::LineFeeds(1)]),
                ),
                Token::new(Minus, b"-", Trivia::from([
                        TriviaPiece::LineFeeds(2),
                        TriviaPiece::BlockComment(Comment::new(b"\ncomment\n")),
                        TriviaPiece::LineFeeds(2),
                    ])),
                Token::new(
                    AbstractLiteral,
                    b"2",
                    Trivia::default(),
                    
                ),
                Token::eof(Trivia::from([
                        TriviaPiece::Spaces(1),
                        TriviaPiece::BlockComment(Comment::new("\ncomment\n")),
                        TriviaPiece::LineFeeds(2),
                    ]),)
            ]
        )
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
}
