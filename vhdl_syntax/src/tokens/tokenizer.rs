// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2024, Lukas Scheller lukasscheller@icloud.com

use crate::latin_1::Latin1String;
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

/// The Tokenizer is an iterator that consumes some char iterator (i.e., an iterator that
/// produces u8 items) and generates tokens.
/// All iterators that implement `IntoIter<Item = u8>` can be used for this purpose. For example.
/// ```
/// use vhdl_syntax::tokens::{Keyword, TokenKind, Tokenizer};
/// // &str implements IntoIter<u8>, therefore it can be converted to a tokenizer
/// let mut tokenizer = Tokenizer::from("entity foo".bytes());
/// assert_eq!(tokenizer.next().unwrap().kind(), TokenKind::Keyword(Keyword::Entity));
/// assert_eq!(tokenizer.next().unwrap().kind(), TokenKind::Identifier);
/// assert_eq!(tokenizer.next(), None);
/// ```
///
/// The [Tokenize] trait enables syntactic sugar to work with tokenizers using iterators:
/// ```
/// use vhdl_syntax::tokens::tokenizer::Tokenize;
/// use vhdl_syntax::tokens::TokenKind;
///
/// let tokens = "a ?> b".tokenize().map(|tok| tok.kind()).collect::<Vec<_>>();
/// assert_eq!(tokens, vec![TokenKind::Identifier, TokenKind::QueGT, TokenKind::Identifier])
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
}

impl<I: Iterator<Item = u8>> Tokenizer<I> {
    /// Creates a new tokenizer from some iterator.
    pub fn new(mut text: I) -> Self {
        let current = text.next();
        Tokenizer {
            text: text.peekable(),
            current,
            last_token_kind: None,
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
                    let mut comment = Latin1String::default();
                    while let Some(ch) = self.skip_if(|ch| !matches!(ch, b'\r' | b'\n')) {
                        comment.push(ch);
                    }
                    TriviaPiece::LineComment(comment.to_string())
                } else {
                    return None;
                }
            }
            b'/' => {
                if self.peek() == Some(b'*') {
                    self.skip();
                    self.skip();
                    let mut comment = Latin1String::default();
                    loop {
                        if self.current == Some(b'*') && self.peek() == Some(b'/') {
                            self.next();
                            self.next();
                            break;
                        }
                        let Some(ch) = self.skip() else { break };
                        comment.push(ch)
                    }
                    TriviaPiece::BlockComment(comment.to_string())
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
            assert!(
                leading_trivia.is_empty(),
                "Leading trivia is not empty at last token"
            );
            return None;
        };
        let (kind, text) = match current {
            b'a'..=b'z' | b'A'..=b'Z' => {
                let mut buf = Latin1String::default();
                let kind = self.identifier_keyword_or_bistring_literal(&mut buf);
                let ident_str = buf.to_string();
                if let Some(kind) = kind {
                    (kind, ident_str)
                } else if let Some(kw) = str_to_keyword(&ident_str) {
                    (Keyword(kw), ident_str)
                } else {
                    (Identifier, ident_str)
                }
            }
            b'0'..=b'9' => {
                let mut buf = Latin1String::default();
                let kind = self.abstract_literal(&mut buf);
                (kind, buf.to_string())
            }
            b':' => {
                self.skip();
                if self.current == Some(b'=') {
                    self.skip();
                    (ColonEq, ":=".to_string())
                } else {
                    (Colon, ":".to_string())
                }
            }
            b'\'' => {
                self.skip();
                if can_be_char(self.last_token_kind) {
                    if self.peek() == Some(b'\'') {
                        let ret = (
                            CharacterLiteral,
                            format!("'{}'", self.skip().unwrap() as char),
                        );
                        self.skip();
                        ret
                    } else {
                        (Tick, "'".to_string())
                    }
                } else {
                    (Tick, "'".to_string())
                }
            }
            b'-' => {
                self.skip();
                (Minus, "-".to_string())
            }
            b'"' => {
                let mut buf = Latin1String::default();
                let kind = self.quoted(&mut buf);
                (kind, buf.to_string())
            }
            b';' => {
                self.skip();
                (SemiColon, ";".to_string())
            }
            b'(' => {
                self.skip();
                (LeftPar, "(".to_string())
            }
            b')' => {
                self.skip();
                (RightPar, ")".to_string())
            }
            b'+' => {
                self.skip();
                (Plus, "+".to_string())
            }
            b'.' => {
                self.skip();
                (Dot, ".".to_string())
            }
            b'&' => {
                self.skip();
                (Concat, "&".to_string())
            }
            b',' => {
                self.skip();
                (Comma, ",".to_string())
            }
            b'=' => {
                self.skip();
                if self.current == Some(b'>') {
                    self.skip();
                    (RightArrow, "=>".to_string())
                } else {
                    (EQ, "=".to_string())
                }
            }
            b'<' => {
                self.skip();
                match self.current {
                    Some(b'=') => {
                        self.skip();
                        (LTE, "<=".to_string())
                    }
                    Some(b'>') => {
                        self.skip();
                        (BOX, "<>".to_string())
                    }
                    Some(b'<') => {
                        self.skip();
                        (LtLt, "<<".to_string())
                    }
                    _ => (LT, "<".to_string()),
                }
            }
            b'>' => {
                self.skip();
                match self.current {
                    Some(b'=') => {
                        self.skip();
                        (GTE, ">=".to_string())
                    }
                    Some(b'>') => {
                        self.skip();
                        (GtGt, ">>".to_string())
                    }
                    _ => (GT, ">".to_string()),
                }
            }
            b'/' => {
                self.skip();
                if self.current == Some(b'=') {
                    self.skip();
                    (NE, "/=".to_string())
                } else {
                    (Div, "/".to_string())
                }
            }
            b'*' => {
                self.skip();
                if self.current == Some(b'*') {
                    self.skip();
                    (Pow, "**".to_string())
                } else {
                    (Times, "*".to_string())
                }
            }
            b'?' => {
                self.skip();
                match self.current {
                    Some(b'?') => {
                        self.skip();
                        (QueQue, "??".to_string())
                    }
                    Some(b'=') => {
                        self.skip();
                        (QueEQ, "?=".to_string())
                    }
                    Some(b'/') => {
                        if self.peek() == Some(b'=') {
                            self.skip();
                            self.skip();
                            (QueNE, "?/=".to_string())
                        } else {
                            (Que, "?".to_string())
                        }
                    }
                    Some(b'<') => {
                        self.skip();
                        if self.current == Some(b'=') {
                            self.skip();
                            (QueLTE, "?<=".to_string())
                        } else {
                            (QueLT, "?<".to_string())
                        }
                    }
                    Some(b'>') => {
                        self.skip();
                        if self.current == Some(b'=') {
                            self.skip();
                            (QueGTE, "?>=".to_string())
                        } else {
                            (QueGT, "?>".to_string())
                        }
                    }
                    _ => (Que, "?".to_string()),
                }
            }
            b'^' => {
                self.skip();
                (Circ, "^".to_string())
            }
            b'@' => {
                self.skip();
                (CommAt, "@".to_string())
            }
            b'|' => {
                self.skip();
                (Bar, "|".to_string())
            }
            b'[' => {
                self.skip();
                (LeftSquare, "[".to_string())
            }
            b']' => {
                self.skip();
                (RightSquare, "]".to_string())
            }
            b'\\' => {
                let mut buf = Latin1String::default();
                let kind = self.quoted(&mut buf);
                (kind, buf.to_string())
            }
            b'`' => {
                let mut buf = Latin1String::default();
                self.tool_directive(&mut buf);
                (ToolDirective, buf.to_string())
            }
            ch => {
                self.skip();
                (Unknown, ch.to_string())
            }
        };
        self.last_token_kind = Some(kind);
        let trailing_trivia = self.consume_trivia();
        Some(Token {
            text,
            kind,
            leading_trivia,
            trailing_trivia,
        })
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

fn str_to_keyword(inp: &str) -> Option<Kw> {
    Some(match inp.to_ascii_lowercase().as_str() {
        "abs" => Kw::Abs,
        "access" => Kw::Access,
        "after" => Kw::After,
        "alias" => Kw::Alias,
        "all" => Kw::All,
        "and" => Kw::And,
        "architecture" => Kw::Architecture,
        "array" => Kw::Array,
        "assert" => Kw::Assert,
        "assume" => Kw::Assume,
        "attribute" => Kw::Attribute,
        "begin" => Kw::Begin,
        "block" => Kw::Block,
        "body" => Kw::Body,
        "buffer" => Kw::Buffer,
        "bus" => Kw::Bus,
        "case" => Kw::Case,
        "component" => Kw::Component,
        "configuration" => Kw::Configuration,
        "constant" => Kw::Constant,
        "context" => Kw::Context,
        "cover" => Kw::Cover,
        "default" => Kw::Default,
        "disconnect" => Kw::Disconnect,
        "downto" => Kw::Downto,
        "else" => Kw::Else,
        "elsif" => Kw::Elsif,
        "end" => Kw::End,
        "entity" => Kw::Entity,
        "exit" => Kw::Exit,
        "fairness" => Kw::Fairness,
        "file" => Kw::File,
        "for" => Kw::For,
        "force" => Kw::Force,
        "function" => Kw::Function,
        "generate" => Kw::Generate,
        "generic" => Kw::Generic,
        "group" => Kw::Group,
        "guarded" => Kw::Guarded,
        "if" => Kw::If,
        "impure" => Kw::Impure,
        "in" => Kw::In,
        "inertial" => Kw::Inertial,
        "inout" => Kw::Inout,
        "is" => Kw::Is,
        "label" => Kw::Label,
        "library" => Kw::Library,
        "linkage" => Kw::Linkage,
        "literal" => Kw::Literal,
        "loop" => Kw::Loop,
        "map" => Kw::Map,
        "mod" => Kw::Mod,
        "nand" => Kw::Nand,
        "new" => Kw::New,
        "next" => Kw::Next,
        "nor" => Kw::Nor,
        "not" => Kw::Not,
        "null" => Kw::Null,
        "of" => Kw::Of,
        "on" => Kw::On,
        "open" => Kw::Open,
        "or" => Kw::Or,
        "others" => Kw::Others,
        "out" => Kw::Out,
        "package" => Kw::Package,
        "parameter" => Kw::Parameter,
        "port" => Kw::Port,
        "postponed" => Kw::Postponed,
        "procedure" => Kw::Procedure,
        "process" => Kw::Process,
        "property" => Kw::Property,
        "protected" => Kw::Protected,
        "private" => Kw::Private,
        "pure" => Kw::Pure,
        "range" => Kw::Range,
        "record" => Kw::Record,
        "register" => Kw::Register,
        "reject" => Kw::Reject,
        "release" => Kw::Release,
        "rem" => Kw::Rem,
        "report" => Kw::Report,
        "restrict" => Kw::Restrict,
        "return" => Kw::Return,
        "rol" => Kw::Rol,
        "ror" => Kw::Ror,
        "select" => Kw::Select,
        "sequence" => Kw::Sequence,
        "severity" => Kw::Severity,
        "signal" => Kw::Signal,
        "shared" => Kw::Shared,
        "sla" => Kw::Sla,
        "sll" => Kw::Sll,
        "sra" => Kw::Sra,
        "srl" => Kw::Srl,
        "strong" => Kw::Strong,
        "subtype" => Kw::Subtype,
        "then" => Kw::Then,
        "to" => Kw::To,
        "transport" => Kw::Transport,
        "type" => Kw::Type,
        "unaffected" => Kw::Unaffected,
        "units" => Kw::Units,
        "until" => Kw::Until,
        "use" => Kw::Use,
        "variable" => Kw::Variable,
        "view" => Kw::View,
        "vpgk" => Kw::Vpgk,
        "vmode" => Kw::Vmode,
        "vprop" => Kw::Vprop,
        "vunit" => Kw::Vunit,
        "wait" => Kw::Wait,
        "when" => Kw::When,
        "while" => Kw::While,
        "with" => Kw::With,
        "xnor" => Kw::Xnor,
        "xor" => Kw::Xor,
        _ => return None,
    })
}

#[cfg(test)]
mod tests {

    use crate::tokens::tokenizer::Tokenize;
    use crate::tokens::TokenKind;
    use crate::tokens::TokenKind::*;
    use crate::tokens::{Keyword as Kw, Token, Trivia, TriviaPiece};
    use pretty_assertions::assert_eq;

    fn kinds_tokenize(code: &str) -> Vec<TokenKind> {
        code.tokenize().map(|tok| tok.kind()).collect()
    }

    trait TokenizeVec {
        fn tokenize_vec(&self) -> Vec<Token>;

        fn tokenize_kind_value(&self) -> Vec<(TokenKind, String)>;

        fn tokenize_kinds(&self) -> Vec<TokenKind>;
    }

    impl<T> TokenizeVec for T
    where
        T: Tokenize,
    {
        fn tokenize_vec(&self) -> Vec<Token> {
            self.tokenize().collect()
        }

        fn tokenize_kind_value(&self) -> Vec<(TokenKind, String)> {
            self.tokenize().map(|tok| (tok.kind, tok.text)).collect()
        }

        fn tokenize_kinds(&self) -> Vec<TokenKind> {
            self.tokenize().map(|tok| tok.kind).collect()
        }
    }

    #[test]
    fn tokenize_keywords() {
        assert_eq!(
            kinds_tokenize("architecture"),
            vec![Keyword(Kw::Architecture)]
        );
        assert_eq!(kinds_tokenize("entity"), vec![Keyword(Kw::Entity)]);
        assert_eq!(kinds_tokenize("is"), vec![Keyword(Kw::Is)]);
        assert_eq!(kinds_tokenize("generic"), vec![Keyword(Kw::Generic)]);
        assert_eq!(kinds_tokenize("port"), vec![Keyword(Kw::Port)]);
        assert_eq!(kinds_tokenize("begin"), vec![Keyword(Kw::Begin)]);
        assert_eq!(kinds_tokenize("end"), vec![Keyword(Kw::End)]);
        assert_eq!(kinds_tokenize("all"), vec![Keyword(Kw::All)]);
        assert_eq!(kinds_tokenize("abs"), vec![Keyword(Kw::Abs)]);
        assert_eq!(kinds_tokenize("not"), vec![Keyword(Kw::Not)]);
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
                Keyword(Kw::Entity)
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
                Token {
                    kind: Keyword(Kw::Entity),
                    text: "entity".to_string(),
                    leading_trivia: Trivia::new([TriviaPiece::LineFeeds(2)]),
                    trailing_trivia: Trivia::new([TriviaPiece::Spaces(1)]),
                },
                Token {
                    kind: Identifier,
                    text: "foo".to_string(),
                    leading_trivia: Trivia::new([]),
                    trailing_trivia: Trivia::new([]),
                }
            ]
        );
    }

    #[test]
    fn tokenize_keywords_case_insensitive() {
        assert_eq!(kinds_tokenize("entity"), vec![Keyword(Kw::Entity)]);
        assert_eq!(kinds_tokenize("Entity"), vec![Keyword(Kw::Entity)]);
        assert_eq!(
            kinds_tokenize("arCHitecture"),
            vec![Keyword(Kw::Architecture)]
        );
    }

    #[test]
    fn tokenize_identifier() {
        assert_eq!(
            "my_ident".tokenize_vec(),
            vec![Token {
                kind: Identifier,
                text: "my_ident".to_string(),
                leading_trivia: Trivia::default(),
                trailing_trivia: Trivia::default(),
            }]
        );
    }

    #[test]
    fn tokenize_extended_identifier() {
        assert_eq!(
            "\\1$my_ident\\".tokenize_vec(),
            vec![Token {
                kind: Identifier,
                text: "\\1$my_ident\\".to_string(),
                leading_trivia: Trivia::default(),
                trailing_trivia: Trivia::default(),
            }]
        );
        assert_eq!(
            "\\my\\\\_ident\\".tokenize_vec(),
            vec![Token {
                kind: Identifier,
                text: "\\my\\\\_ident\\".to_string(),
                leading_trivia: Trivia::default(),
                trailing_trivia: Trivia::default(),
            }]
        );
    }

    #[test]
    fn tokenize_many_identifiers() {
        assert_eq!(
            "my_ident

my_other_ident"
                .tokenize_vec(),
            vec![
                Token {
                    kind: Identifier,
                    text: "my_ident".to_string(),
                    leading_trivia: Trivia::default(),
                    trailing_trivia: Trivia::new([TriviaPiece::LineFeeds(2)]),
                },
                Token {
                    kind: Identifier,
                    text: "my_other_ident".to_string(),
                    leading_trivia: Trivia::default(),
                    trailing_trivia: Trivia::default(),
                }
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
            ]
        );
    }

    #[test]
    fn tokenize_real() {
        assert_eq!(
            "0.1 -2_2.3_3 2.0e3 3.33E2 2.1e-2 4.4e+1 2.5E+3"
                .tokenize()
                .map(|tok| (tok.kind, tok.text))
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
            ]
        );
    }

    #[test]
    fn tokenize_real_many_fractional_digits() {
        assert_eq!(
            "0.1000_0000_0000_0000_0000_0000_0000_0000".tokenize_kind_value(),
            vec![(
                AbstractLiteral,
                "0.1000_0000_0000_0000_0000_0000_0000_0000".to_string()
            )]
        );
    }

    #[test]
    fn tokenize_real_many_integer_digits() {
        assert_eq!(
            "1000_0000_0000_0000_0000_0000_0000_0000.0".tokenize_kind_value(),
            vec![(
                AbstractLiteral,
                "1000_0000_0000_0000_0000_0000_0000_0000.0".to_string()
            )]
        );
    }

    #[test]
    fn tokenize_string_literal() {
        assert_eq!(
            "\"string\"".tokenize_vec(),
            vec![Token {
                kind: StringLiteral,
                text: "\"string\"".to_string(),
                leading_trivia: Trivia::default(),
                trailing_trivia: Trivia::default()
            }]
        );
    }

    #[test]
    fn tokenize_string_literal_quote() {
        assert_eq!(
            "\"str\"\"ing\"".tokenize_vec(),
            vec![Token {
                kind: StringLiteral,
                text: "\"str\"\"ing\"".to_string(),
                leading_trivia: Trivia::default(),
                trailing_trivia: Trivia::default()
            }]
        );
    }

    #[test]
    fn tokenize_string_literal_quote_separated() {
        assert_eq!(
            "\"str\" \"ing\"".tokenize_vec(),
            vec![
                Token {
                    kind: StringLiteral,
                    text: "\"str\"".to_string(),
                    leading_trivia: Trivia::default(),
                    trailing_trivia: Trivia::new([TriviaPiece::Spaces(1)])
                },
                Token {
                    kind: StringLiteral,
                    text: "\"ing\"".to_string(),
                    leading_trivia: Trivia::default(),
                    trailing_trivia: Trivia::default()
                },
            ]
        );
    }

    #[test]
    fn tokenize_string_literal_multiline() {
        assert_eq!(
            "\"str\ning\"".tokenize_vec(),
            vec![Token {
                kind: StringLiteral,
                text: "\"str\ning\"".to_string(),
                leading_trivia: Trivia::default(),
                trailing_trivia: Trivia::default()
            }]
        );
    }

    #[test]
    fn tokenize_string_literal_error_on_early_eof() {
        assert_eq!(
            "\"string".tokenize_vec(),
            vec![Token {
                kind: Unterminated,
                text: "\"string".to_string(),
                leading_trivia: Trivia::default(),
                trailing_trivia: Trivia::default()
            }]
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

                    let mut code = format!("{length_str}{base_spec}\"{value}\"");

                    if upper_case {
                        code.make_ascii_uppercase()
                    }

                    let tokens = code.tokenize_vec();
                    assert_eq!(
                        tokens,
                        vec![Token {
                            kind: BitStringLiteral,
                            text: code,
                            leading_trivia: Trivia::default(),
                            trailing_trivia: Trivia::default(),
                        }]
                    );
                }
            }
        }
    }

    #[test]
    fn tokenize_illegal_bit_string() {
        assert_eq!(
            "10x".tokenize_vec(),
            vec![Token {
                kind: Unknown,
                text: "10x".to_string(),
                leading_trivia: Trivia::default(),
                trailing_trivia: Trivia::default(),
            }]
        );
        assert_eq!(
            "10ux".tokenize_vec(),
            vec![Token {
                kind: Unknown,
                text: "10ux".to_string(),
                leading_trivia: Trivia::default(),
                trailing_trivia: Trivia::default(),
            }]
        );
    }

    #[test]
    fn tokenize_based_integer() {
        assert_eq!(
            "2#101#".tokenize_kind_value(),
            vec![(AbstractLiteral, "2#101#".to_string())]
        );
        assert_eq!(
            "8#321#".tokenize_kind_value(),
            vec![(AbstractLiteral, "8#321#".to_string())]
        );
        assert_eq!(
            "16#eEFfa#".tokenize_kind_value(),
            vec![(AbstractLiteral, "16#eEFfa#".to_string())]
        );
        // This is illegal, but the checking happens at a later stage
        assert_eq!(
            "3#3#".tokenize_kind_value(),
            vec![(AbstractLiteral, "3#3#".to_string())]
        );
    }

    macro_rules! check_tokenize {
        ($tokens:literal, $kind:expr) => {
            assert_eq!(
                $tokens.tokenize_kind_value(),
                vec![($kind, $tokens.to_string())]
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
                Token {
                    kind: AbstractLiteral,
                    text: "1".to_string(),
                    leading_trivia: Trivia::new([TriviaPiece::LineFeeds(1)]),
                    trailing_trivia: Trivia::new([
                        TriviaPiece::LineFeeds(1),
                        TriviaPiece::LineComment("comment".to_string()),
                        TriviaPiece::LineFeeds(1)
                    ]),
                },
                Token {
                    kind: Minus,
                    text: "-".to_string(),
                    leading_trivia: Trivia::default(),
                    trailing_trivia: Trivia::default(),
                },
                Token {
                    kind: AbstractLiteral,
                    text: "2".to_string(),
                    leading_trivia: Trivia::default(),
                    trailing_trivia: Trivia::new([TriviaPiece::LineFeeds(1)]),
                }
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
                Token {
                    kind: AbstractLiteral,
                    text: "1".to_string(),
                    leading_trivia: Trivia::new([TriviaPiece::LineFeeds(1)]),
                    trailing_trivia: Trivia::new([
                        TriviaPiece::LineFeeds(2),
                        TriviaPiece::BlockComment("\ncomment\n".to_string())
                    ]),
                },
                Token {
                    kind: Minus,
                    text: "-".to_string(),
                    leading_trivia: Trivia::default(),
                    trailing_trivia: Trivia::default(),
                },
                Token {
                    kind: AbstractLiteral,
                    text: "2".to_string(),
                    leading_trivia: Trivia::default(),
                    trailing_trivia: Trivia::new([
                        TriviaPiece::Spaces(1),
                        TriviaPiece::BlockComment("\ncomment\n".to_string()),
                    ]),
                },
            ]
        )
    }

    #[test]
    fn tokenize_ir1045() {
        // http://www.eda-stds.org/isac/IRs-VHDL-93/IR1045.txt
        assert_eq!(
            "string'('a')".tokenize_kinds(),
            vec![Identifier, Tick, LeftPar, CharacterLiteral, RightPar]
        );
    }

    #[test]
    fn tokenize_illegal() {
        assert_eq!(
            "begin!end".tokenize_kinds(),
            vec![Keyword(Kw::Begin), Unknown, Keyword(Kw::End),]
        );
    }
}
