// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use fnv::FnvHashMap;

use crate::ast::token_range::WithToken;
use crate::ast::{self, AttributeDesignator, Operator, WithRef};
use crate::ast::{BaseSpecifier, Ident};
use crate::data::*;

/// The kind of a Token
#[derive(PartialEq, Eq, Clone, Copy, Debug, IntoStaticStr)]
#[strum(serialize_all = "lowercase")]
pub enum Kind {
    // Keywords
    Architecture,
    Entity,
    Configuration,
    Package,
    Block,
    Process,
    Generate,
    Postponed,
    Library,
    Label,
    Use,
    Context,
    Body,
    Component,
    Is,
    Return,
    Null,
    Of,
    On,
    Generic,
    Map,
    Default,
    Port,
    Attribute,
    Begin,
    If,
    Loop,
    While,
    Case,
    Else,
    Elsif,
    Then,
    When,
    With,
    Select,
    Next,
    Exit,
    For,
    Force,
    Release,
    Assert,
    Report,
    Severity,
    Wait,
    After,
    Transport,
    Inertial,
    Reject,
    Unaffected,
    Until,
    End,
    All,
    Range,
    Downto,
    To,
    In,
    Out,
    InOut,
    Buffer,
    Linkage,
    Signal,
    Constant,
    Variable,
    File,
    Open,
    Alias,
    Shared,
    Others,
    Record,
    Type,
    Subtype,
    Access,
    Units,
    New,
    Array,
    Protected,
    Pure,
    Impure,
    Function,
    Procedure,
    Vunit,
    Parameter,
    Literal,
    Bus,
    Disconnect,
    Group,
    Guarded,
    Register,
    Assume,
    #[strum(serialize = "assume_guarantee")]
    AssumeGuarantee,
    Cover,
    Fairness,
    Property,
    Restrict,
    #[strum(serialize = "restrict_guarantee")]
    RestrictGuarantee,
    Sequence,
    Strong,
    Vmode,
    Vprop,
    Private,
    View,
    Vpkg,

    // Unary operators
    Abs,
    Not,

    // Unary and binary operators
    #[strum(serialize = "+")]
    Plus,
    #[strum(serialize = "-")]
    Minus,
    #[strum(serialize = "??")]
    QueQue,

    // Binary operators
    And,
    Or,
    Nand,
    Nor,
    Xor,
    Xnor,
    SLL,
    SRL,
    SLA,
    SRA,
    ROL,
    ROR,

    Mod,
    Rem,

    #[strum(serialize = "=")]
    EQ,
    #[strum(serialize = "/=")]
    NE,
    #[strum(serialize = "<")]
    LT,
    #[strum(serialize = "<=")]
    LTE,
    #[strum(serialize = ">")]
    GT,
    #[strum(serialize = ">=")]
    GTE,

    #[strum(serialize = "?=")]
    QueEQ,
    #[strum(serialize = "?/=")]
    QueNE,
    #[strum(serialize = "?<")]
    QueLT,
    #[strum(serialize = "?<=")]
    QueLTE,
    #[strum(serialize = "?>")]
    QueGT,
    #[strum(serialize = "?>=")]
    QueGTE,
    #[strum(serialize = "?")]
    Que,

    #[strum(serialize = "*")]
    Times,
    #[strum(serialize = "**")]
    Pow,
    #[strum(serialize = "/")]
    Div,

    #[strum(serialize = "{identifier}")]
    Identifier,
    #[strum(serialize = "{abstract_literal}")]
    AbstractLiteral,
    #[strum(serialize = "{string}")]
    StringLiteral,
    #[strum(serialize = "{bit_string}")]
    BitString,
    #[strum(serialize = "{character}")]
    Character,
    #[strum(serialize = "'")]
    Tick,
    #[strum(serialize = "(")]
    LeftPar,
    #[strum(serialize = ")")]
    RightPar,
    #[strum(serialize = "[")]
    LeftSquare,
    #[strum(serialize = "]")]
    RightSquare,
    #[strum(serialize = ";")]
    SemiColon,
    #[strum(serialize = ":")]
    Colon,
    #[strum(serialize = "|")]
    Bar,
    #[strum(serialize = ".")]
    Dot,
    #[strum(serialize = "<>")]
    BOX,
    #[strum(serialize = "<<")]
    LtLt,
    #[strum(serialize = ">>")]
    GtGt,
    #[strum(serialize = "^")]
    Circ,
    #[strum(serialize = "@")]
    CommAt,
    #[strum(serialize = "&")]
    Concat,
    #[strum(serialize = ",")]
    Comma,
    #[strum(serialize = ":=")]
    ColonEq,
    #[strum(serialize = "=>")]
    RightArrow,
    #[strum(serialize = "`")]
    GraveAccent,
    #[strum(serialize = "{text}")]
    Text, // Raw text that is not processed (i.e. tokenized) further. Used in tool directives
}

impl Kind {
    pub fn as_str(&self) -> &str {
        self.into()
    }
}

use self::Kind::*;

/// Expect any number of token kind patterns, return on no match with
/// error diagnostic based on expected kinds
#[macro_export]
macro_rules! peek_token {
    ($tokens:expr, $token:ident, $($($kind:ident)|+ => $result:expr),*) => {
        {
            let $token = $tokens.peek_expect()?;
            match $token.kind {
                $(
                    $($kind)|+ => $result
                ),*,
                _ => {
                    let kinds = vec![
                        $(
                            $(
                                $kind,
                            )*
                        )*
                    ];

                    return Err($crate::syntax::tokens::kinds_error($tokens.pos_before($token), &kinds));
                }
            }
        }
    };
    ($tokens:expr, $token:ident, $token_id:ident, $($($kind:ident)|+ => $result:expr),*) => {
        {
            let $token = $tokens.peek_expect()?;
            let $token_id = $tokens.get_current_token_id();
            match $token.kind {
                $(
                    $($kind)|+ => $result
                ),*,
                _ => {
                    let kinds = vec![
                        $(
                            $(
                                $kind,
                            )*
                        )*
                    ];

                    return Err($crate::syntax::tokens::kinds_error($tokens.pos_before($token), &kinds));
                }
            }
        }
    }
}

/// Expect any number of token kind patterns, return on no match with
/// error diagnostic based on expected kinds
///
/// Note: never consumes a token it does not expect
#[macro_export]
macro_rules! expect_token {
    ($tokens:expr, $token:ident, $($($kind:ident)|+ => $result:expr),*) => {
        {
            let $token = $tokens.peek_expect()?;
            match $token.kind {
                $(
                    $($kind)|+ => {
                        $tokens.skip();
                        $result
                    }
                ),*
                _ => {
                    let kinds = vec![
                        $(
                            $(
                                $kind,
                            )*
                        )*
                    ];

                    return Err($crate::syntax::tokens::kinds_error($tokens.pos_before($token), &kinds));
                }
            }
        }
    };
    ($tokens:expr, $token:ident, $token_id:ident, $($($kind:ident)|+ => $result:expr),*) => {
        {
            let $token = $tokens.peek_expect()?;
            let $token_id = $tokens.get_current_token_id();
            match $token.kind {
                $(
                    $($kind)|+ => {
                        $tokens.skip();
                        $result
                    }
                ),*
                _ => {
                    let kinds = vec![
                        $(
                            $(
                                $kind,
                            )*
                        )*
                    ];

                    return Err($crate::syntax::tokens::kinds_error($tokens.pos_before($token), &kinds));
                }
            }
        }
    }
}

/// Expect any number of token kind patterns, return on no match with
/// error diagnostic based on expected kinds
///
/// Unlike the try_token_kind this macro gives errors always on the next token
/// Example:
///
/// ```vhdl
/// entity ent is
/// end entity;
///            ~ <- error should not be here
///
/// foo
/// ~~~ <- error should be here
/// ```
#[macro_export]
macro_rules! try_init_token_kind {
    ($token:expr, $($($kind:ident)|+ => $result:expr),*) => {
        match $token.kind {
            $(
                $($kind)|+ => $result
            ),*,
            _ => {
                let kinds = vec![
                    $(
                        $(
                            $kind,
                        )*
                    )*
                ];

                return Err($token.kinds_error(&kinds));
            }
        }
    }
}

pub fn kind_str(kind: Kind) -> &'static str {
    kind.into()
}

/// Create s string representation of the kinds separated by a separator
pub fn kinds_str(kinds: &[Kind]) -> String {
    let mut result = String::new();
    for (i, kind) in kinds.iter().enumerate() {
        result.push('\'');
        result.push_str(kind_str(*kind));
        result.push('\'');

        if i == kinds.len() - 1 {
        } else if i == kinds.len() - 2 {
            result.push_str(" or ");
        } else {
            result.push_str(", ");
        }
    }
    result
}

/// The value of a Token
#[derive(PartialEq, Clone, Debug)]
pub enum Value {
    Identifier(Symbol),
    String(Latin1String),
    BitString(Latin1String, ast::BitString),
    AbstractLiteral(Latin1String, ast::AbstractLiteral),
    Character(u8),
    // Raw text that is not processed (i.e. tokenized) further. Used in tool directives
    Text(Latin1String),
    None,
}

/// A Token
#[derive(PartialEq, Clone, Debug)]
pub struct Token {
    pub kind: Kind,
    pub value: Value,
    pub pos: SrcPos,
    pub comments: Option<Box<TokenComments>>,
}

/// A TokenId represents a unique value that is used to access a token.
/// A token ID cannot be created directly by the user. Instead, the value must be taken
/// from the AST.
#[derive(PartialEq, Eq, Debug, Clone, Copy, Ord, PartialOrd)]
pub struct TokenId(usize);

/// The TokenId represents an index into an array of tokens.
/// As access is restricted (only the token stream creates and raw access to the
/// vector of tokens is done using the parse context), a `TokenId` is always guaranteed to
/// point to a valid token.
impl TokenId {
    pub(crate) fn new(idx: usize) -> TokenId {
        TokenId(idx)
    }

    pub fn pos<'a>(&'a self, ctx: &'a dyn TokenAccess) -> &'a SrcPos {
        ctx.get_pos(*self)
    }
}

impl From<TokenId> for TokenSpan {
    fn from(value: TokenId) -> Self {
        TokenSpan {
            start_token: value,
            end_token: value,
        }
    }
}

impl AddAssign<usize> for TokenId {
    fn add_assign(&mut self, rhs: usize) {
        self.0 += rhs
    }
}

impl Sub<usize> for TokenId {
    type Output = TokenId;

    fn sub(self, rhs: usize) -> Self::Output {
        TokenId(self.0 - rhs)
    }
}

impl Add<usize> for TokenId {
    type Output = TokenId;

    fn add(self, rhs: usize) -> Self::Output {
        TokenId(self.0 + rhs)
    }
}

/// AST elements for which it is necessary to get the underlying tokens can implement the `HasTokenSpan` trait.
/// The trait provides getters for the start and end token.
///
/// Using the `with_token_span` attribute macro, the necessary fields can be inserted, and `HasTokenSpan` is implemented automatically.
///
/// For enums containing alternative AST elements the custom derive macro can be used directly under certain constraints:
/// 1. All variants must contain exactly one unnamed field.
/// 2. The fields of all variants must implement the `HasTokenSpan` trait one way or another.
///
/// Example:
/// ```rust
/// use vhdl_lang::ast::Name;
/// use vhdl_lang::ast::token_range::WithTokenSpan;
/// use vhdl_lang_macros::{with_token_span, TokenSpan};
///
/// // With `with_token_span` a field `info` of type `(TokenId, TokenId)` is inserted.
/// // Additionally the `HasTokenSpan` trait is implemented using the `TokenSpan` derive macro
/// #[with_token_span]
/// #[derive(PartialEq, Debug, Clone)]
/// pub struct UseClause {
///     pub name_list: Vec<WithTokenSpan<Name>>,
/// }
///
/// #[with_token_span]
/// #[derive(PartialEq, Debug, Clone)]
/// pub struct ContextReference {
///     pub name_list: Vec<WithTokenSpan<Name>>,
/// }
///
/// #[with_token_span]
/// #[derive(PartialEq, Debug, Clone)]
/// pub struct LibraryClause {
///     pub name_list: Vec<::vhdl_lang::ast::WithRef<::vhdl_lang::ast::Ident>>,
/// }
///
/// // Enums can use the `TokenSpan` derive macro directly
/// #[derive(PartialEq, Debug, Clone, TokenSpan)]
/// pub enum ContextItem {
///     Use(UseClause),
///     Library(LibraryClause),
///     Context(ContextReference),
/// }
/// ```
pub trait HasTokenSpan {
    fn get_start_token(&self) -> TokenId;
    fn get_end_token(&self) -> TokenId;

    fn get_token_slice<'a>(&self, tokens: &'a dyn TokenAccess) -> &'a [Token] {
        tokens.get_token_slice(self.get_start_token(), self.get_end_token())
    }

    fn get_pos(&self, tokens: &dyn TokenAccess) -> SrcPos {
        tokens.get_span(self.get_start_token(), self.get_end_token())
    }

    fn get_span(&self, ctx: &dyn TokenAccess) -> SrcPos {
        ctx.get_span(self.get_start_token(), self.get_end_token())
    }

    fn span(&self) -> TokenSpan {
        TokenSpan::new(self.get_start_token(), self.get_end_token())
    }
}

/// Holds token information about an AST element.
/// Since the different pieces may be gathered in different locations,
/// the fields are gated behind accessor functions which also check some invariants every time they are called.
#[derive(PartialEq, Eq, Clone, Copy)]
pub struct TokenSpan {
    pub start_token: TokenId,
    pub end_token: TokenId,
}

impl Display for TokenSpan {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.start_token.0, self.end_token.0)
    }
}

impl Debug for TokenSpan {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

impl TokenSpan {
    pub fn new(start_token: TokenId, end_token: TokenId) -> Self {
        debug_assert!(
            start_token <= end_token,
            "start token {:} is past end token {}",
            start_token.0,
            end_token.0
        );
        Self {
            start_token,
            end_token,
        }
    }

    pub fn for_library() -> Self {
        Self {
            start_token: TokenId(0),
            end_token: TokenId(0),
        }
    }

    /// Skips the stream to the token given by the offset
    #[cfg(test)]
    pub fn skip_to(&self, offset: TokenId) -> TokenSpan {
        let new_token = TokenId(self.start_token.0 + offset.0);
        debug_assert!(
            new_token <= self.end_token,
            "[TokenSpan::skip_to] skipping past end of the span"
        );
        TokenSpan {
            start_token: new_token,
            end_token: self.end_token,
        }
    }

    pub fn pos(&self, ctx: &dyn TokenAccess) -> SrcPos {
        ctx.get_span(self.start_token, self.end_token)
    }

    pub(crate) fn combine(&self, other: impl Into<TokenSpan>) -> TokenSpan {
        let other = other.into();
        debug_assert!(self.start_token <= other.end_token);
        TokenSpan {
            start_token: self.start_token,
            end_token: other.end_token,
        }
    }

    pub(crate) fn end_with(&self, other: TokenId) -> TokenSpan {
        debug_assert!(self.start_token <= other);
        TokenSpan {
            start_token: self.start_token,
            end_token: other,
        }
    }

    pub(crate) fn start_with(&self, other: TokenId) -> TokenSpan {
        debug_assert!(other <= self.end_token);
        TokenSpan {
            start_token: other,
            end_token: self.end_token,
        }
    }
}

struct TokenSpanIterator {
    end: TokenId,
    current: TokenId,
}

impl Iterator for TokenSpanIterator {
    type Item = TokenId;

    fn next(&mut self) -> Option<Self::Item> {
        let old_current = self.current;
        // Note: in this example, current may point to an
        // invalid token (as it is greater than and).
        // This is OK because the invalid ID is never returned.
        if self.current > self.end {
            None
        } else {
            self.current += 1;
            Some(old_current)
        }
    }
}

#[test]
fn token_iterator() {
    let span = TokenSpan {
        start_token: TokenId(0),
        end_token: TokenId(0),
    };
    let mut itr = span.iter();
    assert_eq!(itr.next(), Some(TokenId(0)));
    assert_eq!(itr.next(), None);

    let span = TokenSpan {
        start_token: TokenId(0),
        end_token: TokenId(1),
    };
    let mut itr = span.iter();
    assert_eq!(itr.next(), Some(TokenId(0)));
    assert_eq!(itr.next(), Some(TokenId(1)));
    assert_eq!(itr.next(), None);
}

impl TokenSpan {
    pub fn iter(&self) -> impl Iterator<Item = TokenId> {
        TokenSpanIterator {
            current: self.start_token,
            end: self.end_token,
        }
    }

    pub fn len(&self) -> usize {
        self.end_token.0 - self.start_token.0 + 1
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

/// A type that conforms to `TokenAccess` can be indexed using a `TokenId`.
/// Convenience methods exist to directly get the `SrcPos` for a given `TokenId`
/// or a span starting at a certain token and ending at another.
///
/// Types such as `Vec` and `array` implement `TokenAccess`
pub trait TokenAccess {
    /// Get a token by its ID
    fn get_token(&self, id: TokenId) -> Option<&Token>;

    fn index(&self, id: TokenId) -> &Token;

    /// Get a slice of tokens by using a start ID and an end ID
    fn get_token_slice(&self, start_id: TokenId, end_id: TokenId) -> &[Token];

    /// Get a token's position by its ID
    fn get_pos(&self, id: TokenId) -> &SrcPos {
        &self.index(id).pos
    }

    /// Get a span where the beginning of that span is the beginning of the token indexed by
    /// `start_id` and the end is the end of the token indexed by `end_id`
    fn get_span(&self, start_id: TokenId, end_id: TokenId) -> SrcPos {
        self.get_pos(start_id).combine(self.get_pos(end_id))
    }
}

impl TokenAccess for Vec<Token> {
    fn get_token(&self, id: TokenId) -> Option<&Token> {
        self.get(id.0)
    }

    fn index(&self, id: TokenId) -> &Token {
        &self[id.0]
    }

    fn get_token_slice(&self, start_id: TokenId, end_id: TokenId) -> &[Token] {
        &self[start_id.0..end_id.0 + 1]
    }
}

impl TokenAccess for [Token] {
    fn get_token(&self, id: TokenId) -> Option<&Token> {
        self.get(id.0)
    }

    fn index(&self, id: TokenId) -> &Token {
        &self[id.0]
    }

    fn get_token_slice(&self, start_id: TokenId, end_id: TokenId) -> &[Token] {
        &self[start_id.0..end_id.0 + 1]
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct TokenComments {
    pub leading: Vec<Comment>,
    pub trailing: Option<Comment>,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Comment {
    pub value: String,
    pub range: crate::data::Range,
    pub multi_line: bool,
}

use crate::standard::VHDLStandard;
use std::convert::AsRef;
use std::fmt::{Debug, Display, Formatter};
use std::ops::{Add, AddAssign, Sub};
use strum::IntoStaticStr;

impl AsRef<SrcPos> for Token {
    fn as_ref(&self) -> &SrcPos {
        &self.pos
    }
}

impl From<Token> for SrcPos {
    fn from(token: Token) -> SrcPos {
        token.pos
    }
}

pub fn kinds_error<T: AsRef<SrcPos>>(pos: T, kinds: &[Kind]) -> Diagnostic {
    Diagnostic::syntax_error(
        pos.as_ref(),
        format!("Expected {}", kinds_str(kinds)).as_str(),
    )
}

impl Token {
    pub fn kinds_error(&self, kinds: &[Kind]) -> Diagnostic {
        kinds_error(&self.pos, kinds)
    }

    pub fn to_identifier_value(&self, id: TokenId) -> DiagnosticResult<Ident> {
        if let Token {
            kind: Identifier,
            value: Value::Identifier(value),
            ..
        } = self
        {
            Ok(WithToken::new(value.clone(), id))
        } else {
            Err(self.kinds_error(&[Identifier]))
        }
    }

    pub fn to_character_value(&self, id: TokenId) -> DiagnosticResult<WithToken<u8>> {
        if let Token {
            kind: Character,
            value: Value::Character(value),
            ..
        } = self
        {
            Ok(WithToken::new(*value, id))
        } else {
            Err(self.kinds_error(&[Character]))
        }
    }

    pub fn to_bit_string(&self, id: TokenId) -> DiagnosticResult<WithToken<ast::BitString>> {
        if let Token {
            kind: BitString,
            value: Value::BitString(_, value),
            ..
        } = self
        {
            Ok(WithToken::new(value.clone(), id))
        } else {
            Err(self.kinds_error(&[BitString]))
        }
    }

    pub fn to_abstract_literal(
        &self,
        id: TokenId,
    ) -> DiagnosticResult<WithToken<ast::AbstractLiteral>> {
        if let Token {
            kind: AbstractLiteral,
            value: Value::AbstractLiteral(_, value),
            ..
        } = self
        {
            Ok(WithToken::new(*value, id))
        } else {
            Err(self.kinds_error(&[AbstractLiteral]))
        }
    }

    pub fn to_string_value(&self, id: TokenId) -> DiagnosticResult<WithToken<Latin1String>> {
        if let Token {
            kind: StringLiteral,
            value: Value::String(value),
            ..
        } = self
        {
            Ok(WithToken::new(value.clone(), id))
        } else {
            Err(self.kinds_error(&[StringLiteral]))
        }
    }

    pub fn to_operator_symbol(&self, id: TokenId) -> DiagnosticResult<WithToken<Operator>> {
        let string = self.to_string_value(id)?;
        if let Some(op) = Operator::from_latin1(string.item) {
            Ok(WithToken::new(op, string.token))
        } else {
            Err(Diagnostic::syntax_error(
                &self.pos,
                "Invalid operator symbol",
            ))
        }
    }

    /// Returns the full range of this token, respecting any potential comments.
    /// Note that [Token::pos] only returns the position of the token itself.
    pub fn full_range(&self) -> crate::data::Range {
        let mut range = self.pos.range();
        if let Some(comments) = &self.comments {
            if let Some(comment) = comments.leading.first() {
                range.start = comment.range.start
            }
            if let Some(trailing) = &comments.trailing {
                range.end = trailing.range.end
            }
        }
        range
    }

    /// return `true` when `self` is equal to `other` while ignoring all
    /// changes that are attributed to their position in the source file
    /// and all changes that only affect comments.
    pub fn equal_format(&self, other: &Token) -> bool {
        self.kind == other.kind && self.value == other.value
    }
}

impl Operator {
    pub fn to_latin1(&self) -> Latin1String {
        Latin1String::from_vec(self.to_string().into_bytes())
    }

    pub fn from_latin1(mut latin1: Latin1String) -> Option<Self> {
        latin1.make_lowercase();
        Some(match latin1.bytes.as_slice() {
            b"and" => Operator::And,
            b"or" => Operator::Or,
            b"nand" => Operator::Nand,
            b"nor" => Operator::Nor,
            b"xor" => Operator::Xor,
            b"xnor" => Operator::Xnor,
            b"=" => Operator::EQ,
            b"/=" => Operator::NE,
            b"<" => Operator::LT,
            b"<=" => Operator::LTE,
            b">" => Operator::GT,
            b">=" => Operator::GTE,
            b"?=" => Operator::QueEQ,
            b"?/=" => Operator::QueNE,
            b"?<" => Operator::QueLT,
            b"?<=" => Operator::QueLTE,
            b"?>" => Operator::QueGT,
            b"?>=" => Operator::QueGTE,
            b"sll" => Operator::SLL,
            b"srl" => Operator::SRL,
            b"sla" => Operator::SLA,
            b"sra" => Operator::SRA,
            b"rol" => Operator::ROL,
            b"ror" => Operator::ROR,
            b"+" => Operator::Plus,
            b"-" => Operator::Minus,
            b"&" => Operator::Concat,
            b"*" => Operator::Times,
            b"/" => Operator::Div,
            b"mod" => Operator::Mod,
            b"rem" => Operator::Rem,
            b"**" => Operator::Pow,
            b"abs" => Operator::Abs,
            b"not" => Operator::Not,
            b"??" => Operator::QueQue,
            _ => {
                return None;
            }
        })
    }
}

struct TokenError {
    range: crate::data::Range,
    message: String,
}

impl TokenError {
    fn range(start: Position, end: Position, message: impl Into<String>) -> TokenError {
        TokenError {
            range: crate::data::Range::new(start, end),
            message: message.into(),
        }
    }

    fn pos(pos: Position, message: impl Into<String>) -> TokenError {
        Self::range(pos, pos.next_char(), message)
    }
}

impl From<Utf8ToLatin1Error> for TokenError {
    fn from(err: Utf8ToLatin1Error) -> TokenError {
        TokenError::range(err.pos, err.pos.after_char(err.value), err.message())
    }
}

/// Resolves ir1045
/// http://www.eda-stds.org/isac/IRs-VHDL-93/IR1045.txt
/// char may not come after ], ), all, or identifier
fn can_be_char(last_token_kind: Option<Kind>) -> bool {
    if let Some(kind) = last_token_kind {
        !matches!(kind, RightSquare | RightPar | All | Identifier)
    } else {
        true
    }
}

fn parse_integer(
    reader: &mut ContentReader<'_>,
    base: u64,
    stop_on_suffix: bool,
) -> Result<(u64, Latin1String), TokenError> {
    let mut result = Some(0_u64);
    let mut result_str = Latin1String::empty();
    let mut too_large_digit = None;
    let mut invalid_character = None;

    let start = reader.pos();
    while let Some(b) = reader.peek()? {
        let digit = u64::from(match b {
            // Bit string literal or exponent
            // Lower case
            b's' | b'u' | b'b' | b'o' | b'x' | b'd' | b'e' if stop_on_suffix => {
                break;
            }
            // Upper case
            b'S' | b'U' | b'B' | b'O' | b'X' | b'D' | b'E' if stop_on_suffix => {
                break;
            }

            b'0'..=b'9' => b - b'0',
            b'a'..=b'f' => 10 + b - b'a',
            b'A'..=b'F' => 10 + b - b'A',
            b'_' => {
                result_str.push(b);
                reader.skip();
                continue;
            }
            b'g'..=b'z' | b'G'..=b'Z' => {
                result_str.push(b);
                invalid_character = Some((b, reader.pos()));
                reader.skip();
                continue;
            }
            _ => {
                break;
            }
        });

        if digit >= base {
            too_large_digit = Some((b, reader.pos()));
        }

        result_str.push(b);
        reader.skip();

        result = result
            .and_then(|x| base.checked_mul(x))
            .and_then(|x| x.checked_add(digit));
    }

    if let Some((b, pos)) = invalid_character {
        Err(TokenError::pos(
            pos,
            format!("Invalid integer character '{}'", Latin1String::new(&[b])),
        ))
    } else if let Some((b, pos)) = too_large_digit {
        Err(TokenError::pos(
            pos,
            format!(
                "Illegal digit '{}' for base {}",
                Latin1String::new(&[b]),
                base
            ),
        ))
    } else if let Some(result) = result {
        Ok((result, result_str))
    } else {
        Err(TokenError::range(
            start,
            reader.pos(),
            "Integer too large for 64-bit unsigned",
        ))
    }
}

fn parse_exponent(reader: &mut ContentReader<'_>) -> Result<(i32, Latin1String), TokenError> {
    let start = reader.pos();
    let mut buffer = Latin1String::empty();
    let negative = {
        if reader.peek()? == Some(b'-') {
            buffer.push(b'-');
            reader.skip();
            true
        } else {
            if reader.skip_if(b'+')? {
                buffer.push(b'+');
            }
            false
        }
    };

    let (exp, mut exp_name) = parse_integer(reader, 10, false)?;
    buffer.append(&mut exp_name);
    if negative {
        if exp <= (-(i32::MIN as i64)) as u64 {
            return Ok(((-(exp as i64)) as i32, buffer));
        }
    } else if exp <= i32::MAX as u64 {
        return Ok((exp as i32, buffer));
    }

    Err(TokenError::range(
        start,
        reader.pos(),
        "Exponent too large for 32-bits signed",
    ))
}

#[derive(Clone, Copy)]
pub struct TokenState {
    last_token_kind: Option<Kind>,
    start: ReaderState,
}

impl TokenState {
    pub fn new(start: ReaderState) -> TokenState {
        TokenState {
            last_token_kind: None,
            start,
        }
    }
}

// Assumes first quote is already consumed
fn parse_quoted(
    buffer: &mut Latin1String,
    reader: &mut ContentReader<'_>,
    quote: u8,
    include_quote: bool,
) -> Result<Latin1String, TokenError> {
    let start = reader.pos();

    buffer.bytes.clear();
    let mut is_multiline = false;
    let mut found_end = false;

    if include_quote {
        buffer.bytes.push(quote)
    }

    while let Some(chr) = reader.pop()? {
        is_multiline |= chr == b'\n';
        if chr == quote {
            if reader.peek()? == Some(quote) {
                reader.skip();
            } else {
                found_end = true;
                break;
            }
        }
        buffer.bytes.push(chr);
    }

    if include_quote {
        buffer.bytes.push(quote)
    }

    if !found_end {
        Err(TokenError::range(
            start.prev_char(),
            reader.pos(),
            "Reached EOF before end quote",
        ))
    } else if is_multiline {
        Err(TokenError::range(
            start.prev_char(),
            reader.pos(),
            "Multi line string",
        ))
    } else {
        Ok(buffer.clone())
    }
}

// Assumes first quote is already consumed
fn parse_string(
    buffer: &mut Latin1String,
    reader: &mut ContentReader<'_>,
) -> Result<Latin1String, TokenError> {
    parse_quoted(buffer, reader, b'"', false)
}

/// Assume -- has already been consumed
fn parse_comment(reader: &mut ContentReader<'_>) -> Comment {
    let start_pos = reader.pos().prev_char().prev_char();
    let mut value = String::new();
    while let Some(chr) = reader.peek_char() {
        if chr == '\n' {
            break;
        } else {
            reader.skip();
            value.push(chr);
        }
    }
    let end_pos = reader.pos();
    Comment {
        value,
        range: start_pos.range_to(end_pos),
        multi_line: false,
    }
}

/// Assume /* has been consumed
fn parse_multi_line_comment(reader: &mut ContentReader<'_>) -> Result<Comment, TokenError> {
    let start_pos = reader.pos().prev_char().prev_char();
    let mut value = String::new();
    while let Some(chr) = reader.pop_char() {
        if chr == '*' {
            if reader.peek_char() == Some('/') {
                reader.skip();
                // Comment ended
                let end_pos = reader.pos();
                return Ok(Comment {
                    value,
                    range: start_pos.range_to(end_pos),
                    multi_line: true,
                });
            } else {
                value.push(chr);
            }
        } else {
            value.push(chr);
        }
    }

    let end_pos = reader.pos();
    Err(TokenError::range(
        start_pos,
        end_pos,
        "Incomplete multi-line comment",
    ))
}

fn parse_real_literal(
    buffer: &mut Latin1String,
    reader: &mut ContentReader<'_>,
) -> Result<(f64, Latin1String), TokenError> {
    buffer.clear();
    let mut text = Latin1String::empty();
    let start = reader.pos();
    while let Some(b) = reader.peek_lowercase()? {
        match b {
            b'e' => {
                break;
            }
            b'E' => {
                break;
            }
            b'0'..=b'9' | b'a'..=b'd' | b'f' | b'A'..=b'F' | b'.' => {
                text.push(b);
                reader.skip();
                buffer.push(b);
            }
            b'_' => {
                text.push(b);
                reader.skip();
                continue;
            }
            _ => {
                break;
            }
        };
    }

    let string = unsafe { std::str::from_utf8_unchecked(&buffer.bytes) };

    string
        .parse::<f64>()
        .map(|val| (val, text))
        .map_err(|err: std::num::ParseFloatError| {
            TokenError::range(start, reader.pos(), err.to_string())
        })
}

fn exponentiate(value: u64, exp: u32) -> Option<u64> {
    10_u64.checked_pow(exp).and_then(|x| x.checked_mul(value))
}

/// LRM 15.5 Abstract literals
fn parse_abstract_literal(
    buffer: &mut Latin1String,
    reader: &mut ContentReader<'_>,
) -> Result<(Kind, Value), TokenError> {
    let state = reader.state();
    let initial = parse_integer(reader, 10, true);
    let pos_after_initial = reader.pos();

    match reader.peek_lowercase()? {
        // Real
        Some(b'.') => {
            reader.set_state(state);
            let (real, mut text) = parse_real_literal(buffer, reader)?;

            match reader.peek()? {
                // Exponent
                Some(b'e') | Some(b'E') => {
                    text.push(reader.peek().unwrap().unwrap());
                    reader.skip();
                    let (exp, mut exp_text) = parse_exponent(reader)?;
                    text.append(&mut exp_text);
                    Ok((
                        AbstractLiteral,
                        Value::AbstractLiteral(
                            text,
                            ast::AbstractLiteral::Real(real * 10_f64.powi(exp)),
                        ),
                    ))
                }
                _ => Ok((
                    AbstractLiteral,
                    Value::AbstractLiteral(text, ast::AbstractLiteral::Real(real)),
                )),
            }
        }

        // Integer exponent
        Some(b'e') => {
            let mut text = Latin1String::empty();
            let (integer, mut int_text) = initial?;
            text.append(&mut int_text);
            text.push(reader.peek().unwrap().unwrap());
            reader.skip();
            let (exp, mut exp_text) = parse_exponent(reader)?;
            text.append(&mut exp_text);
            if exp >= 0 {
                if let Some(value) = exponentiate(integer, exp as u32) {
                    Ok((
                        AbstractLiteral,
                        Value::AbstractLiteral(text, ast::AbstractLiteral::Integer(value)),
                    ))
                } else {
                    Err(TokenError::range(
                        state.pos(),
                        reader.pos(),
                        "Integer too large for 64-bit unsigned",
                    ))
                }
            } else {
                Err(TokenError::range(
                    state.pos(),
                    reader.pos(),
                    "Integer literals may not have negative exponent",
                ))
            }
        }

        // Based integer
        Some(b'#') => {
            let (base, mut base_text) = initial?;
            base_text.push(b'#');
            reader.skip();
            let base_result = parse_integer(reader, base, false);

            if let Some(b'#') = reader.peek()? {
                reader.skip();
                let (integer, mut int_text) = base_result?;
                base_text.append(&mut int_text);
                base_text.push(b'#');
                if (2..=16).contains(&base) {
                    Ok((
                        AbstractLiteral,
                        Value::AbstractLiteral(base_text, ast::AbstractLiteral::Integer(integer)),
                    ))
                } else {
                    Err(TokenError::range(
                        state.pos(),
                        pos_after_initial,
                        format!("Base must be at least 2 and at most 16, got {base}"),
                    ))
                }
            } else {
                Err(TokenError::range(
                    state.pos(),
                    reader.pos(),
                    "Based integer did not end with #",
                ))
            }
        }

        // Bit string literal
        Some(b's') | Some(b'u') | Some(b'b') | Some(b'o') | Some(b'x') | Some(b'd') => {
            let (integer, _) = initial?;

            if let Some(base_spec) = parse_base_specifier(reader)? {
                parse_bit_string(
                    buffer,
                    reader,
                    base_spec,
                    Some(integer as u32),
                    state.pos().character as usize,
                )
            } else {
                Err(TokenError::range(
                    state.pos(),
                    reader.pos(),
                    "Invalid bit string literal",
                ))
            }
        }
        _ => {
            // Plain integer
            let (integer, integer_text) = initial?;
            Ok((
                AbstractLiteral,
                Value::AbstractLiteral(integer_text, ast::AbstractLiteral::Integer(integer)),
            ))
        }
    }
}

/// LRM 15.8 Bit string literals
/// Parse the base specifier such as ub, sx, b etc
/// Also requires and consumes the trailing quote "
fn parse_base_specifier(
    reader: &mut ContentReader<'_>,
) -> Result<Option<BaseSpecifier>, TokenError> {
    let base_specifier = match reader.pop_lowercase()? {
        Some(b'u') => match reader.pop_lowercase()? {
            Some(b'b') => BaseSpecifier::UB,
            Some(b'o') => BaseSpecifier::UO,
            Some(b'x') => BaseSpecifier::UX,
            _ => return Ok(None),
        },
        Some(b's') => match reader.pop_lowercase()? {
            Some(b'b') => BaseSpecifier::SB,
            Some(b'o') => BaseSpecifier::SO,
            Some(b'x') => BaseSpecifier::SX,
            _ => return Ok(None),
        },
        Some(b'b') => BaseSpecifier::B,
        Some(b'o') => BaseSpecifier::O,
        Some(b'x') => BaseSpecifier::X,
        Some(b'd') => BaseSpecifier::D,
        _ => return Ok(None),
    };

    Ok(if reader.pop()? == Some(b'"') {
        Some(base_specifier)
    } else {
        None
    })
}

// Only consume reader if it is a base specifier
fn maybe_base_specifier(
    reader: &mut ContentReader<'_>,
) -> Result<Option<BaseSpecifier>, TokenError> {
    let mut lookahead = reader.clone();
    if let Some(value) = parse_base_specifier(&mut lookahead)? {
        reader.set_to(&lookahead);
        Ok(Some(value))
    } else {
        Ok(None)
    }
}

fn parse_bit_string(
    buffer: &mut Latin1String,
    reader: &mut ContentReader<'_>,
    base_specifier: BaseSpecifier,
    bit_string_length: Option<u32>,
    start: usize,
) -> Result<(Kind, Value), TokenError> {
    let value = match parse_string(buffer, reader) {
        Ok(value) => value,
        Err(mut err) => {
            err.message = "Invalid bit string literal".to_string();
            return Err(err);
        }
    };

    let end_pos = reader.state().pos();
    let actual_value = reader
        .value_at(end_pos.line as usize, start, end_pos.character as usize)
        .unwrap();

    Ok((
        BitString,
        Value::BitString(
            actual_value,
            ast::BitString {
                length: bit_string_length,
                base: base_specifier,
                value,
            },
        ),
    ))
}

/// LRM 15.4 Identifiers
fn parse_basic_identifier_or_keyword(
    buffer: &mut Latin1String,
    reader: &mut ContentReader<'_>,
    symbols: &Symbols,
) -> Result<(Kind, Value), TokenError> {
    buffer.bytes.clear();
    while let Some(b) = reader.peek()? {
        match b {
            b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_' => {
                buffer.bytes.push(b);
                reader.skip();
            }
            _ => {
                break;
            }
        }
    }

    Ok(symbols.insert_or_keyword(buffer))
}

/// Assumes leading ' has already been consumed
/// Only consumes from reader if Some is returned
fn parse_character_literal(
    reader: &mut ContentReader<'_>,
) -> Result<Option<(Kind, Value)>, TokenError> {
    let mut lookahead = reader.clone();

    if let Some(chr) = lookahead.pop()? {
        if lookahead.skip_if(b'\'')? {
            reader.set_to(&lookahead);
            Ok(Some((Character, Value::Character(chr))))
        } else {
            Ok(None)
        }
    } else {
        Ok(None)
    }
}

/// Reads into `buffer` until a newline character is observed.
/// Does not consume the newline character.
///
/// Clears the buffer prior to reading
fn read_until_newline(
    buffer: &mut Latin1String,
    reader: &mut ContentReader<'_>,
) -> Result<(), TokenError> {
    buffer.bytes.clear();
    while let Some(b) = reader.peek()? {
        if b == b'\n' {
            break;
        }
        buffer.bytes.push(b);
        reader.skip();
    }
    Ok(())
}

fn get_leading_comments(reader: &mut ContentReader<'_>) -> Result<Vec<Comment>, TokenError> {
    let mut comments: Vec<Comment> = Vec::new();

    loop {
        skip_whitespace(reader);
        let state = reader.state();

        let byte = if let Some(byte) = reader.pop()? {
            byte
        } else {
            break;
        };

        match byte {
            b'/' => {
                if reader.pop()? == Some(b'*') {
                    comments.push(parse_multi_line_comment(reader)?);
                } else {
                    reader.set_state(state);
                    break;
                }
            }
            b'-' => {
                if reader.pop()? == Some(b'-') {
                    comments.push(parse_comment(reader));
                } else {
                    reader.set_state(state);
                    break;
                }
            }
            _ => {
                reader.set_state(state);
                break;
            }
        }
    }

    Ok(comments)
}

/// Skip whitespace but not newline
fn skip_whitespace_in_line(reader: &mut ContentReader<'_>) {
    while let Ok(Some(byte)) = reader.peek() {
        match byte {
            b' ' | b'\t' => {
                reader.skip();
            }
            _ => {
                break;
            }
        }
    }
}

/// Skip all whitespace
fn skip_whitespace(reader: &mut ContentReader<'_>) {
    while let Ok(Some(byte)) = reader.peek() {
        match byte {
            b' ' | b'\t' | b'\n' => {
                reader.skip();
            }
            _ => {
                break;
            }
        }
    }
}

fn get_trailing_comment(reader: &mut ContentReader<'_>) -> Result<Option<Comment>, TokenError> {
    skip_whitespace_in_line(reader);
    let state = reader.state();

    match reader.pop()? {
        Some(b'-') => {
            if reader.pop()? == Some(b'-') {
                Ok(Some(parse_comment(reader)))
            } else {
                reader.set_state(state);
                Ok(None)
            }
        }
        _ => {
            reader.set_state(state);
            Ok(None)
        }
    }
}

/// Static tokenizer data
pub struct Symbols {
    symtab: SymbolTable,
    keywords: Vec<Kind>,
    attributes: FnvHashMap<Symbol, AttributeDesignator>,
}

impl Symbols {
    pub fn symtab(&self) -> &SymbolTable {
        &self.symtab
    }

    fn insert_or_keyword(&self, name: &Latin1String) -> (Kind, Value) {
        let symbol = self.symtab.insert(name);
        if let Some(kind) = self.keywords.get(symbol.id) {
            (*kind, Value::None)
        } else {
            (Identifier, Value::Identifier(symbol))
        }
    }

    pub fn from_standard(version: VHDLStandard) -> Symbols {
        let symtab = SymbolTable::default();
        let kw = version.keywords();
        let mut keywords = Vec::with_capacity(kw.len());

        let mut latin1 = Latin1String::empty();
        for kind in kw {
            latin1.bytes.clear();
            latin1.bytes.extend_from_slice(kind.as_str().as_bytes());
            let symbol = symtab.insert(&latin1);
            assert_eq!(symbol.id, keywords.len());
            keywords.push(*kind);
        }

        let attributes = version
            .builtin_attributes()
            .iter()
            .map(|attr| (symtab.insert_utf8(format!("{attr}").as_str()), attr.clone()))
            .collect();

        Symbols {
            symtab,
            keywords,
            attributes,
        }
    }
}

impl std::default::Default for Symbols {
    fn default() -> Symbols {
        Self::from_standard(VHDLStandard::default())
    }
}

pub struct Tokenizer<'a> {
    symbols: &'a Symbols,
    buffer: Latin1String,
    state: TokenState,
    pub source: &'a Source,
    reader: ContentReader<'a>,
    final_comments: Option<Vec<Comment>>,
}

impl<'a> Tokenizer<'a> {
    pub fn new(
        symbols: &'a Symbols,
        source: &'a Source,
        reader: ContentReader<'a>,
    ) -> Tokenizer<'a> {
        Tokenizer {
            symbols,
            state: TokenState::new(reader.state()),
            buffer: Latin1String::empty(),
            source,
            reader,
            final_comments: None,
        }
    }

    pub fn attribute(&self, sym: Symbol) -> AttributeDesignator {
        self.symbols
            .attributes
            .get(&sym)
            .cloned()
            .unwrap_or_else(|| AttributeDesignator::Ident(WithRef::new(sym)))
    }

    fn parse_token(&mut self) -> Result<Option<(Kind, Value)>, TokenError> {
        macro_rules! illegal_token {
            () => {
                return Err(TokenError::range(
                    self.state.start.pos(),
                    self.reader.pos(),
                    "Illegal token",
                ));
            };
        }

        let byte = if let Some(byte) = self.reader.peek()? {
            byte
        } else {
            // End of file
            return Ok(None);
        };

        let (kind, value) = match byte {
            b'a'..=b'z' | b'A'..=b'Z' => {
                let state = self.reader.state();
                if let Some(base_spec) = maybe_base_specifier(&mut self.reader)? {
                    parse_bit_string(
                        &mut self.buffer,
                        &mut self.reader,
                        base_spec,
                        None,
                        state.pos().character as usize,
                    )?
                } else {
                    parse_basic_identifier_or_keyword(
                        &mut self.buffer,
                        &mut self.reader,
                        self.symbols,
                    )?
                }
            }
            b'0'..=b'9' => parse_abstract_literal(&mut self.buffer, &mut self.reader)?,
            b':' => {
                self.reader.skip();
                if self.reader.skip_if(b'=')? {
                    (ColonEq, Value::None)
                } else {
                    (Colon, Value::None)
                }
            }
            b'\'' => {
                self.reader.skip();
                if can_be_char(self.state.last_token_kind) {
                    if let Some(chr_lit) = parse_character_literal(&mut self.reader)? {
                        chr_lit
                    } else {
                        (Tick, Value::None)
                    }
                } else {
                    (Tick, Value::None)
                }
            }
            b'-' => {
                self.reader.skip();
                (Minus, Value::None)
            }
            b'"' => {
                self.reader.skip();
                let result = parse_string(&mut self.buffer, &mut self.reader)?;
                (StringLiteral, Value::String(result))
            }
            b';' => {
                self.reader.skip();
                (SemiColon, Value::None)
            }
            b'(' => {
                self.reader.skip();
                (LeftPar, Value::None)
            }
            b')' => {
                self.reader.skip();
                (RightPar, Value::None)
            }
            b'+' => {
                self.reader.skip();
                (Plus, Value::None)
            }
            b'.' => {
                self.reader.skip();
                (Dot, Value::None)
            }
            b'&' => {
                self.reader.skip();
                (Concat, Value::None)
            }
            b',' => {
                self.reader.skip();
                (Comma, Value::None)
            }
            b'=' => {
                self.reader.skip();
                if self.reader.skip_if(b'>')? {
                    (RightArrow, Value::None)
                } else {
                    (EQ, Value::None)
                }
            }
            b'<' => {
                self.reader.skip();
                match self.reader.peek()? {
                    Some(b'=') => {
                        self.reader.skip();
                        (LTE, Value::None)
                    }
                    Some(b'>') => {
                        self.reader.skip();
                        (BOX, Value::None)
                    }
                    Some(b'<') => {
                        self.reader.skip();
                        (LtLt, Value::None)
                    }
                    _ => (LT, Value::None),
                }
            }
            b'>' => {
                self.reader.skip();
                match self.reader.peek()? {
                    Some(b'=') => {
                        self.reader.skip();
                        (GTE, Value::None)
                    }
                    Some(b'>') => {
                        self.reader.skip();
                        (GtGt, Value::None)
                    }
                    _ => (GT, Value::None),
                }
            }
            b'/' => {
                self.reader.skip();

                if self.reader.skip_if(b'=')? {
                    (NE, Value::None)
                } else {
                    (Div, Value::None)
                }
            }
            b'*' => {
                self.reader.skip();

                if self.reader.skip_if(b'*')? {
                    (Pow, Value::None)
                } else {
                    (Times, Value::None)
                }
            }
            b'?' => {
                self.reader.skip();
                match self.reader.peek()? {
                    Some(b'?') => {
                        self.reader.skip();
                        (QueQue, Value::None)
                    }
                    Some(b'=') => {
                        self.reader.skip();
                        (QueEQ, Value::None)
                    }
                    Some(b'/') => {
                        self.reader.skip();
                        if self.reader.skip_if(b'=')? {
                            (QueNE, Value::None)
                        } else {
                            illegal_token!();
                        }
                    }
                    Some(b'<') => {
                        self.reader.skip();
                        if self.reader.skip_if(b'=')? {
                            (QueLTE, Value::None)
                        } else {
                            (QueLT, Value::None)
                        }
                    }
                    Some(b'>') => {
                        self.reader.skip();
                        if self.reader.skip_if(b'=')? {
                            (QueGTE, Value::None)
                        } else {
                            (QueGT, Value::None)
                        }
                    }
                    _ => (Que, Value::None),
                }
            }
            b'^' => {
                self.reader.skip();
                (Circ, Value::None)
            }
            b'@' => {
                self.reader.skip();
                (CommAt, Value::None)
            }
            b'|' => {
                self.reader.skip();
                (Bar, Value::None)
            }
            b'[' => {
                self.reader.skip();
                (LeftSquare, Value::None)
            }
            b']' => {
                self.reader.skip();
                (RightSquare, Value::None)
            }
            b'\\' => {
                self.reader.skip();
                // LRM 15.4.3 Extended identifers
                let result = parse_quoted(&mut self.buffer, &mut self.reader, b'\\', true)?;
                let result = Value::Identifier(self.symbols.symtab().insert_extended(&result));
                (Identifier, result)
            }
            b'`' => {
                self.reader.skip();
                (GraveAccent, Value::None)
            }
            _ => {
                self.reader.skip();
                illegal_token!();
            }
        };
        Ok(Some((kind, value)))
    }

    fn pop_raw(&mut self) -> Result<Option<Token>, TokenError> {
        let leading_comments = get_leading_comments(&mut self.reader)?;
        self.state.start = self.reader.state();

        match self.parse_token()? {
            Some((kind, value)) => {
                // Parsed a token.
                let pos_start = self.state.start.pos();
                let pos_end = self.reader.pos();
                let trailing_comment = get_trailing_comment(&mut self.reader)?;
                let token_comments = if (!leading_comments.is_empty()) | trailing_comment.is_some()
                {
                    Some(Box::new(TokenComments {
                        leading: leading_comments,
                        trailing: trailing_comment,
                    }))
                } else {
                    None
                };
                let token = Token {
                    kind,
                    value,
                    pos: self.source.pos(pos_start, pos_end),
                    comments: token_comments,
                };
                self.state.last_token_kind = Some(token.kind);
                Ok(Some(token))
            }
            None => {
                // End of file.
                self.final_comments = Some(leading_comments);
                Ok(None)
            }
        }
    }

    pub fn pop(&mut self) -> DiagnosticResult<Option<Token>> {
        match self.pop_raw() {
            Ok(token) => Ok(token),
            Err(err) => {
                self.state.start = self.reader.state();
                Err(Diagnostic::syntax_error(
                    self.source.pos(err.range.start, err.range.end),
                    err.message,
                ))
            }
        }
    }

    #[allow(dead_code)]
    pub fn get_final_comments(&self) -> Option<Vec<Comment>> {
        self.final_comments.clone()
    }

    pub fn text_until_newline(&mut self) -> DiagnosticResult<Token> {
        let start_pos = self.reader.pos();
        if let Err(err) = read_until_newline(&mut self.buffer, &mut self.reader) {
            self.state.start = self.reader.state();
            return Err(Diagnostic::syntax_error(
                self.source.pos(err.range.start, err.range.end),
                err.message,
            ));
        }
        let text = self.buffer.clone();
        let end_pos = self.reader.pos();
        Ok(Token {
            kind: Text,
            value: Value::Text(text),
            pos: self.source.pos(start_pos, end_pos),
            comments: None,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::test::Code;
    use itertools::Itertools;
    use pretty_assertions::assert_eq;

    fn kinds(tokens: &[Token]) -> Vec<Kind> {
        tokens.iter().map(|tok| tok.kind).collect()
    }

    // Shorthand for testing
    fn kinds_tokenize(code: &str) -> Vec<Kind> {
        kinds(&Code::new(code).tokenize())
    }

    // Shorthand for testing
    fn kind_value_tokenize(code: &str) -> Vec<(Kind, Value)> {
        Code::new(code)
            .tokenize()
            .iter()
            .map(|tok| (tok.kind, tok.value.clone()))
            .collect()
    }

    #[test]
    fn tokenize_keywords() {
        assert_eq!(kinds_tokenize("architecture"), vec![Architecture]);
        assert_eq!(kinds_tokenize("entity"), vec![Entity]);
        assert_eq!(kinds_tokenize("is"), vec![Is]);
        assert_eq!(kinds_tokenize("generic"), vec![Generic]);
        assert_eq!(kinds_tokenize("port"), vec![Port]);
        assert_eq!(kinds_tokenize("begin"), vec![Begin]);
        assert_eq!(kinds_tokenize("end"), vec![End]);
        assert_eq!(kinds_tokenize("all"), vec![All]);
        assert_eq!(kinds_tokenize("abs"), vec![Abs]);
        assert_eq!(kinds_tokenize("not"), vec![Not]);
    }

    #[test]
    fn tokenize_newline() {
        assert_eq!(
            kinds_tokenize(
                "
entity is
end entity"
            ),
            vec![Entity, Is, End, Entity]
        );
    }

    #[test]
    fn tokenize_pos() {
        let code = Code::new("entity foo");
        let tokens = code.tokenize();

        assert_eq!(
            tokens[0],
            Token {
                kind: Entity,
                value: Value::None,
                pos: code.s1("entity").pos(),
                comments: None,
            }
        );

        assert_eq!(
            tokens[1],
            Token {
                kind: Identifier,
                value: Value::Identifier(code.symbol("foo")),
                pos: code.s1("foo").pos(),
                comments: None,
            }
        );
    }

    #[test]
    fn tokenize_keywords_case_insensitive() {
        assert_eq!(kinds_tokenize("entity"), vec![Entity]);
        assert_eq!(kinds_tokenize("Entity"), vec![Entity]);
        assert_eq!(kinds_tokenize("arCHitecture"), vec![Architecture]);
    }

    #[test]
    fn tokenize_identifier() {
        let code = Code::new("my_ident");
        let tokens = code.tokenize();

        assert_eq!(
            tokens,
            vec![Token {
                kind: Identifier,
                value: Value::Identifier(code.symbol("my_ident")),
                pos: code.pos(),
                comments: None,
            }]
        );
    }

    #[test]
    fn tokenize_identifier_case_insensitive() {
        let code = Code::new("My_Ident");
        let tokens = code.tokenize();

        assert_eq!(
            tokens,
            vec![Token {
                kind: Identifier,
                value: Value::Identifier(code.symbol("my_ident")),
                pos: code.pos(),
                comments: None,
            }]
        );
    }

    #[test]
    fn tokenize_extended_identifier() {
        let code = Code::new("\\1$my_ident\\");
        let tokens = code.tokenize();

        assert_eq!(
            tokens,
            vec![Token {
                kind: Identifier,
                value: Value::Identifier(code.symbol("\\1$my_ident\\")),
                pos: code.pos(),
                comments: None,
            }]
        );
        let code = Code::new("\\my\\\\_ident\\");
        let tokens = code.tokenize();
        assert_eq!(
            tokens,
            vec![Token {
                kind: Identifier,
                value: Value::Identifier(code.symbol("\\my\\_ident\\")),
                pos: code.pos(),
                comments: None,
            }]
        );
    }

    #[test]
    fn tokenize_many_identifiers() {
        let code = Code::new(
            "my_ident

my_other_ident",
        );
        let tokens = code.tokenize();
        assert_eq!(
            tokens,
            vec![
                Token {
                    kind: Identifier,
                    value: Value::Identifier(code.symbol("my_ident")),
                    pos: code.s1("my_ident").pos(),
                    comments: None,
                },
                Token {
                    kind: Identifier,
                    value: Value::Identifier(code.symbol("my_other_ident")),
                    pos: code.s1("my_other_ident").pos(),
                    comments: None,
                },
            ]
        );
    }

    #[test]
    fn tokenize_integer() {
        assert_eq!(
            kind_value_tokenize("100 -123 1_6_2 1e3 2E4"),
            vec![
                (
                    AbstractLiteral,
                    Value::AbstractLiteral(
                        Latin1String::new(b"100"),
                        ast::AbstractLiteral::Integer(100)
                    )
                ),
                (Minus, Value::None),
                (
                    AbstractLiteral,
                    Value::AbstractLiteral(
                        Latin1String::new(b"123"),
                        ast::AbstractLiteral::Integer(123)
                    )
                ),
                (
                    AbstractLiteral,
                    Value::AbstractLiteral(
                        Latin1String::new(b"1_6_2"),
                        ast::AbstractLiteral::Integer(162)
                    )
                ),
                (
                    AbstractLiteral,
                    Value::AbstractLiteral(
                        Latin1String::new(b"1e3"),
                        ast::AbstractLiteral::Integer(1000)
                    )
                ),
                (
                    AbstractLiteral,
                    Value::AbstractLiteral(
                        Latin1String::new(b"2E4"),
                        ast::AbstractLiteral::Integer(20000)
                    )
                ),
            ]
        );
    }

    #[test]
    fn tokenize_non_latin1_error() {
        // Euro takes 1 utf-16 code and Bomb emojii requires 2 utf-16 codes
        let code = Code::new("€\u{1F4A3}");
        let (tokens, _) = code.tokenize_result();

        assert_eq!(
            tokens,
            vec![
                Err(Diagnostic::syntax_error(
                    code.s1("€"),
                    "Found invalid latin-1 character '€'",
                )),
                Err(Diagnostic::syntax_error(
                    code.s1("\u{1F4A3}"),
                    "Found invalid latin-1 character '\u{1F4A3}'",
                )),
            ]
        );
    }

    #[test]
    fn tokenize_integer_negative_exponent() {
        let code = Code::new("1e-1");
        let (tokens, _) = code.tokenize_result();

        assert_eq!(
            tokens,
            vec![Err(Diagnostic::syntax_error(
                code.pos(),
                "Integer literals may not have negative exponent",
            ))]
        );
    }

    #[test]
    fn tokenize_real() {
        assert_eq!(
            kind_value_tokenize("0.1 -2_2.3_3 2.0e3 3.33E2 2.1e-2 4.4e+1 2.5E+3"),
            vec![
                (
                    AbstractLiteral,
                    Value::AbstractLiteral(
                        Latin1String::new(b"0.1"),
                        ast::AbstractLiteral::Real(0.1)
                    )
                ),
                (Minus, Value::None),
                (
                    AbstractLiteral,
                    Value::AbstractLiteral(
                        Latin1String::new(b"2_2.3_3"),
                        ast::AbstractLiteral::Real(22.33)
                    )
                ),
                (
                    AbstractLiteral,
                    Value::AbstractLiteral(
                        Latin1String::new(b"2.0e3"),
                        ast::AbstractLiteral::Real(2000.0)
                    )
                ),
                (
                    AbstractLiteral,
                    Value::AbstractLiteral(
                        Latin1String::new(b"3.33E2"),
                        ast::AbstractLiteral::Real(333.0)
                    )
                ),
                (
                    AbstractLiteral,
                    Value::AbstractLiteral(
                        Latin1String::new(b"2.1e-2"),
                        ast::AbstractLiteral::Real(0.021)
                    )
                ),
                (
                    AbstractLiteral,
                    Value::AbstractLiteral(
                        Latin1String::new(b"4.4e+1"),
                        ast::AbstractLiteral::Real(44.0)
                    )
                ),
                (
                    AbstractLiteral,
                    Value::AbstractLiteral(
                        Latin1String::new(b"2.5E+3"),
                        ast::AbstractLiteral::Real(2500.0)
                    )
                ),
            ]
        );
    }

    #[test]
    fn tokenize_real_many_fractional_digits() {
        assert_eq!(
            kind_value_tokenize("0.1000_0000_0000_0000_0000_0000_0000_0000"),
            vec![(
                AbstractLiteral,
                Value::AbstractLiteral(
                    Latin1String::new(b"0.1000_0000_0000_0000_0000_0000_0000_0000"),
                    ast::AbstractLiteral::Real(1e-1)
                )
            )]
        );
    }

    #[test]
    fn tokenize_real_many_integer_digits() {
        assert_eq!(
            kind_value_tokenize("1000_0000_0000_0000_0000_0000_0000_0000.0"),
            vec![(
                AbstractLiteral,
                Value::AbstractLiteral(
                    Latin1String::new(b"1000_0000_0000_0000_0000_0000_0000_0000.0"),
                    ast::AbstractLiteral::Real(1e31)
                )
            )]
        );
    }

    #[test]
    #[allow(clippy::approx_constant)]
    fn tokenize_real_truncates_precision() {
        assert_eq!(
            kind_value_tokenize("2.71828182845904523536"),
            vec![(
                AbstractLiteral,
                Value::AbstractLiteral(
                    Latin1String::new(b"2.71828182845904523536"),
                    ast::AbstractLiteral::Real(2.718_281_828_459_045)
                )
            )]
        );
    }

    #[test]
    fn tokenize_string_literal() {
        let code = Code::new("\"string\"");
        let tokens = code.tokenize();
        assert_eq!(
            tokens,
            vec![Token {
                kind: StringLiteral,
                value: Value::String(Latin1String::from_utf8_unchecked("string")),
                pos: code.pos(),
                comments: None,
            },]
        );
    }

    #[test]
    fn tokenize_string_literal_quote() {
        let code = Code::new("\"str\"\"ing\"");
        let tokens = code.tokenize();
        assert_eq!(
            tokens,
            vec![Token {
                kind: StringLiteral,
                value: Value::String(Latin1String::from_utf8_unchecked("str\"ing")),
                pos: code.pos(),
                comments: None,
            },]
        );
    }

    #[test]
    fn tokenize_string_literal_quote_separated() {
        let code = Code::new("\"str\" \"ing\"");
        let tokens = code.tokenize();
        assert_eq!(
            tokens,
            vec![
                Token {
                    kind: StringLiteral,
                    value: Value::String(Latin1String::from_utf8_unchecked("str")),
                    pos: code.s1("\"str\"").pos(),
                    comments: None,
                },
                Token {
                    kind: StringLiteral,
                    value: Value::String(Latin1String::from_utf8_unchecked("ing")),
                    pos: code.s1("\"ing\"").pos(),
                    comments: None,
                },
            ]
        );
    }

    #[test]
    fn tokenize_string_literal_error_on_multiline() {
        // Multiline is illegal
        let code = Code::new("\"str\ning\"");
        let (tokens, _) = code.tokenize_result();
        assert_eq!(
            tokens,
            vec![Err(Diagnostic::syntax_error(
                code.pos(),
                "Multi line string"
            ))]
        );
    }

    #[test]
    fn tokenize_string_literal_error_on_early_eof() {
        // End of file
        let code = Code::new("\"string");
        let (tokens, _) = code.tokenize_result();
        assert_eq!(
            tokens,
            vec![Err(Diagnostic::syntax_error(
                code.pos(),
                "Reached EOF before end quote",
            ))]
        );
    }

    #[test]
    fn tokenize_bit_string_literal() {
        use BaseSpecifier::{B, D, O, SB, SO, SX, UB, UO, UX, X};

        // Test all base specifiers
        for &base in [B, O, X, D, SB, SO, SX, UB, UO, UX].iter() {
            let (base_spec, value, length) = match base {
                B => ("b", "10", 2),
                O => ("o", "76543210", 8 * 3),
                X => ("x", "fedcba987654321", 16 * 4),
                D => ("d", "9876543210", 34),
                SB => ("sb", "10", 2),
                SO => ("so", "76543210", 8 * 3),
                SX => ("sx", "fedcba987654321", 16 * 4),
                UB => ("ub", "10", 2),
                UO => ("uo", "76543210", 8 * 3),
                UX => ("ux", "fedcba987654321", 16 * 4),
            };

            // Test with upper and lower case base specifier
            for &upper_case in [true, false].iter() {
                // Test with and without length prefix
                for &use_length in [true, false].iter() {
                    let (length_str, length_opt) = if use_length {
                        (length.to_string(), Some(length))
                    } else {
                        ("".to_owned(), None)
                    };

                    let mut code = format!("{length_str}{base_spec}\"{value}\"");

                    if upper_case {
                        code.make_ascii_uppercase()
                    }

                    let value = if upper_case {
                        value.to_ascii_uppercase()
                    } else {
                        value.to_owned()
                    };

                    let original_code = code.clone();

                    let code = Code::new(code.as_str());
                    let tokens = code.tokenize();
                    assert_eq!(
                        tokens,
                        vec![Token {
                            kind: BitString,
                            value: Value::BitString(
                                Latin1String::from_utf8_unchecked(original_code.as_str()),
                                ast::BitString {
                                    length: length_opt,
                                    base,
                                    value: Latin1String::from_utf8_unchecked(value.as_str()),
                                }
                            ),
                            pos: code.pos(),
                            comments: None,
                        },]
                    );
                }
            }
        }
    }

    #[test]
    fn tokenize_illegal_bit_string() {
        let code = Code::new("10x");
        let (tokens, _) = code.tokenize_result();
        assert_eq!(
            tokens,
            vec![Err(Diagnostic::syntax_error(
                code.pos(),
                "Invalid bit string literal",
            ))]
        );

        let code = Code::new("10ux");
        let (tokens, _) = code.tokenize_result();
        assert_eq!(
            tokens,
            vec![Err(Diagnostic::syntax_error(
                code.pos(),
                "Invalid bit string literal",
            ))]
        );
    }

    #[test]
    fn tokenize_based_integer() {
        assert_eq!(
            kind_value_tokenize("2#101#"),
            vec![(
                AbstractLiteral,
                Value::AbstractLiteral(
                    Latin1String::new(b"2#101#"),
                    ast::AbstractLiteral::Integer(5)
                )
            ),]
        );
        assert_eq!(
            kind_value_tokenize("8#321#"),
            vec![(
                AbstractLiteral,
                Value::AbstractLiteral(
                    Latin1String::new(b"8#321#"),
                    ast::AbstractLiteral::Integer(3 * 8 * 8 + 2 * 8 + 1)
                )
            ),]
        );
        assert_eq!(
            kind_value_tokenize("16#eEFfa#"),
            vec![(
                AbstractLiteral,
                Value::AbstractLiteral(
                    Latin1String::new(b"16#eEFfa#"),
                    ast::AbstractLiteral::Integer(0xeeffa)
                )
            ),]
        );
    }

    #[test]
    fn tokenize_illegal_integer() {
        let code = Code::new("100k");

        let (tokens, _) = code.tokenize_result();
        assert_eq!(
            tokens,
            vec![Err(Diagnostic::syntax_error(
                code.s1("k"),
                "Invalid integer character 'k'",
            ))]
        );
    }

    #[test]
    fn tokenize_illegal_based_integer() {
        // May not use digit larger than or equal base
        let code = Code::new("3#3#");
        let (tokens, _) = code.tokenize_result();
        println!("{:?}", tokens);
        assert_eq!(
            tokens,
            vec![Err(Diagnostic::syntax_error(
                code.s("3", 2),
                "Illegal digit '3' for base 3",
            ))]
        );
        // Base may only be 2-16
        let code = Code::new("1#0#");
        let (tokens, _) = code.tokenize_result();
        assert_eq!(
            tokens,
            vec![Err(Diagnostic::syntax_error(
                code.s1("1"),
                "Base must be at least 2 and at most 16, got 1",
            ))]
        );
        let code = Code::new("17#f#");
        let (tokens, _) = code.tokenize_result();
        assert_eq!(
            tokens,
            vec![Err(Diagnostic::syntax_error(
                code.s1("17"),
                "Base must be at least 2 and at most 16, got 17",
            ))]
        );
        let code = Code::new("15#f#");
        let (tokens, _) = code.tokenize_result();
        assert_eq!(
            tokens,
            vec![Err(Diagnostic::syntax_error(
                code.s1("f"),
                "Illegal digit 'f' for base 15",
            ))]
        );
    }

    #[test]
    fn tokenize_char_literal() {
        assert_eq!(
            kind_value_tokenize("'c'"),
            vec![(Character, Value::Character(b'c'))]
        );
    }

    #[test]
    fn tokenize_tick() {
        assert_eq!(kinds_tokenize("'"), vec![Tick]);
    }

    #[test]
    fn tokenize_plus() {
        assert_eq!(kinds_tokenize("+"), vec![Plus]);
    }

    #[test]
    fn tokenize_minus() {
        assert_eq!(kinds_tokenize("-"), vec![Minus]);
    }

    #[test]
    fn tokenize_semi_colon() {
        assert_eq!(kinds_tokenize(";"), vec![SemiColon]);
    }

    #[test]
    fn tokenize_colon() {
        assert_eq!(kinds_tokenize(":"), vec![Colon]);
    }

    #[test]
    fn tokenize_bar() {
        assert_eq!(kinds_tokenize("|"), vec![Bar]);
    }

    #[test]
    fn tokenize_dot() {
        assert_eq!(kinds_tokenize("."), vec![Dot]);
    }

    #[test]
    fn tokenize_concat() {
        assert_eq!(kinds_tokenize("&"), vec![Concat]);
    }

    #[test]
    fn tokenize_eq() {
        assert_eq!(kinds_tokenize("="), vec![EQ]);
    }

    #[test]
    fn tokenize_colon_eq() {
        assert_eq!(kinds_tokenize(":="), vec![ColonEq]);
    }

    #[test]
    fn tokenize_right_arrow() {
        assert_eq!(kinds_tokenize("=>"), vec![RightArrow]);
    }

    #[test]
    fn tokenize_cmp() {
        assert_eq!(kinds_tokenize("< <= > >="), vec![LT, LTE, GT, GTE]);
    }

    #[test]
    fn tokenize_box() {
        assert_eq!(kinds_tokenize("<>"), vec![BOX]);
    }

    #[test]
    fn tokenize_external_name() {
        assert_eq!(kinds_tokenize("<< >>"), vec![LtLt, GtGt]);
    }

    #[test]
    fn tokenize_questionmark_cmp() {
        assert_eq!(
            kinds_tokenize("? ?< ?<= ?> ?>= ??"),
            vec![Que, QueLT, QueLTE, QueGT, QueGTE, QueQue]
        );
    }

    #[test]
    fn tokenize_ne() {
        assert_eq!(kinds_tokenize("/="), vec![NE]);
    }

    #[test]
    fn tokenize_times() {
        assert_eq!(kinds_tokenize("*"), vec![Times]);
    }

    #[test]
    fn tokenize_pow() {
        assert_eq!(kinds_tokenize("**"), vec![Pow]);
    }

    #[test]
    fn tokenize_div() {
        assert_eq!(kinds_tokenize("/"), vec![Div]);
    }

    #[test]
    fn tokenize_comma() {
        assert_eq!(kinds_tokenize(","), vec![Comma]);
    }

    #[test]
    fn tokenize_pars() {
        assert_eq!(kinds_tokenize("()"), vec![LeftPar, RightPar]);
    }

    #[test]
    fn tokenize_squares() {
        assert_eq!(kinds_tokenize("[]"), vec![LeftSquare, RightSquare]);
    }

    #[test]
    fn tokenize_ignores_comments() {
        assert_eq!(
            kinds_tokenize(
                "
1
--comment
-2
"
            ),
            vec![AbstractLiteral, Minus, AbstractLiteral]
        );
    }

    #[test]
    fn tokenize_ignores_multi_line_comments() {
        assert_eq!(
            kinds_tokenize(
                "
1

/*
comment
*/

-2 /*
comment
*/

"
            ),
            vec![AbstractLiteral, Minus, AbstractLiteral]
        );
    }

    #[test]
    fn tokenize_ir1045() {
        // http://www.eda-stds.org/isac/IRs-VHDL-93/IR1045.txt
        assert_eq!(
            kinds_tokenize("string'('a')"),
            vec![Identifier, Tick, LeftPar, Character, RightPar]
        );
    }

    #[test]
    fn tokenize_too_large_integer() {
        let large_int = "100000000000000000000000000000000000000000";
        let code = Code::new(large_int);
        let (tokens, _) = code.tokenize_result();
        assert_eq!(
            tokens,
            vec![Err(Diagnostic::syntax_error(
                code.pos(),
                "Integer too large for 64-bit unsigned",
            ))]
        );

        let large_int = "1e100";
        let code = Code::new(large_int);
        let (tokens, _) = code.tokenize_result();
        assert_eq!(
            tokens,
            vec![Err(Diagnostic::syntax_error(
                code.pos(),
                "Integer too large for 64-bit unsigned",
            ))]
        );

        let exponent_str = ((i32::MAX as i64) + 1).to_string();
        let large_int = format!("1e{exponent_str}");
        let code = Code::new(&large_int);
        let (tokens, _) = code.tokenize_result();
        assert_eq!(
            tokens,
            vec![Err(Diagnostic::syntax_error(
                code.s1(&exponent_str),
                "Exponent too large for 32-bits signed",
            ))]
        );

        let exponent_str = ((i32::MIN as i64) - 1).to_string();
        let large_int = format!("1.0e{exponent_str}");
        let code = Code::new(&large_int);
        let (tokens, _) = code.tokenize_result();
        assert_eq!(
            tokens,
            vec![Err(Diagnostic::syntax_error(
                code.s1(&exponent_str),
                "Exponent too large for 32-bits signed",
            ))]
        );

        let large_int = ((u64::MAX as i128) + 1).to_string();
        let code = Code::new(&large_int);
        let (tokens, _) = code.tokenize_result();
        assert_eq!(
            tokens,
            vec![Err(Diagnostic::syntax_error(
                code.pos(),
                "Integer too large for 64-bit unsigned",
            ))]
        );

        let large_int = u64::MAX.to_string();
        let code = Code::new(&large_int);
        let (tokens, _) = code.tokenize_result();
        assert_eq!(
            tokens,
            vec![Ok(Token {
                kind: AbstractLiteral,
                value: Value::AbstractLiteral(
                    Latin1String::from_utf8_unchecked(&u64::MAX.to_string()),
                    ast::AbstractLiteral::Integer(u64::MAX)
                ),
                pos: code.pos(),
                comments: None,
            })]
        );
    }

    #[test]
    fn tokenize_illegal() {
        let code = Code::new("begin!end");
        let (tokens, _) = code.tokenize_result();
        assert_eq!(
            tokens,
            vec![
                Ok(Token {
                    kind: Begin,
                    value: Value::None,
                    pos: code.s1("begin").pos(),
                    comments: None,
                }),
                Err(Diagnostic::syntax_error(code.s1("!"), "Illegal token")),
                Ok(Token {
                    kind: End,
                    value: Value::None,
                    pos: code.s1("end").pos(),
                    comments: None,
                }),
            ]
        );
    }

    #[test]
    fn extract_final_comments() {
        let code = Code::new("--final");
        let (tokens, final_comments) = code.tokenize_result();
        assert_eq!(tokens, vec![]);
        assert_eq!(
            final_comments,
            vec![Comment {
                value: "final".to_string(),
                range: code.s1("--final").pos().range(),
                multi_line: false,
            },]
        );
    }

    #[test]
    fn extract_incomplete_multi_line_comment() {
        let code = Code::new("/* final");
        let (tokens, final_comments) = code.tokenize_result();
        assert_eq!(
            tokens,
            vec![Err(Diagnostic::syntax_error(
                code.s1("/* final"),
                "Incomplete multi-line comment",
            ))]
        );

        assert_eq!(final_comments, vec![]);
    }

    #[test]
    fn extract_comments() {
        let code = Code::new(
            "
 --this is a plus
+--this is still a plus
--- this is not a minus

    -- Neither is this
- -- this is a minus
-- a comment at the end of the file
       -- and another one
",
        );
        let (tokens, final_comments) = code.tokenize_result();
        let minus_pos = code.s1("- --").s1("-").pos();

        assert_eq!(
            tokens,
            vec![
                Ok(Token {
                    kind: Plus,
                    value: Value::None,
                    pos: code.s1("+").pos(),
                    comments: Some(Box::new(TokenComments {
                        leading: vec![Comment {
                            value: "this is a plus".to_string(),
                            range: code.s1("--this is a plus").pos().range(),
                            multi_line: false,
                        },],
                        trailing: Some(Comment {
                            range: code.s1("--this is still a plus").pos().range(),
                            value: "this is still a plus".to_string(),
                            multi_line: false,
                        }),
                    })),
                }),
                Ok(Token {
                    kind: Minus,
                    value: Value::None,
                    pos: minus_pos,
                    comments: Some(Box::new(TokenComments {
                        leading: vec![
                            Comment {
                                value: "- this is not a minus".to_string(),
                                range: code.s1("--- this is not a minus").pos().range(),
                                multi_line: false,
                            },
                            Comment {
                                value: " Neither is this".to_string(),
                                range: code.s1("-- Neither is this").pos().range(),
                                multi_line: false,
                            },
                        ],
                        trailing: Some(Comment {
                            range: code.s1("-- this is a minus").pos().range(),
                            value: " this is a minus".to_string(),
                            multi_line: false,
                        }),
                    })),
                }),
            ]
        );
        assert_eq!(
            final_comments,
            vec![
                Comment {
                    value: " a comment at the end of the file".to_string(),
                    range: code.s1("-- a comment at the end of the file").pos().range(),
                    multi_line: false,
                },
                Comment {
                    value: " and another one".to_string(),
                    range: code.s1("-- and another one").pos().range(),
                    multi_line: false,
                },
            ]
        );
    }

    #[test]
    fn extract_multi_line_comments() {
        let code = Code::new(
            "
/*foo
com*ment
bar*/

2

/*final*/
",
        );
        let (tokens, final_comments) = code.tokenize_result();

        assert_eq!(
            tokens,
            vec![Ok(Token {
                kind: AbstractLiteral,
                value: Value::AbstractLiteral(
                    Latin1String::new(b"2"),
                    ast::AbstractLiteral::Integer(2)
                ),
                pos: code.s1("2").pos(),
                comments: Some(Box::new(TokenComments {
                    leading: vec![Comment {
                        value: "foo\ncom*ment\nbar".to_string(),
                        range: code.s1("/*foo\ncom*ment\nbar*/").pos().range(),
                        multi_line: true,
                    },],
                    trailing: None,
                })),
            }),]
        );
        assert_eq!(
            final_comments,
            vec![Comment {
                value: "final".to_string(),
                range: code.s1("/*final*/").pos().range(),
                multi_line: true,
            },]
        );
    }

    #[test]
    fn comments_allow_non_latin1() {
        let code = Code::new(
            "
/* € */
entity -- €
",
        );
        let (tokens, _) = code.tokenize_result();

        assert_eq!(
            tokens,
            vec![Ok(Token {
                kind: Entity,
                value: Value::None,
                pos: code.s1("entity").pos(),
                comments: Some(Box::new(TokenComments {
                    leading: vec![Comment {
                        value: " € ".to_string(),
                        range: code.s1("/* € */").pos().range(),
                        multi_line: true,
                    },],
                    trailing: Some(Comment {
                        value: " €".to_string(),
                        range: code.s1("-- €").pos().range(),
                        multi_line: false,
                    }),
                })),
            })]
        );
    }

    #[test]
    fn tokenize_different_versions() {
        let code_str = "view default";
        let code = Code::with_standard(code_str, VHDLStandard::VHDL1993);
        let (tokens, _) = code.tokenize_result();
        assert_eq!(
            tokens
                .into_iter()
                .map(|tok| tok.unwrap().kind)
                .collect_vec(),
            vec![Identifier, Identifier]
        );

        let code = Code::with_standard(code_str, VHDLStandard::VHDL2008);
        let (tokens, _) = code.tokenize_result();
        assert_eq!(
            tokens
                .into_iter()
                .map(|tok| tok.unwrap().kind)
                .collect_vec(),
            vec![Identifier, Default]
        );

        let code = Code::with_standard(code_str, VHDLStandard::VHDL2019);
        let (tokens, _) = code.tokenize_result();
        assert_eq!(
            tokens
                .into_iter()
                .map(|tok| tok.unwrap().kind)
                .collect_vec(),
            vec![View, Default]
        );
    }
}
