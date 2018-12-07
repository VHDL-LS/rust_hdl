// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use message::{Message, ParseResult};
use source::{Source, SrcPos, WithPos};
extern crate fnv;
use self::fnv::FnvHashMap;

use ast;
use ast::{BaseSpecifier, Ident};
use latin_1::Latin1String;
use std::sync::Arc;
use symbol_table::{Symbol, SymbolTable};

/// The kind of a Token
#[derive(PartialEq, Clone, Copy, Debug)]
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
    Impure,
    Function,
    Procedure,
    Vunit,

    // Unary operators
    Abs,
    Not,

    // Unary and binary operators
    Plus,
    Minus,
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

    EQ,
    NE,
    LT,
    LTE,
    GT,
    GTE,

    QueEQ,
    QueNE,
    QueLT,
    QueLTE,
    QueGT,
    QueGTE,

    Times,
    Pow,
    Div,

    Identifier,
    AbstractLiteral,
    StringLiteral,
    BitString,
    Character,
    Tick,
    LeftPar,
    RightPar,
    LeftSquare,
    RightSquare,
    SemiColon,
    Colon,
    Bar,
    Dot,
    BOX,
    LtLt,
    GtGt,
    Circ,
    CommAt,
    Concat,
    Comma,
    ColonEq,
    RightArrow,
}
use self::Kind::*;

/// Expect any number of token kind patterns, returns Ok variant when
/// match else Err variant
#[macro_export]
macro_rules! match_token_kind {
    ($token:expr, $($($kind:ident)|+ => $result:expr),*) => {
        {
            match $token.kind {
                $(
                    $($kind)|+ => $result
                ),*,
                _ => {
                    let mut kinds = Vec::new();
                    $(
                        $(
                            kinds.push($kind);
                        );*;
                    );*;

                    Err($token.kinds_error(&kinds))
                }
            }
        }
    }
}

/// Expect any number of token kind patterns, return on no match with
/// error message based on expected kinds
#[macro_export]
macro_rules! try_token_kind {
    ($token:expr, $($($kind:ident)|+ => $result:expr),*) => {
        match $token.kind {
            $(
                $($kind)|+ => $result
            ),*,
            _ => {
                let mut kinds = Vec::new();
                $(
                    $(
                        kinds.push($kind);
                    );*;
                );*;

                return Err($token.kinds_error(&kinds));
            }
        }
    }
}

pub fn kind_str(kind: Kind) -> &'static str {
    match kind {
        // Keywords
        Architecture => &"architecture",
        Entity => &"entity",
        Configuration => &"configuration",
        Package => &"package",
        Block => &"block",
        Process => &"process",
        Generate => &"generate",
        Postponed => &"postponed",
        Library => &"library",
        Use => &"use",
        Context => &"context",
        Body => &"body",
        Component => &"component",
        Is => &"is",
        Return => &"return",
        Null => &"null",
        Of => &"of",
        On => &"on",
        Generic => &"generic",
        Map => &"map",
        Default => &"default",
        Port => &"port",
        Attribute => &"attribute",
        Begin => &"begin",
        If => &"if",
        Loop => &"loop",
        While => &"while",
        Case => &"case",
        Else => &"else",
        Elsif => &"elsif",
        Then => &"then",
        When => &"when",
        With => &"with",
        Select => &"select",
        Next => &"next",
        Exit => &"exit",
        For => &"for",
        Assert => &"assert",
        Report => &"report",
        Severity => &"severity",
        Wait => &"wait",
        After => &"after",
        Transport => &"transport",
        Inertial => &"inertial",
        Reject => &"reject",
        Unaffected => &"unaffected",
        Until => &"until",
        End => &"end",
        All => &"all",
        Range => &"range",
        Downto => &"downto",
        To => &"to",
        In => &"in",
        Out => &"out",
        InOut => &"inout",
        Buffer => &"buffer",
        Linkage => &"linkage",
        Signal => &"signal",
        Constant => &"constant",
        Variable => &"variable",
        File => &"file",
        Open => &"open",
        Alias => &"alias",
        Shared => &"shared",
        Others => &"others",
        Record => &"record",
        Type => &"type",
        Subtype => &"subtype",
        Access => &"access",
        Units => &"units",
        New => &"new",
        Array => &"array",
        Protected => &"protected",
        Impure => &"impure",
        Function => &"function",
        Procedure => &"procedure",
        Vunit => &"vunit",

        // Unary operators
        Abs => &"abs",
        Not => &"not",

        // Unary and binary operators
        Plus => &"plus",
        Minus => &"minus",
        QueQue => &"??",

        // Binary operators
        And => &"and",
        Or => &"or",
        Nand => &"nand",
        Nor => &"nor",
        Xor => &"xor",
        Xnor => &"xnor",
        SLL => &"sll",
        SRL => &"srl",
        SLA => &"sla",
        SRA => &"sra",
        ROL => &"rol",
        ROR => &"ror",

        Mod => &"mod",
        Rem => &"rem",

        EQ => &"=",
        NE => &"/=",
        LT => &"<",
        LTE => &"<=",
        GT => &">",
        GTE => &">=",

        QueEQ => &"?=",
        QueNE => &"?/=",
        QueLT => &"?<",
        QueLTE => &"?<=",
        QueGT => &"?>",
        QueGTE => &"?>=",

        Times => &"*",
        Pow => &"**",
        Div => &"/",

        Identifier => &"{identifier}",
        AbstractLiteral => &"{abstract_literal}",
        StringLiteral => &"{string}",
        BitString => &"{bit_string}",
        Character => &"{character}",
        Tick => &"'",
        LeftPar => &"(",
        RightPar => &")",
        LeftSquare => &"[",
        RightSquare => &"]",
        SemiColon => &";",
        Colon => &":",
        Bar => &"|",
        Dot => &".",
        BOX => &"<>",
        LtLt => &"<<",
        GtGt => &">>",
        Circ => &"^",
        CommAt => &"@",
        Concat => &"&",
        Comma => &",",
        ColonEq => &":=",
        RightArrow => &"=>",
    }
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
    BitString(ast::BitString),
    AbstractLiteral(ast::AbstractLiteral),
    Character(u8),
    NoValue,
}

/// A Token
#[derive(PartialEq, Clone, Debug)]
pub struct Token {
    pub kind: Kind,
    pub value: Value,
    pub pos: SrcPos,
}

use std::convert::AsRef;
impl AsRef<SrcPos> for Token {
    fn as_ref(&self) -> &SrcPos {
        &self.pos
    }
}

use std::convert::Into;
impl Into<SrcPos> for Token {
    fn into(self) -> SrcPos {
        self.pos
    }
}

pub fn kinds_error<T: AsRef<SrcPos>>(pos: T, kinds: &[Kind]) -> Message {
    Message::error(
        pos.as_ref(),
        format!("Expected {}", kinds_str(&kinds)).as_str(),
    )
}

impl Token {
    pub fn kinds_error(&self, kinds: &[Kind]) -> Message {
        kinds_error(self, kinds)
    }

    pub fn expect_ident(self) -> ParseResult<Ident> {
        if let Token {
            kind: Identifier,
            value: Value::Identifier(value),
            pos,
            ..
        } = self
        {
            Ok(WithPos::from(value, pos))
        } else {
            Err(self.kinds_error(&[Identifier]))
        }
    }

    pub fn expect_character(self) -> ParseResult<WithPos<u8>> {
        if let Token {
            kind: Character,
            value: Value::Character(value),
            pos,
            ..
        } = self
        {
            Ok(WithPos::from(value, pos))
        } else {
            Err(self.kinds_error(&[Character]))
        }
    }

    pub fn expect_kind(self, kind: Kind) -> ParseResult<Token> {
        if self.kind == kind {
            Ok(self)
        } else {
            Err(self.kinds_error(&[kind]))
        }
    }

    pub fn expect_bit_string(self) -> ParseResult<WithPos<ast::BitString>> {
        if let Token {
            kind: BitString,
            value: Value::BitString(value),
            pos,
            ..
        } = self
        {
            Ok(WithPos::from(value, pos))
        } else {
            Err(self.kinds_error(&[BitString]))
        }
    }

    pub fn expect_abstract_literal(self) -> ParseResult<WithPos<ast::AbstractLiteral>> {
        if let Token {
            kind: AbstractLiteral,
            value: Value::AbstractLiteral(value),
            pos,
            ..
        } = self
        {
            Ok(WithPos::from(value, pos))
        } else {
            Err(self.kinds_error(&[AbstractLiteral]))
        }
    }

    pub fn expect_string(self) -> ParseResult<WithPos<Latin1String>> {
        if let Token {
            kind: StringLiteral,
            value: Value::String(value),
            pos,
            ..
        } = self
        {
            Ok(WithPos::from(value, pos))
        } else {
            Err(self.kinds_error(&[StringLiteral]))
        }
    }
}

/// Resolves ir1045
/// http://www.eda-stds.org/isac/IRs-VHDL-93/IR1045.txt
/// char may not come after ], ), all, or identifier
fn can_be_char(last_token_kind: Option<Kind>) -> bool {
    if let Some(kind) = last_token_kind {
        match kind {
            RightSquare | RightPar | All | Identifier => false,
            _ => true,
        }
    } else {
        true
    }
}

pub struct ByteCursor<'a> {
    bytes: &'a [u8],
    idx: usize,
}

impl<'a> ByteCursor<'a> {
    fn new(bytes: &'a [u8], idx: usize) -> ByteCursor {
        ByteCursor { bytes, idx }
    }

    fn pop(&mut self) -> Option<u8> {
        if let Some(byte) = self.bytes.get(self.idx) {
            self.idx += 1;
            Some(*byte)
        } else {
            None
        }
    }

    fn skip_if(&mut self, value: u8) -> bool {
        if self.bytes.get(self.idx) == Some(&value) {
            self.idx += 1;
            true
        } else {
            false
        }
    }

    fn back(&mut self) {
        self.idx = self.idx.saturating_sub(1);
    }

    fn set(&mut self, pos: usize) {
        self.idx = pos;
    }

    fn pos(&self) -> usize {
        self.idx
    }

    fn peek(&mut self, offset: usize) -> Option<u8> {
        if let Some(byte) = self.bytes.get(self.idx + offset) {
            Some(*byte)
        } else {
            None
        }
    }
}

fn parse_integer(cursor: &mut ByteCursor, base: i64, stop_on_e: bool) -> Result<i64, String> {
    let mut result = Some(0);
    let mut too_large_base = false;

    while let Some(b) = cursor.peek(0) {
        let digit = i64::from(match b {
            b'0'..=b'9' => {
                cursor.pop();
                (b - b'0')
            }
            b'a'..=b'f' => {
                if stop_on_e && b == b'e' {
                    break;
                }
                cursor.pop();
                (10 + b - b'a')
            }
            b'A'..=b'F' => {
                if stop_on_e && b == b'E' {
                    break;
                }
                cursor.pop();
                (10 + b - b'A')
            }
            b'_' => {
                cursor.pop();
                continue;
            }
            _ => {
                break;
            }
        });

        too_large_base = too_large_base || (digit >= base);

        result = result
            .and_then(|x| base.checked_mul(x))
            .and_then(|x| x.checked_add(digit));
    }

    if too_large_base {
        Err(format!("Illegal digit for base {}", base).to_string())
    } else if let Some(result) = result {
        Ok(result)
    } else {
        Err("Integer too large for 64-bits signed".to_string())
    }
}

fn parse_exponent(cursor: &mut ByteCursor) -> Result<i32, String> {
    let sign = {
        if cursor.peek(0) == Some(b'-') {
            cursor.pop();
            -1
        } else {
            1
        }
    };

    let exp = parse_integer(cursor, 10, false)?;
    if let Some(exp) = exp.checked_mul(sign) {
        if i64::from(i32::min_value()) <= exp && exp <= i64::from(i32::max_value()) {
            return Ok(exp as i32);
        }
    }

    Err("Exponent too large for 32-bits signed".to_string())
}

fn pow(value: i64, exp: u32) -> Option<i64> {
    // @TODO use no_panic_pow once stable
    let mut value = value;
    for _ in 0..exp {
        value = value.checked_mul(10)?;
    }
    Some(value)
}

#[derive(PartialEq, Clone, Copy)]
pub struct TokenState {
    last_token_kind: Option<Kind>,
    start: usize,
}

impl TokenState {
    pub fn new() -> TokenState {
        TokenState {
            last_token_kind: None,
            start: 0,
        }
    }

    /// Set state to after token
    pub fn set_after(&mut self, token: &Token) {
        self.last_token_kind = Some(token.kind);
        self.start = token.pos.start + token.pos.length;
    }
}

fn parse_quoted(
    buffer: &mut Latin1String,
    cursor: &mut ByteCursor,
    quote: u8,
    include_quote: bool,
) -> Result<Latin1String, String> {
    buffer.bytes.clear();
    let mut is_multiline = false;
    let mut found_end = false;

    if include_quote {
        buffer.bytes.push(quote)
    }

    cursor.pop();
    while let Some(chr) = cursor.pop() {
        is_multiline |= chr == b'\n';
        if chr == quote {
            if cursor.peek(0) == Some(quote) {
                cursor.pop();
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
        Err("Reached EOF before end quote".to_string())
    } else if is_multiline {
        Err("Multi line string".to_string())
    } else {
        Ok(buffer.clone())
    }
}

fn parse_string(
    buffer: &mut Latin1String,
    cursor: &mut ByteCursor,
) -> Result<Latin1String, String> {
    parse_quoted(buffer, cursor, b'"', false)
}

fn parse_real_literal(buffer: &mut Latin1String, cursor: &mut ByteCursor) -> Result<f64, String> {
    buffer.bytes.clear();

    while let Some(b) = cursor.peek(0) {
        match b {
            b'e' => {
                break;
            }
            b'0'..=b'9' | b'a'..=b'd' | b'f' | b'A'..=b'F' | b'.' => {
                cursor.pop();
                buffer.bytes.push(b);
            }
            b'_' => {
                cursor.pop();
                continue;
            }
            _ => {
                break;
            }
        };
    }

    let string = unsafe { std::str::from_utf8_unchecked(&buffer.bytes) };
    let result: Result<f64, String> = string
        .parse()
        .map_err(|err: std::num::ParseFloatError| err.to_string());
    result
}

/// LRM 15.5 Abstract literals
fn parse_abstract_literal(
    buffer: &mut Latin1String,
    cursor: &mut ByteCursor,
) -> Result<(Kind, Value), String> {
    let pos = cursor.pos();
    let initial = parse_integer(cursor, 10, true);

    match cursor.peek(0) {
        // Real
        Some(b'.') => {
            cursor.set(pos);
            let real = parse_real_literal(buffer, cursor)?;

            match cursor.peek(0) {
                // Exponent
                Some(b'e') | Some(b'E') => {
                    cursor.pop();
                    let exp = parse_exponent(cursor)?;
                    Ok((
                        AbstractLiteral,
                        Value::AbstractLiteral(ast::AbstractLiteral::Real(
                            real * (10.0 as f64).powi(exp),
                        )),
                    ))
                }
                _ => Ok((
                    AbstractLiteral,
                    Value::AbstractLiteral(ast::AbstractLiteral::Real(real)),
                )),
            }
        }

        // Integer exponent
        Some(b'e') | Some(b'E') => {
            let integer = initial?;
            cursor.pop();
            let exp = parse_exponent(cursor)?;
            if exp >= 0 {
                if let Some(value) = pow(integer, exp as u32) {
                    Ok((
                        AbstractLiteral,
                        Value::AbstractLiteral(ast::AbstractLiteral::Integer(value)),
                    ))
                } else {
                    Err("Integer too large for 64-bits signed".to_string())
                }
            } else {
                Err("Integer literals may not have negative exponent".to_string())
            }
        }

        // Based integer
        Some(b'#') => {
            let base = initial?;
            cursor.pop();
            let base_result = parse_integer(cursor, base, false);

            if let Some(b'#') = cursor.peek(0) {
                cursor.pop();
                let integer = base_result?;
                if base >= 2 && base <= 16 {
                    Ok((
                        AbstractLiteral,
                        Value::AbstractLiteral(ast::AbstractLiteral::Integer(integer)),
                    ))
                } else {
                    Err(format!("Base must be at least 2 and at most 16, got {}", base).to_string())
                }
            } else {
                Err("Based integer did not end with #".to_string())
            }
        }
        _ => {
            let integer = initial?;
            // @TODO check overflow
            if let Some((kind, bit_string)) =
                parse_bit_string(buffer, cursor, Some(integer as u32))?
            {
                Ok((kind, bit_string))
            } else {
                // Plain integer
                Ok((
                    AbstractLiteral,
                    Value::AbstractLiteral(ast::AbstractLiteral::Integer(integer)),
                ))
            }
        }
    }
}

/// LRM 15.8 Bit string literals
fn parse_bit_string(
    buffer: &mut Latin1String,
    cursor: &mut ByteCursor,
    bit_string_length: Option<u32>,
) -> Result<Option<(Kind, Value)>, String> {
    let first_byte = {
        if let Some(byte) = cursor.peek(0) {
            Latin1String::lowercase(byte)
        } else {
            return Ok(None);
        }
    };

    let second_byte = {
        if let Some(byte) = cursor.peek(1) {
            Latin1String::lowercase(byte)
        } else {
            return Ok(None);
        }
    };

    let (len, base_specifier) = match first_byte {
        b'u' => match second_byte {
            b'b' => (2, BaseSpecifier::UB),
            b'o' => (2, BaseSpecifier::UO),
            b'x' => (2, BaseSpecifier::UX),
            _ => return Ok(None),
        },
        b's' => match second_byte {
            b'b' => (2, BaseSpecifier::SB),
            b'o' => (2, BaseSpecifier::SO),
            b'x' => (2, BaseSpecifier::SX),
            _ => return Ok(None),
        },
        b'b' => (1, BaseSpecifier::B),
        b'o' => (1, BaseSpecifier::O),
        b'x' => (1, BaseSpecifier::X),
        b'd' => (1, BaseSpecifier::D),
        _ => return Ok(None),
    };

    cursor.idx += len;

    if let Some(b'"') = cursor.peek(0) {
        Ok(Some((
            BitString,
            Value::BitString(ast::BitString {
                length: bit_string_length,
                base: base_specifier,
                value: parse_string(buffer, cursor)?,
            }),
        )))
    } else {
        return Ok(None);
    }
}

/// LRM 15.4 Identifiers
fn parse_basic_identifier_or_keyword(
    buffer: &mut Latin1String,
    cursor: &mut ByteCursor,
    keywords: &FnvHashMap<&'static [u8], Kind>,
    symtab: &SymbolTable,
) -> Result<(Kind, Value), String> {
    let start = cursor.idx;
    while let Some(b) = cursor.peek(0) {
        match b {
            b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_' => {
                cursor.pop();
            }
            _ => {
                break;
            }
        }
    }

    buffer.bytes.clear();
    buffer
        .bytes
        .extend_from_slice(&cursor.bytes[start..cursor.idx]);
    buffer.make_lowercase();

    match keywords.get(buffer.bytes.as_slice()) {
        Some(kind) => Ok((*kind, Value::NoValue)),
        None => {
            buffer.bytes.clear();
            buffer
                .bytes
                .extend_from_slice(&cursor.bytes[start..cursor.idx]);
            Ok((Identifier, Value::Identifier(symtab.insert(&buffer))))
        }
    }
}

#[derive(Clone)]
pub struct Tokenizer {
    keywords: FnvHashMap<&'static [u8], Kind>,
    symtab: Arc<SymbolTable>,
    buffer: Latin1String,
    state: TokenState,
    source: Source,
    code: Arc<Latin1String>,
    pub range_ident: Symbol,
    pub reverse_range_ident: Symbol,
}

impl Tokenizer {
    pub fn new(symtab: Arc<SymbolTable>, source: Source, code: Arc<Latin1String>) -> Tokenizer {
        let keywords = [
            ("architecture", Architecture),
            ("entity", Entity),
            ("configuration", Configuration),
            ("package", Package),
            ("block", Block),
            ("process", Process),
            ("generate", Generate),
            ("postponed", Postponed),
            ("library", Library),
            ("use", Use),
            ("context", Context),
            ("body", Body),
            ("component", Component),
            ("is", Is),
            ("return", Return),
            ("null", Null),
            ("of", Of),
            ("on", On),
            ("generic", Generic),
            ("map", Map),
            ("default", Default),
            ("port", Port),
            ("attribute", Attribute),
            ("begin", Begin),
            ("end", End),
            ("if", If),
            ("loop", Loop),
            ("while", While),
            ("case", Case),
            ("else", Else),
            ("elsif", Elsif),
            ("then", Then),
            ("when", When),
            ("with", With),
            ("select", Select),
            ("next", Next),
            ("exit", Exit),
            ("for", For),
            ("assert", Assert),
            ("report", Report),
            ("severity", Severity),
            ("wait", Wait),
            ("after", After),
            ("transport", Transport),
            ("inertial", Inertial),
            ("reject", Reject),
            ("unaffected", Unaffected),
            ("until", Until),
            ("all", All),
            ("range", Range),
            ("downto", Downto),
            ("to", To),
            ("in", In),
            ("out", Out),
            ("inout", InOut),
            ("buffer", Buffer),
            ("linkage", Linkage),
            ("signal", Signal),
            ("constant", Constant),
            ("variable", Variable),
            ("file", File),
            ("open", Open),
            ("alias", Alias),
            ("shared", Shared),
            ("others", Others),
            ("record", Record),
            ("type", Type),
            ("subtype", Subtype),
            ("access", Access),
            ("units", Units),
            ("new", New),
            ("array", Array),
            ("protected", Protected),
            ("impure", Impure),
            ("function", Function),
            ("procedure", Procedure),
            ("abs", Abs),
            ("not", Not),
            ("and", And),
            ("or", Or),
            ("nand", Nand),
            ("nor", Nor),
            ("xor", Xor),
            ("xnor", Xnor),
            ("sll", SLL),
            ("srl", SRL),
            ("sla", SLA),
            ("sra", SRA),
            ("rol", ROL),
            ("ror", ROR),
            ("mod", Mod),
            ("rem", Rem),
            ("vunit", Vunit),
        ];

        let keywords: FnvHashMap<&[u8], Kind> = keywords
            .into_iter()
            .map(|(string, kind)| (string.as_bytes(), *kind))
            .collect();

        let range_ident = symtab.insert(&Latin1String::new(b"range"));
        let reverse_range_ident = symtab.insert(&Latin1String::new(b"reverse_range"));

        Tokenizer {
            keywords,
            symtab,
            state: TokenState::new(),
            buffer: Latin1String::empty(),
            source,
            code,
            range_ident,
            reverse_range_ident,
        }
    }

    pub fn state(&self) -> TokenState {
        self.state
    }

    pub fn eof_error(&self) -> Message {
        Message::error(self.source.pos(self.state.start, 1), "Unexpected EOF")
    }

    pub fn set_state(&mut self, state: TokenState) {
        self.state = state;
    }

    pub fn move_after(&mut self, token: &Token) {
        self.state.set_after(token);
    }

    pub fn pop(&mut self) -> ParseResult<Option<Token>> {
        let mut cursor = ByteCursor::new(&self.code.bytes, self.state.start);

        macro_rules! error {
            ($message:expr) => {
                let length = cursor.pos() - self.state.start;
                let err = Err(Message::error(
                    &self.source.pos(self.state.start, length),
                    $message,
                ));
                self.state.start = cursor.pos();
                return err;
            };
        }

        while let Some(byte) = cursor.pop() {
            let (kind, value) = match byte {
                b' ' | b'\n' | b'\r' | b'\t' => {
                    // Ignore whitespace
                    self.state.start = cursor.pos();
                    continue;
                }
                b':' => {
                    if cursor.skip_if(b'=') {
                        (ColonEq, Value::NoValue)
                    } else {
                        (Colon, Value::NoValue)
                    }
                }
                b';' => (SemiColon, Value::NoValue),
                b'(' => (LeftPar, Value::NoValue),
                b')' => (RightPar, Value::NoValue),
                b'[' => (LeftSquare, Value::NoValue),
                b']' => (RightSquare, Value::NoValue),
                b'+' => (Plus, Value::NoValue),
                b'|' => (Bar, Value::NoValue),
                b'.' => (Dot, Value::NoValue),
                b'&' => (Concat, Value::NoValue),
                b',' => (Comma, Value::NoValue),
                b'^' => (Circ, Value::NoValue),
                b'@' => (CommAt, Value::NoValue),
                b'=' => {
                    if cursor.skip_if(b'>') {
                        (RightArrow, Value::NoValue)
                    } else {
                        (EQ, Value::NoValue)
                    }
                }
                b'?' => match cursor.pop() {
                    Some(b'?') => (QueQue, Value::NoValue),
                    Some(b'=') => (QueEQ, Value::NoValue),
                    Some(b'/') => {
                        if cursor.skip_if(b'=') {
                            (QueNE, Value::NoValue)
                        } else {
                            error!("Illegal token");
                        }
                    }
                    Some(b'<') => {
                        if cursor.skip_if(b'=') {
                            (QueLTE, Value::NoValue)
                        } else {
                            (QueLT, Value::NoValue)
                        }
                    }
                    Some(b'>') => {
                        if cursor.skip_if(b'=') {
                            (QueGTE, Value::NoValue)
                        } else {
                            (QueGT, Value::NoValue)
                        }
                    }
                    _ => {
                        cursor.back();
                        error!("Illegal token");
                    }
                },
                b'<' => match cursor.pop() {
                    Some(b'=') => (LTE, Value::NoValue),
                    Some(b'>') => (BOX, Value::NoValue),
                    Some(b'<') => (LtLt, Value::NoValue),
                    _ => {
                        cursor.back();
                        (LT, Value::NoValue)
                    }
                },
                b'>' => match cursor.pop() {
                    Some(b'=') => (GTE, Value::NoValue),
                    Some(b'>') => (GtGt, Value::NoValue),
                    _ => {
                        cursor.back();
                        (GT, Value::NoValue)
                    }
                },
                b'/' => {
                    if cursor.skip_if(b'=') {
                        (NE, Value::NoValue)
                    } else {
                        (Div, Value::NoValue)
                    }
                }
                b'*' => {
                    if cursor.skip_if(b'*') {
                        (Pow, Value::NoValue)
                    } else {
                        (Times, Value::NoValue)
                    }
                }
                b'\'' => {
                    if can_be_char(self.state.last_token_kind) && cursor.peek(1) == Some(b'\'') {
                        cursor.pop();
                        cursor.pop();
                        (
                            Character,
                            Value::Character(self.code.bytes[cursor.pos() - 2]),
                        )
                    } else {
                        (Tick, Value::NoValue)
                    }
                }
                b'-' => {
                    if cursor.skip_if(b'-') {
                        // Comment
                        while let Some(byte) = cursor.pop() {
                            if byte == b'\n' {
                                break;
                            }
                        }
                        self.state.start = cursor.pos();
                        continue;
                    } else {
                        (Minus, Value::NoValue)
                    }
                }
                b'"' => {
                    cursor.back();
                    match parse_string(&mut self.buffer, &mut cursor) {
                        Ok(result) => (StringLiteral, Value::String(result)),
                        Err(msg) => {
                            error!(msg);
                        }
                    }
                }
                b'0'...b'9' => {
                    cursor.back();
                    match parse_abstract_literal(&mut self.buffer, &mut cursor) {
                        Ok((kind, value)) => (kind, value),
                        Err(msg) => {
                            error!(msg);
                        }
                    }
                }
                b'\\' => {
                    // LRM 15.4.3 Extended identifers
                    cursor.back();
                    match parse_quoted(&mut self.buffer, &mut cursor, b'\\', true) {
                        Ok(result) => {
                            let result = Value::Identifier(self.symtab.insert_extended(&result));
                            (Identifier, result)
                        }
                        Err(msg) => {
                            error!(msg);
                        }
                    }
                }
                b'a'..=b'z' | b'A'..=b'Z' => {
                    cursor.back();
                    let pos = cursor.pos();
                    match parse_bit_string(&mut self.buffer, &mut cursor, None) {
                        Ok(opt) => {
                            if let Some((kind, bit_string)) = opt {
                                (kind, bit_string)
                            } else {
                                cursor.set(pos);
                                match parse_basic_identifier_or_keyword(
                                    &mut self.buffer,
                                    &mut cursor,
                                    &self.keywords,
                                    &self.symtab,
                                ) {
                                    Ok((kind, value)) => (kind, value),
                                    Err(msg) => {
                                        error!(msg);
                                    }
                                }
                            }
                        }
                        Err(msg) => {
                            error!(msg);
                        }
                    }
                }
                _ => {
                    error!("Illegal token");
                }
            };

            let length = cursor.pos() - self.state.start;
            let token = Some(Token {
                kind,
                value,
                pos: self.source.pos(self.state.start, length),
            });
            self.state.start = cursor.pos();
            self.state.last_token_kind = Some(kind);
            return Ok(token);
        }
        Ok(None)
    }
}

/// Tokenize the code into a vector of tokens
/// String symbols are added to the SymbolTable
#[cfg(test)]
fn tokenize_result(code: &str) -> (Source, Arc<SymbolTable>, Vec<Result<Token, Message>>) {
    let symtab = Arc::new(SymbolTable::new());
    let source = Source::from_str(code);
    let mut tokens = Vec::new();
    {
        let code = source.contents().unwrap();
        let mut tokenizer = Tokenizer::new(symtab.clone(), source.clone(), code);
        loop {
            let token = tokenizer.pop();

            match token {
                Ok(None) => break,
                Ok(Some(token)) => tokens.push(Ok(token)),
                Err(err) => tokens.push(Err(err)),
            }
        }
    }
    (source, symtab, tokens)
}

#[cfg(test)]
pub fn tokenize(code: &str) -> (Source, Arc<SymbolTable>, Vec<Token>) {
    let (source, symtab, tokens) = tokenize_result(code);
    (
        source,
        symtab,
        tokens.into_iter().map(|tok| tok.unwrap()).collect(),
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    fn kinds(tokens: &[Token]) -> Vec<Kind> {
        tokens.iter().map(|ref tok| tok.kind.clone()).collect()
    }

    // Shorthand for testing
    fn kinds_tokenize(code: &str) -> Vec<Kind> {
        let (_, _, tokens) = tokenize(code);
        kinds(&tokens)
    }

    // Shorthand for testing
    fn kind_value_tokenize(code: &str) -> Vec<(Kind, Value)> {
        let (_, _, ref tokens) = tokenize(code);
        tokens
            .iter()
            .map(|ref tok| (tok.kind.clone(), tok.value.clone()))
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
        let (source, symtab, tokens) = tokenize("entity foo");

        assert_eq!(
            tokens[0],
            Token {
                kind: Entity,
                value: Value::NoValue,
                pos: source.first_substr_pos("entity")
            }
        );

        assert_eq!(
            tokens[1],
            Token {
                kind: Identifier,
                value: Value::Identifier(symtab.insert_utf8("foo")),
                pos: source.first_substr_pos("foo")
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
        let (source, symtab, tokens) = tokenize("my_ident");
        assert_eq!(
            tokens,
            vec![Token {
                kind: Identifier,
                value: Value::Identifier(symtab.insert_utf8("my_ident")),
                pos: source.entire_pos()
            }]
        );
    }

    #[test]
    fn tokenize_identifier_case_insensitive() {
        let (source, symtab, tokens) = tokenize("My_Ident");
        assert_eq!(
            tokens,
            vec![Token {
                kind: Identifier,
                value: Value::Identifier(symtab.insert_utf8("my_ident")),
                pos: source.entire_pos()
            }]
        );
    }

    #[test]
    fn tokenize_extended_identifier() {
        let (source, symtab, tokens) = tokenize("\\1$my_ident\\");
        assert_eq!(
            tokens,
            vec![Token {
                kind: Identifier,
                value: Value::Identifier(symtab.insert_utf8("\\1$my_ident\\")),
                pos: source.entire_pos()
            }]
        );
        let (source, symtab, tokens) = tokenize("\\my\\\\_ident\\");
        assert_eq!(
            tokens,
            vec![Token {
                kind: Identifier,
                value: Value::Identifier(symtab.insert_utf8("\\my\\_ident\\")),
                pos: source.entire_pos()
            }]
        );
    }

    #[test]
    fn tokenize_many_identifiers() {
        let (source, symtab, tokens) = tokenize("my_ident my_other_ident");
        assert_eq!(
            tokens,
            vec![
                Token {
                    kind: Identifier,
                    value: Value::Identifier(symtab.insert_utf8("my_ident")),
                    pos: source.first_substr_pos("my_ident")
                },
                Token {
                    kind: Identifier,
                    value: Value::Identifier(symtab.insert_utf8("my_other_ident")),
                    pos: source.first_substr_pos("my_other_ident")
                }
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
                    Value::AbstractLiteral(ast::AbstractLiteral::Integer(100))
                ),
                (Minus, Value::NoValue),
                (
                    AbstractLiteral,
                    Value::AbstractLiteral(ast::AbstractLiteral::Integer(123))
                ),
                (
                    AbstractLiteral,
                    Value::AbstractLiteral(ast::AbstractLiteral::Integer(162))
                ),
                (
                    AbstractLiteral,
                    Value::AbstractLiteral(ast::AbstractLiteral::Integer(1000))
                ),
                (
                    AbstractLiteral,
                    Value::AbstractLiteral(ast::AbstractLiteral::Integer(20000))
                ),
            ]
        );
    }

    #[test]
    fn tokenize_integer_negative_exponent() {
        let (source, _, tokens) = tokenize_result("1e-1");
        assert_eq!(
            tokens,
            vec![Err(Message::error(
                &source.entire_pos(),
                "Integer literals may not have negative exponent"
            ))]
        );
    }

    #[test]
    fn tokenize_real() {
        assert_eq!(
            kind_value_tokenize("0.1 -2_2.3_3 2.0e3 3.33E2 2.1e-2"),
            vec![
                (
                    AbstractLiteral,
                    Value::AbstractLiteral(ast::AbstractLiteral::Real(0.1))
                ),
                (Minus, Value::NoValue),
                (
                    AbstractLiteral,
                    Value::AbstractLiteral(ast::AbstractLiteral::Real(22.33))
                ),
                (
                    AbstractLiteral,
                    Value::AbstractLiteral(ast::AbstractLiteral::Real(2000.0))
                ),
                (
                    AbstractLiteral,
                    Value::AbstractLiteral(ast::AbstractLiteral::Real(333.0))
                ),
                (
                    AbstractLiteral,
                    Value::AbstractLiteral(ast::AbstractLiteral::Real(0.021))
                )
            ]
        );
    }

    #[test]
    fn tokenize_real_many_fractional_digits() {
        assert_eq!(
            kind_value_tokenize("0.1000_0000_0000_0000_0000_0000_0000_0000"),
            vec![(
                AbstractLiteral,
                Value::AbstractLiteral(ast::AbstractLiteral::Real(1e-1))
            )]
        );
    }

    #[test]
    fn tokenize_real_many_integer_digits() {
        assert_eq!(
            kind_value_tokenize("1000_0000_0000_0000_0000_0000_0000_0000.0"),
            vec![(
                AbstractLiteral,
                Value::AbstractLiteral(ast::AbstractLiteral::Real(1e31))
            )]
        );
    }

    #[test]
    fn tokenize_real_truncates_precision() {
        assert_eq!(
            kind_value_tokenize("2.71828182845904523536"),
            vec![(
                AbstractLiteral,
                Value::AbstractLiteral(ast::AbstractLiteral::Real(2.7182818284590452))
            )]
        );
    }

    #[test]
    fn tokenize_string_literal() {
        let (source, _, tokens) = tokenize("\"string\"");
        assert_eq!(
            tokens,
            vec![Token {
                kind: StringLiteral,
                value: Value::String(Latin1String::from_utf8_unchecked("string")),
                pos: source.entire_pos()
            },]
        );
    }

    #[test]
    fn tokenize_string_literal_quote() {
        let (source, _, tokens) = tokenize("\"str\"\"ing\"");
        assert_eq!(
            tokens,
            vec![Token {
                kind: StringLiteral,
                value: Value::String(Latin1String::from_utf8_unchecked("str\"ing")),
                pos: source.entire_pos()
            },]
        );
    }

    #[test]
    fn tokenize_string_literal_quote_separated() {
        let (source, _, tokens) = tokenize("\"str\" \"ing\"");
        assert_eq!(
            tokens,
            vec![
                Token {
                    kind: StringLiteral,
                    value: Value::String(Latin1String::from_utf8_unchecked("str")),
                    pos: source.first_substr_pos("\"str\"")
                },
                Token {
                    kind: StringLiteral,
                    value: Value::String(Latin1String::from_utf8_unchecked("ing")),
                    pos: source.first_substr_pos("\"ing\"")
                },
            ]
        );
    }

    #[test]
    fn tokenize_string_literal_error_on_multiline() {
        // Multiline is illegal
        let (source, _, tokens) = tokenize_result("\"str\ning\"");
        assert_eq!(
            tokens,
            vec![Err(Message::error(
                &source.entire_pos(),
                "Multi line string"
            ))]
        );
    }

    #[test]
    fn tokenize_string_literal_error_on_early_eof() {
        // End of file
        let (source, _, tokens) = tokenize_result("\"string");
        assert_eq!(
            tokens,
            vec![Err(Message::error(
                &source.entire_pos(),
                "Reached EOF before end quote"
            ))]
        );
    }

    #[test]
    fn tokenize_bit_string_literal() {
        let (source, _, tokens) = tokenize("X\"ff\"");
        assert_eq!(
            tokens,
            vec![Token {
                kind: BitString,
                value: Value::BitString(ast::BitString {
                    length: None,
                    base: BaseSpecifier::X,
                    value: Latin1String::from_utf8_unchecked("ff")
                }),
                pos: source.entire_pos()
            },]
        );
        let (source, _, tokens) = tokenize("12ux\"ff\"");
        assert_eq!(
            tokens,
            vec![Token {
                kind: BitString,
                value: Value::BitString(ast::BitString {
                    length: Some(12),
                    base: BaseSpecifier::UX,
                    value: Latin1String::from_utf8_unchecked("ff")
                }),
                pos: source.entire_pos()
            },]
        );

        let (source, symtab, tokens) = tokenize("u");
        assert_eq!(
            tokens,
            vec![Token {
                kind: Identifier,
                value: Value::Identifier(symtab.insert_utf8("u")),
                pos: source.entire_pos()
            },]
        );
    }

    #[test]
    fn tokenize_based_integer() {
        assert_eq!(
            kind_value_tokenize("2#101#"),
            vec![(
                AbstractLiteral,
                Value::AbstractLiteral(ast::AbstractLiteral::Integer(5))
            ),]
        );
        assert_eq!(
            kind_value_tokenize("8#321#"),
            vec![(
                AbstractLiteral,
                Value::AbstractLiteral(ast::AbstractLiteral::Integer(3 * 8 * 8 + 2 * 8 + 1))
            ),]
        );
        assert_eq!(
            kind_value_tokenize("16#eEFfa#"),
            vec![(
                AbstractLiteral,
                Value::AbstractLiteral(ast::AbstractLiteral::Integer(0xeEffa))
            ),]
        );
    }

    #[test]
    fn tokenize_illegal_based_integer() {
        // Base may only be 2-16
        let (source, _, tokens) = tokenize_result("1#0#");
        assert_eq!(
            tokens,
            vec![Err(Message::error(
                &source.entire_pos(),
                "Base must be at least 2 and at most 16, got 1"
            ))]
        );
        let (source, _, tokens) = tokenize_result("17#f#");
        assert_eq!(
            tokens,
            vec![Err(Message::error(
                &source.entire_pos(),
                "Base must be at least 2 and at most 16, got 17"
            ))]
        );
        // May not use digit larger than or equal base
        let (source, _, tokens) = tokenize_result("3#3#");
        assert_eq!(
            tokens,
            vec![Err(Message::error(
                &source.entire_pos(),
                "Illegal digit for base 3"
            ))]
        );
        let (source, _, tokens) = tokenize_result("15#f#");
        assert_eq!(
            tokens,
            vec![Err(Message::error(
                &source.entire_pos(),
                "Illegal digit for base 15"
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
            kinds_tokenize("?< ?<= ?> ?>= ??"),
            vec![QueLT, QueLTE, QueGT, QueGTE, QueQue]
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
        let (source, _, tokens) = tokenize_result(large_int);
        assert_eq!(
            tokens,
            vec![Err(Message::error(
                &source.entire_pos(),
                "Integer too large for 64-bits signed"
            ))]
        );

        let large_int = "1e100";
        let (source, _, tokens) = tokenize_result(large_int);
        assert_eq!(
            tokens,
            vec![Err(Message::error(
                &source.entire_pos(),
                "Integer too large for 64-bits signed"
            ))]
        );

        let large_int = format!("1e{}", (i32::max_value() as i64) + 1).to_string();
        let (source, _, tokens) = tokenize_result(&large_int);
        assert_eq!(
            tokens,
            vec![Err(Message::error(
                &source.entire_pos(),
                "Exponent too large for 32-bits signed"
            ))]
        );

        let large_int = format!("1.0e{}", (i32::min_value() as i64) - 1).to_string();
        let (source, _, tokens) = tokenize_result(&large_int);
        assert_eq!(
            tokens,
            vec![Err(Message::error(
                &source.entire_pos(),
                "Exponent too large for 32-bits signed"
            ))]
        );

        let large_int = ((i64::max_value() as i128) + 1).to_string();
        let (source, _, tokens) = tokenize_result(&large_int);
        assert_eq!(
            tokens,
            vec![Err(Message::error(
                &source.entire_pos(),
                "Integer too large for 64-bits signed"
            ))]
        );

        let large_int = i64::max_value().to_string();
        let (source, _, tokens) = tokenize_result(&large_int);
        assert_eq!(
            tokens,
            vec![Ok(Token {
                kind: AbstractLiteral,
                value: Value::AbstractLiteral(ast::AbstractLiteral::Integer(i64::max_value())),
                pos: source.entire_pos()
            })]
        );
    }

    #[test]
    fn tokenize_illegal() {
        let (source, _, tokens) = tokenize_result("begin?end");
        assert_eq!(
            tokens,
            vec![
                Ok(Token {
                    kind: Begin,
                    value: Value::NoValue,
                    pos: source.first_substr_pos("begin")
                }),
                Err(Message::error(
                    &source.first_substr_pos("?"),
                    "Illegal token"
                )),
                Ok(Token {
                    kind: End,
                    value: Value::NoValue,
                    pos: source.first_substr_pos("end")
                })
            ]
        );
    }

    #[test]
    fn test_match_token_kind() {
        let (_, _, tokens) = tokenize("entity");
        let result = match_token_kind!(
            tokens[0],
            Identifier => Ok(1),
            Entity => Ok(2));
        assert_eq!(result, Ok(2));
    }

    #[test]
    fn test_match_token_kind_error() {
        let (_, _, tokens) = tokenize("+");
        let result = match_token_kind!(
            tokens[0],
            Identifier => Ok(1),
            Entity => Ok(2));
        assert_eq!(result, Err(tokens[0].kinds_error(&[Identifier, Entity])));
    }

    #[test]
    fn test_match_token_kind_pattern_error() {
        let (_, _, tokens) = tokenize("+");
        let result = match_token_kind!(
            tokens[0],
            Identifier | StringLiteral => Ok(1),
            Entity => Ok(2));
        assert_eq!(
            result,
            Err(tokens[0].kinds_error(&[Identifier, StringLiteral, Entity]))
        );
    }

}
