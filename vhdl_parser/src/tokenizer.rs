// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use self::fnv::FnvHashMap;
use super::contents::ContentReader;
use crate::diagnostic::{Diagnostic, ParseResult};
use crate::source::{Position, Range, Source, SrcPos, WithPos};
use fnv;

use crate::ast;
use crate::ast::{BaseSpecifier, Ident};
use crate::latin_1::Latin1String;
use crate::symbol_table::{Symbol, SymbolTable};
use std::sync::Arc;

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
                        )*
                    )*

                    Err($token.kinds_error(&kinds))
                }
            }
        }
    }
}

/// Expect any number of token kind patterns, return on no match with
/// error diagnostic based on expected kinds
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
                    )*
                )*

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
        Label => &"label",
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
        Force => &"force",
        Release => &"release",
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
    pub next_pos: Position,
    pub comments: Option<Box<TokenComments>>,
}

#[derive(PartialEq, Clone, Debug)]
pub struct TokenComments {
    pub leading: Vec<Comment>,
    pub trailing: Option<Comment>,
}

#[derive(PartialEq, Clone, Debug)]
pub struct Comment {
    pub value: Latin1String,
    pub range: Range,
    pub multi_line: bool,
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

pub fn kinds_error<T: AsRef<SrcPos>>(pos: T, kinds: &[Kind]) -> Diagnostic {
    Diagnostic::error(
        pos.as_ref(),
        format!("Expected {}", kinds_str(&kinds)).as_str(),
    )
}

impl Token {
    pub fn kinds_error(&self, kinds: &[Kind]) -> Diagnostic {
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

fn parse_integer(
    reader: &mut ContentReader,
    base: u64,
    stop_on_suffix: bool,
) -> Result<u64, String> {
    let mut result = Some(0 as u64);
    let mut too_large_digit = None;
    let mut invalid_character = None;

    while let Some(b) = reader.peek(0) {
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

            b'0'..=b'9' => {
                reader.pop();
                (b - b'0')
            }
            b'a'..=b'f' => {
                reader.pop();
                (10 + b - b'a')
            }
            b'A'..=b'F' => {
                reader.pop();
                (10 + b - b'A')
            }
            b'_' => {
                reader.pop();
                continue;
            }
            b'g'..=b'z' | b'G'..=b'Z' => {
                reader.pop();
                invalid_character = Some(b);
                continue;
            }
            _ => {
                break;
            }
        });

        if digit >= base {
            too_large_digit = Some(b);
        }

        result = result
            .and_then(|x| base.checked_mul(x))
            .and_then(|x| x.checked_add(digit));
    }

    if let Some(b) = invalid_character {
        Err(format!("Invalid integer character '{}'", Latin1String::new(&[b])).to_string())
    } else if let Some(b) = too_large_digit {
        Err(format!(
            "Illegal digit '{}' for base {}",
            Latin1String::new(&[b]),
            base
        )
        .to_string())
    } else if let Some(result) = result {
        Ok(result)
    } else {
        Err("Integer too large for 64-bit unsigned".to_string())
    }
}

fn parse_exponent(reader: &mut ContentReader) -> Result<i32, String> {
    let negative = {
        if reader.peek(0) == Some(b'-') {
            reader.pop();
            true
        } else {
            reader.skip_if(b'+');
            false
        }
    };

    let exp = parse_integer(reader, 10, false)?;
    if negative {
        if exp <= (-(i32::min_value() as i64)) as u64 {
            return Ok((-(exp as i64)) as i32);
        }
    } else {
        if exp <= i32::max_value() as u64 {
            return Ok(exp as i32);
        }
    }

    Err("Exponent too large for 32-bits signed".to_string())
}

fn exponentiate(value: u64, exp: u32) -> Option<u64> {
    // @TODO use checked_pow once it is common in recent releases
    // (10 as u64)
    //     .checked_pow(exp)
    //     and_then(|x| x.checked_mul(value))

    let mut value = value;
    for _ in 0..exp {
        value = value.checked_mul(10)?;
    }
    Some(value)
}

#[derive(PartialEq, Clone, Copy)]
pub struct TokenState {
    last_token_kind: Option<Kind>,
    start: Position,
}

impl TokenState {
    pub fn new(start: Position) -> TokenState {
        TokenState {
            last_token_kind: None,
            start,
        }
    }

    /// Set state to after token
    pub fn set_after(&mut self, token: &Token) {
        self.last_token_kind = Some(token.kind);
        self.start = token.next_pos;
    }
}

// Assumes first quoute is already consumed
fn parse_quoted(
    buffer: &mut Latin1String,
    reader: &mut ContentReader,
    quote: u8,
    include_quote: bool,
) -> Result<Latin1String, String> {
    buffer.bytes.clear();
    let mut is_multiline = false;
    let mut found_end = false;

    if include_quote {
        buffer.bytes.push(quote)
    }

    while let Some(chr) = reader.pop() {
        is_multiline |= chr == b'\n';
        if chr == quote {
            if reader.peek(0) == Some(quote) {
                reader.pop();
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

// Assumes first quote is already consumed
fn parse_string(
    buffer: &mut Latin1String,
    reader: &mut ContentReader,
) -> Result<Latin1String, String> {
    parse_quoted(buffer, reader, b'"', false)
}

/// Assume -- has already been seen but not consumed
fn parse_comment(buffer: &mut Latin1String, reader: &mut ContentReader) -> Comment {
    let start_pos = reader.pos();
    reader.pop();
    reader.pop();
    buffer.bytes.clear();
    while let Some(chr) = reader.peek(0) {
        if chr == b'\n' {
            break;
        } else {
            reader.pop();
            buffer.bytes.push(chr);
        }
    }
    let end_pos = reader.pos();
    Comment {
        value: buffer.clone(),
        range: start_pos.range_to(end_pos),
        multi_line: false,
    }
}

/// Assume /* has been seen but not consumed
fn parse_multi_line_comment(
    source: &Source,
    buffer: &mut Latin1String,
    reader: &mut ContentReader,
) -> ParseResult<Comment> {
    let start_pos = reader.pos();
    reader.pop();
    reader.pop();
    buffer.bytes.clear();
    while let Some(chr) = reader.pop() {
        if chr == b'*' {
            if reader.skip_if(b'/') {
                // Comment ended
                let end_pos = reader.pos();
                return Ok(Comment {
                    value: buffer.clone(),
                    range: start_pos.range_to(end_pos),
                    multi_line: true,
                });
            } else {
                buffer.bytes.push(chr);
            }
        } else {
            buffer.bytes.push(chr);
        }
    }

    let end_pos = reader.pos();
    Err(Diagnostic::error(
        source.pos(start_pos, end_pos),
        "Incomplete multi-line comment",
    ))
}

fn parse_real_literal(
    buffer: &mut Latin1String,
    reader: &mut ContentReader,
) -> Result<f64, String> {
    buffer.bytes.clear();

    while let Some(b) = reader.peek(0).map(Latin1String::lowercase) {
        match b {
            b'e' => {
                break;
            }
            b'E' => {
                break;
            }
            b'0'..=b'9' | b'a'..=b'd' | b'f' | b'A'..=b'F' | b'.' => {
                reader.pop();
                buffer.bytes.push(b);
            }
            b'_' => {
                reader.pop();
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
    reader: &mut ContentReader,
) -> Result<(Kind, Value), String> {
    let pos = reader.pos();
    let initial = parse_integer(reader, 10, true);

    match reader.peek(0).map(Latin1String::lowercase) {
        // Real
        Some(b'.') => {
            reader.set_pos(pos);
            let real = parse_real_literal(buffer, reader)?;

            match reader.peek(0) {
                // Exponent
                Some(b'e') | Some(b'E') => {
                    reader.pop();
                    let exp = parse_exponent(reader)?;
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
        Some(b'e') => {
            let integer = initial?;
            reader.pop();
            let exp = parse_exponent(reader)?;
            if exp >= 0 {
                if let Some(value) = exponentiate(integer, exp as u32) {
                    Ok((
                        AbstractLiteral,
                        Value::AbstractLiteral(ast::AbstractLiteral::Integer(value)),
                    ))
                } else {
                    Err("Integer too large for 64-bit unsigned".to_string())
                }
            } else {
                Err("Integer literals may not have negative exponent".to_string())
            }
        }

        // Based integer
        Some(b'#') => {
            let base = initial?;
            reader.pop();
            let base_result = parse_integer(reader, base, false);

            if let Some(b'#') = reader.peek(0) {
                reader.pop();
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

        // Bit string literal
        Some(b's') | Some(b'u') | Some(b'b') | Some(b'o') | Some(b'x') | Some(b'd') => {
            let integer = initial?;

            if let Some(base_spec) = parse_base_specifier(reader) {
                // @TODO check overflow
                parse_bit_string(buffer, reader, base_spec, Some(integer as u32))
            } else {
                Err("Invalid bit string literal".to_string())
            }
        }
        _ => {
            // Plain integer
            Ok((
                AbstractLiteral,
                Value::AbstractLiteral(ast::AbstractLiteral::Integer(initial?)),
            ))
        }
    }
}

/// LRM 15.8 Bit string literals
/// Parse the base specifier such as ub, sx, b etc
/// Also requires and consumes the trailing quoute "
fn parse_base_specifier(reader: &mut ContentReader) -> Option<BaseSpecifier> {
    let base_specifier = match reader.pop_lowercase()? {
        b'u' => match reader.pop_lowercase()? {
            b'b' => BaseSpecifier::UB,
            b'o' => BaseSpecifier::UO,
            b'x' => BaseSpecifier::UX,
            _ => return None,
        },
        b's' => match reader.pop_lowercase()? {
            b'b' => BaseSpecifier::SB,
            b'o' => BaseSpecifier::SO,
            b'x' => BaseSpecifier::SX,
            _ => return None,
        },
        b'b' => BaseSpecifier::B,
        b'o' => BaseSpecifier::O,
        b'x' => BaseSpecifier::X,
        b'd' => BaseSpecifier::D,
        _ => return None,
    };

    if reader.pop()? == b'"' {
        Some(base_specifier)
    } else {
        None
    }
}

// Only consume reader if it is a base specifier
fn maybe_base_specifier(reader: &mut ContentReader) -> Option<BaseSpecifier> {
    let mut lookahead = reader.clone();
    let value = parse_base_specifier(&mut lookahead)?;
    *reader = lookahead;
    Some(value)
}

fn parse_bit_string(
    buffer: &mut Latin1String,
    reader: &mut ContentReader,
    base_specifier: BaseSpecifier,
    bit_string_length: Option<u32>,
) -> Result<(Kind, Value), String> {
    let value = match parse_string(buffer, reader) {
        Ok(value) => value,
        Err(_) => return Err("Invalid bit string literal".to_string()),
    };

    Ok((
        BitString,
        Value::BitString(ast::BitString {
            length: bit_string_length,
            base: base_specifier,
            value,
        }),
    ))
}

/// LRM 15.4 Identifiers
fn parse_basic_identifier_or_keyword(
    buffer: &mut Latin1String,
    reader: &mut ContentReader,
    keywords: &FnvHashMap<&'static [u8], Kind>,
    symtab: &SymbolTable,
) -> Result<(Kind, Value), String> {
    let start_pos = reader.pos();
    while let Some(b) = reader.peek(0) {
        match b {
            b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_' => {
                reader.pop();
            }
            _ => {
                break;
            }
        }
    }

    buffer.bytes.clear();
    buffer
        .bytes
        .extend_from_slice(reader.slice(start_pos, reader.pos()));
    buffer.make_lowercase();

    match keywords.get(buffer.bytes.as_slice()) {
        Some(kind) => Ok((*kind, Value::NoValue)),
        None => {
            buffer.bytes.clear();
            buffer
                .bytes
                .extend_from_slice(reader.slice(start_pos, reader.pos()));
            Ok((Identifier, Value::Identifier(symtab.insert(&buffer))))
        }
    }
}

fn get_leading_comments(
    source: &Source,
    buffer: &mut Latin1String,
    reader: &mut ContentReader,
) -> ParseResult<Vec<Comment>> {
    let mut comments: Vec<Comment> = Vec::new();
    while let Some(byte) = reader.peek(0) {
        match byte {
            b' ' | b'\t' | b'\n' => {
                reader.pop();
                continue;
            }
            b'/' => {
                if reader.peek(1) == Some(b'*') {
                    comments.push(parse_multi_line_comment(source, buffer, reader)?);
                } else {
                    break;
                }
            }
            b'-' => {
                if reader.peek(1) == Some(b'-') {
                    comments.push(parse_comment(buffer, reader));
                } else {
                    break;
                }
            }
            _ => {
                break;
            }
        };
    }
    Ok(comments)
}

fn get_trailing_comment(buffer: &mut Latin1String, reader: &mut ContentReader) -> Option<Comment> {
    while let Some(byte) = reader.peek(0) {
        let comment = match byte {
            b' ' | b'\t' => {
                reader.pop();
                continue;
            }
            b'\n' => None,
            b'-' => {
                if reader.peek(1) == Some(b'-') {
                    Some(parse_comment(buffer, reader))
                } else {
                    None
                }
            }
            _ => None,
        };
        return comment;
    }
    // This should never happen.
    // FIXME: There must be a nicer way to write this function.
    None
}

pub struct Tokenizer<'a> {
    keywords: FnvHashMap<&'static [u8], Kind>,
    symtab: Arc<SymbolTable>,
    buffer: Latin1String,
    state: TokenState,
    source: &'a Source,
    reader: ContentReader<'a>,
    final_comments: Option<Vec<Comment>>,
    pub range_sym: Symbol,
    pub reverse_range_sym: Symbol,
}

impl<'a> Tokenizer<'a> {
    pub fn new(
        symtab: Arc<SymbolTable>,
        source: &'a Source,
        reader: ContentReader<'a>,
    ) -> Tokenizer<'a> {
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
            ("label", Label),
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
            ("force", Force),
            ("release", Release),
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

        let range_sym = symtab.insert(&Latin1String::new(b"range"));
        let reverse_range_sym = symtab.insert(&Latin1String::new(b"reverse_range"));

        Tokenizer {
            keywords,
            symtab,
            state: TokenState::new(reader.pos()),
            buffer: Latin1String::empty(),
            source,
            reader,
            final_comments: None,
            range_sym,
            reverse_range_sym,
        }
    }

    pub fn state(&self) -> TokenState {
        self.state
    }

    pub fn eof_error(&self) -> Diagnostic {
        Diagnostic::error(
            self.source
                .pos(self.state.start, self.state.start.next_char()),
            "Unexpected EOF",
        )
    }

    pub fn set_state(&mut self, state: TokenState) {
        self.state = state;
        self.reader.set_pos(state.start);
    }

    pub fn move_after(&mut self, token: &Token) {
        self.state.set_after(token);
        self.reader.set_pos(self.state.start);
    }

    pub fn parse_token(&mut self) -> Result<Option<(Kind, Value)>, Diagnostic> {
        macro_rules! error {
            ($message:expr) => {
                let err = Err(Diagnostic::error(
                    &self.source.pos(self.state.start, self.reader.pos()),
                    $message,
                ));
                self.state.start = self.reader.pos();
                return err;
            };
        }

        let byte = if let Some(byte) = self.reader.peek(0) {
            byte
        } else {
            // End of file
            return Ok(None);
        };

        let (kind, value) = match byte {
            b'a'..=b'z' | b'A'..=b'Z' => {
                if let Some(base_spec) = maybe_base_specifier(&mut self.reader) {
                    match parse_bit_string(&mut self.buffer, &mut self.reader, base_spec, None) {
                        Ok((kind, bit_string)) => (kind, bit_string),
                        Err(msg) => {
                            error!(msg);
                        }
                    }
                } else {
                    match parse_basic_identifier_or_keyword(
                        &mut self.buffer,
                        &mut self.reader,
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
            b'0'..=b'9' => match parse_abstract_literal(&mut self.buffer, &mut self.reader) {
                Ok((kind, value)) => (kind, value),
                Err(msg) => {
                    error!(msg);
                }
            },
            b':' => {
                self.reader.pop();
                if self.reader.skip_if(b'=') {
                    (ColonEq, Value::NoValue)
                } else {
                    (Colon, Value::NoValue)
                }
            }
            b'\'' => {
                self.reader.pop();
                if can_be_char(self.state.last_token_kind) && self.reader.peek(1) == Some(b'\'') {
                    let chr = self.reader.pop().unwrap();
                    self.reader.pop();
                    (Character, Value::Character(chr))
                } else {
                    (Tick, Value::NoValue)
                }
            }
            b'-' => {
                self.reader.pop();
                (Minus, Value::NoValue)
            }
            b'"' => {
                self.reader.pop();
                match parse_string(&mut self.buffer, &mut self.reader) {
                    Ok(result) => (StringLiteral, Value::String(result)),
                    Err(msg) => {
                        error!(msg);
                    }
                }
            }
            b';' => {
                self.reader.pop();
                (SemiColon, Value::NoValue)
            }
            b'(' => {
                self.reader.pop();
                (LeftPar, Value::NoValue)
            }
            b')' => {
                self.reader.pop();
                (RightPar, Value::NoValue)
            }
            b'+' => {
                self.reader.pop();
                (Plus, Value::NoValue)
            }
            b'.' => {
                self.reader.pop();
                (Dot, Value::NoValue)
            }
            b'&' => {
                self.reader.pop();
                (Concat, Value::NoValue)
            }
            b',' => {
                self.reader.pop();
                (Comma, Value::NoValue)
            }
            b'=' => {
                self.reader.pop();
                if self.reader.skip_if(b'>') {
                    (RightArrow, Value::NoValue)
                } else {
                    (EQ, Value::NoValue)
                }
            }
            b'<' => {
                self.reader.pop();
                match self.reader.peek(0) {
                    Some(b'=') => {
                        self.reader.pop();
                        (LTE, Value::NoValue)
                    }
                    Some(b'>') => {
                        self.reader.pop();
                        (BOX, Value::NoValue)
                    }
                    Some(b'<') => {
                        self.reader.pop();
                        (LtLt, Value::NoValue)
                    }
                    _ => (LT, Value::NoValue),
                }
            }
            b'>' => {
                self.reader.pop();
                match self.reader.peek(0) {
                    Some(b'=') => {
                        self.reader.pop();
                        (GTE, Value::NoValue)
                    }
                    Some(b'>') => {
                        self.reader.pop();
                        (GtGt, Value::NoValue)
                    }
                    _ => (GT, Value::NoValue),
                }
            }
            b'/' => {
                self.reader.pop();

                if self.reader.skip_if(b'=') {
                    (NE, Value::NoValue)
                } else {
                    (Div, Value::NoValue)
                }
            }
            b'*' => {
                self.reader.pop();

                if self.reader.skip_if(b'*') {
                    (Pow, Value::NoValue)
                } else {
                    (Times, Value::NoValue)
                }
            }
            b'?' => {
                self.reader.pop();
                match self.reader.peek(0) {
                    Some(b'?') => {
                        self.reader.pop();
                        (QueQue, Value::NoValue)
                    }
                    Some(b'=') => {
                        self.reader.pop();
                        (QueEQ, Value::NoValue)
                    }
                    Some(b'/') => {
                        self.reader.pop();
                        if self.reader.skip_if(b'=') {
                            (QueNE, Value::NoValue)
                        } else {
                            error!("Illegal token");
                        }
                    }
                    Some(b'<') => {
                        self.reader.pop();
                        if self.reader.skip_if(b'=') {
                            (QueLTE, Value::NoValue)
                        } else {
                            (QueLT, Value::NoValue)
                        }
                    }
                    Some(b'>') => {
                        self.reader.pop();
                        if self.reader.skip_if(b'=') {
                            (QueGTE, Value::NoValue)
                        } else {
                            (QueGT, Value::NoValue)
                        }
                    }
                    _ => {
                        error!("Illegal token");
                    }
                }
            }
            b'^' => {
                self.reader.pop();
                (Circ, Value::NoValue)
            }
            b'@' => {
                self.reader.pop();
                (CommAt, Value::NoValue)
            }
            b'|' => {
                self.reader.pop();
                (Bar, Value::NoValue)
            }
            b'[' => {
                self.reader.pop();
                (LeftSquare, Value::NoValue)
            }
            b']' => {
                self.reader.pop();
                (RightSquare, Value::NoValue)
            }
            b'\\' => {
                self.reader.pop();
                // LRM 15.4.3 Extended identifers
                match parse_quoted(&mut self.buffer, &mut self.reader, b'\\', true) {
                    Ok(result) => {
                        let result = Value::Identifier(self.symtab.insert_extended(&result));
                        (Identifier, result)
                    }
                    Err(msg) => {
                        error!(msg);
                    }
                }
            }
            _ => {
                self.reader.pop();
                error!("Illegal token");
            }
        };
        Ok(Some((kind, value)))
    }

    pub fn pop(&mut self) -> ParseResult<Option<Token>> {
        let leading_comments =
            get_leading_comments(&self.source, &mut self.buffer, &mut self.reader)?;
        self.state.start = self.reader.pos();

        match self.parse_token() {
            Ok(Some((kind, value))) => {
                // Parsed a token.
                let pos_start = self.state.start;
                let pos_end = self.reader.pos();
                let trailing_comment = get_trailing_comment(&mut self.buffer, &mut self.reader);
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
                    next_pos: self.reader.pos(),
                    comments: token_comments,
                };
                self.move_after(&token);
                Ok(Some(token))
            }
            Err(diagnostic) => {
                // Got a tokenizing error.
                Err(diagnostic)
            }
            Ok(None) => {
                // End of file.
                self.final_comments = Some(leading_comments);
                Ok(None)
            }
        }
    }

    #[allow(dead_code)]
    pub fn get_final_comments(&self) -> Option<Vec<Comment>> {
        self.final_comments.clone()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_util::Code;
    use pretty_assertions::assert_eq;

    fn kinds(tokens: &[Token]) -> Vec<Kind> {
        tokens.iter().map(|ref tok| tok.kind.clone()).collect()
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
            .map(|tok| (tok.kind.clone(), tok.value.clone()))
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
                value: Value::NoValue,
                pos: code.s1("entity").pos(),
                next_pos: code.s1("entity ").end(),
                comments: None,
            }
        );

        assert_eq!(
            tokens[1],
            Token {
                kind: Identifier,
                value: Value::Identifier(code.symtab.insert_utf8("foo")),
                pos: code.s1("foo").pos(),
                next_pos: code.end(),
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
                value: Value::Identifier(code.symtab.insert_utf8("my_ident")),
                pos: code.pos(),
                next_pos: code.end(),
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
                value: Value::Identifier(code.symtab.insert_utf8("my_ident")),
                pos: code.pos(),
                next_pos: code.end(),
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
                value: Value::Identifier(code.symtab.insert_utf8("\\1$my_ident\\")),
                pos: code.pos(),
                next_pos: code.end(),
                comments: None,
            }]
        );
        let code = Code::new("\\my\\\\_ident\\");
        let tokens = code.tokenize();
        assert_eq!(
            tokens,
            vec![Token {
                kind: Identifier,
                value: Value::Identifier(code.symtab.insert_utf8("\\my\\_ident\\")),
                pos: code.pos(),
                next_pos: code.end(),
                comments: None,
            }]
        );
    }

    #[test]
    fn tokenize_many_identifiers() {
        let code = Code::new("my_ident my_other_ident");
        let tokens = code.tokenize();
        assert_eq!(
            tokens,
            vec![
                Token {
                    kind: Identifier,
                    value: Value::Identifier(code.symtab.insert_utf8("my_ident")),
                    pos: code.s1("my_ident").pos(),
                    next_pos: code.s1("my_ident ").end(),
                    comments: None,
                },
                Token {
                    kind: Identifier,
                    value: Value::Identifier(code.symtab.insert_utf8("my_other_ident")),
                    pos: code.s1("my_other_ident").pos(),
                    next_pos: code.end(),
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
        let code = Code::new("1e-1");
        let (tokens, _) = code.tokenize_result();

        assert_eq!(
            tokens,
            vec![Err(Diagnostic::error(
                &code.pos(),
                "Integer literals may not have negative exponent"
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
                ),
                (
                    AbstractLiteral,
                    Value::AbstractLiteral(ast::AbstractLiteral::Real(44.0))
                ),
                (
                    AbstractLiteral,
                    Value::AbstractLiteral(ast::AbstractLiteral::Real(2500.0))
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
        let code = Code::new("\"string\"");
        let tokens = code.tokenize();
        assert_eq!(
            tokens,
            vec![Token {
                kind: StringLiteral,
                value: Value::String(Latin1String::from_utf8_unchecked("string")),
                pos: code.pos(),
                next_pos: code.end(),
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
                next_pos: code.end(),
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
                    next_pos: code.s1("\"str\" ").end(),
                    comments: None,
                },
                Token {
                    kind: StringLiteral,
                    value: Value::String(Latin1String::from_utf8_unchecked("ing")),
                    pos: code.s1("\"ing\"").pos(),
                    next_pos: code.end(),
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
            vec![Err(Diagnostic::error(&code.pos(), "Multi line string"))]
        );
    }

    #[test]
    fn tokenize_string_literal_error_on_early_eof() {
        // End of file
        let code = Code::new("\"string");
        let (tokens, _) = code.tokenize_result();
        assert_eq!(
            tokens,
            vec![Err(Diagnostic::error(
                &code.pos(),
                "Reached EOF before end quote"
            ))]
        );
    }

    // @TODO test incorrect value for base, ex: 2x"ff"
    // @TODO test incorrect value for length ex: 2x"1111"
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

                    let code = format!("{}{}\"{}\"", length_str, base_spec, value);

                    let code = if upper_case {
                        code.to_ascii_uppercase()
                    } else {
                        code
                    };

                    let value = if upper_case {
                        value.to_ascii_uppercase()
                    } else {
                        value.to_owned()
                    };

                    let code = Code::new(code.as_str());
                    let tokens = code.tokenize();
                    assert_eq!(
                        tokens,
                        vec![Token {
                            kind: BitString,
                            value: Value::BitString(ast::BitString {
                                length: length_opt,
                                base: base,
                                value: Latin1String::from_utf8_unchecked(value.as_str())
                            }),
                            pos: code.pos(),
                            next_pos: code.end(),
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
            vec![Err(Diagnostic::error(
                &code.pos(),
                "Invalid bit string literal"
            ))]
        );

        let code = Code::new("10ux");
        let (tokens, _) = code.tokenize_result();
        assert_eq!(
            tokens,
            vec![Err(Diagnostic::error(
                &code.pos(),
                "Invalid bit string literal"
            ))]
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
    fn tokenize_illegal_integer() {
        let code = Code::new("100k");

        let (tokens, _) = code.tokenize_result();
        assert_eq!(
            tokens,
            vec![Err(Diagnostic::error(
                &code.pos(),
                "Invalid integer character 'k'"
            ))]
        );
    }

    #[test]
    fn tokenize_illegal_based_integer() {
        // Base may only be 2-16
        let code = Code::new("1#0#");
        let (tokens, _) = code.tokenize_result();
        assert_eq!(
            tokens,
            vec![Err(Diagnostic::error(
                &code.pos(),
                "Base must be at least 2 and at most 16, got 1"
            ))]
        );
        let code = Code::new("17#f#");
        let (tokens, _) = code.tokenize_result();
        assert_eq!(
            tokens,
            vec![Err(Diagnostic::error(
                &code.pos(),
                "Base must be at least 2 and at most 16, got 17"
            ))]
        );
        // May not use digit larger than or equal base
        let code = Code::new("3#3#");
        let (tokens, _) = code.tokenize_result();
        assert_eq!(
            tokens,
            vec![Err(Diagnostic::error(
                &code.pos(),
                "Illegal digit '3' for base 3"
            ))]
        );
        let code = Code::new("15#f#");
        let (tokens, _) = code.tokenize_result();
        assert_eq!(
            tokens,
            vec![Err(Diagnostic::error(
                &code.pos(),
                "Illegal digit 'f' for base 15"
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
            vec![Err(Diagnostic::error(
                &code.pos(),
                "Integer too large for 64-bit unsigned"
            ))]
        );

        let large_int = "1e100";
        let code = Code::new(large_int);
        let (tokens, _) = code.tokenize_result();
        assert_eq!(
            tokens,
            vec![Err(Diagnostic::error(
                &code.pos(),
                "Integer too large for 64-bit unsigned"
            ))]
        );

        let large_int = format!("1e{}", (i32::max_value() as i64) + 1).to_string();
        let code = Code::new(&large_int);
        let (tokens, _) = code.tokenize_result();
        assert_eq!(
            tokens,
            vec![Err(Diagnostic::error(
                &code.pos(),
                "Exponent too large for 32-bits signed"
            ))]
        );

        let large_int = format!("1.0e{}", (i32::min_value() as i64) - 1).to_string();
        let code = Code::new(&large_int);
        let (tokens, _) = code.tokenize_result();
        assert_eq!(
            tokens,
            vec![Err(Diagnostic::error(
                &code.pos(),
                "Exponent too large for 32-bits signed"
            ))]
        );

        let large_int = ((u64::max_value() as i128) + 1).to_string();
        let code = Code::new(&large_int);
        let (tokens, _) = code.tokenize_result();
        assert_eq!(
            tokens,
            vec![Err(Diagnostic::error(
                &code.pos(),
                "Integer too large for 64-bit unsigned"
            ))]
        );

        let large_int = u64::max_value().to_string();
        let code = Code::new(&large_int);
        let (tokens, _) = code.tokenize_result();
        assert_eq!(
            tokens,
            vec![Ok(Token {
                kind: AbstractLiteral,
                value: Value::AbstractLiteral(ast::AbstractLiteral::Integer(u64::max_value())),
                pos: code.pos(),
                next_pos: code.end(),
                comments: None,
            })]
        );
    }

    #[test]
    fn tokenize_illegal() {
        let code = Code::new("begin?end");
        let (tokens, _) = code.tokenize_result();
        assert_eq!(
            tokens,
            vec![
                Ok(Token {
                    kind: Begin,
                    value: Value::NoValue,
                    pos: code.s1("begin").pos(),
                    next_pos: code.s1("begin").end(),
                    comments: None,
                }),
                Err(Diagnostic::error(&code.s1("?"), "Illegal token")),
                Ok(Token {
                    kind: End,
                    value: Value::NoValue,
                    pos: code.s1("end").pos(),
                    next_pos: code.end(),
                    comments: None,
                }),
            ]
        );
    }

    #[test]
    fn test_match_token_kind() {
        let tokens = Code::new("entity").tokenize();
        let result = match_token_kind!(
            tokens[0],
            Identifier => Ok(1),
            Entity => Ok(2));
        assert_eq!(result, Ok(2));
    }

    #[test]
    fn test_match_token_kind_error() {
        let tokens = Code::new("+").tokenize();
        let result = match_token_kind!(
            tokens[0],
            Identifier => Ok(1),
            Entity => Ok(2));
        assert_eq!(result, Err(tokens[0].kinds_error(&[Identifier, Entity])));
    }

    #[test]
    fn test_match_token_kind_pattern_error() {
        let tokens = Code::new("+").tokenize();
        let result = match_token_kind!(
            tokens[0],
            Identifier | StringLiteral => Ok(1),
            Entity => Ok(2));
        assert_eq!(
            result,
            Err(tokens[0].kinds_error(&[Identifier, StringLiteral, Entity]))
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
                value: Latin1String::from_utf8_unchecked("final"),
                range: code.s1("--final").pos().range(),
                multi_line: false
            },]
        );
    }

    #[test]
    fn extract_incomplete_multi_line_comment() {
        let code = Code::new("/* final");
        let (tokens, final_comments) = code.tokenize_result();
        assert_eq!(
            tokens,
            vec![Err(Diagnostic::error(
                &code.s1("/* final"),
                "Incomplete multi-line comment"
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
                    value: Value::NoValue,
                    pos: code.s1("+").pos(),
                    next_pos: code.s1("+--this is still a plus").end(),
                    comments: Some(Box::new(TokenComments {
                        leading: vec![Comment {
                            value: Latin1String::from_utf8_unchecked("this is a plus"),
                            range: code.s1("--this is a plus").pos().range(),
                            multi_line: false
                        },],
                        trailing: Some(Comment {
                            range: code.s1("--this is still a plus").pos().range(),
                            value: Latin1String::from_utf8_unchecked("this is still a plus"),
                            multi_line: false
                        }),
                    })),
                }),
                Ok(Token {
                    kind: Minus,
                    value: Value::NoValue,
                    pos: minus_pos,
                    next_pos: code.s1("-- this is a minus").end(),
                    comments: Some(Box::new(TokenComments {
                        leading: vec![
                            Comment {
                                value: Latin1String::from_utf8_unchecked("- this is not a minus"),
                                range: code.s1("--- this is not a minus").pos().range(),
                                multi_line: false
                            },
                            Comment {
                                value: Latin1String::from_utf8_unchecked(" Neither is this"),
                                range: code.s1("-- Neither is this").pos().range(),
                                multi_line: false
                            },
                        ],
                        trailing: Some(Comment {
                            range: code.s1("-- this is a minus").pos().range(),
                            value: Latin1String::from_utf8_unchecked(" this is a minus"),
                            multi_line: false
                        }),
                    })),
                }),
            ]
        );
        assert_eq!(
            final_comments,
            vec![
                Comment {
                    value: Latin1String::from_utf8_unchecked(" a comment at the end of the file"),
                    range: code.s1("-- a comment at the end of the file").pos().range(),
                    multi_line: false
                },
                Comment {
                    value: Latin1String::from_utf8_unchecked(" and another one"),
                    range: code.s1("-- and another one").pos().range(),
                    multi_line: false
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
                value: Value::AbstractLiteral(ast::AbstractLiteral::Integer(2)),
                pos: code.s1("2").pos(),
                next_pos: code.s1("2").end(),
                comments: Some(Box::new(TokenComments {
                    leading: vec![Comment {
                        value: Latin1String::from_utf8_unchecked("foo\ncom*ment\nbar"),
                        range: code.s1("/*foo\ncom*ment\nbar*/").pos().range(),
                        multi_line: true
                    },],
                    trailing: None,
                })),
            }),]
        );
        assert_eq!(
            final_comments,
            vec![Comment {
                value: Latin1String::from_utf8_unchecked("final"),
                range: code.s1("/*final*/").pos().range(),
                multi_line: true
            },]
        );
    }
}
