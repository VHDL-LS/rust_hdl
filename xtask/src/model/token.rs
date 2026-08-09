// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2025, Lukas Scheller lukasscheller@icloud.com

// NOTE: TokenKind and Keyword are duplicated in vhdl_syntax/src/tokens/token_kind.rs.
// Making xtask depend on vhdl_syntax would create a chicken-and-egg issue (generated files
// may be absent or broken). Keep the two definitions in sync manually.

use convert_case::{Case, Casing};
use std::str::FromStr;

pub fn str_to_token_kind(s: &str) -> Result<TokenKind, strum::ParseError> {
    use TokenKind::*;
    Ok(match s {
        "+" => Plus,
        "-" => Minus,
        "=" => EQ,
        "/=" => NE,
        "<" => LT,
        ">" => GT,
        "<=" => LTE,
        ">=" => GTE,
        "?=" => QueEQ,
        "?/=" => QueNE,
        "?<" => QueLT,
        "?>" => QueGT,
        "?<=" => QueLTE,
        "?>=" => QueGTE,
        "?" => Que,
        "??" => QueQue,
        "*" => Times,
        "**" => Pow,
        "/" => Div,
        "'" => Tick,
        "(" => LeftPar,
        ")" => RightPar,
        "[" => LeftSquare,
        "]" => RightSquare,
        ";" => SemiColon,
        ":" => Colon,
        "|" => Bar,
        "." => Dot,
        "<>" => BOX,
        "<<" => LtLt,
        ">>" => GtGt,
        "^" => Circ,
        "@" => CommAt,
        "&" => Concat,
        "," => Comma,
        ":=" => ColonEq,
        "=>" => RightArrow,
        _ => return TokenKind::from_str(&s.to_case(Case::UpperCamel)),
    })
}

#[allow(clippy::upper_case_acronyms)]
#[derive(PartialEq, Eq, Copy, Clone, Debug, strum::Display, strum::EnumString)]
pub enum TokenKind {
    /// A keyword, such as `entity`, `architecture` or `abs`.
    #[strum(disabled)]
    Keyword(Keyword),

    Plus,  // +
    Minus, // -

    EQ,  // =
    NE,  // /=
    LT,  // <
    LTE, // <=
    GT,  // >
    GTE, // >=

    QueEQ,  // ?=
    QueNE,  // ?/=
    QueLT,  // ?<
    QueLTE, // ?<=
    QueGT,  // ?>
    QueGTE, // ?>=
    Que,    // ?
    QueQue, // ??

    Times, // *
    Pow,   // **
    Div,   // /

    Tick,        // '
    LeftPar,     // (
    RightPar,    // )
    LeftSquare,  // [
    RightSquare, // ]
    SemiColon,   // ;
    Colon,       // :
    Bar,         // |
    Dot,         // .
    BOX,         // <>
    LtLt,        // <<
    GtGt,        // >>
    Circ,        // ^
    CommAt,      // @
    Concat,      // &
    Comma,       // ,
    ColonEq,     // :=
    RightArrow,  // =>

    Identifier,
    AbstractLiteral,
    StringLiteral,
    BitStringLiteral,
    CharacterLiteral,
    ToolDirective,

    // Erroneous input
    /// String, extended identifier or based integer without final quotation char
    Unterminated,

    /// Unknown input
    ///
    /// Produced, for example, when there is an unknown char or illegal bit string
    Unknown,

    Eof,
}

impl TokenKind {
    /// The default name of this kind, i.e., the name used for accessors when the grammar does
    /// not label the token. `to_string()` cannot be used directly: the `Keyword` variant is
    /// disabled in strum and panics when formatted.
    pub fn default_name(&self) -> String {
        match self {
            TokenKind::Keyword(kw) => kw.to_string(),
            other => other.to_string(),
        }
    }
}

/// All available keywords in the latest (VHDL 2019) edition of VHDL
#[derive(PartialEq, Eq, Clone, Copy, Debug, strum::Display, strum::EnumString)]
pub enum Keyword {
    Abs,
    Access,
    After,
    Alias,
    All,
    And,
    Architecture,
    Array,
    Assert,
    Assume,
    Attribute,
    Begin,
    Block,
    Body,
    Buffer,
    Bus,
    Case,
    Component,
    Configuration,
    Constant,
    Context,
    Cover,
    Default,
    Disconnect,
    Downto,
    Else,
    Elsif,
    End,
    Entity,
    Exit,
    Fairness,
    File,
    For,
    Force,
    Function,
    Generate,
    Generic,
    Group,
    Guarded,
    If,
    Impure,
    In,
    Inertial,
    Inout,
    Is,
    Label,
    Library,
    Linkage,
    Literal,
    Loop,
    Map,
    Mod,
    Nand,
    New,
    Next,
    Nor,
    Not,
    Null,
    Of,
    On,
    Open,
    Or,
    Others,
    Out,
    Package,
    Parameter,
    Port,
    Postponed,
    Procedure,
    Process,
    Property,
    Protected,
    Private,
    Pure,
    Range,
    Record,
    Register,
    Reject,
    Release,
    Rem,
    Report,
    Restrict,
    Return,
    Rol,
    Ror,
    Select,
    Sequence,
    Severity,
    Signal,
    Shared,
    Sla,
    Sll,
    Sra,
    Srl,
    Strong,
    Subtype,
    Then,
    To,
    Transport,
    Type,
    Unaffected,
    Units,
    Until,
    Use,
    Variable,
    View,
    Vpgk,
    Vmode,
    Vprop,
    Vunit,
    Wait,
    When,
    While,
    With,
    Xnor,
    Xor,
}

impl TokenKind {
    /// The accessor name for a token of this kind, when it carries no grammar label.
    pub fn getter_name(&self) -> String {
        format!("{}_token", self.default_name().to_case(Case::Snake))
    }
}
