// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2024, Lukas Scheller lukasscheller@icloud.com

use crate::latin_1::Latin1Str;
use crate::standard::VHDLStandard;

#[derive(PartialEq, Eq, Copy, Clone, Debug)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum TokenKind {
    /// A keyword, such as `entity`, `architecture` or `abs`.
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

    /// Special End of File token.
    /// Has no source representation but may carry trivia
    Eof,
}

impl TokenKind {
    /// Checks whether this token is the special EoF (End of File) token.
    // Note: This is commonly used in the parser and therefore defined here.
    pub fn is_eof(&self) -> bool {
        self == &TokenKind::Eof
    }
}

/// All available keywords in the latest (VHDL 2019) edition of VHDL
#[derive(PartialEq, Eq, Clone, Copy, Debug)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
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

impl Keyword {
    /// Returns the keyword corresponding to the given Latin-1 string, or `None` if the string
    /// is not a keyword. The comparison is case-insensitive.
    pub fn from_latin1(s: &Latin1Str) -> Option<Self> {
        Some(match s.to_lowercase().as_bytes() {
            b"abs" => Self::Abs,
            b"access" => Self::Access,
            b"after" => Self::After,
            b"alias" => Self::Alias,
            b"all" => Self::All,
            b"and" => Self::And,
            b"architecture" => Self::Architecture,
            b"array" => Self::Array,
            b"assert" => Self::Assert,
            b"assume" => Self::Assume,
            b"attribute" => Self::Attribute,
            b"begin" => Self::Begin,
            b"block" => Self::Block,
            b"body" => Self::Body,
            b"buffer" => Self::Buffer,
            b"bus" => Self::Bus,
            b"case" => Self::Case,
            b"component" => Self::Component,
            b"configuration" => Self::Configuration,
            b"constant" => Self::Constant,
            b"context" => Self::Context,
            b"cover" => Self::Cover,
            b"default" => Self::Default,
            b"disconnect" => Self::Disconnect,
            b"downto" => Self::Downto,
            b"else" => Self::Else,
            b"elsif" => Self::Elsif,
            b"end" => Self::End,
            b"entity" => Self::Entity,
            b"exit" => Self::Exit,
            b"fairness" => Self::Fairness,
            b"file" => Self::File,
            b"for" => Self::For,
            b"force" => Self::Force,
            b"function" => Self::Function,
            b"generate" => Self::Generate,
            b"generic" => Self::Generic,
            b"group" => Self::Group,
            b"guarded" => Self::Guarded,
            b"if" => Self::If,
            b"impure" => Self::Impure,
            b"in" => Self::In,
            b"inertial" => Self::Inertial,
            b"inout" => Self::Inout,
            b"is" => Self::Is,
            b"label" => Self::Label,
            b"library" => Self::Library,
            b"linkage" => Self::Linkage,
            b"literal" => Self::Literal,
            b"loop" => Self::Loop,
            b"map" => Self::Map,
            b"mod" => Self::Mod,
            b"nand" => Self::Nand,
            b"new" => Self::New,
            b"next" => Self::Next,
            b"nor" => Self::Nor,
            b"not" => Self::Not,
            b"null" => Self::Null,
            b"of" => Self::Of,
            b"on" => Self::On,
            b"open" => Self::Open,
            b"or" => Self::Or,
            b"others" => Self::Others,
            b"out" => Self::Out,
            b"package" => Self::Package,
            b"parameter" => Self::Parameter,
            b"port" => Self::Port,
            b"postponed" => Self::Postponed,
            b"procedure" => Self::Procedure,
            b"process" => Self::Process,
            b"property" => Self::Property,
            b"protected" => Self::Protected,
            b"private" => Self::Private,
            b"pure" => Self::Pure,
            b"range" => Self::Range,
            b"record" => Self::Record,
            b"register" => Self::Register,
            b"reject" => Self::Reject,
            b"release" => Self::Release,
            b"rem" => Self::Rem,
            b"report" => Self::Report,
            b"restrict" => Self::Restrict,
            b"return" => Self::Return,
            b"rol" => Self::Rol,
            b"ror" => Self::Ror,
            b"select" => Self::Select,
            b"sequence" => Self::Sequence,
            b"severity" => Self::Severity,
            b"signal" => Self::Signal,
            b"shared" => Self::Shared,
            b"sla" => Self::Sla,
            b"sll" => Self::Sll,
            b"sra" => Self::Sra,
            b"srl" => Self::Srl,
            b"strong" => Self::Strong,
            b"subtype" => Self::Subtype,
            b"then" => Self::Then,
            b"to" => Self::To,
            b"transport" => Self::Transport,
            b"type" => Self::Type,
            b"unaffected" => Self::Unaffected,
            b"units" => Self::Units,
            b"until" => Self::Until,
            b"use" => Self::Use,
            b"variable" => Self::Variable,
            b"view" => Self::View,
            b"vpgk" => Self::Vpgk,
            b"vmode" => Self::Vmode,
            b"vprop" => Self::Vprop,
            b"vunit" => Self::Vunit,
            b"wait" => Self::Wait,
            b"when" => Self::When,
            b"while" => Self::While,
            b"with" => Self::With,
            b"xnor" => Self::Xnor,
            b"xor" => Self::Xor,
            _ => return None,
        })
    }

    /// Returns the VHDL standard that first reserved this keyword.
    pub fn introduced_in(self) -> VHDLStandard {
        use VHDLStandard::*;
        match self {
            // VHDL 1993
            Self::Group
            | Self::Impure
            | Self::Inertial
            | Self::Literal
            | Self::Postponed
            | Self::Pure
            | Self::Reject
            | Self::Rol
            | Self::Ror
            | Self::Shared
            | Self::Sla
            | Self::Sll
            | Self::Sra
            | Self::Srl
            | Self::Unaffected
            | Self::Xnor => VHDL1993,
            // VHDL 2000
            // Also introduces keywords "Procedural" and "Reference"
            // However as they only appear in this specific standard and are revoked
            // later we will not treat them special unless there is a feature request to support this
            Self::Protected => VHDL2000,
            // VHDL 2008
            // Similar cases as for VHDL2000 (the following keywords are only valid on 2008 and are thus omitted):
            // "assume_guarantee", "restrict_guarantee"
            Self::Assume
            | Self::Context
            | Self::Cover
            | Self::Default
            | Self::Fairness
            | Self::Force
            | Self::Parameter
            | Self::Property
            | Self::Release
            | Self::Restrict
            | Self::Sequence
            | Self::Strong
            | Self::Vmode
            | Self::Vprop
            | Self::Vunit => VHDL2008,
            // VHDL 2019
            Self::View | Self::Private | Self::Vpgk => VHDL2019,
            // Everything else: VHDL 1987
            _ => VHDL1987,
        }
    }
}
