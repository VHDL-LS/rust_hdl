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

    /// Returns the canonical text representation of this token kind, or `None` if the token
    /// kind has no fixed text (e.g. identifiers or literals).
    pub fn canonical_text(&self) -> Option<&'static Latin1Str> {
        match self {
            Self::Keyword(kw) => Some(kw.canonical_text()),
            Self::Plus => Some(Latin1Str::new(b"+")),
            Self::Minus => Some(Latin1Str::new(b"-")),
            Self::EQ => Some(Latin1Str::new(b"=")),
            Self::NE => Some(Latin1Str::new(b"/=")),
            Self::LT => Some(Latin1Str::new(b"<")),
            Self::LTE => Some(Latin1Str::new(b"<=")),
            Self::GT => Some(Latin1Str::new(b">")),
            Self::GTE => Some(Latin1Str::new(b">=")),
            Self::QueEQ => Some(Latin1Str::new(b"?=")),
            Self::QueNE => Some(Latin1Str::new(b"?/=")),
            Self::QueLT => Some(Latin1Str::new(b"?<")),
            Self::QueLTE => Some(Latin1Str::new(b"?<=")),
            Self::QueGT => Some(Latin1Str::new(b"?>")),
            Self::QueGTE => Some(Latin1Str::new(b"?>=")),
            Self::Que => Some(Latin1Str::new(b"?")),
            Self::QueQue => Some(Latin1Str::new(b"??")),
            Self::Times => Some(Latin1Str::new(b"*")),
            Self::Pow => Some(Latin1Str::new(b"**")),
            Self::Div => Some(Latin1Str::new(b"/")),
            Self::Tick => Some(Latin1Str::new(b"'")),
            Self::LeftPar => Some(Latin1Str::new(b"(")),
            Self::RightPar => Some(Latin1Str::new(b")")),
            Self::LeftSquare => Some(Latin1Str::new(b"[")),
            Self::RightSquare => Some(Latin1Str::new(b"]")),
            Self::SemiColon => Some(Latin1Str::new(b";")),
            Self::Colon => Some(Latin1Str::new(b":")),
            Self::Bar => Some(Latin1Str::new(b"|")),
            Self::Dot => Some(Latin1Str::new(b".")),
            Self::BOX => Some(Latin1Str::new(b"<>")),
            Self::LtLt => Some(Latin1Str::new(b"<<")),
            Self::GtGt => Some(Latin1Str::new(b">>")),
            Self::Circ => Some(Latin1Str::new(b"^")),
            Self::CommAt => Some(Latin1Str::new(b"@")),
            Self::Concat => Some(Latin1Str::new(b"&")),
            Self::Comma => Some(Latin1Str::new(b",")),
            Self::ColonEq => Some(Latin1Str::new(b":=")),
            Self::RightArrow => Some(Latin1Str::new(b"=>")),
            _ => None,
        }
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
    /// Returns the canonical (lowercase) text for this keyword.
    pub fn canonical_text(&self) -> &'static Latin1Str {
        match self {
            Self::Abs => Latin1Str::new(b"abs"),
            Self::Access => Latin1Str::new(b"access"),
            Self::After => Latin1Str::new(b"after"),
            Self::Alias => Latin1Str::new(b"alias"),
            Self::All => Latin1Str::new(b"all"),
            Self::And => Latin1Str::new(b"and"),
            Self::Architecture => Latin1Str::new(b"architecture"),
            Self::Array => Latin1Str::new(b"array"),
            Self::Assert => Latin1Str::new(b"assert"),
            Self::Assume => Latin1Str::new(b"assume"),
            Self::Attribute => Latin1Str::new(b"attribute"),
            Self::Begin => Latin1Str::new(b"begin"),
            Self::Block => Latin1Str::new(b"block"),
            Self::Body => Latin1Str::new(b"body"),
            Self::Buffer => Latin1Str::new(b"buffer"),
            Self::Bus => Latin1Str::new(b"bus"),
            Self::Case => Latin1Str::new(b"case"),
            Self::Component => Latin1Str::new(b"component"),
            Self::Configuration => Latin1Str::new(b"configuration"),
            Self::Constant => Latin1Str::new(b"constant"),
            Self::Context => Latin1Str::new(b"context"),
            Self::Cover => Latin1Str::new(b"cover"),
            Self::Default => Latin1Str::new(b"default"),
            Self::Disconnect => Latin1Str::new(b"disconnect"),
            Self::Downto => Latin1Str::new(b"downto"),
            Self::Else => Latin1Str::new(b"else"),
            Self::Elsif => Latin1Str::new(b"elsif"),
            Self::End => Latin1Str::new(b"end"),
            Self::Entity => Latin1Str::new(b"entity"),
            Self::Exit => Latin1Str::new(b"exit"),
            Self::Fairness => Latin1Str::new(b"fairness"),
            Self::File => Latin1Str::new(b"file"),
            Self::For => Latin1Str::new(b"for"),
            Self::Force => Latin1Str::new(b"force"),
            Self::Function => Latin1Str::new(b"function"),
            Self::Generate => Latin1Str::new(b"generate"),
            Self::Generic => Latin1Str::new(b"generic"),
            Self::Group => Latin1Str::new(b"group"),
            Self::Guarded => Latin1Str::new(b"guarded"),
            Self::If => Latin1Str::new(b"if"),
            Self::Impure => Latin1Str::new(b"impure"),
            Self::In => Latin1Str::new(b"in"),
            Self::Inertial => Latin1Str::new(b"inertial"),
            Self::Inout => Latin1Str::new(b"inout"),
            Self::Is => Latin1Str::new(b"is"),
            Self::Label => Latin1Str::new(b"label"),
            Self::Library => Latin1Str::new(b"library"),
            Self::Linkage => Latin1Str::new(b"linkage"),
            Self::Literal => Latin1Str::new(b"literal"),
            Self::Loop => Latin1Str::new(b"loop"),
            Self::Map => Latin1Str::new(b"map"),
            Self::Mod => Latin1Str::new(b"mod"),
            Self::Nand => Latin1Str::new(b"nand"),
            Self::New => Latin1Str::new(b"new"),
            Self::Next => Latin1Str::new(b"next"),
            Self::Nor => Latin1Str::new(b"nor"),
            Self::Not => Latin1Str::new(b"not"),
            Self::Null => Latin1Str::new(b"null"),
            Self::Of => Latin1Str::new(b"of"),
            Self::On => Latin1Str::new(b"on"),
            Self::Open => Latin1Str::new(b"open"),
            Self::Or => Latin1Str::new(b"or"),
            Self::Others => Latin1Str::new(b"others"),
            Self::Out => Latin1Str::new(b"out"),
            Self::Package => Latin1Str::new(b"package"),
            Self::Parameter => Latin1Str::new(b"parameter"),
            Self::Port => Latin1Str::new(b"port"),
            Self::Postponed => Latin1Str::new(b"postponed"),
            Self::Procedure => Latin1Str::new(b"procedure"),
            Self::Process => Latin1Str::new(b"process"),
            Self::Property => Latin1Str::new(b"property"),
            Self::Protected => Latin1Str::new(b"protected"),
            Self::Private => Latin1Str::new(b"private"),
            Self::Pure => Latin1Str::new(b"pure"),
            Self::Range => Latin1Str::new(b"range"),
            Self::Record => Latin1Str::new(b"record"),
            Self::Register => Latin1Str::new(b"register"),
            Self::Reject => Latin1Str::new(b"reject"),
            Self::Release => Latin1Str::new(b"release"),
            Self::Rem => Latin1Str::new(b"rem"),
            Self::Report => Latin1Str::new(b"report"),
            Self::Restrict => Latin1Str::new(b"restrict"),
            Self::Return => Latin1Str::new(b"return"),
            Self::Rol => Latin1Str::new(b"rol"),
            Self::Ror => Latin1Str::new(b"ror"),
            Self::Select => Latin1Str::new(b"select"),
            Self::Sequence => Latin1Str::new(b"sequence"),
            Self::Severity => Latin1Str::new(b"severity"),
            Self::Signal => Latin1Str::new(b"signal"),
            Self::Shared => Latin1Str::new(b"shared"),
            Self::Sla => Latin1Str::new(b"sla"),
            Self::Sll => Latin1Str::new(b"sll"),
            Self::Sra => Latin1Str::new(b"sra"),
            Self::Srl => Latin1Str::new(b"srl"),
            Self::Strong => Latin1Str::new(b"strong"),
            Self::Subtype => Latin1Str::new(b"subtype"),
            Self::Then => Latin1Str::new(b"then"),
            Self::To => Latin1Str::new(b"to"),
            Self::Transport => Latin1Str::new(b"transport"),
            Self::Type => Latin1Str::new(b"type"),
            Self::Unaffected => Latin1Str::new(b"unaffected"),
            Self::Units => Latin1Str::new(b"units"),
            Self::Until => Latin1Str::new(b"until"),
            Self::Use => Latin1Str::new(b"use"),
            Self::Variable => Latin1Str::new(b"variable"),
            Self::View => Latin1Str::new(b"view"),
            Self::Vpgk => Latin1Str::new(b"vpgk"),
            Self::Vmode => Latin1Str::new(b"vmode"),
            Self::Vprop => Latin1Str::new(b"vprop"),
            Self::Vunit => Latin1Str::new(b"vunit"),
            Self::Wait => Latin1Str::new(b"wait"),
            Self::When => Latin1Str::new(b"when"),
            Self::While => Latin1Str::new(b"while"),
            Self::With => Latin1Str::new(b"with"),
            Self::Xnor => Latin1Str::new(b"xnor"),
            Self::Xor => Latin1Str::new(b"xor"),
        }
    }

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
            // VHDL 2019
            Self::View | Self::Private | Self::Vpgk => VHDL2019,
            // VHDL 2008
            // Also introduces keywords "assume_guarantee" and "restrict_guarantee".
            // However as they only appear in this specific standard and are revoked later,
            // they are treated as identifiers unless there is a feature request to support this.
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
            // VHDL 2000
            // Also introduces keywords "Procedural" and "Reference"
            // Similar cases as for VHDL2000 (the keywords are only valid in 2008 and thus omitted).
            Self::Protected => VHDL2000,
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
            // Everything else: VHDL 1987
            _ => VHDL1987,
        }
    }
}
