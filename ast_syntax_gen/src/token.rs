// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com

use convert_case::{Case, Casing};
use proc_macro2::{Ident, Literal, TokenStream};
use quote::{format_ident, quote, ToTokens};
use std::str::FromStr;

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
}

impl TokenKind {
    pub fn from_str_expect(s: &str) -> TokenKind {
        TokenKind::from_str(s).unwrap_or_else(|_| panic!("Token kind {s} not valid"))
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

impl Keyword {
    pub fn from_str_expect(s: &str) -> Keyword {
        Keyword::from_str(s).unwrap_or_else(|_| panic!("Keyword {s} not valid"))
    }
}

impl TokenKind {
    pub fn build_expression(&self) -> proc_macro2::TokenStream {
        match self {
            TokenKind::Keyword(kw) => {
                let kw_name = format_ident!("{}", kw.to_string());
                quote! {
                    Keyword(Kw::#kw_name)
                }
            }
            _ => format_ident!("{}", self.to_string()).into_token_stream(),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    /// The name of the token; defines the function-name
    pub name: String,
    /// the occurrence of this token, i.e., whether this is the
    /// 1st, second, third, e.t.c. token in the parent node.
    pub nth: usize,
    /// whether this token can occur repeatedly
    pub repeated: bool,
}

impl From<TokenKind> for Token {
    fn from(kind: TokenKind) -> Self {
        Token {
            name: kind.to_string(),
            kind,
            repeated: false,
            nth: 0,
        }
    }
}

impl Token {
    pub fn getter_name(&self) -> Ident {
        format_ident!("{}_token", self.name.to_case(Case::Snake))
    }

    pub fn enum_variant_ident(&self) -> Ident {
        format_ident!("{}", self.name.to_case(Case::UpperCamel))
    }

    pub fn build_getter(&self) -> TokenStream {
        let function_name = self.getter_name();
        let kind_ident = self.kind.build_expression();
        let nth = Literal::usize_unsuffixed(self.nth);
        if self.repeated {
            assert_eq!(self.nth, 0, "{} multiple", self.name);
            quote! {
                pub fn #function_name(&self) -> impl Iterator<Item = SyntaxToken>  + use<'_> {
                    self.0
                        .tokens()
                        .filter(|token| token.kind() == #kind_ident)
                }
            }
        } else {
            quote! {
                pub fn #function_name(&self) -> Option<SyntaxToken> {
                    self.0
                        .tokens()
                        .filter(|token| token.kind() == #kind_ident)
                        .nth(#nth)
                }
            }
        }
    }
}
