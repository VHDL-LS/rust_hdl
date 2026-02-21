// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2026, Lukas Scheller lukasscheller@icloud.com

//! Domain types for the builder API. Each type has named constructors that accept
//! Rust values and produces a VHDL token.
//!
//! Use `.with_trivia(Trivia)` for full trivia control (default: one leading space).
//!
//! Each domain type implements `From<Token>` as an escape hatch for passing tokens from a parsed AST
//! (panics at runtime if the token kind doesn't match).
//!
//! # Example
//!
//! ```
//! use vhdl_syntax::builder::Identifier;
//! use vhdl_syntax::syntax::{AstNode, EntityDeclarationPreambleBuilder};
//!
//! let node = EntityDeclarationPreambleBuilder::new(Identifier::from(b"my_entity")).build();
//! assert_eq!(node.raw().to_string(), " entity my_entity is");
//! ```

use crate::latin_1::{char_to_latin1, Latin1Str, Latin1String, NonLatin1CharError};
use crate::tokens::{Token, TokenKind, Trivia, TriviaPiece};

fn default_trivia() -> Trivia {
    Trivia::from([TriviaPiece::Spaces(1)])
}

macro_rules! domain_type {
    ($(#[$meta:meta])* struct $name:ident($token_kind:expr);) => {

        $(#[$meta])*
        pub struct $name(Token);

        impl $name {
            /// Override the leading trivia (default: one space).
            pub fn with_trivia(mut self, trivia: Trivia) -> Self {
                self.0.set_leading_trivia(trivia);
                self
            }
        }

        impl From<$name> for Token {
            fn from(value: $name) -> Token {
                value.0
            }
        }

        impl From<Token> for $name {
            /// Escape hatch: wrap a token from a parsed AST.
            ///
            /// # Panics
            ///
            /// Panics for incorrect token kinds.
            fn from(token: Token) -> Self {
                assert_eq!(token.kind(), $token_kind);
                $name(token)
            }
        }
    };
}

// MARK: Identifier

domain_type!(
    /// A VHDL identifier for use in builder methods.
    ///
    /// # Examples
    ///
    /// ```
    /// use vhdl_syntax::builder::Identifier;
    /// use vhdl_syntax::latin_1::Latin1String;
    ///
    /// // Canonical initializer: from a Latin1String
    /// let id = Identifier::new(Latin1String::from(b"my_entity"));
    ///
    /// // From raw bytes
    /// let id = Identifier::from(b"my_entity");
    ///
    /// // Fallible: from a UTF-8 string
    /// let id = Identifier::try_from("my_entity").unwrap();
    /// ```
    struct Identifier(TokenKind::Identifier);
);

impl Identifier {
    /// Create an identifier from a `&Latin1Str`.
    pub fn new(value: Latin1String) -> Self {
        Identifier(Token::new(TokenKind::Identifier, value, default_trivia()))
    }
}

impl From<&[u8]> for Identifier {
    fn from(value: &[u8]) -> Self {
        Identifier::new(Latin1String::from(value))
    }
}

impl<const N: usize> From<[u8; N]> for Identifier {
    fn from(value: [u8; N]) -> Self {
        Identifier::new(Latin1String::from(&value))
    }
}

impl<const N: usize> From<&[u8; N]> for Identifier {
    fn from(value: &[u8; N]) -> Self {
        Identifier::new(Latin1String::from(value))
    }
}

impl TryFrom<&str> for Identifier {
    type Error = crate::latin_1::Utf8ToLatin1Error;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Ok(Identifier::new(Latin1String::from_utf8(value)?))
    }
}

impl TryFrom<String> for Identifier {
    type Error = crate::latin_1::Utf8ToLatin1Error;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        Ok(Identifier::new(Latin1String::from_utf8(&value)?))
    }
}

// MARK: AbstractLiteral

domain_type!(
    /// A VHDL abstract literal (integer or real) for use in builder methods.
    ///
    /// # Examples
    ///
    /// ```
    /// use vhdl_syntax::builder::AbstractLiteral;
    ///
    /// let int_lit = AbstractLiteral::integer(42);
    /// let real_lit = AbstractLiteral::real(2.5);
    /// ```
    struct AbstractLiteral(TokenKind::AbstractLiteral);
);

impl AbstractLiteral {
    fn new(text: impl Into<Box<Latin1Str>>) -> Self {
        AbstractLiteral(Token::new(
            TokenKind::AbstractLiteral,
            text,
            default_trivia(),
        ))
    }

    /// Create an integer abstract literal.
    ///
    /// # Example
    ///
    /// ```
    /// use vhdl_syntax::builder::AbstractLiteral;
    /// use vhdl_syntax::tokens::{Token, TokenKind};
    ///
    /// let tok: Token = AbstractLiteral::integer(42).into();
    /// assert_eq!(tok.text().to_string(), "42");
    /// ```
    pub fn integer(value: u64) -> Self {
        Self::new(Latin1Str::new(format!("{}", value).as_bytes()))
    }

    /// Create a real abstract literal.
    ///
    /// Guarantees a decimal point is present: `1.0_f64` produces `"1.0"`, not `"1"`.
    ///
    /// # Example
    ///
    /// ```
    /// use vhdl_syntax::builder::AbstractLiteral;
    /// use vhdl_syntax::tokens::{Token, TokenKind};
    ///
    /// let tok: Token = AbstractLiteral::real(1.0).into();
    /// assert_eq!(tok.text().to_string(), "1.0");
    ///
    /// let tok: Token = AbstractLiteral::real(2.5).into();
    /// assert_eq!(tok.text().to_string(), "2.5");
    /// ```
    pub fn real(value: f64) -> Self {
        let s = format!("{}", value);
        let text = if s.contains('.') || s.contains('e') || s.contains('E') {
            s
        } else {
            format!("{}.0", s)
        };
        Self::new(Latin1Str::new(text.as_bytes()))
    }
}

// MARK: StringLiteral

domain_type!(
    /// A VHDL string literal for use in builder methods.
    ///
    /// The caller supplies the raw content (no surrounding quotes). The library
    /// adds the outer VHDL double-quotes and doubles any embedded `"` per VHDL rules.
    ///
    /// # Examples
    ///
    /// ```
    /// use vhdl_syntax::builder::StringLiteral;
    /// use vhdl_syntax::tokens::{Token, TokenKind};
    ///
    /// let tok: Token = StringLiteral::new("say \"hi\"").into();
    /// assert_eq!(tok.text().to_string(), "\"say \"\"hi\"\"\"");
    /// ```
    struct StringLiteral(TokenKind::StringLiteral);
);

impl StringLiteral {
    /// Create a string literal from raw content (no surrounding quotes).
    ///
    /// Any embedded `"` in the content is doubled per VHDL rules.
    pub fn new(content: &str) -> Self {
        let mut encoded = Latin1String::with_capacity(content.len() + 2);
        encoded.push(b'"');
        for byte in content.bytes() {
            if byte == b'"' {
                encoded.push(b'"');
                encoded.push(b'"');
            } else {
                encoded.push(byte);
            }
        }
        encoded.push(b'"');
        StringLiteral(Token::new(
            TokenKind::StringLiteral,
            encoded,
            default_trivia(),
        ))
    }
}

// MARK: CharLiteral

domain_type!(
    /// A VHDL character literal for use in builder methods.
    ///
    /// Holds a single Latin-1 byte and emits it wrapped in single quotes.
    ///
    /// # Examples
    ///
    /// ```
    /// use vhdl_syntax::builder::CharLiteral;
    /// use vhdl_syntax::tokens::{Token, TokenKind};
    ///
    /// let tok: Token = CharLiteral::new(b'A').into();
    /// assert_eq!(tok.text().to_string(), "'A'");
    ///
    /// // Quote character itself
    /// let tok: Token = CharLiteral::new(b'\'').into();
    /// assert_eq!(tok.text().to_string(), "'''");
    /// ```
    struct CharLiteral(TokenKind::CharacterLiteral);
);

impl TryFrom<char> for CharLiteral {
    type Error = NonLatin1CharError;

    fn try_from(value: char) -> Result<Self, Self::Error> {
        Ok(CharLiteral::new(char_to_latin1(value)?))
    }
}

impl CharLiteral {
    /// Create a character literal from a Latin-1 byte.
    pub fn new(byte: u8) -> Self {
        let text = [b'\'', byte, b'\''];
        CharLiteral(Token::new(
            TokenKind::CharacterLiteral,
            &text,
            default_trivia(),
        ))
    }
}

// MARK: BitStringLiteral

domain_type!(
    /// A VHDL bit-string literal for use in builder methods.
    ///
    /// # Examples
    ///
    /// ```
    /// use vhdl_syntax::builder::BitStringLiteral;
    /// use vhdl_syntax::tokens::{Token, TokenKind};
    ///
    /// let tok: Token = BitStringLiteral::hex(b"FF").into();
    /// assert_eq!(tok.text().to_string(), r#"X"FF""#);
    ///
    /// let tok: Token = BitStringLiteral::binary(b"1010").into();
    /// assert_eq!(tok.text().to_string(), r#"B"1010""#);
    /// ```
    struct BitStringLiteral(TokenKind::BitStringLiteral);
);

impl BitStringLiteral {
    fn new(prefix: &Latin1Str, digits: &Latin1Str) -> Self {
        let mut text = prefix.to_latin1_string();
        text.push(b'"');
        text.extend(digits);
        text.push(b'"');
        BitStringLiteral(Token::new(
            TokenKind::BitStringLiteral,
            text,
            default_trivia(),
        ))
    }

    /// Binary bit-string literal: `B"1010"`.
    pub fn binary(digits: impl AsRef<Latin1Str>) -> Self {
        Self::new(Latin1Str::new(b"B"), digits.as_ref())
    }

    /// Unsigned binary bit-string literal: `UB"1010"`.
    pub fn binary_unsigned(digits: impl AsRef<Latin1Str>) -> Self {
        Self::new(Latin1Str::new(b"UB"), digits.as_ref())
    }

    /// Signed binary bit-string literal: `SB"1010"`.
    pub fn binary_signed(digits: impl AsRef<Latin1Str>) -> Self {
        Self::new(Latin1Str::new(b"SB"), digits.as_ref())
    }

    /// Octal bit-string literal: `O"77"`.
    pub fn octal(digits: impl AsRef<Latin1Str>) -> Self {
        Self::new(Latin1Str::new(b"O"), digits.as_ref())
    }

    /// Unsigned octal bit-string literal: `O"77"`.
    pub fn octal_unsigned(digits: impl AsRef<Latin1Str>) -> Self {
        Self::new(Latin1Str::new(b"UO"), digits.as_ref())
    }

    /// Signed octal bit-string literal: `SO"77"`.
    pub fn octal_signed(digits: impl AsRef<Latin1Str>) -> Self {
        Self::new(Latin1Str::new(b"SO"), digits.as_ref())
    }

    /// Hexadecimal bit-string literal: `X"FF"`.
    pub fn hex(digits: impl AsRef<Latin1Str>) -> Self {
        Self::new(Latin1Str::new(b"X"), digits.as_ref())
    }

    /// Unsigned hexadecimal bit-string literal: `UX"FF"`.
    pub fn hex_unsigned(digits: impl AsRef<Latin1Str>) -> Self {
        Self::new(Latin1Str::new(b"UX"), digits.as_ref())
    }

    /// Signed hexadecimal bit-string literal: `SX"FF"`.
    pub fn hex_signed(digits: impl AsRef<Latin1Str>) -> Self {
        Self::new(Latin1Str::new(b"SX"), digits.as_ref())
    }

    /// Decimal bit-string literal: `D"10"`.
    pub fn decimal(digits: impl AsRef<Latin1Str>) -> Self {
        Self::new(Latin1Str::new(b"D"), digits.as_ref())
    }
}
