// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2026, Lukas Scheller lukasscheller@icloud.com

//! Integration tests for the generated builder API.
//!
//! Each test builds a syntax node programmatically and verifies the emitted text.
//! All tokens default to a single leading space, so the text of a node built
//! entirely from defaults is a space-separated sequence of the keyword/symbol texts.

use vhdl_syntax::{
    builder::{
        AbstractLiteral, BitStringLiteral, CharLiteral, Identifier, StringLiteral,
    },
    parser,
    syntax::{
        ArchitectureEpilogueBuilder, AstNode, EntityDeclarationBuilder, EntityDeclarationEpilogueBuilder, EntityDeclarationPreambleBuilder, LibraryUnitSyntax, PrimaryUnitSyntax
    },
    tokens::{Token, TokenKind, Trivia},
};

// ── ArchitectureEpilogue ─────────────────────────────────────────────────────

/// A default-built epilogue has no optional tokens: just `end ;`.
#[test]
fn arch_epilogue_default_text() {
    let node = ArchitectureEpilogueBuilder::default().build();
    assert_eq!(node.raw().to_string(), " end ;");
}

/// Optional tokens can be added with `with_*` setters.
#[test]
fn arch_epilogue_with_optional_tokens() {
    use vhdl_syntax::tokens::Keyword;
    let arch_tok = Token::new(
        TokenKind::Keyword(Keyword::Architecture),
        Keyword::Architecture.canonical_text(),
        Trivia::from([vhdl_syntax::tokens::TriviaPiece::Spaces(1)]),
    );
    let node = ArchitectureEpilogueBuilder::default()
        .with_architecture_token(arch_tok)
        .with_identifier_token(Identifier::from(b"my_arch"))
        .build();
    assert_eq!(node.raw().to_string(), " end architecture my_arch ;");
}

/// Preamble requires the entity name; keywords are auto-filled.
#[test]
fn entity_preamble_text() {
    let node = EntityDeclarationPreambleBuilder::new(Identifier::from(b"my_entity")).build();
    assert_eq!(node.raw().to_string(), " entity my_entity is");
}

/// The auto-filled `entity` keyword can be overridden by passing a full `Token`.
/// This demonstrates that `impl Into<Token>` still accepts raw tokens for canonical fields.
#[test]
fn entity_preamble_custom_entity_token() {
    use vhdl_syntax::tokens::Keyword;
    // Provide the keyword with no leading trivia to verify the override is used.
    let kw = Token::new(
        TokenKind::Keyword(Keyword::Entity),
        Keyword::Entity.canonical_text(),
        Trivia::default(),
    );
    let node = EntityDeclarationPreambleBuilder::new(Identifier::from(b"e"))
        .with_entity_token(kw) // full Token for explicit trivia control (canonical field)
        .build();
    // No space before "entity" because we overrode the trivia.
    assert_eq!(node.raw().to_string(), "entity e is");
}

/// Default epilogue: just `end ;`. The `entity` keyword and identifier are both optional.
#[test]
fn entity_epilogue_default_text() {
    let node = EntityDeclarationEpilogueBuilder::default().build();
    assert_eq!(node.raw().to_string(), " end ;");
}

/// Epilogue with an optional identifier added via setter.
#[test]
fn entity_epilogue_with_identifier() {
    let node = EntityDeclarationEpilogueBuilder::default()
        .with_identifier_token(Identifier::from(b"foo"))
        .build();
    assert_eq!(node.raw().to_string(), " end foo ;");
}

/// Parse a real entity, extract the name token, rebuild the preamble, and verify
/// the keyword/structure is preserved while the name is taken from the live AST.
/// The parsed token is a full `Token` — it is passed directly to `new()` via the
/// `From<Token> for Identifier` impl, so no manual wrapping is needed.
#[test]
fn roundtrip_entity_preamble_name_from_parsed_ast() {
    let (file, diags) = parser::parse("entity work is end entity work;");
    assert!(diags.is_empty(), "unexpected parse diagnostics: {diags:?}");

    let design_unit = file
        .design_units()
        .next()
        .expect("expected at least one design unit");
    let LibraryUnitSyntax::PrimaryUnit(primary) =
        design_unit.library_unit().expect("missing library_unit")
    else {
        panic!("expected a primary unit");
    };
    let PrimaryUnitSyntax::EntityDeclaration(entity) = primary else {
        panic!("expected entity declaration");
    };
    let preamble = entity
        .entity_declaration_preamble()
        .expect("missing entity_declaration_preamble");

    let name_tok = preamble.name_token().expect("missing name token");

    // Rebuild using the builder, passing the parsed Token directly.
    // Token implements Into<Identifier> via From<Token> for Identifier.
    let rebuilt =
        EntityDeclarationPreambleBuilder::new(name_tok.token().clone()).build();
    // The trivia of the original token is preserved; keywords use single-space trivia.
    assert_eq!(rebuilt.raw().to_string(), " entity work is");
}

/// Default entity declaration: preamble + default (empty) header + epilogue.
/// No generic/port clause, no begin, minimal epilogue (`end ;`).
#[test]
fn entity_declaration_default() {
    let node = EntityDeclarationBuilder::new(
        EntityDeclarationPreambleBuilder::new(Identifier::from(b"foo")).build(),
    )
    .build();
    assert_eq!(node.raw().to_string(), " entity foo is end ;");
}

// ── Domain type unit tests ───────────────────────────────────────────────────

/// AbstractLiteral::real(1.0) must produce "1.0", not "1".
#[test]
fn abstract_literal_real_always_has_decimal_point() {
    let tok: Token = AbstractLiteral::real(1.0).into();
    assert_eq!(tok.kind(), TokenKind::AbstractLiteral);
    assert_eq!(tok.text().to_string(), "1.0");

    let tok: Token = AbstractLiteral::real(2.5).into();
    assert_eq!(tok.text().to_string(), "2.5");

    // Scientific notation already has 'e', so no ".0" appended.
    let tok: Token = AbstractLiteral::real(1e10).into();
    let text = tok.text().to_string();
    assert!(
        text.contains('.') || text.contains('e') || text.contains('E'),
        "expected decimal point or exponent in {text}"
    );
}

/// AbstractLiteral::integer produces a plain decimal string.
#[test]
fn abstract_literal_integer() {
    let tok: Token = AbstractLiteral::integer(42).into();
    assert_eq!(tok.kind(), TokenKind::AbstractLiteral);
    assert_eq!(tok.text().to_string(), "42");
}

/// CharLiteral::new produces tick-wrapped text.
#[test]
fn char_literal_has_tick_marks() {
    let tok: Token = CharLiteral::new(b'A').into();
    assert_eq!(tok.kind(), TokenKind::CharacterLiteral);
    assert_eq!(tok.text().to_string(), "'A'");
}

/// CharLiteral for the single-quote character itself.
#[test]
fn char_literal_quote_char() {
    let tok: Token = CharLiteral::new(b'\'').into();
    assert_eq!(tok.text().to_string(), "'''");
}

/// CharLiteral::try_from_char rejects non-Latin-1 codepoints.
#[test]
fn char_literal_try_from_char_err() {
    assert!(CharLiteral::try_from('€').is_err());
    assert!(CharLiteral::try_from('A').is_ok());
}

/// StringLiteral::new adds surrounding quotes and doubles embedded quotes.
#[test]
fn string_literal_quote_doubling() {
    let tok: Token = StringLiteral::new("say \"hi\"").into();
    assert_eq!(tok.kind(), TokenKind::StringLiteral);
    // "say ""hi""" → opening " + say  + "" (doubled ") + hi + "" (doubled ") + closing "
    assert_eq!(tok.text().to_string(), "\"say \"\"hi\"\"\"");
}

/// BitStringLiteral helpers produce correct prefixes.
#[test]
fn bit_string_literal_prefixes() {
    let tok: Token = BitStringLiteral::binary(b"1010").into();
    assert_eq!(tok.text().to_string(), r#"B"1010""#);

    let tok: Token = BitStringLiteral::octal(b"77").into();
    assert_eq!(tok.text().to_string(), r#"O"77""#);

    let tok: Token = BitStringLiteral::hex(b"FF").into();
    assert_eq!(tok.text().to_string(), r#"X"FF""#);
}

/// From<Token> for Identifier works for correctly-kinded tokens.
#[test]
fn identifier_from_token_escape_hatch() {
    let tok = Token::new(
        TokenKind::Identifier,
        b"foo".as_slice(),
        Trivia::default(),
    );
    let id = Identifier::from(tok);
    let out: Token = id.into();
    assert_eq!(out.text().to_string(), "foo");
}

/// From<Token> for Identifier panics when the kind is wrong.
#[test]
#[should_panic(expected = "Identifier")]
fn identifier_from_token_wrong_kind_panics() {
    let tok = Token::new(
        TokenKind::AbstractLiteral,
        b"42".as_slice(),
        Trivia::default(),
    );
    let _ = Identifier::from(tok);
}
