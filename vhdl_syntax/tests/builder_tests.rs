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
    latin_1::Latin1Str,
    parser,
    syntax::{
        ArchitectureEpilogueBuilder, AstNode, EntityDeclarationEpilogueBuilder,
        EntityDeclarationPreambleBuilder, LibraryUnitSyntax, PrimaryUnitSyntax,
    },
    tokens::{Token, TokenKind, Trivia, TriviaPiece},
};

// Helper: build a single-space-prefixed identifier token.
fn ident(name: &[u8]) -> Token {
    Token::new(
        TokenKind::Identifier,
        Latin1Str::new(name),
        Trivia::from([TriviaPiece::Spaces(1)]),
    )
}

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
        Trivia::from([TriviaPiece::Spaces(1)]),
    );
    let node = ArchitectureEpilogueBuilder::default()
        .with_architecture_token(arch_tok)
        .with_identifier_token(ident(b"my_arch"))
        .build();
    assert_eq!(node.raw().to_string(), " end architecture my_arch ;");
}

// ── EntityDeclarationPreamble ────────────────────────────────────────────────

/// Preamble requires the entity name; keywords are auto-filled.
#[test]
fn entity_preamble_text() {
    let node = EntityDeclarationPreambleBuilder::new(ident(b"my_entity")).build();
    assert_eq!(node.raw().to_string(), " entity my_entity is");
}

/// The auto-filled `entity` keyword can be overridden with `with_entity_token`.
#[test]
fn entity_preamble_custom_entity_token() {
    use vhdl_syntax::tokens::Keyword;
    // Provide the keyword with no leading trivia to verify the override is used.
    let kw = Token::new(
        TokenKind::Keyword(Keyword::Entity),
        Keyword::Entity.canonical_text(),
        Trivia::default(),
    );
    let node = EntityDeclarationPreambleBuilder::new(ident(b"e"))
        .with_entity_token(kw)
        .build();
    // No space before "entity" because we overrode the trivia.
    assert_eq!(node.raw().to_string(), "entity e is");
}

/// Epilogue requires the identifier; end/entity/semicolon are auto-filled.
#[test]
fn entity_epilogue_text() {
    let node = EntityDeclarationEpilogueBuilder::new(ident(b"foo")).build();
    assert_eq!(node.raw().to_string(), " end entity foo ;");
}

// ── Round-trip: rebuild a parsed preamble ───────────────────────────────────

/// Parse a real entity, extract the name token, rebuild the preamble, and verify
/// the keyword/structure is preserved while the name is taken from the live AST.
#[test]
fn roundtrip_entity_preamble_name_from_parsed_ast() {
    let (file, diags) = parser::parse("entity work is end entity work;");
    assert!(diags.is_empty(), "unexpected parse diagnostics: {diags:?}");

    // Navigate to the entity name token via the generated getters.
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

    // Rebuild using the builder, feeding in the parsed token.
    let rebuilt = EntityDeclarationPreambleBuilder::new(name_tok.token().clone()).build();
    // The trivia of the original token is preserved; keywords use single-space trivia.
    assert_eq!(rebuilt.raw().to_string(), " entity work is");
}
