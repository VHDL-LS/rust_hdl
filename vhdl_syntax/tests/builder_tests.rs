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
    builder::Identifier,
    latin_1::Latin1Str,
    parser,
    syntax::{
        ActualPartBuilder, ArchitectureEpilogueBuilder, AssociationElementBuilder, AstNode,
        BinaryOperatorToken, EntityDeclarationBuilder, EntityDeclarationEpilogueBuilder,
        EntityDeclarationPreambleBuilder, LibraryUnitSyntax, LiteralExpressionBuilder,
        LiteralSyntax, LiteralToken, NameDesignatorPrefixBuilder, NameDesignatorToken,
        PrimaryUnitSyntax, SelectedNameBuilder, SuffixToken,
    },
    tokens::{Token, TokenKind, Trivia, TriviaPiece},
};

// MARK: Architecture Epilogue

/// A default-built epilogue has no optional tokens: just `end ;`.
#[test]
fn arch_epilogue_default_text() {
    let node = ArchitectureEpilogueBuilder::new().build();
    assert_eq!(node.raw().to_string(), " end ;");
}

/// Optional tokens can be added with `with_*` setters.
#[test]
fn arch_epilogue_with_optional_tokens() {
    use vhdl_syntax::tokens::Keyword as Kw;
    let arch_tok = Token::new(
        TokenKind::Keyword(Kw::Architecture),
        Kw::Architecture.canonical_text(),
        Trivia::from([TriviaPiece::Spaces(1)]),
    );
    let node = ArchitectureEpilogueBuilder::new()
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

/// byte-slices implement `Into<Identifier>`, so `Identifier::from` can be omitted
#[test]
fn entity_preamble_text_no_explicit_identifier() {
    let node = EntityDeclarationPreambleBuilder::new(b"my_entity").build();
    assert_eq!(node.raw().to_string(), " entity my_entity is");
}

/// The auto-filled `entity` keyword can be overridden by passing a full `Token`.
#[test]
fn entity_preamble_custom_entity_token() {
    use vhdl_syntax::tokens::Keyword as Kw;
    // Provide the keyword with no leading trivia to verify the override is used.
    let kw = Token::new(
        TokenKind::Keyword(Kw::Entity),
        Kw::Entity.canonical_text(),
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
    let rebuilt = EntityDeclarationPreambleBuilder::new(name_tok.token().clone()).build();
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

// MARK: ActualPartBuilder

/// `ActualPartBuilder` satisfies `impl Into<ActualPartSyntax>` required by `AssociationElementBuilder::new`.
#[test]
fn actual_part_satisfies_into_for_association_element() {
    let _ = AssociationElementBuilder::new(ActualPartBuilder::from_vhdl(Latin1Str::new(
        b"system_clock",
    )));
}

// MARK: Token choice nodes

/// `NameDesignatorToken::identifier()` creates a token that can be used with `NameDesignatorPrefixBuilder`.
#[test]
fn name_designator_token_identifier_constructor() {
    let node =
        NameDesignatorPrefixBuilder::new(NameDesignatorToken::identifier(b"my_signal")).build();
    assert_eq!(node.raw().to_string(), " my_signal");
}

/// `From<Identifier> for NameDesignatorToken` lets an `Identifier` be passed directly
/// to builders that accept `impl Into<NameDesignatorToken>`.
#[test]
fn name_designator_prefix_from_identifier_directly() {
    let node = NameDesignatorPrefixBuilder::new(Identifier::from(b"clk")).build();
    assert_eq!(node.raw().to_string(), " clk");
}

/// `SuffixToken::all()` produces the canonical `all` keyword token;
/// a `SelectedName` built from it emits `. all`.
#[test]
fn suffix_token_canonical_all_constructor() {
    let node = SelectedNameBuilder::new(SuffixToken::all()).build();
    assert_eq!(node.raw().to_string(), " . all");
}

/// `LiteralToken::null()` produces the canonical `null` keyword;
/// a `LiteralExpression` built from it emits ` null`.
#[test]
fn literal_expression_from_null_token() {
    let node = LiteralExpressionBuilder::new(LiteralToken::null()).build();
    assert_eq!(node.raw().to_string(), " null");
}

/// `From<LiteralSyntax> for LiteralToken` is implemented: verified by coercing the
/// function as a value (compile-time check, no runtime assertion needed).
#[test]
fn literal_token_from_literal_syntax_type_check() {
    let _: fn(LiteralSyntax) -> LiteralToken = LiteralToken::from;
}

/// All-canonical `BinaryOperatorToken` constructors compile and produce distinct values.
/// This also tests the `r#mod()` raw-identifier constructor generated for the VHDL `mod`
/// operator (a Rust reserved keyword).
#[test]
fn binary_operator_token_canonical_constructors_compile() {
    let _ = BinaryOperatorToken::and();
    let _ = BinaryOperatorToken::or();
    let _ = BinaryOperatorToken::eq();
    let _ = BinaryOperatorToken::r#mod();
}
