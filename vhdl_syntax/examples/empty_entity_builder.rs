// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2026, Lukas Scheller lukasscheller@icloud.com

//! This example uses the builder-API to show how to programatically create a testbench entity.
//! The builder-API is feature-complete, but may not be ergonomic for common cases such as
//! testbench generation.
use vhdl_syntax::syntax::{
    builders::*, AstNode, LibraryUnitSyntax, NameDesignatorToken, NamePrefixSyntax,
    PrimaryUnitSyntax, SecondaryUnitSyntax,
};

fn main() {
    // The builder API is used to programaticall build VHDL nodes.
    // Each SyntaxNode has an associated Builder Node. E.g., `EntityDeclarationSyntax` -> `EntityDeclarationBuilder`.
    // The constructor takes all required nodes and entities.
    // required keywords and nodes are auto-generated, but you may change them using `.with_*(token)` methods.

    // An entity declaration syntax is split into multiple nodes, the most important being
    // the preamble ("entity foo is"), the declarations, concurrent statements and the epilogue ("end entity foo;").
    // Consult the EntityDeclarationSyntax struct for more information.
    // Of all these nodes, only the PreambleSyntax must be provided. The remaining nodes are either optional,
    // repeated or tokens (which will be default-initialized).
    // An entity as defined above will produce the VHDL code ` entity tb_foo is end ;`
    let mut builder = EntityDeclarationBuilder::new(EntityDeclarationPreambleBuilder::new(b"foo"));
    // In order to repeat the second identifier, we overwrite the default-generated `EntityDeclarationEpilogue`
    // and provide the name. The new epilogue is equally generated through a builder.
    builder = builder.with_entity_declaration_epilogue(
        EntityDeclarationEpilogueBuilder::new().with_identifier_token(b"foo"),
    );
    // transform the builder into syntax by calling `build()` on it
    let entity_declaration_node = builder.build();

    // Names are highly complicated in VHDL and so is building them.
    // The ergonomics of this may change in the future, but currently this is the only way to build names.
    // `NameDesignatorToken::identifier` creates a token-choice wrapper from a plain identifier.
    let entity_name = NameBuilder::new(NamePrefixSyntax::NameDesignatorPrefix(
        NameDesignatorPrefixBuilder::new(NameDesignatorToken::identifier(b"foo")).build(),
    ));

    // Create an architecture syntax.
    let architecture_syntax =
        ArchitectureBodyBuilder::new(ArchitecturePreambleBuilder::new(b"arch", entity_name))
            .build();

    // build the VHDL file with entity and architecture
    let file = DesignFileBuilder::new()
        .add_design_units(DesignUnitBuilder::new(LibraryUnitSyntax::PrimaryUnit(
            PrimaryUnitSyntax::EntityDeclaration(entity_declaration_node),
        )))
        .add_design_units(DesignUnitBuilder::new(LibraryUnitSyntax::SecondaryUnit(
            SecondaryUnitSyntax::ArchitectureBody(architecture_syntax),
        )))
        .build();

    // The canonical output consists of tokens separated by a single space.
    // Formatters can be used to nicely display this to the user.
    assert_eq!(
        file.raw().to_string(),
        " entity foo is end foo ; architecture arch of foo is begin end ; "
    )
}
