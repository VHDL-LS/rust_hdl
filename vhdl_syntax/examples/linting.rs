// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com

use vhdl_syntax::parser;
use vhdl_syntax::syntax::visitor::WalkEvent;
use vhdl_syntax::syntax::AstNode;
use vhdl_syntax::syntax::EntityDeclarationSyntax;

/// Showcases how one could write a simple linter that checks that the declared name and final name
/// of an entity match.

fn main() {
    let vhdl = "\
entity baz is
end entity baz;

entity foo is
end entity bar;
    ";
    let (design, diagnostics) = parser::parse(vhdl);
    assert!(
        diagnostics.is_empty(),
        "Did not expect diagnostics for correct VHDL"
    );
    for entity_declaration in design.walk().filter_map(|event| match event {
        WalkEvent::Enter(node) => EntityDeclarationSyntax::cast(node),
        WalkEvent::Leave(_) => None,
    }) {
        if let Some(first_ident) = entity_declaration
            .entity_declaration_preamble()
            .and_then(|preamble| preamble.name_token())
        {
            if let Some(second_ident) = entity_declaration
                .entity_declaration_epilogue()
                .and_then(|epilogue| epilogue.identifier_token())
            {
                // print, if the identifiers mismatch.
                // Note that the text position is the number of chars.
                if first_ident.text() != second_ident.text() {
                    println!(
                        "Identifier mismatch. First: {}@{}, second: {}@{}",
                        first_ident.text(),
                        first_ident.text_pos(),
                        second_ident.text(),
                        second_ident.text_pos()
                    );
                }
            }
        }
    }
}
