//! Similar to `source_refactoring.rs`, this example shows how to perform simple changes,
//! for example, to change the name of an entity using the `Rewriter`.
use vhdl_syntax::parser;
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com
use vhdl_syntax::syntax::node::SyntaxElement;
use vhdl_syntax::syntax::node_kind::NodeKind;
use vhdl_syntax::syntax::rewrite::RewriteAction;
use vhdl_syntax::syntax::AstNode;
use vhdl_syntax::tokens::TokenKind;

fn main() {
    // The file to change
    let vhdl = "\
entity foo is
    port (foo : in std_logic);
end foo;
    ";
    let (file, diagnostics) = parser::parse(vhdl);
    assert!(
        diagnostics.is_empty(),
        "Did not expect diagnostics for correct VHDL"
    );

    let new_file = file.raw().rewrite_tokens(|token| {
        if token.kind() == TokenKind::Identifier
            && token.parent().kind() == NodeKind::EntityDeclaration
        {
            let new_name = format!("tb_{}", token.text());
            RewriteAction::Change(SyntaxElement::Token(
                token
                    .clone_with_utf8_text(new_name)
                    .expect("Name was not Latin1"),
            ))
        } else {
            RewriteAction::Leave
        }
    });

    assert_eq!(
        format!("{}", new_file),
        "\
entity tb_foo is
    port (foo : in std_logic);
end tb_foo;
    "
    );
}
