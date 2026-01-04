//! Shows how a user can refactor source code based on parsed input.
//! The goal of this executable is to replace every entity named 'foo' with a new entity named
//! 'no_longer_foo'.
//!
//! Rewriting is still under heavy construction. The basic problem is that a `SyntaxNode` is
//! immutable and can thus not easily be modified. The proposed solution is to use the capabilities
//! in [vhdl_syntax::syntax::rewrite]. This is, essentially, a visitor where the user provides a
//! function that is applied to every node. The function returns [RewriteAction::Leave], if the
//! node is to be left as-is. However, the user can also return [RewriteAction::Change] to
//! change the current node into a different one.
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com
use vhdl_syntax::parser;
use vhdl_syntax::parser::Parser;
use vhdl_syntax::syntax::node::SyntaxElement;
use vhdl_syntax::syntax::rewrite::RewriteAction;
use vhdl_syntax::syntax::AstNode;
use vhdl_syntax::syntax::EntityDeclarationSyntax;

fn main() {
    // The file to change
    let vhdl = "\
entity foo is
end foo;

entity bar1 is
end bar1;

entity foobar is
end foobar;
    ";
    let (file, diagnostics) = parser::parse(vhdl);
    assert!(
        diagnostics.is_empty(),
        "Did not expect diagnostics for correct VHDL"
    );

    // The target
    // NOTE: Usage of the parser API will be significantly changed or removed in favor of a better alternative in a future version.
    let mut parser = Parser::new(
        "\
entity no_longer_foo is
end no_longer_foo;

"
        .into(),
    );
    parser.entity();
    let (replacement_entity, diagnostics) = parser.into_root();
    assert!(
        diagnostics.is_empty(),
        "Did not expect diagnostics for correct VHDL"
    );

    let new_file = file.raw().rewrite(|node| match node {
        SyntaxElement::Node(node) => match EntityDeclarationSyntax::cast(node.clone()) {
            // If the syntax node is an entity and is named 'foo', replace it with the replacement entity.
            Some(ent) if ent.name_token().is_some_and(|tok| tok.text() == "foo") => {
                RewriteAction::Change(SyntaxElement::Node(replacement_entity.clone()))
            }
            // If the syntax node is not an entity, or the name of the entity is not 'foo', leave the node as-is
            _ => RewriteAction::Leave,
        },
        SyntaxElement::Token(_) => RewriteAction::Leave,
    });

    assert_eq!(
        format!("{}", new_file),
        "\
entity no_longer_foo is
end no_longer_foo;

entity bar1 is
end bar1;

entity foobar is
end foobar;
    "
    );
}
