// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2023, Olof Kraigher olof.kraigher@gmail.com

use crate::analysis::DesignRoot;
use crate::ast::{AttributeDesignator, Designator};
use crate::completion::attributes::completions_for_attribute_name;
use crate::completion::generic::generic_completions;
use crate::completion::libraries::list_all_libraries;
use crate::completion::map_aspect::completions_for_map_aspect;
use crate::completion::selected::completions_for_selected_name;
use crate::completion::tokenizer::tokenize_input;
use crate::syntax::Kind;
use crate::{EntRef, Position, Source};

mod attributes;
mod entity_instantiation;
mod generic;
mod libraries;
mod map_aspect;
mod region;
mod selected;
mod tokenizer;

#[derive(Debug, PartialEq, Clone)]
pub enum CompletionItem<'a> {
    /// Simply complete the entities
    /// e.g., `use std.` should simply list all elements in the std library
    Simple(EntRef<'a>),
    /// Formal parameter, e.g., in a port map
    /// `port map (` might choose to complete `<item> => $1`
    Formal(EntRef<'a>),
    /// Multiple overloaded items are applicable.
    /// The argument is the count of overloaded items in total.
    Overloaded(Designator, usize),
    /// Complete a keyword
    Keyword(Kind),
    /// Complete the 'work' library.
    /// This is handled in a special manner because the
    /// actual work library (using [CompletionItem::Simple] would complete the actual name
    /// of the library, not the string 'work'.
    Work,
    /// Entity or component instantiation, i.e.,
    /// ```vhdl
    /// my_ent: entity work.foo
    ///     generic map (
    ///         -- ...
    ///     )
    ///     port map (
    ///         -- ...
    ///     );
    /// ```
    ///
    /// The second argument is a vector of architectures that are associated
    /// to the entity, if the first argument is an entity.
    ///
    /// For a component instantiation, the first argument is a reference to the
    /// component. The second argument will always be empty.
    Instantiation(EntRef<'a>, Vec<EntRef<'a>>),
    /// Complete an attribute designator (i.e. `'range`, `'stable`, ...)
    Attribute(AttributeDesignator),
}

macro_rules! kind {
    ($kind: pat) => {
        crate::syntax::Token { kind: $kind, .. }
    };
}

/// Main entry point for completion. Given a source-file and a cursor position,
/// lists available completion options at the cursor position.
pub fn list_completion_options<'a>(
    root: &'a DesignRoot,
    source: &Source,
    cursor: Position,
) -> Vec<CompletionItem<'a>> {
    use crate::syntax::Kind::*;
    let tokens = tokenize_input(root.symbols(), source, cursor);
    match &tokens[..] {
        // With the current implementation of completions, this is annoying, rather than helpful.
        // SemiColons will try to complete the ';' character, which when pressing enter will cause
        // ';' to appear instead of a simple ; character.
        // Therefore, we do not return any completions here.
        [.., kind!(SemiColon)] => vec![],
        [.., kind!(Library)]
        | [.., kind!(Library), kind!(Identifier)]
        | [.., kind!(Use)]
        | [.., kind!(Use), kind!(Identifier)] => list_all_libraries(root),
        [.., token, kind!(Dot)] | [.., token, kind!(Dot), kind!(Identifier)] => {
            // get the entity before the token.
            // We rely on the syntax parsing to be resilient enough for this to yield a reasonable value.
            // Otherwise, we just return an empty value.
            if let Some((_, ent)) = root.item_at_cursor(source, token.pos.start()) {
                completions_for_selected_name(root, ent)
            } else {
                vec![]
            }
        }
        [.., token, kind!(Tick)] | [.., token, kind!(Tick), kind!(Identifier)] => {
            if let Some((_, ent)) = root.item_at_cursor(source, token.pos.start()) {
                completions_for_attribute_name(ent)
            } else {
                vec![]
            }
        }
        [.., kind!(LeftPar | Comma)] | [.., kind!(LeftPar | Comma), kind!(Identifier)] => {
            completions_for_map_aspect(root, cursor, source)
        }
        [.., kind!(CommAt)] | [.., kind!(CommAt), kind!(Identifier)] => list_all_libraries(root),
        _ => generic_completions(root, cursor, source),
    }
}
