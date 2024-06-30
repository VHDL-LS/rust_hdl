// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2024, Olof Kraigher olof.kraigher@gmail.com
use crate::analysis::DesignRoot;
use crate::CompletionItem;
use std::iter::once;

/// Produces all available libraries.
pub(crate) fn list_all_libraries(root: &DesignRoot) -> Vec<CompletionItem> {
    root.libraries()
        .map(|lib| CompletionItem::Simple(root.get_ent(lib.id())))
        .chain(once(CompletionItem::Work))
        .collect()
}

#[cfg(test)]
mod tests {
    use crate::analysis::tests::{Code, LibraryBuilder};
    use crate::completion::libraries::list_all_libraries;
    use crate::list_completion_options;

    #[test]
    pub fn completing_libraries() {
        let input = LibraryBuilder::new();
        let code = Code::new("library ");
        let (root, _) = input.get_analyzed_root();
        let cursor = code.end();
        let options = list_completion_options(&root, code.source(), cursor);
        assert_eq!(options, list_all_libraries(&root))
    }
}
