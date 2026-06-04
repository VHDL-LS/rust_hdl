//! Resolution of [`SourceId`]s into the data the renderer needs.
//!
//! Diagnostics carry only raw source spans and a [`SourceId`]; turning those
//! into a rendered snippet requires the file's syntax tree (the snippet text is
//! rendered on demand from it) and its [`SourceLocConverter`]. The renderer is
//! written against the [`SourceProvider`] trait rather than a concrete
//! container, so a richer owner (e.g. a future analysis state) can supply
//! sources without the rendering code changing.

use std::collections::HashMap;

use vhdl_syntax::{syntax::node::SyntaxNode, text::source_loc::SourceLocConverter};

use crate::diagnostic::SourceId;

/// A resolved source file: a display name, the syntax tree the snippet text is
/// rendered from, and the converter mapping raw source spans into the render
/// encoding.
pub struct SourceFile<'a> {
    pub name: Option<&'a str>,
    pub tree: &'a SyntaxNode,
    pub converter: &'a SourceLocConverter,
}

/// Resolves the [`SourceId`]s referenced by diagnostics to their [`SourceFile`].
pub trait SourceProvider {
    fn lookup(&self, id: SourceId) -> Option<SourceFile<'_>>;
}

struct Entry {
    name: Option<String>,
    tree: SyntaxNode,
    converter: SourceLocConverter,
}

/// A simple in-memory [`SourceProvider`] suitable for the CLI, where every
/// source is known up front.
#[derive(Default)]
pub struct SourceMap {
    entries: HashMap<SourceId, Entry>,
}

impl SourceMap {
    pub fn new() -> SourceMap {
        SourceMap {
            entries: HashMap::new(),
        }
    }

    /// Register a source under `id`. The tree is kept so its snippet text can be
    /// rendered on demand when a diagnostic referencing this source is shown.
    pub fn insert(
        &mut self,
        id: SourceId,
        name: Option<String>,
        tree: &SyntaxNode,
        converter: SourceLocConverter,
    ) {
        self.entries.insert(
            id,
            Entry {
                name,
                tree: tree.clone(),
                converter,
            },
        );
    }
}

impl SourceProvider for SourceMap {
    fn lookup(&self, id: SourceId) -> Option<SourceFile<'_>> {
        let entry = self.entries.get(&id)?;
        Some(SourceFile {
            name: entry.name.as_deref(),
            tree: &entry.tree,
            converter: &entry.converter,
        })
    }
}
