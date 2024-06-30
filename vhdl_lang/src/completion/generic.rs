// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2024, Olof Kraigher olof.kraigher@gmail.com
use crate::ast::search::{Finished, Found, FoundDeclaration, NotFinished, SearchState, Searcher};
use crate::ast::ArchitectureBody;
use crate::completion::entity_instantiation::get_visible_entities_from_architecture;
use crate::completion::region::completion_items_from_region;
use crate::named_entity::{DesignEnt, Visibility};
use crate::{CompletionItem, Design, HasTokenSpan, Position, Source, TokenAccess};
use itertools::{chain, Itertools};
use vhdl_lang::analysis::DesignRoot;

pub(crate) fn generic_completions<'a>(
    root: &'a DesignRoot,
    cursor: Position,
    source: &Source,
) -> Vec<CompletionItem<'a>> {
    let mut searcher = CompletionSearcher::new(cursor, root);
    let _ = root.search_source(source, &mut searcher);
    searcher.completions
}

/// This is the most general-purpose completion provider.
/// This provider publishes all visible symbols reachable from some context.
/// This will, among other things produce many "non-regular" symbols, such as
/// operator symbols or specific characters. If possible,
/// this searcher should therefore be avoided in favor of a more specific completion provider.
struct CompletionSearcher<'a> {
    root: &'a DesignRoot,
    cursor: Position,
    completions: Vec<CompletionItem<'a>>,
}

impl<'a> CompletionSearcher<'a> {
    pub fn new(cursor: Position, design_root: &'a DesignRoot) -> CompletionSearcher<'a> {
        CompletionSearcher {
            root: design_root,
            cursor,
            completions: Vec::new(),
        }
    }
}

impl<'a> CompletionSearcher<'a> {
    /// Add entity instantiation completions that are visible from within an architecture body
    fn add_entity_instantiations(&mut self, ctx: &dyn TokenAccess, body: &ArchitectureBody) {
        let Some(ent_id) = body.ident.decl.get() else {
            return;
        };
        let Some(ent) = DesignEnt::from_any(self.root.get_ent(ent_id)) else {
            return;
        };
        // Early-exit for when we are inside a statement.
        for statement in &body.statements {
            let pos = &statement.statement.pos(ctx);

            // Early exit. The cursor is below the current statement.
            if pos.start() > self.cursor {
                break;
            }

            if pos.contains(self.cursor) {
                return;
            }
        }
        self.completions
            .extend(get_visible_entities_from_architecture(self.root, &ent));
    }
}

impl<'a> Searcher for CompletionSearcher<'a> {
    fn search_decl(&mut self, ctx: &dyn TokenAccess, decl: FoundDeclaration) -> SearchState {
        let ent_id = match decl {
            FoundDeclaration::Entity(ent_decl) => {
                if !ent_decl.get_pos(ctx).contains(self.cursor) {
                    return NotFinished;
                }
                ent_decl.ident.decl.get()
            }
            FoundDeclaration::Architecture(body) => {
                if !body.get_pos(ctx).contains(self.cursor) {
                    return NotFinished;
                }
                self.add_entity_instantiations(ctx, body);
                body.ident.decl.get()
            }
            FoundDeclaration::Package(package) => {
                if !package.get_pos(ctx).contains(self.cursor) {
                    return NotFinished;
                }
                package.ident.decl.get()
            }
            FoundDeclaration::PackageBody(package) => {
                if !package.get_pos(ctx).contains(self.cursor) {
                    return NotFinished;
                }
                package.ident.decl.get()
            }
            _ => return NotFinished,
        };
        let Some(ent_id) = ent_id else {
            return Finished(Found);
        };
        let Some(ent) = DesignEnt::from_any(self.root.get_ent(ent_id)) else {
            return Finished(Found);
        };
        self.completions.extend(visible_entities_from(ent.kind()));
        Finished(Found)
    }
}

fn visible_entities_from<'a>(design: &'a Design<'a>) -> Vec<CompletionItem<'a>> {
    use Design::*;
    match design {
        Entity(visibility, region)
        | UninstPackage(visibility, region)
        | Architecture(visibility, region, _)
        | Package(visibility, region)
        | PackageBody(visibility, region) => chain(
            completion_items_from_region(region),
            completion_items_from_visibility(visibility),
        )
        .collect_vec(),
        PackageInstance(region) | InterfacePackageInstance(region) | Context(region) => {
            completion_items_from_region(region).collect_vec()
        }
        Configuration => vec![],
    }
}

fn completion_items_from_visibility<'a>(
    visibility: &'a Visibility<'a>,
) -> impl Iterator<Item = CompletionItem<'a>> {
    visibility
        .visible()
        .unique()
        .map(CompletionItem::Simple)
        .chain(
            visibility
                .all_in_region()
                .flat_map(|visible_region| completion_items_from_region(visible_region.region())),
        )
}
