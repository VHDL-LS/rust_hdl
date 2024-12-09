// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2024, Olof Kraigher olof.kraigher@gmail.com
use crate::ast::search::{
    DeclarationItem, Finished, Found, FoundDeclaration, NotFinished, SearchState, Searcher,
};
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

impl CompletionSearcher<'_> {
    /// Add entity instantiation completions that are visible from within an architecture body
    fn add_entity_instantiations(&mut self, body: &ArchitectureBody) {
        let Some(ent_id) = body.ident.decl.get() else {
            return;
        };
        let Some(ent) = DesignEnt::from_any(self.root.get_ent(ent_id)) else {
            return;
        };
        self.completions
            .extend(get_visible_entities_from_architecture(self.root, &ent));
    }
}

impl Searcher for CompletionSearcher<'_> {
    fn search_decl(&mut self, ctx: &dyn TokenAccess, decl: FoundDeclaration<'_>) -> SearchState {
        let ent_id = match &decl.ast {
            DeclarationItem::Entity(ent_decl) => {
                if !ent_decl.get_pos(ctx).contains(self.cursor) {
                    return NotFinished;
                }
                ent_decl.ident.decl.get()
            }
            DeclarationItem::Architecture(body) => {
                if !body.get_pos(ctx).contains(self.cursor) {
                    return NotFinished;
                }
                if body.statement_span().get_pos(ctx).contains(self.cursor) {
                    self.add_entity_instantiations(body);
                }
                body.ident.decl.get()
            }
            DeclarationItem::Package(package) => {
                if !package.get_pos(ctx).contains(self.cursor) {
                    return NotFinished;
                }
                package.ident.decl.get()
            }
            DeclarationItem::PackageBody(package) => {
                if !package.get_pos(ctx).contains(self.cursor) {
                    return NotFinished;
                }
                package.ident.decl.get()
            }
            DeclarationItem::Subprogram(subprogram) => {
                if !subprogram.get_pos(ctx).contains(self.cursor) {
                    return NotFinished;
                }
                self.completions.extend(
                    subprogram
                        .declarations
                        .iter()
                        .flat_map(|decl| decl.item.declarations())
                        .map(|id| CompletionItem::Simple(self.root.get_ent(id))),
                );
                return NotFinished;
            }
            _ => return NotFinished,
        };
        let Some(ent_id) = ent_id else {
            return Finished(Found);
        };
        let Some(ent) = DesignEnt::from_any(self.root.get_ent(ent_id)) else {
            return Finished(Found);
        };
        self.completions
            .extend(visible_entities_from(self.root, ent.kind()));
        NotFinished
    }
}

fn visible_entities_from<'a>(
    root: &'a DesignRoot,
    design: &'a Design<'a>,
) -> Vec<CompletionItem<'a>> {
    use Design::*;
    match design {
        Entity(visibility, region)
        | UninstPackage(visibility, region)
        | Architecture(visibility, region, _)
        | Package(visibility, region)
        | PackageBody(visibility, region) => chain(
            completion_items_from_region(root, region),
            completion_items_from_visibility(root, visibility),
        )
        .collect_vec(),
        PackageInstance(region) | InterfacePackageInstance(region) | Context(region) => {
            completion_items_from_region(root, region).collect_vec()
        }
        Configuration => vec![],
    }
}

fn completion_items_from_visibility<'a>(
    root: &'a DesignRoot,
    visibility: &'a Visibility<'a>,
) -> impl Iterator<Item = CompletionItem<'a>> {
    visibility
        .visible()
        .unique()
        .map(CompletionItem::Simple)
        .chain(
            visibility.all_in_region().flat_map(|visible_region| {
                completion_items_from_region(root, visible_region.region())
            }),
        )
}
