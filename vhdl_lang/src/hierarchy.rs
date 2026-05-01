// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.

//! Design instantiation hierarchy.
//!
//! Given a top entity, [`compute_design_hierarchy`] walks
//! [`InstantiationStatement`](crate::ast::InstantiationStatement)s inside the
//! entity's architecture(s), resolves entity / component / configuration
//! bindings via the analyzer's named-entity model, and recurses into bound
//! entities to produce a [`DesignHierarchyNode`] tree describing the
//! elaborated design.
//!
//! [`list_top_candidates`] enumerates every entity in the design with its
//! in-degree and subtree depth, suitable for a "pick a top" UI: roots (no
//! incoming instantiations) sort first, deepest first.
//!
//! Limitations of the current implementation:
//!
//! * For-/if-/case-generate statements are visited once: each instance shows
//!   up at the point it is written, not multiplied by the elaborated count.
//! * Direct configuration instantiations (`: configuration cfg`) appear as
//!   leaves; the bound entity is not chased through the configuration's
//!   block-configuration tree.
//! * When an entity has multiple architectures, the architecture named
//!   `rtl` is preferred; otherwise the alphabetically first architecture
//!   wins, with a note recorded on the node.

use std::collections::{HashMap, HashSet};
use std::ops::Deref;

use crate::analysis::{DesignRoot, Library, LockedUnit};
use crate::ast::search::{
    DeclarationItem, FoundDeclaration, NotFinished, Search, SearchState, Searcher,
};
use crate::ast::{
    AnyDesignUnit, AnySecondaryUnit, ConcurrentStatement, Designator, InstantiatedUnit,
    LabeledConcurrentStatement,
};
use crate::data::Symbol;
use crate::named_entity::{AnyEntKind, Design, EntRef, EntityId, HasEntityId};
use crate::syntax::{HasTokenSpan, TokenAccess};
use crate::SrcPos;

/// One node in the design hierarchy tree.
#[derive(Debug, Clone)]
pub struct DesignHierarchyNode {
    /// Instance label (`None` for the top node and for instances declared
    /// without an explicit label, which is illegal VHDL but possible in the
    /// presence of parse errors).
    pub label: Option<String>,
    /// `library.entity` (or `library.component` for unbound components) of
    /// the unit at this node.
    pub entity_path: String,
    /// Architecture selected for this entity, when known.
    pub architecture: Option<String>,
    /// How this child was reached from its parent.
    pub kind: HierarchyKind,
    /// Source position of the instantiation statement (`None` for the top).
    pub instance_pos: Option<SrcPos>,
    /// Source position of the entity (or component) declaration.
    pub entity_pos: Option<SrcPos>,
    pub children: Vec<DesignHierarchyNode>,
    /// Diagnostic notes for this node (e.g. multiple architectures, unbound
    /// component, cyclic instantiation).
    pub notes: Vec<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HierarchyKind {
    /// Root of the tree.
    Top,
    /// Direct entity instantiation (`: entity work.foo`).
    DirectEntity,
    /// Component instantiation that bound to an entity by default binding.
    BoundComponent,
    /// Component instantiation with no matching entity in scope.
    UnboundComponent,
    /// Configuration instantiation (`: configuration cfg`); binding is not
    /// chased.
    Configuration,
    /// Instantiation target could not be resolved by analysis.
    Unresolved,
}

#[derive(Debug)]
pub enum HierarchyError {
    UnknownLibrary {
        library: String,
        known: Vec<String>,
    },
    UnknownEntity {
        library: String,
        entity: String,
        known: Vec<String>,
    },
}

impl std::fmt::Display for HierarchyError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            HierarchyError::UnknownLibrary { library, known } => {
                write!(f, "library '{library}' not found")?;
                if !known.is_empty() {
                    write!(f, " (known: {})", known.join(", "))?;
                }
                Ok(())
            }
            HierarchyError::UnknownEntity {
                library,
                entity,
                known,
            } => {
                write!(f, "entity '{entity}' not found in library '{library}'")?;
                if !known.is_empty() {
                    write!(f, " (known: {})", known.join(", "))?;
                }
                Ok(())
            }
        }
    }
}

impl std::error::Error for HierarchyError {}

/// Resolve `library.entity` and walk its instantiation tree.
pub fn compute_design_hierarchy(
    root: &DesignRoot,
    library_name: &str,
    entity_name: &str,
) -> Result<DesignHierarchyNode, HierarchyError> {
    let lib_sym = root.symbol_utf8(library_name);
    let library = root
        .get_lib(&lib_sym)
        .ok_or_else(|| HierarchyError::UnknownLibrary {
            library: library_name.to_string(),
            known: known_libraries(root),
        })?;

    let ent_sym = root.symbol_utf8(entity_name);
    let top_ent = find_entity(root, library, &ent_sym).ok_or_else(|| {
        HierarchyError::UnknownEntity {
            library: library_name.to_string(),
            entity: entity_name.to_string(),
            known: known_entities(root, library),
        }
    })?;

    let mut visiting: HashSet<EntityId> = HashSet::new();
    Ok(walk_entity(
        root,
        top_ent,
        None,
        None,
        HierarchyKind::Top,
        &mut visiting,
    ))
}

fn known_libraries(root: &DesignRoot) -> Vec<String> {
    let mut names: Vec<String> = root.libraries().map(|l| l.name().name_utf8()).collect();
    names.sort();
    names
}

fn known_entities(root: &DesignRoot, library: &Library) -> Vec<String> {
    let mut names: Vec<String> = library
        .units()
        .filter_map(|locked| {
            let data = locked.unit.expect_analyzed();
            let primary = match *data.deref() {
                AnyDesignUnit::Primary(ref p) => p,
                _ => return None,
            };
            let id = primary.ent_id()?;
            let ent = root.get_ent(id);
            if !matches!(ent.kind(), AnyEntKind::Design(Design::Entity(..))) {
                return None;
            }
            match ent.designator() {
                Designator::Identifier(s) => Some(s.name_utf8()),
                _ => None,
            }
        })
        .collect();
    names.sort();
    names
}

fn find_entity<'a>(
    root: &'a DesignRoot,
    library: &Library,
    entity: &Symbol,
) -> Option<EntRef<'a>> {
    for locked in library.units() {
        let data = locked.unit.expect_analyzed();
        let AnyDesignUnit::Primary(ref primary) = *data.deref() else {
            continue;
        };
        let Some(id) = primary.ent_id() else { continue };
        let ent = root.get_ent(id);
        if matches!(ent.kind(), AnyEntKind::Design(Design::Entity(..)))
            && matches!(ent.designator(), Designator::Identifier(s) if s == entity)
        {
            return Some(ent);
        }
    }
    None
}

/// Pick a single architecture for an entity.
///
/// Behaviour: prefer the architecture literally named `rtl` (case-sensitive
/// to the analyzer, which means latin-1 case-insensitive comparison via
/// `Symbol`). If that is absent, fall back to alphabetical order so the
/// pick is deterministic across runs. The list of all candidates is also
/// returned so the caller can record a note when more than one was found.
fn select_architecture<'a>(architectures: Vec<EntRef<'a>>) -> (Option<EntRef<'a>>, Vec<String>) {
    let mut named: Vec<(String, EntRef<'a>)> = architectures
        .into_iter()
        .filter_map(|e| match e.designator() {
            Designator::Identifier(s) => Some((s.name_utf8(), e)),
            _ => None,
        })
        .collect();
    named.sort_by(|a, b| a.0.cmp(&b.0));

    let names: Vec<String> = named.iter().map(|(n, _)| n.clone()).collect();
    let pick = named
        .iter()
        .find(|(n, _)| n == "rtl")
        .or_else(|| named.first())
        .map(|(_, e)| *e);
    (pick, names)
}

fn walk_entity<'a>(
    root: &'a DesignRoot,
    entity: EntRef<'a>,
    label: Option<String>,
    instance_pos: Option<SrcPos>,
    kind: HierarchyKind,
    visiting: &mut HashSet<EntityId>,
) -> DesignHierarchyNode {
    let library_name = entity
        .library_name()
        .map(|s| s.name_utf8())
        .unwrap_or_else(|| "?".into());
    let entity_text = match entity.designator() {
        Designator::Identifier(s) => s.name_utf8(),
        d => format!("{d}"),
    };
    let entity_path = format!("{library_name}.{entity_text}");

    let mut node = DesignHierarchyNode {
        label,
        entity_path,
        architecture: None,
        kind,
        instance_pos,
        entity_pos: entity.decl_pos().cloned(),
        children: Vec::new(),
        notes: Vec::new(),
    };

    if !visiting.insert(entity.id()) {
        node.notes
            .push("cyclic instantiation - traversal stopped".into());
        return node;
    }

    let architectures: Vec<EntRef<'a>> = root
        .find_implementation(entity)
        .into_iter()
        .filter(|e| matches!(e.kind(), AnyEntKind::Design(Design::Architecture(..))))
        .collect();

    let arch_count = architectures.len();
    let (arch_ent, arch_names) = select_architecture(architectures);

    if let Some(arch) = arch_ent {
        if let Designator::Identifier(s) = arch.designator() {
            node.architecture = Some(s.name_utf8());
        }
    }

    match arch_count {
        0 => node
            .notes
            .push("no architecture found for this entity".into()),
        1 => {}
        _ => {
            let chosen = node.architecture.as_deref().unwrap_or("?");
            node.notes.push(format!(
                "multiple architectures ({}); using {}",
                arch_names.join(", "),
                chosen,
            ));
        }
    }

    if let Some(arch) = arch_ent {
        let lib_sym = entity.library_name().expect("entity has library");
        let library = root.get_lib(lib_sym).expect("library exists");
        for raw in collect_instances_for_arch(library, arch) {
            node.children.push(resolve_instance(root, raw, visiting));
        }
    }

    visiting.remove(&entity.id());
    node
}

fn resolve_instance<'a>(
    root: &'a DesignRoot,
    raw: RawInstance,
    visiting: &mut HashSet<EntityId>,
) -> DesignHierarchyNode {
    let RawInstance {
        label,
        instance_pos,
        target_id,
        target_kind,
    } = raw;

    let unresolved = |label: Option<String>, kind: HierarchyKind, note: &str| {
        DesignHierarchyNode {
            label,
            entity_path: "<unresolved>".into(),
            architecture: None,
            kind,
            instance_pos: Some(instance_pos.clone()),
            entity_pos: None,
            children: Vec::new(),
            notes: vec![note.into()],
        }
    };

    let Some(id) = target_id else {
        return unresolved(
            label,
            HierarchyKind::Unresolved,
            "instantiation target was not resolved by analysis",
        );
    };

    let target = root.get_ent(id);
    match target.kind() {
        AnyEntKind::Design(Design::Entity(..)) => {
            let kind = match target_kind {
                TargetKind::Entity => HierarchyKind::DirectEntity,
                TargetKind::Component => HierarchyKind::BoundComponent,
                // entity_reference() of a Configuration instantiation
                // returns the configuration entity itself, not a Design::Entity,
                // so this arm is unreachable in practice.
                TargetKind::Configuration => HierarchyKind::DirectEntity,
            };
            walk_entity(root, target, label, Some(instance_pos), kind, visiting)
        }
        AnyEntKind::Component(_) => follow_component(root, target, label, instance_pos, visiting),
        AnyEntKind::Design(Design::Configuration) => leaf_for_design(
            target,
            label,
            instance_pos,
            HierarchyKind::Configuration,
            "configuration binding not traversed",
        ),
        _ => unresolved(
            label,
            HierarchyKind::Unresolved,
            "instantiation target is not an entity, component, or configuration",
        ),
    }
}

fn follow_component<'a>(
    root: &'a DesignRoot,
    component: EntRef<'a>,
    label: Option<String>,
    instance_pos: SrcPos,
    visiting: &mut HashSet<EntityId>,
) -> DesignHierarchyNode {
    // Default binding: find_implementation(component) returns the entity
    // with the same name in the same library, if any.
    let bound = root
        .find_implementation(component)
        .into_iter()
        .find(|e| matches!(e.kind(), AnyEntKind::Design(Design::Entity(..))));

    if let Some(entity) = bound {
        walk_entity(
            root,
            entity,
            label,
            Some(instance_pos),
            HierarchyKind::BoundComponent,
            visiting,
        )
    } else {
        leaf_for_design(
            component,
            label,
            instance_pos,
            HierarchyKind::UnboundComponent,
            "component has no matching entity in its library",
        )
    }
}

fn leaf_for_design(
    target: EntRef<'_>,
    label: Option<String>,
    instance_pos: SrcPos,
    kind: HierarchyKind,
    note: &str,
) -> DesignHierarchyNode {
    let name = match target.designator() {
        Designator::Identifier(s) => s.name_utf8(),
        d => format!("{d}"),
    };
    let lib = target
        .library_name()
        .map(|s| s.name_utf8())
        .unwrap_or_else(|| "?".into());
    DesignHierarchyNode {
        label,
        entity_path: format!("{lib}.{name}"),
        architecture: None,
        kind,
        instance_pos: Some(instance_pos),
        entity_pos: target.decl_pos().cloned(),
        children: Vec::new(),
        notes: vec![note.into()],
    }
}

#[derive(Debug)]
struct RawInstance {
    label: Option<String>,
    instance_pos: SrcPos,
    target_id: Option<EntityId>,
    target_kind: TargetKind,
}

#[derive(Debug, Copy, Clone)]
enum TargetKind {
    Entity,
    Component,
    Configuration,
}

fn collect_instances_for_arch(library: &Library, arch_ent: EntRef<'_>) -> Vec<RawInstance> {
    let arch_id = arch_ent.id();
    for locked in library.units() {
        let data = locked.unit.expect_analyzed();
        if let AnyDesignUnit::Secondary(AnySecondaryUnit::Architecture(ref arch)) = *data.deref() {
            if arch.ident.decl.get() == Some(arch_id) {
                drop(data);
                return collect_from_locked(locked);
            }
        }
    }
    Vec::new()
}

fn collect_from_locked(locked: &LockedUnit) -> Vec<RawInstance> {
    let mut searcher = InstanceCollector {
        instances: Vec::new(),
    };
    let _ = locked
        .unit
        .expect_analyzed()
        .search(&locked.tokens, &mut searcher);
    searcher.instances
}

struct InstanceCollector {
    instances: Vec<RawInstance>,
}

impl Searcher for InstanceCollector {
    fn search_decl(&mut self, ctx: &dyn TokenAccess, decl: FoundDeclaration<'_>) -> SearchState {
        if let DeclarationItem::ConcurrentStatement(labeled) = decl.ast {
            if let Some(raw) = make_raw_instance(ctx, labeled) {
                self.instances.push(raw);
            }
        }
        NotFinished
    }
}

fn make_raw_instance(
    ctx: &dyn TokenAccess,
    labeled: &LabeledConcurrentStatement,
) -> Option<RawInstance> {
    let ConcurrentStatement::Instance(ref inst) = labeled.statement.item else {
        return None;
    };

    let label = labeled
        .label
        .tree
        .as_ref()
        .map(|ident| ident.item.name_utf8());
    let instance_pos = match labeled.label.tree {
        Some(ref ident) => ident.pos(ctx).clone(),
        None => inst.get_pos(ctx),
    };

    let target_kind = match inst.unit {
        InstantiatedUnit::Entity(..) => TargetKind::Entity,
        InstantiatedUnit::Component(..) => TargetKind::Component,
        InstantiatedUnit::Configuration(..) => TargetKind::Configuration,
    };

    Some(RawInstance {
        label,
        instance_pos,
        target_id: inst.entity_reference(),
        target_kind,
    })
}

// --- Candidate enumeration ----------------------------------------------------

#[derive(Debug, Clone)]
pub struct TopCandidate {
    pub library: String,
    pub entity: String,
    pub entity_pos: Option<SrcPos>,
    /// Number of times this entity is instantiated anywhere in the design,
    /// after default-binding components to entities.
    pub instance_count: u32,
    /// Height of the subtree rooted at this entity (1 == leaf, no instances).
    pub depth: u32,
    /// `true` when no other entity instantiates this one - i.e. a real top.
    pub is_root: bool,
}

/// Enumerate every entity in the design and rank it as a hierarchy-top
/// candidate. Output is sorted: roots first (deepest first, then by
/// `library.entity` name), followed by non-roots (deepest first, then by
/// name).
pub fn list_top_candidates(root: &DesignRoot) -> Vec<TopCandidate> {
    let entities: Vec<(EntityId, String, String)> = collect_all_entities(root);

    let mut children_of: HashMap<EntityId, Vec<EntityId>> = HashMap::new();
    let mut in_deg: HashMap<EntityId, u32> = HashMap::new();
    for (eid, _, _) in &entities {
        in_deg.entry(*eid).or_insert(0);
    }
    for (eid, _, _) in &entities {
        let ent = root.get_ent(*eid);
        let children = entity_children(root, ent);
        for child in &children {
            *in_deg.entry(*child).or_insert(0) += 1;
        }
        children_of.insert(*eid, children);
    }

    let mut depth_cache: HashMap<EntityId, u32> = HashMap::new();
    let mut on_stack: HashSet<EntityId> = HashSet::new();
    for (eid, _, _) in &entities {
        compute_depth(*eid, &children_of, &mut depth_cache, &mut on_stack);
    }

    let mut out: Vec<TopCandidate> = entities
        .into_iter()
        .map(|(eid, library, entity)| {
            let ent = root.get_ent(eid);
            TopCandidate {
                library,
                entity,
                entity_pos: ent.decl_pos().cloned(),
                instance_count: *in_deg.get(&eid).unwrap_or(&0),
                depth: *depth_cache.get(&eid).unwrap_or(&1),
                is_root: in_deg.get(&eid).copied().unwrap_or(0) == 0,
            }
        })
        .collect();

    out.sort_by(|a, b| {
        b.is_root
            .cmp(&a.is_root)
            .then_with(|| b.depth.cmp(&a.depth))
            .then_with(|| a.library.cmp(&b.library))
            .then_with(|| a.entity.cmp(&b.entity))
    });
    out
}

fn collect_all_entities(root: &DesignRoot) -> Vec<(EntityId, String, String)> {
    let mut out = Vec::new();
    for library in root.libraries() {
        let lib_name = library.name().name_utf8();
        for locked in library.units() {
            let data = locked.unit.expect_analyzed();
            let AnyDesignUnit::Primary(ref primary) = *data.deref() else {
                continue;
            };
            let Some(id) = primary.ent_id() else { continue };
            let ent = root.get_ent(id);
            if !matches!(ent.kind(), AnyEntKind::Design(Design::Entity(..))) {
                continue;
            }
            let name = match ent.designator() {
                Designator::Identifier(s) => s.name_utf8(),
                d => format!("{d}"),
            };
            out.push((id, lib_name.clone(), name));
        }
    }
    out
}

fn entity_children(root: &DesignRoot, entity: EntRef<'_>) -> Vec<EntityId> {
    let Some(lib_sym) = entity.library_name() else {
        return Vec::new();
    };
    let Some(library) = root.get_lib(lib_sym) else {
        return Vec::new();
    };

    let architectures: Vec<EntRef<'_>> = root
        .find_implementation(entity)
        .into_iter()
        .filter(|e| matches!(e.kind(), AnyEntKind::Design(Design::Architecture(..))))
        .collect();
    let (arch_ent, _) = select_architecture(architectures);
    let Some(arch) = arch_ent else {
        return Vec::new();
    };

    let mut children = Vec::new();
    for raw in collect_instances_for_arch(library, arch) {
        let Some(id) = raw.target_id else { continue };
        let target = root.get_ent(id);
        match target.kind() {
            AnyEntKind::Design(Design::Entity(..)) => children.push(id),
            AnyEntKind::Component(_) => {
                if let Some(bound) = root
                    .find_implementation(target)
                    .into_iter()
                    .find(|e| matches!(e.kind(), AnyEntKind::Design(Design::Entity(..))))
                {
                    children.push(bound.id());
                }
            }
            _ => {}
        }
    }
    children
}

fn compute_depth(
    eid: EntityId,
    children_of: &HashMap<EntityId, Vec<EntityId>>,
    cache: &mut HashMap<EntityId, u32>,
    on_stack: &mut HashSet<EntityId>,
) -> u32 {
    if let Some(d) = cache.get(&eid) {
        return *d;
    }
    if !on_stack.insert(eid) {
        // Cycle: treat as height 1 to avoid infinite recursion. Do not
        // cache, since the value depends on the call stack.
        return 1;
    }
    let mut max_child = 0u32;
    if let Some(children) = children_of.get(&eid) {
        for c in children {
            let d = compute_depth(*c, children_of, cache, on_stack);
            if d > max_child {
                max_child = d;
            }
        }
    }
    on_stack.remove(&eid);
    let d = 1 + max_child;
    cache.insert(eid, d);
    d
}
