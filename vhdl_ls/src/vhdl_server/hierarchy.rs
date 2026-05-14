// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.

//! Custom LSP request `vhdl/designHierarchy`.
//!
//! Given a `library.entity`, returns the design instantiation tree as JSON
//! with LSP `Location`s, ready for a VS Code TreeView.

use lsp_types::Location;
use serde::{Deserialize, Serialize};

use super::{srcpos_to_location, VHDLServer};
use vhdl_lang::{DesignHierarchyNode, HierarchyError, HierarchyKind, TopCandidate};

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct DesignHierarchyParams {
    pub library: String,
    pub entity: String,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct HierarchyResponseNode {
    pub label: Option<String>,
    pub entity: String,
    pub architecture: Option<String>,
    pub kind: &'static str,
    pub instance_location: Option<Location>,
    pub entity_location: Option<Location>,
    pub notes: Vec<String>,
    pub children: Vec<HierarchyResponseNode>,
}

impl VHDLServer {
    pub fn design_hierarchy(
        &self,
        params: &DesignHierarchyParams,
    ) -> Result<HierarchyResponseNode, HierarchyError> {
        let node = self
            .project
            .design_hierarchy(&params.library, &params.entity)?;
        Ok(convert(node))
    }

    pub fn design_hierarchy_candidates(&self) -> CandidatesResponse {
        let candidates = self
            .project
            .design_hierarchy_candidates()
            .into_iter()
            .map(convert_candidate)
            .collect();
        CandidatesResponse { candidates }
    }
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct CandidatesResponse {
    pub candidates: Vec<CandidateItem>,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct CandidateItem {
    pub library: String,
    pub entity: String,
    pub depth: u32,
    pub instance_count: u32,
    pub is_root: bool,
    pub entity_location: Option<Location>,
}

fn convert_candidate(c: TopCandidate) -> CandidateItem {
    CandidateItem {
        library: c.library,
        entity: c.entity,
        depth: c.depth,
        instance_count: c.instance_count,
        is_root: c.is_root,
        entity_location: c.entity_pos.as_ref().map(srcpos_to_location),
    }
}

fn convert(node: DesignHierarchyNode) -> HierarchyResponseNode {
    HierarchyResponseNode {
        label: node.label,
        entity: node.entity_path,
        architecture: node.architecture,
        kind: kind_str(node.kind),
        instance_location: node.instance_pos.as_ref().map(srcpos_to_location),
        entity_location: node.entity_pos.as_ref().map(srcpos_to_location),
        notes: node.notes,
        children: node.children.into_iter().map(convert).collect(),
    }
}

fn kind_str(k: HierarchyKind) -> &'static str {
    match k {
        HierarchyKind::Top => "top",
        HierarchyKind::DirectEntity => "entity",
        HierarchyKind::BoundComponent => "boundComponent",
        HierarchyKind::UnboundComponent => "unboundComponent",
        HierarchyKind::Configuration => "configuration",
        HierarchyKind::Unresolved => "unresolved",
    }
}
