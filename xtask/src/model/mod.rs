// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2025, Lukas Scheller lukasscheller@icloud.com

pub mod node;
pub mod token;

pub use node::*;
pub use token::*;

use crate::config::yaml;
use std::fs::File;
use std::path::Path;

/// Read all `.yaml` files from `definitions_dir`, deserialize, convert, validate,
/// postprocess, and return a [Model].
pub fn load_model(definitions_dir: &Path) -> Model {
    let mut model = Model::default();

    let mut entries: Vec<_> = std::fs::read_dir(definitions_dir)
        .unwrap_or_else(|err| {
            panic!(
                "cannot read definitions dir {}: {err}",
                definitions_dir.display()
            )
        })
        .filter_map(|e| e.ok())
        .filter(|e| {
            e.path()
                .extension()
                .map(|ext| ext == "yaml")
                .unwrap_or(false)
        })
        .collect();

    // Sort for deterministic section ordering
    entries.sort_by_key(|e| e.path());

    for entry in entries {
        let path = entry.path();
        let file =
            File::open(&path).unwrap_or_else(|err| panic!("cannot open {}: {err}", path.display()));
        let nodes: yaml::Nodes = serde_yml::from_reader(file)
            .unwrap_or_else(|err| panic!("{err} while processing {}", path.display()));
        let section = path.file_stem().unwrap().to_str().unwrap().to_owned();
        model.insert_ser_nodes(&section, nodes);
    }

    model.fixup_empty_capable_optional_markers();
    model.do_checks();
    model.do_postprocessing();
    model
}
