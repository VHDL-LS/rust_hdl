// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2023, Olof Kraigher olof.kraigher@gmail.com

use brunch::{Bench, Benches};
use std::{path::Path, time::Duration};
use vhdl_lang::{
    ast::search::{SearchState, Searcher},
    Config, MessagePrinter, NullMessages, Project,
};

fn load_config(include_example_project: bool) -> Config {
    let repo_root = Path::new(env!("CARGO_MANIFEST_DIR")).join("..");

    let mut config = Config::default();
    config.append(
        &Config::read_file_path(&repo_root.join("vhdl_libraries").join("vhdl_ls.toml"))
            .expect("Failed to read installed config file"),
        &mut MessagePrinter::default(),
    );

    if include_example_project {
        config.append(
            &Config::read_file_path(&repo_root.join("example_project").join("vhdl_ls.toml"))
                .expect("Failed to read project config file"),
            &mut MessagePrinter::default(),
        );
    }

    config
}

fn main() {
    let mut benches = Benches::default();

    {
        // Only use standard libraries to benchmark parse and analyze as the time taken to get 100 samples
        // is very big with the example project
        let config = load_config(false);
        benches.push(Bench::new("parse and analyze").with_samples(10).run(|| {
            let mut project = Project::from_config(config.clone(), &mut NullMessages);
            project.analyse();
        }));
    }

    {
        let mut project = Project::from_config(load_config(true), &mut NullMessages);
        project.analyse();

        let integer = project
        .public_symbols()
        .find(|ent| matches!(ent.designator().as_identifier(), Some(sym) if sym.name_utf8() == "INTEGER"))
        .unwrap();

        benches.push(Bench::new("find all references").run(|| {
            assert!(!project.find_all_references(integer).is_empty());
        }));

        let integer_pos = integer.decl_pos().unwrap();
        benches.push(Bench::new("item at cursor").run(|| {
            assert_eq!(
                project
                    .item_at_cursor(&integer_pos.source, integer_pos.start())
                    .unwrap()
                    .1,
                integer
            );
        }));

        benches.push(
            Bench::new("search entire ast")
                .with_timeout(Duration::from_secs(30))
                .run(|| {
                    project.search(&mut MySearcher {});
                }),
        );
    }

    benches.finish();
}

struct MySearcher {}

impl Searcher for MySearcher {
    fn search_pos_with_ref(
        &mut self,
        _ctx: &dyn vhdl_lang::TokenAccess,
        _pos: &vhdl_lang::SrcPos,
        _ref: &vhdl_lang::Reference,
    ) -> SearchState {
        std::hint::black_box(SearchState::NotFinished)
    }
}
