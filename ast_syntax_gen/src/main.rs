use crate::node::Model;
use chrono::Datelike;
use quote::quote;
use std::error::Error;
use std::fs::{File, OpenOptions};
use std::io;
use std::io::prelude::*;
use std::io::BufWriter;
use std::path::PathBuf;
use std::process::Command;

mod node;
mod serialize;
mod serialized_to_model;
mod token;

struct RustFile {
    path: PathBuf,
    inner: BufWriter<File>,
}

impl Drop for RustFile {
    fn drop(&mut self) {
        self.inner.flush().expect("Cannot flush file");
        Command::new("rustfmt")
            .arg(&self.path)
            .spawn()
            .expect("cannot spawn rustfmt")
            .wait()
            .expect("rustfmt wasn't running");
    }
}

impl Write for RustFile {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.inner.write(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.inner.flush()
    }
}

fn write_header(write: &mut impl Write) -> io::Result<()> {
    write!(
        write,
        "\
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) {}, Lukas Scheller lukasscheller@icloud.com
",
        chrono::Utc::now().year()
    )
}

fn new_rust_file(name: impl Into<PathBuf>) -> io::Result<RustFile> {
    let path = name.into().with_extension("rs");
    let file = OpenOptions::new()
        .create(true)
        .truncate(true)
        .write(true)
        .open(&path)?;
    let mut writer = BufWriter::new(file);
    write_header(&mut writer)?;
    Ok(RustFile {
        inner: writer,
        path,
    })
}

fn main() -> Result<(), Box<dyn Error>> {
    let mut syntax_definitions = std::env::current_dir()?;

    syntax_definitions.extend(["src", "syntax_definitions"]);

    let mut model = Model::default();

    for entry in std::fs::read_dir(&syntax_definitions)? {
        let entry = entry?;
        let path = entry.path();
        if let Some(extension) = path.extension() {
            if extension == "yaml" {
                let file = File::open(&path)?;
                let nodes: serialize::Nodes = serde_yml::from_reader(file)
                    .map_err(|err| format!("{err} while processing {}", path.display()))?;
                let section = path.file_stem().unwrap().to_str().unwrap();
                model.insert_ser_nodes(section, nodes);
            }
        }
    }

    model.do_checks();
    model.do_postprocessing();

    // Generate output files
    for (category, nodes) in model.sections() {
        let mut rust_file = new_rust_file(category)?;
        // Header
        writeln!(
            rust_file,
            "{}",
            quote! {
                use super::*;
                use crate::syntax::node::{SyntaxNode, SyntaxToken};
                use crate::syntax::node_kind::NodeKind;
                use crate::syntax::AstNode;
                use crate::tokens::Keyword as Kw;
                use crate::tokens::TokenKind::*;
            }
        )?;
        writeln!(rust_file)?;
        for node in nodes {
            writeln!(rust_file, "{}", node.generate_rust_struct())?;
            writeln!(rust_file, "{}", node.generate_ast_node_rust_impl())?;
            writeln!(rust_file, "{}", node.generate_rust_impl_getters())?;
        }
    }

    // Generate node_kind.rs file
    let mut node_kinds = new_rust_file("node_kind")?;
    writeln!(node_kinds, "{}", model.generate_node_kind_enum())?;

    // Generate mod.rs file
    let mut mod_rs = new_rust_file("mod")?;
    writeln!(mod_rs, "{}", model.generate_mod())?;

    Ok(())
}
