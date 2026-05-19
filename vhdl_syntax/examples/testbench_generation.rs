// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2026, Lukas Scheller lukasscheller@icloud.com

//! Walks a parsed VHDL design and emits a minimal testbench skeleton for every
//! entity it finds. For each entity the generic and port names are extracted
//! from the interface lists, and the testbench instantiates the unit under
//! test as `entity work.<name>` with `name => name` associations.
//!
//! The example only reads the syntax tree (no builder API), so it stays small
//! and works on syntactically valid input.

use vhdl_syntax::parser;
use vhdl_syntax::syntax::node::SyntaxNode;
use vhdl_syntax::syntax::visitor::WalkEvent;
use vhdl_syntax::syntax::{
    AstNode, EntityDeclarationSyntax, InterfaceDeclarationSyntax, InterfaceListSyntax,
    InterfaceObjectDeclarationSyntax,
};

fn main() {
    let vhdl = "\
entity adder is
    generic (WIDTH : positive := 8);
    port (
        a, b : in  std_logic_vector(WIDTH - 1 downto 0);
        sum  : out std_logic_vector(WIDTH - 1 downto 0)
    );
end entity adder;

entity counter is
    port (
        clk : in  std_logic;
        q   : out std_logic_vector(7 downto 0)
    );
end entity counter;

entity blinky is
end entity blinky;
    ";

    let (design, diagnostics) = parser::parse(vhdl);
    assert!(
        diagnostics.is_empty(),
        "Did not expect diagnostics for correct VHDL: {diagnostics:?}"
    );

    for entity in design.walk().filter_map(extract_entity_declaration) {
        let name = extract_entity_name(&entity);
        let generics = extract_generics(&entity);
        let ports = extract_ports(&entity);

        println!("{}", build_testbench(&name, &generics, &ports));
    }
}

fn extract_entity_declaration(event: WalkEvent<SyntaxNode>) -> Option<EntityDeclarationSyntax> {
    match event {
        WalkEvent::Enter(node) => EntityDeclarationSyntax::cast(node),
        WalkEvent::Leave(_) => None,
    }
}

fn extract_entity_name(entity: &EntityDeclarationSyntax) -> String {
    entity
        .entity_declaration_preamble()
        .and_then(|p| p.name_token())
        .map(|t| t.text().to_string())
        .expect("Invalid VHDL")
}

fn extract_generics(entity: &EntityDeclarationSyntax) -> Vec<String> {
    entity
        .entity_header()
        .and_then(|h| h.generic_clause())
        .and_then(|c| c.interface_list())
        .map(|l| interface_names(&l))
        .unwrap_or_default()
}

fn extract_ports(entity: &EntityDeclarationSyntax) -> Vec<String> {
    entity
        .entity_header()
        .and_then(|h| h.port_clause())
        .and_then(|c| c.interface_list())
        .map(|l| interface_names(&l))
        .unwrap_or_default()
}

/// Collect the identifier names declared by a generic- or port-clause's
/// interface list. Subprogram/package/type interfaces are skipped for the simple example.
fn interface_names(list: &InterfaceListSyntax) -> Vec<String> {
    let mut names = Vec::new();
    for decl in list.interface_declarations() {
        let object = match decl {
            InterfaceDeclarationSyntax::InterfaceObjectDeclaration(o) => o,
            _ => continue,
        };
        let id_list = match object {
            InterfaceObjectDeclarationSyntax::InterfaceConstantDeclaration(d) => {
                d.identifier_list()
            }
            InterfaceObjectDeclarationSyntax::InterfaceSignalDeclaration(d) => d.identifier_list(),
            InterfaceObjectDeclarationSyntax::InterfaceVariableDeclaration(d) => {
                d.identifier_list()
            }
            InterfaceObjectDeclarationSyntax::InterfaceFileDeclaration(d) => d.identifier_list(),
        };
        if let Some(id_list) = id_list {
            for token in id_list.identifier_token() {
                names.push(token.text().to_string());
            }
        }
    }
    names
}

fn build_testbench(name: &str, generics: &[String], ports: &[String]) -> String {
    let generic_map = format_map("generic", generics);
    let port_map = format_map("port", ports);
    format!(
        "
entity tb_{name} is
end entity tb_{name};

architecture arch of tb_{name} is
begin
    uut: entity work.{name}{generic_map}{port_map};
end architecture arch;
    "
    )
}

/// Formats a generic or port map associating each name by formal (`name => name`)
fn format_map(kind: &str, names: &[String]) -> String {
    if names.is_empty() {
        return String::default();
    }
    let assocs: Vec<String> = names.iter().map(|n| format!("{n} => {n}")).collect();
    format!(
        "
        {kind} map (
            {}
        )",
        assocs.join(",\n            ")
    )
}
