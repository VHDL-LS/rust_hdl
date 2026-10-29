// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2026, Olof Kraigher olof.kraigher@gmail.com

//! A `std.standard` that names the required types in lower case is still `std.standard`.
//!
//! VHDL identifiers are case insensitive, so a vendor is free to write `type character is`
//! rather than `type CHARACTER is`, and some do. `StandardTypes::new` has to find those
//! types whatever case they are written in, or analysis cannot start at all.

use crate::analysis::DesignRoot;
use crate::data::*;
use crate::syntax::test::CodeBuilder;
use std::path::Path;

/// The real standard package, with every required type name written in lower case.
///
/// Only the type and subtype names are changed. Lower casing the whole file would also
/// change the enumeration literals of `CHARACTER`, where `'A'` and `'a'` are two distinct
/// literals, and the result would not be legal VHDL.
fn lower_case_standard_package() -> Source {
    let mut text =
        Latin1String::new(include_bytes!("../../../../vhdl_libraries/std/standard.vhd")).to_string();

    for name in [
        "BOOLEAN",
        "BIT",
        "BIT_VECTOR",
        "CHARACTER",
        "STRING",
        "INTEGER",
        "NATURAL",
        "REAL",
        "TIME",
        "FILE_OPEN_KIND",
        "FILE_OPEN_STATUS",
        "SEVERITY_LEVEL",
    ] {
        for keyword in ["type", "subtype"] {
            let declaration = format!("{keyword} {name} is");
            text = text.replace(&declaration, &declaration.to_lowercase());
        }
    }

    assert!(
        text.contains("type character is"),
        "the standard package should now declare 'character' in lower case"
    );
    Source::inline(Path::new("standard.vhd"), &text)
}

fn std_source(file_name: &str, bytes: &[u8]) -> Source {
    Source::inline(Path::new(file_name), &Latin1String::new(bytes).to_string())
}

#[test]
fn standard_package_type_names_are_case_insensitive() {
    let builder = CodeBuilder::new();
    let symbols = builder.symbols.clone();
    let mut root = DesignRoot::new(symbols.clone());

    let std_sym = symbols.symtab().insert_utf8("std");
    for source in [
        lower_case_standard_package(),
        std_source(
            "textio.vhd",
            include_bytes!("../../../../vhdl_libraries/std/textio.vhd"),
        ),
        std_source(
            "env.vhd",
            include_bytes!("../../../../vhdl_libraries/std/env.vhd"),
        ),
    ] {
        let code = builder.code_from_source(source);
        root.add_design_file(std_sym.clone(), code.design_file());
    }

    let lib_sym = symbols.symtab().insert_utf8("libname");

    // Every one of the required types has to have been found for this to analyze.
    let uses_them = builder.code(
        "
package uses_the_standard_types is
  constant letter    : character       := 'a';
  constant yes       : boolean         := true;
  constant word      : string          := \"text\";
  constant how_many  : natural         := 1;
  constant pattern   : bit_vector(1 downto 0) := \"01\";
  constant one_bit   : bit             := '1';
  constant how_long  : time            := 1 ns;
  constant how_much  : real            := 1.0;
  constant how_bad   : severity_level  := note;
end package;
",
    );
    root.add_design_file(lib_sym.clone(), uses_them.design_file());

    // And a positive control, so an empty diagnostic list cannot be read as success when
    // analysis never actually ran.
    let has_a_real_error = builder.code(
        "
package refers_to_nothing is
  constant bad : no_such_type := 0;
end package;
",
    );
    root.add_design_file(lib_sym, has_a_real_error.design_file());

    let mut diagnostics = Vec::new();
    root.analyze(&mut diagnostics);

    let messages: Vec<&str> = diagnostics
        .iter()
        .map(|diagnostic| diagnostic.message.as_str())
        .collect();
    assert_eq!(messages, vec!["No declaration of 'no_such_type'"]);
}
