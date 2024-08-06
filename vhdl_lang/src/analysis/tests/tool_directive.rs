// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2023, Olof Kraigher olof.kraigher@gmail.com
use crate::analysis::tests::{check_no_diagnostics, LibraryBuilder};

#[test]
fn simple_tool_directive() {
    let mut builder = LibraryBuilder::new();
    builder.code("libname", "`protect begin");
    check_no_diagnostics(&builder.analyze());
}

#[test]
fn tool_directive() {
    let mut builder = LibraryBuilder::new();
    builder.code(
        "libname",
        "\
entity my_ent is
end my_ent;

`protect begin_protected
`protect version = 1
`protect encrypt_agent = \"XILINX\"
`protect encrypt_agent_info = \"Xilinx Encryption Tool 2020.2\"
`protect key_keyowner = \"Cadence Design Systems.\", key_keyname = \"cds_rsa_key\", key_method = \"rsa\"
`protect encoding = (enctype = \"BASE64\", line_length = 76, bytes = 64)
        ",
    );
    check_no_diagnostics(&builder.analyze());
}
