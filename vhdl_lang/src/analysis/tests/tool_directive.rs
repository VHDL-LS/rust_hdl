use crate::analysis::tests::{check_no_diagnostics, LibraryBuilder};

#[test]
fn simple_tool_directive() {
    let mut builder = LibraryBuilder::new();
    builder.code("libname", "`protect begin");
    let (_, diagnostics) = builder.get_analyzed_root();

    check_no_diagnostics(&diagnostics);
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
    let (_, diagnostics) = builder.get_analyzed_root();

    check_no_diagnostics(&diagnostics);
}
