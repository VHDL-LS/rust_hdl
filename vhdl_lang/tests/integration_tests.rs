use assert_cmd::prelude::*;
use itertools::Itertools;
use predicates::prelude::*;
use std::error::Error;
use std::path::PathBuf;
use std::process::Command;
use vhdl_lang::{Config, MessagePrinter, Project, Severity};

#[test]
pub fn parses_example_project_without_errors() {
    let mut config = Config::default();
    let mut msg_printer = MessagePrinter::default();

    let mut vhdl_libraries_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    // Load the VHDL standard libraries
    vhdl_libraries_path.push("../vhdl_libraries/vhdl_ls.toml");
    config.append(
        &Config::read_file_path(&vhdl_libraries_path).expect("Failed to read config file"),
        &mut msg_printer,
    );

    // Load the configuration from the example project
    let mut config_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    config_path.push("../example_project/vhdl_ls.toml");
    config.append(
        &Config::read_file_path(&config_path).expect("Failed to read config file"),
        &mut msg_printer,
    );

    let severity_map = *config.severities();
    let mut project = Project::from_config(config, &mut msg_printer);
    project.enable_unused_declaration_detection();

    let diagnostics = project.analyse();
    let diagnostics_with_errors = diagnostics
        .iter()
        .filter(|diag| severity_map[diag.code] == Some(Severity::Error))
        .collect_vec();
    if !diagnostics_with_errors.is_empty() {
        for diagnostic in diagnostics_with_errors {
            println!("{}", diagnostic.show(&severity_map).unwrap())
        }
        panic!("Found diagnostics with severity error in the example project");
    }
}

#[test]
fn unused_function_gets_detected() -> Result<(), Box<dyn Error>> {
    let mut cmd = Command::cargo_bin("vhdl_lang")?;

    cmd.arg("--config")
        .arg("tests/unused_declarations/vhdl_ls.toml");
    cmd.assert().failure().stdout(predicate::str::contains(
        "error: Unused declaration of port 'baz' : inout",
    ));

    Ok(())
}
