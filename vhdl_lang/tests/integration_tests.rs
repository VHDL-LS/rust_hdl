use itertools::Itertools;
use std::path::{Path, PathBuf};
use vhdl_lang::{Config, MessagePrinter, Project, Severity};

#[test]
pub fn parses_example_project_without_errors() {
    let mut config_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    config_path.push("../example_project/vhdl_ls.toml");
    let mut config = Config::default();
    let mut msg_printer = MessagePrinter::default();
    config.load_external_config(&mut msg_printer);
    config.append(
        &Config::read_file_path(Path::new(&config_path)).expect("Failed to read config file"),
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
