use std::error::Error;
use std::fs;
use std::iter::zip;
use std::path::{Path, PathBuf};
use vhdl_lang::{format_design_file, SeverityMap, Source, VHDLParser, VHDLStandard};

// excluded file contains PSL statements
const EXCLUDED_FILES: [&str; 1] = ["vunit/examples/vhdl/array_axis_vcs/src/fifo.vhd"];

fn format_file(path: &Path) -> Result<(), Box<dyn Error>> {
    let severity_map = SeverityMap::default();
    let parser = VHDLParser::new(VHDLStandard::default());
    let mut diagnostics = Vec::new();
    let (_, design_file) = parser.parse_design_file(&path, &mut diagnostics)?;
    if !diagnostics.is_empty() {
        for diagnostic in diagnostics {
            println!("{}", diagnostic.show(&severity_map).unwrap())
        }
        panic!("Found diagnostics with severity error in the example project");
    }

    let result = format_design_file(&design_file);
    let new_file = parser.parse_design_source(&Source::inline(&path, &result), &mut diagnostics);
    if !diagnostics.is_empty() {
        for diagnostic in diagnostics {
            println!("{}", diagnostic.show(&severity_map).unwrap())
        }
        panic!("Formatting failed! File was OK before, but is not after");
    }
    for ((tokens_a, _), (tokens_b, _)) in zip(new_file.design_units, design_file.design_units) {
        for (a, b) in zip(tokens_a, tokens_b) {
            if a.kind != b.kind || a.value != b.value {
                println!("New Token={a:#?}");
                let contents = a.pos.source.contents();
                let a_line = contents.get_line(a.pos.range.start.line as usize).unwrap();
                println!("    {a_line}");
                println!("Old Token={b:#?}");
                let b_line = result.lines().nth(b.pos.range.start.line as usize).unwrap();
                println!("    {b_line}");
                panic!("Token Mismatch")
            }
        }
    }
    Ok(())
}

fn format_dir(path: &Path) -> Result<(), Box<dyn Error>> {
    for entry in fs::read_dir(path)? {
        let entry = entry?;
        let file_type = entry.file_type()?;
        if file_type.is_dir() {
            format_dir(&entry.path())?
        } else if let Some(extension) = entry.path().extension() {
            if (extension == "vhd" || extension == "vhdl") && !is_file_excluded(&entry.path()) {
                format_file(&entry.path())?
            }
        }
    }
    Ok(())
}

fn is_file_excluded(path: &Path) -> bool {
    for file in EXCLUDED_FILES {
        if path.ends_with(file) {
            return true;
        }
    }
    false
}

// Checks that all files in the example project are correctly formatted
// while retaining their token stream.
#[test]
fn formats_all_vhdl_files_without_producing_different_code() -> Result<(), Box<dyn Error>> {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push("../example_project");
    format_dir(&path)
}
