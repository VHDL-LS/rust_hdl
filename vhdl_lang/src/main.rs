// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use clap::{Parser, ValueEnum};
use itertools::Itertools;
use serde::Serialize;
use std::iter::zip;
use std::path::{Path, PathBuf};
use vhdl_lang::ast::DesignFile;
use vhdl_lang::{
    Config, DesignHierarchyNode, Diagnostic, HierarchyKind, MessagePrinter, Project, Severity,
    SeverityMap, Source, SrcPos, VHDLFormatter, VHDLParser, VHDLStandard,
};

#[derive(Debug, clap::Args)]
#[group(required = true, multiple = false)]
pub struct Group {
    /// Config file in TOML format containing libraries and settings
    #[arg(short, long)]
    config: Option<String>,

    /// Format the passed file and write the contents to stdout.
    ///
    /// This is experimental and the formatting behavior will change in the future.
    #[arg(short, long)]
    format: Option<String>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, ValueEnum)]
enum HierarchyFormat {
    Text,
    Json,
}

/// Run vhdl analysis
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// The number of threads to use. By default, the maximum is selected based on process cores
    #[arg(short = 'p', long)]
    num_threads: Option<usize>,

    /// Path to the config file for the VHDL standard libraries (i.e., IEEE std_logic_1164).
    /// If omitted, will search for these libraries in a set of standard paths
    #[arg(short = 'l', long)]
    libraries: Option<String>,

    /// Print the design instantiation hierarchy starting at the given top
    /// entity. Accepts `library.entity`. A bare `entity` is resolved against
    /// the project's libraries: if exactly one user library is configured,
    /// or if a library named `work` is present, that one is used; otherwise
    /// the known libraries are listed and the command exits non-zero.
    /// Requires `--config`.
    #[arg(long, value_name = "[LIB.]ENTITY")]
    hierarchy: Option<String>,

    /// Output format for `--hierarchy`.
    #[arg(long, value_enum, default_value_t = HierarchyFormat::Text)]
    hierarchy_format: HierarchyFormat,

    /// Suppress diagnostics output when running `--hierarchy`. The hierarchy
    /// is still printed even if analysis produced errors.
    #[arg(long)]
    hierarchy_quiet: bool,

    #[clap(flatten)]
    group: Group,
}

fn main() {
    let args = Args::parse();
    if let Some(top) = args.hierarchy.clone() {
        let config_path = args.group.config.clone().unwrap_or_else(|| {
            eprintln!("--hierarchy requires --config");
            std::process::exit(2);
        });
        run_hierarchy(
            &config_path,
            args.num_threads,
            args.libraries.as_ref(),
            &top,
            args.hierarchy_format,
            args.hierarchy_quiet,
        );
    } else if let Some(config_path) = args.group.config {
        parse_and_analyze_project(&config_path, args.num_threads, args.libraries.as_ref());
    } else if let Some(format) = args.group.format {
        format_file(format);
    }
}

fn format_file(format: String) {
    let path = PathBuf::from(format);
    let parser = VHDLParser::new(VHDLStandard::default());
    let mut diagnostics = Vec::new();
    let result = parser.parse_design_file(&path, &mut diagnostics);
    match result {
        Ok((_, design_file)) => {
            if !diagnostics.is_empty() {
                show_diagnostics(&diagnostics, &SeverityMap::default());
                std::process::exit(1);
            }
            let result = VHDLFormatter::format_design_file(&design_file);
            println!("{result}");
            check_formatted_file(&path, &parser, design_file, &result);
            std::process::exit(0);
        }
        Err(err) => {
            println!("{err}");
            std::process::exit(1);
        }
    }
}

fn check_formatted_file(path: &Path, parser: &VHDLParser, design_file: DesignFile, result: &str) {
    let mut diagnostics: Vec<Diagnostic> = Vec::new();
    let new_file = parser.parse_design_source(&Source::inline(path, result), &mut diagnostics);
    if !diagnostics.is_empty() {
        println!("Formatting failed as it resulted in a syntactically incorrect file.");
        show_diagnostics(&diagnostics, &SeverityMap::default());
        std::process::exit(1);
    }
    for ((tokens_a, _), (tokens_b, _)) in zip(new_file.design_units, design_file.design_units) {
        for (a, b) in zip(tokens_a, tokens_b) {
            if !a.equal_format(&b) {
                println!("Token mismatch");
                println!("New Token={a:#?}");
                let contents = a.pos.source.contents();
                let a_line = contents.get_line(a.pos.range.start.line as usize).unwrap();
                println!("    {a_line}");
                println!("Old Token={b:#?}");
                let b_line = result.lines().nth(b.pos.range.start.line as usize).unwrap();
                println!("    {b_line}");
                break;
            }
        }
    }
}

fn parse_and_analyze_project(
    config_path: &str,
    num_threads: Option<usize>,
    libraries: Option<&String>,
) {
    rayon::ThreadPoolBuilder::new()
        .num_threads(num_threads.unwrap_or(0))
        .build_global()
        .unwrap();

    let mut config = Config::default();
    let mut msg_printer = MessagePrinter::default();
    config.load_external_config(&mut msg_printer, libraries.cloned());
    config.append(
        &Config::read_file_path(Path::new(&config_path)).expect("Failed to read config file"),
        &mut msg_printer,
    );

    let severity_map = *config.severities();
    let mut project = Project::from_config(config, &mut msg_printer);
    project.enable_all_linters();
    let diagnostics = project.analyse();

    show_diagnostics(&diagnostics, &severity_map);

    if diagnostics
        .iter()
        .any(|diag| severity_map[diag.code].is_some_and(|severity| severity == Severity::Error))
    {
        std::process::exit(1);
    } else {
        std::process::exit(0);
    }
}

fn run_hierarchy(
    config_path: &str,
    num_threads: Option<usize>,
    libraries: Option<&String>,
    top: &str,
    fmt: HierarchyFormat,
    quiet: bool,
) {
    rayon::ThreadPoolBuilder::new()
        .num_threads(num_threads.unwrap_or(0))
        .build_global()
        .ok();

    let mut config = Config::default();
    let mut msg_printer = MessagePrinter::default();
    config.load_external_config(&mut msg_printer, libraries.cloned());
    config.append(
        &Config::read_file_path(Path::new(config_path)).expect("Failed to read config file"),
        &mut msg_printer,
    );

    let severity_map = *config.severities();
    let mut project = Project::from_config(config, &mut msg_printer);
    let diagnostics = project.analyse();

    if !quiet {
        show_diagnostics(&diagnostics, &severity_map);
    }

    let (lib, ent) = match top.split_once('.') {
        Some((l, e)) => (l.to_string(), e.to_string()),
        None => match resolve_default_library(&project) {
            Ok(l) => (l, top.to_string()),
            Err(err) => {
                eprintln!("hierarchy error: {err}");
                std::process::exit(1);
            }
        },
    };

    match project.design_hierarchy(&lib, &ent) {
        Ok(node) => {
            let out = match fmt {
                HierarchyFormat::Text => format_hierarchy_text(&node),
                HierarchyFormat::Json => format_hierarchy_json(&node),
            };
            print!("{out}");
            std::process::exit(0);
        }
        Err(err) => {
            eprintln!("hierarchy error: {err}");
            std::process::exit(1);
        }
    }
}

/// Pick a default library when the user passes a bare entity name.
///
/// Strategy:
/// * if a library named `work` is configured, use it;
/// * else if there is exactly one user library, use it;
/// * else fail and list the known libraries.
///
/// "User libraries" excludes the standard libraries (`std`, `ieee`, `vunit_lib`)
/// that the analyzer always loads, since those are never sensible defaults.
fn resolve_default_library(project: &Project) -> Result<String, String> {
    let all = project.library_names();
    if all.iter().any(|n| n == "work") {
        return Ok("work".into());
    }
    let user: Vec<String> = all
        .iter()
        .filter(|n| !is_standard_library(n))
        .cloned()
        .collect();
    match user.len() {
        1 => Ok(user.into_iter().next().unwrap()),
        0 => Err(format!(
            "no user libraries configured (only standard libraries: {})",
            all.join(", ")
        )),
        _ => Err(format!(
            "ambiguous library: prefix the entity with one of [{}]",
            user.join(", ")
        )),
    }
}

fn is_standard_library(name: &str) -> bool {
    matches!(name, "std" | "ieee" | "vunit_lib" | "vunit_libs")
}

// --- Hierarchy text & JSON rendering -----------------------------------------

fn format_hierarchy_text(node: &DesignHierarchyNode) -> String {
    let mut out = String::new();
    write_text(node, &mut out, "", true, true);
    out
}

fn write_text(
    node: &DesignHierarchyNode,
    out: &mut String,
    prefix: &str,
    is_last: bool,
    is_root: bool,
) {
    use std::fmt::Write;

    let connector = if is_root {
        ""
    } else if is_last {
        "`- "
    } else {
        "+- "
    };
    let _ = write!(out, "{prefix}{connector}");

    if let Some(label) = &node.label {
        let _ = write!(out, "{label} : ");
    }
    let _ = write!(out, "{}", node.entity_path);
    if let Some(arch) = &node.architecture {
        let _ = write!(out, "({arch})");
    }
    let suffix = match node.kind {
        HierarchyKind::Top | HierarchyKind::DirectEntity | HierarchyKind::BoundComponent => "",
        HierarchyKind::UnboundComponent => "  [component, unbound]",
        HierarchyKind::Configuration => "  [configuration]",
        HierarchyKind::Unresolved => "  [unresolved]",
    };
    let _ = write!(out, "{suffix}");

    let pos = node.instance_pos.as_ref().or(node.entity_pos.as_ref());
    if let Some(pos) = pos {
        let _ = write!(out, "  @{}", format_pos(pos));
    }
    out.push('\n');

    let note_prefix = format!(
        "{prefix}{}",
        if is_root || is_last { "   " } else { "|  " }
    );
    for note in &node.notes {
        let _ = writeln!(out, "{note_prefix}# {note}");
    }
    let child_prefix = format!(
        "{prefix}{}",
        if is_root {
            ""
        } else if is_last {
            "   "
        } else {
            "|  "
        }
    );
    for (i, child) in node.children.iter().enumerate() {
        write_text(
            child,
            out,
            &child_prefix,
            i + 1 == node.children.len(),
            false,
        );
    }
}

fn format_pos(pos: &SrcPos) -> String {
    format!(
        "{}:{}:{}",
        pos.source.file_name().display(),
        pos.range.start.line + 1,
        pos.range.start.character + 1,
    )
}

fn format_hierarchy_json(node: &DesignHierarchyNode) -> String {
    let serializable = SerNode::from(node);
    let mut s = serde_json::to_string_pretty(&serializable).expect("serialize hierarchy");
    s.push('\n');
    s
}

#[derive(Serialize)]
#[serde(rename_all = "snake_case")]
struct SerNode<'a> {
    label: Option<&'a str>,
    entity: &'a str,
    architecture: Option<&'a str>,
    kind: &'static str,
    instance_pos: Option<String>,
    entity_pos: Option<String>,
    notes: &'a [String],
    children: Vec<SerNode<'a>>,
}

impl<'a> From<&'a DesignHierarchyNode> for SerNode<'a> {
    fn from(node: &'a DesignHierarchyNode) -> SerNode<'a> {
        SerNode {
            label: node.label.as_deref(),
            entity: &node.entity_path,
            architecture: node.architecture.as_deref(),
            kind: kind_str(node.kind),
            instance_pos: node.instance_pos.as_ref().map(format_pos),
            entity_pos: node.entity_pos.as_ref().map(format_pos),
            notes: &node.notes,
            children: node.children.iter().map(SerNode::from).collect(),
        }
    }
}

fn kind_str(k: HierarchyKind) -> &'static str {
    match k {
        HierarchyKind::Top => "top",
        HierarchyKind::DirectEntity => "entity",
        HierarchyKind::BoundComponent => "bound_component",
        HierarchyKind::UnboundComponent => "unbound_component",
        HierarchyKind::Configuration => "configuration",
        HierarchyKind::Unresolved => "unresolved",
    }
}

fn show_diagnostics(diagnostics: &[Diagnostic], severity_map: &SeverityMap) {
    let diagnostics = diagnostics
        .iter()
        .filter_map(|diag| diag.show(severity_map))
        .collect_vec();
    for str in &diagnostics {
        println!("{str}");
    }

    if !diagnostics.is_empty() {
        println!("Found {} diagnostics", diagnostics.len());
    }
}
