use std::{error::Error, fs, path::PathBuf, process::exit};

use clap::{Parser, ValueEnum};
use vhdl_syntax::{
    parser::{self},
    serde::{SerdeFlags, ToSerializable},
    syntax::{AstNode, node::SyntaxNode},
};

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// Path to the file
    file: PathBuf,

    /// Output format for the AST dump
    #[arg(short, long, value_enum, default_value_t)]
    format: OutputFormat,

    /// Do not pretty-prints the AST. Has only an effect, if the formats supports this
    #[arg(short, long, default_value = "false")]
    no_pretty: bool,

    /// Includes trivia (spaces, comments) into the dumped AST
    #[arg(short, long, default_value = "false")]
    trivia: bool,

    /// Include source-code location into the dumped AST
    #[arg(short, long, default_value = "false")]
    loc: bool,

    /// Specify the encoding to use for comments
    #[arg(short, long, default_value = "None")]
    comment_encoding: Option<String>,
}

#[derive(Debug, Clone, Copy, Default, ValueEnum)]
enum OutputFormat {
    #[default]
    Json,
    Yaml,
}

fn serialize(
    node: &SyntaxNode,
    format: OutputFormat,
    pretty: bool,
    trivia: bool,
    loc: bool,
    comment_encoding: String,
) -> Result<String, Box<dyn Error>> {
    let serde_flags = SerdeFlags::default()
        .with_comment_encoding(comment_encoding)
        .include_trivia(trivia)
        .include_loc(loc);
    let serializable_node = node.serialize_with(serde_flags);
    Ok(match (format, pretty) {
        (OutputFormat::Json, false) => serde_json::to_string(&serializable_node)?,
        (OutputFormat::Json, true) => serde_json::to_string_pretty(&serializable_node)?,
        (OutputFormat::Yaml, _) => serde_yaml_bw::to_string(&serializable_node)?,
    })
}

const DEFAULT_COMMENT_ENCODING: &str = "utf-8";

fn main() {
    let args = Args::parse();
    // TODO: encoding
    let vhdl = match fs::read_to_string(&args.file) {
        Ok(contents) => contents,
        Err(e) => {
            println!("Cannot read file {}: {}", args.file.display(), e);
            exit(e.raw_os_error().unwrap_or(1))
        }
    };
    // TODO: Do not ignore errors
    let (node, _) = parser::parse(vhdl);
    let text = match serialize(
        &node.raw(),
        args.format,
        !args.no_pretty,
        args.trivia,
        args.loc,
        args.comment_encoding
            .unwrap_or(DEFAULT_COMMENT_ENCODING.to_string()),
    ) {
        Ok(text) => text,
        Err(e) => {
            println!("Cannot serialize AST: {e}");
            exit(2);
        }
    };
    println!("{}", text)
}
