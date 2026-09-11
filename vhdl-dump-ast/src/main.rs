use std::{error::Error, fs, path::PathBuf, process::exit};

use clap::{Parser, ValueEnum};
use vhdl_syntax::{
    parser::{self, error::display_errors},
    serde::{SerdeFlags, ToSerializable},
    syntax::node::SyntaxNode,
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

    /// Specify the encoding to use for comments
    #[arg(short, long, default_value = "utf-8")]
    comment_encoding: String,
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
    comment_encoding: String,
) -> Result<String, Box<dyn Error>> {
    let serde_flags = SerdeFlags::default()
        .with_comment_encoding(comment_encoding)
        .include_trivia(trivia);
    let serializable_node = node.serialize_with(serde_flags);
    Ok(match (format, pretty) {
        (OutputFormat::Json, false) => serde_json::to_string(&serializable_node)?,
        (OutputFormat::Json, true) => serde_json::to_string_pretty(&serializable_node)?,
        (OutputFormat::Yaml, _) => serde_yaml_bw::to_string(&serializable_node)?,
    })
}

const EXIT_IO_ERROR: i32 = 1;
const EXIT_SYNTAX_ERROR: i32 = 2;
const EXIT_SERIALIZATION_ERROR: i32 = 3;

fn main() {
    let args = Args::parse();
    let vhdl = match fs::read(&args.file) {
        Ok(contents) => contents,
        Err(e) => {
            eprintln!("Cannot read file {}: {}", args.file.display(), e);
            exit(EXIT_IO_ERROR)
        }
    };
    let (node, errors) = parser::parse(vhdl);
    if !errors.is_empty() {
        eprintln!("{}", display_errors(&errors));
        exit(EXIT_SYNTAX_ERROR);
    }
    let text = match serialize(
        &node,
        args.format,
        !args.no_pretty,
        args.trivia,
        args.comment_encoding,
    ) {
        Ok(text) => text,
        Err(e) => {
            eprintln!("Cannot serialize AST: {e}");
            exit(EXIT_SERIALIZATION_ERROR);
        }
    };
    println!("{}", text);
}
