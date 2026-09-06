# VHDL Syntax Library

A lossless library for tokenizing, parsing, inspecting, and modifying VHDL code.

## Status

> [!WARNING]
> **Early Stage**: This crate is in an early stage of development.
> Until Version 1.0, all public API methods are subject to change at any given time.
> Bugs are expected to occur at any time.

## Quickstart

Add the package:

```shell
cargo add vhdl_syntax
```

To parse a VHDL file, call the `parse` function from the `parser` submodule:

```rust,ignore
use vhdl_syntax::parser;

let vhdl_file = ...
let (cst, diagnostics) = parser::parse(vhdl_file);
```

`diagnostics` is a `Vec<SyntaxErr>` that reports any syntactical errors from the input file. `cst` is the concrete syntax tree describing the parsed VHDL file.

`SyntaxErr` implements `std::error::Error` and `Display` (`<span> <message>`, e.g. `4..12 unterminated string literal`); use `parser::error::display_errors` to render a whole diagnostic list, one error per line. Human-oriented messages with line/column information require true source locations — see the `text` module for that.

## Examples

All examples can be found in the [examples](./examples/) folder.

### [doc_extraction](./examples/doc_extraction.rs)

Contains a minimalistic documentation extraction tool.

### [linting](./examples/linting.rs)

Shows how to use this crate to build a simple VHDL linter

### [name_changing](./examples/name_changing.rs)

Showcases the AST rewriting capabilities to change the name of a VHDL entity.

### [source_refactoring](./examples/source_refactoring.rs)

Shows how the library can be used to exchange source code elements that are more than just a token.
