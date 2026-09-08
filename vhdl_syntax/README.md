# VHDL Syntax Library

A lossless library for tokenizing, parsing, inspecting, and modifying VHDL code.

Currently processes VHDL-2008 code.

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

```rust
use vhdl_syntax::parser;

let vhdl_file = b"\
library ieee;
    use ieee.std_logic_1164.all;

entity foo is
    port (
        clk : in std_logic
    );
end entity foo;
";
let (cst, diagnostics) = parser::parse(vhdl_file);
```

`diagnostics` is a `Vec<SyntaxErr>` that reports any syntactical errors from the input file. `cst` is the concrete syntax tree describing the parsed VHDL file.

Refer to the examples for concrete use-cases:

## Examples

All examples can be found in the [examples](./examples/) folder.

### [doc_extraction](./examples/doc_extraction.rs)

Contains a minimalistic documentation extraction tool.

### [empty_entity_builder](./examples/empty_entity_builder.rs)

Uses the builder API to programmatically construct an entity declaration from scratch.

### [import_sorting](./examples/import_sorting.rs)

Alphabetically sorts and deduplicates the `use` clauses of a context clause using the rewriter API.

### [linting](./examples/linting.rs)

Shows how to use this crate to build a simple VHDL linter

### [name_changing](./examples/name_changing.rs)

Showcases the AST rewriting capabilities to change the name of a VHDL entity.

### [source_refactoring](./examples/source_refactoring.rs)

Shows how the library can be used to exchange source code elements that are more than just a token.

### [testbench_generation](./examples/testbench_generation.rs)

Walks a design and emits a minimal testbench skeleton for every entity it finds.
