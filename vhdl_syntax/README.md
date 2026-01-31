# VHDL Syntax Library

This crate contains a library to tokenize, parse, and, in general, work with VHDL files.

## Status

⚠️ **Early Stage**: This crate is in a very early stage of development and is **not intended for production use**.
All public API methods are subject to change at any given time.

## Quickstart

<!-- TODO: Simple qickstart guide -->

## Examples

All examples can be found in the [examples](./examples/) folder.

### [doc_extraction](./examples/doc_extraction.rs)

Contains a minimalistic documentation extraction tool.

### [linting](./examples/linting.rs)

Shows how to use this crate to build a simple VHDL linter

### [name_changing](./examples/name_changing.rs)

Showcases the AST rewriting capabilities to change the name of a VHDL entity.

### [source_refactring](./examples/source_refactoring.rs)

Shows how the library can be used to exchange source code elements that are more
than just a token.
