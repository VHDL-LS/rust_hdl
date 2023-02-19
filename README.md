# Overview
This repository contains a fast VHDL language server and analysis library written in Rust.

The speed makes the tool very pleasant to use since it loads projects really fast and does not consume a lot of ram.
A 200.000 line VHDL project is analyzed in 160 ms on my Desktop using 8 cores and only consumes 180 MByte of RAM when loaded. 

I very much appreciate help from other people especially regarding semantic analysis of VHDL. You do not need to be a programmer to help, it is even more helpful to interpret and clarify the VHDL standard and provide minimal examples and describe how they should work according to the standard. Further information about contributing can be found by reading the [Contributors Guide](https://github.com/kraigher/rust_hdl/wiki/Contributor-Guide) 

[![Chat](https://img.shields.io/matrix/VHDL-LS:matrix.org)](https://matrix.to/#/#VHDL-LS:matrix.org)
[![Build Status](https://github.com/kraigher/rust_hdl/workflows/Build%20%26%20test%20all%20configs/badge.svg)](https://github.com/kraigher/rust_hdl/actions?query=workflow%3A%22Build+%26+test+all+configs%22)

# Projects
## VHDL Language Server
[![vhdl ls crate](https://img.shields.io/crates/v/vhdl_ls.svg)](https://crates.io/crates/vhdl_ls)
### Goals
- A complete VHDL language server protocol implementation with diagnostics, navigate to symbol, find all references etc.

### Features
- Live syntax and type checking 
- Checks for missing and duplicate declarations
- Supports goto-definition/declaration (also in presence of overloading)
- Supports find-references (also in presence of overloading)
- Supports goto-implementation
  - From component declaration to matching entity by default binding
  - From entity to matching component declaration by default binding
- Supports hovering symbols
- Rename symbol
- Find workspace symbols
- View/find document symbols



## Trying it out
A language server is never used directly by the end user and it is integrated into different editor plugins. The ones I know about are listed here.

## Use in VSCode
https://github.com/Bochlin/rust_hdl_vscode

## Use in emacs
VHDL LS has built-in support by emacs `lsp-mode` since 2020-01-04.

### Configuration
The language server needs to know your library mapping to perform full analysis of the code. For this it uses a configuration file in the [TOML](https://github.com/toml-lang/toml) format named `vhdl_ls.toml`.

`vhdl_ls` will load configuration files in the following order of priority (first to last):
1. A file named `.vhdl_ls.toml` in the user home folder.
2. A file name from the `VHDL_LS_CONFIG` environment variable.
3. A file named `vhdl_ls.toml` in the workspace root.

Settings in a later files overwrites those from previously loaded files.

**Example vhdl_ls.toml**

```toml
# File names are either absolute or relative to the parent folder of the vhdl_ls.toml file
[libraries]
lib2.files = [
  'pkg2.vhd',
]
lib1.files = [
  'pkg1.vhd',
  'tb_ent.vhd'
]
```

## As an LSP-client developer how should I integrate VHDL-LS?
I recommend that the `lsp-client` polls GitHub and downloads the [latest](https://github.com/VHDL-LS/rust_hdl/releases/latest) VHDL-LS release from GitHub.

VHDL-LS has frequent releases and the automatic update ensures minimal maintenance for the `lsp-client` developer as well as ensuring the users are not running and outdated version.

## VHDL Language Frontend
[![vhdl language frontend crate](https://img.shields.io/crates/v/vhdl_lang.svg)](https://crates.io/crates/vhdl_lang)
### Goals
- This project aims to provide a fully featured open source VHDL frontend that is easy to integrate into other tools.
- A design goal of the frontend is to be able to recover from syntax errors such that it is useful for building a language server.
- Analysis order must be automatically computed such that the user does not have to maintain a compile order.
- Comments will be part of the AST to support document generation.
- Separate parsing from semantic analysis to allow code formatting on non-semantically correct code.

