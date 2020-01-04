# Overview
This repository is a collection of HDL related tools.

**NOTE**: These tools are at a usable state however they are still intensively developed and thus they are currently only recommended to be used by early adopters and people interested in contributing to the development.

I am interested in [collaboration](#Collaboration) with other people especially regarding semantic analysis of VHDL.

[![Join the chat at https://gitter.im/rust_hdl/Lobby](https://badges.gitter.im/rust_hdl/Lobby.svg)](https://gitter.im/rust_hdl/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![Build Status](https://github.com/kraigher/rust_hdl/workflows/Build%20%26%20test%20all%20configs/badge.svg)](https://github.com/kraigher/rust_hdl/actions?query=workflow%3A%22Build+%26+test+all+configs%22)

# Projects
## VHDL Language Frontend
[![vhdl language frontend crate](https://img.shields.io/crates/v/vhdl_lang.svg)](https://crates.io/crates/vhdl_lang)
### Goals
- This project aims to provide a fully featured open source VHDL frontend that is easy to integrate into other tools.
- A design goal of the frontend is to be able to recover from syntax errors such that it is useful for building a language server.
- Analysis order must be automatically computed such that the user does not have to maintain a compile order.
- Comments will be part of the AST to support document generation.
- Separate parsing from semantic analysis to allow code formatting on non-semantically correct code.

### Current status
- Almost all of VHDL-2008 is parsed, enough to parse the most popular VHDL projects on GitHub:
   - https://github.com/VUnit/vunit
   - https://github.com/OSVVM/OSVVM
   - https://github.com/UVVM/UVVM
   - https://github.com/VLSI-EDA/PoC
   - https://github.com/FPHDL/fphdl/
   - https://github.com/inforichland/freezing-spice (Contains one file with actual syntax error)
   - https://github.com/sergeykhbr/riscv_vhdl
   - https://github.com/fabioperez/space-invaders-vhdl
   - https://github.com/BG2BKK/img_process_vhdl (Contains one file with actual syntax error)
   - https://github.com/kevinpt/vhdl-extras
   - https://github.com/xesscorp/VHDL_Lib
   - Also parses IEEE/IEEE_2008/VITAL_95/VITAL_2000
- Good performance, can parse 440k lines of code (all repos above) in 500 ms on my laptop which is 45.5 MB/s of parsing throughput.
- Provides nice error messages such as:
```
error: Expected 'use', 'type', 'subtype', 'shared', 'constant', 'signal', 'variable', 'file', 'component', 'attribute', 'alias', 'impure', 'function', 'procedure', 'package' or 'for'
   --> example.vhd:19
    |
17  |  package pkg2 is
18  |    constant foo : natural := 22;
19 -->   error
    |    ~~~~~
20  |  end package;
21  |
```

- The parser is a using hand written recursive descent since VHDL is not suitable for parser generators.
- Error recovery is still very rudimentary.
- Semantic analysis is ongoing work, currently checks for:
  - Legal primary/secondary design unit combinations
  - Duplicate design units
  - Secondary units without primary unit
  - Duplicate definitions in declarative parts
    - Forbids overloaded name to co-exist with non-overloaded name
  - Missing full constant for deferred constant
  - Missing full type for deferred type
  - Missing body for protected type and vice versa
  - Missing declarations in type marks of most subtype indications
- Comments not part of AST yet.

## Trying it out
The VHDL language frontend has a command line demonstrator which will parse a list of files and print syntax and analysis errors. The command line tool currently only serves as a demonstrator and has no intended usability at this point.

## VHDL Language Server
[![vhdl ls crate](https://img.shields.io/crates/v/vhdl_ls.svg)](https://crates.io/crates/vhdl_ls)
### Goals
- A complete VHDL language server protocol implementation with diagnostics, navigate to symbol, find all references etc.

### Status
- Publishes diagnosics based on parse errors and warnings as well as semantic analysis.
- Usable today to get full live syntax error checking.
- Checks for missing and duplicate declarations
- Supports goto-definition/declaration
- Supports find-references

## Trying it out
The language server has a command line binary `vhdl_ls` which implements a stdio based language server.
This repository includes a medium sized example [project](example_project/README.md) which can be used to trying out the language server.

### Building
```console
> cd rust_hdl
> cargo build --release
```

### Configuration
The language server uses a configuration file in the [TOML](https://github.com/toml-lang/toml) format named `vhdl_ls.toml`.
The file contains the library mapping of all files within the project. Files outside of the project without library mapping are checked for syntax errors only.

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

### Use in emacs
#### lsp-mode
Add the following to your `.emacs.el`:
```elisp
(require 'lsp-mode)
(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection "${PATH_TO_RUST_HDL}/target/release/vhdl_ls")
                  :major-modes '(vhdl-mode)
                  :server-id 'vhdl-lsp))
(add-to-list 'lsp-language-id-configuration '(vhdl-mode . "vhdl-mode"))
(add-hook 'vhdl-mode-hook #'lsp)
```

### Use in Atom
https://github.com/mbrobbel/atom-ide-vhdl

### Use in VSCode
https://github.com/Bochlin/rust_hdl_vscode

# Collaboration
My hope is that other people will be interested in this project and contribute.

Some rules for collaboration:
- Contributions will have to assign copyright to me. Copyright is not about attribution or recognition it is about legal control of a project. Having several independent copyright holders in a project makes future changes difficult.
  - I will add a list of notable contributors to the project to assign recognition.
- Quality is a priorty. Code has to be elegant and well tested.

Collaboration is also hard, you have to communicate with other people to do something coordinated together. Thus to collaborate we need to spend some time discussing *what*, *why* and *how* in GitHub issues or on the chat channel before diving in to the coding. This ensures that we work in the same direction.

# Why Rust?
The tools within this repository have been written in the Rust programming language, thus the name `rust_hdl`.
The Rust programming language was choosen over Python, C or C++ as the best suited language to write the tools in.
The main advantages are;
- Excellent performance
- Strong modern type system
- Good libraries and tools
- Strong community and momentum

To install rust and its build tool cargo: https://rustup.rs/
