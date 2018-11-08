# Overview
This repository is a collection of HDL related tools.

**NOTE**: These tools are currently only recommended to be used by people interested in contributing to their development as they are still immature.

I am interested in collaboration with other people especially regarding semantic analysis of VHDL.

[![Join the chat at https://gitter.im/rust_hdl/Lobby](https://badges.gitter.im/rust_hdl/Lobby.svg)](https://gitter.im/rust_hdl/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

## Why Rust?
The tools within this repository have been written in the Rust programming language, thus the name `rust_hdl`. 
The Rust programming language was choosen over Python, C or C++ as the best suited language to write the tools in.
The main advantages are; 
- Excellent performance
- Strong modern type system
- Good libraries and tools
- Strong community and momentum

To install rust and its build tool cargo: https://rustup.rs/

# Projects
# VHDL Parser
### Goals
- This project aims to provide a fully feature open source VHDL parser that is easy to integrate into other tools.
- A design goal of the parser is to be able to recover from errors such that it is useful for building a language server. 
- Comments will be part of the AST to support document generation.
- Separate parsing from semantic analysis to allow code formatting on non-semantically correct code.

### Current status
- The current status is that a lot VHDL-2008 is parsed, enough to parse (VUnit, PoC, UVVM, OSVVM).
- The parser is a using hand written recursive descent since VHDL is not suitable for parser generators.
- Error recovery is still very rudimentary.
- No semantic analysis is done yet.
- Comments not part of AST yet.
- Good performance, can parse 200k lines of code in 200 ms on my laptop which is 39 MB/s of parsing throughput.

## Trying it out
The VHDL parser has a command line demonstrator which will parse a list of files and print information about the parse results. The command line tool currently only serves as a demonstrator and has no intended usability at this point.

### Example output
```console
> cd rust_hdl/
> cargo build --release
> cargo run --bin vhdl_parser uart_rx.vhd uart_tx.vhd
Results from uart_rx.vhd
entity uart_rx
  with 1 generics
  with 6 ports
  with 6 concurrent statements
architecture a of uart_rx
  with 1 declarations
  with 2 concurrent statements

Results from uart_tx.vhd
entity uart_tx
  with 1 generics
  with 5 ports
  with 6 concurrent statements
architecture a of uart_tx
  with 1 declarations
  with 2 concurrent statements
```

## VHDL Language Server
### Goals
- A complete VHDL language server protocol implementation with diagnostics, navigate to symbol, find all references etc.

### Status
- Basic diagnosics based on parse errors and warnings.
- Only full document sync

## Trying it out
The language server has a command line binary `vhdl_ls` which implements a stdio based language server. 

### Building
```console
> cd rust_hdl
> cargo build --release
```
### Use in emacs
Using the language server in `emacs` with the `eglot` language server package it is enough to simply add the following line to your `.emacs.el`:
```elisp
(require 'eglot)
(add-to-list 'eglot-server-programs
             '(vhdl-mode . ("<PATH_TO_RUST_HDL>/target/release/vhdl_ls")))
```
