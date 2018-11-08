This repository is a collection of HDL related tools.

**NOTE**: These tools are currently only recommended to use by people interested in contributing to their development as they are still immature.

I am interested in collaboration with other people especially regarding semantic analysis of VHDL.

# VHDL Parser
## Goals
- This project aims to provide a fully feature open source VHDL parser that is easy to integrate into other tools.
- A design goal of the parser is to be able to recover from errors such that it is useful for building a language server. 
- Comments will be part of the AST to support document generation.
- Separate parsing from semantic analysis to allow code formatting on non-semantically correct code.

## Current status
- The current status is that a lot VHDL-2008 is parsed, enough to parse (VUnit, PoC, UVVM, OSVVM).
- The parser is a using hand written recursive descent since VHDL is not suitable for parser generators.
- Error recovery is still very rudimentary.
- No semantic analysis is done yet.
- Comments not part of AST yet.
- Good performance, can parse 200k lines of code in 200 ms on my laptop which is 39 MB/s of parsing throughput.

# VHDL Language Server
## Goals
- A complete VHDL language server protocol implementation with diagnostics, navigate to symbol, find all references etc.

## Status
- Basic diagnosics based on parse errors and warnings.
- Only full document sync
