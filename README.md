# Overview

This repository contains a fast VHDL language server and analysis library written in Rust.

The speed makes the tool very pleasant to use since it loads projects really fast and does not consume a lot of ram.
A 200.000 line VHDL project is analyzed in 160 ms on my Desktop using 8 cores and only consumes 180 MByte of RAM when
loaded.

I very much appreciate help from other people especially regarding semantic analysis of VHDL. You do not need to be a
programmer to help, it is even more helpful to interpret and clarify the VHDL standard and provide minimal examples and
describe how they should work according to the standard. Further information about contributing can be found by reading
the [Contributors Guide](https://github.com/kraigher/rust_hdl/wiki/Contributor-Guide)

[![Chat](https://img.shields.io/matrix/VHDL-LS:matrix.org)](https://matrix.to/#/#VHDL-LS:matrix.org)
[![Build Status](https://github.com/kraigher/rust_hdl/workflows/Build%20%26%20test%20all%20configs/badge.svg)](https://github.com/kraigher/rust_hdl/actions?query=workflow%3A%22Build+%26+test+all+configs%22)

## Contributors

- Maintainer: [Lukas Scheller](https://github.com/Schottkyc137)
- Founder: [Olof Kraigher](https://github.com/kraigher)

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

## When Installing it from Crate

When installing the VHDL_LS from [crates.io](https://crates.io/crates/vhdl_ls) the required  
[vhdl_libraries](https://github.com/VHDL-LS/rust_hdl/tree/master/vhdl_libraries) directory will not be installed
automatically and  
will need to be copied into the parent directory of the VHDL_LS binary manually.

## Trying it out

A language server is never used directly by the end user and it is integrated into different editor plugins. The ones I
know about are listed here.

## Use in VSCode

https://github.com/Bochlin/rust_hdl_vscode

## Use in emacs

VHDL LS has built-in support by emacs `lsp-mode` since 2020-01-04.

It can be set up automatically by installing the package
[`vhdl-ext`](https://github.com/gmlarumbe/vhdl-ext/) and adding the
following snippet to your config:

```elisp
(require 'vhdl-ext)
(vhdl-ext-mode-setup)
(vhdl-ext-eglot-set-server 've-rust-hdl) ;`eglot' config
(vhdl-ext-lsp-set-server 've-rust-hdl)   ; `lsp' config
```

## Installation for Neovim

### Automatic Installation

You can install `rust_hdl` automatically in Neovim using [`:Mason`](https://github.com/williamboman/mason.nvim). Within
Mason, the package is called `rust_hdl`. If you don't have `:Mason`, you can simply install the binary as previously
described.

### Automatic Configuration using `nvim-lspconfig`

[`nvim-lspconfig`](https://github.com/neovim/nvim-lspconfig) has a built in configuration
for [`vhdl_ls`](https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md#vhdl_ls)

In order to configure it, simply add

```lua
lspconfig = require('lspconfig')
lspconfig['vhdl_ls'].setup({
  on_attach = on_attach,
  capabilities = capabilities
})
```

### Manual Configuration using Neovim's built in client

Neovim provides an LSP client to the VHDL_LS language server. Download the  
VHDL_LS release. The binary must be on the path and executable (if you can run  
"vhdl_ls -h" in the terminal then you're good).

In your Neovim config.lua add the following:

```lua
function STARTVHDLLS()
  vim.lsp.start({
    name = 'vhdl_ls',
    cmd = {'vhdl_ls'},
  })
end
vim.api.nvim_set_keymap('n', '<F5>', ':lua STARTVHDLLS()<CR>', { noremap = true, silent = true })
```

Using the example above, pressing F5 while inside Neovim starts the language  
server. There are also other options, like automatically starting it when  
opening a certain file type, see the [Neovim LSP documentation](https://neovim.io/doc/user/lsp.html) for more.

## Configuration

The language server needs to know your library mapping to perform full analysis of the code. For this it uses a
configuration file in the [TOML](https://github.com/toml-lang/toml) format named `vhdl_ls.toml`.

`vhdl_ls` will load configuration files in the following order of priority (first to last):

1. A file named `.vhdl_ls.toml` in the user home folder.
2. A file name from the `VHDL_LS_CONFIG` environment variable.
3. A file named `vhdl_ls.toml` in the workspace root.

Settings in a later files overwrites those from previously loaded files.

Define the VHDL revision to use for parsing and analysis with the `standard` key.
The expected value is the year associated the VHDL standard.
Supported standards are 1993, 2008 and 2019 where both the long version ("2008") and the short version ("08") can be
used.
If nothing is specified, 2008 is used.

> [!NOTE]
> Defining the standard feature is a relatively new feature (since april 2024).
> Anything but the 2008 standard will not change much at the moment.

**Example vhdl_ls.toml**

```toml
# What standard to use. This is optional and defaults to VHDL2008.
standard = "2008"
# File names are either absolute or relative to the parent folder of the vhdl_ls.toml file
[libraries]
lib2.files = [
    'pkg2.vhd',
]
lib1.files = [
    'pkg1.vhd',
    'tb_ent.vhd'
]

# Wildcards are supported
lib3.files = [
    'test/*.vhd',
    'src/*.vhd',
    'src/*/*.vhd',
]

# Libraries can be marked as third-party to disable some analysis warnings, such as unused declarations
UNISIM.files = [
    'C:\Xilinx\Vivado\2023.1\data\vhdl\src\unisims\unisim_VCOMP.vhd',
]
UNISIM.is_third_party = true

[lint]
unused = 'error' # Upgrade the 'unused' diagnostic to the 'error' severity
unnecessary_work_library = false # Disable linting for the 'library work;' statement
```

Using the `lint` table, you can configure the severity of diagnostics or turn of diagnostics altogether.

> [!WARNING]
> You can overwrite every diagnostic error code including syntax or analysis errors using the lint table.
> However, the intended use-case is for lints only.
> Overwriting syntax or analysis errors (e.g., error codes `mismatched_kinds` or `syntax`) can cause unwanted side
> effects

Paths in the `vhdl_ls.toml` can contain glob patterns (i.e., `.../*/`).
On Unix machines, they can contain environment variables using the `$NAME` or `${NAME}` syntax.
On Windows machines, use the `%NAME%` syntax to substitute environment variables.

## Ignoring errors

You can use the comment-pair `-- vhdl_ls off` and `-- vhdl_ls on` to conditionally disable and re-enable parsing of
source code. This can be helpful to ignore errors from correct code that vhdl_ls does not yet support, i.e., PSL
statements or certain VHDL-2019 constructs.

```vhdl
library ieee;
    use ieee.std_logic_1164.all;

entity ent is
    port (
       clk : in std_logic
    );
end entity;

architecture arch of ent is
begin
    -- vhdl_ls off
    default clock is rising_edge(clk);
    -- vhdl_ls on
end architecture;
```

## As an LSP-client developer how should I integrate VHDL-LS?

I recommend that the `lsp-client` polls GitHub and downloads
the [latest](https://github.com/VHDL-LS/rust_hdl/releases/latest) VHDL-LS release from GitHub.

VHDL-LS has frequent releases and the automatic update ensures minimal maintenance for the `lsp-client` developer as
well as ensuring the users are not running and outdated version.

## VHDL Language Frontend

[![vhdl language frontend crate](https://img.shields.io/crates/v/vhdl_lang.svg)](https://crates.io/crates/vhdl_lang)

### Goals

- This project aims to provide a fully featured open source VHDL frontend that is easy to integrate into other tools.
- A design goal of the frontend is to be able to recover from syntax errors such that it is useful for building a
  language server.
- Analysis order must be automatically computed such that the user does not have to maintain a compile order.
- Comments will be part of the AST to support document generation.
- Separate parsing from semantic analysis to allow code formatting on non-semantically correct code.

## Building the project locally

1) Make sure that you have the [Rust toolchain](https://www.rust-lang.org/tools/install) installed.
   This repository always follows the latest toolchain in the `stable` channel.
2) Run `cargo install --path vhdl_lang` to install the language frontend. Run instead `cargo install --path vhdl_ls`
   to install the language server.
3) Make sure that the default libraries are available at a visible path. Search paths are, for example,
   `/usr/lib/rust_hdl/vhdl_libraries` or `/usr/local/lib/rust_hdl/vhdl_libraries`.
4) Run the command `vhdl_lang` or `vhdl_ls` to run the language front-end binary or the language server

**Testing the Language Server**

For checking the language server, [rust_hdl_vscode](https://github.com/VHDL-LS/rust_hdl_vscode) is recommended.
To instruct the extension to use the new binary, instead of a downloaded one, go to the extension settings and set
the Language server location to `systemPath`. To specify the exact path, set it to `user` and set Language Server User
Path to the path that points to the `vhdl_ls` binary.
