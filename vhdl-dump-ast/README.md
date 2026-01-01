# vhdl-dump-ast

A utility to dump VHDL ASTs in various output formats (JSON, YAML).

## Status

⚠️ **Early Stage**: This crate is in a very early stage of development and is **not intended for production use**. The output format and command-line interface may change at any time without further notice.

## Usage

```bash
vhdl-dump-ast <FILE> [OPTIONS]
```

### Options

- `-f, --format <FORMAT>` — Output format: `json` (default) or `yaml`
- `-p, --no-pretty` — Disable pretty printing
- `-t, --trivia` — Include trivia (whitespace, comments) in the dump
- `-l, --loc` — Include source code locations in the dump

### Example

```bash
vhdl-dump-ast design.vhd --format json --trivia --loc
```

## Contributing

Found an issue or have a feature request? Please open an issue on the [GitHub repository](TODO: link to issues).

Want to discuss the serialization format or request additional output formats? Start a [discussion](TODO: link to discussions).

## License

See the root repository for license information.
