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
- `-c --comment-encoding` - Define how comments are encoded, see [Comments](#Comments). Default is utf-8.

### Example

```bash
vhdl-dump-ast design.vhd --format json --trivia --loc
```

## Comments

Comments (line comments starting with `--` or block comments enclosed in `/*...*/`) can have arbitrary encoding in VHDL.
The serialized AST, therefore, includes those comments as byte-array with an additional `encoding` field attached that informs consumers on how to interpret the data.

## Encoding

Serialized data is UTF-8. All textual data originates from [ISO-8859-1](https://de.wikipedia.org/wiki/ISO_8859-1) (Latin-1) and is decoded losslessly. For byte-to byte compatibility, the text must be re-encoded into Latin-1 to be a valid VHDL file.

## Contributing

Found an issue or have a feature request? Please open an issue on the [GitHub repository](TODO: link to issues).

Want to discuss the serialization format or request additional output formats? Start a [discussion](TODO: link to discussions).

## License

See the root repository for license information.
