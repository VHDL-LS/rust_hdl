# Machine-Readable Output

`vhdl-lint` can output a custom machine-readable JSON format.
In the future it will also support different formats for better integration with tools like CI.
Print the format to stdout using `--output-format json` in the CLI.

## Custom format

The custom format prints large portions of the internal diagnostic object.
Like the entire crate, its exact format is unstable and bound to change.
Therefore, this section only provides several design decisions of note:

### Position encoding

Reported line and column positions are 1-based offsets into the source file.
Columns are reported as UTF-32 characters.
The `end` position points past the last character.
 