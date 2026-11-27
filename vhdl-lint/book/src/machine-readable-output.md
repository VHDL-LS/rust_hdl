# Machine-Readable Output

`vhdl-lint` can output its findings in a machine-readable format.
Print the format to stdout using `--output-format` in the CLI.

## SARIF

`--output-format sarif` writes [SARIF 2.1.0](https://sarifweb.azurewebsites.net/),
the interchange format for static analysis results. GitHub code scanning and
other CI services can use it to report findings.

```shell
vhdl-lint --output-format sarif rtl/ > vhdl-lint.sarif
```

In GitHub Actions, upload the file with `github/codeql-action/upload-sarif`.

## Custom format

`--output-format json` prints large portions of the internal diagnostic object.
Like the entire crate, its exact format is unstable and bound to change.
Therefore, this section only provides several design decisions of note:

### Position encoding

Reported line and column positions are 1-based offsets into the source file.
Columns are reported as UTF-32 characters.
The `end` position points past the last character.
