# Machine-Readable Output

`vhdl-lint` can output its findings in a machine-readable format.
Print the format to stdout using `--output-format` in the CLI.

## SARIF

`--output-format sarif` writes [SARIF 2.1.0](https://sarifweb.azurewebsites.net/),
the interchange format for static analysis results.
It is the format GitHub code scanning reads, so a run can annotate a pull
request without any further conversion.

```shell
vhdl-lint --output-format sarif rtl/ > vhdl-lint.sarif
```

The mapping is one to one. Every finding is a result, the rule code is the
result's `ruleId`, and a rule's fix becomes a set of text replacements.
Rules that produced no finding are not reported, and a rule's documentation
becomes its `shortDescription` and `fullDescription`.

The run's exit code is reported in `invocations[].exitCode`, and files that
could not be analyzed are reported as `toolExecutionNotifications`, so a
partial run does not read as a complete one.

## Custom format

`--output-format json` prints large portions of the internal diagnostic object.
Like the entire crate, its exact format is unstable and bound to change.
Therefore, this section only provides several design decisions of note:

### Position encoding

Reported line and column positions are 1-based offsets into the source file.
Columns are reported as UTF-32 characters.
The `end` position points past the last character.
