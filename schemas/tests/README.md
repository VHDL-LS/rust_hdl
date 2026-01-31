# Schemas tests

This folder contains Schema tests for `vhdl_ls.schema.json`.

## Running the tests locally:

> [!NOTE]  
> Usage of [uv](https://docs.astral.sh/uv/) is recommended. The minimum Python version is 3.11

### Using `uv`

`uv run schemas/validate_tests.py`

### Using regular Python

1. Install the Python dependency for schema validation:

```
python3 -m pip install jsonschema
```

2. Run the validator script from the repository root:

```
python3 schemas/validate_tests.py
```


## Return codes:

- `0`: all tests behaved as expected
- `1`: There was at least one failure
- `2`: The `vhdl_ls.schema.json` file was not found
