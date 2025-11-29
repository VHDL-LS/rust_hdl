# /// script
# requires-python = ">=3.11"
# dependencies = [
#     "jsonschema",
# ]
# ///
"""
Validate JSON tests under `schemas/tests` against `schemas/vhdl_ls.schema.json`.

Usage:
  uv run schemas/validate_tests.py or
  python3 schemas/validate_tests.py
"""
import json
from pathlib import Path
import sys

import jsonschema
from jsonschema import Draft4Validator
import tomllib


ROOT = Path(__file__).parent
SCHEMA_PATH = ROOT / "vhdl_ls.schema.json"
TESTS_DIR = ROOT / "tests"


def load_toml(path):
    with open(path, "rb") as file:
        return tomllib.load(file)


def load_json(path):
    with open(path, "rb") as file:
        return json.load(file)


def main():
    if not SCHEMA_PATH.exists():
        print(f"Schema not found at {SCHEMA_PATH}")
        return 2

    schema = load_json(SCHEMA_PATH)
    validator = Draft4Validator(schema)

    failures = 0
    total = 0

    for sub in ("valid", "invalid"):
        folder = TESTS_DIR / sub
        for path in sorted(folder.iterdir()):
            total += 1
            data = load_toml(path)
            errors = list(validator.iter_errors(data))
            should_be_valid = (sub == "valid")

            if should_be_valid and errors:
                print(f"FAIL (expected valid): {path}")
                for e in errors[:5]:
                    print("  -", e.message)
                failures += 1
            elif (not should_be_valid) and (not errors):
                print(f"FAIL (expected invalid): {path} -- no validation errors found")
                failures += 1
            else:
                print(f"OK: {path}")

    print(f"\nChecked {total} files: {total - failures} OK, {failures} FAILED")
    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main())
