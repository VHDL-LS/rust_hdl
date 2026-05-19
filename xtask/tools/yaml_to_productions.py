"""
Generate a `productions.txt`-style EBNF file from the YAML syntax definitions
in `xtask/src/syntax_definitions/`.

Usage:
    python yaml_to_productions.py [-o OUTPUT] [--no-sort]

By default, productions are sorted alphabetically by name (matching the layout
of `xtask/doc/productions.txt`). Pass `--no-sort` to keep the per-file source
order with section comments instead.
"""
from __future__ import annotations

import argparse
import re
from pathlib import Path

import yaml


# Mapping from YAML token kinds to the text emitted in `productions.txt`.
# Punctuation tokens are quoted with their canonical text; meta tokens
# (e.g. identifiers, literals) are emitted as bare snake_case names.
TOKEN_TEXT: dict[str, str] = {
    "Plus": '"+"',
    "Minus": '"-"',
    "EQ": '"="',
    "NE": '"/="',
    "LT": '"<"',
    "LTE": '"<="',
    "GT": '">"',
    "GTE": '">="',
    "QueEQ": '"?="',
    "QueNE": '"?/="',
    "QueLT": '"?<"',
    "QueLTE": '"?<="',
    "QueGT": '"?>"',
    "QueGTE": '"?>="',
    "Que": '"?"',
    "QueQue": '"??"',
    "Times": '"*"',
    "Pow": '"**"',
    "Div": '"/"',
    "Tick": "\"'\"",
    "LeftPar": '"("',
    "RightPar": '")"',
    "LeftSquare": '"["',
    "RightSquare": '"]"',
    "SemiColon": '";"',
    "Colon": '":"',
    "Bar": '"|"',
    "Dot": '"."',
    "BOX": '"<>"',
    "LtLt": '"<<"',
    "GtGt": '">>"',
    "Circ": '"^"',
    "CommAt": '"@"',
    "Concat": '"&"',
    "Comma": '","',
    "ColonEq": '":="',
    "RightArrow": '"=>"',
    "Identifier": "identifier",
    "AbstractLiteral": "abstract_literal",
    "StringLiteral": "string_literal",
    "BitStringLiteral": "bit_string_literal",
    "CharacterLiteral": "character_literal",
    "ToolDirective": "tool_directive",
    "Unterminated": "unterminated",
    "Unknown": "unknown",
    "Eof": "eof",
}


class SeqTag(list):
    pass


class ChoiceTag(list):
    pass


def _build_loader() -> type[yaml.SafeLoader]:
    class Loader(yaml.SafeLoader):
        pass

    def seq_ctor(loader: yaml.SafeLoader, node: yaml.Node) -> SeqTag:
        return SeqTag(loader.construct_sequence(node, deep=True))

    def choice_ctor(loader: yaml.SafeLoader, node: yaml.Node) -> ChoiceTag:
        return ChoiceTag(loader.construct_sequence(node, deep=True))

    Loader.add_constructor("!Sequence", seq_ctor)
    Loader.add_constructor("!Choice", choice_ctor)
    return Loader


_LOADER = _build_loader()


def to_snake(name: str) -> str:
    s = re.sub(r"([A-Z]+)([A-Z][a-z])", r"\1_\2", name)
    s = re.sub(r"([a-z0-9])([A-Z])", r"\1_\2", s)
    return s.lower()


_BOOL_KEYWORD = {True: "on", False: "off"}


def _yaml_str(v: object) -> str:
    """Coerce a YAML scalar to its source-level identifier text.

    PyYAML maps YAML 1.1 booleans like `on`, `off`, `yes`, `no` to Python
    bool, so values written as bare keywords come back as `True`/`False`.
    """
    if isinstance(v, bool):
        return _BOOL_KEYWORD[v]
    return str(v)


def render_atom(item: dict) -> str:
    """Render the bare text for a single sequence/choice item, without
    optional/repeated wrappers, but including parenthesized/terminated."""
    if "node" in item:
        text = to_snake(_yaml_str(item["node"]))
        if item.get("parenthesized"):
            text = f'"(" {text} ")"'
        if item.get("terminated"):
            term_kind = _yaml_str(item["terminated"])
            term = TOKEN_TEXT.get(term_kind, term_kind)
            text = f"{text} {term}"
        return text
    if "token" in item:
        kind = _yaml_str(item["token"])
        return TOKEN_TEXT.get(kind, kind)
    if "keyword" in item:
        return '"' + _yaml_str(item["keyword"]).lower() + '"'
    return "<?>"


def render_seq(items: list[dict]) -> str:
    parts: list[str] = []
    i = 0
    while i < len(items):
        item = items[i]
        repeated = bool(item.get("repeated"))
        optional = bool(item.get("optional"))
        text = render_atom(item)

        # Detect separator-list pattern: a repeated `node` immediately
        # followed by a repeated `token` (typically Comma or Bar) means
        # `node { sep node }`.
        if (
            repeated
            and "node" in item
            and i + 1 < len(items)
            and "token" in items[i + 1]
            and items[i + 1].get("repeated")
        ):
            sep = render_atom(items[i + 1])
            inner = f"{text} {{ {sep} {text} }}"
            parts.append(f"[ {inner} ]" if optional else inner)
            i += 2
            continue

        if repeated:
            text = f"{{ {text} }}"
        elif optional:
            text = f"[ {text} ]"
        parts.append(text)
        i += 1
    return " ".join(parts) if parts else '""'


def render_choice(items: list[dict]) -> str:
    parts: list[str] = []
    for item in items:
        text = render_atom(item)
        if item.get("repeated"):
            text = f"{{ {text} }}"
        if item.get("optional"):
            text = f"[ {text} ]"
        parts.append(text)
    return " | ".join(parts)


def render_node(name: str, contents: object) -> str:
    head = f"{to_snake(name)} ::="
    if isinstance(contents, SeqTag):
        body = render_seq(list(contents))
    elif isinstance(contents, ChoiceTag):
        body = render_choice(list(contents))
    else:
        body = "<?>"
    return f"{head} {body}"


def main() -> None:
    workspace_root = Path(__file__).resolve().parent.parent
    default_out = workspace_root / "doc" / "productions.txt"

    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "-o",
        "--output",
        type=Path,
        default=default_out,
        help=f"output path (default: {default_out})",
    )
    parser.add_argument(
        "--no-sort",
        dest="sort",
        action="store_false",
        help="keep per-file source order with section comments "
        "(default: sort alphabetically)",
    )
    parser.set_defaults(sort=True)
    args = parser.parse_args()

    defs_dir = workspace_root / "src" / "syntax_definitions"
    lines: list[str] = []

    if args.sort:
        productions: list[tuple[str, str]] = []
        for yaml_path in sorted(defs_dir.glob("*.yaml")):
            with open(yaml_path) as f:
                nodes = yaml.load(f, Loader=_LOADER)
            if not nodes:
                continue
            for node_name, node_contents in nodes.items():
                productions.append((to_snake(node_name), render_node(node_name, node_contents)))
        productions.sort(key=lambda p: p[0])
        for _, rendered in productions:
            lines.append(rendered)
            lines.append("")
    else:
        for yaml_path in sorted(defs_dir.glob("*.yaml")):
            lines.append(f"// {yaml_path.stem}")
            lines.append("")
            with open(yaml_path) as f:
                nodes = yaml.load(f, Loader=_LOADER)
            if not nodes:
                continue
            for node_name, node_contents in nodes.items():
                lines.append(render_node(node_name, node_contents))
                lines.append("")

    args.output.write_text("\n".join(lines))
    print(f"Wrote {args.output}")


if __name__ == "__main__":
    main()
