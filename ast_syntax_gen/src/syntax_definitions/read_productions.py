from __future__ import annotations

from abc import ABC, abstractmethod
from enum import Enum
from typing import Callable, Generator, Iterable
from dataclasses import dataclass


class TokenKind(Enum):
    IDENT = 0
    STRING = 1
    COLON_COLON_EQ = 2  # ::=
    BAR = 3  # |
    LEFT_BRAK = 4  # [
    RIGHT_BRAK = 5  # ]
    LEFT_BRACE = 4  # {
    RIGHT_BRACE = 5  # }
    SEMICOLON = 6  # ;


RECURSE_ITERATION = None
STOP_ITERATION = object()


@dataclass
class Token:
    kind: TokenKind
    value: str


class Tokenizer:
    def __init__(self, text: str):
        self.text = text
        self.cursor = 0

    def peek(self, n: int = 0) -> str:
        return self.text[self.cursor + n]

    def consume(self) -> str:
        ch = self.peek()
        self.skip()
        return ch

    def skip(self, n: int = 1):
        self.cursor += n

    def _skip_ws(self) -> None:
        try:
            while self.peek().isspace():
                self.consume()
        except IndexError:
            pass

    def _skip_comment(self) -> None:
        try:
            if self.peek(0) == "/" and self.peek(1) == "/":
                self.skip(2)
                while self.consume() != "\n":
                    pass
        except IndexError:
            pass

    def _read_ident(self) -> str:
        buf = ""
        try:
            while self.peek().isalpha() or self.peek() == "_":
                buf += self.consume()
        except IndexError:
            pass

        assert buf, "empty after reading ident"
        return buf

    def _read_string(self) -> str:
        buf = ""
        assert self.consume() == '"'
        while True:
            el = self.consume()
            if el == '"':
                break
            if el == "\\":
                el = self.consume()
            buf += el
        return buf

    def __iter__(self):
        while True:
            try:
                ch = self.peek()
                if ch.isalpha():
                    yield Token(TokenKind.IDENT, self._read_ident())
                elif ch == "|":
                    self.consume()
                    yield Token(TokenKind.BAR, "|")
                elif ch == ":":
                    self.consume()
                    assert self.consume() == ":"
                    assert self.consume() == "="
                    yield Token(TokenKind.COLON_COLON_EQ, "::=")
                elif ch == "[":
                    self.consume()
                    yield Token(TokenKind.LEFT_BRAK, "[")
                elif ch == "]":
                    self.consume()
                    yield Token(TokenKind.RIGHT_BRAK, "]")
                elif ch == "{":
                    self.consume()
                    yield Token(TokenKind.LEFT_BRACE, "{")
                elif ch == "}":
                    self.consume()
                    yield Token(TokenKind.RIGHT_BRACE, "}")
                elif ch == ";":
                    self.consume()
                    yield Token(TokenKind.SEMICOLON, ";")
                elif ch == '"':
                    yield Token(TokenKind.STRING, self._read_string())
                elif ch.isspace():
                    self._skip_ws()
                elif ch == "/":
                    self._skip_comment()
                else:
                    raise ValueError(f"Unknown char {ch}")
            except IndexError:
                break


class Production(ABC):
    @abstractmethod
    def visit(self) -> Generator[Production]:
        pass

    @abstractmethod
    def rewrite(self, action: Callable[[Production], Production | None]) -> Production:
        pass


@dataclass
class Concatenation(Production):
    elements: list[Production]

    def __str__(self) -> str:
        return " ".join(map(str, self.elements))

    def visit(self) -> Generator[Production]:
        action = yield self
        if action is STOP_ITERATION:
            return
        for child in self.elements:
            yield from child.visit()

    def rewrite(self, action: Callable[[Production], Production | None]) -> Production:
        if (new_concat := action(self)) is not None:
            return new_concat
        return Concatenation([el.rewrite(action) for el in self.elements])


@dataclass
class Alternative(Production):
    elements: list[Production]

    def __str__(self) -> str:
        return " | ".join(map(str, self.elements))

    def visit(self) -> Generator[Production]:
        action = yield self
        if action is STOP_ITERATION:
            return
        for child in self.elements:
            yield from child.visit()

    def rewrite(self, action: Callable[[Production], Production | None]) -> Production:
        if (new_alternative := action(self)) is not None:
            return new_alternative
        return Alternative([el.rewrite(action) for el in self.elements])


@dataclass
class Repetition(Production):
    value: Production

    def __str__(self) -> str:
        return f"{{ {self.value} }}"

    def visit(self) -> Generator[Production]:
        action = yield self
        if action is STOP_ITERATION:
            return
        yield from self.value.visit()

    def rewrite(self, action: Callable[[Production], Production | None]) -> Production:
        if (new_repetition := action(self)) is not None:
            return new_repetition
        return Repetition(self.value.rewrite(action))


@dataclass
class Optional(Production):
    value: Production

    def __str__(self) -> str:
        return f"[ {self.value} ]"

    def visit(self) -> Generator[Production]:
        action = yield self
        if action is STOP_ITERATION:
            return
        yield from self.value.visit()

    def rewrite(self, action: Callable[[Production], Production | None]) -> Production:
        if (new_optional := action(self)) is not None:
            return new_optional
        return Optional(self.value.rewrite(action))


@dataclass
class Terminal(Production):
    value: str

    def __str__(self) -> str:
        return '"' + self.value.translate(str.maketrans({'"': '"', "\\": "\\\\"})) + '"'

    def visit(self) -> Generator[Production]:
        yield self

    def rewrite(self, action: Callable[[Production], Production | None]) -> Production:
        if (new_terminal := action(self)) is not None:
            return new_terminal
        else:
            return self


@dataclass
class NonTerminal(Production):
    value: str

    def __str__(self) -> str:
        return self.value

    def visit(self) -> Generator[Production]:
        yield self

    def rewrite(self, action: Callable[[Production], Production | None]) -> Production:
        if (new_self := action(self)) is not None:
            return new_self
        else:
            return self


@dataclass
class ResolvedNonTerminal(Production):
    value: Rule

    @property
    def name(self) -> str:
        return self.value.name

    @property
    def production(self) -> Production:
        return self.value.production

    def __str__(self) -> str:
        return self.name

    def visit(self) -> Generator[Production, None, None]:
        action = yield self
        if action is STOP_ITERATION:
            return
        yield from self.production.visit()

    def rewrite(self, action: Callable[[Production], Production | None]) -> Production:
        if (new_self := action(self)) is not None:
            return new_self
        else:
            return self


@dataclass
class Rule:
    name: str
    production: Production

    def __str__(self) -> str:
        return f"{self.name} ::= {self.production}"


def insert_semicolons(tokens: list[Token]):
    while True:
        insert_idx = None
        for i, token in enumerate(tokens):
            if (
                token.kind == TokenKind.COLON_COLON_EQ
                and i - 2 > 0
                and tokens[i - 2].kind != TokenKind.SEMICOLON
            ):
                insert_idx = i - 1
                break
        if insert_idx is not None:
            tokens.insert(insert_idx, Token(TokenKind.SEMICOLON, ";"))
        else:
            break
    if len(tokens) > 0 and tokens[-1].kind != TokenKind.SEMICOLON:
        tokens.append(Token(TokenKind.SEMICOLON, ";"))


class Parser:
    def __init__(self, tokens: Iterable[Token]) -> None:
        self.tokens = list(iter(tokens))
        self.cursor = 0

    def peek(self, n: int = 0) -> Token | None:
        if self.cursor + n >= len(self.tokens):
            return None
        return self.tokens[self.cursor + n]

    def consume(self) -> Token | None:
        tok = self.peek()
        self.skip()
        return tok

    def skip(self, n: int = 1):
        self.cursor += n

    def _expect(self, kind: TokenKind) -> Token:
        el = self.consume()
        assert el is not None
        assert el.kind == kind
        return el

    def production(self) -> Production:
        return self.alternative()

    def optional(self) -> Optional:
        self._expect(TokenKind.LEFT_BRAK)
        inner = self.production()
        self._expect(TokenKind.RIGHT_BRAK)
        return Optional(inner)

    def repetition(self) -> Repetition:
        self._expect(TokenKind.LEFT_BRACE)
        inner = self.production()
        self._expect(TokenKind.RIGHT_BRACE)
        return Repetition(inner)

    def alternative(self) -> Production:
        elements = [self.concatenation()]
        while (peeked := self.peek()) is not None and peeked.kind == TokenKind.BAR:
            self.skip()
            elements.append(self.concatenation())
        assert elements
        if len(elements) == 1:
            return elements[0]
        return Alternative(elements)

    def concatenation(self) -> Production:
        elements: list[Production] = []
        while (primary := self._primary()) is not None:
            elements.append(primary)
        assert elements
        if len(elements) == 1:
            return elements[0]
        return Concatenation(elements)

    def _primary(self) -> Production | None:
        el = self.peek()
        assert el is not None
        if el.kind == TokenKind.STRING:
            self.skip()
            return Terminal(el.value)
        elif el.kind == TokenKind.IDENT:
            self.skip()
            return NonTerminal(el.value)
        elif el.kind == TokenKind.LEFT_BRAK:
            return self.optional()
        elif el.kind == TokenKind.LEFT_BRACE:
            return self.repetition()
        else:
            return None

    def rule(self) -> Rule:
        name = self._expect(TokenKind.IDENT).value
        self._expect(TokenKind.COLON_COLON_EQ)
        prod = self.production()
        self._expect(TokenKind.SEMICOLON)
        return Rule(name, prod)

    def rules(self) -> Generator[Rule]:
        while self.peek() is not None:
            yield self.rule()


def rewrite_rules(
    rules: dict[str, Rule], action: Callable[[Production], Production | None]
) -> dict[str, Rule]:
    new_rules: dict[str, Rule] = {}
    for key, rule in rules.items():
        new_production = rule.production.rewrite(action)
        assert isinstance(new_production, Production)
        new_rules[key] = Rule(key, new_production)

    return new_rules


def resolve_non_terminals(rules: dict[str, Rule]) -> dict[str, Rule]:
    def rewrite_fn(production: Production) -> Production | None:
        if isinstance(production, NonTerminal):
            return ResolvedNonTerminal(rules[production.value])
        else:
            return None

    return rewrite_rules(rules, rewrite_fn)


def resolve_well_known_names(rules: dict[str, Rule]) -> dict[str, Rule]:
    mapping = {
        "signal_name": "name",
        "variable_name": "name",
        "file_name": "name",
        "subprogram_name": "name",
        "entity_name": "name",
        "function_name": "name",
        "instantiated_package_name": "name",
        "architecture_name": "name",
        "component_name": "name",
        "configuration_name": "name",
        "entity_simple_name": "identifier",
        "architecture_simple_name": "identifier",
        "attribute_simple_name": "identifier",
        "parameter_association_list": "association_list",
        "shared_variable_declaration": "variable_declaration",
        "block_statement_label": "identifier",
        "generate_statement_label": "identifier",
        "block_label": "identifier",
        "alternative_label": "identifier",
        "generate_label": "identifier",
        "case_label": "identifier",
        "instantiation_label": "identifier",
        "element_simple_name": "identifier",
        "local_generic_clause": "generic_clause",
        "formal_generic_clause": "generic_clause",
        "local_port_clause": "port_clause",
        "formal_port_clause": "port_clause",
        "component_simple_name": "identifier",
        "configuration_simple_name": "identifier",
        "element_subtype_indication": "subtype_indication",
        "context_simple_name": "identifier",
        "time_expression": "expression",
        "discrete_subtype_indication": "subtype_indication",
        "architecture_identifier": "identifier",
        "guard_condition": "condition",
        "passive_concurrent_procedure_call_statement": "concurrent_procedure_call_statement",
        "passive_process_statement": "process_statement",
        "loop_label": "identifier",
        "string_expression": "expression",
        "file_open_kind_expression": "expression",
        "generate_parameter_specification": "parameter_specification",
        "generic_name": "name",
        "port_name": "name",
        "parameter_name": "name",
        "parameter_interface_list": "interface_list",
        "static_discrete_range": "discrete_range",
        "static_expression": "expression",
        "generic_interface_list": "interface_list",
        "generic_association_list": "association_list",
        "group_template_name": "name",
        "guarded_signal_list": "signal_list",
        "if_label": "identifier",
        "uninstantiated_package_name": "name",
        "loop_parameter_specification": "parameter_specification",
        "package_simple_name": "identifier",
        "library_logical_name": "identifier",
        "object_simple_name": "identifier",
        "component_instantiation_label": "identifier",
        "unit_name": "name",
        "physical_type_simple_name": "identifier",
        "port_interface_list": "interface_list",
        "port_association_list": "association_list",
        "procedure_name": "name",
        "process_label": "identifier",
        "protected_type_simple_name": "identifier",
        "range_attribute_name": "name",
        "record_element_simple_name": "identifier",
        "record_type_simple_name": "identifier",
        "resolution_function_name": "name",
        "uninstantiated_subprogram_name": "name",
        "type_name": "name",
        "subtype_name": "name",
        "verification_unit_name": "name",
        "value_expression": "expression",
    }
    def rewrite_fn(production: Production) -> Production | None:
        if isinstance(production, NonTerminal) and (
            new_name := mapping.get(production.value)
        ):
            return NonTerminal(new_name)
        else:
            return None

    return rewrite_rules(rules, rewrite_fn)


def insert_builtins(rules: dict[str, Rule]) -> dict[str, Rule]:
    @dataclass
    class Builtin(Production):
        name: str

        def visit(self) -> Generator[Production, None, None]:
            yield self

        def rewrite(
            self, action: Callable[[Production], Production | None]
        ) -> Production:
            if (new_self := action(self)) is not None:
                return new_self
            else:
                return self
        
        def __str__(self) -> str:
            return f"<builtin.{self.name}>"

    mapping = {
        "identifier",
        "integer",
        "abstract_literal",
        "string_literal",
        "graphic_character",
        "bit_string_literal",
        "PSL_Property_Declaration",
        "PSL_Sequence_Declaration",
        "PSL_Clock_Declaration",
        "PSL_PSL_Directive",
        "PSL_Verification_Unit",
    }

    def rewrite_fn(production: Production) -> Production | None:
        if isinstance(production, NonTerminal) and (production.value in mapping):
            return Builtin(production.value)
        else:
            return None

    return rewrite_rules(rules, rewrite_fn)


def remove_dead_productions(rules: dict[str, Rule], top_level: str) -> dict[str, Rule]:
    """Remove all productions that are not reachable from the top-level production.

    Args:
        rules: Dictionary of rules to filter
        top_level: Name of the top-level production that is exempt from removal

    Returns:
        A new dictionary containing only reachable productions
    """
    reachable: set[str] = set()

    def collect_references(production: Production) -> None:
        """Recursively collect all non-terminal references in a production."""
        for node in production.visit():
            if isinstance(node, NonTerminal):
                if node.value not in reachable:
                    reachable.add(node.value)
                    if node.value in rules:
                        collect_references(rules[node.value].production)
            elif isinstance(node, ResolvedNonTerminal):
                if node.name not in reachable:
                    reachable.add(node.name)
                    collect_references(node.production)

    # Start from top-level production
    reachable.add(top_level)
    if top_level in rules:
        collect_references(rules[top_level].production)

    # Return only reachable rules
    return {name: rule for name, rule in rules.items() if name in reachable}


if __name__ == "__main__":
    with open("productions.txt") as infile:
        text = infile.read()
        tokenizer = Tokenizer(text)
        tokens = list(tokenizer)
        insert_semicolons(tokens)
        parser = Parser(tokens)
        rules: dict[str, Rule] = {}
        for rule in parser.rules():
            rules[rule.name] = rule
        mappings = {"signal_name": "name"}
        rules = resolve_well_known_names(rules)
        rules = insert_builtins(rules)
        rules = remove_dead_productions(rules, "design_file")
        rules = resolve_non_terminals(rules)
        for rule in rules.values():
            print(rule)
