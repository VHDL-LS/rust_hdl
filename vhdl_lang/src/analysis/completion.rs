use crate::analysis::DesignRoot;
use crate::ast::{AnyDesignUnit, AnyPrimaryUnit, Declaration, UnitKey};
use crate::data::{ContentReader, Symbol};
use crate::syntax::Kind::*;
use crate::syntax::{Symbols, Token, Tokenizer, Value};
use crate::{Position, Source};
use itertools::Itertools;
use std::default::Default;

macro_rules! kind {
    ($kind: pat) => {
        Token { kind: $kind, .. }
    };
}

macro_rules! ident {
    ($bind: pat) => {
        Token {
            kind: Identifier,
            value: Value::Identifier($bind),
            ..
        }
    };
}

/// Returns the completable string representation of a declaration
/// for example:
/// `let alias = parse_vhdl("alias my_alias is ...")`
/// `declaration_to_string(Declaration::Alias(alias)) == "my_alias"`
/// Returns `None` if the declaration has no string representation that can be used for completion
/// purposes.
fn declaration_to_string(decl: &Declaration) -> Option<String> {
    match decl {
        Declaration::Object(o) => Some(o.ident.tree.item.to_string()),
        Declaration::File(f) => Some(f.ident.tree.item.to_string()),
        Declaration::Type(t) => Some(t.ident.tree.item.to_string()),
        Declaration::Component(c) => Some(c.ident.tree.item.to_string()),
        Declaration::Attribute(a) => match a {
            crate::ast::Attribute::Specification(spec) => Some(spec.ident.item.to_string()),
            crate::ast::Attribute::Declaration(decl) => Some(decl.ident.tree.item.to_string()),
        },
        Declaration::Alias(a) => Some(a.designator.to_string()),
        Declaration::SubprogramDeclaration(decl) => Some(decl.subpgm_designator().to_string()),
        Declaration::SubprogramBody(_) => None,
        Declaration::Use(_) => None,
        Declaration::Package(p) => Some(p.ident.to_string()),
        Declaration::Configuration(_) => None,
    }
}

/// Tokenizes `source` up to `cursor` but no further. The last token returned is the token
/// where the cursor currently resides or the token right before the cursor.
///
/// Examples:
///
/// input = "use ieee.std_logic_1|164.a"
///                              ^ cursor position
/// `tokenize_input(input)` -> {USE, ieee, DOT, std_logic_1164}
///
/// input = "use ieee.std_logic_1164|.a"
///                                 ^ cursor position
/// `tokenize_input(input)` -> {USE, ieee, DOT, std_logic_1164}
///
/// input = "use ieee.std_logic_1164.|a"
///                                  ^ cursor position
/// `tokenize_input(input)` -> {USE, ieee, DOT, std_logic_1164, DOT}
/// input = "use ieee.std_logic_1164.a|"
///                                   ^ cursor position
/// `tokenize_input(input)` -> {USE, ieee, DOT, std_logic_1164, DOT, a}
///
/// On error, or if the source is empty, returns an empty vector.
fn tokenize_input(symbols: &Symbols, source: &Source, cursor: Position) -> Vec<Token> {
    let contents = source.contents();
    let mut tokenizer = Tokenizer::new(symbols, source, ContentReader::new(&contents));
    let mut tokens = Vec::new();
    loop {
        match tokenizer.pop() {
            Ok(Some(token)) => {
                if token.pos.start() >= cursor {
                    break;
                }
                tokens.push(token);
            }
            Ok(None) => break,
            Err(_) => return vec![],
        }
    }
    tokens
}

impl DesignRoot {
    /// helper function to list the name of all available libraries
    fn list_all_libraries(&self) -> Vec<String> {
        self.available_libraries()
            .map(|k| k.name().to_string())
            .collect()
    }

    /// List the name of all primary units for a given library.
    /// If the library is non-resolvable, list an empty vector
    fn list_primaries_for_lib(&self, lib: &Symbol) -> Vec<String> {
        let Some(lib) = self.get_library_units(lib) else {
            return vec![];
        };
        lib.keys()
            .filter_map(|key| match key {
                UnitKey::Primary(prim) => Some(prim.name().to_string()),
                UnitKey::Secondary(_, _) => None,
            })
            .collect()
    }

    /// Lists all available declarations for a primary unit inside a given library
    /// If the library does not exist or there is no primary unit with the given name for that library,
    /// return an empty vector
    fn list_available_declarations(&self, lib: &Symbol, primary_unit: &Symbol) -> Vec<String> {
        let Some(lib) = self.get_library_units(lib) else {
            return vec![];
        };
        let Some(unit) = lib.get(&UnitKey::Primary(primary_unit.clone())) else {
            return vec![];
        };
        let unit = unit.unit.get();
        match unit.unwrap().to_owned() {
            AnyDesignUnit::Primary(AnyPrimaryUnit::Package(pkg)) => pkg
                .decl
                .iter()
                .filter_map(declaration_to_string)
                .unique()
                .chain(vec!["all".to_string()])
                .collect_vec(),
            _ => Vec::default(),
        }
    }

    pub fn list_completion_options(&self, source: &Source, cursor: Position) -> Vec<String> {
        let tokens = tokenize_input(&self.symbols, source, cursor);
        match &tokens[..] {
            [.., kind!(Library)] | [.., kind!(Use)] | [.., kind!(Use), kind!(Identifier)] => {
                self.list_all_libraries()
            }
            [.., kind!(Use), ident!(library), kind!(Dot)]
            | [.., kind!(Use), ident!(library), kind!(Dot), kind!(Identifier)] => {
                self.list_primaries_for_lib(library)
            }
            [.., kind!(Use), ident!(library), kind!(Dot), ident!(selected), kind!(Dot)]
            | [.., kind!(Use), ident!(library), kind!(Dot), ident!(selected), kind!(Dot), kind!(StringLiteral | Identifier)] => {
                self.list_available_declarations(library, selected)
            }
            _ => vec![],
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::analysis::completion::tokenize_input;
    use crate::analysis::tests::LibraryBuilder;
    use crate::syntax::test::Code;
    use assert_matches::assert_matches;

    #[test]
    fn tokenizing_an_empty_input() {
        let input = Code::new("");
        let tokens = tokenize_input(&input.symbols, input.source(), Position::new(0, 0));
        assert_eq!(tokens.len(), 0);
    }

    #[test]
    fn tokenizing_stops_at_the_cursors_position() {
        let input = Code::new("use ieee.std_logic_1164.all");
        let mut cursor = input.s1("std_logic_11").pos().end();
        let tokens = tokenize_input(&input.symbols, input.source(), cursor);
        assert_matches!(
            tokens[..],
            [kind!(Use), kind!(Identifier), kind!(Dot), kind!(Identifier)]
        );
        cursor = input.s1("std_logic_1164").pos().end();
        let tokens = tokenize_input(&input.symbols, input.source(), cursor);
        assert_matches!(
            tokens[..],
            [kind!(Use), kind!(Identifier), kind!(Dot), kind!(Identifier)]
        );
        cursor = input.s1("std_logic_1164.").pos().end();
        let tokens = tokenize_input(&input.symbols, input.source(), cursor);
        assert_matches!(
            tokens[..],
            [
                kind!(Use),
                kind!(Identifier),
                kind!(Dot),
                kind!(Identifier),
                kind!(Dot)
            ]
        );
        cursor = input.s1("std_logic_1164.all").pos().end();
        let tokens = tokenize_input(&input.symbols, input.source(), cursor);
        assert_matches!(
            tokens[..],
            [
                kind!(Use),
                kind!(Identifier),
                kind!(Dot),
                kind!(Identifier),
                kind!(Dot),
                kind!(All)
            ]
        );
    }

    #[test]
    pub fn completing_libraries() {
        let input = LibraryBuilder::new();
        let code = Code::new("library ");
        let (root, _) = input.get_analyzed_root();
        let cursor = code.s1("library ").pos().end();
        let options = root.list_completion_options(code.source(), cursor);
        assert_eq!(options, root.list_all_libraries())
    }

    #[test]
    pub fn completing_primaries() {
        let (root, _) = LibraryBuilder::new().get_analyzed_root();
        let code = Code::new("use std.");
        let cursor = code.pos().end();
        let options = root.list_completion_options(code.source(), cursor);
        assert_eq!(options, vec!["textio", "standard", "env"]);

        let code = Code::new("use std.t");
        let cursor = code.pos().end();
        let options = root.list_completion_options(code.source(), cursor);
        // Note that the filtering only happens at client side
        assert_eq!(options, vec!["textio", "standard", "env"]);
    }

    #[test]
    pub fn completing_declarations() {
        let input = LibraryBuilder::new();
        let code = Code::new("use std.env.");
        let (root, _) = input.get_analyzed_root();
        let cursor = code.pos().end();
        let options = root.list_completion_options(code.source(), cursor);
        assert_eq!(options, vec!["stop", "finish", "resolution_limit", "all"])
    }
}
