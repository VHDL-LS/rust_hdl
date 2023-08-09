use std::default::Default;
use crate::analysis::DesignRoot;
use crate::ast::{AnyDesignUnit, AnyPrimaryUnit, UnitKey};
use crate::data::{ContentReader, Symbol};
use crate::syntax::Kind::*;
use crate::syntax::{Symbols, Token, Tokenizer, Value};
use crate::{Position, Source};
use itertools::Itertools;

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

fn tokenize_input(symbols: &Symbols, source: &Source, cursor: Position) -> Vec<Token> {
    let contents = source.contents();
    // let symbols = Symbols::default();
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
        let Some(lib) = self.get_library_units(lib) else { return vec![]; };
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
        let Some(lib) = self.get_library_units(lib) else { return vec![]; };
        let Some(unit) = lib.get(&UnitKey::Primary(primary_unit.clone())) else { return vec![]; };
        let unit = unit.unit.get();
        match unit.unwrap().to_owned() {
            AnyDesignUnit::Primary(AnyPrimaryUnit::Package(pkg)) => pkg
                .decl
                .iter()
                .filter_map(|d| d.ident())
                .unique()
                .chain(vec!["all".to_string()].into_iter())
                .collect_vec(),
            _ => Vec::default(),
        }
    }

    pub fn list_completion_options(&self, source: &Source, cursor: Position) -> Vec<String> {
        let tokens = tokenize_input(&self.symbols, source, cursor);
        let cursor_at_end = tokens.last().map(|tok| tok.pos.end() == cursor).unwrap_or(false);

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
            },
            [.., kind!(Entity), kind!(Identifier)] => {
                if cursor_at_end {
                    vec![]
                } else {
                    vec!["is".to_string()]
                }
            },
            [.., kind!(Architecture), kind!(Identifier)] => {
                if cursor_at_end {
                    vec![]
                } else {
                    vec!["of".to_string()]
                }
            },
            _ => vec![],
        }
    }
}

#[cfg(test)]
mod test {
    use assert_matches::assert_matches;
    use super::*;
    use crate::analysis::completion::tokenize_input;
    use crate::syntax::test::Code;

    #[test]
    fn tokenizing_an_empty_input() {
        let input = Code::new("");
        let tokens = tokenize_input(&input.symbols, input.source(), Position::new(0, 0));
        assert_eq!(tokens.len(), 0);
    }

    #[test]
    fn tokenizing_stops_at_the_cursors_position() {
        let input = Code::new("entity my_ent is");
        // mid of `my_ent`
        let mut cursor = Position::new(0, 11);
        let tokens = tokenize_input(&input.symbols, input.source(), cursor);
        assert_matches!(tokens[..], [kind!(Entity), kind!(Identifier)]);
        // end of `my_ent`
        cursor = Position::new(0, 13);
        let tokens = tokenize_input(&input.symbols, input.source(), cursor);
        assert_matches!(tokens[..], [kind!(Entity), kind!(Identifier)]);
        // start of `is`
        cursor = Position::new(0, 14);
        let tokens = tokenize_input(&input.symbols, input.source(), cursor);
        assert_matches!(tokens[..], [kind!(Entity), kind!(Identifier)]);
        // inside of `is`
        cursor = Position::new(0, 15);
        let tokens = tokenize_input(&input.symbols, input.source(), cursor);
        assert_matches!(tokens[..], [kind!(Entity), kind!(Identifier), kind!(Is)]);
    }
}
