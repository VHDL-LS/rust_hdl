use crate::analysis::DesignRoot;
use crate::ast::search::{Finished, NotFinished, NotFound, RegionCategory, SearchState, Searcher};
use crate::ast::{AnyDesignUnit, AnyPrimaryUnit, Declaration, UnitKey};
use crate::data::{ContentReader, Symbol};
use crate::syntax::Kind::*;
use crate::syntax::{Symbols, Token, Tokenizer, Value};
use crate::{Position, Source};
use itertools::Itertools;
use std::default::Default;

struct RegionSearcher<'a> {
    region: Option<(RegionCategory, crate::Range)>,
    cursor: Position,
    source: &'a Source,
}

impl<'a> RegionSearcher<'a> {
    pub fn new(cursor: Position, source: &Source) -> RegionSearcher {
        RegionSearcher {
            region: None,
            cursor,
            source,
        }
    }

    pub fn region(&self) -> Option<RegionCategory> {
        self.region.as_ref().map(|reg| reg.0.clone())
    }
}

impl<'a> Searcher for RegionSearcher<'a> {
    fn search_region(&mut self, region: crate::Range, kind: RegionCategory) -> SearchState {
        if region.contains(self.cursor) {
            match &self.region {
                Some((_, old_region)) => {
                    // The new region is more specific than the old region
                    if region.start >= old_region.start && region.end <= old_region.end {
                        self.region = Some((kind, region))
                    }
                }
                None => self.region = Some((kind, region)),
            }
        }
        NotFinished
    }

    fn search_source(&mut self, source: &Source) -> SearchState {
        if source == self.source {
            NotFinished
        } else {
            Finished(NotFound)
        }
    }
}

#[cfg(test)]
mod region_searcher_test {
    use super::*;
    use crate::analysis::tests::LibraryBuilder;

    #[test]
    pub fn test_searcher_finds_region() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "work",
            "\
entity my_ent is
end entity;

architecture arch of my_ent is
    signal x: natural := 4;
begin

end;
        ",
        );
        let (root, _) = builder.get_analyzed_root();
        let mut searcher = RegionSearcher::new(Position::new(4, 12), code.source());
        let _ = root.search(&mut searcher);
        assert_eq!(searcher.region(), Some(RegionCategory::Declarative));
        searcher = RegionSearcher::new(Position::new(6, 0), code.source());
        assert_eq!(searcher.region(), None);
    }

    #[test]
    pub fn test_searcher_finds_specific_region() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "work",
            "\
entity my_ent is
end entity;

architecture arch of my_ent is
    procedure my_proc is
        variable x: bit := '0';
    begin
        x := '1';

        x := '1';

    end procedure;
begin

end;
        ",
        );
        let (root, _) = builder.get_analyzed_root();
        let mut searcher = RegionSearcher::new(Position::new(5, 10), code.source());
        let _ = root.search(&mut searcher);
        assert_eq!(searcher.region(), Some(RegionCategory::Declarative));
        searcher = RegionSearcher::new(Position::new(7, 12), code.source());
        let _ = root.search(&mut searcher);
        assert_eq!(
            searcher.region(),
            Some(RegionCategory::SequentialStatements)
        );
        searcher = RegionSearcher::new(Position::new(8, 0), code.source());
        let _ = root.search(&mut searcher);
        assert_eq!(
            searcher.region(),
            Some(RegionCategory::SequentialStatements)
        );
    }
}

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
                .filter_map(declaration_to_string)
                .unique()
                .chain(vec!["all".to_string()].into_iter())
                .collect_vec(),
            _ => Vec::default(),
        }
    }

    pub fn list_completion_options(&self, source: &Source, cursor: Position) -> Vec<String> {
        let tokens = tokenize_input(&self.symbols, source, cursor);
        let cursor_at_end = tokens
            .last()
            .map(|tok| tok.pos.end() == cursor)
            .unwrap_or(false);

        let mut region_searcher = RegionSearcher::new(cursor, source);
        let _ = self.search(&mut region_searcher);
        match region_searcher.region() {
            Some(RegionCategory::Declarative) => match &tokens[..] {
                [.., kind!(SemiColon | Is)] | [.., kind!(SemiColon | Is), kind!(Identifier)] => {
                    vec![
                        "procedure".to_string(),
                        "pure function".to_string(),
                        "impure function".to_string(),
                        "function".to_string(),
                        "package".to_string(),
                        "type".to_string(),
                        "subtype".to_string(),
                        "constant".to_string(),
                        "signal".to_string(),
                        "variable".to_string(),
                        "shared variable".to_string(),
                        "file".to_string(),
                        "alias".to_string(),
                        "attribute".to_string(),
                        "component".to_string(),
                        "group".to_string(),
                        "configuration".to_string(),
                        "disconnect".to_string(),
                        "use".to_string(),
                    ]
                }
                _ => vec![],
            },
            Some(RegionCategory::SequentialStatements) => {
                vec![]
            }
            _ => match &tokens[..] {
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
                [.., kind!(Entity), kind!(Identifier)] => {
                    if cursor_at_end {
                        vec![]
                    } else {
                        vec!["is".to_string()]
                    }
                }
                [.., kind!(Architecture), kind!(Identifier)] => {
                    if cursor_at_end {
                        vec![]
                    } else {
                        vec!["of".to_string()]
                    }
                }
                _ => vec![],
            },
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::analysis::completion::tokenize_input;
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
        // mid of `my_ent`
        let mut cursor = Position::new(0, 21);
        let tokens = tokenize_input(&input.symbols, input.source(), cursor);
        assert_matches!(
            tokens[..],
            [kind!(Use), kind!(Identifier), kind!(Dot), kind!(Identifier)]
        );
        // end of `my_ent`
        cursor = Position::new(0, 23);
        let tokens = tokenize_input(&input.symbols, input.source(), cursor);
        assert_matches!(
            tokens[..],
            [kind!(Use), kind!(Identifier), kind!(Dot), kind!(Identifier)]
        );
        // start of `is`
        cursor = Position::new(0, 24);
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
        // inside of `is`
        cursor = Position::new(0, 26);
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
}
