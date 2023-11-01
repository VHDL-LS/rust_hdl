use crate::analysis::region::{AsUnique, NamedEntities};
use crate::analysis::{DesignRoot, HasEntityId};
use crate::ast::visitor::{Visitor, VisitorResult};
use crate::ast::{
    AnyDesignUnit, AnyPrimaryUnit, AnySecondaryUnit, ComponentDeclaration, Designator,
    EntityDeclaration, InstantiationStatement, InterfaceDeclaration, MapAspect, PackageDeclaration,
};
use crate::data::{ContentReader, Symbol};
use crate::syntax::Kind::*;
use crate::syntax::{Kind, Symbols, Token, TokenAccess, Tokenizer, Value};
use crate::AnyEntKind::Design;
use crate::{EntRef, EntityId, Position, Source};
use std::collections::HashSet;
use std::default::Default;
use std::iter::once;

#[derive(Debug, PartialEq, Clone)]
pub enum CompletionItem<'a> {
    /// Simply complete the entities
    /// e.g., `use std.` should simply list all elements in the std library
    Simple(EntRef<'a>),
    /// Formal parameter, e.g., in a port map
    /// `port map (` might choose to complete `<item> => $1`
    Formal(EntRef<'a>),
    /// Multiple overloaded items are applicable.
    /// The argument is the count of overloaded items in total.
    Overloaded(Designator, usize),
    /// Complete a keyword
    Keyword(Kind),
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

#[derive(Eq, PartialEq, Debug)]
enum MapAspectKind {
    Port,
    Generic,
}

/// Extracts the name of ports or generics from an AST for an entity with a certain ID.
/// The entity can be an `Entity`, `Component` or `Package`.
/// After walking the AST, the ports or generics are written to the `items` vector.
/// The `kind` member chooses whether to select ports or generics.
struct PortsOrGenericsExtractor {
    id: EntityId,
    items: Vec<EntityId>,
    kind: MapAspectKind,
}

impl DesignRoot {
    fn extract_port_or_generic_names(&self, id: EntityId, kind: MapAspectKind) -> Vec<EntityId> {
        let mut searcher = PortsOrGenericsExtractor::new(id, kind);
        self.walk(&mut searcher);
        searcher.items
    }
}

impl PortsOrGenericsExtractor {
    pub fn new(id: EntityId, kind: MapAspectKind) -> PortsOrGenericsExtractor {
        PortsOrGenericsExtractor {
            id,
            items: vec![],
            kind,
        }
    }

    fn add_map_aspect_items(&mut self, map_aspect: &Vec<InterfaceDeclaration>) {
        for decl in map_aspect {
            if let Some(id) = decl.ent_id() {
                self.items.push(id)
            }
        }
    }

    fn add_optional_map_aspect_items(&mut self, map_aspect: &Option<Vec<InterfaceDeclaration>>) {
        if let Some(map_aspect) = map_aspect {
            self.add_map_aspect_items(map_aspect);
        }
    }
}

impl Visitor for PortsOrGenericsExtractor {
    fn visit_component_declaration(
        &mut self,
        node: &ComponentDeclaration,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        if node.ident.decl != Some(self.id) {
            return VisitorResult::Skip;
        }
        if self.kind == MapAspectKind::Port {
            self.add_map_aspect_items(&node.port_list);
        }
        if self.kind == MapAspectKind::Generic {
            self.add_map_aspect_items(&node.generic_list);
        }
        VisitorResult::Stop
    }

    fn visit_entity_declaration(
        &mut self,
        node: &EntityDeclaration,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        if node.ident.decl != Some(self.id) {
            return VisitorResult::Skip;
        }
        if self.kind == MapAspectKind::Port {
            self.add_optional_map_aspect_items(&node.port_clause);
        }
        if self.kind == MapAspectKind::Generic {
            self.add_optional_map_aspect_items(&node.generic_clause);
        }
        VisitorResult::Stop
    }

    fn visit_package_declaration(
        &mut self,
        node: &PackageDeclaration,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        if node.ident.decl != Some(self.id) {
            return VisitorResult::Skip;
        }
        if self.kind == MapAspectKind::Generic {
            self.add_optional_map_aspect_items(&node.generic_clause);
        }
        VisitorResult::Stop
    }
}

/// Visitor responsible for completions in selected AST elements
struct AutocompletionVisitor<'a> {
    root: &'a DesignRoot,
    cursor: Position,
    completions: Vec<CompletionItem<'a>>,
    tokens: Vec<Token>,
}

impl<'a> AutocompletionVisitor<'a> {
    pub fn new(
        root: &'a DesignRoot,
        cursor: Position,
        tokens: Vec<Token>,
    ) -> AutocompletionVisitor<'a> {
        AutocompletionVisitor {
            root,
            cursor,
            completions: Vec::new(),
            tokens,
        }
    }

    /// Loads completion options for the given map aspect.
    /// Returns `true`, when the cursor is inside the map aspect and the search should not continue.
    /// Returns `false` otherwise
    fn load_completions_for_map_aspect(
        &mut self,
        node: &InstantiationStatement,
        map: &MapAspect,
        ctx: &dyn TokenAccess,
        kind: MapAspectKind,
    ) -> bool {
        let Some(tok) = self.tokens.last() else {
            return false;
        };
        if tok.kind != LeftPar && tok.kind != Comma {
            return false;
        }
        if !map.span(ctx).contains(self.cursor) {
            return false;
        }
        let formals_in_map: HashSet<EntityId> =
            HashSet::from_iter(map.formals().filter_map(|it| *it));
        if let Some(ent) = node.entity_reference() {
            let ids = self.root.extract_port_or_generic_names(ent, kind);
            self.completions.extend(
                ids.iter()
                    .filter(|id| !formals_in_map.contains(id))
                    .map(|id| CompletionItem::Formal(self.root.get_ent(*id))),
            );
        }
        true
    }
}

impl<'a> Visitor for AutocompletionVisitor<'a> {
    /// Visit an instantiation statement extracting completions for ports or generics.
    fn visit_instantiation_statement(
        &mut self,
        node: &InstantiationStatement,
        ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        if let Some(map) = &node.generic_map {
            if self.load_completions_for_map_aspect(node, map, ctx, MapAspectKind::Generic) {
                return VisitorResult::Stop;
            }
        }
        if let Some(map) = &node.port_map {
            if self.load_completions_for_map_aspect(node, map, ctx, MapAspectKind::Port) {
                return VisitorResult::Stop;
            }
        }
        VisitorResult::Skip
    }

    // preliminary optimizations: only visit architecture
    fn visit_any_primary_unit(
        &mut self,
        _node: &AnyPrimaryUnit,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        VisitorResult::Skip
    }

    // preliminary optimizations: only visit architecture
    fn visit_any_secondary_unit(
        &mut self,
        node: &AnySecondaryUnit,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        match node {
            AnySecondaryUnit::Architecture(_) => VisitorResult::Continue,
            AnySecondaryUnit::PackageBody(_) => VisitorResult::Skip,
        }
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
    fn list_all_libraries(&self) -> Vec<CompletionItem> {
        self.libraries()
            .map(|lib| CompletionItem::Simple(self.get_ent(lib.id())))
            .collect()
    }

    /// List the name of all primary units for a given library.
    /// If the library is non-resolvable, list an empty vector
    fn list_primaries_for_lib(&self, lib: &Symbol) -> Vec<CompletionItem> {
        let Some(lib) = self.get_lib(lib) else {
            return vec![];
        };
        lib.primary_units()
            .filter_map(|it| it.unit.get().and_then(|unit| unit.ent_id()))
            .map(|id| CompletionItem::Simple(self.get_ent(id)))
            .collect()
    }

    /// Lists all available declarations for a primary unit inside a given library
    /// If the library does not exist or there is no primary unit with the given name for that library,
    /// return an empty vector
    fn list_available_declarations(
        &self,
        lib: &Symbol,
        primary_unit: &Symbol,
    ) -> Vec<CompletionItem> {
        let Some(unit) = self
            .get_lib(lib)
            .and_then(|lib| lib.primary_unit(primary_unit))
            .and_then(|unit| unit.unit.get())
        else {
            return vec![];
        };

        match unit.data() {
            AnyDesignUnit::Primary(AnyPrimaryUnit::Package(pkg)) => {
                let Some(pkg_id) = pkg.ident.decl else {
                    return Vec::default();
                };
                let ent = self.get_ent(pkg_id);
                match &ent.kind {
                    Design(crate::analysis::Design::Package(_, region)) => region
                        .entities
                        .values()
                        .map(|named_ent| match named_ent {
                            NamedEntities::Single(ent) => CompletionItem::Simple(ent),
                            NamedEntities::Overloaded(overloaded) => match overloaded.as_unique() {
                                None => CompletionItem::Overloaded(
                                    overloaded.designator().clone(),
                                    overloaded.len(),
                                ),
                                Some(ent_ref) => CompletionItem::Simple(ent_ref),
                            },
                        })
                        .chain(once(CompletionItem::Keyword(All)))
                        .collect(),
                    _ => Vec::default(),
                }
            }
            _ => Vec::default(),
        }
    }

    /// Main entry point for completion. Given a source-file and a cursor position,
    /// lists available completion options at the cursor position.
    pub fn list_completion_options(
        &self,
        source: &Source,
        cursor: Position,
    ) -> Vec<CompletionItem> {
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
            _ => {
                let mut visitor = AutocompletionVisitor::new(self, cursor, tokens);
                self.walk(&mut visitor);
                visitor.completions
            }
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
        assert!(options.contains(&CompletionItem::Simple(root.find_textio_pkg())));
        assert!(options.contains(&CompletionItem::Simple(root.find_standard_pkg())));
        assert!(options.contains(&CompletionItem::Simple(root.find_env_pkg())));
        assert_eq!(options.len(), 3);

        let code = Code::new("use std.t");
        let cursor = code.pos().end();
        let options = root.list_completion_options(code.source(), cursor);
        // Note that the filtering only happens at client side
        assert!(options.contains(&CompletionItem::Simple(root.find_textio_pkg())));
        assert!(options.contains(&CompletionItem::Simple(root.find_standard_pkg())));
        assert!(options.contains(&CompletionItem::Simple(root.find_env_pkg())));
        assert_eq!(options.len(), 3);
    }

    #[test]
    pub fn completing_declarations() {
        let input = LibraryBuilder::new();
        let code = Code::new("use std.env.");
        let (root, _) = input.get_analyzed_root();
        let cursor = code.pos().end();
        let options = root.list_completion_options(code.source(), cursor);
        println!("{:?}", options);

        assert!(options.contains(&CompletionItem::Overloaded(
            Designator::Identifier(root.symbol_utf8("stop")),
            2
        )));
        assert!(options.contains(&CompletionItem::Overloaded(
            Designator::Identifier(root.symbol_utf8("finish")),
            2
        )));
        assert!(options.contains(&CompletionItem::Simple(
            root.find_env_symbol("resolution_limit"),
        )));
        assert!(options.contains(&CompletionItem::Keyword(All)));
        assert_eq!(options.len(), 4);
    }

    #[test]
    pub fn completing_instantiation_statement() {
        let mut input = LibraryBuilder::new();
        let code = input.code(
            "libname",
            "\
    entity my_ent is
    end entity my_ent;

    architecture arch of my_ent is
        component comp is
        generic (
          A: natural := 5;
          B: integer
        );
        port (
          clk : in bit;
          rst : in bit;
          dout : out bit
        );
        end component comp;
        signal clk, rst: bit;
    begin
        comp_inst: comp
        generic map (
            A => 2
        )
        port map (
            clk => clk
        );
    end arch;
            ",
        );
        let (root, _) = input.get_analyzed_root();
        let cursor = code.s1("generic map (").pos().end();
        let options = root.list_completion_options(code.source(), cursor);
        let ent = root
            .search_reference(code.source(), code.s1("B").start())
            .unwrap();
        assert_eq!(options, vec![CompletionItem::Formal(ent)]);

        let rst = root
            .search_reference(code.source(), code.s1("rst").start())
            .unwrap();

        let dout = root
            .search_reference(code.source(), code.s1("dout").start())
            .unwrap();

        let cursor = code.s1("port map (").pos().end();
        let options = root.list_completion_options(code.source(), cursor);
        assert!(options.contains(&CompletionItem::Formal(rst)));
        assert!(options.contains(&CompletionItem::Formal(dout)));
        assert_eq!(options.len(), 2);
    }
}
