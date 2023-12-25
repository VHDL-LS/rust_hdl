use crate::analysis::DesignRoot;
use crate::ast::search::{Found, FoundDeclaration, NotFinished, NotFound, SearchState, Searcher};
use crate::ast::{
    AnyDesignUnit, AnyPrimaryUnit, ConcurrentStatement, Designator, MapAspect, ObjectClass,
};
use crate::data::{ContentReader, Symbol};
use crate::named_entity::{self, AsUnique, HasEntityId, NamedEntities, Region};
use crate::syntax::Kind::*;
use crate::syntax::{Kind, Symbols, Token, TokenAccess, Tokenizer, Value};
use crate::{AnyEntKind, Design, EntRef, EntityId, HasTokenSpan, Overloaded, Position, Source};
use std::collections::HashSet;
use std::default::Default;
use std::iter::once;
use vhdl_lang::ast::search::Finished;

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
    /// Entity instantiation, i.e.,
    /// ```vhdl
    /// my_ent: entity work.foo
    ///     generic map (
    ///         -- ...
    ///     )
    ///     port map (
    ///         -- ...
    ///     );
    EntityInstantiation(EntRef<'a>),
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

impl<'a> Region<'a> {
    /// From this region, extracts those `AnyEntKind::Object`s where the class of the
    /// object matches the specified class.
    fn extract_objects_with_class(&self, object_class: ObjectClass) -> Vec<EntityId> {
        self.entities
            .values()
            .filter_map(|ent| ent.as_unique())
            .filter_map(|ent| match &ent.kind {
                AnyEntKind::Object(obj) if obj.class == object_class => Some(ent.id),
                AnyEntKind::Overloaded(Overloaded::InterfaceSubprogram(_))
                    if object_class == ObjectClass::Constant =>
                {
                    Some(ent.id)
                }
                AnyEntKind::Type(named_entity::Type::Interface)
                    if object_class == ObjectClass::Constant =>
                {
                    Some(ent.id)
                }
                _ => None,
            })
            .collect()
    }
}

impl DesignRoot {
    /// Extracts the name of ports or generics from an AST for an entity with a certain ID.
    /// The entity can be an `Entity`, `Component` or `Package`.
    ///
    /// # Arguments
    ///
    /// * `object_class` - What to extract. `ObjectClass::Signal` extracts ports
    /// while `ObjectClass::Constant` extracts constants.
    fn extract_port_or_generic_names(
        &self,
        id: EntityId,
        object_class: ObjectClass,
    ) -> Vec<EntityId> {
        let cmp_ent = self.get_ent(id);
        match cmp_ent.kind() {
            AnyEntKind::Component(region) => region.extract_objects_with_class(object_class),
            AnyEntKind::Design(Design::Entity(_, region)) => {
                region.extract_objects_with_class(object_class)
            }
            AnyEntKind::Design(Design::UninstPackage(_, region)) => {
                region.extract_objects_with_class(object_class)
            }
            _ => vec![],
        }
    }

    pub fn extract_port_names(&self, id: EntityId) -> Vec<EntityId> {
        self.extract_port_or_generic_names(id, ObjectClass::Signal)
    }

    pub fn extract_generic_names(&self, id: EntityId) -> Vec<EntityId> {
        self.extract_port_or_generic_names(id, ObjectClass::Constant)
    }
}

/// Visitor responsible for completions in selected AST elements
struct AutocompletionSearcher<'a> {
    root: &'a DesignRoot,
    cursor: Position,
    completions: Vec<CompletionItem<'a>>,
}

impl<'a> AutocompletionSearcher<'a> {
    pub fn new(root: &'a DesignRoot, cursor: Position) -> AutocompletionSearcher<'a> {
        AutocompletionSearcher {
            root,
            cursor,
            completions: Vec::new(),
        }
    }

    /// Loads completion options for the given map aspect.
    /// Returns `true`, when the cursor is inside the map aspect and the search should not continue.
    /// Returns `false` otherwise
    fn load_completions_for_map_aspect(
        &mut self,
        ent_ref: Option<EntityId>,
        map: &MapAspect,
        ctx: &dyn TokenAccess,
        kind: MapAspectKind,
    ) -> bool {
        if !map.span(ctx).contains(self.cursor) {
            return false;
        }
        let formals_in_map: HashSet<EntityId> = HashSet::from_iter(map.formals().flatten());
        if let Some(ent) = ent_ref {
            let ids = match kind {
                MapAspectKind::Port => self.root.extract_port_names(ent),
                MapAspectKind::Generic => self.root.extract_generic_names(ent),
            };
            self.completions.extend(
                ids.iter()
                    .filter(|id| !formals_in_map.contains(id))
                    .map(|id| CompletionItem::Formal(self.root.get_ent(*id))),
            );
        }
        true
    }
}

impl<'a> Searcher for AutocompletionSearcher<'a> {
    /// Visit an instantiation statement extracting completions for ports or generics.
    fn search_decl(&mut self, ctx: &dyn TokenAccess, decl: FoundDeclaration) -> SearchState {
        match decl {
            FoundDeclaration::ConcurrentStatement(stmt) => {
                if let ConcurrentStatement::Instance(inst) = &stmt.statement.item {
                    if let Some(map) = &inst.generic_map {
                        if self.load_completions_for_map_aspect(
                            inst.entity_reference(),
                            map,
                            ctx,
                            MapAspectKind::Generic,
                        ) {
                            return Finished(Found);
                        }
                    }
                    if let Some(map) = &inst.port_map {
                        if self.load_completions_for_map_aspect(
                            inst.entity_reference(),
                            map,
                            ctx,
                            MapAspectKind::Port,
                        ) {
                            return Finished(Found);
                        }
                    }
                }
            }
            FoundDeclaration::PackageInstance(inst) => {
                if let Some(map) = &inst.generic_map {
                    if self.load_completions_for_map_aspect(
                        inst.package_name.item.get_suffix_reference(),
                        map,
                        ctx,
                        MapAspectKind::Generic,
                    ) {
                        return Finished(Found);
                    }
                }
            }
            _ => {}
        }
        NotFinished
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

/// helper function to list the name of all available libraries
fn list_all_libraries(root: &DesignRoot) -> Vec<CompletionItem> {
    root.libraries()
        .map(|lib| CompletionItem::Simple(root.get_ent(lib.id())))
        .collect()
}

/// List the name of all primary units for a given library.
/// If the library is non-resolvable, list an empty vector
fn list_primaries_for_lib<'a>(root: &'a DesignRoot, lib: &Symbol) -> Vec<CompletionItem<'a>> {
    let Some(lib) = root.get_lib(lib) else {
        return vec![];
    };
    lib.primary_units()
        .filter_map(|it| it.unit.get().and_then(|unit| unit.ent_id()))
        .map(|id| CompletionItem::Simple(root.get_ent(id)))
        .collect()
}

/// Lists all available declarations for a primary unit inside a given library
/// If the library does not exist or there is no primary unit with the given name for that library,
/// return an empty vector
fn list_available_declarations<'a>(
    root: &'a DesignRoot,
    lib: &Symbol,
    primary_unit: &Symbol,
) -> Vec<CompletionItem<'a>> {
    let Some(unit) = root
        .get_lib(lib)
        .and_then(|lib| lib.primary_unit(primary_unit))
        .and_then(|unit| unit.unit.get())
    else {
        return vec![];
    };

    match unit.data() {
        AnyDesignUnit::Primary(AnyPrimaryUnit::Package(pkg)) => {
            let Some(pkg_id) = pkg.ident.decl.get() else {
                return Vec::default();
            };
            let ent = root.get_ent(pkg_id);
            match &ent.kind {
                AnyEntKind::Design(Design::Package(_, region)) => region
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

struct RegionSearcher<'a> {
    root: &'a DesignRoot,
    cursor: Position,
    completions: Vec<CompletionItem<'a>>,
}

impl<'a> RegionSearcher<'a> {
    pub fn new(cursor: Position, design_root: &'a DesignRoot) -> RegionSearcher<'a> {
        RegionSearcher {
            root: design_root,
            cursor,
            completions: Vec::new(),
        }
    }
}

impl<'a> Searcher for RegionSearcher<'a> {
    fn search_decl(&mut self, ctx: &dyn TokenAccess, decl: FoundDeclaration) -> SearchState {
        match decl {
            FoundDeclaration::Architecture(body) => {
                for statement in &body.statements {
                    let pos = &statement.statement.pos;

                    if pos.start() > self.cursor {
                        break;
                    }

                    if pos.contains(self.cursor) {
                        return Finished(NotFound);
                    }
                }
                if ctx
                    .get_span(body.begin_token, body.get_end_token())
                    .contains(self.cursor)
                {
                    self.completions = self
                        .root
                        .list_visible_entities()
                        .map(|eid| CompletionItem::EntityInstantiation(self.root.get_ent(eid)))
                        .collect()
                }
                Finished(Found)
            }
            _ => NotFinished,
        }
    }
}

impl DesignRoot {
    pub fn list_visible_entities(&self) -> impl Iterator<Item = EntityId> + '_ {
        self.libraries()
            .flat_map(|lib| lib.primary_units())
            .filter(|unit| unit.unit.get().map(|it| it.is_entity()).unwrap_or(false))
            .filter_map(|unit| unit.unit.get().and_then(|it| it.ent_id()))
    }
}

/// Main entry point for completion. Given a source-file and a cursor position,
/// lists available completion options at the cursor position.
pub fn list_completion_options<'a>(
    root: &'a DesignRoot,
    source: &Source,
    cursor: Position,
) -> Vec<CompletionItem<'a>> {
    let tokens = tokenize_input(root.symbols(), source, cursor);
    match &tokens[..] {
        [.., kind!(Library)] | [.., kind!(Use)] | [.., kind!(Use), kind!(Identifier)] => {
            list_all_libraries(root)
        }
        [.., kind!(Use), ident!(library), kind!(Dot)]
        | [.., kind!(Use), ident!(library), kind!(Dot), kind!(Identifier)] => {
            list_primaries_for_lib(root, library)
        }
        [.., kind!(Use), ident!(library), kind!(Dot), ident!(selected), kind!(Dot)]
        | [.., kind!(Use), ident!(library), kind!(Dot), ident!(selected), kind!(Dot), kind!(StringLiteral | Identifier)] => {
            list_available_declarations(root, library, selected)
        }
        [.., kind!(LeftPar | Comma)] | [.., kind!(LeftPar | Comma), kind!(Identifier)] => {
            let mut searcher = AutocompletionSearcher::new(root, cursor);
            let _ = root.search_source(source, &mut searcher);
            searcher.completions
        }
        _ => {
            let mut searcher = RegionSearcher::new(cursor, root);
            let _ = root.search_source(source, &mut searcher);
            searcher.completions
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::analysis::tests::LibraryBuilder;
    use crate::completion::tokenize_input;
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
        let options = list_completion_options(&root, code.source(), cursor);
        assert_eq!(options, list_all_libraries(&root))
    }

    #[test]
    pub fn completing_primaries() {
        let (root, _) = LibraryBuilder::new().get_analyzed_root();
        let code = Code::new("use std.");
        let cursor = code.pos().end();
        let options = list_completion_options(&root, code.source(), cursor);
        assert!(options.contains(&CompletionItem::Simple(root.find_textio_pkg())));
        assert!(options.contains(&CompletionItem::Simple(root.find_standard_pkg())));
        assert!(options.contains(&CompletionItem::Simple(root.find_env_pkg())));
        assert_eq!(options.len(), 3);

        let code = Code::new("use std.t");
        let cursor = code.pos().end();
        let options = list_completion_options(&root, code.source(), cursor);
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
        let options = list_completion_options(&root, code.source(), cursor);

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
        let options = list_completion_options(&root, code.source(), cursor);
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
        let options = list_completion_options(&root, code.source(), cursor);
        assert!(options.contains(&CompletionItem::Formal(rst)));
        assert!(options.contains(&CompletionItem::Formal(dout)));
        assert_eq!(options.len(), 2);
        let cursor = code
            .s1("port map (
            clk =>")
            .pos()
            .end();
        let options = list_completion_options(&root, code.source(), cursor);
        assert_eq!(options.len(), 0);
        let cursor = code
            .s1("port map (
            clk => c")
            .pos()
            .end();
        let options = list_completion_options(&root, code.source(), cursor);
        assert_eq!(options.len(), 0);
    }

    #[test]
    pub fn complete_in_generic_map() {
        let mut input = LibraryBuilder::new();
        let code = input.code(
            "libname",
            "\
    package my_pkg is
    generic (
        function foo(x: Integer) return bit;
        function bar(y: Integer) return boolean;
        type T;
        x: natural
    );
    end my_pkg;

    use work.my_pkg.all ; 
    package my_pkg_inst is new work.my_pkg
    generic map (
         foo => foo
    );",
        );
        let (root, _) = input.get_analyzed_root();
        let bar_func = root
            .search_reference(code.source(), code.s1("bar").start())
            .unwrap();
        let x = root
            .search_reference(code.source(), code.s1("x: natural").s1("x").start())
            .unwrap();
        let t = root
            .search_reference(code.source(), code.s1("type T").s1("T").start())
            .unwrap();
        let cursor = code.s1("generic map (").pos().end();
        let options = list_completion_options(&root, code.source(), cursor);
        assert!(options.contains(&CompletionItem::Formal(bar_func)));
        assert!(options.contains(&CompletionItem::Formal(x)));
        assert!(options.contains(&CompletionItem::Formal(t)));
        assert_eq!(options.len(), 3);
    }
}
