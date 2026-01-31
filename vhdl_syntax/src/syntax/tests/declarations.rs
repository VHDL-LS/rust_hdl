use crate::check;
use crate::parser::Parser;
use crate::syntax::{declarations::*, IncompleteTypeDeclarationSyntax};
use crate::syntax::tests::{node, TestToString};
use crate::syntax::AstNode;

#[test]
fn alias_declaration() {
    let syntax_node =
        node::<AliasDeclarationSyntax>(Parser::alias_declaration, "alias foo is bar;");
    check!(syntax_node,
        alias_token => "alias",
        colon_token => None,
        subtype_indication => None,
        is_token => "is",
        name => "bar",
        signature => None,
        semi_colon_token => ";"
    );
    assert_eq!(
        syntax_node.alias_designator().unwrap().raw().debug_to_string().trim(),
        "foo"
    );
}

#[test]
fn attribute_declaration() {
    let syntax_node = node::<AttributeDeclarationSyntax>(
        Parser::attribute_declaration,
        "attribute foo : bar;",
    );
    check!(syntax_node,
        attribute_token => "attribute",
        identifier_token => "foo",
        colon_token => ":",
        name => "bar",
        semi_colon_token => ";"
    );
}

#[test]
fn component_declaration() {
    let syntax_node = node::<ComponentDeclarationSyntax>(
        Parser::component_declaration,
        "component Foo is\n  port (a: integer);\nend component Foo;",
    );
    check!(syntax_node,
        component_token => "component",
        name_token => "Foo",
        is_token => "is",
        generic_clause => None,
        port_clause => "port (a: integer);",
        end_token => "end",
        trailing_component_token => "component",
        trailing_name_token => "Foo",
        semi_colon_token => ";"
    );
}

#[test]
fn constant_declaration() {
    let syntax_node = node::<ConstantDeclarationSyntax>(
        Parser::constant_declaration,
        "constant C : integer := 42;",
    );
    check!(syntax_node,
        constant_token => "constant",
        identifier_list => "C",
        colon_token => ":",
        subtype_indication => "integer",
        colon_eq_token => ":=",
        expression => "42",
        semi_colon_token => ";"
    );
}

#[test]
fn file_declaration() {
    let syntax_node = node::<FileDeclarationSyntax>(
        Parser::file_declaration,
        "file f : text open std_logic_vector is \"fname\";",
    );
    check!(syntax_node,
        file_token => "file",
        identifier_list => "f",
        colon_token => ":",
        subtype_indication => "text",
        file_open_information => "open std_logic_vector is \"fname\"",
        semi_colon_token => ";"
    );
}

#[test]
fn signal_declaration() {
    let syntax_node = node::<SignalDeclarationSyntax>(
        Parser::signal_declaration,
        "signal s : std_logic;",
    );
    check!(syntax_node,
        signal_token => "signal",
        identifier_list => "s",
        colon_token => ":",
        subtype_indication => "std_logic",
        signal_kind => None,
        colon_eq_token => None,
        expression => None,
        semi_colon_token => ";"
    );
}

#[test]
fn subtype_declaration() {
    let syntax_node = node::<SubtypeDeclarationSyntax>(
        Parser::subtype_declaration,
        "subtype S is integer;",
    );
    check!(syntax_node,
        subtype_token => "subtype",
        identifier_token => "S",
        is_token => "is",
        subtype_indication => "integer",
        semi_colon_token => ";"
    );
}

#[test]
fn variable_and_shared_variable_declaration() {
    let syntax_node = node::<VariableDeclarationSyntax>(
        Parser::variable_declaration,
        "variable v : integer := 0;",
    );
    check!(syntax_node,
        variable_token => "variable",
        identifier_list => "v",
        colon_token => ":",
        subtype_indication => "integer",
        colon_eq_token => ":=",
        expression => "0",
        semi_colon_token => ";"
    );

    // shared variable is parsed by the same production; optional 'shared' token
    let syntax_node = node::<VariableDeclarationSyntax>(
        Parser::variable_declaration,
        "shared variable sv : integer;",
    );
    check!(syntax_node,
        variable_token => "variable",
        identifier_list => "sv",
        colon_token => ":",
        subtype_indication => "integer",
        semi_colon_token => ";"
    );
}

#[test]
#[ignore]
fn package_declaration() {
    todo!("Package in declarative part");
}

#[test]
#[ignore]
fn package_body_declaration() {
    todo!("Package body in declarative part");
}

#[test]
fn package_instantiation_declaration() {
    let syntax_node = node::<PackageInstantiationDeclarationSyntax>(
        Parser::package_instantiation_declaration,
        "package P1 is new pkg_inst generic map (g => 1);",
    );
    check!(syntax_node,
        package_instantiation => "package P1 is new pkg_inst generic map (g => 1)"
    );
}

#[test]
fn use_clause_declaration() {
    let syntax_node = node::<UseClauseDeclarationSyntax>(Parser::use_clause_declaration, "use work.pkg.all;");
    check!(syntax_node,
        use_clause => "use work.pkg.all;"
    );
}

#[test]
fn generic_and_port_clauses() {
    let syntax_node = node::<GenericClauseSyntax>(Parser::generic_clause, "generic (G: integer);");
    check!(syntax_node,
        generic_token => "generic",
        left_par_token => "(",
        interface_list => "G: integer",
        right_par_token => ")",
        semi_colon_token => ";"
    );

    let syntax_node = node::<PortClauseSyntax>(Parser::port_clause, "port (P: in integer);");
    check!(syntax_node,
        port_token => "port",
        left_par_token => "(",
        interface_list => "P: in integer",
        right_par_token => ")",
        semi_colon_token => ";"
    );
}

#[test]
fn generic_and_port_map_aspects() {
    let syntax_node = node::<GenericMapAspectSyntax>(
        Parser::generic_map_aspect,
        "generic map (g => 1)",
    );
    check!(syntax_node,
        generic_token => "generic",
        map_token => "map",
        left_par_token => "(",
        association_list => "g => 1",
        right_par_token => ")"
    );

    let syntax_node = node::<PortMapAspectSyntax>(
        Parser::port_map_aspect,
        "port map (p => 1);",
    );
    check!(syntax_node,
        port_token => "port",
        map_token => "map",
        left_par_token => "(",
        association_list => "p => 1",
        right_par_token => ")"
    );
}

#[test]
fn association_list_decl() {
    let syntax_node = node::<AssociationListSyntax>(Parser::association_list, "a => 1, b => 2");
    assert_eq!(syntax_node.raw().to_string().trim(), "a => 1, b => 2");
}

#[test]
fn interface_constant_declaration_test() {
    let syntax_node = node::<InterfaceConstantDeclarationSyntax>(
        Parser::interface_declaration,
        "constant a : in integer := 5;",
    );
    check!(syntax_node,
        constant_token => "constant",
        identifier_list => "a",
        colon_token => ":",
        in_token => "in",
        subtype_indication => "integer",
        colon_eq_token => ":=",
        expression => "5",
    );
}

#[test]
#[ignore]
fn interface_file_declaration_test() {
    let syntax_node = node::<InterfaceFileDeclarationSyntax>(
        Parser::interface_declaration,
        "file f : text;",
    );
    check!(syntax_node,
        file_token => "file",
        identifier_list => "a",
        colon_token => ":",
        subtype_indication => "text",
    );
}

#[test]
fn type_declarations() {
    let syntax_node = node::<FullTypeDeclarationSyntax>(Parser::type_declaration, "type T is file of integer;");
    check!(syntax_node,
        type_token => "type",
        identifier_token => "T",
        is_token => "is",
        type_definition => "file of integer",
        semi_colon_token => ";"
    );

    let syntax_node = node::<IncompleteTypeDeclarationSyntax>(Parser::type_declaration, "type incomplete; ");
    check!(syntax_node,
        type_token => "type",
        identifier_token => "incomplete",
        semi_colon_token => ";"
    );
}
