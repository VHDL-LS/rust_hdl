use vhdl_syntax::{
    syntax::{
        valid_nodes::ValidInterfaceDeclaration, validate::valid_node::Valid, PortClauseSyntax,
    },
    tokens::Keyword,
};

use crate::{
    error_code::{Category, ErrorCode},
    fix::Fix,
    rule::AstRule,
};

/// Checks that port clauses have an explicit mode set.
///
/// # Non-compliant example
///
/// ```vhdl,design-unit
/// entity foo is
///     port (
///         clk : std_logic
///     );
/// end foo;
/// ```
///
/// # Compliant example
///
/// ```vhdl,design-unit
/// entity foo is
///     port (
///         clk : in std_logic
///     );
/// end foo;
/// ```
pub struct ExplicitPortMode;

impl AstRule for ExplicitPortMode {
    type Node = PortClauseSyntax;

    const CODE: ErrorCode = ErrorCode::new(Category::Idiom, 2);

    fn check(&self, node: &Valid<Self::Node>, ctx: &mut super::AstRuleCtx<'_>) {
        for item in node.port_list().interface_elements() {
            if let ValidInterfaceDeclaration::InterfaceObjectDeclaration(object_declaration) =
                item.alternative()
            {
                if object_declaration.mode().is_none() {
                    let singular = object_declaration
                        .identifier_list()
                        .identifier_token()
                        .count()
                        == 1;

                    let edits = ctx.edits();

                    ctx.push(
                        object_declaration.identifier_list().text_range(),
                        "missing port mode",
                    )
                    .with_note(if singular {
                        "a port without a mode is an input"
                    } else {
                        "ports without a mode are inputs"
                    })
                    .with_fix(Fix::display_only(
                        "spell out 'in' if that is intended",
                        vec![edits.insert_after(&object_declaration.colon_token(), Keyword::In)],
                    ));
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use annotate_snippets::Renderer;
    use insta::assert_snapshot;

    use crate::{
        diagnostic::render_diagnostics,
        parse_and_analyze_file,
        rule::{
            explicit_port_mode::ExplicitPortMode,
            selection::{RuleOverrides, RuleSelector},
            AstRule, RuleRegistry,
        },
        AnalysisResult, File, FileId, FileSettings, FileStore,
    };

    // TODO: generalize the lint and assert_no_diagnostics function once more rules want tests.

    fn lint(expr: &str) -> String {
        let mut registry = RuleRegistry::new();
        registry.register(ExplicitPortMode).unwrap();
        let file = format!(
            "\
        entity foo is
        {expr}
        end;
        "
        );
        let mut files = FileStore::new();
        let id = files.insert(
            "<inline>",
            file.as_bytes().to_vec(),
            FileSettings::default(),
        );
        let overrides =
            RuleOverrides::new(vec![RuleSelector::Code(ExplicitPortMode::CODE)], vec![]);
        let diagnostics =
            parse_and_analyze_file(files.get(id), id, &registry, &overrides).into_diagnostics();
        let rendered = render_diagnostics(&diagnostics, &files).collect::<Vec<_>>();
        let renderer = Renderer::plain().anonymized_line_numbers(true);
        renderer.render(&rendered)
    }

    fn assert_no_diagnostics(expr: &str) {
        let mut registry = RuleRegistry::new();
        registry.register(ExplicitPortMode).unwrap();
        let file = format!(
            "\
        entity foo is
        {expr}
        end;
        "
        );
        let id = FileId(0);
        let file = File::new("<inline>", file.into_bytes(), FileSettings::default());
        let overrides =
            RuleOverrides::new(vec![RuleSelector::Code(ExplicitPortMode::CODE)], vec![]);
        match parse_and_analyze_file(&file, id, &registry, &overrides) {
            AnalysisResult::Lints(diagnostics) => assert!(
                diagnostics.is_empty(),
                "Unexpectedly got lints: {:#?}",
                diagnostics
            ),
            AnalysisResult::SyntaxErrs(diagnostics) => assert!(
                diagnostics.is_empty(),
                "Unexpectedly got syntax errors: {:#?}",
                diagnostics
            ),
        };
    }

    #[test]
    fn ports_with_explicit_mode() {
        assert_no_diagnostics(
            "\
port (
    clk: in std_logic;
    a, b : out std_logic_vector
);
        ",
        )
    }

    #[test]
    fn no_port_mode_around() {
        assert_snapshot!(lint("
port (
    clk: std_logic
);
        "), @"
        warning[IDM002]: missing port mode
          --> <inline>:4:5
           |
        LL |     clk: std_logic
           |     ^^^
           |
           = note: a port without a mode is an input
           = suggestion: spell out 'in' if that is intended
           |
        LL |     clk: in std_logic
           |          ++
        ");
    }

    #[test]
    fn ports_with_and_without_mode() {
        assert_snapshot!(lint("
port (
    clk: in std_logic;
    rst : std_logic;
    a, b : out std_logic_vector(7 downto 0);
    c : boolean
);
        "), @"
        warning[IDM002]: missing port mode
          --> <inline>:5:5
           |
        LL |     rst : std_logic;
           |     ^^^
           |
           = note: a port without a mode is an input
           = suggestion: spell out 'in' if that is intended
           |
        LL |     rst : in std_logic;
           |           ++
        warning[IDM002]: missing port mode
          --> <inline>:7:5
           |
        LL |     c : boolean
           |     ^
           = note: a port without a mode is an input
           = suggestion: spell out 'in' if that is intended
           |
        LL |     c : in boolean
           |         ++
        ");
    }
}
