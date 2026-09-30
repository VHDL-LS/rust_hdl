use vhdl_syntax::syntax::{validate::valid_node::Valid, LibraryClauseSyntax, SyntaxNode};

use crate::{
    error_code::{Category, ErrorCode},
    fix::{Applicability, Fix},
    rule::{AstRule, Documented},
};

/// Checks that the 'work' library is not explicitly declared
///
/// In VHDL, every design unit already implicitly declares the 'work' library.
/// Writing it out explicitly is redundant.
/// Note that this currently only flags clauses with nothing but 'work'
/// (i.e., `library work;` is flagged while `library ieee, work;` is not).
///
/// # Non-compliant example
///
/// ```vhdl,design-unit,non-compliant
/// library work;
/// use work.foo.all;
///
/// entity bar is
///
/// end entity bar;
/// ```
///
/// # Compliant example
///
/// ```vhdl,design-unit,compliant
/// use work.foo.all;
///
/// entity bar is
///
/// end entity bar;
/// ```
#[derive(Documented)]
pub struct ExplicitWorkLibrary;

fn line_of_node_contains_comments(node: &SyntaxNode) -> bool {
    if node
        .visit_tokens()
        .skip(1)
        .any(|tok| tok.leading_trivia().contains_comments())
    {
        return true;
    }
    if node
        .first_token()
        .leading_trivia()
        .iter()
        .rev()
        .take_while(|piece| !piece.is_newline())
        .any(|piece| piece.is_comment())
    {
        return true;
    }
    node.last_token()
        .trailing_trivia()
        .iter()
        .take_while(|piece| !piece.is_newline())
        .any(|piece| piece.is_comment())
}

impl AstRule for ExplicitWorkLibrary {
    type Node = LibraryClauseSyntax;

    const CODE: ErrorCode = ErrorCode::new(Category::Idiom, 3);

    const DEFAULT_ENABLED: bool = false;

    fn check(&self, node: &Valid<Self::Node>, ctx: &mut super::AstRuleCtx<'_>) {
        if node
            .logical_name_list()
            .logical_names()
            .all(|lib| lib.text().eq_ignore_case(b"work"))
        {
            let edits = ctx.edits();
            ctx.push(node.text_range(), "explicit work library")
                .with_note("the work library is implicitly declared and can be omitted")
                .with_fix(Fix::new(
                    "remove this clause",
                    vec![edits.delete_line_of(node)],
                    if line_of_node_contains_comments(node) {
                        Applicability::Unsafe
                    } else {
                        Applicability::Safe
                    },
                ));
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
        rule::{explicit_work_library::ExplicitWorkLibrary, registry::ActiveRules},
        AnalysisResult, File, FileId, FileSettings, FileStore,
    };

    // TODO: generalize the lint and assert_no_diagnostics function once more rules want tests.

    fn lint(expr: &str) -> String {
        let file = format!(
            "\
        {expr}
        entity foo is
        end;
        "
        );
        let mut files = FileStore::new();
        let id = files.insert(
            "<inline>",
            file.as_bytes().to_vec(),
            FileSettings::default(),
        );
        let diagnostics = parse_and_analyze_file(
            files.get(id),
            id,
            &ActiveRules::single(&ExplicitWorkLibrary),
        )
        .into_diagnostics();
        let rendered = render_diagnostics(&diagnostics, &files).collect::<Vec<_>>();
        let renderer = Renderer::plain().anonymized_line_numbers(true);
        renderer.render(&rendered)
    }

    fn assert_no_diagnostics(expr: &str) {
        let file = format!(
            "\
        {expr}
        entity foo is
        end;
        "
        );
        let id = FileId(0);
        let file = File::new("<inline>", file.into_bytes(), FileSettings::default());
        match parse_and_analyze_file(&file, id, &ActiveRules::single(&ExplicitWorkLibrary)) {
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
    fn non_work_library() {
        assert_no_diagnostics(
            "\
library ieee;
library std;
        ",
        )
    }

    #[test]
    fn explicit_single_work_library() {
        assert_snapshot!(lint("
library work;
        "), @"
        warning[IDM003]: explicit work library
          --> <inline>:2:1
           |
        LL | library work;
           | ^^^^^^^^^^^^^
           |
           = note: the work library is implicitly declared and can be omitted
           = help: remove this clause
           |
        LL - library work;
           |
        ");
    }

    #[test]
    fn multiple_work_libraries_same_lint() {
        assert_snapshot!(lint("
library work, work;
        "), @"
        warning[IDM003]: explicit work library
          --> <inline>:2:1
           |
        LL | library work, work;
           | ^^^^^^^^^^^^^^^^^^^
           |
           = note: the work library is implicitly declared and can be omitted
           = help: remove this clause
           |
        LL - library work, work;
           |
        ");
    }
}
