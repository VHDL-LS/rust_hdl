use vhdl_syntax::syntax::{AstNode, IfStatementSyntax, ParenthesizedExpressionOrAggregateSyntax};

use crate::{
    error_code::{Category, ErrorCode},
    fix::{Edit, Fix},
    rule::AstRule,
};

/// Checks that the conditions of an `if` statement have no parenthesis.
///
/// # Non-compliant example
///
/// ```vhdl,sequential_statement
/// if (condition) then
///     foo <= bar;
/// end if;
/// ```
///
/// # Compliant example
/// ```vhdl,sequential_statement
/// if condition then
///     foo <= bar;
/// end if;
/// ```
pub struct NoParensAroundIf;

fn emit_redundant_parens(
    condition: &ParenthesizedExpressionOrAggregateSyntax,
    ctx: &mut super::AstRuleCtx,
    name: &str,
) {
    let Some(list) = condition.element_association_list() else {
        return;
    };
    let mut associations = list.element_associations();
    let Some(association) = associations.next() else {
        return;
    };
    if associations.next().is_some() {
        return;
    }
    if association.element_choices().is_some() {
        return;
    }
    let builder = ctx.push(
        condition.text_range(),
        format!("unnecessary parentheses around '{name}' condition"),
    );
    let mut edits = Vec::with_capacity(2);
    if let Some(left_par) = condition.left_par_token() {
        edits.push(Edit::delete(&left_par));
    }
    if let Some(right_par) = condition.right_par_token() {
        edits.push(Edit::delete(&right_par));
    }
    // Empty edits should never occur, but better be safe than sorry
    if !edits.is_empty() {
        builder.with_fix(Fix::safe("remove these parentheses", edits));
    }
}

impl AstRule for NoParensAroundIf {
    type Node = IfStatementSyntax;

    const CODE: ErrorCode = ErrorCode::new(Category::Idiom, 1);

    const DEFAULT_ENABLED: bool = false;

    fn check(&self, node: &Self::Node, ctx: &mut super::AstRuleCtx) {
        if let Some(condition) = node
            .if_statement_preamble()
            .and_then(|preamble| preamble.condition())
            .and_then(|condition| ParenthesizedExpressionOrAggregateSyntax::cast(condition.raw()))
        {
            emit_redundant_parens(&condition, ctx, "if");
        };

        for elsif in node.if_statement_elsifs() {
            let Some(condition) = elsif.condition().and_then(|condition| {
                ParenthesizedExpressionOrAggregateSyntax::cast(condition.raw())
            }) else {
                continue;
            };
            emit_redundant_parens(&condition, ctx, "elsif");
        }
    }
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use annotate_snippets::Renderer;
    use insta::assert_snapshot;

    use crate::{
        diagnostic::render_diagnostics,
        parse_and_analyze_file,
        rule::{
            no_parens_around_if::NoParensAroundIf,
            selection::{RuleOverwrites, RuleSelector},
            AstRule, RuleRegistry,
        },
        AnalysisResult, FileId,
    };

    // TODO: generalize the lint and assert_no_diagnostics function once more rules want tests.

    fn lint(expr: &str) -> String {
        let mut registry = RuleRegistry::new();
        registry.register(NoParensAroundIf).unwrap();
        let file = format!(
            "\
        architecture a of e is
            procedure foo is
            begin
            {expr}
            end;
        begin
        end;
        "
        );
        let id = FileId(0);
        let files = vec![(PathBuf::from("<inline>"), file.as_bytes().into())];
        let overrides =
            RuleOverwrites::new(vec![RuleSelector::Code(NoParensAroundIf::CODE)], vec![]);
        let diagnostics =
            parse_and_analyze_file(file.as_bytes(), id, &registry, &overrides).into_diagnostics();
        let rendered = render_diagnostics(&diagnostics, &files).collect::<Vec<_>>();
        let renderer = Renderer::plain().anonymized_line_numbers(true);
        renderer.render(&rendered)
    }

    fn assert_no_diagnostics(expr: &str) {
        let mut registry = RuleRegistry::new();
        registry.register(NoParensAroundIf).unwrap();
        let file = format!(
            "\
            architecture a of e is
                procedure foo is
                begin
                {expr}
                end;
            begin
            end;
        "
        );
        let id = FileId(0);
        let overrides =
            RuleOverwrites::new(vec![RuleSelector::Code(NoParensAroundIf::CODE)], vec![]);
        match parse_and_analyze_file(file.as_bytes(), id, &registry, &overrides) {
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
    fn if_statement_without_parenthesis() {
        assert_no_diagnostics(
            "\
if condition then
    foo <= bar;
end if;
        ",
        )
    }

    #[test]
    fn parentheses_that_are_not_a_single_positional_element_are_kept() {
        // An aggregate, and a named association, are not redundant parentheses
        assert_no_diagnostics("if (a, b) = c then end if;");
        assert_no_diagnostics("if (x => a) = c then end if;");
        // The parentheses only cover part of the condition
        assert_no_diagnostics("if (a) = b then end if;");
        assert_no_diagnostics("if a then elsif (b) and c then end if;");
    }

    #[test]
    fn simple_if_statement() {
        assert_snapshot!(lint("
if (condition) then
    foo <= bar;
end if;
        "), @"
        warning[IDM001]: unnecessary parentheses around 'if' condition
          --> <inline>:5:4
           |
        LL | if (condition) then
           |    ^^^^^^^^^^^
           |
           = help: remove these parentheses
           |
        LL - if (condition) then
        LL + if condition then
           |
        ");
    }

    #[test]
    fn elsif_statement() {
        assert_snapshot!(lint("
if (condition) then
    foo <= bar;
elsif (other_condition) then
    foo <= baz;
end if;
        "), @"
        warning[IDM001]: unnecessary parentheses around 'if' condition
          --> <inline>:5:4
           |
        LL | if (condition) then
           |    ^^^^^^^^^^^
           |
           = help: remove these parentheses
           |
        LL - if (condition) then
        LL + if condition then
           |
        warning[IDM001]: unnecessary parentheses around 'elsif' condition
          --> <inline>:7:7
           |
        LL | elsif (other_condition) then
           |       ^^^^^^^^^^^^^^^^^
           = help: remove these parentheses
           |
        LL - elsif (other_condition) then
        LL + elsif other_condition then
           |
        ");

        assert_snapshot!(lint("
if (condition) then
    foo <= bar;
elsif (other_condition) then
    foo <= baz;
elsif (third_condition) then
    foo <= foobar;
end if;
        "), @"
        warning[IDM001]: unnecessary parentheses around 'if' condition
          --> <inline>:5:4
           |
        LL | if (condition) then
           |    ^^^^^^^^^^^
           |
           = help: remove these parentheses
           |
        LL - if (condition) then
        LL + if condition then
           |
        warning[IDM001]: unnecessary parentheses around 'elsif' condition
          --> <inline>:7:7
           |
        LL | elsif (other_condition) then
           |       ^^^^^^^^^^^^^^^^^
           = help: remove these parentheses
           |
        LL - elsif (other_condition) then
        LL + elsif other_condition then
           |
        warning[IDM001]: unnecessary parentheses around 'elsif' condition
          --> <inline>:9:7
           |
        LL | elsif (third_condition) then
           |       ^^^^^^^^^^^^^^^^^
           = help: remove these parentheses
           |
        LL - elsif (third_condition) then
        LL + elsif third_condition then
           |
        ");
    }
}
