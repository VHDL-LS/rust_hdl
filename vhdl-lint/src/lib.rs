pub mod diagnostic;
pub mod error_code;
pub mod fix;
pub mod rule;
pub mod severity;
pub mod source_loc;

use vhdl_syntax::parser::parse;

use crate::{
    diagnostic::Diagnostic,
    fix::{apply_fixes, Fix},
    rule::{
        selection::{OverwriteResult, RuleOverwrites},
        AstRuleCtx, ErasedAstRule, RuleRegistry,
    },
};

/// ID that points to a file on disk
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct FileId(pub u32);

fn is_rule_active(rule: &dyn ErasedAstRule, overwrites: &RuleOverwrites) -> bool {
    match overwrites.get(rule.code()) {
        OverwriteResult::Ignore => false,
        OverwriteResult::Select => true,
        OverwriteResult::Default => rule.is_enabled_by_default(),
    }
}

/// Outcome of analyzing a single file
#[derive(Debug)]
pub enum AnalysisResult {
    /// The file has syntax errors. Lints were not run.
    SyntaxErrs(Vec<Diagnostic>),
    /// The file parsed with the given (potentially empty) errors.
    Lints(Vec<Diagnostic>),
}

impl AnalysisResult {
    pub fn into_diagnostics(self) -> Vec<Diagnostic> {
        match self {
            AnalysisResult::SyntaxErrs(diag) | AnalysisResult::Lints(diag) => diag,
        }
    }
}

/// Parse and perform single-file analysis.
// Multi-file analysis with proper dependency tracking etc needs a proper analyzer.
// This is designed to be the "escape hatch" when such analysis is not available, e.g.,
// due to missing library mapping.
pub fn parse_and_analyze_file(
    file: &[u8],
    file_id: FileId,
    rules: &RuleRegistry,
    overwrites: &RuleOverwrites,
) -> AnalysisResult {
    let (design, errors) = parse(file);
    if !errors.is_empty() {
        return AnalysisResult::SyntaxErrs(
            errors
                .into_iter()
                .map(|err| Diagnostic::from_syntax_err(err, file_id))
                .collect::<Vec<_>>(),
        );
    }

    let mut diagnostics = Vec::new();
    for node in design.descendants() {
        for rule in rules.for_kind(node.kind()) {
            if !is_rule_active(rule, overwrites) {
                continue;
            }
            let mut ctx = AstRuleCtx::new(&mut diagnostics, rule.severity(), file_id, rule.code());
            rule.check_raw(node.clone(), &mut ctx);
        }
    }

    AnalysisResult::Lints(diagnostics)
}

fn get_fixes(diagnostics: &[Diagnostic]) -> Vec<&Fix> {
    diagnostics.iter().filter_map(|diag| diag.fix()).collect()
}

const MAX_TRIES: usize = 100;

pub enum FixResult {
    /// The file contained syntax errors; fixes aren't applied
    SyntaxErrs(Vec<Diagnostic>),
    /// The file was fixed
    Fixed {
        /// Contents of the new file
        file: Box<[u8]>,
        /// Number of applied fixes
        applied_fixes: usize,
        /// Remaining (unfixeable) diagnostics
        diagnostics: Vec<Diagnostic>,
    },
    /// The file was not fixed because there are no fixes applicable
    NotFixed { diagnostics: Vec<Diagnostic> },
    /// Syntax errors occured after a fixing iteration
    ErrAfterFixing {
        /// The syntax errors
        diagnostics: Vec<Diagnostic>,
        /// The partial file that remained after fixing
        partial_file: Box<[u8]>,
    },
    /// Fixing the file yields no fixed-point convergence after `MAX_TRIES`
    TooManyTries {
        /// The remaining diagnostics after `MAX_TRIES`
        diagnostics: Vec<Diagnostic>,
        /// The partial file that remained after all tries
        partial_file: Box<[u8]>,
    },
}

pub fn fix_file(
    file: &[u8],
    file_id: FileId,
    rules: &RuleRegistry,
    overwrites: &RuleOverwrites,
) -> FixResult {
    let mut diagnostics = match parse_and_analyze_file(file, file_id, rules, overwrites) {
        AnalysisResult::Lints(diagnostics) => diagnostics,
        AnalysisResult::SyntaxErrs(diagnostics) => return FixResult::SyntaxErrs(diagnostics),
    };

    if get_fixes(&diagnostics).is_empty() {
        return FixResult::NotFixed { diagnostics };
    }

    let mut output_file: Vec<u8> = Vec::from(file);

    let mut applied_fixes = 0usize;

    // Every pass applies as many fixes as do not conflict with each other and re-derives
    // the rest from the edited text, so it may take several passes to reach a fixpoint.
    for _ in 0..MAX_TRIES {
        let mut fixes = get_fixes(&diagnostics);
        if fixes.is_empty() {
            return FixResult::Fixed {
                file: output_file.into_boxed_slice(),
                applied_fixes,
                diagnostics,
            };
        }
        fixes.sort_by_key(|fix| (fix.extent().start, fix.extent().end));
        let (next, number_fixes) = apply_fixes(&output_file, &fixes);
        applied_fixes += number_fixes;
        if next == output_file {
            if output_file == file {
                return FixResult::NotFixed { diagnostics };
            } else {
                return FixResult::Fixed {
                    file: output_file.into_boxed_slice(),
                    applied_fixes,
                    diagnostics,
                };
            }
        }
        output_file = next;
        diagnostics = match parse_and_analyze_file(&output_file, file_id, rules, overwrites) {
            AnalysisResult::Lints(diagnostics) => diagnostics,
            AnalysisResult::SyntaxErrs(diagnostics) => {
                return FixResult::ErrAfterFixing {
                    diagnostics,
                    partial_file: output_file.into_boxed_slice(),
                }
            }
        };
    }

    FixResult::TooManyTries {
        diagnostics,
        partial_file: output_file.into_boxed_slice(),
    }
}

#[cfg(test)]
mod tests {
    use vhdl_syntax::syntax::{AstNode, IfStatementSyntax};

    use super::*;
    use crate::{
        error_code::{Category, ErrorCode},
        fix::Edit,
        rule::{no_parens_around_if::NoParensAroundIf, selection::RuleSelector, AstRule},
    };

    fn in_procedure(statements: &str) -> String {
        format!(
            "\
architecture a of e is
    procedure foo is
    begin
        {statements}
    end;
begin
end;
"
        )
    }

    fn registry<R: AstRule>(rule: R) -> RuleRegistry {
        let mut registry = RuleRegistry::new();
        registry.register(rule).unwrap();
        registry
    }

    fn select_all() -> RuleOverwrites {
        RuleOverwrites::new(vec![RuleSelector::All], vec![])
    }

    fn fix(source: &str, rules: &RuleRegistry) -> FixResult {
        fix_file(source.as_bytes(), FileId(0), rules, &select_all())
    }

    /// Replaces every `if` statement with text that does not parse.
    struct BreaksSyntax;

    impl AstRule for BreaksSyntax {
        type Node = IfStatementSyntax;
        const CODE: ErrorCode = ErrorCode::new(Category::Idiom, 900);
        fn check(&self, node: &Self::Node, ctx: &mut AstRuleCtx<'_>) {
            let range = node.raw().text_range();
            ctx.push(range.clone(), "break")
                .with_fix(Fix::safe("break", vec![Edit::new(range, *b"if")]));
        }
    }

    /// Inserts a space before every `if` statement, which it then reports again.
    struct NeverConverges;

    impl AstRule for NeverConverges {
        type Node = IfStatementSyntax;
        const CODE: ErrorCode = ErrorCode::new(Category::Idiom, 901);
        fn check(&self, node: &Self::Node, ctx: &mut AstRuleCtx<'_>) {
            let start = node.raw().text_range().start;
            ctx.push(start..start, "indent")
                .with_fix(Fix::safe("indent", vec![Edit::new(start..start, *b" ")]));
        }
    }

    #[test]
    fn syntax_errors_are_reported_as_err_and_rules_do_not_run() {
        let source = in_procedure("if (a) then end;");
        let result = parse_and_analyze_file(
            source.as_bytes(),
            FileId(3),
            &registry(NoParensAroundIf),
            &select_all(),
        );
        match result {
            AnalysisResult::SyntaxErrs(errors) => {
                assert!(!errors.is_empty());
                for error in &errors {
                    assert_eq!(error.code().to_string(), "SYX001");
                    assert_eq!(error.loc().file(), FileId(3));
                }
            }
            AnalysisResult::Lints(_) => panic!("Expect analysis errors"),
        }
    }

    #[test]
    fn a_rule_disabled_by_default_only_runs_when_selected() {
        let source = in_procedure("if (a) then end if;");
        let rules = registry(NoParensAroundIf);
        let analyze = |overwrites: &RuleOverwrites| match parse_and_analyze_file(
            source.as_bytes(),
            FileId(0),
            &rules,
            overwrites,
        ) {
            AnalysisResult::SyntaxErrs(_) => panic!("Unexpected syntax errors"),
            AnalysisResult::Lints(diagnostics) => diagnostics.len(),
        };
        assert_eq!(analyze(&RuleOverwrites::default()), 0);
        assert_eq!(analyze(&select_all()), 1);
        assert_eq!(
            analyze(&RuleOverwrites::new(
                vec![RuleSelector::All],
                vec![RuleSelector::Code(NoParensAroundIf::CODE)]
            )),
            0
        );
    }

    #[test]
    fn fixing_a_file_with_syntax_errors_does_nothing() {
        let result = fix(
            &in_procedure("if (a) then end;"),
            &registry(NoParensAroundIf),
        );
        assert!(matches!(result, FixResult::SyntaxErrs(errors) if !errors.is_empty()));
    }

    #[test]
    fn a_file_without_fixes_is_not_fixed() {
        let result = fix(
            &in_procedure("if a then end if;"),
            &registry(NoParensAroundIf),
        );
        assert!(matches!(result, FixResult::NotFixed { diagnostics } if diagnostics.is_empty()));
    }

    #[test]
    fn fixes_are_applied_until_a_fixpoint_is_reached() {
        // The inner parentheses are only reported once the outer ones are gone
        let source = in_procedure("if ((a)) then elsif (b) then end if;");
        let FixResult::Fixed {
            file,
            applied_fixes,
            diagnostics,
        } = fix(&source, &registry(NoParensAroundIf))
        else {
            panic!("expected the file to be fixed");
        };
        assert_eq!(
            str::from_utf8(&file).unwrap(),
            in_procedure("if a then elsif b then end if;")
        );
        assert_eq!(applied_fixes, 3);
        assert!(diagnostics.is_empty());
    }

    #[test]
    fn a_fix_that_breaks_the_syntax_is_reported() {
        let source = in_procedure("if a then end if;");
        let FixResult::ErrAfterFixing {
            diagnostics,
            partial_file,
        } = fix(&source, &registry(BreaksSyntax))
        else {
            panic!("expected a syntax error after fixing");
        };
        assert!(!diagnostics.is_empty());
        assert_eq!(str::from_utf8(&partial_file).unwrap(), in_procedure("if"));
    }

    #[test]
    fn fixes_that_never_converge_give_up() {
        let source = in_procedure("if a then end if;");
        let FixResult::TooManyTries {
            diagnostics,
            partial_file,
        } = fix(&source, &registry(NeverConverges))
        else {
            panic!("expected fixing to give up");
        };
        assert_eq!(diagnostics.len(), 1);
        assert_eq!(partial_file.len(), source.len() + MAX_TRIES);
    }
}
