pub mod config;
pub mod diagnostic;
pub mod error_code;
pub mod fix;
pub mod rule;
pub mod severity;
pub mod source_loc;

use std::{
    fmt::Display,
    path::{Path, PathBuf},
    sync::OnceLock,
};

use clap::ValueEnum;
use rayon::iter::{IndexedParallelIterator, IntoParallelRefIterator, ParallelIterator};
use vhdl_syntax::{
    fmt::{
        encoding::{Latin1Encoder, LossyUtf8Encoder},
        write::FormatToExt,
    },
    latin_1::Latin1Str,
    parser::{parse_valid_with_standard, parse_with_standard},
    standard::VHDLStandard,
    text::{char_encoding::Utf8, source_loc::SourceLocConverter},
};

use crate::{
    diagnostic::Diagnostic,
    fix::{apply_fixes, Fix},
    rule::{
        selection::{OverwriteResult, RuleOverrides},
        AstRuleCtx, ErasedAstRule, RuleRegistry,
    },
};

/// ID that points to a file on disk
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct FileId(u32);

/// How comments should be interpreted in text
#[derive(
    ValueEnum, Debug, Clone, Copy, Default, PartialEq, Eq, serde::Serialize, serde::Deserialize,
)]
pub enum Encoding {
    #[value(name = "latin-1")]
    #[serde(rename = "latin-1")]
    Latin1,
    #[default]
    #[value(name = "utf-8")]
    #[serde(rename = "utf-8")]
    Utf8,
}

impl Display for Encoding {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.to_possible_value().unwrap().get_name().fmt(f)
    }
}

#[derive(Debug, Copy, Clone, Default, PartialEq, Eq)]
pub struct FileSettings {
    pub standard: VHDLStandard,
    pub encoding: Encoding,
}

#[derive(Debug)]
pub struct File {
    path: PathBuf,
    contents: Vec<u8>,
    settings: FileSettings,
    rendered: OnceLock<(String, SourceLocConverter)>,
}

impl File {
    pub fn new(path: impl Into<PathBuf>, contents: Vec<u8>, settings: FileSettings) -> File {
        File {
            path: path.into(),
            contents,
            settings,
            rendered: OnceLock::new(),
        }
    }

    pub fn path(&self) -> &Path {
        &self.path
    }

    pub fn contents(&self) -> &[u8] {
        &self.contents
    }

    pub fn settings(&self) -> FileSettings {
        self.settings
    }

    pub fn set_contents(&mut self, contents: Vec<u8>) {
        self.rendered = OnceLock::new();
        self.contents = contents
    }

    fn rendered(&self) -> &(String, SourceLocConverter) {
        self.rendered.get_or_init(|| {
            let (design, _) = parse_with_standard(self.settings.standard, self.contents.as_slice());
            match self.settings.encoding {
                Encoding::Latin1 => (
                    // Optimization: Since Latin-1 is the text encoding, we can simply encode the full text.
                    // There is no need to go through the `display` trait which walks the tree.
                    Latin1Str::new(&self.contents).to_string(),
                    SourceLocConverter::new_lossy::<Latin1Encoder, Utf8>(&design),
                ),
                Encoding::Utf8 => (
                    design.display().to_string(),
                    SourceLocConverter::new_lossy::<LossyUtf8Encoder, Utf8>(&design),
                ),
            }
        })
    }

    pub fn source_mapping(&self) -> &SourceLocConverter {
        &self.rendered().1
    }

    pub fn utf8_contents(&self) -> &str {
        self.rendered().0.as_str()
    }
}

/// The contents of every file under analysis, addressed by [`FileId`].
#[derive(Debug, Default)]
pub struct FileStore {
    files: Vec<File>,
}

impl FileStore {
    pub fn new() -> FileStore {
        FileStore::default()
    }

    pub fn insert_file(&mut self, file: File) -> FileId {
        let len = self.files.len();
        self.files.push(file);
        FileId(len as u32)
    }

    pub fn insert(
        &mut self,
        path: impl Into<PathBuf>,
        file: Vec<u8>,
        settings: FileSettings,
    ) -> FileId {
        self.insert_file(File::new(path, file, settings))
    }

    pub fn get(&self, id: FileId) -> &File {
        &self.files[id.0 as usize]
    }

    /// Replace the contents of `id`, e.g. after fixes were written back to disk.
    pub fn set_contents(&mut self, id: FileId, contents: Vec<u8>) {
        self.files[id.0 as usize].set_contents(contents);
    }

    pub fn len(&self) -> usize {
        self.files.len()
    }

    pub fn is_empty(&self) -> bool {
        self.files.is_empty()
    }

    pub fn par_iter(&self) -> impl IndexedParallelIterator<Item = (FileId, &File)> {
        self.files
            .par_iter()
            .enumerate()
            .map(|(idx, file)| (FileId(idx as u32), file))
    }
}

fn is_rule_active(rule: &dyn ErasedAstRule, overrides: &RuleOverrides) -> bool {
    match overrides.get(rule.code()) {
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

fn analyze(
    contents: &[u8],
    settings: FileSettings,
    file_id: FileId,
    rules: &RuleRegistry,
    overrides: &RuleOverrides,
) -> AnalysisResult {
    match parse_valid_with_standard(settings.standard, contents) {
        Ok(design) => {
            let mut diagnostics = Vec::new();
            for node in design.descendants() {
                for rule in rules.for_kind(node.kind()) {
                    if !is_rule_active(rule, overrides) {
                        continue;
                    }
                    let mut ctx = AstRuleCtx::new(
                        &mut diagnostics,
                        rule.severity(),
                        file_id,
                        rule.code(),
                        settings,
                    );
                    rule.check_raw(node.clone(), &mut ctx);
                }
            }
            AnalysisResult::Lints(diagnostics)
        }
        Err(e) => AnalysisResult::SyntaxErrs(
            e.errors
                .into_iter()
                .map(|err| Diagnostic::from_syntax_err(err, file_id))
                .collect::<Vec<_>>(),
        ),
    }
}

/// Parse and perform single-file analysis.
// Multi-file analysis with proper dependency tracking etc needs a proper analyzer.
// This is designed to be the "escape hatch" when such analysis is not available, e.g.,
// due to missing library mapping.
pub fn parse_and_analyze_file(
    file: &File,
    file_id: FileId,
    rules: &RuleRegistry,
    overrides: &RuleOverrides,
) -> AnalysisResult {
    analyze(file.contents(), file.settings(), file_id, rules, overrides)
}

fn get_fixes(diagnostics: &[Diagnostic]) -> Vec<&Fix> {
    diagnostics
        .iter()
        .filter_map(|diag| diag.fix())
        .filter(|fix| fix.is_safe())
        .collect()
}

const MAX_TRIES: usize = 100;

/// The reason a fix did not yield a change
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnchangedReason {
    /// No fixes were applied because there are syntax errors
    SyntaxErrors,
    /// There are no fixes available to apply, only non-fixeable diagnostics or no diagnostics at all
    NoFixesAvailable,
}

/// The outcome of a fixed file
#[derive(Debug)]
pub enum FixOutcome {
    /// The file was left unchanged.
    /// Diagnostics associated with the file and the reason why it was left unchanged are passed.
    Unchanged {
        diagnostics: Vec<Diagnostic>,
        /// Why the file was not changed
        reason: UnchangedReason,
    },
    /// The file did change due to fixes
    Changed {
        /// Contents of the new file
        file: Box<[u8]>,
        /// Number of applied fixes
        applied_fixes: usize,
        /// Remaining (unfixeable) diagnostics
        diagnostics: Vec<Diagnostic>,
    },
}

/// How fixing a file might fail.
/// Note that all of these issues are likely bugs in a lint rule
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FixErrKind {
    /// Fixing did not yield a fixed point after too many iterations
    TooManyTries,
    /// Fixing a file yielded syntax errors
    ErrAfterFixing,
    /// No progress was made
    /// (the contents of a file was the same as the original after fixing)
    NoProgress,
}

/// Error returned when fixing a file failed
#[derive(Debug)]
pub struct FixErr {
    /// Remaining diagnostics or syntax errors
    pub diagnostics: Vec<Diagnostic>,
    /// The partial file that remained
    pub partial_file: Box<[u8]>,
    /// How fixing failed
    pub kind: FixErrKind,
}

pub fn fix_file(
    file: &File,
    file_id: FileId,
    rules: &RuleRegistry,
    overrides: &RuleOverrides,
) -> Result<FixOutcome, FixErr> {
    let mut diagnostics = match analyze(file.contents(), file.settings(), file_id, rules, overrides)
    {
        AnalysisResult::Lints(diagnostics) => diagnostics,
        AnalysisResult::SyntaxErrs(diagnostics) => {
            return Ok(FixOutcome::Unchanged {
                diagnostics,
                reason: UnchangedReason::SyntaxErrors,
            })
        }
    };

    let mut output_file: Vec<u8> = Vec::from(file.contents());

    let mut applied_fixes = 0usize;

    // Every pass applies as many fixes as do not conflict with each other and re-derives
    // the rest from the edited text, so it may take several passes to reach a fixpoint.
    for _ in 0..MAX_TRIES {
        let mut fixes = get_fixes(&diagnostics);
        if fixes.is_empty() {
            if output_file == file.contents() {
                return Ok(FixOutcome::Unchanged {
                    diagnostics,
                    reason: UnchangedReason::NoFixesAvailable,
                });
            } else {
                return Ok(FixOutcome::Changed {
                    file: output_file.into_boxed_slice(),
                    applied_fixes,
                    diagnostics,
                });
            }
        }
        fixes.sort_by_key(|fix| (fix.extent().start, fix.extent().end));
        let (next, number_fixes) = apply_fixes(&output_file, &fixes);
        applied_fixes += number_fixes;
        if next == output_file {
            return Err(FixErr {
                diagnostics,
                partial_file: output_file.into_boxed_slice(),
                kind: FixErrKind::NoProgress,
            });
        }
        output_file = next;
        diagnostics = match analyze(&output_file, file.settings(), file_id, rules, overrides) {
            AnalysisResult::Lints(diagnostics) => diagnostics,
            AnalysisResult::SyntaxErrs(diagnostics) => {
                return Err(FixErr {
                    diagnostics,
                    partial_file: output_file.into_boxed_slice(),
                    kind: FixErrKind::ErrAfterFixing,
                })
            }
        };
    }

    Err(FixErr {
        diagnostics,
        partial_file: output_file.into_boxed_slice(),
        kind: FixErrKind::TooManyTries,
    })
}

#[cfg(test)]
mod tests {
    use vhdl_syntax::syntax::{validate::valid_node::Valid, AstNode, IfStatementSyntax};

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

    fn select_all() -> RuleOverrides {
        RuleOverrides::new(vec![RuleSelector::All], vec![])
    }

    fn fix(source: &str, rules: &RuleRegistry) -> Result<FixOutcome, FixErr> {
        fix_file(&file(source), FileId(0), rules, &select_all())
    }

    fn file(source: &str) -> File {
        File::new(
            "<inline>",
            source.as_bytes().to_vec(),
            FileSettings::default(),
        )
    }

    /// Replaces every `if` statement with text that does not parse.
    struct BreaksSyntax;

    impl AstRule for BreaksSyntax {
        type Node = IfStatementSyntax;
        const CODE: ErrorCode = ErrorCode::new(Category::Idiom, 900);
        fn check(&self, node: &Valid<Self::Node>, ctx: &mut AstRuleCtx<'_>) {
            let range = node.raw().text_range();
            ctx.push(range.clone(), "break")
                .with_fix(Fix::safe("break", vec![Edit::new(range, b"if")]));
        }
    }

    /// Inserts a space before every `if` statement, which it then reports again.
    struct NeverConverges;

    impl AstRule for NeverConverges {
        type Node = IfStatementSyntax;
        const CODE: ErrorCode = ErrorCode::new(Category::Idiom, 901);
        fn check(&self, node: &Valid<Self::Node>, ctx: &mut AstRuleCtx<'_>) {
            let start = node.raw().text_range().start;
            ctx.push(start..start, "indent")
                .with_fix(Fix::safe("indent", vec![Edit::new(start..start, b" ")]));
        }
    }

    /// Replaces the `if` keyword of every `if` statement with itself.
    struct ChangesNothing;

    impl AstRule for ChangesNothing {
        type Node = IfStatementSyntax;
        const CODE: ErrorCode = ErrorCode::new(Category::Idiom, 902);
        fn check(&self, node: &Valid<Self::Node>, ctx: &mut AstRuleCtx<'_>) {
            let start = node.raw().text_range().start;
            let range = start..start + b"if".len();
            ctx.push(range.clone(), "no-op")
                .with_fix(Fix::safe("no-op", vec![Edit::new(range, b"if")]));
        }
    }

    #[test]
    fn syntax_errors_are_reported_as_err_and_rules_do_not_run() {
        let source = in_procedure("if (a) then end;");
        let result = parse_and_analyze_file(
            &file(&source),
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
        let source = file(&in_procedure("if (a) then end if;"));
        let rules = registry(NoParensAroundIf);
        let analyze = |overrides: &RuleOverrides| match parse_and_analyze_file(
            &source,
            FileId(0),
            &rules,
            overrides,
        ) {
            AnalysisResult::SyntaxErrs(_) => panic!("Unexpected syntax errors"),
            AnalysisResult::Lints(diagnostics) => diagnostics.len(),
        };
        assert_eq!(analyze(&RuleOverrides::default()), 0);
        assert_eq!(analyze(&select_all()), 1);
        assert_eq!(
            analyze(&RuleOverrides::new(
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
        assert!(matches!(
            result,
            Ok(FixOutcome::Unchanged {
                diagnostics,
                reason: UnchangedReason::SyntaxErrors,
            }) if !diagnostics.is_empty()
        ));
    }

    #[test]
    fn a_file_without_fixes_is_not_fixed() {
        let result = fix(
            &in_procedure("if a then end if;"),
            &registry(NoParensAroundIf),
        );
        assert!(matches!(
            result,
            Ok(FixOutcome::Unchanged {
                diagnostics,
                reason: UnchangedReason::NoFixesAvailable,
            }) if diagnostics.is_empty()
        ));
    }

    #[test]
    fn fixes_are_applied_until_a_fixpoint_is_reached() {
        // The inner parentheses are only reported once the outer ones are gone
        let source = in_procedure("if ((a)) then elsif (b) then end if;");
        let Ok(FixOutcome::Changed {
            file,
            applied_fixes,
            diagnostics,
        }) = fix(&source, &registry(NoParensAroundIf))
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
        let Err(FixErr {
            diagnostics,
            partial_file,
            kind: FixErrKind::ErrAfterFixing,
        }) = fix(&source, &registry(BreaksSyntax))
        else {
            panic!("expected a syntax error after fixing");
        };
        assert!(!diagnostics.is_empty());
        assert_eq!(str::from_utf8(&partial_file).unwrap(), in_procedure("if"));
    }

    #[test]
    fn fixes_that_never_converge_give_up() {
        let source = in_procedure("if a then end if;");
        let Err(FixErr {
            diagnostics,
            partial_file,
            kind: FixErrKind::TooManyTries,
        }) = fix(&source, &registry(NeverConverges))
        else {
            panic!("expected fixing to give up");
        };
        assert_eq!(diagnostics.len(), 1);
        assert_eq!(partial_file.len(), source.len() + MAX_TRIES);
    }

    #[test]
    fn fixes_that_change_nothing_are_reported() {
        let source = in_procedure("if a then end if;");
        let Err(FixErr {
            diagnostics,
            partial_file,
            kind: FixErrKind::NoProgress,
        }) = fix(&source, &registry(ChangesNothing))
        else {
            panic!("expected fixing to make no progress");
        };
        assert_eq!(diagnostics.len(), 1);
        assert_eq!(str::from_utf8(&partial_file).unwrap(), source);
    }

    fn encoded_file(contents: &[u8], encoding: Encoding) -> File {
        File::new(
            "<inline>",
            contents.to_vec(),
            FileSettings {
                encoding,
                ..FileSettings::default()
            },
        )
    }

    #[test]
    fn utf8_comments_are_displayed_as_utf8() {
        let contents = "-- ä€💣\nentity e is end;";
        let file = encoded_file(contents.as_bytes(), Encoding::Utf8);
        assert_eq!(file.utf8_contents(), contents);
    }

    #[test]
    fn latin1_comments_are_displayed_as_latin1() {
        let file = encoded_file(b"-- \xE4\xF6\xFC\nentity e is end;", Encoding::Latin1);
        assert_eq!(file.utf8_contents(), "-- äöü\nentity e is end;");
    }

    #[test]
    fn utf8_comments_read_as_latin1_are_decoded_byte_by_byte() {
        let file = encoded_file("-- ä\nentity e is end;".as_bytes(), Encoding::Latin1);
        assert_eq!(file.utf8_contents(), "-- Ã¤\nentity e is end;");
    }

    #[test]
    fn invalid_utf8_in_comments_is_replaced() {
        let file = encoded_file(b"-- \xE4\nentity e is end;", Encoding::Utf8);
        assert_eq!(file.utf8_contents(), "-- \u{FFFD}\nentity e is end;");
    }

    #[test]
    fn literals_are_latin1_regardless_of_the_comment_encoding() {
        let source = b"constant c : string := \"\xE4\"; -- \xE4";
        let utf8 = encoded_file(source, Encoding::Utf8);
        assert_eq!(
            utf8.utf8_contents(),
            "constant c : string := \"ä\"; -- \u{FFFD}"
        );
        let latin1 = encoded_file(source, Encoding::Latin1);
        assert_eq!(latin1.utf8_contents(), "constant c : string := \"ä\"; -- ä");
    }

    #[test]
    fn byte_offsets_are_mapped_into_the_displayed_text() {
        let cases: [(&[u8], Encoding, usize); _] = [
            // 'ä' is one byte in Latin-1 and two bytes in UTF-8
            (b"-- \xE4\nentity e is end;", Encoding::Latin1, 1),
            // Already UTF-8, so nothing moves
            ("-- ä\nentity e is end;".as_bytes(), Encoding::Utf8, 0),
            // Each of the two UTF-8 bytes is one Latin-1 character, which is two UTF-8 bytes
            ("-- ä\nentity e is end;".as_bytes(), Encoding::Latin1, 2),
            // The invalid byte is replaced by U+FFFD, which is three bytes wide
            (b"-- \xE4\nentity e is end;", Encoding::Utf8, 2),
        ];
        for (source, encoding, shift) in cases {
            let file = encoded_file(source, encoding);
            let start = source.len() - b"entity e is end;".len();
            let span = file
                .source_mapping()
                .convert_byte_span(&(start..start + b"entity".len()));
            assert_eq!(
                (span.start().raw(), span.end().raw()),
                (start + shift, start + shift + b"entity".len()),
                "{encoding}: {:?}",
                file.utf8_contents()
            );
            assert_eq!(
                &file.utf8_contents()[span.start().raw()..span.end().raw()],
                "entity"
            );
        }
    }

    #[test]
    fn replacing_the_contents_updates_the_displayed_text() {
        let mut file = encoded_file(b"-- \xE4\nentity e is end;", Encoding::Latin1);
        assert_eq!(file.utf8_contents(), "-- ä\nentity e is end;");
        file.set_contents(b"-- \xF6\nentity e is end;".to_vec());
        assert_eq!(file.utf8_contents(), "-- ö\nentity e is end;");
        let span = file.source_mapping().convert_byte_span(&(5..11));
        assert_eq!(
            &file.utf8_contents()[span.start().raw()..span.end().raw()],
            "entity"
        );
    }

    #[test]
    fn fixing_preserves_non_ascii_bytes_as_they_are_on_disk() {
        // Fixes are applied to the raw bytes, so neither a Latin-1 comment in UTF-8 mode
        // nor a UTF-8 comment in Latin-1 mode is re-encoded when writing the file back.
        let sources: [&[u8]; 2] = [b"-- \xE4 \xFF", "-- ä€💣".as_bytes()];
        for encoding in [Encoding::Utf8, Encoding::Latin1] {
            for comment in sources {
                let procedure = |condition: &str| {
                    let mut source =
                        in_procedure(&format!("if {condition} then end if; ")).into_bytes();
                    source.splice(0..0, comment.iter().copied().chain(*b"\n"));
                    source
                };
                let Ok(FixOutcome::Changed { file, .. }) = fix_file(
                    &encoded_file(&procedure("(a)"), encoding),
                    FileId(0),
                    &registry(NoParensAroundIf),
                    &select_all(),
                ) else {
                    panic!("expected the file to be fixed");
                };
                assert_eq!(*file, *procedure("a"), "{encoding}");
            }
        }
    }
}
