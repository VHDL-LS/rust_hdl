use annotate_snippets::{renderer::DecorStyle, Group, Level, Renderer};
use clap::Parser;
use ignore::{
    overrides::{Override, OverrideBuilder},
    types::TypesBuilder,
    ParallelVisitor, ParallelVisitorBuilder, WalkBuilder, WalkState,
};
use itertools::Itertools;
use rayon::iter::ParallelIterator;
use std::{
    collections::HashMap,
    env::current_dir,
    fs,
    io::{self, Write},
    path::{Path, PathBuf},
    process::ExitCode,
    sync::Mutex,
};
use vhdl_lint::{
    config::{Config, ConfigFile, Layer, CONFIG_NAME},
    diagnostic::{render_diagnostics, Diagnostic},
    error_code::ErrorCode,
    fix::Applicability,
    fix_file, parse_and_analyze_file,
    rule::{
        register_builtin_rules,
        selection::{RuleOverrides, RuleSelector},
        ErasedAstRule, RuleRegistry,
    },
    serialize::RenderableDiagnostic,
    Encoding, File, FileId, FileStore, FixErrKind, FixOutcome,
};
use vhdl_syntax::standard::VHDLStandard;

/// Whether `path`, or any directory it lies in, matches an exclusion.
fn is_excluded(overrides: &Override, cwd: &Path, path: &Path) -> bool {
    let is_dir = path.is_dir();
    path.strip_prefix(cwd)
        .unwrap_or(path)
        .ancestors()
        .take_while(|ancestor| !ancestor.as_os_str().is_empty() && *ancestor != Path::new("."))
        .enumerate()
        .any(|(depth, ancestor)| overrides.matched(ancestor, depth > 0 || is_dir).is_ignore())
}

fn resolve(args: &Args) -> Result<WalkBuilder, ignore::Error> {
    // TODO: deduplicate. Will need to evaluate various options
    let cwd = current_dir()?;
    let mut ov = OverrideBuilder::new(&cwd);
    for pat in &args.file_selection.exclude {
        ov.add(&format!("!{pat}"))?;
    }
    let overrides = ov.build()?;
    // `ignore` never filters the paths a walk starts from, so a path given on the
    // command line would otherwise be linted even when an exclusion matches it.
    let roots = args
        .files
        .iter()
        .filter(|path| !is_excluded(&overrides, &cwd, path));
    let mut builder = WalkBuilder::from_iter(roots);
    let types = TypesBuilder::new().add_defaults().select("vhdl").build()?;
    builder.overrides(overrides).types(types);
    if args.file_selection.no_respect_gitignore {
        builder.standard_filters(false);
    }
    Ok(builder)
}

#[derive(clap::ValueEnum, Debug, Clone, Copy, PartialEq, Eq, Default)]
enum OutputFormat {
    #[default]
    Full,
    Json,
}

#[derive(clap::Args)]
#[command(next_help_heading = "File selection")]
struct FileSelection {
    /// Patterns to exclude from analysis
    #[clap(long, value_name = "FILE PATTERN")]
    exclude: Vec<String>,

    /// Disable respecting file exclusions via `.gitignore` and other standard ignore files.
    #[clap(long)]
    no_respect_gitignore: bool,
}

#[derive(clap::Args)]
#[command(next_help_heading = "Rule selection")]
struct RuleSelection {
    /// Comma-separated list of rules to select
    #[clap(long, value_delimiter = ',', value_name = "RULE")]
    select: Vec<RuleSelector>,

    /// Comma-separated list of rules to disable
    #[clap(long, value_delimiter = ',', value_name = "RULE")]
    ignore: Vec<RuleSelector>,
}

impl RuleSelection {
    pub fn overrides(&self) -> RuleOverrides {
        RuleOverrides::new(self.select.clone(), self.ignore.clone())
    }

    fn selectors(&self) -> impl Iterator<Item = &RuleSelector> {
        self.select.iter().chain(self.ignore.iter())
    }
}

#[derive(clap::Parser)]
#[command(version, about, author, long_about = None)]
struct Args {
    /// Files or directories to check
    #[clap(default_value = ".")]
    files: Vec<PathBuf>,

    /// Select the VHDL standard under which the file should be parsed and linted. Default is VHDL-2008
    #[arg(long)]
    std: Option<VHDLStandard>,

    /// Encoding used to read comments. Default is UTF-8
    #[arg(long)]
    encoding: Option<Encoding>,

    /// Path to the config
    #[arg(long)]
    config: Option<PathBuf>,

    /// Output serialization format for violations
    #[arg(long, value_enum, default_value_t)]
    output_format: OutputFormat,

    /// Apply fixes to resolve lint violations
    #[arg(long)]
    fix: bool,

    /// Include fixes that may not retain the original intent of the code or remove comments.
    /// Disable with `--no-unsafe-fixes`
    #[arg(long, overrides_with = "no_unsafe_fixes")]
    unsafe_fixes: bool,

    #[arg(long, overrides_with = "unsafe_fixes", hide = true)]
    no_unsafe_fixes: bool,

    #[clap(flatten)]
    file_selection: FileSelection,

    #[clap(flatten)]
    rule_selection: RuleSelection,

    /// Exit with status code "0", even upon detecting lint violations
    #[arg(short, long, help_heading = "Miscellaneous")]
    exit_zero: bool,

    /// Print the documentation of a rule and exit
    #[arg(long, value_name = "CODE", help_heading = "Miscellaneous")]
    explain: Option<ErrorCode>,
}

impl Args {
    pub fn layer(&self) -> Layer {
        Layer {
            standard: self.std,
            encoding: self.encoding,
            unsafe_fixes: match (self.unsafe_fixes, self.no_unsafe_fixes) {
                (true, _) => Some(true),
                (_, true) => Some(false),
                _ => None,
            },
            rules: self.rule_selection.overrides(),
        }
    }
}

#[derive(Debug, Default, PartialEq, Eq)]
struct Fixable {
    safe_fixes: usize,
    unsafe_fixes: usize,
}

fn fixable(diagnostics: &[Diagnostic], files: &FileStore) -> Fixable {
    let mut fixable = Fixable::default();
    for diag in diagnostics {
        let Some(fix) = diag.fix() else { continue };
        let unsafe_fixes = files.get(diag.loc().file()).settings().unsafe_fixes;
        match fix.applicability_with_unsafe_fixes(unsafe_fixes) {
            Applicability::Safe => fixable.safe_fixes += 1,
            Applicability::Unsafe => fixable.unsafe_fixes += 1,
            _ => {}
        }
    }
    fixable
}

fn fixable_summary(fixable: &Fixable, fix: bool) -> Option<String> {
    let hidden = if fixable.unsafe_fixes > 0 {
        Some(format!(
            "{} unsafe fix{} can be enabled with the `--unsafe-fixes` option",
            fixable.unsafe_fixes,
            if fixable.unsafe_fixes == 1 { "" } else { "es" }
        ))
    } else {
        None
    };
    let applicable = if !fix && fixable.safe_fixes > 0 {
        Some(format!(
            "{} issue{} fixable with the `--fix` option",
            fixable.safe_fixes,
            if fixable.safe_fixes == 1 {
                " is"
            } else {
                "s are"
            }
        ))
    } else {
        None
    };
    match (applicable, hidden) {
        (Some(applicable), Some(hidden)) => Some(format!("{applicable} ({hidden})")),
        (Some(applicable), None) => Some(applicable),
        (None, Some(hidden)) => Some(format!("No fixes available ({hidden})")),
        (None, None) => None,
    }
}

fn pluralize(value: usize) -> &'static str {
    if value == 1 {
        ""
    } else {
        "s"
    }
}

/// Atomically save a file by writing it to a tempfile and then persisting the tempfile
fn save_file_atomically(file: &Path, buf: &[u8]) -> io::Result<()> {
    let file = fs::canonicalize(file)?;
    let permissions = fs::metadata(&file)?.permissions();
    let dir = file.parent().unwrap_or(Path::new("."));
    let mut tmp = tempfile::NamedTempFile::new_in(dir)?;
    tmp.write_all(buf)?;
    tmp.as_file().set_permissions(permissions)?;
    tmp.as_file().sync_all()?;
    tmp.persist(file)?;
    Ok(())
}

fn load_config(args: &Args, cwd: &Path, registry: &RuleRegistry) -> Result<Config, String> {
    // The path of the config file alongside its directory and contents
    let found = match &args.config {
        Some(path) => Some((
            path.clone(),
            ConfigFile::from_file(path).map_err(|e| e.to_string())?,
        )),
        None => ConfigFile::resolve(cwd)
            .map_err(|e| e.to_string())?
            .map(|(dir, file)| (dir.join(CONFIG_NAME), (dir, file))),
    };
    let (dir, file) = match found {
        Some((path, (dir, file))) => {
            check_codes_exist(registry, file.selectors())
                .map_err(|e| format!("{}: {e}", path.display()))?;
            (dir, file)
        }
        None => (cwd.to_owned(), ConfigFile::default()),
    };
    file.into_config(dir, args.layer())
        .map_err(|e| format!("invalid config: {e}"))
}

#[derive(Default)]
struct Walk {
    files: FileStore,
    /// The canonical path of every file
    canonical: HashMap<FileId, PathBuf>,
    skipped: Vec<String>,
}

type Out = Result<Walk, ignore::Error>;

struct CollectorBuilder<'a, 's> {
    out: &'s Mutex<Out>,
    config: &'a Config,
}

struct Collector<'a, 's> {
    files: Vec<(File, PathBuf)>,
    skipped: Vec<String>,
    err: Option<ignore::Error>,
    config: &'a Config,
    out: &'s Mutex<Out>,
}

impl Drop for Collector<'_, '_> {
    fn drop(&mut self) {
        let mut out = self.out.lock().unwrap_or_else(|e| e.into_inner());
        if let Some(err) = self.err.take() {
            *out = Err(err);
            return;
        }
        if let Ok(walk) = out.as_mut() {
            for (file, canonical) in std::mem::take(&mut self.files) {
                let id = walk.files.insert_file(file);
                walk.canonical.insert(id, canonical);
            }
            walk.skipped.append(&mut self.skipped);
        }
    }
}

impl<'a, 's> ParallelVisitorBuilder<'s> for CollectorBuilder<'a, 's>
where
    'a: 's,
{
    fn build(&mut self) -> Box<dyn ParallelVisitor + 's> {
        Box::new(Collector {
            files: Vec::new(),
            skipped: Vec::new(),
            err: None,
            out: self.out,
            config: self.config,
        })
    }
}

impl<'a, 's> ParallelVisitor for Collector<'a, 's>
where
    'a: 's,
{
    fn visit(&mut self, entry: Result<ignore::DirEntry, ignore::Error>) -> ignore::WalkState {
        match entry {
            Ok(entry) => {
                if entry.file_type().is_some_and(|typ| typ.is_file()) {
                    let path = entry.path();
                    let read = fs::canonicalize(path)
                        .and_then(|canonical| Ok((fs::read(path)?, canonical)));
                    match read {
                        Ok((data, canonical)) => {
                            let settings = self.config.settings(&canonical);
                            self.files
                                .push((File::new(path, data, settings), canonical));
                        }
                        Err(e) => self
                            .skipped
                            .push(format!("could not read {}: {e}", path.display())),
                    }
                }
            }
            Err(e) if e.depth() == Some(0) => {
                self.err = Some(e);
                return WalkState::Quit;
            }
            Err(e) => self.skipped.push(e.to_string()),
        }
        WalkState::Continue
    }
}

/// The analyzed sources have diagnostics.
const EXIT_DIAGNOSTICS: u8 = 1;
/// The tool could not do its job (bad arguments, unreadable paths).
const EXIT_TOOL_FAILURE: u8 = 2;

fn main() -> ExitCode {
    let args = Args::parse();

    let cwd = match current_dir() {
        Ok(cwd) => cwd,
        Err(e) => {
            anstream::eprintln!("Cannot get cwd: {e}");
            return ExitCode::from(EXIT_TOOL_FAILURE);
        }
    };

    let mut registry = RuleRegistry::new();
    register_builtin_rules(&mut registry);

    if let Some(code) = args.explain {
        return match registry.by_code(&code) {
            Some(rule) => {
                anstream::println!("{}", explain(rule));
                ExitCode::SUCCESS
            }
            None => {
                anstream::eprintln!("error: '{code}' does not exist");
                ExitCode::from(EXIT_TOOL_FAILURE)
            }
        };
    }

    if let Err(e) = check_codes_exist(&registry, args.rule_selection.selectors()) {
        anstream::eprintln!("error: {e}");
        return ExitCode::from(EXIT_TOOL_FAILURE);
    }

    let config = match load_config(&args, &cwd, &registry) {
        Ok(config) => config,
        Err(e) => {
            anstream::eprintln!("Cannot load config: {e}");
            return ExitCode::from(EXIT_TOOL_FAILURE);
        }
    };

    let builder = match resolve(&args) {
        Ok(builder) => builder,
        Err(e) => {
            anstream::eprintln!("error: {e}");
            return ExitCode::from(EXIT_TOOL_FAILURE);
        }
    };
    let out = Mutex::new(Ok(Walk::default()));
    let mut collector = CollectorBuilder {
        config: &config,
        out: &out,
    };
    builder.build_parallel().visit(&mut collector);
    let (mut files, canonical, mut skipped) = match out.into_inner().unwrap() {
        Ok(Walk {
            files,
            canonical,
            skipped,
        }) => (files, canonical, skipped),
        Err(e) => {
            anstream::eprintln!("error: {e}");
            return ExitCode::from(EXIT_TOOL_FAILURE);
        }
    };

    let mut total_fixes = 0usize;
    let mut errors = if args.fix {
        let outcomes = files
            .par_iter()
            .map(|(file_id, file)| {
                let rules = registry.get_active_rules(&config, &canonical[&file_id]);
                let outcome = fix_file(file, file_id, &rules);
                (file_id, outcome)
            })
            .collect::<Vec<_>>();

        let mut errors = Vec::new();
        for (file_id, outcome) in outcomes {
            let rules = registry.get_active_rules(&config, &canonical[&file_id]);
            match outcome {
                Ok(FixOutcome::Unchanged { diagnostics, .. }) => errors.extend(diagnostics),
                Ok(FixOutcome::Changed {
                    file,
                    applied_fixes,
                    mut diagnostics,
                }) => {
                    match save_file_atomically(files.get(file_id).path(), &file) {
                        Ok(()) => {
                            total_fixes += applied_fixes;
                            // The remaining diagnostics refer to the fixed text
                            files.set_contents(file_id, file.into_vec());
                        }
                        Err(e) => {
                            skipped.push(format!(
                                "could not write {}: {e}",
                                files.get(file_id).path().display()
                            ));
                            // The fixed text never reached the disk, so report against the original
                            diagnostics =
                                parse_and_analyze_file(files.get(file_id), file_id, &rules)
                                    .into_diagnostics();
                        }
                    }
                    errors.extend(diagnostics);
                }
                Err(e) => {
                    let msg = match e.kind {
                        FixErrKind::ErrAfterFixing => format!(
                            "applying fixes to {} introduced syntax errors ({}); the file was left unchanged. This is a bug in a lint rule.",
                            files.get(file_id).path().display(),
                            e.diagnostics.iter().map(|diag| diag.message()).join("; ")),
                        FixErrKind::TooManyTries => format!(
                            "fixes for {} did not converge; the file was left unchanged. This is likely a bug in a lint rule.",
                            files.get(file_id).path().display()
                        ),
                        FixErrKind::NoProgress => format!(
                            "fixes for {} made no progress; the file was left unchanged. This is a bug in a lint rule.",
                            files.get(file_id).path().display()
                        )
                    };
                    skipped.push(msg);
                    // re-report the old diagnostics
                    let old_diagnostics =
                        parse_and_analyze_file(files.get(file_id), file_id, &rules)
                            .into_diagnostics();
                    errors.extend(old_diagnostics);
                }
            }
        }
        errors
    } else {
        files
            .par_iter()
            .flat_map(|(file_id, file)| {
                let rules = registry.get_active_rules(&config, &canonical[&file_id]);
                parse_and_analyze_file(file, file_id, &rules).into_diagnostics()
            })
            .collect::<Vec<_>>()
    };

    let renderer = Renderer::styled().decor_style(DecorStyle::Unicode);

    errors.sort_by_key(|diag| {
        let loc = diag.loc();
        (
            files.get(loc.file()).path(),
            loc.span().start,
            loc.span().end,
        )
    });

    match args.output_format {
        OutputFormat::Full => {
            if !errors.is_empty() {
                let report = render_diagnostics(&errors, &files).collect::<Vec<_>>();
                anstream::println!("{}", renderer.render(&report));
            }
        }
        OutputFormat::Json => {
            let diagnostics = errors
                .iter()
                .map(|diag| RenderableDiagnostic::from_diagnostic(diag, &files))
                .collect::<Box<_>>();
            anstream::println!("{}", serde_json::to_string(&diagnostics).unwrap())
        }
    }

    if !skipped.is_empty() {
        skipped.sort();
        let rendered = skipped
            .iter()
            .map(|msg| Group::with_title(Level::ERROR.primary_title(msg)))
            .collect::<Box<[_]>>();
        anstream::eprintln!("{}", renderer.render(&rendered));
    }

    if args.output_format == OutputFormat::Full {
        if total_fixes > 0 {
            anstream::println!("Fixed {total_fixes} issue{}", pluralize(total_fixes));
        }
        if let Some(summary) = fixable_summary(&fixable(&errors, &files), args.fix) {
            anstream::println!("{summary}");
        }
    }

    if !skipped.is_empty() {
        ExitCode::from(EXIT_TOOL_FAILURE)
    } else if !errors.is_empty() {
        if args.exit_zero {
            ExitCode::SUCCESS
        } else {
            ExitCode::from(EXIT_DIAGNOSTICS)
        }
    } else {
        if args.output_format == OutputFormat::Full {
            let preamble = if total_fixes > 0 {
                "No new issues"
            } else {
                "No issues"
            };
            anstream::println!(
                "{preamble} found in {} file{}",
                files.len(),
                pluralize(files.len())
            );
        }
        ExitCode::SUCCESS
    }
}

fn explain(rule: &dyn ErasedAstRule) -> String {
    format!(
        "# {} ({})\n\nSeverity: {}\nEnabled by default: {}\n\n{}",
        rule.code(),
        rule.docs().name,
        rule.severity(),
        if rule.is_enabled_by_default() {
            "yes"
        } else {
            "no"
        },
        rule.docs().text
    )
}

/// Every code among `selectors` that no registered rule uses, in the order the
/// user spelled them and without repeats.
fn unknown_codes<'a>(
    registry: &RuleRegistry,
    selectors: impl Iterator<Item = &'a RuleSelector>,
) -> Vec<ErrorCode> {
    selectors
        .filter_map(|selector| match selector {
            RuleSelector::All | RuleSelector::Category(_) => None,
            RuleSelector::Code(code) => registry.by_code(code).is_none().then_some(*code),
        })
        .unique()
        .collect()
}

fn check_codes_exist<'a>(
    registry: &RuleRegistry,
    selectors: impl Iterator<Item = &'a RuleSelector>,
) -> Result<(), String> {
    let unknown = unknown_codes(registry, selectors);
    if unknown.is_empty() {
        return Ok(());
    }
    let codes = unknown.iter().map(|code| format!("'{code}'")).join(", ");
    let verb = if unknown.len() == 1 { "does" } else { "do" };
    Err(format!("{codes} {verb} not exist"))
}

#[cfg(test)]
mod tests {
    use super::*;
    use vhdl_lint::{
        rule::{no_parens_around_if::NoParensAroundIf, AstRule},
        FileSettings,
    };

    #[test]
    fn only_a_fix_that_fix_applies_counts_as_fixable() {
        use vhdl_lint::{
            error_code::Category,
            fix::{edit::Edit, Fix},
            severity::Severity,
            source_loc::SourceLoc,
        };
        let mut file_store = FileStore::new();
        let id = file_store.insert(Path::new("inline"), vec![], FileSettings::default());
        let unsafe_id = file_store.insert(
            Path::new("unsafe"),
            vec![],
            FileSettings {
                unsafe_fixes: true,
                ..FileSettings::default()
            },
        );

        let diagnostic_in = |id, fix: Option<Fix>| {
            let mut diagnostic = Diagnostic::new(
                "message",
                Severity::Warning,
                SourceLoc::new(id, 0..1),
                ErrorCode::new(Category::Idiom, 1),
            );
            if let Some(fix) = fix {
                diagnostic.set_fix(fix);
            }
            diagnostic
        };
        let diagnostic = |fix| diagnostic_in(id, fix);
        let edits = || vec![Edit::delete_raw(0..1)];

        assert_eq!(
            fixable(
                &[
                    diagnostic(Some(Fix::safe_edits("safe", edits()))),
                    diagnostic(Some(Fix::unsafe_edits("unsafe", edits()))),
                    diagnostic(Some(Fix::display_only_edits("display only", edits()))),
                    diagnostic(None),
                    diagnostic_in(unsafe_id, Some(Fix::unsafe_edits("unsafe", edits()))),
                    diagnostic_in(
                        unsafe_id,
                        Some(Fix::display_only_edits("display only", edits()))
                    ),
                ],
                &file_store
            ),
            Fixable {
                safe_fixes: 2,
                unsafe_fixes: 1
            }
        );
    }

    #[test]
    fn the_fixable_summary_mentions_hidden_unsafe_fixes() {
        let summary = |safe_fixes, unsafe_fixes, fix| {
            fixable_summary(
                &Fixable {
                    safe_fixes,
                    unsafe_fixes,
                },
                fix,
            )
        };

        assert_eq!(summary(0, 0, false), None);
        assert_eq!(
            summary(2, 0, false).as_deref(),
            Some("2 issues are fixable with the `--fix` option")
        );
        assert_eq!(
            summary(1, 1, false).as_deref(),
            Some(
                "1 issue is fixable with the `--fix` option \
                 (1 unsafe fix can be enabled with the `--unsafe-fixes` option)"
            )
        );
        assert_eq!(
            summary(0, 2, false).as_deref(),
            Some("No fixes available (2 unsafe fixes can be enabled with the `--unsafe-fixes` option)")
        );
        // After `--fix`, only the hidden fixes are worth mentioning
        assert_eq!(summary(3, 0, true), None);
        assert_eq!(
            summary(3, 1, true).as_deref(),
            Some(
                "No fixes available (1 unsafe fix can be enabled with the `--unsafe-fixes` option)"
            )
        );
    }

    #[test]
    fn explain_heads_the_documentation_with_the_rule_and_its_defaults() {
        let explained = explain(&NoParensAroundIf);
        assert!(
            explained.starts_with(
                "# IDM001 (no-parens-around-if)\n\n\
                 Severity: warning\n\
                 Enabled by default: no\n\n\
                 Checks that the conditions of an `if` statement have no parenthesis."
            ),
            "{explained}"
        );
    }

    #[test]
    fn check_codes_exist_reports_only_unregistered_codes() {
        let mut registry = RuleRegistry::new();
        registry.register(NoParensAroundIf).unwrap();
        let selectors = |selectors: &[&str]| -> Vec<RuleSelector> {
            selectors.iter().map(|s| s.parse().unwrap()).collect()
        };

        let registered = NoParensAroundIf::CODE.to_string();
        assert_eq!(
            check_codes_exist(&registry, selectors(&["ALL", "IDM", &registered]).iter()),
            Ok(())
        );
        assert_eq!(
            check_codes_exist(&registry, selectors(&["IDM999"]).iter()),
            Err("'IDM999' does not exist".to_owned())
        );
        assert_eq!(
            check_codes_exist(&registry, selectors(&["IDM998", "IDM999"]).iter()),
            Err("'IDM998', 'IDM999' do not exist".to_owned())
        );
    }

    #[test]
    fn load_config_rejects_unknown_codes_in_the_config_file() {
        let dir = std::env::temp_dir().join(format!("vhdl-lint-test-{}", std::process::id()));
        fs::create_dir_all(&dir).unwrap();
        let path = dir.join(CONFIG_NAME);
        fs::write(
            &path,
            "[[overrides]]\nfiles = [\"a.vhd\"]\nignore = [\"IDM999\"]",
        )
        .unwrap();
        let registry = RuleRegistry::new();
        let args = Args::parse_from(["vhdl-lint", "--config", path.to_str().unwrap()]);

        let err = load_config(&args, &dir, &registry).unwrap_err();
        fs::remove_dir_all(&dir).unwrap();
        assert_eq!(err, format!("{}: 'IDM999' does not exist", path.display()));
    }

    #[test]
    fn the_cli_overrides_unsafe_fixes_from_the_config_in_both_directions() {
        let dir = tempfile::tempdir().unwrap();
        let unsafe_fixes = |config: &str, flags: &[&str]| {
            let path = dir.path().join(CONFIG_NAME);
            fs::write(&path, config).unwrap();
            let args = Args::parse_from(
                ["vhdl-lint", "--config", path.to_str().unwrap()]
                    .iter()
                    .chain(flags),
            );
            let config = load_config(&args, dir.path(), &RuleRegistry::new()).unwrap();
            config.settings(&dir.path().join("a.vhd")).unsafe_fixes
        };

        assert!(!unsafe_fixes("", &[]));
        assert!(unsafe_fixes("unsafe-fixes = true", &[]));
        assert!(unsafe_fixes("", &["--unsafe-fixes"]));
        assert!(!unsafe_fixes("unsafe-fixes = true", &["--no-unsafe-fixes"]));
        assert!(unsafe_fixes("unsafe-fixes = false", &["--unsafe-fixes"]));
        // The flag given last wins
        assert!(unsafe_fixes("", &["--no-unsafe-fixes", "--unsafe-fixes"]));
        assert!(!unsafe_fixes("", &["--unsafe-fixes", "--no-unsafe-fixes"]));
    }

    fn overrides(cwd: &Path, patterns: &[&str]) -> Override {
        let mut builder = OverrideBuilder::new(cwd);
        for pattern in patterns {
            builder.add(&format!("!{pattern}")).unwrap();
        }
        builder.build().unwrap()
    }

    #[test]
    fn a_path_is_excluded_by_itself_or_by_any_directory_it_lies_in() {
        let cwd = Path::new("/project");
        let overrides = overrides(cwd, &["vendor/", "*_tb.vhd"]);
        let excluded = |path: &str| is_excluded(&overrides, cwd, Path::new(path));

        assert!(excluded("/project/vendor/lib/a.vhd"));
        assert!(excluded("vendor/a.vhd"));
        assert!(excluded("/project/src/top_tb.vhd"));
        // A directory-only pattern does not match a file of that name
        assert!(!excluded("/project/vendor"));
        assert!(!excluded("/project/src/vendor.vhd"));
        assert!(!excluded("/project/src/top.vhd"));
        // The part of the path up to the working directory is not matched
        assert!(!is_excluded(
            &overrides,
            Path::new("/vendor/project"),
            Path::new("/vendor/project/a.vhd")
        ));
    }

    #[test]
    #[cfg(unix)]
    fn saving_a_file_keeps_its_permissions() {
        use std::os::unix::fs::PermissionsExt;

        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("a.vhd");
        fs::write(&path, "old").unwrap();
        fs::set_permissions(&path, fs::Permissions::from_mode(0o644)).unwrap();

        save_file_atomically(&path, b"new").unwrap();

        assert_eq!(fs::read(&path).unwrap(), b"new");
        let mode = fs::metadata(&path).unwrap().permissions().mode();
        assert_eq!(mode & 0o777, 0o644);
    }

    #[test]
    #[cfg(unix)]
    fn saving_through_a_symlink_rewrites_the_target_and_keeps_the_link() {
        let dir = tempfile::tempdir().unwrap();
        let target = dir.path().join("real.vhd");
        let link = dir.path().join("link.vhd");
        fs::write(&target, "old").unwrap();
        std::os::unix::fs::symlink(&target, &link).unwrap();

        save_file_atomically(&link, b"new").unwrap();

        assert!(fs::symlink_metadata(&link).unwrap().is_symlink());
        assert_eq!(fs::read(&target).unwrap(), b"new");
    }

    #[test]
    fn unknown_codes_are_reported_once_in_the_order_given() {
        let mut registry = RuleRegistry::new();
        registry.register(NoParensAroundIf).unwrap();
        let selectors = ["IDM002", "ALL", "IDM", "IDM001", "IDM900", "idm2"]
            .map(|selector| selector.parse::<RuleSelector>().unwrap());

        let unknown = unknown_codes(&registry, selectors.iter())
            .iter()
            .map(ErrorCode::to_string)
            .collect::<Vec<_>>();
        assert_eq!(unknown, ["IDM002", "IDM900"]);
    }
}
