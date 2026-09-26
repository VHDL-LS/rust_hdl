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
    env::current_dir,
    fs,
    io::{self, Write},
    path::{Path, PathBuf},
    process::ExitCode,
    sync::Mutex,
};
use vhdl_lint::{
    config::{Config, ConfigFile, Layer},
    diagnostic::{render_diagnostics, Diagnostic},
    error_code::ErrorCode,
    fix::Fix,
    fix_file, parse_and_analyze_file,
    rule::{
        explicit_port_mode::ExplicitPortMode,
        no_parens_around_if::NoParensAroundIf,
        selection::{RuleOverrides, RuleSelector},
        RuleRegistry,
    },
    Encoding, File, FileStore, FixErrKind, FixOutcome,
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

    /// Apply fixes to resolve lint violations
    #[arg(long)]
    fix: bool,

    #[clap(flatten)]
    file_selection: FileSelection,

    #[clap(flatten)]
    rule_selection: RuleSelection,

    /// Exit with status code "0", even upon detecting lint violations
    #[arg(short, long, help_heading = "Miscellaneous")]
    exit_zero: bool,
}

impl Args {
    pub fn layer(&self) -> Layer {
        Layer {
            standard: self.std,
            encoding: self.encoding,
        }
    }
}

/// How many of `diagnostics` carry a fix that `--fix` would apply.
fn fixable(diagnostics: &[Diagnostic]) -> usize {
    diagnostics
        .iter()
        .filter(|diag| diag.fix().is_some_and(Fix::is_safe))
        .count()
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

fn load_config(args: &Args, cwd: &Path) -> Result<Config, String> {
    let file = match &args.config {
        Some(path) => Some(ConfigFile::from_file(path).map_err(|e| e.to_string())?),
        None => ConfigFile::resolve(cwd).map_err(|e| e.to_string())?,
    };
    let (dir, file) = match file {
        Some(file) => file,
        None => (cwd.to_owned(), ConfigFile::default()),
    };
    file.into_config(dir, args.layer())
        .map_err(|e| format!("invalid config: {e}"))
}

#[derive(Default)]
struct Walk {
    files: FileStore,
    skipped: Vec<String>,
}

type Out = Result<Walk, ignore::Error>;

struct CollectorBuilder<'a, 's> {
    out: &'s Mutex<Out>,
    config: &'a Config,
}

struct Collector<'a, 's> {
    files: Vec<File>,
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
            for file in std::mem::take(&mut self.files) {
                walk.files.insert_file(file);
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
                            self.files.push(File::new(path, data, settings));
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

    let config = match load_config(&args, &cwd) {
        Ok(config) => config,
        Err(e) => {
            anstream::eprintln!("Cannot load config: {e}");
            return ExitCode::from(EXIT_TOOL_FAILURE);
        }
    };

    let mut registry = RuleRegistry::new();
    registry.register(NoParensAroundIf).unwrap();
    registry.register(ExplicitPortMode).unwrap();

    let unknown = unknown_codes(&registry, args.rule_selection.selectors());
    if !unknown.is_empty() {
        let codes = unknown.iter().map(|code| format!("'{code}'")).join(", ");
        let verb = if unknown.len() == 1 { "does" } else { "do" };
        anstream::eprintln!("error: {codes} {verb} not exist");
        return ExitCode::from(EXIT_TOOL_FAILURE);
    }

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
    let (mut files, mut skipped) = match out.into_inner().unwrap() {
        Ok(Walk { files, skipped }) => (files, skipped),
        Err(e) => {
            anstream::eprintln!("error: {e}");
            return ExitCode::from(EXIT_TOOL_FAILURE);
        }
    };

    let overrides = args.rule_selection.overrides();

    let mut total_fixes = 0usize;
    let mut errors = if args.fix {
        let outcomes = files
            .par_iter()
            .map(|(file_id, file)| {
                let outcome = fix_file(file, file_id, &registry, &overrides);
                (file_id, outcome)
            })
            .collect::<Vec<_>>();

        let mut errors = Vec::new();
        for (file_id, outcome) in outcomes {
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
                            diagnostics = parse_and_analyze_file(
                                files.get(file_id),
                                file_id,
                                &registry,
                                &overrides,
                            )
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
                        parse_and_analyze_file(files.get(file_id), file_id, &registry, &overrides)
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
                parse_and_analyze_file(file, file_id, &registry, &overrides).into_diagnostics()
            })
            .collect::<Vec<_>>()
    };

    if !errors.is_empty() || !skipped.is_empty() {
        errors.sort_by_key(|diag| {
            let loc = diag.loc();
            (
                files.get(loc.file()).path(),
                loc.span().start,
                loc.span().end,
            )
        });
        skipped.sort();
        let renderer = Renderer::styled().decor_style(DecorStyle::Unicode);

        let report = render_diagnostics(&errors, &files)
            .chain(
                skipped
                    .iter()
                    .map(|msg| Group::with_title(Level::ERROR.primary_title(msg))),
            )
            .collect::<Vec<_>>();
        anstream::eprintln!("{}", renderer.render(&report));
    }

    if total_fixes > 0 {
        anstream::println!("Fixed {total_fixes} issue{}", pluralize(total_fixes));
    }

    if !args.fix {
        let fixable = fixable(&errors);
        if fixable > 0 {
            anstream::println!(
                "{fixable} issue{} fixable with the `--fix` option",
                if fixable == 1 { " is" } else { "s are" }
            );
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
        ExitCode::SUCCESS
    }
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

#[cfg(test)]
mod tests {
    use super::*;
    use vhdl_lint::FileSettings;

    #[test]
    fn only_a_fix_that_fix_applies_counts_as_fixable() {
        use vhdl_lint::{
            error_code::Category, fix::Edit, severity::Severity, source_loc::SourceLoc,
        };
        let mut file_store = FileStore::new();
        let id = file_store.insert(Path::new("inline"), vec![], FileSettings::default());

        let diagnostic = |fix: Option<Fix>| {
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
        let edits = || vec![Edit::delete_raw(0..1)];

        assert_eq!(
            fixable(&[
                diagnostic(Some(Fix::safe("safe", edits()))),
                diagnostic(Some(Fix::display_only("display only", edits()))),
                diagnostic(None),
            ]),
            1
        );
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
