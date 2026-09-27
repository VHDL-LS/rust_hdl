use std::{collections::BTreeSet, fs, io, path::Path, process::ExitCode};

use anyhow::Context;
use clap::Parser;
use vhdl_lint::rule::{register_builtin_rules, RuleRegistry};
use vhdl_lint_docs::{pages, update_summary, Page, RULES_DIR};

/// Generate the rule pages of the vhdl-lint book
#[derive(Parser)]
struct Args {
    /// Check that the generated pages are up-to-date; exit 1 if any differ
    #[arg(long)]
    check: bool,
}

/// The `.md` files directly below `dir`, relative to `src`.
fn existing_pages(src: &Path, dir: &Path) -> io::Result<BTreeSet<String>> {
    let entries = match fs::read_dir(dir) {
        Ok(entries) => entries,
        Err(e) if e.kind() == io::ErrorKind::NotFound => return Ok(BTreeSet::new()),
        Err(e) => return Err(e),
    };
    let mut pages = BTreeSet::new();
    for entry in entries {
        let path = entry?.path();
        if path.extension().is_some_and(|ext| ext == "md") {
            let relative = path.strip_prefix(src).unwrap();
            pages.insert(relative.to_string_lossy().replace('\\', "/"));
        }
    }
    Ok(pages)
}

fn run(args: &Args, src: &Path) -> anyhow::Result<ExitCode> {
    let mut registry = RuleRegistry::new();
    register_builtin_rules(&mut registry);

    let summary_path = src.join("SUMMARY.md");
    let summary = fs::read_to_string(&summary_path)
        .with_context(|| format!("could not read {}", summary_path.display()))?;
    let mut expected = pages(&registry);
    expected.push(Page {
        path: "SUMMARY.md".to_owned(),
        contents: update_summary(&summary, &registry)?,
    });

    let outdated = expected
        .iter()
        .filter(|page| {
            fs::read_to_string(src.join(&page.path)).ok().as_deref() != Some(&page.contents)
        })
        .collect::<Vec<_>>();
    // Pages of rules that no longer exist, or were renamed
    let mut removed = existing_pages(src, &src.join(RULES_DIR))?;
    for page in &expected {
        removed.remove(&page.path);
    }

    if args.check {
        if outdated.is_empty() && removed.is_empty() {
            println!("All rule pages are up-to-date.");
            return Ok(ExitCode::SUCCESS);
        }
        eprintln!("The following rule pages are out of date:");
        for page in &outdated {
            eprintln!("  {}", page.path);
        }
        for path in &removed {
            eprintln!("  {path} (no longer generated)");
        }
        eprintln!("Run `cargo xtask lint-docs` to regenerate.");
        return Ok(ExitCode::FAILURE);
    }

    fs::create_dir_all(src.join(RULES_DIR))?;
    for page in &outdated {
        let path = src.join(&page.path);
        fs::write(&path, &page.contents)
            .with_context(|| format!("could not write {}", path.display()))?;
    }
    for path in &removed {
        let path = src.join(path);
        fs::remove_file(&path).with_context(|| format!("could not remove {}", path.display()))?;
    }
    println!(
        "Updated {} and removed {} rule page(s) below {}",
        outdated.len(),
        removed.len(),
        src.display()
    );
    Ok(ExitCode::SUCCESS)
}

fn main() -> ExitCode {
    let args = Args::parse();
    // CARGO_MANIFEST_DIR is the vhdl-lint-docs/ directory at compile time
    let src = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .join("vhdl-lint/book/src");
    match run(&args, &src) {
        Ok(code) => code,
        Err(e) => {
            eprintln!("error: {e:#}");
            ExitCode::from(2)
        }
    }
}
