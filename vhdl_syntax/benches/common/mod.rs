use std::path::{Path, PathBuf};

fn paths(path: PathBuf) -> Box<dyn Iterator<Item = PathBuf>> {
    Box::new(
        std::fs::read_dir(&path)
            .unwrap_or_else(|error| panic!("failed to read {}: {error}", path.display()))
            .map(|entry| entry.expect("failed to read vhdl_libraries entry").path())
            .flat_map(|path| {
                if path.is_dir() {
                    paths(path)
                } else {
                    Box::new(std::iter::once(path)) as Box<dyn Iterator<Item = PathBuf>>
                }
            }),
    )
}

pub fn corpus(dir: &str) -> Vec<Vec<u8>> {
    paths(
        Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .expect("crate manifest has no parent")
            .join(dir),
    )
    .filter(|p| matches!(p.extension().and_then(|e| e.to_str()), Some("vhd" | "vhdl")))
    .map(|p| std::fs::read(&p).unwrap_or_else(|e| panic!("failed to read {}: {e}", p.display())))
    .collect()
}

pub fn total_bytes(sources: &[Vec<u8>]) -> u64 {
    sources.iter().map(|source| source.len() as u64).sum()
}
