use std::{
    error::Error,
    fmt::Display,
    fs, io,
    path::{Component, Path, PathBuf},
    str::FromStr,
};

use globset::{Candidate, GlobBuilder, GlobSet, GlobSetBuilder};
use serde::{Deserialize, Serialize};
use vhdl_syntax::standard::VHDLStandard;

use crate::{Encoding, FileSettings};

/// Global overrides that allow narrowing configuration options
/// based on a set of files
#[derive(Debug, Default, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case", deny_unknown_fields)]
pub struct FileOverride {
    /// The files that this applies to, relative to the directory of the config
    #[serde(deserialize_with = "non_empty")]
    files: Vec<String>,
    /// The VHDL standard
    standard: Option<VHDLStandard>,
    /// The comment-encoding
    encoding: Option<Encoding>,
}

fn non_empty<'de, D>(deserializer: D) -> Result<Vec<String>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    let files = Vec::<String>::deserialize(deserializer)?;
    if files.is_empty() {
        return Err(serde::de::Error::invalid_length(0, &"at least one pattern"));
    }
    Ok(files)
}

impl FileOverride {
    pub fn compile(self) -> Result<(GlobSet, Layer), globset::Error> {
        let mut builder = GlobSetBuilder::new();
        for file in &self.files {
            let pattern = file.strip_prefix("./").unwrap_or(file);
            builder.add(GlobBuilder::new(pattern).literal_separator(true).build()?);
        }
        Ok((
            builder.build()?,
            Layer {
                standard: self.standard,
                encoding: self.encoding,
            },
        ))
    }
}

/// The top-level configuration file
#[derive(Debug, Default, Serialize, Deserialize)]
#[serde(default, rename_all = "kebab-case", deny_unknown_fields)]
pub struct ConfigFile {
    /// The default standard to parse and analyze the file under
    standard: Option<VHDLStandard>,
    /// The default comment-encoding
    encoding: Option<Encoding>,
    /// Overrides that apply to a specific set of file-patterns
    overrides: Vec<FileOverride>,
}

impl ConfigFile {
    pub fn into_config(self, root: PathBuf, cli: Layer) -> Result<Config, globset::Error> {
        // Resolution is CLI -> overrides -> config
        let mut layers = vec![(LayerScope::All, cli)];
        for ovr in self.overrides.into_iter().rev() {
            let (globs, layer) = ovr.compile()?;
            layers.push((LayerScope::Matching(globs), layer));
        }
        layers.push((
            LayerScope::All,
            Layer {
                standard: self.standard,
                encoding: self.encoding,
            },
        ));
        Ok(Config { root, layers })
    }
}

pub const CONFIG_NAME: &str = "vhdl-lint.toml";

// normalizes lexically only on purpose
fn normalize(path: &Path) -> io::Result<PathBuf> {
    let mut out = PathBuf::new();
    for component in std::path::absolute(path)?.components() {
        match component {
            Component::ParentDir => {
                out.pop();
            }
            Component::CurDir => {}
            other => out.push(other),
        }
    }
    Ok(out)
}

fn resolve_config_path(path: &Path) -> Result<Option<PathBuf>, ConfigResolutionError> {
    let path = normalize(path).map_err(|err| ConfigResolutionError::io(err, path))?;
    for path in path.ancestors().map(|dir| dir.join(CONFIG_NAME)) {
        match fs::metadata(&path) {
            Ok(metadata) if metadata.is_file() => return Ok(Some(path)),
            Ok(_) => {}
            Err(err) if err.kind() == io::ErrorKind::NotFound => {}
            Err(err) => return Err(ConfigResolutionError::io(err, path)),
        }
    }
    Ok(None)
}

#[derive(Debug)]
pub struct ConfigResolutionError {
    path: PathBuf,
    kind: ConfigResolutionErrorKind,
}

impl ConfigResolutionError {
    fn io(err: io::Error, path: impl Into<PathBuf>) -> ConfigResolutionError {
        ConfigResolutionError {
            path: path.into(),
            kind: ConfigResolutionErrorKind::IoError(err),
        }
    }

    fn parse(err: toml::de::Error, path: impl Into<PathBuf>) -> ConfigResolutionError {
        ConfigResolutionError {
            path: path.into(),
            kind: ConfigResolutionErrorKind::ParseError(err),
        }
    }
}

#[derive(Debug)]
pub enum ConfigResolutionErrorKind {
    IoError(io::Error),
    ParseError(toml::de::Error),
}

impl Display for ConfigResolutionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "cannot read {}: ", self.path.display())?;
        match &self.kind {
            ConfigResolutionErrorKind::IoError(error) => write!(f, "{error}"),
            ConfigResolutionErrorKind::ParseError(err) => {
                write!(f, "{err}")
            }
        }
    }
}

impl Error for ConfigResolutionError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match &self.kind {
            ConfigResolutionErrorKind::IoError(error) => Some(error),
            ConfigResolutionErrorKind::ParseError(error) => Some(error),
        }
    }
}

impl ConfigFile {
    /// Resolve the config file from a directory.
    ///
    /// This function errors when the path is empty, or it is a file.
    ///
    /// On success, returns the parent path and the parsed file.
    pub fn resolve(
        path: impl AsRef<Path>,
    ) -> Result<Option<(PathBuf, ConfigFile)>, ConfigResolutionError> {
        let path = path.as_ref();
        let Some(config) = resolve_config_path(path)? else {
            return Ok(None);
        };
        Ok(Some(ConfigFile::from_file(config.as_path())?))
    }

    /// Reads the config from a file
    ///
    /// On success, returns the parent path and the parsed file.
    pub fn from_file(
        path: impl AsRef<Path>,
    ) -> Result<(PathBuf, ConfigFile), ConfigResolutionError> {
        let path = path.as_ref();
        let config = fs::canonicalize(path).map_err(|err| ConfigResolutionError::io(err, path))?;
        let contents =
            fs::read_to_string(&config).map_err(|err| ConfigResolutionError::io(err, path))?;
        let file =
            Self::from_str(&contents).map_err(|err| ConfigResolutionError::parse(err, path))?;
        let parent = config
            .parent()
            .expect("canonical file path has a parent")
            .to_owned();
        Ok((parent, file))
    }
}

impl FromStr for ConfigFile {
    type Err = toml::de::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        toml::from_str(s)
    }
}

/// A hierarchical layer where a layer that was added later has a lower priority
#[derive(Debug, Default, Clone)]
pub struct Layer {
    /// The standard to parse this layer under
    pub standard: Option<VHDLStandard>,
    /// The encoding to parse this layer under
    pub encoding: Option<Encoding>,
}

#[derive(Debug, Clone)]
pub enum LayerScope {
    /// Applies to every file
    All,
    /// Only applies to file matching the globs
    Matching(GlobSet),
}

impl LayerScope {
    fn matches(&self, candidate: Option<&Candidate<'_>>) -> bool {
        match self {
            LayerScope::All => true,
            LayerScope::Matching(glob_set) => {
                candidate.is_some_and(|candidate| glob_set.is_match_candidate(candidate))
            }
        }
    }
}

#[derive(Debug)]
pub struct Config {
    root: PathBuf,
    layers: Vec<(LayerScope, Layer)>,
}

impl Config {
    fn layers_for<'a>(&'a self, path: &'a Path) -> impl Iterator<Item = &'a Layer> {
        let candidate = path.strip_prefix(&self.root).ok().map(Candidate::new);
        self.layers
            .iter()
            .filter(move |(appl, _)| appl.matches(candidate.as_ref()))
            .map(|(_, layer)| layer)
    }

    pub fn settings(&self, path: &Path) -> FileSettings {
        FileSettings {
            standard: self
                .layers_for(path)
                .find_map(|l| l.standard)
                .unwrap_or_default(),
            encoding: self
                .layers_for(path)
                .find_map(|l| l.encoding)
                .unwrap_or_default(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_config_is_default() {
        let config: ConfigFile = toml::from_str("").unwrap();
        assert_eq!(config.standard, None);
        assert_eq!(config.encoding, None);
        assert!(config.overrides.is_empty());
    }

    #[test]
    fn deserialize_full_config() {
        let config: ConfigFile = toml::from_str(
            r#"
            standard = "93"
            encoding = "latin-1"

            [[overrides]]
            files = ["legacy/**"]
            standard = 1987
            "#,
        )
        .unwrap();
        assert_eq!(config.standard, Some(VHDLStandard::VHDL1993));
        assert_eq!(config.encoding, Some(Encoding::Latin1));
        let file_override = &config.overrides[0];
        assert_eq!(file_override.standard, Some(VHDLStandard::VHDL1987));
        assert_eq!(file_override.encoding, None);
    }

    #[test]
    fn rejects_unknown_fields_and_invalid_values() {
        assert!(toml::from_str::<ConfigFile>(r#"standrd = "08""#).is_err());
        assert!(toml::from_str::<ConfigFile>(r#"standard = "2010""#).is_err());
        assert!(toml::from_str::<ConfigFile>("standard = 2010").is_err());
    }

    #[test]
    fn round_trip() {
        let config: ConfigFile = toml::from_str(
            r#"
            standard = 2008

            [[overrides]]
            files = ["a.vhd"]
            "#,
        )
        .unwrap();
        let text = toml::to_string(&config).unwrap();
        let again: ConfigFile = toml::from_str(&text).unwrap();
        assert_eq!(format!("{config:?}"), format!("{again:?}"));
    }

    #[test]
    fn override_requires_files() {
        assert!(toml::from_str::<ConfigFile>("[[overrides]]\nstandard = 1993").is_err());
        assert!(toml::from_str::<ConfigFile>("[[overrides]]\nfiles = []").is_err());
    }

    fn config(contents: &str, cli: Layer) -> Config {
        let file: ConfigFile = toml::from_str(contents).unwrap();
        file.into_config(PathBuf::from("/project"), cli).unwrap()
    }

    fn standard(config: &Config, path: &str) -> VHDLStandard {
        config.settings(Path::new(path)).standard
    }

    #[test]
    fn settings_default_without_any_layer() {
        let config = config("", Layer::default());
        let settings = config.settings(Path::new("/project/a.vhd"));
        assert_eq!(settings.standard, VHDLStandard::default());
        assert_eq!(settings.encoding, Encoding::default());
    }

    #[test]
    fn override_applies_only_to_matching_files() {
        let config = config(
            r#"
            standard = 2002
            [[overrides]]
            files = ["legacy/**"]
            standard = 1993
            "#,
            Layer::default(),
        );
        assert_eq!(
            standard(&config, "/project/legacy/a/b.vhd"),
            VHDLStandard::VHDL1993
        );
        assert_eq!(
            standard(&config, "/project/src/b.vhd"),
            VHDLStandard::VHDL2002
        );
    }

    #[test]
    fn later_override_takes_precedence() {
        let config = config(
            r#"
            [[overrides]]
            files = ["**"]
            standard = 1993
            [[overrides]]
            files = ["legacy/*.vhd"]
            standard = 1987
            "#,
            Layer::default(),
        );
        assert_eq!(
            standard(&config, "/project/legacy/a.vhd"),
            VHDLStandard::VHDL1987
        );
        assert_eq!(
            standard(&config, "/project/src/a.vhd"),
            VHDLStandard::VHDL1993
        );
    }

    #[test]
    fn cli_takes_precedence_over_overrides() {
        let config = config(
            r#"
            [[overrides]]
            files = ["**"]
            standard = 1993
            encoding = "latin-1"
            "#,
            Layer {
                standard: Some(VHDLStandard::VHDL2008),
                encoding: None,
            },
        );
        let settings = config.settings(Path::new("/project/a.vhd"));
        assert_eq!(settings.standard, VHDLStandard::VHDL2008);
        assert_eq!(settings.encoding, Encoding::Latin1);
    }

    #[test]
    fn star_does_not_cross_directories() {
        let config = config(
            "[[overrides]]\nfiles = [\"*.vhd\"]\nstandard = 1993",
            Layer::default(),
        );
        assert_eq!(standard(&config, "/project/a.vhd"), VHDLStandard::VHDL1993);
        assert_eq!(
            standard(&config, "/project/src/a.vhd"),
            VHDLStandard::default()
        );
    }

    #[test]
    fn leading_dot_slash_in_pattern_is_ignored() {
        let config = config(
            "[[overrides]]\nfiles = [\"./legacy/**\"]\nstandard = 1993",
            Layer::default(),
        );
        assert_eq!(
            standard(&config, "/project/legacy/a.vhd"),
            VHDLStandard::VHDL1993
        );
    }

    #[test]
    fn overrides_do_not_apply_outside_the_root() {
        let config = config(
            r#"
            standard = 2002
            [[overrides]]
            files = ["**/a.vhd"]
            standard = 1993
            "#,
            Layer::default(),
        );
        assert_eq!(
            standard(&config, "/project/x/a.vhd"),
            VHDLStandard::VHDL1993
        );
        assert_eq!(standard(&config, "/other/x/a.vhd"), VHDLStandard::VHDL2002);
    }

    fn write_config(dir: &Path, contents: &str) -> PathBuf {
        let path = dir.join(CONFIG_NAME);
        fs::write(&path, contents).unwrap();
        path
    }

    #[test]
    fn normalize_removes_dot_components_lexically() {
        let dir = tempfile::tempdir().unwrap();
        let root = dir.path();
        assert_eq!(
            normalize(&root.join("a/./b/../c")).unwrap(),
            root.join("a/c")
        );
        // `..` is applied lexically, so the popped component need not exist
        assert_eq!(
            normalize(&root.join("does/not/exist/../..")).unwrap(),
            root.join("does")
        );
    }

    #[test]
    fn normalize_does_not_pop_past_root() {
        let root = Path::new(std::path::MAIN_SEPARATOR_STR);
        let path = root.join("..").join("..");
        assert_eq!(normalize(&path).unwrap(), normalize(root).unwrap());
    }

    #[test]
    fn resolves_config_in_given_directory() {
        let dir = tempfile::tempdir().unwrap();
        let config = write_config(dir.path(), "");
        assert_eq!(resolve_config_path(dir.path()).unwrap(), Some(config));
    }

    #[test]
    fn resolves_config_in_ancestor_directory() {
        let dir = tempfile::tempdir().unwrap();
        let config = write_config(dir.path(), "");
        let nested = dir.path().join("a/b/c");
        fs::create_dir_all(&nested).unwrap();
        assert_eq!(resolve_config_path(&nested).unwrap(), Some(config));
    }

    #[test]
    fn nearest_config_wins() {
        let dir = tempfile::tempdir().unwrap();
        write_config(dir.path(), "");
        let nested = dir.path().join("a/b");
        fs::create_dir_all(&nested).unwrap();
        let inner = write_config(&dir.path().join("a"), "");
        assert_eq!(resolve_config_path(&nested).unwrap(), Some(inner));
    }

    #[test]
    fn resolves_through_nonexistent_directories() {
        let dir = tempfile::tempdir().unwrap();
        let config = write_config(dir.path(), "");
        let missing = dir.path().join("missing/dir");
        assert_eq!(resolve_config_path(&missing).unwrap(), Some(config));
    }

    #[test]
    fn parent_dir_components_are_resolved_before_searching() {
        let dir = tempfile::tempdir().unwrap();
        let config = write_config(dir.path(), "");
        let sibling = dir.path().join("sibling");
        fs::create_dir(&sibling).unwrap();
        write_config(&sibling, "");
        // `sibling/..` must not pick up the config inside `sibling`
        let path = sibling.join("..");
        assert_eq!(resolve_config_path(&path).unwrap(), Some(config));
    }

    #[test]
    fn directory_named_like_config_is_skipped() {
        let dir = tempfile::tempdir().unwrap();
        let config = write_config(dir.path(), "");
        let nested = dir.path().join("nested");
        fs::create_dir_all(nested.join(CONFIG_NAME)).unwrap();
        assert_eq!(resolve_config_path(&nested).unwrap(), Some(config));
    }

    #[test]
    fn empty_path_is_an_error() {
        let err = ConfigFile::resolve("").unwrap_err();
        assert_eq!(err.path, PathBuf::new());
        assert!(matches!(err.kind, ConfigResolutionErrorKind::IoError(_)));
    }

    #[cfg(unix)]
    #[test]
    fn file_path_is_an_error() {
        let dir = tempfile::tempdir().unwrap();
        write_config(dir.path(), "");
        let file = dir.path().join("design.vhd");
        fs::write(&file, "").unwrap();
        let err = ConfigFile::resolve(&file).unwrap_err();
        assert_eq!(err.path, file.join(CONFIG_NAME));
        let ConfigResolutionErrorKind::IoError(io_err) = &err.kind else {
            panic!("expected an io error, got {:?}", err.kind);
        };
        assert_eq!(io_err.kind(), io::ErrorKind::NotADirectory);
    }

    #[test]
    fn resolve_reads_and_parses_config() {
        let dir = tempfile::tempdir().unwrap();
        write_config(dir.path(), "standard = 1993");
        let nested = dir.path().join("src");
        fs::create_dir(&nested).unwrap();
        let (found, config) = ConfigFile::resolve(&nested).unwrap().unwrap();
        assert_eq!(found, fs::canonicalize(dir.path()).unwrap());
        assert_eq!(config.standard, Some(VHDLStandard::VHDL1993));
    }

    #[test]
    fn resolve_reports_parse_errors_with_path() {
        let dir = tempfile::tempdir().unwrap();
        let path = write_config(dir.path(), "standard = 2010");
        let err = ConfigFile::resolve(dir.path()).unwrap_err();
        assert_eq!(err.path, path);
        assert!(matches!(err.kind, ConfigResolutionErrorKind::ParseError(_)));
        assert!(err.source().is_some());
        assert!(err
            .to_string()
            .starts_with(&format!("cannot read {}: ", path.display())));
    }

    #[test]
    fn from_file_reports_missing_file() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join(CONFIG_NAME);
        let err = ConfigFile::from_file(&path).unwrap_err();
        assert_eq!(err.path, path);
        let ConfigResolutionErrorKind::IoError(io_err) = &err.kind else {
            panic!("expected an io error, got {:?}", err.kind);
        };
        assert_eq!(io_err.kind(), io::ErrorKind::NotFound);
    }
}
