// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

//! Configuration of the design hierarchy and other settings

use std::collections::BTreeSet;
use std::env;
use std::fs::File;
use std::io;
use std::io::prelude::*;
use std::path::Path;
use std::str::FromStr;

use fnv::FnvHashMap;
use subst::VariableMap;
use toml::{Table, Value};

use crate::data::error_codes::ErrorCode;
use crate::data::*;
use crate::standard::VHDLStandard;

/// Defines standard VHDL case conventions.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Case {
    /// All lower case, i.e., `std_logic_vector`
    Lower,
    /// All upper-case, i.e., `STD_LOGIC_VECTOR`
    Upper,
    /// Pascal case, i.e., `Std_Logic_Vector`
    Pascal,
}

impl FromStr for Case {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "lower" | "snake" => Case::Lower,
            "upper" | "upper_snake" => Case::Upper,
            "pascal" | "upper_camel" => Case::Pascal,
            other => return Err(other.to_string()),
        })
    }
}

impl Case {
    /// Converts the case in place, modifying the passed string.
    pub fn convert(&self, val: &mut str) {
        match self {
            Case::Lower => val.make_ascii_lowercase(),
            Case::Upper => val.make_ascii_uppercase(),
            Case::Pascal => {
                // SAFETY: changing ASCII letters only does not invalidate UTF-8.
                let bytes = unsafe { val.as_bytes_mut() };
                // First letter should be uppercased
                let mut next_uppercase = true;
                for byte in bytes {
                    if byte == &b'_' {
                        next_uppercase = true;
                        continue;
                    }
                    if next_uppercase {
                        byte.make_ascii_uppercase();
                    } else {
                        byte.make_ascii_lowercase();
                    }
                    next_uppercase = false;
                }
            }
        }
    }
}

#[cfg(test)]
mod case_tests {
    use super::*;

    #[test]
    fn test_case_lower() {
        let mut test = String::from("STD_LOGIC_VECTOR");
        Case::Lower.convert(&mut test);
        assert_eq!(test, "std_logic_vector");
    }

    #[test]
    fn test_case_upper() {
        let mut test = String::from("std_logic_vector");
        Case::Upper.convert(&mut test);
        assert_eq!(test, "STD_LOGIC_VECTOR");
    }

    #[test]
    fn test_case_pascal() {
        let mut test = String::from("std_logic_vector");
        Case::Pascal.convert(&mut test);
        assert_eq!(test, "Std_Logic_Vector");
    }

    #[test]
    fn test_case_empty() {
        for case in &[Case::Lower, Case::Upper, Case::Pascal] {
            let mut test = String::new();
            case.convert(&mut test);
            assert_eq!(test, "");
        }
    }

    #[test]
    fn test_case_underscore_only() {
        for case in &[Case::Lower, Case::Upper, Case::Pascal] {
            let mut test = String::from("___");
            case.convert(&mut test);
            assert_eq!(test, "___");
        }
    }

    #[test]
    fn test_case_consecutive_underscore() {
        let mut test = String::from("std__logic___vector");
        Case::Pascal.convert(&mut test);
        assert_eq!(test, "Std__Logic___Vector");
    }

    #[test]
    fn test_case_mixed() {
        let mut test = String::from("StD_LoGiC_VeCToR");
        Case::Pascal.convert(&mut test);
        assert_eq!(test, "Std_Logic_Vector");
    }
}

#[derive(Clone, PartialEq, Eq, Default, Debug)]
pub struct Config {
    // A map from library name to file name
    libraries: FnvHashMap<String, LibraryConfig>,
    standard: VHDLStandard,
    // Defines the severity that diagnostics are displayed with
    severities: SeverityMap,
    preferred_case: Option<Case>,
}

#[derive(Clone, PartialEq, Eq, Default, Debug)]
pub struct LibraryConfig {
    name: String,
    patterns: Vec<String>,
    exclude_patterns: Vec<String>,
    pub(crate) is_third_party: bool,
}

impl LibraryConfig {
    /// Return a vector of file names
    /// Only include files that exists
    /// Files that do not exist produce a warning message
    pub fn file_names(&self, messages: &mut dyn MessageHandler) -> Vec<PathBuf> {
        let mut result = match_file_patterns(&self.patterns, messages);
        let exclude = match_file_patterns(&self.exclude_patterns, messages);
        // Remove excluded files from result
        result.retain(|e| !exclude.contains(e));
        Vec::from_iter(result)
    }

    /// Returns the name of the library
    pub fn name(&self) -> &str {
        self.name.as_str()
    }
}

impl Config {
    pub fn from_str(string: &str, parent: &Path) -> Result<Config, String> {
        let config = string.parse::<Value>().map_err(|err| err.to_string())?;
        let mut libraries = FnvHashMap::default();

        let standard = if let Some(std) = config.get("standard") {
            let std_str = std.as_str().ok_or("standard must be a string")?;
            VHDLStandard::try_from(std_str)
                .map_err(|_| format!("Unsupported standard '{std_str}'"))?
        } else {
            VHDLStandard::default()
        };

        let libs = config
            .get("libraries")
            .ok_or("missing field libraries")?
            .as_table()
            .ok_or("libraries must be a table")?;

        for (name, lib) in libs.iter() {
            if name.to_lowercase() == "work" {
                return Err(format!(
                    "The '{name}' library is not a valid library.\nHint: To use a library that contains all files, use a common name for all libraries, i.e., 'defaultlib'"
                ));
            }

            let file_arr = lib
                .get("files")
                .ok_or_else(|| format!("missing field files for library {name}"))?
                .as_array()
                .ok_or_else(|| format!("files for library {name} is not array"))?;
            let patterns = check_file_patterns(file_arr, parent)?;

            let mut exclude_patterns = Vec::new();
            if let Some(opt) = lib.get("exclude") {
                if let Some(opt) = opt.as_array() {
                    exclude_patterns = check_file_patterns(opt, parent)?;
                } else {
                    return Err(format!("excludes for library {name} is not array"));
                }
            }

            let mut is_third_party = false;
            if let Some(opt) = lib.get("is_third_party") {
                if let Some(opt) = opt.as_bool() {
                    is_third_party = opt;
                } else {
                    return Err(format!(
                        "Expected is_third_party to be boolean for library {name}"
                    ));
                }
            }

            libraries.insert(
                name.to_owned(),
                LibraryConfig {
                    name: name.to_owned(),
                    patterns,
                    exclude_patterns,
                    is_third_party,
                },
            );
        }

        let severities = if let Some(lint) = config.get("lint") {
            Self::read_severity_overwrites(lint.as_table().ok_or("lint must be a table")?)?
        } else {
            SeverityMap::default()
        };

        let case = if let Some(case) = config.get("preferred_case") {
            Some(
                case.as_str()
                    .ok_or("preferred_case must be a string")?
                    .parse()
                    .map_err(|other| format!("Case '{other}' not valid"))?,
            )
        } else {
            None
        };

        Ok(Config {
            libraries,
            severities,
            standard,
            preferred_case: case,
        })
    }

    fn read_severity_overwrites(severity_overwrites: &Table) -> Result<SeverityMap, String> {
        let mut severities = SeverityMap::default();

        for (name, severity) in severity_overwrites {
            let error_code = ErrorCode::try_from(name.as_str())
                .map_err(|_| format!("'{name}' is not a valid error code"))?;
            match severity {
                Value::String(severity) => {
                    let severity = Severity::try_from(severity.as_str())
                        .map_err(|_| format!("'{severity}' is not a valid severity level"))?;
                    severities[error_code] = Some(severity);
                }
                Value::Boolean(should_show) => {
                    if !should_show {
                        severities[error_code] = None
                    }
                }
                _ => return Err("severity must be a string or boolean".to_string()),
            }
        }
        Ok(severities)
    }

    pub fn read_file_path(file_name: &Path) -> io::Result<Config> {
        let mut file = File::open(file_name)?;
        let mut contents = String::new();
        file.read_to_string(&mut contents)?;

        let parent = file_name.parent().unwrap();

        Config::from_str(&contents, parent).map_err(io::Error::other)
    }

    pub fn get_library(&self, name: &str) -> Option<&LibraryConfig> {
        self.libraries.get(name)
    }

    pub fn iter_libraries(&self) -> impl Iterator<Item = &LibraryConfig> {
        self.libraries.values()
    }

    /// Append another config to self
    ///
    /// In case of conflict the appended config takes precedence
    pub fn append(&mut self, config: &Config, messages: &mut dyn MessageHandler) {
        self.standard = config.standard;
        for library in config.iter_libraries() {
            if let Some(parent_library) = self.libraries.get_mut(&library.name) {
                *parent_library = library.clone();

                messages.push(Message::warning(format!(
                    "Re-defined library {}",
                    &library.name
                )));
            } else {
                self.libraries.insert(library.name.clone(), library.clone());
            }
        }
        self.severities = config.severities;
        self.preferred_case = config.preferred_case;
    }

    /// Load configuration file from installation folder
    fn load_installed_config(
        &mut self,
        messages: &mut dyn MessageHandler,
        location: Option<String>,
    ) {
        if let Some(location) = location {
            let mut path = PathBuf::from(location);
            path.push("vhdl_ls.toml");
            self.load_config(&path, "Installation", messages);
            return;
        }
        let search_paths = [
            "../vhdl_libraries",
            "../../vhdl_libraries",
            "/usr/lib/rust_hdl/vhdl_libraries",
            "/usr/local/lib/rust_hdl/vhdl_libraries",
            "../share/vhdl_libraries",
        ];

        for dir in search_paths.into_iter() {
            let mut file_name = PathBuf::from(dir);
            // Expand a relative path
            if !file_name.is_absolute() {
                let exe_path = env::current_exe().expect("Executable path needed");
                let exe_folder = exe_path.parent().expect("Executable folder must exist");
                file_name = exe_folder.join(file_name)
            }
            file_name.push("vhdl_ls.toml");
            if file_name.exists() {
                self.load_config(&file_name, "Installation", messages);
                return;
            }
        }

        // Panic if we did not yet find the installed libraries
        panic!(
            "Couldn't find installed libraries at {}.",
            search_paths.join(", ")
        );
    }

    /// Load configuration file from home folder
    fn load_home_config(&mut self, messages: &mut dyn MessageHandler) {
        if let Some(home_dir) = dirs::home_dir() {
            let file_name = home_dir.join(".vhdl_ls.toml");

            if !file_name.exists() {
                return;
            }

            self.load_config(&file_name, "HOME folder", messages);
        }
    }

    /// Load configuration file from environment
    fn load_env_config(&mut self, env_name: &str, messages: &mut dyn MessageHandler) {
        if let Some(file_name) = std::env::var_os(env_name) {
            self.load_config(Path::new(&file_name), env_name, messages);
        };
    }

    /// Load and append configuration file
    fn load_config(&mut self, file_name: &Path, desc: &str, messages: &mut dyn MessageHandler) {
        match Config::read_file_path(Path::new(&file_name)) {
            Ok(env_config) => {
                messages.push(Message::log(format!(
                    "Loaded {} configuration file: {}",
                    desc,
                    file_name.to_string_lossy()
                )));

                self.append(&env_config, messages);
            }
            Err(ref err) => {
                messages.push(Message::error(format!(
                    "Error while loading {desc} configuration file: {err} "
                )));
            }
        }
    }

    /// Load all external configuration
    /// If the `standard_libraries_path` is given, it must point to a valid
    /// `vhdl_ls.toml` file, which will be used as source for the standard libraries
    /// i.e., `std` or `ieee`.
    /// If this path is `None`, a set of standard search paths will be queried for the location
    /// of this file.
    pub fn load_external_config(
        &mut self,
        messages: &mut dyn MessageHandler,
        standard_libraries_path: Option<String>,
    ) {
        self.load_installed_config(messages, standard_libraries_path);
        self.load_home_config(messages);
        self.load_env_config("VHDL_LS_CONFIG", messages);
    }

    pub fn severities(&self) -> &SeverityMap {
        &self.severities
    }

    /// The VHDL standard to use if no more specific config is present.
    /// By default, VHDL 2008 is assumed
    pub fn standard(&self) -> VHDLStandard {
        self.standard
    }

    /// Returns the casing that is preferred by the user for linting or completions.
    pub fn preferred_case(&self) -> Option<Case> {
        self.preferred_case
    }
}

fn match_file_patterns(
    patterns: &[String],
    messages: &mut dyn MessageHandler,
) -> BTreeSet<PathBuf> {
    let mut result = BTreeSet::new();
    for pattern in patterns.iter() {
        let stripped_pattern = if cfg!(windows) {
            pattern.strip_prefix("\\\\?\\").unwrap_or(pattern.as_str())
        } else {
            pattern.as_str()
        };

        if is_literal(stripped_pattern) {
            let file_path = PathBuf::from(pattern);

            if file_path.exists() {
                result.insert(file_path);
            } else {
                messages.push(Message::warning(format! {"File {pattern} does not exist"}));
            }
        } else {
            match glob::glob(stripped_pattern) {
                Ok(paths) => {
                    let mut empty_pattern = true;

                    for file_path_or_error in paths {
                        empty_pattern = false;
                        match file_path_or_error {
                            Ok(file_path) => {
                                result.insert(file_path);
                            }
                            Err(err) => {
                                messages.push(Message::error(err.to_string()));
                            }
                        }
                    }

                    if empty_pattern {
                        messages.push(Message::warning(format!(
                            "Pattern '{stripped_pattern}' did not match any file"
                        )));
                    }
                }
                Err(err) => {
                    messages.push(Message::error(format!("Invalid pattern '{pattern}' {err}")));
                }
            }
        }
    }
    result
}

fn check_file_patterns(file_arr: &[Value], parent: &Path) -> Result<Vec<String>, String> {
    let mut patterns = Vec::new();
    for file in file_arr.iter() {
        let file = file
            .as_str()
            .ok_or_else(|| format!("not a string {file}"))?;

        let file = substitute_environment_variables(file, &subst::Env)?;

        let path = parent.join(file);
        let path = path
            .to_str()
            .ok_or_else(|| format!("Could not convert {path:?} to string"))?
            .to_owned();
        patterns.push(path);
    }
    Ok(patterns)
}

fn substitute_environment_variables<'a, M>(s: &str, map: &'a M) -> Result<String, String>
where
    M: VariableMap<'a> + ?Sized,
    M::Value: AsRef<str>,
{
    if cfg!(windows) {
        substitute_variables_windows(s, map)
    } else {
        subst::substitute(s, map).map_err(|err| err.to_string())
    }
}

fn substitute_variables_windows<'a, M>(s: &str, map: &'a M) -> Result<String, String>
where
    M: VariableMap<'a> + ?Sized,
    M::Value: AsRef<str>,
{
    let mut output: Vec<char> = Vec::with_capacity(s.len());
    let mut var_buf: Vec<char> = Vec::new();

    let mut var_found = false;

    for ch in s.chars() {
        if ch == '%' {
            if var_found {
                let var_name = String::from_iter(var_buf);
                var_buf = Vec::new();
                match map.get(&var_name) {
                    None => {
                        return Err(format!("Variable '{var_name}' not found"));
                    }
                    Some(value) => {
                        output.extend(value.as_ref().chars());
                    }
                }
            }
            var_found = !var_found;
        } else if !var_found {
            output.push(ch);
        } else {
            var_buf.push(ch)
        }
    }

    if var_found {
        Err("Unterminated variable".into())
    } else {
        Ok(String::from_iter(output))
    }
}

/// Returns true if the pattern is a plain file name and not a glob pattern
fn is_literal(pattern: &str) -> bool {
    !pattern.chars().any(|chr| matches!(&chr, '?' | '*' | '['))
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use pretty_assertions::assert_eq;

    use super::*;

    /// Utility function to create an empty file in parent folder
    fn touch(parent: &Path, file_name: &str) -> PathBuf {
        let path = parent.join(file_name);
        File::create(&path).expect("Assume file can be created");
        path
    }

    fn abspath(path: &Path) -> PathBuf {
        dunce::canonicalize(path).unwrap()
    }

    fn abspaths(paths: &[PathBuf]) -> Vec<PathBuf> {
        paths.iter().map(|path| abspath(path)).collect()
    }

    // Check that two PathBuf slices are the same, ignoring order
    fn assert_files_eq(got: &[PathBuf], expected: &[PathBuf]) {
        assert_eq!(got.len(), expected.len());
        assert_eq!(
            BTreeSet::from_iter(abspaths(got)),
            BTreeSet::from_iter(abspaths(expected))
        );
    }

    #[test]
    fn test_is_literal() {
        assert!(is_literal("file.vhd"));
        assert!(!is_literal("file*.vhd"));
        assert!(!is_literal("file?.vhd"));
        assert!(!is_literal("file[ab].vhd"));
    }

    #[test]
    fn config_from_str() {
        let tempdir = tempfile::tempdir().unwrap();
        let parent = tempdir.path();

        let tempdir2 = tempfile::tempdir().unwrap();
        let absolute_path = abspath(tempdir2.path());
        let absolute_vhd = touch(&absolute_path, "absolute.vhd");

        let config = Config::from_str(
            &format!(
                "
[libraries]
lib2.files = [
  'pkg2.vhd',
  '{}'
]
lib1.files = [
  'pkg1.vhd',
  'tb_ent.vhd'
]

[lint]
unused = 'error'
duplicate = false
",
                absolute_vhd.to_str().unwrap()
            ),
            parent,
        )
        .unwrap();
        let mut libraries: Vec<&str> = config.iter_libraries().map(|lib| lib.name()).collect();
        libraries.sort_unstable();
        assert_eq!(libraries, &["lib1", "lib2"]);

        let lib1 = config.get_library("lib1").unwrap();
        let lib2 = config.get_library("lib2").unwrap();

        let pkg1_path = touch(parent, "pkg1.vhd");
        let pkg2_path = touch(parent, "pkg2.vhd");
        let tb_ent_path = touch(parent, "tb_ent.vhd");

        let mut messages = vec![];
        assert_files_eq(&lib1.file_names(&mut messages), &[pkg1_path, tb_ent_path]);
        assert_files_eq(&lib2.file_names(&mut messages), &[pkg2_path, absolute_vhd]);
        assert_eq!(messages, vec![]);

        let mut expected_map = SeverityMap::default();
        expected_map[ErrorCode::Unused] = Some(Severity::Error);
        expected_map[ErrorCode::Duplicate] = None;
        assert_eq!(config.severities, expected_map)
    }

    #[test]
    fn test_append_config() {
        let parent0 = Path::new("parent_folder0");
        let config0 = Config::from_str(
            "
[libraries]
lib1.files = [
  'pkg1.vhd',
]
lib2.files = [
  'pkg2.vhd'
]
",
            parent0,
        )
        .unwrap();

        let parent1 = Path::new("parent_folder1");
        let config1 = Config::from_str(
            "
[libraries]
lib2.files = [
  'ent.vhd'
]
lib3.files = [
  'pkg3.vhd',
]
",
            parent1,
        )
        .unwrap();

        let expected_parent = Path::new("");
        let expected_config = Config::from_str(
            &format!(
                "
[libraries]
lib1.files = [
  '{pkg1}',
]
lib2.files = [
  '{ent}'
]
lib3.files = [
  '{pkg3}',
]
",
                pkg1 = parent0.join("pkg1.vhd").to_str().unwrap(),
                ent = parent1.join("ent.vhd").to_str().unwrap(),
                pkg3 = parent1.join("pkg3.vhd").to_str().unwrap()
            ),
            expected_parent,
        )
        .unwrap();

        let mut merged_config = config0;
        merged_config.append(&config1, &mut Vec::new());
        assert_eq!(merged_config, expected_config);
    }

    #[test]
    fn test_warning_on_missing_file() {
        let parent = Path::new("parent_folder");
        let config = Config::from_str(
            "
[libraries]
lib.files = [
  'missing.vhd'
]
",
            parent,
        )
        .unwrap();

        let mut messages = vec![];
        let file_names = config.get_library("lib").unwrap().file_names(&mut messages);
        assert_files_eq(&file_names, &[]);
        assert_eq!(
            messages,
            vec![Message::warning(format!(
                "File {} does not exist",
                parent.join("missing.vhd").to_str().unwrap()
            ))]
        );
    }

    #[test]
    fn test_file_wildcard_pattern() {
        let tempdir = tempfile::tempdir().unwrap();
        let parent = tempdir.path();
        let config = Config::from_str(
            "
[libraries]
lib.files = [
  '*.vhd'
]
",
            parent,
        )
        .unwrap();

        let file1 = touch(parent, "file1.vhd");
        let file2 = touch(parent, "file2.vhd");

        let mut messages = vec![];
        let file_names = config.get_library("lib").unwrap().file_names(&mut messages);
        assert_files_eq(&file_names, &[file1, file2]);
        assert_eq!(messages, vec![]);
    }

    #[test]
    fn test_file_wildcard_pattern_removes_duplicates() {
        let tempdir = tempfile::tempdir().unwrap();
        let parent = tempdir.path();
        let config = Config::from_str(
            "
[libraries]
lib.files = [
  '*.vhd',
  'file*.vhd'
]
",
            parent,
        )
        .unwrap();

        let file1 = touch(parent, "file1.vhd");
        let file2 = touch(parent, "file2.vhd");

        let mut messages = vec![];
        let file_names = config.get_library("lib").unwrap().file_names(&mut messages);
        assert_files_eq(&file_names, &[file1, file2]);
        assert_eq!(messages, vec![]);
    }

    #[test]
    fn test_warning_on_emtpy_file_glob_pattern() {
        let parent = Path::new("parent_folder");
        let config = Config::from_str(
            "
[libraries]
lib.files = [
  'missing*.vhd'
]
",
            parent,
        )
        .unwrap();

        let mut messages = vec![];
        let file_names = config.get_library("lib").unwrap().file_names(&mut messages);
        assert_files_eq(&file_names, &[]);
        assert_eq!(
            messages,
            vec![Message::warning(format!(
                "Pattern '{}' did not match any file",
                parent.join("missing*.vhd").to_str().unwrap()
            ))]
        );
    }

    #[test]
    fn test_exclude_file_is_excluded() {
        let tempdir = tempfile::tempdir().unwrap();
        let parent = tempdir.path();
        let config = Config::from_str(
            "
[libraries]
lib.files = [
  '*.vhd'
]
lib.exclude = [
  'file2.vhd'
]
",
            parent,
        )
        .unwrap();

        let file1 = touch(parent, "file1.vhd");
        let _file2 = touch(parent, "file2.vhd");

        let mut messages = vec![];
        let file_names = config.get_library("lib").unwrap().file_names(&mut messages);
        assert_files_eq(&file_names, &[file1]);
        assert_eq!(messages, vec![]);
    }

    #[test]
    fn test_exclude_pattern_is_excluded() {
        let tempdir = tempfile::tempdir().unwrap();
        let parent = tempdir.path();
        let config = Config::from_str(
            "
[libraries]
lib.files = [
  '*.vhd'
]
lib.exclude = [
  'b*.vhd'
]
",
            parent,
        )
        .unwrap();

        let a1 = touch(parent, "a1.vhd");
        let a2 = touch(parent, "a2.vhd");
        let _b1 = touch(parent, "b1.vhd");
        let _b2 = touch(parent, "b2.vhd");

        let mut messages = vec![];
        let file_names = config.get_library("lib").unwrap().file_names(&mut messages);
        assert_files_eq(&file_names, &[a1, a2]);
        assert_eq!(messages, vec![]);
    }

    #[test]
    fn the_work_library_is_an_illegal_library() {
        let parent = Path::new("parent_folder");
        let config = Config::from_str(
            "
[libraries]
work.files = [
  'a.vhd', 'b.vhd'
]
",
            parent,
        );
        assert_eq!(config.expect_err("Expected erroneous config"), "The 'work' library is not a valid library.\nHint: To use a library that contains all files, use a common name for all libraries, i.e., 'defaultlib'")
    }

    #[test]
    #[cfg(unix)]
    fn substitute() {
        let mut map = HashMap::new();
        map.insert("A".to_owned(), "a".to_owned());
        map.insert("ABCD".to_owned(), "abcd".to_owned());
        map.insert("A_0".to_owned(), "a0".to_owned());
        map.insert("_".to_owned(), "u".to_owned());
        map.insert("PATH".to_owned(), "some/path".to_owned());

        // simple pattern tests
        assert_eq!(
            Ok("test".to_owned()),
            substitute_environment_variables("test", &map)
        );
        assert_eq!(
            Ok("a".to_owned()),
            substitute_environment_variables("$A", &map)
        );
        assert_eq!(
            Ok("abcd".to_owned()),
            substitute_environment_variables("$ABCD", &map)
        );
        assert_eq!(
            Ok("a0".to_owned()),
            substitute_environment_variables("$A_0", &map)
        );
        assert_eq!(
            Ok("u".to_owned()),
            substitute_environment_variables("$_", &map)
        );
        assert_eq!(
            Ok("some/path".to_owned()),
            substitute_environment_variables("$PATH", &map)
        );

        // embedded in longer string
        assert_eq!(
            Ok("test/a/test".to_owned()),
            substitute_environment_variables("test/$A/test", &map)
        );
        assert_eq!(
            Ok("test/a".to_owned()),
            substitute_environment_variables("test/$A", &map)
        );
        assert_eq!(
            Ok("a/test".to_owned()),
            substitute_environment_variables("$A/test", &map)
        );
        assert_eq!(
            Ok("test/some/path/test".to_owned()),
            substitute_environment_variables("test/$PATH/test", &map)
        );

        // error cases
        assert!(substitute_environment_variables("$not_present", &map).is_err());
        assert!(substitute_environment_variables("$not_unicode", &map).is_err());
    }

    #[test]
    fn windows_variable_names() {
        let mut map = HashMap::new();
        map.insert("A".to_owned(), "a".to_owned());
        map.insert("ABCD".to_owned(), "abcd".to_owned());
        map.insert("A_0".to_owned(), "a0".to_owned());
        map.insert("_".to_owned(), "u".to_owned());
        map.insert("PATH".to_owned(), r#"some\path"#.to_owned());

        assert_eq!(Ok("".to_owned()), substitute_variables_windows("", &map));
        assert_eq!(
            Ok("test".to_owned()),
            substitute_variables_windows("test", &map)
        );
        assert_eq!(
            Ok("a".to_owned()),
            substitute_variables_windows("%A%", &map)
        );
        assert_eq!(
            Ok("abcd".to_owned()),
            substitute_variables_windows("%ABCD%", &map)
        );
        assert_eq!(
            Ok("a0".to_owned()),
            substitute_variables_windows("%A_0%", &map)
        );
        assert_eq!(
            Ok("u".to_owned()),
            substitute_variables_windows("%_%", &map)
        );
        assert_eq!(
            Ok(r#"some\path"#.to_owned()),
            substitute_variables_windows("%PATH%", &map)
        );

        // embedded in longer string
        assert_eq!(
            Ok(r#"test\a\test"#.to_owned()),
            substitute_variables_windows(r#"test\%A%\test"#, &map)
        );
        assert_eq!(
            Ok(r#"test\a"#.to_owned()),
            substitute_variables_windows(r#"test\%A%"#, &map)
        );
        assert_eq!(
            Ok(r#"a\test"#.to_owned()),
            substitute_variables_windows(r#"%A%\test"#, &map)
        );
        assert_eq!(
            Ok(r#"C:\test\some\path\test"#.to_owned()),
            substitute_variables_windows(r#"C:\test\%PATH%\test"#, &map)
        );

        // error cases
        assert_eq!(
            substitute_variables_windows("%not_present%", &map),
            Err("Variable 'not_present' not found".into())
        );
        assert!(substitute_variables_windows("%not_unicode%", &map).is_err());
    }

    // Issue #278
    #[test]
    #[cfg(windows)]
    fn substitute_ok_windows_paths() {
        let map: HashMap<String, String> = HashMap::default();
        let str = r#"\\networklocation\cad$\apps\xilinx_vitis\Vivado_2020.2\Vivado\2020.2\data\vhdl\src\unisims\unisim_VCOMP.vhd"#;
        let res = substitute_environment_variables(str, &map);
        assert_eq!(res, Ok(str.to_owned()));
    }
}
