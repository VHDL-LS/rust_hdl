// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

//! Configuration of the design hierarchy and other settings

use std::env;
use std::fs::File;
use std::io;
use std::io::prelude::*;
use std::path::Path;

use fnv::FnvHashMap;
use subst::VariableMap;
use toml::Value;

use crate::data::error_codes::ErrorCode;
use crate::data::*;

#[derive(Clone, PartialEq, Eq, Default, Debug)]
pub struct Config {
    // A map from library name to file name
    libraries: FnvHashMap<String, LibraryConfig>,
    error_codes: FnvHashMap<ErrorCode, Option<Severity>>,
}

#[derive(Clone, PartialEq, Eq, Default, Debug)]
pub struct LibraryConfig {
    name: String,
    patterns: Vec<String>,
    pub(crate) is_third_party: bool,
}

impl LibraryConfig {
    /// Return a vector of file names
    /// Only include files that exists
    /// Files that do not exist produce a warning message
    pub fn file_names(&self, messages: &mut dyn MessageHandler) -> Vec<PathBuf> {
        let mut result = Vec::new();
        for pattern in self.patterns.iter() {
            let stripped_pattern = if cfg!(windows) {
                pattern.strip_prefix("\\\\?\\").unwrap_or(pattern.as_str())
            } else {
                pattern.as_str()
            };

            if is_literal(stripped_pattern) {
                let file_path = Path::new(pattern).to_owned();

                if file_path.exists() {
                    result.push(file_path);
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
                                    result.push(file_path);
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
        Self::remove_duplicates(result)
    }

    /// Remove duplicate file names from the result
    fn remove_duplicates(file_names: Vec<PathBuf>) -> Vec<PathBuf> {
        let mut result = Vec::with_capacity(file_names.len());
        let mut fileset = std::collections::HashSet::new();

        for file_name in file_names.into_iter() {
            if fileset.insert(file_name.clone()) {
                result.push(file_name);
            }
        }
        result
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

        let libs = config
            .get("libraries")
            .ok_or("missing field libraries")?
            .as_table()
            .ok_or("libraries must be a table")?;

        for (name, lib) in libs.iter() {
            if name.to_lowercase() == "work" {
                return Err(format!(
                    "The '{}' library is not a valid library.\nHint: To use a library that contains all files, use a common name for all libraries, i.e., 'defaultlib'",
                    name
                ));
            }

            let file_arr = lib
                .get("files")
                .ok_or_else(|| format!("missing field files for library {name}"))?
                .as_array()
                .ok_or_else(|| format!("files for library {name} is not array"))?;

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
                    is_third_party,
                },
            );
        }

        let mut error_code_overwrites = FnvHashMap::default();

        if let Some(error_codes) = config.get("lint") {
            let error_codes = error_codes.as_table().ok_or("lint must be a table")?;
            for (name, severity) in error_codes.iter() {
                let error_code = ErrorCode::try_from(name.as_str())
                    .map_err(|_| format!("'{name}' is not a valid error code"))?;
                match severity.as_bool() {
                    Some(should_show) => {
                        if !should_show {
                            error_code_overwrites.insert(error_code, None);
                        }
                    }
                    None => {
                        let severity = Severity::try_from(
                            severity.as_str().ok_or("Severity must be a string")?,
                        )
                        .map_err(|_| format!("'{severity}' is not a valid severity level"))?;
                        error_code_overwrites.insert(error_code, Some(severity));
                    }
                }
            }
        }

        Ok(Config {
            libraries,
            error_codes: error_code_overwrites,
        })
    }

    pub fn read_file_path(file_name: &Path) -> io::Result<Config> {
        let mut file = File::open(file_name)?;
        let mut contents = String::new();
        file.read_to_string(&mut contents)?;

        let parent = file_name.parent().unwrap();

        Config::from_str(&contents, parent).map_err(|msg| io::Error::new(io::ErrorKind::Other, msg))
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
        self.error_codes.extend(config.error_codes())
    }

    /// Load configuration file from installation folder
    fn load_installed_config(&mut self, messages: &mut dyn MessageHandler) {
        let search_paths = [
            "../vhdl_libraries",
            "../../vhdl_libraries",
            "/usr/lib/rust_hdl/vhdl_libraries",
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
    pub fn load_external_config(&mut self, messages: &mut dyn MessageHandler) {
        self.load_installed_config(messages);
        self.load_home_config(messages);
        self.load_env_config("VHDL_LS_CONFIG", messages);
    }

    pub fn error_codes(&self) -> &FnvHashMap<ErrorCode, Option<Severity>> {
        &self.error_codes
    }
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
    for chr in pattern.chars() {
        match chr {
            '?' | '*' | '[' => {
                return false;
            }
            _ => {}
        }
    }
    true
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

    fn assert_files_eq(got: &[PathBuf], expected: &[PathBuf]) {
        assert_eq!(abspaths(got), abspaths(expected));
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

[error_codes]
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

        let mut expected_map = FnvHashMap::default();
        expected_map.insert(ErrorCode::Unused, Some(Severity::Error));
        expected_map.insert(ErrorCode::Duplicate, None);
        assert_eq!(config.error_codes, expected_map)
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
    fn test_warning_on_emtpy_glob_pattern() {
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
