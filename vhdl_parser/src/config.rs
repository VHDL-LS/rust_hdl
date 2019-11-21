// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

//! Configuration of the design hierarchy and other settings

use toml;

use self::fnv::FnvHashMap;
use self::toml::Value;
use crate::message::Message;
use fnv;
use std::fs::File;
use std::io;
use std::io::prelude::*;
use std::path::Path;

#[derive(Clone, PartialEq, Default, Debug)]
pub struct Config {
    // A map from library name to file name
    libraries: FnvHashMap<String, LibraryConfig>,
}

#[derive(Clone, PartialEq, Default, Debug)]
pub struct LibraryConfig {
    name: String,
    files: Vec<String>,
}

impl LibraryConfig {
    /// Return a vector of file names
    /// Only include files that exists
    /// Files that do not exist produce a warning message
    pub fn file_names(&self, messages: &mut Vec<Message>) -> Vec<String> {
        let mut result = Vec::new();
        for file_name in self.files.iter() {
            if !Path::new(file_name).exists() {
                messages.push(Message::warning(
                    format! {"File {} does not exist", file_name},
                ));
            } else {
                result.push(file_name.clone());
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
            let file_arr = lib
                .get("files")
                .ok_or_else(|| format!("missing field files for library {}", name))?
                .as_array()
                .ok_or_else(|| format!("files for library {} is not array", name))?;

            let mut files = Vec::new();
            for file in file_arr.iter() {
                let file = file
                    .as_str()
                    .ok_or_else(|| format!("not a string {}", file))?;

                let path = parent.join(file);
                let path = path
                    .to_str()
                    .ok_or_else(|| format!("Could not convert {:?} to string", path))?
                    .to_owned();
                files.push(path);
            }

            libraries.insert(
                name.to_owned(),
                LibraryConfig {
                    name: name.to_owned(),
                    files,
                },
            );
        }

        Ok(Config { libraries })
    }

    pub fn read_file_path(file_name: &Path) -> io::Result<Config> {
        let mut file = File::open(file_name)?;
        let mut contents = String::new();
        file.read_to_string(&mut contents)?;

        let parent = file_name.parent().unwrap();

        Config::from_str(&contents, parent).map_err(|msg| io::Error::new(io::ErrorKind::Other, msg))
    }

    pub fn get_library<'a>(&'a self, name: &str) -> Option<&'a LibraryConfig> {
        self.libraries.get(name)
    }

    pub fn iter_libraries(&self) -> impl Iterator<Item = &LibraryConfig> {
        self.libraries.values()
    }

    /// Append another config to self
    ///
    /// In case of conflict the appended config takes precedence
    pub fn append(&mut self, config: &Config) {
        for library in config.iter_libraries() {
            if let Some(parent_library) = self.libraries.get_mut(&library.name) {
                for file_name in library.files.iter() {
                    parent_library.files.push(file_name.clone());
                }
            } else {
                self.libraries.insert(
                    library.name.clone(),
                    LibraryConfig {
                        name: library.name.clone(),
                        files: library.files.clone(),
                    },
                );
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;
    use tempfile;

    /// Utility function to create an empty file in parent folder
    fn touch(parent: &Path, file_name: &str) -> String {
        let path = parent.join(file_name);
        File::create(&path).expect("Assume file can be created");
        path.to_str().expect("Assume valid string").to_owned()
    }

    #[test]
    fn config_from_str() {
        let tempdir = tempfile::tempdir().unwrap();
        let parent = tempdir.path();

        let tempdir2 = tempfile::tempdir().unwrap();
        let absolute_path = tempdir2
            .path()
            .canonicalize()
            .expect("Assume valid abspath");
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
",
                absolute_vhd
            ),
            &parent,
        )
        .unwrap();
        let mut libraries: Vec<&str> = config.iter_libraries().map(|lib| lib.name()).collect();
        libraries.sort();
        assert_eq!(libraries, &["lib1", "lib2"]);

        let lib1 = config.get_library("lib1").unwrap();
        let lib2 = config.get_library("lib2").unwrap();

        let pkg1_path = touch(&parent, "pkg1.vhd");
        let pkg2_path = touch(&parent, "pkg2.vhd");
        let tb_ent_path = touch(&parent, "tb_ent.vhd");

        let mut messages = vec![];
        assert_eq!(lib1.file_names(&mut messages), &[pkg1_path, tb_ent_path]);
        assert_eq!(lib2.file_names(&mut messages), &[pkg2_path, absolute_vhd]);
        assert_eq!(messages, vec![]);
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
            &parent0,
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
            &parent1,
        )
        .unwrap();

        let expected_parent = Path::new("");
        let expected_config = Config::from_str(
            "
[libraries]
lib1.files = [
  'parent_folder0/pkg1.vhd',
]
lib2.files = [
  'parent_folder0/pkg2.vhd',
  'parent_folder1/ent.vhd'
]
lib3.files = [
  'parent_folder1/pkg3.vhd',
]
",
            &expected_parent,
        )
        .unwrap();

        let mut merged_config = config0.clone();
        merged_config.append(&config1);
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
            &parent,
        )
        .unwrap();

        let mut messages = vec![];
        let file_names = config.get_library("lib").unwrap().file_names(&mut messages);
        let expected: Vec<String> = Vec::new();
        assert_eq!(file_names, expected);
        assert_eq!(
            messages,
            vec![Message::warning(
                "File parent_folder/missing.vhd does not exist"
            )]
        );
    }
}
