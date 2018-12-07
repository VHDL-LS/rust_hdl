// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

//! Configuration of the design hierarchy and other settings

extern crate toml;

extern crate fnv;
use self::fnv::FnvHashMap;
use self::toml::Value;
use std::fs::File;
use std::io;
use std::io::prelude::*;
use std::path::Path;

pub struct Config {
    // A map from library name to file name
    libraries: FnvHashMap<String, LibraryConfig>,
}

pub struct LibraryConfig {
    name: String,
    files: Vec<String>,
}

impl LibraryConfig {
    pub fn file_names(&self) -> &Vec<String> {
        &self.files
    }

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
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn config_from_str() {
        let parent = Path::new("parent_folder");

        let absolute_path = {
            let win_path = Path::new("C:\\");
            let unix_path = Path::new("/");

            if unix_path.is_absolute() {
                unix_path
            } else if win_path.is_absolute() {
                win_path
            } else {
                panic!("Cannot create absolute path");
            }
        };

        let absolute_vhd = absolute_path
            .join("absolute.vhd")
            .to_str()
            .unwrap()
            .to_owned();

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

        let pkg1_path = parent.join("pkg1.vhd").to_str().unwrap().to_owned();
        let pkg2_path = parent.join("pkg2.vhd").to_str().unwrap().to_owned();
        let tb_ent_path = parent.join("tb_ent.vhd").to_str().unwrap().to_owned();

        assert_eq!(lib1.file_names(), &[pkg1_path, tb_ent_path]);
        assert_eq!(lib2.file_names(), &[pkg2_path, absolute_vhd]);
    }

}
