use enum_map::{enum_map, Enum, EnumMap};
use fnv::FnvHashMap;
use std::str::FromStr;
use toml::Value;

/// Defines standard VHDL case conventions.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Case {
    /// All lower case, i.e., `std_logic_vector`
    Lower,
    /// All upper-case, i.e., `STD_LOGIC_VECTOR`
    Upper,
    /// Pascal case, i.e., `Std_Logic_Vector`
    Pascal,
    /// Keep the case without aplying any transformations
    Keep,
}

impl FromStr for Case {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "lower" | "snake" => Case::Lower,
            "upper" | "upper_snake" => Case::Upper,
            "pascal" | "upper_camel" => Case::Pascal,
            "keep" => Case::Keep,
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
            Case::Keep => {}
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

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, Enum, strum::EnumString)]
#[strum(serialize_all = "snake_case")]
pub enum Category {
    /// Constants and generics
    Constant,
    /// Design units (entities, architectures, packages)
    DesignUnit,
    /// Signals, Variables, Files
    Object,
    /// Functions and Procedures
    Subprogram,
    /// Types
    Type,
    /// Enumeration literal
    EnumLiteral,
    /// Statement labels.
    Label,
    /// Attrbiutes such as 'high, 'low, e.t.c.
    Attribute,
    /// Library names (ieee, std, ...)
    Library,
    /// Keywords
    Keyword,
    /// Aliases
    Alias,
}

pub type CategoryToCaseMap = EnumMap<Category, Case>;

pub fn default_category_to_case_map() -> CategoryToCaseMap {
    enum_map! {
        Category::Constant => Case::Upper,
        Category::Attribute => Case::Lower,
        Category::DesignUnit => Case::Lower,
        Category::EnumLiteral => Case::Upper,
        Category::Keyword => Case::Lower,
        Category::Label => Case::Lower,
        Category::Library => Case::Lower,
        Category::Object => Case::Lower,
        Category::Subprogram => Case::Lower,
        Category::Type => Case::Lower,
        Category::Alias => Case::Lower
    }
}

pub fn all_same_case_category_to_case_map(case: Case) -> CategoryToCaseMap {
    EnumMap::from_fn(|_| case)
}

#[derive(Eq, PartialEq, Debug, Copy, Clone, Enum, Default, strum::EnumString)]
#[strum(serialize_all = "snake_case")]
pub enum Preset {
    #[default]
    VhdlCommon,
    Keep,
}

impl Preset {
    pub fn to_category_case_map(&self) -> CategoryToCaseMap {
        match self {
            Preset::VhdlCommon => enum_map! {
                Category::Constant => Case::Upper,
                Category::Attribute => Case::Lower,
                Category::DesignUnit => Case::Lower,
                Category::EnumLiteral => Case::Upper,
                Category::Keyword => Case::Lower,
                Category::Label => Case::Lower,
                Category::Library => Case::Lower,
                Category::Object => Case::Lower,
                Category::Subprogram => Case::Lower,
                Category::Type => Case::Lower,
                Category::Alias => Case::Lower,
            },
            Preset::Keep => CategoryToCaseMap::from_fn(|_| Case::Keep),
        }
    }
}

#[derive(Debug, Clone)]
pub struct CasingConfig {
    pub preset: Preset,
    pub overrides: FnvHashMap<Category, Case>,
}

impl CasingConfig {
    pub fn into_category_case_map(self) -> CategoryToCaseMap {
        let mut ccm = self.preset.to_category_case_map();
        for (category, case) in self.overrides {
            ccm[category] = case
        }
        ccm
    }
}

impl CasingConfig {
    pub fn parse(root: &Value) -> Result<Option<CasingConfig>, String> {
        let Some(casing) = root.get("casing") else {
            return Ok(None);
        };
        let casing = casing
            .as_table()
            .ok_or("[casing] must be a table".to_string())?;

        let preset_str = casing
            .get("preset")
            .ok_or("preset must be present under 'casing'")?
            .as_str()
            .ok_or("casing.preset must be a string".to_string())?;

        let preset = Preset::from_str(preset_str)
            .map_err(|_| format!("Unknown casing preset '{preset_str}'"))?;

        let mut overrides = FnvHashMap::default();

        if let Some(Value::Table(tbl)) = casing.get("overrides") {
            for (key, value) in tbl {
                let kind =
                    Category::from_str(key).map_err(|_| format!("Unknown category '{key}'"))?;
                let style_str = value
                    .as_str()
                    .ok_or_else(|| format!("Override for '{}' must be a string", key))?;
                let style =
                    Case::from_str(style_str).map_err(|_| format!("Unknown case '{style_str}'"))?;
                overrides.insert(kind, style);
            }
        } else if casing.contains_key("overrides") {
            return Err("casing.overrides must be a table".into());
        }

        Ok(Some(CasingConfig { preset, overrides }))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use toml::Value;

    fn parse(toml_str: &str) -> Result<Option<CasingConfig>, String> {
        let value: Value = toml_str.parse().unwrap();
        CasingConfig::parse(&value)
    }

    #[test]
    fn disabled_when_missing() {
        let cfg = parse("").unwrap();
        assert!(cfg.is_none());
    }

    #[test]
    fn default_preset_no_overrides() {
        let cfg = parse(
            r#"
            [casing]
            preset = "vhdl_common"
            "#,
        )
        .unwrap()
        .unwrap();

        assert!(cfg.preset == Preset::VhdlCommon);
    }

    #[test]
    fn overrides_work() {
        let cfg = parse(
            r#"
            [casing]
            preset = "vhdl_common"

            [casing.overrides]
            constant = "upper"
            type = "pascal"
            "#,
        )
        .unwrap()
        .unwrap();

        assert_eq!(cfg.overrides.get(&Category::Constant), Some(&Case::Upper));
        assert_eq!(cfg.overrides.get(&Category::Type), Some(&Case::Pascal));
    }

    #[test]
    fn invalid_category_errors() {
        let err = parse(
            r#"
            [casing]
            preset = "vhdl_common"

            [casing.overrides]
            foo_bar = "upper"
            "#,
        )
        .unwrap_err();

        assert!(err.contains("Unknown category"));
    }

    #[test]
    fn invalid_casing_style_errors() {
        let err = parse(
            r#"
            [casing]
            preset = "dne"
            "#,
        )
        .unwrap_err();

        assert!(err.contains("Unknown casing preset"));
    }
}
