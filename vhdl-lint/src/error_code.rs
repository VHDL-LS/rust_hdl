use std::fmt;
use std::str::FromStr;

use strum::{EnumString, IntoStaticStr, VariantArray};

/// The group a rule belongs to.
///
/// A category is spelled as its prefix, both when displayed and when parsed;
/// parsing is case-insensitive.
#[derive(
    Copy,
    Clone,
    PartialEq,
    Eq,
    Hash,
    Debug,
    PartialOrd,
    Ord,
    EnumString,
    IntoStaticStr,
    VariantArray,
)]
#[strum(ascii_case_insensitive)]
pub enum Category {
    /// The source does not conform to the VHDL grammar.
    /// Should only be produced by the parser.
    #[strum(serialize = "SYX")]
    Syntax,
    /// There is a more idiomatic spelling.
    #[strum(serialize = "IDM")]
    Idiom,
}

impl Category {
    /// Every category, in the order they are presented to the user.
    pub const ALL: &'static [Category] = Category::VARIANTS;

    pub fn prefix(&self) -> &'static str {
        (*self).into()
    }

    /// Whether this category is configurable, i.e., can be overwritten by a user.
    pub const fn is_configurable(&self) -> bool {
        !matches!(self, Category::Syntax)
    }
}

impl fmt::Display for Category {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.prefix())
    }
}

/// Error code attached to a rule
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct ErrorCode {
    category: Category,
    number: u16,
}

impl ErrorCode {
    pub const fn new(category: Category, number: u16) -> ErrorCode {
        ErrorCode { category, number }
    }

    pub const fn category(&self) -> Category {
        self.category
    }

    pub const fn number(&self) -> u16 {
        self.number
    }
}

impl fmt::Display for ErrorCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{:03}", self.category.prefix(), self.number)
    }
}

/// The string does not name an [ErrorCode].
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParseErrorCodeErr {
    /// The leading prefix does not name a category.
    UnknownCategory,
    /// The number is invalid.
    NumberErr(String),
}

impl fmt::Display for ParseErrorCodeErr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseErrorCodeErr::UnknownCategory => {
                write!(f, "not a rule category")
            }
            ParseErrorCodeErr::NumberErr(err) => {
                write!(f, "{err}")
            }
        }
    }
}

impl std::error::Error for ParseErrorCodeErr {}

impl FromStr for ErrorCode {
    type Err = ParseErrorCodeErr;

    /// Parses an error code from its displayed form, e.g. `IDM001`.
    /// The category prefix is matched case-insensitively and the number may
    /// carry any number of leading zeroes.
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let split = s
            .find(|ch: char| !ch.is_ascii_alphabetic())
            .unwrap_or(s.len());
        let (prefix, number) = s.split_at(split);
        let category = prefix
            .parse::<Category>()
            .map_err(|_| ParseErrorCodeErr::UnknownCategory)?;
        if !number.bytes().all(|byte| byte.is_ascii_digit()) {
            return Err(ParseErrorCodeErr::NumberErr(
                "invalid non-digit character".to_string(),
            ));
        }
        let number = number
            .parse::<u16>()
            .map_err(|err| ParseErrorCodeErr::NumberErr(err.to_string()))?;
        Ok(ErrorCode::new(category, number))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn category_displays_as_its_prefix() {
        assert_eq!(Category::Syntax.to_string(), "SYX");
        assert_eq!(Category::Idiom.to_string(), "IDM");
    }

    #[test]
    fn error_code_is_padded_to_three_digits() {
        assert_eq!(ErrorCode::new(Category::Idiom, 1).to_string(), "IDM001");
        assert_eq!(ErrorCode::new(Category::Syntax, 42).to_string(), "SYX042");
    }

    #[test]
    fn error_code_beyond_three_digits_is_not_truncated() {
        assert_eq!(ErrorCode::new(Category::Idiom, 1234).to_string(), "IDM1234");
    }

    #[test]
    fn error_code_keeps_its_parts() {
        let code = ErrorCode::new(Category::Syntax, 7);
        assert_eq!(code.category(), Category::Syntax);
        assert_eq!(code.number(), 7);
    }

    #[test]
    fn error_code_is_parsed_from_its_displayed_form() {
        assert_eq!("IDM001".parse(), Ok(ErrorCode::new(Category::Idiom, 1)));
        assert_eq!("syx0042".parse(), Ok(ErrorCode::new(Category::Syntax, 42)));
    }

    #[test]
    fn malformed_error_codes_are_rejected() {
        assert_eq!(
            "ABC001".parse::<ErrorCode>(),
            Err(ParseErrorCodeErr::UnknownCategory)
        );
        assert_eq!(
            "001".parse::<ErrorCode>(),
            Err(ParseErrorCodeErr::UnknownCategory)
        );
        for malformed in ["IDM", "IDM-1", "IDM+1", "IDM1a", "IDM65536"] {
            assert!(
                matches!(
                    malformed.parse::<ErrorCode>(),
                    Err(ParseErrorCodeErr::NumberErr(_))
                ),
                "{malformed}"
            );
        }
    }

    #[test]
    fn codes_differing_only_in_category_are_distinct() {
        assert_ne!(
            ErrorCode::new(Category::Syntax, 1),
            ErrorCode::new(Category::Idiom, 1)
        );
    }
}
