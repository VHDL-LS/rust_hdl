use std::fmt;
use std::str::FromStr;

use itertools::Itertools;

use crate::error_code::{Category, ErrorCode, ParseErrorCodeErr};

/// The set of rules an `--select` or `--ignore` argument refers to.
#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum RuleSelector {
    All,
    Category(Category),
    Code(ErrorCode),
}

/// The text spelling [RuleSelector::All].
const ALL: &str = "ALL";

/// The string does not name a [RuleSelector].
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParseRuleSelectorErr {
    /// The selector is empty.
    Empty,
    /// The selector is all letters, but names neither `ALL` nor a category.
    UnknownCategory(String),
    /// The selector names a real category, but not one the user may configure.
    NotConfigurable(Category),
    /// The selector looks like an error code, but is not one.
    Code(ParseErrorCodeErr),
}

impl fmt::Display for ParseRuleSelectorErr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseRuleSelectorErr::Empty => f.write_str("empty rule selector"),
            ParseRuleSelectorErr::UnknownCategory(found) => write!(
                f,
                "'{found}' is not a rule selector (expected '{ALL}', one of {}, or an error code such as 'IDM001')",
                Category::ALL
                    .iter()
                    .filter(|category| category.is_configurable())
                    .map(Category::prefix)
                    .join(", ")
            ),
            ParseRuleSelectorErr::NotConfigurable(category) => write!(
                f,
                "'{category}' diagnostics are always reported and cannot be selected or ignored"
            ),
            ParseRuleSelectorErr::Code(err) => err.fmt(f),
        }
    }
}

impl std::error::Error for ParseRuleSelectorErr {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            ParseRuleSelectorErr::Empty
            | ParseRuleSelectorErr::UnknownCategory(_)
            | ParseRuleSelectorErr::NotConfigurable(_) => None,
            ParseRuleSelectorErr::Code(err) => Some(err),
        }
    }
}

/// `category`, if the user may configure it.
fn configurable(category: Category) -> Result<Category, ParseRuleSelectorErr> {
    if category.is_configurable() {
        Ok(category)
    } else {
        Err(ParseRuleSelectorErr::NotConfigurable(category))
    }
}

impl FromStr for RuleSelector {
    type Err = ParseRuleSelectorErr;

    /// Parses a selector from `ALL`, a category prefix such as `IDM`, or a
    /// full error code such as `IDM001`. Matching is case-insensitive.
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let s = s.trim();
        if s.is_empty() {
            Err(ParseRuleSelectorErr::Empty)
        } else if s.eq_ignore_ascii_case(ALL) {
            Ok(RuleSelector::All)
        } else if s.bytes().all(|byte| byte.is_ascii_alphabetic()) {
            // A bare category; anything else is a code, so that a typo in the
            // number is not reported as an unknown category.
            let category = s
                .parse::<Category>()
                .map_err(|_| ParseRuleSelectorErr::UnknownCategory(s.to_owned()))?;
            Ok(RuleSelector::Category(configurable(category)?))
        } else {
            let code = s.parse::<ErrorCode>().map_err(ParseRuleSelectorErr::Code)?;
            configurable(code.category())?;
            Ok(RuleSelector::Code(code))
        }
    }
}

impl fmt::Display for RuleSelector {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RuleSelector::All => f.write_str(ALL),
            RuleSelector::Category(category) => category.fmt(f),
            RuleSelector::Code(code) => code.fmt(f),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy, PartialOrd, Ord)]
pub struct Specificity(usize);

impl RuleSelector {
    /// whether the override matches the given error-code
    pub fn matches(&self, code: ErrorCode) -> bool {
        match self {
            RuleSelector::All => true,
            RuleSelector::Category(cat) => code.category() == *cat,
            RuleSelector::Code(selected) => code == *selected,
        }
    }

    /// How specific the rule matches. Higher is more specific
    pub fn specificity(&self) -> Specificity {
        Specificity(match self {
            RuleSelector::All => 0,
            RuleSelector::Category(_) => 1,
            RuleSelector::Code(_) => 2,
        })
    }
}

pub enum OverwriteResult {
    Ignore,
    Select,
    Default,
}

/// Resolved rule overwrites.
///
/// Resolution is by specificity: the most
/// precise selector that matches wins, so `--select ALL --ignore IDM --select IDM001`
/// leaves `IDM001` selected.
#[derive(Debug, Default)]
pub struct RuleOverwrites {
    ignores: Vec<RuleSelector>,
    selections: Vec<RuleSelector>,
}

impl RuleOverwrites {
    pub fn new(selections: Vec<RuleSelector>, ignores: Vec<RuleSelector>) -> RuleOverwrites {
        RuleOverwrites {
            ignores,
            selections,
        }
    }

    /// The specificity of the most precise selector in `selectors` that matches `code`,
    /// or `None` if none of them do.
    fn best_match(selectors: &[RuleSelector], code: ErrorCode) -> Option<Specificity> {
        selectors
            .iter()
            .filter(|selector| selector.matches(code))
            .map(RuleSelector::specificity)
            .max()
    }

    /// Get the action to be performed when applying the given rules.
    /// - [OverwriteResult::Ignore] -> Ignore the error specified via the code
    /// - [OverwriteResult::Select] -> Select the rules
    /// - [OverwriteResult::Default] -> Use whatever the default is
    pub fn get(&self, code: ErrorCode) -> OverwriteResult {
        if !code.category().is_configurable() {
            return OverwriteResult::Default;
        }
        let ignore = RuleOverwrites::best_match(&self.ignores, code);
        let select = RuleOverwrites::best_match(&self.selections, code);
        match (ignore, select) {
            (Some(ignore), Some(select)) => {
                if select > ignore {
                    OverwriteResult::Select
                } else {
                    OverwriteResult::Ignore
                }
            }
            (Some(_), None) => OverwriteResult::Ignore,
            (None, Some(_)) => OverwriteResult::Select,
            (None, None) => OverwriteResult::Default,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const IDM001: ErrorCode = ErrorCode::new(Category::Idiom, 1);
    const IDM002: ErrorCode = ErrorCode::new(Category::Idiom, 2);
    const SYX001: ErrorCode = ErrorCode::new(Category::Syntax, 1);

    fn parse(s: &str) -> Result<RuleSelector, ParseRuleSelectorErr> {
        s.parse::<RuleSelector>()
    }

    fn from_args(selections: &[&str], ignores: &[&str]) -> RuleOverwrites {
        let parse_all = |all: &[&str]| all.iter().map(|s| parse(s).unwrap()).collect::<Vec<_>>();
        RuleOverwrites::new(parse_all(selections), parse_all(ignores))
    }

    #[test]
    fn a_selector_is_parsed_from_all_a_category_or_a_code() {
        assert_eq!(parse("ALL"), Ok(RuleSelector::All));
        assert_eq!(parse("IDM"), Ok(RuleSelector::Category(Category::Idiom)));
        assert_eq!(parse("IDM001"), Ok(RuleSelector::Code(IDM001)));
    }

    #[test]
    fn parsing_is_case_insensitive_and_trims() {
        assert_eq!(parse("all"), Ok(RuleSelector::All));
        assert_eq!(parse(" idm "), Ok(RuleSelector::Category(Category::Idiom)));
        assert_eq!(parse("idm1"), Ok(RuleSelector::Code(IDM001)));
    }

    #[test]
    fn an_empty_selector_is_rejected() {
        assert_eq!(parse(""), Err(ParseRuleSelectorErr::Empty));
        assert_eq!(parse("  "), Err(ParseRuleSelectorErr::Empty));
    }

    #[test]
    fn an_all_letter_selector_is_reported_as_a_category_and_anything_else_as_a_code() {
        // So that a typo in the number is not reported as an unknown category.
        assert_eq!(
            parse("XYZ"),
            Err(ParseRuleSelectorErr::UnknownCategory("XYZ".to_owned()))
        );
        assert!(matches!(
            parse("IDM00x"),
            Err(ParseRuleSelectorErr::Code(_))
        ));
    }

    #[test]
    fn the_unknown_category_message_lists_only_configurable_categories() {
        let message = parse("XYZ").unwrap_err().to_string();
        assert!(message.contains("IDM"), "{message}");
        assert!(!message.contains("SYX"), "{message}");
    }

    #[test]
    fn a_non_configurable_category_is_rejected_as_a_selector() {
        assert_eq!(
            parse("SYX"),
            Err(ParseRuleSelectorErr::NotConfigurable(Category::Syntax))
        );
        assert_eq!(
            parse("SYX001"),
            Err(ParseRuleSelectorErr::NotConfigurable(Category::Syntax))
        );
        let message = parse("SYX").unwrap_err().to_string();
        assert!(message.contains("always reported"), "{message}");
    }

    #[test]
    fn a_selector_round_trips_through_its_displayed_form() {
        for spelling in ["ALL", "IDM", "IDM001"] {
            assert_eq!(parse(spelling).unwrap().to_string(), spelling);
        }
    }

    #[test]
    fn nothing_matching_leaves_the_rule_at_its_default() {
        let overwrites = from_args(&["IDM002"], &["IDM002"]);
        assert!(matches!(overwrites.get(IDM001), OverwriteResult::Default));
    }

    #[test]
    fn a_lone_selection_or_ignore_wins() {
        assert!(matches!(
            from_args(&["IDM001"], &[]).get(IDM001),
            OverwriteResult::Select
        ));
        assert!(matches!(
            from_args(&[], &["IDM001"]).get(IDM001),
            OverwriteResult::Ignore
        ));
    }

    #[test]
    fn the_more_specific_selector_wins_in_either_direction() {
        let overwrites = from_args(&["ALL", "IDM001"], &["IDM"]);
        assert!(matches!(overwrites.get(IDM001), OverwriteResult::Select));
        assert!(matches!(overwrites.get(IDM002), OverwriteResult::Ignore));

        let overwrites = from_args(&["IDM"], &["IDM001"]);
        assert!(matches!(overwrites.get(IDM001), OverwriteResult::Ignore));
        assert!(matches!(overwrites.get(IDM002), OverwriteResult::Select));
    }

    #[test]
    fn selectors_of_equal_specificity_resolve_to_ignore() {
        for (selections, ignores) in [
            (&["ALL"][..], &["ALL"][..]),
            (&["IDM"], &["IDM"]),
            (&["IDM001"], &["IDM001"]),
        ] {
            let overwrites = from_args(selections, ignores);
            assert!(
                matches!(overwrites.get(IDM001), OverwriteResult::Ignore),
                "{selections:?} vs {ignores:?}"
            );
        }
    }

    #[test]
    fn a_non_configurable_code_is_left_at_its_default_even_under_ignore_all() {
        let overwrites = from_args(&[], &["ALL"]);
        assert!(matches!(overwrites.get(SYX001), OverwriteResult::Default));
        assert!(matches!(overwrites.get(IDM001), OverwriteResult::Ignore));
    }
}
