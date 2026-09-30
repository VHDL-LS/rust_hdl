pub mod edit;

use vhdl_syntax::parser::error::Span;

use crate::fix::edit::Edit;

#[derive(Debug, Clone)]
pub struct Fix {
    title: String,
    // If modifying after construction is necessary, ensure the `Vec` remains sorted.
    edits: Vec<Edit>,
    applicability: Applicability,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, serde::Serialize)]
#[serde(rename_all = "kebab-case")]
pub enum Applicability {
    /// Can be automatically applied when using `--fix`
    Safe,
    /// Would change the meaning of code or remove comments
    Unsafe,
    /// Only used to display to the user
    DisplayOnly,
}

impl Fix {
    /// A fix that keeps the meaning of the code it rewrites.
    pub fn safe_edits(title: impl Into<String>, edits: Vec<Edit>) -> Fix {
        Fix::new(title, edits, Applicability::Safe)
    }

    pub fn unsafe_edits(title: impl Into<String>, edits: Vec<Edit>) -> Fix {
        Fix::new(title, edits, Applicability::Unsafe)
    }

    pub fn display_only_edits(title: impl Into<String>, edits: Vec<Edit>) -> Fix {
        Fix::new(title, edits, Applicability::DisplayOnly)
    }

    pub fn is_fixeable(&self, allow_unsafe: bool) -> bool {
        self.applicability_with_unsafe_fixes(allow_unsafe) == Applicability::Safe
    }

    pub fn applicability_with_unsafe_fixes(&self, allow_unsafe: bool) -> Applicability {
        match self.applicability {
            Applicability::Safe => Applicability::Safe,
            Applicability::Unsafe => {
                if allow_unsafe {
                    Applicability::Safe
                } else {
                    Applicability::Unsafe
                }
            }
            Applicability::DisplayOnly => Applicability::DisplayOnly,
        }
    }

    /// Create a new fix
    ///
    /// # Invariants
    ///
    /// The `edits` of one fix are applied as a unit, and must therefore satisfy:
    ///
    /// - every span is ordered, i.e., `start <= end`,
    /// - every span lies within the source that the rule was invoked on,
    /// - no two spans overlap. Touching is fine: `0..3` and `3..5` are disjoint.
    pub fn new(
        title: impl Into<String>,
        mut edits: Vec<Edit>,
        applicability: Applicability,
    ) -> Fix {
        assert!(!edits.is_empty());
        let title = title.into();
        edits.sort_by_key(|edit| (edit.span().start, edit.span().end));
        #[cfg(debug_assertions)]
        {
            for win in edits.windows(2) {
                debug_assert!(
                    win[0].span().end <= win[1].span().start,
                    "edits {:?} and {:?} of fix '{}' overlap",
                    win[0].span(),
                    win[1].span(),
                    title
                );
            }
        }
        Fix {
            title,
            edits,
            applicability,
        }
    }

    pub fn edits(&self) -> &[Edit] {
        &self.edits
    }

    pub fn title(&self) -> &str {
        &self.title
    }

    pub fn applicability(&self) -> Applicability {
        self.applicability
    }
}

impl Fix {
    pub fn extent(&self) -> Span {
        self.edits().first().unwrap().span().start..self.edits().last().unwrap().span().end
    }
}

/// Applies as many of `fixes` to `input` as can be applied at once.
///
/// Fixes need to be sorted.
/// A fix with an unordered span, a span outside `input` or overlapping edits is a bug in the
/// rule that produced it: debug builds panic, release builds drop the fix.
pub(crate) fn apply_fixes(input: &[u8], fixes: &[&Fix]) -> (Vec<u8>, usize) {
    let mut edits: Vec<&Edit> = Vec::new();

    let mut applied_fixes = 0usize;
    for fix in fixes {
        let in_bounds = fix
            .edits()
            .iter()
            .all(|edit| edit.span().start <= edit.span().end && edit.span().end <= input.len());
        debug_assert!(
            in_bounds,
            "fix '{}' has an unordered edit or an edit outside the source of {} bytes",
            fix.title(),
            input.len()
        );
        // `Fix::new` sorts the edits, so comparing neighbours finds every overlap
        let disjoint = fix
            .edits()
            .windows(2)
            .all(|win| win[0].span().end <= win[1].span().start);
        debug_assert!(disjoint, "fix '{}' has overlapping edits", fix.title());
        if !in_bounds || !disjoint {
            continue;
        }
        // Only apply non-overlapping fixes
        if edits
            .last()
            .is_some_and(|edit| fix.extent().start <= edit.span().end)
        {
            continue;
        }
        applied_fixes += 1;
        edits.extend(fix.edits());
    }

    let result = apply_sorted_edits(input, &edits);

    (result, applied_fixes)
}

/// Apply edits
///
/// # Invariants
/// - Edits must be sorted
/// - Edits must be in bounds, i.e., for each edit `edit.span().start <= edit.span().end` and `edit.span().end <= input.len()`
/// - Edits cannot overlap, i.e., for each edit: `prev_edit.span().end <= next_edit.span().start`
pub(crate) fn apply_sorted_edits(input: &[u8], edits: &[&Edit]) -> Vec<u8> {
    let mut result = Vec::with_capacity(input.len());
    let mut pos = 0;

    for edit in edits {
        debug_assert!(
            edit.span().start >= pos,
            "applied fixes must have disjoint extents"
        );
        result.extend_from_slice(&input[pos..edit.span().start]);
        result.extend_from_slice(edit.replacement().as_bytes());
        pos = edit.span().end;
    }
    result.extend_from_slice(&input[pos..]);

    result
}

#[cfg(test)]
mod tests {
    use crate::fix::edit::Edits;

    use super::*;
    use vhdl_syntax::{
        parser::parse,
        standard::VHDLStandard,
        syntax::{NodeKind, TokenKind},
    };

    /// Output of deleting both parentheses of the first `if` condition in `statement`.
    fn delete_parens(statement: &str) -> String {
        let source =
            format!("architecture a of e is begin process begin {statement} end process; end;");
        let (design, errors) = parse(source.as_bytes());
        assert!(errors.is_empty(), "{errors:?}");
        let condition = design
            .descendants()
            .find(|node| node.kind() == NodeKind::ParenthesizedExpressionOrAggregate)
            .unwrap();
        let parens = condition
            .children_with_tokens()
            .filter_map(|child| child.as_token())
            .filter(|token| matches!(token.kind(), TokenKind::LeftPar | TokenKind::RightPar))
            .collect::<Vec<_>>();
        let edits = Edits::new(VHDLStandard::default());
        let fix = Fix::safe_edits(
            "parens",
            parens.iter().map(|tok| edits.delete(tok)).collect(),
        );
        let (output, _) = apply_fixes(source.as_bytes(), &[&fix]);
        let output = String::from_utf8(output).unwrap();
        let body = output.strip_prefix("architecture a of e is begin process begin ");
        body.unwrap()
            .strip_suffix(" end process; end;")
            .unwrap()
            .to_owned()
    }

    #[test]
    fn deleting_a_token_keeps_adjacent_tokens_apart() {
        assert_eq!(delete_parens("if(a)then end if;"), "if a then end if;");
    }

    #[test]
    fn deleting_a_token_inserts_no_space_where_trivia_already_separates() {
        assert_eq!(delete_parens("if (a) then end if;"), "if a then end if;");
        assert_eq!(delete_parens("if(a) then end if;"), "if a then end if;");
    }

    #[test]
    fn deleting_a_token_inserts_no_space_where_none_is_needed() {
        assert_eq!(
            delete_parens("if(a+b)=c then end if;"),
            "if a+b=c then end if;"
        );
    }

    #[test]
    fn a_fix_overlapping_or_touching_an_earlier_fix_is_skipped() {
        let first = Fix::safe_edits("first", vec![Edit::delete_raw(1..3)]);
        let overlapping = Fix::safe_edits("overlapping", vec![Edit::delete_raw(2..4)]);
        let touching = Fix::safe_edits("touching", vec![Edit::delete_raw(3..4)]);
        let later = Fix::safe_edits("later", vec![Edit::new(5..5, b"-")]);
        let (output, applied) = apply_fixes(b"abcdef", &[&first, &overlapping, &touching, &later]);
        assert_eq!(output, b"ade-f");
        assert_eq!(applied, 2);
    }

    #[test]
    fn edits_within_one_fix_are_sorted_and_may_touch() {
        let fix = Fix::safe_edits("fix", vec![Edit::new(3..4, b"D"), Edit::new(1..3, b"BC")]);
        assert_eq!(fix.extent(), 1..4);
        let (output, applied) = apply_fixes(b"abcdef", &[&fix]);
        assert_eq!(output, b"aBCDef");
        assert_eq!(applied, 1);
    }

    #[test]
    #[cfg_attr(debug_assertions, should_panic(expected = "outside the source"))]
    fn a_fix_outside_the_source_is_dropped() {
        let fix = Fix::safe_edits("out of bounds", vec![Edit::delete_raw(4..10)]);
        let (output, applied) = apply_fixes(b"abcdef", &[&fix]);
        assert_eq!(output, b"abcdef");
        assert_eq!(applied, 0);
    }

    #[test]
    #[cfg_attr(debug_assertions, should_panic(expected = "overlapping edits"))]
    fn a_fix_with_overlapping_edits_is_dropped() {
        // Built directly, since `Fix::safe` rejects overlapping edits in debug builds
        let overlapping = Fix {
            title: "overlapping".to_owned(),
            edits: vec![Edit::delete_raw(0..2), Edit::delete_raw(1..3)],
            applicability: Applicability::Safe,
        };
        let valid = Fix::safe_edits("valid", vec![Edit::delete_raw(4..5)]);
        let (output, applied) = apply_fixes(b"abcdef", &[&overlapping, &valid]);
        assert_eq!(output, b"abcdf");
        assert_eq!(applied, 1);
    }
}
