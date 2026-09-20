use vhdl_syntax::{
    latin_1::{Latin1Str, Latin1String},
    parser::error::Span,
    standard::VHDLStandard,
    syntax::SyntaxToken,
    tokens::{token::requires_separator, Keyword, Token, TokenKind, TriviaBuf},
};

#[derive(Debug, Clone)]
pub struct Edit {
    span: Span,
    replacement: Box<Latin1Str>,
}

impl Edit {
    // Note: technically, we should also allow non-latin1 strings (e.g., UTF-8)
    // to support UTF-8 replacements in comments.
    // However, there are no convincing use-cases (deletion still works)
    // and so we currently restrict an Edit to be Latin-1.
    // This simplifies things when rendering a diagnostic.
    // Non Latin-1 can be supported by adding an encoding field here, if the
    // use-case exists.
    pub fn new(span: Span, replacement: impl Into<Box<Latin1Str>>) -> Edit {
        debug_assert!(span.start <= span.end);
        Edit {
            span,
            replacement: replacement.into(),
        }
    }

    pub fn delete_raw(span: Span) -> Edit {
        Edit::new(span, b"")
    }

    pub fn span(&self) -> &Span {
        &self.span
    }

    pub fn replacement(&self) -> &Latin1Str {
        &self.replacement
    }
}

#[derive(Debug, Clone)]
pub struct TokenData {
    pub(crate) kind: TokenKind,
    pub(crate) text: Latin1String,
}

impl TokenData {
    pub fn new(kind: TokenKind, text: impl Into<Latin1String>) -> TokenData {
        TokenData {
            kind,
            text: text.into(),
        }
    }
}

impl From<Keyword> for TokenData {
    fn from(value: Keyword) -> Self {
        TokenData::new(TokenKind::Keyword(value), value.canonical_text())
    }
}

impl From<Token> for TokenData {
    fn from(value: Token) -> Self {
        TokenData::new(value.kind(), value.text())
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Edits {
    standard: VHDLStandard,
}

impl Edits {
    pub(crate) fn new(standard: VHDLStandard) -> Edits {
        Edits { standard }
    }

    pub fn delete(&self, token: &SyntaxToken) -> Edit {
        // Check if we need to insert a separator since adjacent tokens could merge
        if let (Some(prev_token), Some(next_token)) = (token.prev_token(), token.next_token()) {
            let requires_sep =
                requires_separator(prev_token.token(), next_token.token(), self.standard);
            if !requires_sep {
                Edit::delete_raw(token.text_range())
            } else if token.leading_trivia().is_empty() && next_token.leading_trivia().is_empty() {
                Edit::new(token.text_range(), b" ")
            } else {
                Edit::delete_raw(token.text_range())
            }
        } else {
            Edit::delete_raw(token.text_range())
        }
    }

    pub fn insert_after(&self, token: &SyntaxToken, replacement: impl Into<TokenData>) -> Edit {
        let replacement = replacement.into();
        let new_token = Token::new(replacement.kind, replacement.text, TriviaBuf::default());
        let requires_sep = requires_separator(token.token(), &new_token, self.standard);
        // Keywords usually look nicer with a leading space.
        let mut replacement = if matches!(new_token.kind(), TokenKind::Keyword(_)) || requires_sep {
            Latin1String::from(b" ")
        } else {
            Latin1String::new()
        };
        replacement.push_str(new_token.text());
        let requires_sep_after = token.trailing_trivia().is_empty();
        if requires_sep_after {
            replacement.push(b' ');
        }
        Edit::new(token.range().end..token.range().end, replacement)
    }
}

#[derive(Debug, Clone)]
pub struct Fix {
    title: String,
    // If modifying after construction is necessary, ensure the `Vec` remains sorted.
    edits: Vec<Edit>,
    applicability: Applicability,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Applicability {
    /// Can be automatically applied when using `--fix`
    Safe,
    /// Only used to display to the user
    DisplayOnly,
}

impl Fix {
    /// A fix that keeps the meaning of the code it rewrites.
    ///
    /// # Invariants
    ///
    /// The `edits` of one fix are applied as a unit, and must therefore satisfy:
    ///
    /// - every span is ordered, i.e., `start <= end`,
    /// - every span lies within the source that the rule was invoked on,
    /// - no two spans overlap. Touching is fine: `0..3` and `3..5` are disjoint.
    // Note: This constructor name is due to a potential future "unsafe" fix
    // (same concept as ruff / clippy / eslint). Currently there are no rules with
    // unsafe fixes.
    pub fn safe(title: impl Into<String>, edits: Vec<Edit>) -> Fix {
        Fix::new(title, edits, Applicability::Safe)
    }

    pub fn display_only(title: impl Into<String>, edits: Vec<Edit>) -> Fix {
        Fix::new(title, edits, Applicability::DisplayOnly)
    }

    pub fn is_safe(&self) -> bool {
        self.applicability == Applicability::Safe
    }

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
            .all(|edit| edit.span.start <= edit.span.end && edit.span.end <= input.len());
        debug_assert!(
            in_bounds,
            "fix '{}' has an unordered edit or an edit outside the source of {} bytes",
            fix.title(),
            input.len()
        );
        // `Fix::safe` sorts the edits, so comparing neighbours finds every overlap
        let disjoint = fix
            .edits()
            .windows(2)
            .all(|win| win[0].span.end <= win[1].span.start);
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

    let mut result = Vec::with_capacity(input.len());
    let mut pos = 0;

    for edit in edits {
        debug_assert!(
            edit.span.start >= pos,
            "applied fixes must have disjoint extents"
        );
        result.extend_from_slice(&input[pos..edit.span.start]);
        result.extend_from_slice(edit.replacement.as_bytes());
        pos = edit.span.end;
    }
    result.extend_from_slice(&input[pos..]);

    (result, applied_fixes)
}

#[cfg(test)]
mod tests {
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
        let fix = Fix::safe(
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
        let first = Fix::safe("first", vec![Edit::delete_raw(1..3)]);
        let overlapping = Fix::safe("overlapping", vec![Edit::delete_raw(2..4)]);
        let touching = Fix::safe("touching", vec![Edit::delete_raw(3..4)]);
        let later = Fix::safe("later", vec![Edit::new(5..5, b"-")]);
        let (output, applied) = apply_fixes(b"abcdef", &[&first, &overlapping, &touching, &later]);
        assert_eq!(output, b"ade-f");
        assert_eq!(applied, 2);
    }

    #[test]
    fn edits_within_one_fix_are_sorted_and_may_touch() {
        let fix = Fix::safe("fix", vec![Edit::new(3..4, b"D"), Edit::new(1..3, b"BC")]);
        assert_eq!(fix.extent(), 1..4);
        let (output, applied) = apply_fixes(b"abcdef", &[&fix]);
        assert_eq!(output, b"aBCDef");
        assert_eq!(applied, 1);
    }

    #[test]
    #[cfg_attr(debug_assertions, should_panic(expected = "outside the source"))]
    fn a_fix_outside_the_source_is_dropped() {
        let fix = Fix::safe("out of bounds", vec![Edit::delete_raw(4..10)]);
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
        let valid = Fix::safe("valid", vec![Edit::delete_raw(4..5)]);
        let (output, applied) = apply_fixes(b"abcdef", &[&overlapping, &valid]);
        assert_eq!(output, b"abcdf");
        assert_eq!(applied, 1);
    }
}
