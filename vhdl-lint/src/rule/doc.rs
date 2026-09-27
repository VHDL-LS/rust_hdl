//! Rule documentation support
//!
//! Use the [`derive@Documented`] proc-macro to derive [`RuleDocs`] from the
//! code documentation automatically:
//!
//! ````text
//! /// Checks that the conditions of an `if` statement have no parentheses.
//! ///
//! /// # Non-compliant example
//! ///
//! /// ```vhdl,sequential-statement,non-compliant
//! /// if (condition) then
//! ///     foo <= bar;
//! /// end if;
//! /// ```
//! #[derive(Documented)]
//! pub struct NoParensAroundIf;
//! ````
//!
//! The first paragraph is the rule's summary. Every `vhdl` code block is an
//! [`Example`], and its info string must be `vhdl,<construct>,<compliance>`: the
//! [`Construct`] the code parses as, then `compliant` or `non-compliant`. Code blocks in other
//! languages are left alone.

pub use vhdl_lint_macros::Documented;

/// A rule that carries its documentation, usually derived with [`derive@Documented`].
pub trait Documented {
    const DOCS: &'static RuleDocs;
}

/// Documentation of a rule
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct RuleDocs {
    /// The rule's name, e.g. `no-parens-around-if`
    pub name: &'static str,

    /// The first paragraph of the documentation, on one line
    pub summary: &'static str,

    /// The full documentation, as Markdown
    pub text: &'static str,

    /// The examples in the documentation, in the order they appear
    pub examples: &'static [Example],
}

/// A code example in the documentation of a rule.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Example {
    pub kind: ExampleKind,
    /// What the code parses as.
    pub construct: Construct,
    pub code: &'static str,
}

/// Whether an example triggers the rule or not
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ExampleKind {
    /// The rule reports nothing for this code
    Compliant,
    /// The rule reports this code
    NonCompliant,
}

/// The syntactic construct an example is written as
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[non_exhaustive]
pub enum Construct {
    DesignUnit,
    SequentialStatement,
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Summary that
    /// spans two lines.
    ///
    /// ```vhdl,design-unit,non-compliant
    /// entity e is
    ///     port (clk : bit);
    /// end;
    /// ```
    ///
    /// ```text
    /// not an example
    /// ```
    #[derive(Documented)]
    #[allow(dead_code)]
    struct SomeRule;

    #[test]
    fn a_rule_documents_itself_with_its_doc_comment() {
        let docs = SomeRule::DOCS;
        assert_eq!(docs.name, "some-rule");
        assert_eq!(docs.summary, "Summary that spans two lines.");
        assert_eq!(
            docs.text,
            "\
Summary that
spans two lines.

```vhdl
entity e is
    port (clk : bit);
end;
```

```text
not an example
```"
        );
        assert_eq!(
            docs.examples,
            [Example {
                kind: ExampleKind::NonCompliant,
                construct: Construct::DesignUnit,
                code: "entity e is\n    port (clk : bit);\nend;\n",
            }]
        );
    }
}
