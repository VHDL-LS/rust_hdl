//! Reads a rule's doc comment: its summary, its text, and its examples.
//!
//! The Markdown is read with pulldown-cmark, the parser rustdoc and mdBook use, so
//! that a code block is an example exactly when those render it as a code block.

use std::ops::Range;

use pulldown_cmark::{CodeBlockKind, Event, Parser, Tag, TagEnd};

/// A construct an example can be parsed as, spelled as in the info string and as
/// the variant of `vhdl_lint::rule::doc::Construct` it maps to.
///
/// Keep in sync with that enum; a name missing there fails to compile.
pub struct ConstructName {
    pub tag: &'static str,
    pub variant: &'static str,
}

pub const CONSTRUCTS: &[ConstructName] = &[
    ConstructName {
        tag: "design-unit",
        variant: "DesignUnit",
    },
    ConstructName {
        tag: "sequential-statement",
        variant: "SequentialStatement",
    },
];

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ExampleKind {
    Compliant,
    NonCompliant,
}

impl ExampleKind {
    const ALL: [ExampleKind; 2] = [ExampleKind::Compliant, ExampleKind::NonCompliant];

    fn tag(self) -> &'static str {
        match self {
            ExampleKind::Compliant => "compliant",
            ExampleKind::NonCompliant => "non-compliant",
        }
    }

    pub fn variant(self) -> &'static str {
        match self {
            ExampleKind::Compliant => "Compliant",
            ExampleKind::NonCompliant => "NonCompliant",
        }
    }
}

pub struct Example {
    pub kind: ExampleKind,
    pub construct: &'static ConstructName,
    pub code: String,
}

pub struct Docs {
    /// The first paragraph, on one line.
    pub summary: String,
    /// The whole doc comment, with the attributes of `vhdl` code blocks removed.
    pub text: String,
    pub examples: Vec<Example>,
}

#[derive(Debug)]
pub struct Error {
    /// The line of the input the error is about, if any.
    pub line: Option<usize>,
    pub message: String,
}

fn error(line: usize, message: impl Into<String>) -> Error {
    Error {
        line: Some(line),
        message: message.into(),
    }
}

fn is_blank(line: &str) -> bool {
    line.trim().is_empty()
}

fn indent(line: &str) -> usize {
    line.len() - line.trim_start().len()
}

fn construct_tags() -> String {
    CONSTRUCTS
        .iter()
        .map(|c| format!("`{}`", c.tag))
        .collect::<Vec<_>>()
        .join(", ")
}

/// Reads the `vhdl` example that an info string describes, or `None` for a block
/// in any other language.
///
/// An example is tagged `vhdl,<construct>,<compliant|non-compliant>`, in that order.
fn example_tags(
    info: &str,
    line: usize,
) -> Result<Option<(ExampleKind, &'static ConstructName)>, Error> {
    let mut attributes = info.split(',').map(str::trim);
    if attributes.next() != Some("vhdl") {
        return Ok(None);
    }
    let (Some(construct), Some(kind)) = (attributes.next(), attributes.next()) else {
        return Err(error(
            line,
            format!(
                "a vhdl example is tagged `vhdl,<construct>,<compliant|non-compliant>`, where the construct is one of {}",
                construct_tags()
            ),
        ));
    };
    let Some(construct) = CONSTRUCTS.iter().find(|c| c.tag == construct) else {
        return Err(error(
            line,
            format!(
                "unknown construct `{construct}`; expected one of {}",
                construct_tags()
            ),
        ));
    };
    let Some(kind) = ExampleKind::ALL.into_iter().find(|k| k.tag() == kind) else {
        return Err(error(
            line,
            format!("expected `compliant` or `non-compliant`, found `{kind}`"),
        ));
    };
    if let Some(extra) = attributes.next() {
        return Err(error(
            line,
            format!("unexpected `{extra}` after `{}`", kind.tag()),
        ));
    }
    Ok(Some((kind, construct)))
}

/// The range of the info string of the fenced code block starting at `start`.
///
/// The block's first line is its opening fence, possibly indented or inside a
/// block quote; the info string is whatever follows the fence on that line.
fn info_string_range(source: &str, start: usize) -> Range<usize> {
    let line_end = source[start..]
        .find('\n')
        .map_or(source.len(), |end| start + end);
    let line = &source[start..line_end];
    let fence_start = line
        .find(['`', '~'])
        .expect("a fenced code block starts with its fence");
    let marker = line[fence_start..].chars().next().unwrap();
    let fence = &line[fence_start..];
    let info_start = fence_start + fence.len() - fence.trim_start_matches(marker).len();
    start + info_start..line_end
}

/// Reads a doc comment, given as the lines of its `#[doc]` attributes.
///
/// Errors refer to lines by their index in `lines`.
pub fn parse(lines: &[String]) -> Result<Docs, Error> {
    // Like rustdoc, remove the indentation that every line shares
    let common = lines
        .iter()
        .filter(|line| !is_blank(line))
        .map(|line| indent(line))
        .min()
        .unwrap_or(0);
    let lines = lines
        .iter()
        .map(|line| line.get(common..).unwrap_or(""))
        .collect::<Vec<_>>();

    let Some(first) = lines.iter().position(|line| !is_blank(line)) else {
        return Err(Error {
            line: None,
            message: "a rule needs a doc comment; its first paragraph is the rule's summary"
                .to_owned(),
        });
    };
    let last = lines.iter().rposition(|line| !is_blank(line)).unwrap();
    let source = lines[first..=last].join("\n");
    let line_of = |offset: usize| first + source[..offset].matches('\n').count();

    let mut events = Parser::new(&source).into_offset_iter().peekable();

    let summary = match events.peek() {
        Some((Event::Start(Tag::Paragraph), range)) => source[range.clone()]
            .lines()
            .map(str::trim)
            .collect::<Vec<_>>()
            .join(" "),
        _ => {
            return Err(error(
                first,
                "a rule's doc comment must start with a one-paragraph summary",
            ))
        }
    };

    // The info strings of examples, to be replaced by a plain `vhdl`
    let mut replaced = Vec::<Range<usize>>::new();
    let mut examples = Vec::new();
    while let Some((event, range)) = events.next() {
        let Event::Start(Tag::CodeBlock(CodeBlockKind::Fenced(info))) = event else {
            continue;
        };
        let Some((kind, construct)) = example_tags(&info, line_of(range.start))? else {
            continue;
        };
        let mut code = String::new();
        for (event, _) in events.by_ref() {
            match event {
                Event::Text(text) => code.push_str(&text),
                Event::End(TagEnd::CodeBlock) => break,
                _ => {}
            }
        }
        replaced.push(info_string_range(&source, range.start));
        examples.push(Example {
            kind,
            construct,
            code,
        });
    }

    let mut text = String::with_capacity(source.len());
    let mut copied = 0;
    for range in replaced {
        text.push_str(&source[copied..range.start]);
        text.push_str("vhdl");
        copied = range.end;
    }
    text.push_str(&source[copied..]);

    Ok(Docs {
        summary,
        text,
        examples,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The lines as the attributes of `///` comments hold them.
    fn doc(text: &str) -> Vec<String> {
        text.lines().map(|line| format!(" {line}")).collect()
    }

    fn err(text: &str) -> Error {
        match parse(&doc(text)) {
            Ok(_) => panic!("expected an error"),
            Err(err) => err,
        }
    }

    #[test]
    fn the_first_paragraph_is_the_summary() {
        let docs = parse(&doc("Checks that\nsomething holds.\n\nMore text.")).unwrap();
        assert_eq!(docs.summary, "Checks that something holds.");
        assert_eq!(docs.text, "Checks that\nsomething holds.\n\nMore text.");
    }

    #[test]
    fn examples_are_collected_and_their_tags_removed_from_the_text() {
        let docs = parse(&doc("\
Summary.

# Non-compliant example

```vhdl,sequential-statement,non-compliant
if (a) then
    b;
end if;
```

```vhdl, design-unit, compliant
entity e is end;
```"))
        .unwrap();

        assert_eq!(
            docs.text,
            "\
Summary.

# Non-compliant example

```vhdl
if (a) then
    b;
end if;
```

```vhdl
entity e is end;
```"
        );
        let examples = docs
            .examples
            .iter()
            .map(|e| (e.kind, e.construct.variant, e.code.as_str()))
            .collect::<Vec<_>>();
        assert_eq!(
            examples,
            [
                (
                    ExampleKind::NonCompliant,
                    "SequentialStatement",
                    "if (a) then\n    b;\nend if;\n"
                ),
                (ExampleKind::Compliant, "DesignUnit", "entity e is end;\n"),
            ]
        );
    }

    #[test]
    fn blocks_in_other_languages_are_left_alone() {
        let docs = parse(&doc("Summary.\n\n```toml,whatever\nselect = []\n```")).unwrap();
        assert!(docs.examples.is_empty());
        assert_eq!(docs.text, "Summary.\n\n```toml,whatever\nselect = []\n```");
    }

    #[test]
    fn a_longer_fence_can_contain_a_shorter_one() {
        let docs = parse(&doc(
            "Summary.\n\n````vhdl,design-unit,compliant\n```\n````",
        ))
        .unwrap();
        assert_eq!(docs.examples[0].code, "```\n");
    }

    #[test]
    fn a_missing_doc_comment_is_rejected() {
        let Err(err) = parse(&[]) else {
            panic!("expected an error");
        };
        assert_eq!(err.line, None);
        assert!(
            err.message.contains("needs a doc comment"),
            "{}",
            err.message
        );
    }

    #[test]
    fn a_doc_comment_must_start_with_a_summary() {
        assert_eq!(err("# Example\n\nText").line, Some(0));
        assert_eq!(err("```vhdl,design-unit,compliant\n```").line, Some(0));
        assert_eq!(err("Heading\n=======\n\nText").line, Some(0));
        assert_eq!(err("- A list\n\nText").line, Some(0));
    }

    #[test]
    fn malformed_examples_are_rejected_at_their_fence() {
        let at = |info: &str| {
            let err = err(&format!("Summary.\n\n```{info}\ncode\n```"));
            assert_eq!(err.line, Some(2), "{}", err.message);
            err.message
        };
        let scheme = "`vhdl,<construct>,<compliant|non-compliant>`";
        assert!(at("vhdl").contains(scheme));
        assert!(at("vhdl,design-unit").contains(scheme));
        assert!(at("vhdl,design_unit,compliant").contains("unknown construct `design_unit`"));
        assert!(at("vhdl,compliant,design-unit").contains("unknown construct `compliant`"));
        assert!(at("vhdl,design-unit,complaint").contains("found `complaint`"));
        assert!(
            at("vhdl,design-unit,compliant,non-compliant").contains("unexpected `non-compliant`")
        );
    }

    #[test]
    fn examples_in_lists_and_block_quotes_are_found() {
        let docs = parse(&doc("\
Summary.

- An item:

  ```vhdl,design-unit,compliant
  entity e is
      port (clk : in bit);
  end;
  ```

> ```vhdl,sequential-statement,non-compliant
> null;
> ```"))
        .unwrap();

        assert_eq!(
            docs.text,
            "\
Summary.

- An item:

  ```vhdl
  entity e is
      port (clk : in bit);
  end;
  ```

> ```vhdl
> null;
> ```"
        );
        let examples = docs
            .examples
            .iter()
            .map(|e| (e.kind, e.construct.variant, e.code.as_str()))
            .collect::<Vec<_>>();
        assert_eq!(
            examples,
            [
                (
                    ExampleKind::Compliant,
                    "DesignUnit",
                    "entity e is\n    port (clk : in bit);\nend;\n"
                ),
                (ExampleKind::NonCompliant, "SequentialStatement", "null;\n"),
            ]
        );
    }

    #[test]
    fn a_fence_inside_an_indented_code_block_is_code() {
        let docs = parse(&doc("Summary.\n\n    ```vhdl\n    not an example\n    ```")).unwrap();
        assert!(docs.examples.is_empty());
    }

    #[test]
    fn a_fence_with_a_tilde_is_an_example_too() {
        let docs = parse(&doc("Summary.\n\n~~~vhdl,design-unit,compliant\ncode\n~~~")).unwrap();
        assert_eq!(docs.text, "Summary.\n\n~~~vhdl\ncode\n~~~");
        assert_eq!(docs.examples[0].code, "code\n");
    }
}
