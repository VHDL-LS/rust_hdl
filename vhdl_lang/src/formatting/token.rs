use crate::formatting::DesignUnitFormatter;
use crate::syntax::{Comment, Value};
use crate::{kind_str, Token, TokenAccess, TokenId};
use vhdl_lang::TokenSpan;

fn leading_comment_is_on_token_line(comment: &Comment, token: &Token) -> bool {
    if !comment.multi_line {
        return false;
    }
    if comment.range.start.line != comment.range.start.line {
        return false;
    }
    token.pos.start().line == comment.range.start.line
}

impl DesignUnitFormatter<'_, '_> {
    fn format_comment(&self, comment: &Comment) -> String {
        let mut result = String::new();
        if !comment.multi_line {
            result.push_str("--");
            result.push_str(comment.value.trim_end())
        } else {
            result.push_str("/*");
            result.push_str(&comment.value);
            result.push_str("*/");
        }
        result
    }

    fn format_leading_comments(&self, comments: &[Comment]) -> String {
        let mut result = String::new();
        for comment in comments {
            result.push_str(&self.format_comment(comment));
            result.push('\n');
        }
        result
    }

    pub(crate) fn format_token_id(&self, id: TokenId) -> String {
        self.format_token(self.tokens.get_token(id))
    }

    pub(crate) fn format_token_span(&self, span: TokenSpan) -> String {
        let mut result = String::new();
        for (index, id) in span.iter().enumerate() {
            result.push_str(&self.format_token_id(id));
            if index < span.len() - 1 {
                result.push(' ');
            }
        }
        result
    }

    pub(crate) fn format_token(&self, token: &Token) -> String {
        let mut result = String::new();
        if let Some(comments) = &token.comments {
            // This is for example the case for situations like
            // some_token /* comment in between */ some_other token
            if comments.leading.len() == 1
                && leading_comment_is_on_token_line(&comments.leading[0], token)
            {
                result.push_str(&self.format_comment(&comments.leading[0]));
                result.push(' ');
            } else {
                result.push_str(&self.format_leading_comments(comments.leading.as_slice()));
            }
        }
        match &token.value {
            Value::Identifier(ident) => result.push_str(&ident.to_string()),
            Value::String(string) => {
                result.push('"');
                result.push_str(&string.to_string());
                result.push('"');
            }
            Value::BitString(..) => unimplemented!(),
            Value::AbstractLiteral(_) => {}
            Value::Character(char) => {
                result.push('\'');
                result.push((*char) as char);
                result.push('\'');
            }
            Value::Text(_) => unimplemented!(),
            Value::None => result.push_str(kind_str(token.kind)),
        }
        if let Some(comments) = &token.comments {
            if let Some(trailing_comment) = &comments.trailing {
                result.push(' ');
                result.push_str(&self.format_comment(trailing_comment));
            }
        }
        result
    }
}

#[cfg(test)]
mod tests {
    use crate::analysis::tests::Code;
    use crate::formatting::DesignUnitFormatter;
    use vhdl_lang::formatting::Formatter;

    fn check_token_formatted(input: &str, expected: &[&str]) {
        let code = Code::new(input);
        let tokens = code.tokenize();
        let parent_formatter = Formatter {};
        let formatter = DesignUnitFormatter {
            formatter: &parent_formatter,
            tokens: &tokens,
        };
        for (i, str) in expected.iter().enumerate() {
            assert_eq!(&formatter.format_token(&tokens[i]), str);
        }
    }

    #[test]
    fn format_simple_token() {
        check_token_formatted("entity", &["entity"]);
        check_token_formatted("foobar", &["foobar"]);
    }

    #[test]
    fn preserves_identifier_casing() {
        check_token_formatted("FooBar foobar", &["FooBar", "foobar"]);
    }

    #[test]
    fn character_formatting() {
        check_token_formatted("'a' 'Z'", &["'a'", "'Z'"]);
    }

    #[test]
    fn string_formatting() {
        check_token_formatted(r#""ABC" "" "DEF""#, &["\"ABC\"", "\"\"", "\"DEF\""]);
    }

    #[test]
    fn leading_comment() {
        check_token_formatted(
            "\
-- I am a comment
foobar
        ",
            &["\
-- I am a comment
foobar"],
        );
    }

    #[test]
    fn multiple_leading_comments() {
        check_token_formatted(
            "\
-- I am a comment
-- So am I
foobar
        ",
            &["\
-- I am a comment
-- So am I
foobar"],
        );
    }

    #[test]
    fn trailing_comments() {
        check_token_formatted(
            "\
foobar --After foobar comes foobaz
        ",
            &["foobar --After foobar comes foobaz"],
        );
    }

    #[test]
    fn single_multiline_comment() {
        check_token_formatted(
            "\
/** Some documentation.
  * This is a token named 'entity'
  */
entity
        ",
            &["\
/** Some documentation.
  * This is a token named 'entity'
  */
entity"],
        );
    }

    #[test]
    fn multiline_comment_and_simple_comment() {
        check_token_formatted(
            "\
/* I am a multiline comment */
-- And I am a single line comment
entity
        ",
            &["\
/* I am a multiline comment */
-- And I am a single line comment
entity"],
        );
    }

    #[test]
    fn leading_comment_and_trailing_comment() {
        check_token_formatted(
            "\
-- Leading comment
entity -- Trailing comment
        ",
            &["\
-- Leading comment
entity -- Trailing comment"],
        );
    }
}
