use crate::syntax::{Comment, Value};
use crate::{kind_str, Token};
use std::iter;

pub struct Buffer {
    inner: String,
    // insert an extra newline before pushing a token.
    // This is relevant when there is a trailing comment
    insert_extra_newline: bool,
    indentation: usize,
    indent_char: char,
    indent_width: usize,
}

impl Buffer {
    pub fn new() -> Buffer {
        Buffer {
            inner: String::new(),
            insert_extra_newline: false,
            indentation: 0,
            indent_char: ' ',
            indent_width: 4,
        }
    }
}

fn leading_comment_is_on_token_line(comment: &Comment, token: &Token) -> bool {
    if !comment.multi_line {
        return false;
    }
    if comment.range.start.line != comment.range.end.line {
        return false;
    }
    token.pos.start().line == comment.range.start.line
}

impl Buffer {
    pub fn as_str(&self) -> &str {
        self.inner.as_str()
    }

    pub fn into_string(self) -> String {
        self.inner
    }

    pub fn push_whitespace(&mut self) {
        if !self.insert_extra_newline {
            self.push_ch(' ');
        }
    }

    fn format_comment(&mut self, comment: &Comment) {
        if !comment.multi_line {
            self.push_str("--");
            self.push_str(comment.value.trim_end())
        } else {
            self.push_str("/*");
            self.push_str(&comment.value);
            self.push_str("*/");
        }
    }

    fn format_leading_comments(&mut self, comments: &[Comment]) {
        for comment in comments {
            self.format_comment(comment);
            self.line_break();
        }
    }

    fn indent(&mut self) {
        self.inner
            .extend(iter::repeat(self.indent_char).take(self.indent_width * self.indentation));
    }

    pub fn push_token(&mut self, token: &Token) {
        if self.insert_extra_newline {
            self.line_break();
        }
        self.insert_extra_newline = false;
        if let Some(comments) = &token.comments {
            // This is for example the case for situations like
            // some_token /* comment in between */ some_other token
            if comments.leading.len() == 1
                && leading_comment_is_on_token_line(&comments.leading[0], token)
            {
                self.format_comment(&comments.leading[0]);
                self.push_ch(' ');
            } else if !comments.leading.is_empty() {
                self.format_leading_comments(comments.leading.as_slice());
            }
        }
        match &token.value {
            Value::Identifier(ident) => self.push_str(&ident.to_string()),
            Value::String(string) => {
                self.push_ch('"');
                self.push_str(&string.to_string());
                self.push_ch('"');
            }
            Value::BitString(value, _) => self.push_str(&value.to_string()),
            Value::AbstractLiteral(value, _) => self.push_str(&value.to_string()),
            Value::Character(char) => {
                self.push_ch('\'');
                self.push_ch((*char) as char);
                if *char == b'\'' {
                    self.push_ch('\'');
                }
                self.push_ch('\'');
            }
            Value::Text(text) => self.push_str(&text.to_string()),
            Value::None => self.push_str(kind_str(token.kind)),
        }
        if let Some(comments) = &token.comments {
            if let Some(trailing_comment) = &comments.trailing {
                self.push_ch(' ');
                self.format_comment(trailing_comment);
                self.insert_extra_newline = true
            }
        }
    }

    fn push_str(&mut self, value: &str) {
        self.inner.push_str(value);
    }

    fn push_ch(&mut self, char: char) {
        self.inner.push(char);
    }

    pub fn increase_indent(&mut self) {
        self.indentation += 1;
    }

    pub fn decrease_indent(&mut self) {
        self.indentation -= 1;
    }

    pub fn line_break(&mut self) {
        self.insert_extra_newline = false;
        self.push_ch('\n');
        self.indent();
    }

    pub fn line_breaks(&mut self, count: usize) {
        self.insert_extra_newline = false;
        for _ in 0..count {
            self.push_ch('\n');
        }
        self.indent();
    }
}

#[cfg(test)]
mod tests {
    use crate::analysis::tests::Code;
    use crate::formatting::buffer::Buffer;
    use std::iter::zip;

    fn check_token_formatted(input: &str, expected: &[&str]) {
        let code = Code::new(input);
        let tokens = code.tokenize();
        for (token, expected) in zip(tokens, expected) {
            let mut buffer = Buffer::new();
            buffer.push_token(&token);
            assert_eq!(buffer.as_str(), *expected);
        }
    }

    #[test]
    fn format_simple_token() {
        check_token_formatted("entity", &["entity"]);
        check_token_formatted("foobar", &["foobar"]);
        check_token_formatted("1 23 4E5 4e5", &["1", "23", "4E5", "4e5"]);
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
    fn bit_string_formatting() {
        check_token_formatted(r#"B"10" 20B"8" X"2F""#, &["B\"10\"", "20B\"8\"", "X\"2F\""]);
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
