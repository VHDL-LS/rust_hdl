use crate::formatting::DesignUnitFormatter;
use crate::syntax::{Comment, Value};
use crate::{kind_str, Token, TokenAccess, TokenId};
use vhdl_lang::TokenSpan;

fn leading_comment_is_on_token_line(comment: &Comment, token: &Token) -> bool {
    if !comment.multi_line {
        return false;
    }
    if comment.range.start.line != comment.range.end.line {
        return false;
    }
    token.pos.start().line == comment.range.start.line
}

impl DesignUnitFormatter<'_> {
    fn format_comment(&self, comment: &Comment, buffer: &mut String) {
        if !comment.multi_line {
            buffer.push_str("--");
            buffer.push_str(comment.value.trim_end())
        } else {
            buffer.push_str("/*");
            buffer.push_str(&comment.value);
            buffer.push_str("*/");
        }
    }

    fn format_leading_comments(&self, comments: &[Comment], buffer: &mut String) {
        for comment in comments {
            self.format_comment(comment, buffer);
            self.newline(buffer);
        }
    }

    pub(crate) fn format_token_id(&self, id: TokenId, buffer: &mut String) {
        self.format_token(self.tokens.get_token(id), buffer);
    }

    pub(crate) fn format_token_span(&self, span: TokenSpan, buffer: &mut String) {
        for (index, id) in span.iter().enumerate() {
            self.format_token_id(id, buffer);
            if index < span.len() - 1 {
                buffer.push(' ');
            }
        }
    }

    pub(crate) fn join_token_span(&self, span: TokenSpan, buffer: &mut String) {
        for id in span.iter() {
            self.format_token_id(id, buffer);
        }
    }

    pub(crate) fn format_token(&self, token: &Token, buffer: &mut String) {
        if let Some(comments) = &token.comments {
            // This is for example the case for situations like
            // some_token /* comment in between */ some_other token
            if comments.leading.len() == 1
                && leading_comment_is_on_token_line(&comments.leading[0], token)
            {
                self.format_comment(&comments.leading[0], buffer);
                buffer.push(' ');
            } else {
                self.format_leading_comments(comments.leading.as_slice(), buffer);
            }
        }
        match &token.value {
            Value::Identifier(ident) => buffer.push_str(&ident.to_string()),
            Value::String(string) => {
                buffer.push('"');
                buffer.push_str(&string.to_string());
                buffer.push('"');
            }
            Value::BitString(..) => unimplemented!(),
            Value::AbstractLiteral(_) => {
                println!("{token:?}")
            }
            Value::Character(char) => {
                buffer.push('\'');
                buffer.push((*char) as char);
                buffer.push('\'');
            }
            Value::Text(_) => unimplemented!(),
            Value::None => buffer.push_str(kind_str(token.kind)),
        }
        if let Some(comments) = &token.comments {
            if let Some(trailing_comment) = &comments.trailing {
                buffer.push(' ');
                self.format_comment(trailing_comment, buffer);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::analysis::tests::Code;
    use crate::formatting::DesignUnitFormatter;

    fn check_token_formatted(input: &str, expected: &[&str]) {
        let code = Code::new(input);
        let tokens = code.tokenize();
        let formatter = DesignUnitFormatter::new(&tokens);
        for (i, str) in expected.iter().enumerate() {
            let mut buffer = String::new();
            formatter.format_token(&tokens[i], &mut buffer);
            assert_eq!(&buffer, str);
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
