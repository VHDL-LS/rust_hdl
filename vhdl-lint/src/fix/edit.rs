use vhdl_syntax::{
    latin_1::{Latin1Str, Latin1String},
    parser::error::Span,
    standard::VHDLStandard,
    syntax::{SyntaxNode, SyntaxToken},
    tokens::{token::requires_separator, Keyword, Token, TokenKind, TriviaBuf, TriviaPiece},
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
        self.delete_tokens(token, token)
    }

    /// Deletes the text of `node`, including any trivia (and therefore comments) inside it.
    pub fn delete_node(&self, node: &SyntaxNode) -> Edit {
        self.delete_tokens(&node.first_token(), &node.last_token())
    }

    /// Deletes `node` and the line it's on, if any.
    /// If the node is not on a new line, this behaves like [`Self::delete_node`]
    pub fn delete_line_of(&self, node: &SyntaxNode) -> Edit {
        let trailing_triv = node.last_token().trailing_trivia();
        let trailing_edge = trailing_triv.iter().position(TriviaPiece::is_newline);
        match trailing_edge {
            None => self.delete_node(node),
            Some(t) => {
                let line_bytes = |piece: &TriviaPiece| match piece {
                    TriviaPiece::CarriageReturnLineFeeds(_) => 2,
                    _ => 1,
                };
                let first = node.first_token();
                let leading_triv = first.leading_trivia();
                let leading_edge = leading_triv.iter().rposition(TriviaPiece::is_newline);
                let first_pos = if let Some(leading_edge) = leading_edge {
                    leading_triv[..=leading_edge].byte_len()
                } else if first.prev_token().is_none() {
                    0
                } else {
                    return self.delete_node(node);
                };
                let last_pos = trailing_triv[..t].byte_len() + line_bytes(&trailing_triv[t]);
                Edit::delete_raw(node.range().start + first_pos..node.range().end + last_pos)
            }
        }
    }

    /// Deletes tokens first..=last, including trivia, but not leading trivia before the first token
    pub fn delete_tokens(&self, first: &SyntaxToken, last: &SyntaxToken) -> Edit {
        let range = first.text_range().start..last.text_range().end;
        // Check if we need to insert a separator since adjacent tokens could merge
        if let (Some(prev_token), Some(next_token)) = (first.prev_token(), last.next_token()) {
            if requires_separator(prev_token.token(), next_token.token(), self.standard)
                && first.leading_trivia().is_empty()
                && next_token.leading_trivia().is_empty()
            {
                return Edit::new(range, b" ");
            }
        }
        Edit::delete_raw(range)
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

#[cfg(test)]
mod tests {
    use vhdl_syntax::{
        latin_1::Latin1Str,
        parser::parse,
        standard::VHDLStandard,
        syntax::{AstNode, LibraryClauseSyntax},
    };

    use crate::fix::{apply_sorted_edits, edit::Edits};

    fn delete_first_work_library(input: &str) -> String {
        let edits = Edits::new(VHDLStandard::default());
        let (file, _) = parse(input);
        let library_clause = file
            .descendants()
            .filter_map(LibraryClauseSyntax::cast)
            .find(|lib| {
                lib.visit_tokens()
                    .any(|tok| tok.text() == Latin1Str::new(b"work"))
            })
            .unwrap();
        let edit = edits.delete_line_of(&library_clause);
        String::from_utf8(apply_sorted_edits(input.as_bytes(), &[&edit])).unwrap()
    }

    #[test]
    fn delete_with_no_leading_newline() {
        assert_eq!(
            delete_first_work_library(
                "\
library work;
entity foo is
end foo;"
            ),
            "\
entity foo is
end foo;"
        )
    }

    #[test]
    fn delete_with_no_leading_and_trailing_newlines() {
        assert_eq!(
            delete_first_work_library(
                "\
library work;entity foo is
end foo;"
            ),
            "\
entity foo is
end foo;"
        )
    }

    #[test]
    fn delete_with_no_trailing_newline() {
        assert_eq!(
            delete_first_work_library(
                "\
-- foo
library work;entity foo is
end foo;"
            ),
            "\
-- foo
entity foo is
end foo;"
        )
    }

    #[test]
    fn delete_with_only_leading_newline() {
        assert_eq!(
            delete_first_work_library(
                "
library work;entity foo is
end foo;"
            ),
            "
entity foo is
end foo;"
        )
    }

    #[test]
    fn leading_comment() {
        assert_eq!(
            delete_first_work_library(
                "\
-- keep me
library work;
entity foo is
end foo;"
            ),
            "\
-- keep me
entity foo is
end foo;"
        )
    }

    #[test]
    fn multiple_libraries() {
        assert_eq!(
            delete_first_work_library(
                "\
library ieee;
library work;
entity foo is
end foo;"
            ),
            "\
library ieee;
entity foo is
end foo;"
        )
    }

    #[test]
    fn trailing_comment() {
        assert_eq!(
            delete_first_work_library(
                "\
library work; -- trailing
entity foo is
end foo;"
            ),
            "\
entity foo is
end foo;"
        )
    }

    #[test]
    fn same_line() {
        assert_eq!(
            delete_first_work_library(
                "\
library ieee;library work;
entity foo is
end foo;"
            ),
            "\
library ieee;
entity foo is
end foo;"
        )
    }

    #[test]
    fn comment_after_library_clause() {
        assert_eq!(
            delete_first_work_library(
                "\
library ieee;
library work; -- c
entity foo is
end foo;"
            ),
            "\
library ieee;
entity foo is
end foo;"
        )
    }

    #[test]
    fn blank_lines_above_node() {
        assert_eq!(
            delete_first_work_library(
                "\
library ieee;

library work;
entity foo is
end foo;"
            ),
            "\
library ieee;

entity foo is
end foo;"
        )
    }
}
