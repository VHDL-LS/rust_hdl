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
