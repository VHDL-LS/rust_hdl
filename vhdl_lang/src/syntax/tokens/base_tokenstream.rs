use super::tokenizer::Kind::*;
use super::tokenizer::*;
use crate::ast::{AttributeDesignator, RangeAttribute, TypeAttribute};
use crate::data::{DiagnosticHandler, DiagnosticResult, WithPos};
use crate::syntax::tokens::{TokenStream, TokenizationException};
use crate::Diagnostic;
use std::cell::Cell;

pub struct BaseTokenStream<'a> {
    tokenizer: Tokenizer<'a>,
    idx: Cell<usize>,
    tokens: Vec<Token>,
}

/// The token stream maintains a collection of tokens and a current state.
/// The state is an index into the vector of tokens and
impl<'a> BaseTokenStream<'a> {
    pub fn new(
        mut tokenizer: Tokenizer<'a>,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> BaseTokenStream<'a> {
        let mut tokens = Vec::new();
        loop {
            match tokenizer.pop() {
                Ok(Some(token)) => tokens.push(token),
                Ok(None) => break,
                Err(err) => diagnostics.push(err),
            }
        }
        BaseTokenStream {
            tokenizer,
            idx: Cell::new(0),
            tokens,
        }
    }

    fn eof_error(&self) -> Diagnostic {
        let end = self.tokenizer.source.contents().end();
        Diagnostic::error(
            self.tokenizer.source.pos(end, end.next_char()),
            "Unexpected EOF",
        )
    }
}

impl<'a> TokenStream for BaseTokenStream<'a> {
    fn state(&self) -> usize {
        self.idx.get()
    }

    fn set_state(&self, state: usize) {
        self.idx.replace(state);
    }

    fn token_at(&self, state: usize) -> Option<&Token> {
        self.tokens.get(state)
    }

    fn state_of(&self, token: &Token) -> Option<usize> {
        let base = self.tokens.as_ptr() as usize;
        let ptr = (token as *const Token) as usize;
        let idx = ptr.checked_sub(base)? / std::mem::size_of::<Token>();

        if idx < self.tokens.len() {
            Some(idx)
        } else {
            None
        }
    }

    fn handle_diagnostic(&self, exception: TokenizationException) -> Diagnostic {
        match exception {
            TokenizationException::KindsError { pos, kinds } => kinds_error(pos, kinds),
            TokenizationException::EofError { expectations } => {
                let mut diagnostic = self.eof_error();
                if let Some(kinds) = expectations {
                    diagnostic = diagnostic.when(format!("expecting {}", kinds_str(kinds)));
                }
                diagnostic
            }
        }
    }

    /// Expect identifier or subtype/range keywords
    /// foo'subtype or foo'range
    fn expect_attribute_designator(&self) -> DiagnosticResult<WithPos<AttributeDesignator>> {
        let des = expect_token!(
            self,
            token,
            Identifier => {
                let ident = token.to_identifier_value()?;
                ident.map_into(|sym| self.tokenizer.attribute(sym))
            },
            Subtype => WithPos::new(AttributeDesignator::Type(TypeAttribute::Subtype), token.pos.clone()),
            Range => WithPos::new(AttributeDesignator::Range(RangeAttribute::Range), token.pos.clone())
        );
        Ok(des)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::data::{ContentReader, Diagnostic, NoDiagnostics};
    use crate::syntax::test::Code;

    macro_rules! new_stream {
        ($code:ident, $stream:ident) => {
            let source = $code.source();
            let contents = source.contents();
            let tokenizer = Tokenizer::new(&$code.symbols, source, ContentReader::new(&contents));
            let $stream = BaseTokenStream::new(tokenizer, &mut NoDiagnostics);
        };
    }

    #[test]
    fn pop_and_peek() {
        let code = Code::new("hello world again");
        let tokens = code.tokenize();
        new_stream!(code, stream);

        stream.skip();
        assert_eq!(stream.peek(), Some(&tokens[1]));
        stream.skip();
        assert_eq!(stream.peek(), Some(&tokens[2]));
        stream.skip();
        assert_eq!(stream.peek(), None);
        stream.skip();
        assert_eq!(stream.peek(), None);
    }

    #[test]
    fn idx_of() {
        let code = Code::new("hello world again");
        new_stream!(code, stream);

        let mut idx = 0;
        while let Some(token) = stream.peek() {
            assert_eq!(idx, stream.state_of(token).unwrap());
            idx += 1;
            stream.skip();
        }
    }

    #[test]
    fn prev_token() {
        let code = Code::new("hello world again");
        new_stream!(code, stream);

        let mut prev = None;
        while let Some(token) = stream.peek() {
            if let Some(prev) = prev {
                assert_eq!(prev, stream.token_before(token).unwrap());
            }
            prev = Some(token);
            stream.skip();
        }
    }

    #[test]
    fn is_peek_kinds() {
        let code = Code::new("hello 1 +");
        new_stream!(code, stream);

        assert!(stream.next_kinds_are(&[Identifier, AbstractLiteral, Plus]),);
        assert!(stream.next_kinds_are(&[Identifier, AbstractLiteral]));
        assert!(stream.next_kinds_are(&[Identifier]));
        assert!(!stream.next_kinds_are(&[Identifier, AbstractLiteral, AbstractLiteral]),);
        assert!(!stream.next_kinds_are(&[AbstractLiteral]));
    }

    #[test]
    fn expect() {
        let code = Code::new("hello");
        let tokens = code.tokenize();
        new_stream!(code, stream);

        assert_eq!(stream.peek_expect(), Ok(&tokens[0]));
        stream.skip();
        assert_eq!(
            stream.peek_expect(),
            Err(Diagnostic::error(code.eof_pos(), "Unexpected EOF"))
        );
    }

    #[test]
    fn set_state_taken_before_peek() {
        let code = Code::new("hello world");
        let tokens = code.tokenize();
        new_stream!(code, stream);

        let state = stream.state();
        assert_eq!(stream.peek(), Some(&tokens[0]));
        stream.skip();
        assert_eq!(stream.peek(), Some(&tokens[1]));
        stream.set_state(state);
        assert_eq!(stream.peek(), Some(&tokens[0]));
    }

    #[test]
    fn expect_when_eof_empty() {
        let code = Code::new("");
        new_stream!(code, stream);

        assert_eq!(
            stream.peek_expect(),
            Err(Diagnostic::error(code.eof_pos(), "Unexpected EOF"))
        );
    }

    #[test]
    fn expect_eof_after_whitespace() {
        let code = Code::new("a  ");
        new_stream!(code, stream);

        stream.skip();
        assert_eq!(
            stream.peek_expect(),
            Err(Diagnostic::error(code.eof_pos(), "Unexpected EOF"))
        );
    }

    #[test]
    fn expect_eof_after_comment() {
        let code = Code::new("a  -- foo");
        new_stream!(code, stream);

        stream.skip();
        assert_eq!(
            stream.peek_expect(),
            Err(Diagnostic::error(code.eof_pos(), "Unexpected EOF"))
        );
    }

    #[test]
    fn skip_until() {
        let code = Code::new("a begin for + ;");
        new_stream!(code, stream);

        assert!(stream.skip_until(|ref k| matches!(k, Plus)).is_ok());
        assert_eq!(stream.peek().map(|t| t.kind), Some(Plus));
    }
}

