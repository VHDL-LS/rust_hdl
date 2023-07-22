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
