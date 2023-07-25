use crate::{Diagnostic, SrcPos};
use crate::ast::AttributeDesignator;
use crate::data::{DiagnosticResult, WithPos};
use crate::syntax::tokens::{BaseTokenStream, Token, TokenizationException, TokenStream};

struct CompletionTokenStream<'a> {
    base: BaseTokenStream<'a>,
    cursor: SrcPos,
    suggestions: Vec<String>,
}

impl TokenStream for CompletionTokenStream {
    fn state(&self) -> usize {
        self.base.state()
    }

    fn set_state(&self, state: usize) {
        self.base.set_state(state)
    }

    fn token_at(&self, state: usize) -> Option<&Token> {
        self.base.token_at(state)
    }

    fn state_of(&self, token: &Token) -> Option<usize> {
        self.base.state_of(token)
    }

    fn handle_diagnostic(&self, exception: TokenizationException) -> Diagnostic {
        self.base.handle_diagnostic(exception)
    }

    fn expect_attribute_designator(&self) -> DiagnosticResult<WithPos<AttributeDesignator>> {
        self.base.expect_attribute_designator()
    }
}
