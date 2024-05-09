use crate::ast::{LabeledConcurrentStatement, LabeledSequentialStatement, WithDecl};
use crate::data::WithTokenSpan;
use crate::{HasTokenSpan, TokenId};
use vhdl_lang::ast::WithToken;
use vhdl_lang::TokenSpan;

impl HasTokenSpan for TokenId {
    fn get_start_token(&self) -> TokenId {
        *self
    }

    fn get_end_token(&self) -> TokenId {
        *self
    }
}

impl HasTokenSpan for TokenSpan {
    fn get_start_token(&self) -> TokenId {
        self.start_token
    }

    fn get_end_token(&self) -> TokenId {
        self.end_token
    }
}

impl<T> HasTokenSpan for WithToken<T> {
    fn get_start_token(&self) -> TokenId {
        self.token
    }

    fn get_end_token(&self) -> TokenId {
        self.token
    }
}

impl<T> HasTokenSpan for WithDecl<WithToken<T>> {
    fn get_start_token(&self) -> TokenId {
        self.tree.get_start_token()
    }

    fn get_end_token(&self) -> TokenId {
        self.tree.get_end_token()
    }
}

impl<T> HasTokenSpan for WithTokenSpan<T> {
    fn get_start_token(&self) -> TokenId {
        self.span.start_token
    }

    fn get_end_token(&self) -> TokenId {
        self.span.end_token
    }
}

impl HasTokenSpan for LabeledConcurrentStatement {
    fn get_start_token(&self) -> TokenId {
        if let Some(label) = &self.label.tree {
            label.token
        } else {
            self.statement.span.start_token
        }
    }

    fn get_end_token(&self) -> TokenId {
        self.statement.span.end_token
    }
}

impl HasTokenSpan for LabeledSequentialStatement {
    fn get_start_token(&self) -> TokenId {
        if let Some(label) = &self.label.tree {
            label.token
        } else {
            self.statement.span.start_token
        }
    }

    fn get_end_token(&self) -> TokenId {
        self.statement.span.end_token
    }
}
