use crate::{SrcPos, TokenAccess, TokenId, TokenSpan};

/// A generic struct that contains source code information along with some other information.
#[derive(Eq, PartialEq, Debug, Clone)]
pub struct WithToken<T> {
    pub item: T,
    pub token: TokenId,
}

impl<T> WithToken<T> {
    pub fn new(item: T, token: TokenId) -> WithToken<T> {
        WithToken { item, token }
    }

    pub fn map_into<F, U>(self, f: F) -> WithToken<U>
    where
        F: FnOnce(T) -> U,
    {
        WithToken {
            item: f(self.item),
            token: self.token,
        }
    }

    pub fn map_into_span<F, U>(self, f: F) -> WithTokenSpan<U>
    where
        F: FnOnce(T) -> U,
    {
        WithTokenSpan {
            item: f(self.item),
            span: self.token.into(),
        }
    }

    pub fn pos<'a>(&'a self, ctx: &'a dyn TokenAccess) -> &'a SrcPos {
        ctx.get_pos(self.token)
    }
}

/// A generic object with an associated source file and lexical range.
#[derive(PartialEq, Eq, Clone, Debug)]
pub struct WithTokenSpan<T> {
    pub item: T,
    pub span: TokenSpan,
}

impl<T> WithTokenSpan<T> {
    pub fn pos(&self, ctx: &dyn TokenAccess) -> SrcPos {
        self.span.to_pos(ctx)
    }

    pub fn new(item: T, span: TokenSpan) -> WithTokenSpan<T> {
        WithTokenSpan { item, span }
    }

    pub fn from(item: T, span: impl Into<TokenSpan>) -> WithTokenSpan<T> {
        WithTokenSpan {
            item,
            span: span.into(),
        }
    }

    pub fn map_into<F, U>(self, f: F) -> WithTokenSpan<U>
    where
        F: FnOnce(T) -> U,
    {
        WithTokenSpan {
            item: f(self.item),
            span: self.span,
        }
    }

    pub fn try_map_into<F, U>(self, f: F) -> Option<WithTokenSpan<U>>
    where
        F: FnOnce(T) -> Option<U>,
    {
        Some(WithTokenSpan {
            item: f(self.item)?,
            span: self.span,
        })
    }

    pub fn combine_span_with(self, other: impl Into<TokenSpan>) -> Self {
        WithTokenSpan {
            item: self.item,
            span: TokenSpan::new(self.span.start_token, other.into().end_token),
        }
    }

    pub fn start_with(self, id: TokenId) -> Self {
        WithTokenSpan {
            item: self.item,
            span: self.span.start_with(id),
        }
    }
}

impl<T> AsRef<TokenSpan> for WithTokenSpan<T> {
    fn as_ref(&self) -> &TokenSpan {
        &self.span
    }
}

impl<T> From<WithTokenSpan<T>> for TokenSpan {
    fn from(with_span: WithTokenSpan<T>) -> TokenSpan {
        with_span.span
    }
}
