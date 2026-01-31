/// For most applications in the context of a language server,
/// the lexical position (i.e., a position in the source code)
/// of all AST nodes must be known.
/// In the context of `vhdl_lang`, this information is provided
/// using Token information. Each AST element knows the token span that it was declared in.
/// Information, such as the position can be queried using the `pos(TokenAccess)` method.
/// A [TokenAccess] is a context object that is passed in all relevant operations
/// (i.e., when traversing the AST using the [Search] trait
/// or when getting the source code information when generating code outlines in a language server).
/// This is also the mechanic used to extract supplementary information, such as comments for
/// documentation generation.
use crate::{SrcPos, TokenAccess, TokenId, TokenSpan};

/// A struct that associates some generic item to a single token.
#[derive(Eq, PartialEq, Debug, Clone)]
pub struct WithToken<T> {
    pub item: T,
    pub token: TokenId,
}

impl<T> WithToken<T> {
    pub fn new(item: T, token: TokenId) -> WithToken<T> {
        WithToken { item, token }
    }

    /// Retrieves the position of this object using the provided `TokenAccess`.
    pub fn pos<'a>(&'a self, ctx: &'a dyn TokenAccess) -> &'a SrcPos {
        ctx.get_pos(self.token)
    }

    /// Maps this element into another element applying the given function
    /// but retaining the source location (i.e., the token).
    pub(crate) fn map_into<F, U>(self, f: F) -> WithToken<U>
    where
        F: FnOnce(T) -> U,
    {
        WithToken {
            item: f(self.item),
            token: self.token,
        }
    }

    /// Maps this element into another element applying the given function
    /// but retaining the source location.
    /// The returned object's `TokenSpan` will have this token id as start and end.
    pub(crate) fn map_into_span<F, U>(self, f: F) -> WithTokenSpan<U>
    where
        F: FnOnce(T) -> U,
    {
        WithTokenSpan {
            item: f(self.item),
            span: self.token.into(),
        }
    }
}

/// A struct that associates some generic item to a contiguous span of tokens.
#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub struct WithTokenSpan<T> {
    pub item: T,
    pub span: TokenSpan,
}

impl<T> WithTokenSpan<T> {
    pub fn new(item: T, span: TokenSpan) -> WithTokenSpan<T> {
        WithTokenSpan { item, span }
    }

    pub fn from(item: T, span: impl Into<TokenSpan>) -> WithTokenSpan<T> {
        WithTokenSpan {
            item,
            span: span.into(),
        }
    }

    /// Retrieves the position of this object using the provided `TokenAccess`.
    pub fn pos(&self, ctx: &dyn TokenAccess) -> SrcPos {
        self.span.pos(ctx)
    }

    /// Maps this element into another element applying the given function
    /// but retaining the source location (i.e., the token span).
    pub(crate) fn map_into<F, U>(self, f: F) -> WithTokenSpan<U>
    where
        F: FnOnce(T) -> U,
    {
        WithTokenSpan {
            item: f(self.item),
            span: self.span,
        }
    }

    /// Attempts to map this element into another element applying the given function.
    /// If the function returns `None`, this will also return `None`.
    /// Otherwise, the semantics are the same as [map_into](WithTokenSpan::map_into)
    pub(crate) fn try_map_into<F, U>(self, f: F) -> Option<WithTokenSpan<U>>
    where
        F: FnOnce(T) -> Option<U>,
    {
        Some(WithTokenSpan {
            item: f(self.item)?,
            span: self.span,
        })
    }

    /// Returns a new `WithTokenSpan` object that encompasses this item
    /// but extends the token span starting with the given token.
    pub(crate) fn start_with(self, id: TokenId) -> Self {
        WithTokenSpan {
            item: self.item,
            span: self.span.start_with(id),
        }
    }

    pub fn as_ref(&self) -> WithTokenSpan<&T> {
        WithTokenSpan {
            item: &self.item,
            span: self.span,
        }
    }
}
