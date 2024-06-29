/// Module for robust parsing
use crate::syntax::parser::ParsingContext;
use crate::syntax::Kind::{Colon, SemiColon};
use crate::syntax::{kinds_error, kinds_str};
use crate::TokenId;

/// Special handling when expecting a semicolon.
/// When the next token is
/// * a semicolon, then consume that token and produce no error
/// * a token that could be confused with a semicolon (i.e., a comma),
///     then consume that token and report an error
/// * none of these choices: do not consume the token and report an error
pub fn expect_semicolon(ctx: &mut ParsingContext<'_>) -> Option<TokenId> {
    let token = match ctx.stream.peek_expect() {
        Ok(token) => token,
        Err(err) => {
            ctx.diagnostics
                .push(err.when(format!("expecting {}", kinds_str(&[SemiColon]))));
            return None;
        }
    };
    match token.kind {
        SemiColon => {
            ctx.stream.skip();
            Some(ctx.stream.get_last_token_id())
        }
        Colon => {
            ctx.stream.skip();
            ctx.diagnostics
                .push(kinds_error(token.pos.clone(), &[SemiColon]));
            Some(ctx.stream.get_last_token_id())
        }
        _ => {
            ctx.diagnostics
                .push(kinds_error(ctx.stream.pos_before(token), &[SemiColon]));
            None
        }
    }
}

/// Expect the next token to be a SemiColon, or return the last token.
/// The behavior is the same as [expect_semicolon].
#[must_use]
pub fn expect_semicolon_or_last(ctx: &mut ParsingContext<'_>) -> TokenId {
    expect_semicolon(ctx).unwrap_or(ctx.stream.get_last_token_id())
}
