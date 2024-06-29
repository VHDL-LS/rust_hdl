// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use super::common::check_end_identifier_mismatch;
use super::common::ParseResult;
use super::names::parse_name;
use super::tokens::{Kind::*, TokenSpan};
use crate::ast::token_range::WithTokenSpan;
use crate::ast::*;
use crate::syntax::parser::ParsingContext;
use crate::syntax::recover;
use crate::syntax::recover::expect_semicolon;
use crate::syntax::separated_list::{parse_ident_list, parse_name_list};
#[derive(PartialEq, Debug)]
pub enum DeclarationOrReference {
    Declaration(ContextDeclaration),
    Reference(ContextReference),
}

/// LRM 13. Design units and their analysis
pub fn parse_library_clause(ctx: &mut ParsingContext<'_>) -> ParseResult<LibraryClause> {
    let library_token = ctx.stream.expect_kind(Library)?;
    let name_list = parse_ident_list(ctx)?;
    let semi_token = recover::expect_semicolon_or_last(ctx);
    Ok(LibraryClause {
        span: TokenSpan::new(library_token, semi_token),
        name_list,
    })
}

/// LRM 12.4. Use clauses
pub fn parse_use_clause(ctx: &mut ParsingContext<'_>) -> ParseResult<WithTokenSpan<UseClause>> {
    let use_token = ctx.stream.expect_kind(Use)?;

    let name_list = parse_name_list(ctx)?;
    let token_id = recover::expect_semicolon(ctx).unwrap_or(ctx.stream.get_last_token_id());
    Ok(WithTokenSpan::new(
        UseClause {
            span: TokenSpan::new(use_token, token_id),
            name_list,
        },
        TokenSpan::new(use_token, token_id),
    ))
}

pub fn parse_context_reference(ctx: &mut ParsingContext<'_>) -> ParseResult<ContextReference> {
    let context_token = ctx.stream.expect_kind(Context)?;

    let name_list = parse_name_list(ctx)?;
    let semi_token = recover::expect_semicolon_or_last(ctx);
    Ok(ContextReference {
        span: TokenSpan::new(context_token, semi_token),
        name_list,
    })
}

/// LRM 13.4 Context clauses
pub fn parse_context(ctx: &mut ParsingContext<'_>) -> ParseResult<DeclarationOrReference> {
    let context_token = ctx.stream.expect_kind(Context)?;
    let name = parse_name(ctx)?;
    if ctx.stream.skip_if_kind(Is) {
        let mut items = Vec::with_capacity(16);
        let end_ident;
        loop {
            let token = ctx.stream.peek_expect()?;
            try_init_token_kind!(
                token,
                Library => items.push(ContextItem::Library(parse_library_clause(ctx)?)),
                Use => items.push(ContextItem::Use(parse_use_clause(ctx)?.item)),
                Context => items.push(ContextItem::Context(parse_context_reference(ctx)?)),
                End => {
                    ctx.stream.skip();
                    ctx.stream.pop_if_kind(Context);
                    end_ident = ctx.stream.pop_optional_ident();
                    expect_semicolon(ctx);
                    break;
                }
            )
        }

        let ident = WithDecl::new(to_simple_name(ctx, name)?);
        let end_token = ctx.stream.get_last_token_id();
        Ok(DeclarationOrReference::Declaration(ContextDeclaration {
            span: TokenSpan::new(context_token, end_token),
            end_ident_pos: check_end_identifier_mismatch(ctx, &ident.tree, end_ident),
            ident,
            items,
        }))
    } else {
        // Context reference
        let mut items = vec![name];
        let mut tokens = Vec::new();
        while let Some(comma) = ctx.stream.pop_if_kind(Comma) {
            items.push(parse_name(ctx)?);
            tokens.push(comma);
        }
        let name_list = SeparatedList { items, tokens };
        let semi_token = recover::expect_semicolon_or_last(ctx);
        Ok(DeclarationOrReference::Reference(ContextReference {
            span: TokenSpan::new(context_token, semi_token),
            name_list,
        }))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::data::Diagnostic;
    use crate::syntax::test::{token_to_string, Code};
    use crate::HasTokenSpan;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_library_clause_single_name() {
        let code = Code::new("library foo;");
        assert_eq!(
            code.with_stream_no_diagnostics(parse_library_clause),
            LibraryClause {
                span: code.token_span(),
                name_list: code.s1("foo").ident_list(),
            }
        )
    }

    #[test]
    fn test_library_clause_multiple_names() {
        let code = Code::new("library foo, bar;");
        assert_eq!(
            code.with_stream_no_diagnostics(parse_library_clause),
            LibraryClause {
                span: code.token_span(),
                name_list: code.s1("foo, bar").ident_list(),
            },
        )
    }

    #[test]
    fn test_use_clause_single_name() {
        let code = Code::new("use lib.foo;");
        assert_eq!(
            code.with_stream_no_diagnostics(parse_use_clause),
            WithTokenSpan::new(
                UseClause {
                    span: code.token_span(),
                    name_list: code.s1("lib.foo").name_list(),
                },
                code.token_span()
            ),
        )
    }

    #[test]
    fn test_use_clause_multiple_names() {
        let code = Code::new("use foo.'a', lib.bar.all;");
        assert_eq!(
            code.with_stream_no_diagnostics(parse_use_clause),
            WithTokenSpan::new(
                UseClause {
                    span: code.token_span(),
                    name_list: code.s1("foo.'a', lib.bar.all").name_list(),
                },
                code.token_span()
            ),
        )
    }

    #[test]
    fn test_context_reference_single_name() {
        let code = Code::new("context lib.foo;");
        assert_eq!(
            code.with_stream_no_diagnostics(parse_context),
            DeclarationOrReference::Reference(ContextReference {
                span: code.token_span(),
                name_list: code.s1("lib.foo").name_list(),
            },)
        )
    }

    #[test]
    fn test_context_clause() {
        let variants = [
            &"\
context ident is
end;
",
            &"\
context ident is
end context;
",
            &"\
context ident is
end ident;
",
            &"\
context ident is
end context ident;
",
        ];
        for (idx, variant) in variants.iter().enumerate() {
            let has_end_ident = idx >= 2;
            let code = Code::new(variant);
            assert_eq!(
                code.with_stream_no_diagnostics(parse_context),
                DeclarationOrReference::Declaration(ContextDeclaration {
                    span: code.token_span(),
                    ident: code.s1("ident").decl_ident(),
                    items: vec![],
                    end_ident_pos: if has_end_ident {
                        Some(code.s("ident", 2).token())
                    } else {
                        None
                    },
                })
            );
        }
    }

    #[test]
    fn test_context_clause_error_end_identifier_mismatch() {
        let code = Code::new(
            "\
context ident is
end context ident2;
",
        );
        let (context, diagnostics) = code.with_stream_diagnostics(parse_context);
        assert_eq!(
            diagnostics,
            vec![Diagnostic::syntax_error(
                code.s1("ident2"),
                "End identifier mismatch, expected ident",
            )]
        );
        assert_eq!(
            context,
            DeclarationOrReference::Declaration(ContextDeclaration {
                span: code.token_span(),
                ident: code.s1("ident").decl_ident(),
                items: vec![],
                end_ident_pos: None,
            })
        );
    }

    #[test]
    fn test_context_clause_items() {
        let code = Code::new(
            "\
context ident is
  library foo;
  use foo.bar;
  context foo.ctx;
end context;
",
        );
        assert_eq!(
            code.with_stream_no_diagnostics(parse_context),
            DeclarationOrReference::Declaration(ContextDeclaration {
                span: code.token_span(),
                ident: code.s1("ident").decl_ident(),
                items: vec![
                    ContextItem::Library(LibraryClause {
                        span: TokenSpan::new(code.s("library", 1).token(), code.s(";", 1).token()),
                        name_list: code.s1("foo").ident_list(),
                    }),
                    ContextItem::Use(UseClause {
                        span: code.s1("use foo.bar;").token_span(),
                        name_list: code.s1("foo.bar").name_list(),
                    }),
                    ContextItem::Context(ContextReference {
                        span: TokenSpan::new(code.s("context", 2).token(), code.s(";", 3).token()),
                        name_list: code.s1("foo.ctx").name_list(),
                    }),
                ],
                end_ident_pos: None,
            })
        )
    }

    #[test]
    pub fn test_pos_of_context_elements() {
        let code = Code::new(
            "\
context my_context is
  library ieee, env;
  context my_context;
  use ieee.std_logic_1164.all, std.env.xyz;
end my_context;
",
        );
        let ctx = code.tokenize();
        let lib = code.context_declaration();
        assert_eq!(
            lib.items[0].get_pos(&ctx),
            code.s1("library ieee, env;").pos()
        );
        assert_eq!(
            lib.items[1].get_pos(&ctx),
            code.s1("context my_context;").pos()
        );
        assert_eq!(
            lib.items[2].get_pos(&ctx),
            code.s1("use ieee.std_logic_1164.all, std.env.xyz;").pos()
        );
    }

    #[test]
    pub fn test_token_span() {
        let code = Code::new(
            "\
context my_context is
  library ieee, env;
  context my_context;
  use ieee.std_logic_1164.all, std.env.xyz;
end my_context;
",
        );
        let ctx = code.tokenize();
        let lib = code.context_declaration();

        let lib_token_string: Vec<String> = lib.items[0]
            .get_token_slice(&ctx)
            .iter()
            .map(token_to_string)
            .collect();
        let ctx_token_string: Vec<String> = lib.items[1]
            .get_token_slice(&ctx)
            .iter()
            .map(token_to_string)
            .collect();
        let use_token_string: Vec<String> = lib.items[2]
            .get_token_slice(&ctx)
            .iter()
            .map(token_to_string)
            .collect();

        assert_eq!(lib_token_string, vec!["library", "ieee", ",", "env", ";"],);
        assert_eq!(ctx_token_string, vec!["context", "my_context", ";"],);
        assert_eq!(
            use_token_string,
            vec![
                "use",
                "ieee",
                ".",
                "std_logic_1164",
                ".",
                "all",
                ",",
                "std",
                ".",
                "env",
                ".",
                "xyz",
                ";"
            ],
        );
    }
}
