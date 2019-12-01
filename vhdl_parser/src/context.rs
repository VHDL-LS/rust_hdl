// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use crate::ast::to_simple_name;
use crate::ast::{ContextDeclaration, ContextItem, ContextReference, LibraryClause, UseClause};
use crate::common::error_on_end_identifier_mismatch;
use crate::diagnostic::{push_some, DiagnosticHandler, ParseResult};
use crate::names::parse_name;
use crate::source::WithPos;
use crate::tokenizer::{Kind::*, Token};
use crate::tokenstream::TokenStream;

/// LRM 13. Design units and their analysis
pub fn parse_library_clause(stream: &mut TokenStream) -> ParseResult<WithPos<LibraryClause>> {
    let token = stream.expect_kind(Library)?;
    parse_library_clause_no_keyword(token, stream)
}

fn parse_library_clause_no_keyword(
    library_token: Token,
    stream: &mut TokenStream,
) -> ParseResult<WithPos<LibraryClause>> {
    let mut name_list = Vec::with_capacity(1);
    loop {
        name_list.push(stream.expect_ident()?);
        if !stream.skip_if_kind(Comma)? {
            break;
        }
    }
    let semi_token = stream.expect_kind(SemiColon)?;
    Ok(WithPos::from(
        LibraryClause { name_list },
        library_token.pos.combine_into(&semi_token),
    ))
}

/// LRM 12.4. Use clauses
pub fn parse_use_clause_no_keyword(
    use_token: Token,
    stream: &mut TokenStream,
) -> ParseResult<WithPos<UseClause>> {
    let mut name_list = Vec::with_capacity(1);
    loop {
        name_list.push(parse_name(stream)?);
        if !stream.skip_if_kind(Comma)? {
            break;
        }
    }
    let semi_token = stream.expect_kind(SemiColon)?;
    Ok(WithPos::from(
        UseClause { name_list },
        use_token.pos.combine_into(&semi_token),
    ))
}

pub fn parse_use_clause(stream: &mut TokenStream) -> ParseResult<WithPos<UseClause>> {
    let use_token = stream.expect_kind(Use)?;
    parse_use_clause_no_keyword(use_token, stream)
}

#[derive(PartialEq, Debug)]
pub enum DeclarationOrReference {
    Declaration(ContextDeclaration),
    Reference(WithPos<ContextReference>),
}

fn parse_context_reference_no_keyword(
    context_token: Token,
    stream: &mut TokenStream,
) -> ParseResult<WithPos<ContextReference>> {
    let name = parse_name(stream)?;
    let mut name_list = vec![name];
    loop {
        if !stream.skip_if_kind(Comma)? {
            break;
        }
        name_list.push(parse_name(stream)?);
    }
    let semi_token = stream.expect_kind(SemiColon)?;
    Ok(WithPos::from(
        ContextReference { name_list },
        context_token.pos.combine_into(&semi_token),
    ))
}

/// LRM 13.4 Context clauses
pub fn parse_context(
    stream: &mut TokenStream,
    diagnostics: &mut dyn DiagnosticHandler,
) -> ParseResult<DeclarationOrReference> {
    let context_token = stream.expect_kind(Context)?;
    let name = parse_name(stream)?;
    if stream.skip_if_kind(Is)? {
        let mut items = Vec::with_capacity(16);
        let end_ident;
        loop {
            let token = stream.expect()?;
            try_token_kind!(
                token,
                Library => items.push(parse_library_clause_no_keyword(token, stream)?.map_into(ContextItem::Library)),
                Use => items.push(parse_use_clause_no_keyword(token, stream)?.map_into(ContextItem::Use)),
                Context => items.push(parse_context_reference_no_keyword(token, stream)?.map_into(ContextItem::Context)),
                End => {
                    stream.pop_if_kind(Context)?;
                    end_ident = stream.pop_optional_ident()?;
                    stream.expect_kind(SemiColon)?;
                    break;
                }
            )
        }

        let ident = to_simple_name(name)?;

        push_some(
            diagnostics,
            error_on_end_identifier_mismatch(&ident, &end_ident),
        );

        Ok(DeclarationOrReference::Declaration(ContextDeclaration {
            ident,
            items,
        }))
    } else {
        // Context reference
        let mut name_list = vec![name];
        loop {
            if !stream.skip_if_kind(Comma)? {
                break;
            }
            name_list.push(parse_name(stream)?);
        }
        let semi_token = stream.expect_kind(SemiColon)?;
        Ok(DeclarationOrReference::Reference(WithPos::from(
            ContextReference { name_list },
            context_token.pos.combine_into(&semi_token),
        )))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::diagnostic::Diagnostic;
    use crate::test_util::Code;

    #[test]
    fn test_library_clause_single_name() {
        let code = Code::new("library foo;");
        assert_eq!(
            code.with_stream(parse_library_clause),
            WithPos::new(
                LibraryClause {
                    name_list: vec![code.s1("foo").ident()]
                },
                code
            )
        )
    }

    #[test]
    fn test_library_clause_multiple_names() {
        let code = Code::new("library foo, bar;");
        assert_eq!(
            code.with_stream(parse_library_clause),
            WithPos::new(
                LibraryClause {
                    name_list: vec![code.s1("foo").ident(), code.s1("bar").ident()]
                },
                code
            )
        )
    }

    #[test]
    fn test_use_clause_single_name() {
        let code = Code::new("use lib.foo;");
        assert_eq!(
            code.with_stream(parse_use_clause),
            WithPos::new(
                UseClause {
                    name_list: vec![code.s1("lib.foo").name()]
                },
                code
            )
        )
    }

    #[test]
    fn test_use_clause_multiple_names() {
        let code = Code::new("use foo.'a', lib.bar.all;");
        assert_eq!(
            code.with_stream(parse_use_clause),
            WithPos::new(
                UseClause {
                    name_list: vec![code.s1("foo.'a'").name(), code.s1("lib.bar.all").name()]
                },
                code
            )
        )
    }

    #[test]
    fn test_context_reference_single_name() {
        let code = Code::new("context lib.foo;");
        assert_eq!(
            code.with_stream_no_diagnostics(parse_context),
            DeclarationOrReference::Reference(WithPos::new(
                ContextReference {
                    name_list: vec![code.s1("lib.foo").name()]
                },
                code
            ))
        )
    }

    #[test]
    fn test_context_reference_multiple_names() {
        let code = Code::new("context work.foo, lib.bar.all;");
        assert_eq!(
            code.with_stream_no_diagnostics(parse_context),
            DeclarationOrReference::Reference(WithPos::new(
                ContextReference {
                    name_list: vec![code.s1("work.foo").name(), code.s1("lib.bar.all").name()]
                },
                code
            ))
        )
    }

    #[test]
    fn test_context_clause() {
        let variants = vec![
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
        for variant in variants {
            let code = Code::new(variant);
            assert_eq!(
                code.with_stream_no_diagnostics(parse_context),
                DeclarationOrReference::Declaration(ContextDeclaration {
                    ident: code.s1("ident").ident(),
                    items: vec![]
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
            vec![Diagnostic::error(
                code.s1("ident2"),
                "End identifier mismatch, expected ident"
            )]
        );
        assert_eq!(
            context,
            DeclarationOrReference::Declaration(ContextDeclaration {
                ident: code.s1("ident").ident(),
                items: vec![]
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
                ident: code.s1("ident").ident(),
                items: vec![
                    WithPos::new(
                        ContextItem::Library(LibraryClause {
                            name_list: vec![code.s1("foo").ident()]
                        }),
                        code.s1("library foo;")
                    ),
                    WithPos::new(
                        ContextItem::Use(UseClause {
                            name_list: vec![code.s1("foo.bar").name()]
                        }),
                        code.s1("use foo.bar;")
                    ),
                    WithPos::new(
                        ContextItem::Context(ContextReference {
                            name_list: vec![code.s1("foo.ctx").name()]
                        }),
                        code.s1("context foo.ctx;")
                    ),
                ]
            })
        )
    }
}
