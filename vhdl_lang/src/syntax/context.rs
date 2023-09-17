// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use super::common::check_end_identifier_mismatch;
use super::common::ParseResult;
use super::names::parse_name;
use super::tokens::{Kind::*, TokenStream};
use crate::ast::*;
use crate::data::*;
use crate::syntax::separated_list::{parse_ident_list, parse_name_list};

/// LRM 13. Design units and their analysis
pub fn parse_library_clause(stream: &TokenStream) -> ParseResult<LibraryClause> {
    let library_token = stream.expect_kind(Library)?.clone();
    let name_list = parse_ident_list(stream)?;
    let semi_token = stream.expect_kind(SemiColon)?.clone();
    Ok(LibraryClause {
        library_token,
        name_list,
        semi_token,
    })
}

/// LRM 12.4. Use clauses
pub fn parse_use_clause(stream: &TokenStream) -> ParseResult<UseClause> {
    let use_token = stream.expect_kind(Use)?.clone();

    let name_list = parse_name_list(stream)?;
    let semi_token = stream.expect_kind(SemiColon)?.clone();
    Ok(UseClause {
        use_token,
        name_list,
        semi_token,
    })
}

#[derive(PartialEq, Debug)]
pub enum DeclarationOrReference {
    Declaration(ContextDeclaration),
    Reference(ContextReference),
}

pub fn parse_context_reference(stream: &TokenStream) -> ParseResult<ContextReference> {
    let context_token = stream.expect_kind(Context)?.clone();

    let name_list = parse_name_list(stream)?;
    let semi_token = stream.expect_kind(SemiColon)?.clone();
    Ok(ContextReference {
        context_token,
        name_list,
        semi_token,
    })
}

/// LRM 13.4 Context clauses
pub fn parse_context(
    stream: &TokenStream,
    diagnostics: &mut dyn DiagnosticHandler,
) -> ParseResult<DeclarationOrReference> {
    let context_token = stream.expect_kind(Context)?.clone();
    let name = parse_name(stream)?;
    if stream.skip_if_kind(Is) {
        let mut items = Vec::with_capacity(16);
        let end_ident;
        loop {
            let token = stream.peek_expect()?;
            try_init_token_kind!(
                token,
                Library => items.push(ContextItem::Library(parse_library_clause(stream)?)),
                Use => items.push(ContextItem::Use(parse_use_clause(stream)?)),
                Context => items.push(ContextItem::Context(parse_context_reference(stream)?)),
                End => {
                    stream.skip();
                    stream.pop_if_kind(Context);
                    end_ident = stream.pop_optional_ident();
                    stream.expect_kind(SemiColon)?;
                    break;
                }
            )
        }

        let ident = WithDecl::new(to_simple_name(name)?);
        Ok(DeclarationOrReference::Declaration(ContextDeclaration {
            end_ident_pos: check_end_identifier_mismatch(&ident.tree, end_ident, diagnostics),
            ident,
            items,
        }))
    } else {
        // Context reference
        let mut name_list = Vec::new();
        while let Some(comma) = stream.pop_if_kind(Comma) {
            name_list.push((comma.clone(), parse_name(stream)?));
        }
        let name_list = SeparatedList {
            first: name,
            remainder: name_list,
        };
        let semi_token = stream.expect_kind(SemiColon)?.clone();
        Ok(DeclarationOrReference::Reference(ContextReference {
            context_token,
            name_list,
            semi_token,
        }))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::data::Diagnostic;
    use crate::syntax::test::Code;

    #[test]
    fn test_library_clause_single_name() {
        let code = Code::new("library foo;");
        assert_eq!(
            code.with_stream(parse_library_clause),
            LibraryClause {
                library_token: code.s1("library").token(),
                name_list: code.s1("foo").ident_list(),
                semi_token: code.s1(";").token(),
            }
        )
    }

    #[test]
    fn test_library_clause_multiple_names() {
        let code = Code::new("library foo, bar;");
        assert_eq!(
            code.with_stream(parse_library_clause),
            LibraryClause {
                library_token: code.s1("library").token(),
                name_list: code.s1("foo, bar").ident_list(),
                semi_token: code.s1(";").token(),
            },
        )
    }

    #[test]
    fn test_use_clause_single_name() {
        let code = Code::new("use lib.foo;");
        assert_eq!(
            code.with_stream(parse_use_clause),
            UseClause {
                use_token: code.s1("use").token(),
                name_list: code.s1("lib.foo").name_list(),
                semi_token: code.s1(";").token(),
            },
        )
    }

    #[test]
    fn test_use_clause_multiple_names() {
        let code = Code::new("use foo.'a', lib.bar.all;");
        assert_eq!(
            code.with_stream(parse_use_clause),
            UseClause {
                use_token: code.s1("use").token(),
                name_list: code.s1("foo.'a', lib.bar.all").name_list(),
                semi_token: code.s1(";").token(),
            },
        )
    }

    #[test]
    fn test_context_reference_single_name() {
        let code = Code::new("context lib.foo;");
        assert_eq!(
            code.with_stream_no_diagnostics(parse_context),
            DeclarationOrReference::Reference(ContextReference {
                context_token: code.s1("context").token(),
                name_list: code.s1("lib.foo").name_list(),
                semi_token: code.s1(";").token(),
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
                    ident: code.s1("ident").decl_ident(),
                    items: vec![],
                    end_ident_pos: if has_end_ident {
                        Some(code.s("ident", 2).pos())
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
            vec![Diagnostic::error(
                code.s1("ident2"),
                "End identifier mismatch, expected ident",
            )]
        );
        assert_eq!(
            context,
            DeclarationOrReference::Declaration(ContextDeclaration {
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
                ident: code.s1("ident").decl_ident(),
                items: vec![
                    ContextItem::Library(LibraryClause {
                        library_token: code.s1("library").token(),
                        name_list: code.s1("foo").ident_list(),
                        semi_token: code.s(";", 1).token(),
                    }),
                    ContextItem::Use(UseClause {
                        use_token: code.s1("use").token(),
                        name_list: code.s1("foo.bar").name_list(),
                        semi_token: code.s(";", 2).token(),
                    }),
                    ContextItem::Context(ContextReference {
                        context_token: code.s("context", 2).token(),
                        name_list: code.s1("foo.ctx").name_list(),
                        semi_token: code.s(";", 3).token(),
                    }),
                ],
                end_ident_pos: None,
            })
        )
    }
}
