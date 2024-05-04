// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use super::common::ParseResult;
/// LRM 8. Names
use super::expression::parse_expression;
use super::subprogram::parse_signature;
use super::subtype_indication::parse_subtype_indication;
use super::tokens::{Kind::*, TokenAccess};
use crate::ast;
use crate::ast::{Literal, *};
use crate::data::error_codes::ErrorCode;
use crate::data::{Diagnostic, WithTokenSpan};
use crate::syntax::separated_list::parse_list_with_separator_or_recover;
use crate::syntax::TokenId;
use vhdl_lang::syntax::parser::ParsingContext;
use vhdl_lang::TokenSpan;

pub fn parse_designator(ctx: &mut ParsingContext<'_>) -> ParseResult<WithToken<Designator>> {
    Ok(expect_token!(
        ctx.stream,
        token,
        token_id,
        Identifier => token.to_identifier_value(token_id)?.map_into(Designator::Identifier),
        StringLiteral => token.to_operator_symbol(token_id)?.map_into(Designator::OperatorSymbol),
        Character => token.to_character_value(token_id)?.map_into(Designator::Character)
    ))
}

pub fn parse_selected_name(ctx: &mut ParsingContext<'_>) -> ParseResult<WithTokenSpan<Name>> {
    let mut name = parse_designator(ctx)?
        .into_ref()
        .map_into_span(Name::Designator);
    loop {
        if !ctx.stream.skip_if_kind(Dot) {
            break;
        }
        if let Some(tok) = ctx.stream.pop_if_kind(All) {
            let span = name.span.with_end(tok);
            name = WithTokenSpan::from(Name::SelectedAll(Box::new(name)), span);
        } else {
            let suffix = parse_designator(ctx)?.into_ref();
            let span = name.span.with_end(suffix.token);
            name = WithTokenSpan::from(Name::Selected(Box::new(name), suffix), span);
        }
    }
    Ok(name)
}

pub fn parse_type_mark(ctx: &mut ParsingContext<'_>) -> ParseResult<WithTokenSpan<TypeMark>> {
    let name = parse_selected_name(ctx)?;
    parse_type_mark_starting_with_name(ctx, name)
}

pub fn parse_type_mark_starting_with_name(
    ctx: &mut ParsingContext<'_>,
    name: WithTokenSpan<Name>,
) -> ParseResult<WithTokenSpan<TypeMark>> {
    let state = ctx.stream.state();

    // Check if it is a type mark with a subtype or element attribute:
    // Example: signal sig0 : sig1'subtype;
    if ctx.stream.pop_if_kind(Tick).is_some() {
        if let Ok(attr) = ctx.stream.expect_attribute_designator() {
            if let AttributeDesignator::Type(typattr) = attr.item {
                return Ok(WithTokenSpan {
                    item: TypeMark {
                        name,
                        attr: Some(typattr),
                    },
                    span: attr.token.into(),
                });
            }
        }

        ctx.stream.set_state(state);
    };

    let span = name.span;
    Ok(WithTokenSpan {
        item: TypeMark { name, attr: None },
        span,
    })
}

impl Name {
    pub fn expect_selected(&self) -> Result<(), String> {
        match &self {
            Name::Designator(_) => Ok(()),
            Name::Selected(prefix, _) | Name::SelectedAll(prefix) => prefix.item.expect_selected(),
            _ => Err("Expected selected name".into()),
        }
    }
}

impl WithTokenSpan<Name> {
    pub fn expect_selected(&self, ctx: &dyn TokenAccess) -> Result<(), Diagnostic> {
        match self.item.expect_selected() {
            Ok(_) => Ok(()),
            Err(msg) => Err(Diagnostic::syntax_error(self.to_pos(ctx), msg)),
        }
    }
}

pub fn expression_to_ident(
    ctx: &mut ParsingContext<'_>,
    name: WithTokenSpan<Expression>,
) -> ParseResult<Ident> {
    let name = expression_to_name(ctx, name)?;
    to_simple_name(ctx, name)
}

pub fn parse_identifier_list(ctx: &mut ParsingContext<'_>) -> ParseResult<Vec<Ident>> {
    let mut idents = Vec::new();
    loop {
        idents.push(ctx.stream.expect_ident()?);
        if !ctx.stream.skip_if_kind(Comma) {
            break;
        }
    }
    Ok(idents)
}

fn expression_to_name(
    ctx: &dyn TokenAccess,
    expr: WithTokenSpan<Expression>,
) -> ParseResult<WithTokenSpan<Name>> {
    match expr.item {
        Expression::Name(name) => Ok(WithTokenSpan {
            item: *name,
            span: expr.span,
        }),
        Expression::Literal(Literal::String(val)) => {
            if let Some(op) = Operator::from_latin1(val) {
                Ok(WithTokenSpan {
                    item: Name::Designator(Designator::OperatorSymbol(op).into_ref()),
                    span: expr.span,
                })
            } else {
                Err(Diagnostic::syntax_error(
                    expr.span.to_pos(ctx),
                    "Invalid operator symbol",
                ))
            }
        }
        Expression::Literal(Literal::Character(val)) => Ok(WithTokenSpan {
            item: Name::Designator(Designator::Character(val).into_ref()),
            span: expr.span,
        }),
        _ => Err(Diagnostic::syntax_error(expr.to_pos(ctx), "Expected name")),
    }
}

fn actual_to_expression(
    ctx: &dyn TokenAccess,
    actual: WithTokenSpan<ActualPart>,
) -> ParseResult<WithTokenSpan<Expression>> {
    match actual.item {
        ActualPart::Expression(expr) => Ok(WithTokenSpan::from(expr, actual.span)),
        _ => Err(Diagnostic::syntax_error(
            actual.to_pos(ctx),
            "Expected expression",
        )),
    }
}

fn actual_part_to_name(
    ctx: &dyn TokenAccess,
    actual: WithTokenSpan<ActualPart>,
) -> ParseResult<WithTokenSpan<Name>> {
    match actual.item {
        ActualPart::Expression(expr) => {
            expression_to_name(ctx, WithTokenSpan::from(expr, actual.span))
        }
        _ => Err(Diagnostic::syntax_error(
            actual.to_pos(ctx),
            "Expected name",
        )),
    }
}

fn assoc_to_expression(
    ctx: &dyn TokenAccess,
    assoc: AssociationElement,
) -> ParseResult<WithTokenSpan<Expression>> {
    match assoc.formal {
        Some(name) => Err(Diagnostic::syntax_error(
            name.to_pos(ctx),
            "Expected expression",
        )),
        None => actual_to_expression(ctx, assoc.actual),
    }
}

fn parse_actual_part(ctx: &mut ParsingContext<'_>) -> ParseResult<WithTokenSpan<ActualPart>> {
    if let Some(token) = ctx.stream.pop_if_kind(Open) {
        Ok(WithTokenSpan::from(ActualPart::Open, token))
    } else {
        Ok(parse_expression(ctx)?.map_into(ActualPart::Expression))
    }
}

pub fn parse_association_element(ctx: &mut ParsingContext<'_>) -> ParseResult<AssociationElement> {
    let actual = parse_actual_part(ctx)?;
    if ctx.stream.skip_if_kind(RightArrow) {
        Ok(AssociationElement {
            formal: Some(actual_part_to_name(ctx, actual)?),
            actual: parse_actual_part(ctx)?,
        })
    } else {
        Ok(AssociationElement {
            formal: None,
            actual,
        })
    }
}

pub fn parse_association_list(
    ctx: &mut ParsingContext<'_>,
) -> ParseResult<(SeparatedList<AssociationElement>, TokenId)> {
    let left_par = ctx.stream.expect_kind(LeftPar)?;
    parse_association_list_no_leftpar(ctx, left_par)
}

pub fn parse_association_list_no_leftpar(
    ctx: &mut ParsingContext<'_>,
    left_par: TokenId,
) -> ParseResult<(SeparatedList<AssociationElement>, TokenId)> {
    if let Some(right_par) = ctx.stream.pop_if_kind(RightPar) {
        ctx.diagnostics.add(
            ctx.stream.get_span(left_par, right_par),
            "Association list cannot be empty",
            ErrorCode::SyntaxError,
        );
        return Ok((SeparatedList::default(), right_par));
    }
    let list = parse_list_with_separator_or_recover(
        ctx,
        Comma,
        parse_association_element,
        Some(RightPar),
    )?;
    let right_par = ctx.stream.expect_kind(RightPar)?;
    Ok((list, right_par))
}

fn parse_function_call(
    ctx: &mut ParsingContext<'_>,
    prefix: WithTokenSpan<Name>,
    first: AssociationElement,
) -> ParseResult<WithTokenSpan<Name>> {
    let mut association_elements = Vec::new();
    association_elements.push(first);

    loop {
        association_elements.push(parse_association_element(ctx)?);
        expect_token!(
            ctx.stream,
            token,
            Comma => {},
            RightPar => {
                let span = TokenSpan::new(prefix.span.start_token, ctx.stream.get_current_token_id());
                return Ok(WithTokenSpan {
                    item: Name::CallOrIndexed(Box::new(CallOrIndexed {
                        name: prefix,
                        parameters: association_elements})),
                    span,
                });
            }
        )
    }
}

fn parse_attribute_name(
    ctx: &mut ParsingContext<'_>,
    name: WithTokenSpan<Name>,
    signature: Option<WithTokenSpan<Signature>>,
) -> ParseResult<WithTokenSpan<Name>> {
    let attr = ctx.stream.expect_attribute_designator()?;

    let (expression, span) = {
        if ctx.stream.skip_if_kind(LeftPar) {
            let ret = Some(parse_expression(ctx)?);
            let rpar_token = ctx.stream.expect_kind(RightPar)?;
            (ret, name.span.with_end(rpar_token))
        } else {
            (None, name.span.with_end(attr.token))
        }
    };

    Ok(WithTokenSpan {
        item: Name::Attribute(Box::new(AttributeName {
            name,
            attr,
            signature,
            expr: expression.map(Box::new),
        })),
        span,
    })
}

enum DesignatorOrAll {
    Designator(Designator),
    All,
}

fn parse_suffix(ctx: &mut ParsingContext<'_>) -> ParseResult<WithToken<DesignatorOrAll>> {
    let name = {
        expect_token!(
            ctx.stream,
            token,
            token_id,
            Identifier => token.to_identifier_value(token_id)?.map_into(|ident| DesignatorOrAll::Designator(Designator::Identifier(ident))),
            Character => token.to_character_value(token_id)?.map_into(|byte| DesignatorOrAll::Designator(Designator::Character(byte))),
            StringLiteral => token.to_operator_symbol(token_id)?.map_into(|string| DesignatorOrAll::Designator(Designator::OperatorSymbol(string))),
            All => WithToken::new(DesignatorOrAll::All, token_id)
        )
    };

    Ok(name)
}

/// LRM 8.7 External names
/// Inside of << >>
fn parse_inner_external_name(ctx: &mut ParsingContext<'_>) -> ParseResult<ExternalName> {
    let token = ctx.stream.peek_expect()?;
    let class = try_init_token_kind!(
        token,
        Signal => ExternalObjectClass::Signal,
        Constant => ExternalObjectClass::Constant,
        Variable => ExternalObjectClass::Variable);
    ctx.stream.skip();

    let path = peek_token!(
        ctx.stream, token, token_id,
        CommAt => {
            ctx.stream.skip();
            let path_name = parse_name(ctx)?;
            let path_pos = path_name.span.with_end(token_id);
            WithTokenSpan::from(ExternalPath::Package(path_name), path_pos)
        },
        Dot => {
            ctx.stream.skip();
            let path_name = parse_name(ctx)?;
            let path_pos = path_name.span.with_end(token_id);
            WithTokenSpan::from(ExternalPath::Absolute(path_name), path_pos)
        },
        Circ => {
            ctx.stream.skip();
            ctx.stream.expect_kind(Dot)?;
            let mut up_levels = 1;
            while ctx.stream.skip_if_kind(Circ) {
                ctx.stream.expect_kind(Dot)?;
                up_levels += 1;
            }
            let path_name = parse_name(ctx)?;
            let path_pos = path_name.span.with_end(token_id);
            WithTokenSpan::from(ExternalPath::Relative(path_name, up_levels), path_pos)
        },
        Identifier => {
            let path_name = parse_name(ctx)?;
            let path_span = path_name.span;
            WithTokenSpan::from(ExternalPath::Relative(path_name, 0), path_span)
        }
    );

    ctx.stream.expect_kind(Colon)?;
    let subtype = parse_subtype_indication(ctx)?;

    Ok(ExternalName {
        class,
        path,
        subtype,
    })
}

/// LRM 8. Names
fn _parse_name(ctx: &mut ParsingContext<'_>) -> ParseResult<WithTokenSpan<Name>> {
    let mut name = {
        if let Some(token) = ctx.stream.pop_if_kind(LtLt) {
            let external_name = Name::External(Box::new(parse_inner_external_name(ctx)?));
            let end_token = ctx.stream.expect_kind(GtGt)?;
            WithTokenSpan::from(external_name, TokenSpan::new(token, end_token))
        } else {
            let suffix = parse_suffix(ctx)?;
            match suffix.item {
                DesignatorOrAll::Designator(designator) => {
                    WithTokenSpan::from(Name::Designator(designator.into_ref()), suffix.token)
                }
                DesignatorOrAll::All => {
                    return Err(Diagnostic::syntax_error(
                        suffix.pos(ctx),
                        "Illegal prefix 'all' for name",
                    ));
                }
            }
        }
    };

    while let Some(token) = ctx.stream.peek() {
        match token.kind {
            Dot => {
                ctx.stream.skip();
                let suffix = parse_suffix(ctx)?;
                let span = name.span.with_end(suffix.token);

                match suffix.item {
                    DesignatorOrAll::Designator(designator) => {
                        name = WithTokenSpan {
                            item: Name::Selected(
                                Box::new(name),
                                WithToken::new(designator.into_ref(), suffix.token),
                            ),
                            span,
                        }
                    }
                    DesignatorOrAll::All => {
                        name = WithTokenSpan {
                            item: Name::SelectedAll(Box::new(name)),
                            span,
                        }
                    }
                }
            }
            LeftSquare => {
                let state = ctx.stream.state();
                let signature = Some(parse_signature(ctx)?);
                if !ctx.stream.skip_if_kind(Tick) {
                    // Alias may have prefix[signature] without tick
                    ctx.stream.set_state(state);
                    break;
                }
                name = parse_attribute_name(ctx, name, signature)?;
            }
            Tick => {
                if ctx.stream.nth_kind_is(1, LeftPar) {
                    break;
                }
                ctx.stream.skip();
                let signature = None;
                name = parse_attribute_name(ctx, name, signature)?;
            }
            LeftPar => {
                ctx.stream.skip();
                if let Some(right_par) = ctx.stream.pop_if_kind(RightPar) {
                    return Err(Diagnostic::syntax_error(
                        token.pos.combine(ctx.stream.get_pos(right_par)),
                        "Association list cannot be empty",
                    ));
                }
                let assoc = parse_association_element(ctx)?;
                expect_token!(
                    ctx.stream,
                    sep_token,
                    sep_token_id,
                    Comma => {
                        name = parse_function_call(ctx, name, assoc)?;
                    },
                    To | Downto => {
                        let right_expr = parse_expression(ctx)?;
                        let direction = {
                            if sep_token.kind == To {
                                Direction::Ascending
                            } else {
                                Direction::Descending
                            }
                        };
                        let rpar_token = ctx.stream.expect_kind(RightPar)?;
                        let span = TokenSpan::new(rpar_token, name.span.end_token);
                        let discrete_range =
                            DiscreteRange::Range(ast::Range::Range(RangeConstraint {
                                left_expr: Box::new(assoc_to_expression(ctx, assoc)?),
                                direction,
                                right_expr: Box::new(right_expr),
                            }));

                        name = WithTokenSpan {
                            item: Name::Slice(Box::new(name), Box::new(discrete_range)),
                            span,
                        };
                    },
                    RightPar => {
                        let pos = TokenSpan::new(sep_token_id, name.span.end_token);
                        let item = match into_range(assoc) {
                            Ok(range) => Name::Slice(Box::new(name), Box::new(DiscreteRange::Range(range))),
                            Err(assoc) => Name::CallOrIndexed(Box::new(CallOrIndexed {
                                name,
                                parameters: vec![assoc],
                            })),
                        };

                        name = WithTokenSpan::new(item, pos);
                    }
                )
            }
            _ => {
                break;
            }
        }
    }

    Ok(name)
}

pub fn into_range(assoc: AssociationElement) -> Result<ast::Range, AssociationElement> {
    if assoc.formal.is_some() {
        return Err(assoc);
    }

    if let ActualPart::Expression(Expression::Name(ref name)) = &assoc.actual.item {
        if let Name::Attribute(attr) = name.as_ref() {
            if attr.as_range().is_some() {
                if let ActualPart::Expression(Expression::Name(name)) = assoc.actual.item {
                    if let Name::Attribute(attr) = *name {
                        return Ok(ast::Range::Attribute(attr));
                    }
                }
                unreachable!();
            } else {
                Err(assoc)
            }
        } else {
            Err(assoc)
        }
    } else {
        Err(assoc)
    }
}

pub fn parse_name(ctx: &mut ParsingContext<'_>) -> ParseResult<WithTokenSpan<Name>> {
    let state = ctx.stream.state();
    _parse_name(ctx).map_err(|err| {
        ctx.stream.set_state(state);
        err
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::test::Code;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_parse_selected_name_single() {
        let code = Code::new("foo");
        assert_eq!(
            code.with_stream(parse_selected_name),
            code.s1("foo")
                .ident()
                .map_into_span(|sym| Name::Designator(Designator::Identifier(sym).into_ref()))
        );
    }

    #[test]
    fn test_parse_selected_name_multiple() {
        let code = Code::new("foo.bar.baz");
        let baz = code
            .s1("baz")
            .ident()
            .map_into(Designator::Identifier)
            .into_ref();
        let bar = code
            .s1("bar")
            .ident()
            .map_into(Designator::Identifier)
            .into_ref();
        let foo = code
            .s1("foo")
            .ident()
            .map_into(Designator::Identifier)
            .into_ref()
            .map_into_span(Name::Designator);
        let foo_bar = WithTokenSpan::from(
            Name::Selected(Box::new(foo), bar),
            code.s1("foo.bar").token_span(),
        );
        let foo_bar_baz = WithTokenSpan::from(
            Name::Selected(Box::new(foo_bar), baz),
            code.s1("foo.bar.baz").token_span(),
        );

        assert_eq!(code.with_stream(parse_selected_name), foo_bar_baz);
    }

    #[test]
    fn test_parse_selected_name_all() {
        let code = Code::new("foo.all");
        let foo = code
            .s1("foo")
            .ident()
            .map_into(Designator::Identifier)
            .into_ref()
            .map_into_span(Name::Designator);
        let foo_all = WithTokenSpan::from(
            Name::SelectedAll(Box::new(foo)),
            code.s1("foo.all").token_span(),
        );
        assert_eq!(code.with_stream(parse_selected_name), foo_all);
    }

    #[test]
    fn test_identifier_list() {
        let code = Code::new("foo, bar, baz");
        assert_eq!(
            code.with_stream(parse_identifier_list),
            vec![
                code.s1("foo").ident(),
                code.s1("bar").ident(),
                code.s1("baz").ident()
            ]
        );
    }

    #[test]
    fn test_simple_name() {
        let code = Code::new("foo");
        assert_eq!(
            code.with_stream(parse_name),
            WithTokenSpan {
                item: Name::Designator(Designator::Identifier(code.symbol("foo")).into_ref()),
                span: code.s1("foo").token_span()
            }
        );
    }

    #[test]
    fn test_characer_name() {
        let code = Code::new("'a'");
        assert_eq!(
            code.with_stream(parse_name),
            WithTokenSpan {
                item: Name::Designator(Designator::Character(b'a').into_ref()),
                span: code.s1("'a'").token_span()
            }
        );
    }

    #[test]
    fn test_operator_symbol() {
        let code = Code::new("\"+\"");
        assert_eq!(
            code.with_stream(parse_name),
            WithTokenSpan {
                item: Name::Designator(Designator::OperatorSymbol(Operator::Plus).into_ref()),
                span: code.s1("\"+\"").token_span()
            }
        );

        // Upper case
        let code = Code::new("\"AND\"");
        assert_eq!(
            code.with_stream(parse_name),
            WithTokenSpan {
                item: Name::Designator(Designator::OperatorSymbol(Operator::And).into_ref()),
                span: code.s1("\"AND\"").token_span()
            }
        );

        // Lower case
        let code = Code::new("\"and\"");
        assert_eq!(
            code.with_stream(parse_name),
            WithTokenSpan {
                item: Name::Designator(Designator::OperatorSymbol(Operator::And).into_ref()),
                span: code.s1("\"and\"").token_span()
            }
        );
    }

    #[test]
    fn test_selected_name() {
        let code = Code::new("foo.bar.baz");

        let foo = WithTokenSpan {
            item: Name::Designator(Designator::Identifier(code.symbol("foo")).into_ref()),
            span: code.s1("foo").token_span(),
        };

        let bar = WithToken {
            item: Designator::Identifier(code.symbol("bar")),
            token: code.s1("bar").token(),
        };

        let baz = WithToken {
            item: Designator::Identifier(code.symbol("baz")),
            token: code.s1("baz").token(),
        };

        let foo_bar = WithTokenSpan {
            item: Name::Selected(Box::new(foo), bar.into_ref()),
            span: code.s1("foo.bar").token_span(),
        };

        let foo_bar_baz = WithTokenSpan {
            item: Name::Selected(Box::new(foo_bar), baz.into_ref()),
            span: code.s1("foo.bar.baz").token_span(),
        };

        assert_eq!(code.with_stream(parse_name), foo_bar_baz);
    }

    #[test]
    fn test_selected_name_all() {
        let code = Code::new("foo.all");

        let foo = WithTokenSpan {
            item: Name::Designator(Designator::Identifier(code.symbol("foo")).into_ref()),
            span: code.s1("foo").token_span(),
        };

        let foo_all = WithTokenSpan {
            item: Name::SelectedAll(Box::new(foo)),
            span: code.s1("foo.all").token_span(),
        };

        assert_eq!(code.with_stream(parse_name), foo_all);
    }

    #[test]
    fn test_slice_name_range_to() {
        let code = Code::new("prefix(0 to 3)");
        let prefix = WithTokenSpan {
            item: Name::Designator(Designator::Identifier(code.symbol("prefix")).into_ref()),
            span: code.s1("prefix").token_span(),
        };
        let slice = WithTokenSpan {
            item: Name::Slice(
                Box::new(prefix),
                Box::new(code.s1("0 to 3").discrete_range()),
            ),
            span: code.s1("prefix(0 to 3)").token_span(),
        };
        assert_eq!(code.with_stream(parse_name), slice);
    }

    #[test]
    fn test_slice_name_range_downto() {
        let code = Code::new("prefix(3 downto 0)");
        let prefix = WithTokenSpan {
            item: Name::Designator(Designator::Identifier(code.symbol("prefix")).into_ref()),
            span: code.s1("prefix").token_span(),
        };
        let slice = WithTokenSpan {
            item: Name::Slice(
                Box::new(prefix),
                Box::new(code.s1("3 downto 0").discrete_range()),
            ),
            span: code.s1("prefix(3 downto 0)").token_span(),
        };
        assert_eq!(code.with_stream(parse_name), slice);
    }

    #[test]
    fn test_slice_range_attribute() {
        let code = Code::new("prefix(foo(0)'range)");
        let prefix = WithTokenSpan {
            item: Name::Designator(Designator::Identifier(code.symbol("prefix")).into_ref()),
            span: code.s1("prefix").token_span(),
        };
        let slice = WithTokenSpan {
            item: Name::Slice(
                Box::new(prefix),
                Box::new(code.s1("foo(0)'range").discrete_range()),
            ),
            span: code.s1("prefix(foo(0)'range)").token_span(),
        };
        assert_eq!(code.with_stream(parse_name), slice);
    }

    #[test]
    fn test_attribute_name() {
        let code = Code::new("prefix'foo");
        let prefix = WithTokenSpan {
            item: Name::Designator(Designator::Identifier(code.symbol("prefix")).into_ref()),
            span: code.s1("prefix").token_span(),
        };
        let attr = WithTokenSpan {
            item: Name::Attribute(Box::new(AttributeName {
                name: prefix,
                attr: code.s1("foo").attr_ident(),
                signature: None,
                expr: None,
            })),
            span: code.s1("prefix'foo").token_span(),
        };
        assert_eq!(code.with_stream(parse_name), attr);
    }

    #[test]
    fn test_attribute_name_range() {
        let code = Code::new("prefix'range");
        let prefix = WithTokenSpan {
            item: Name::Designator(Designator::Identifier(code.symbol("prefix")).into_ref()),
            span: code.s1("prefix").token_span(),
        };
        let attr = WithTokenSpan {
            item: Name::Attribute(Box::new(AttributeName {
                name: prefix,
                attr: WithToken {
                    item: AttributeDesignator::Range(RangeAttribute::Range),
                    token: code.s1("range").token(),
                },
                signature: None,
                expr: None,
            })),
            span: code.s1("prefix'range").token_span(),
        };
        assert_eq!(code.with_stream(parse_name), attr);
    }

    #[test]
    fn test_attribute_name_subtype() {
        let code = Code::new("prefix'subtype");
        let prefix = WithTokenSpan {
            item: Name::Designator(Designator::Identifier(code.symbol("prefix")).into_ref()),
            span: code.s1("prefix").token_span(),
        };
        let attr = WithTokenSpan {
            item: Name::Attribute(Box::new(AttributeName {
                name: prefix,
                attr: WithToken {
                    item: AttributeDesignator::Type(TypeAttribute::Subtype),
                    token: code.s1("subtype").token(),
                },
                signature: None,
                expr: None,
            })),
            span: code.s1("prefix'subtype").token_span(),
        };
        assert_eq!(code.with_stream(parse_name), attr);
    }

    #[test]
    fn test_attribute_name_element() {
        let code = Code::new("prefix'element");
        let prefix = WithTokenSpan {
            item: Name::Designator(Designator::Identifier(code.symbol("prefix")).into_ref()),
            span: code.s1("prefix").token_span(),
        };
        let attr = WithTokenSpan {
            item: Name::Attribute(Box::new(AttributeName {
                name: prefix,
                attr: WithToken {
                    item: AttributeDesignator::Type(TypeAttribute::Element),
                    token: code.s1("element").token(),
                },
                signature: None,
                expr: None,
            })),
            span: code.s1("prefix'element").token_span(),
        };
        assert_eq!(code.with_stream(parse_name), attr);
    }

    #[test]
    fn test_type_mark_without_subtype() {
        let code = Code::new("prefix");
        let name = code.s1("prefix").name();

        assert_eq!(
            code.with_stream(parse_type_mark),
            WithTokenSpan {
                span: name.span,
                item: TypeMark { name, attr: None },
            }
        );
    }

    #[test]
    fn test_type_mark_with_subtype() {
        let code = Code::new("prefix'subtype");

        assert_eq!(
            code.with_stream(parse_type_mark),
            WithTokenSpan {
                span: code.token_span(),
                item: TypeMark {
                    name: code.s1("prefix").name(),
                    attr: Some(TypeAttribute::Subtype)
                },
            }
        );
    }

    #[test]
    fn test_type_mark_with_element() {
        let code = Code::new("prefix'element");

        assert_eq!(
            code.with_stream(parse_type_mark),
            WithTokenSpan {
                span: code.token_span(),
                item: TypeMark {
                    name: code.s1("prefix").name(),
                    attr: Some(TypeAttribute::Element)
                },
            }
        );
    }

    #[test]
    fn test_attribute_name_expression() {
        let code = Code::new("prefix'foo(expr+1)");
        let prefix = WithTokenSpan {
            item: Name::Designator(Designator::Identifier(code.symbol("prefix")).into_ref()),
            span: code.s1("prefix").token_span(),
        };
        let attr = WithTokenSpan {
            item: Name::Attribute(Box::new(AttributeName {
                name: prefix,
                attr: code.s1("foo").attr_ident(),
                signature: None,
                expr: Some(Box::new(code.s1("expr+1").expr())),
            })),
            span: code.s1("prefix'foo(expr+1)").token_span(),
        };
        assert_eq!(code.with_stream(parse_name), attr);
    }

    #[test]
    fn test_attribute_name_signature_expression() {
        let code = Code::new("prefix[return natural]'foo(expr+1)");
        let prefix = WithTokenSpan {
            item: Name::Designator(Designator::Identifier(code.symbol("prefix")).into_ref()),
            span: code.s1("prefix").token_span(),
        };
        let attr = WithTokenSpan {
            item: Name::Attribute(Box::new(AttributeName {
                name: prefix,
                attr: code.s1("foo").attr_ident(),
                signature: Some(code.s1("[return natural]").signature()),
                expr: Some(Box::new(code.s1("expr+1").expr())),
            })),
            span: code.s1("prefix[return natural]'foo(expr+1)").token_span(),
        };
        assert_eq!(code.with_stream(parse_name), attr);
    }

    #[test]
    fn test_name_signature_no_attribute_name() {
        // Alias declarations may use name[signature]
        let code = Code::new("prefix[return natural]");
        let name = code.with_partial_stream(|ctx| {
            let result = parse_name(ctx);
            ctx.stream.expect_kind(LeftSquare)?;
            result
        });
        let prefix = WithTokenSpan {
            item: Name::Designator(Designator::Identifier(code.symbol("prefix")).into_ref()),
            span: code.s1("prefix").token_span(),
        };
        assert_eq!(name, Ok(prefix));
    }

    #[test]
    fn test_qualified_expression_is_not_name() {
        let code = Code::new("prefix'(");
        let name = code.with_stream(|ctx| {
            let result = parse_name(ctx);
            ctx.stream.expect_kind(Tick)?;
            ctx.stream.expect_kind(LeftPar)?;
            result
        });
        let prefix = WithTokenSpan {
            item: Name::Designator(Designator::Identifier(code.symbol("prefix")).into_ref()),
            span: code.s1("prefix").token_span(),
        };
        assert_eq!(name, prefix);
    }

    #[test]
    fn test_function_call_no_formal() {
        let code = Code::new("foo(0)");

        let foo = WithTokenSpan {
            item: Name::Designator(Designator::Identifier(code.symbol("foo")).into_ref()),
            span: code.s1("foo").token_span(),
        };

        let foo_0 = WithTokenSpan {
            item: Name::CallOrIndexed(Box::new(CallOrIndexed {
                name: foo,
                parameters: vec![AssociationElement {
                    formal: None,
                    actual: code.s1("0").expr().map_into(ActualPart::Expression),
                }],
            })),
            span: code.s1("foo(0)").token_span(),
        };

        assert_eq!(code.with_stream(parse_name), foo_0);
    }

    #[test]
    fn test_function_call_many() {
        let code = Code::new("prefix(0, 1)(3).suffix");

        let prefix = WithTokenSpan {
            item: Name::Designator(Designator::Identifier(code.symbol("prefix")).into_ref()),
            span: code.s1("prefix").token_span(),
        };

        let prefix_index = WithTokenSpan {
            item: Name::CallOrIndexed(Box::new(CallOrIndexed {
                name: prefix,
                parameters: vec![
                    AssociationElement {
                        formal: None,
                        actual: code.s1("0").expr().map_into(ActualPart::Expression),
                    },
                    AssociationElement {
                        formal: None,
                        actual: code.s1("1").expr().map_into(ActualPart::Expression),
                    },
                ],
            })),
            span: code.s1("prefix(0, 1)").token_span(),
        };

        let prefix_index_3 = WithTokenSpan {
            item: Name::CallOrIndexed(Box::new(CallOrIndexed {
                name: prefix_index,
                parameters: vec![AssociationElement {
                    formal: None,
                    actual: code.s1("3").expr().map_into(ActualPart::Expression),
                }],
            })),
            span: code.s1("prefix(0, 1)(3)").token_span(),
        };

        let suffix = WithToken {
            item: Designator::Identifier(code.symbol("suffix")),
            token: code.s1("suffix").token(),
        };

        let prefix_index_3_suffix = WithTokenSpan {
            item: Name::Selected(Box::new(prefix_index_3), suffix.into_ref()),
            span: code.s1("prefix(0, 1)(3).suffix").token_span(),
        };

        assert_eq!(code.with_stream(parse_name), prefix_index_3_suffix);
    }

    #[test]
    fn test_function_call() {
        let code = Code::new("foo(arg => 0)");

        let foo = WithTokenSpan {
            item: Name::Designator(Designator::Identifier(code.symbol("foo")).into_ref()),
            span: code.s1("foo").token_span(),
        };

        let arg = WithTokenSpan {
            item: Name::Designator(Designator::Identifier(code.symbol("arg")).into_ref()),
            span: code.s1("arg").token_span(),
        };

        let assoc_elem = AssociationElement {
            formal: Some(arg),
            actual: code.s1("0").expr().map_into(ActualPart::Expression),
        };

        let foo_call = WithTokenSpan {
            item: Name::CallOrIndexed(Box::new(CallOrIndexed {
                name: foo,
                parameters: vec![assoc_elem],
            })),
            span: code.s1("foo(arg => 0)").token_span(),
        };

        assert_eq!(code.with_stream(parse_name), foo_call);
    }

    #[test]
    fn test_association_list_actual_part_open() {
        let code = Code::new("(open, arg => open)");
        let elem1 = AssociationElement {
            formal: None,
            actual: WithTokenSpan::new(ActualPart::Open, code.s1("open").token_span()),
        };
        let elem2 = AssociationElement {
            formal: Some(code.s1("arg").name()),
            actual: WithTokenSpan::new(ActualPart::Open, code.s("open", 2).token_span()),
        };
        assert_eq!(
            code.with_stream_no_diagnostics(parse_association_list),
            (
                SeparatedList {
                    items: vec![elem1, elem2],
                    tokens: vec![code.s1(",").token()]
                },
                code.s1(")").token()
            )
        );
    }

    #[test]
    fn test_external_name_implicit_relative() {
        let code = Code::new("<< signal dut.foo : std_logic >>");
        let external_name = ExternalName {
            class: ExternalObjectClass::Signal,
            path: WithTokenSpan::new(
                ExternalPath::Relative(code.s1("dut.foo").name(), 0),
                code.s1("dut.foo").token_span(),
            ),
            subtype: code.s1("std_logic").subtype_indication(),
        };
        assert_eq!(
            code.with_stream(parse_name),
            WithTokenSpan::new(Name::External(Box::new(external_name)), code.token_span())
        );
    }

    #[test]
    fn test_external_name_explicit_relative() {
        let code = Code::new("<< signal ^.dut.gen(0) : std_logic >>");
        let external_name = ExternalName {
            class: ExternalObjectClass::Signal,
            path: WithTokenSpan::new(
                ExternalPath::Relative(code.s1("dut.gen(0)").name(), 1),
                code.s1("^.dut.gen(0)").token_span(),
            ),
            subtype: code.s1("std_logic").subtype_indication(),
        };
        assert_eq!(
            code.with_stream(parse_name),
            WithTokenSpan::new(Name::External(Box::new(external_name)), code.token_span())
        );
    }

    #[test]
    fn test_external_name_explicit_relative_multiple_levels() {
        let code = Code::new("<< signal ^.^.^.dut.gen(0) : std_logic >>");
        let external_name = ExternalName {
            class: ExternalObjectClass::Signal,
            path: WithTokenSpan::new(
                ExternalPath::Relative(code.s1("dut.gen(0)").name(), 3),
                code.s1("^.^.^.dut.gen(0)").token_span(),
            ),
            subtype: code.s1("std_logic").subtype_indication(),
        };
        assert_eq!(
            code.with_stream(parse_name),
            WithTokenSpan::new(Name::External(Box::new(external_name)), code.token_span())
        );
    }

    #[test]
    fn test_external_name_absolute() {
        let code = Code::new("<< signal .dut.gen(0) : std_logic >>");
        let external_name = ExternalName {
            class: ExternalObjectClass::Signal,
            path: WithTokenSpan::new(
                ExternalPath::Absolute(code.s1("dut.gen(0)").name()),
                code.s1(".dut.gen(0)").token_span(),
            ),
            subtype: code.s1("std_logic").subtype_indication(),
        };
        assert_eq!(
            code.with_stream(parse_name),
            WithTokenSpan::new(Name::External(Box::new(external_name)), code.token_span())
        );
    }

    #[test]
    fn test_external_name_package() {
        let code = Code::new("<< signal @lib.pkg : std_logic >>");
        let external_name = ExternalName {
            class: ExternalObjectClass::Signal,
            path: WithTokenSpan::new(
                ExternalPath::Package(code.s1("lib.pkg").name()),
                code.s1("@lib.pkg").token_span(),
            ),
            subtype: code.s1("std_logic").subtype_indication(),
        };
        assert_eq!(
            code.with_stream(parse_name),
            WithTokenSpan::new(Name::External(Box::new(external_name)), code.token_span())
        );
    }

    #[test]
    fn test_external_name_object_classes() {
        let combinations = [
            ("constant", ExternalObjectClass::Constant),
            ("signal", ExternalObjectClass::Signal),
            ("variable", ExternalObjectClass::Variable),
        ];
        for (string, class) in combinations.iter().cloned() {
            let code = Code::new(&format!("<< {string} dut.foo : std_logic >>"));
            let external_name = ExternalName {
                class,
                path: WithTokenSpan::new(
                    ExternalPath::Relative(code.s1("dut.foo").name(), 0),
                    code.s1("dut.foo").token_span(),
                ),
                subtype: code.s1("std_logic").subtype_indication(),
            };
            assert_eq!(
                code.with_stream(parse_name),
                WithTokenSpan::new(Name::External(Box::new(external_name)), code.token_span())
            );
        }
    }

    #[test]
    fn test_simple_all_is_illegal() {
        let code = Code::new("all");
        assert_eq!(
            code.with_partial_stream(parse_name),
            Err(Diagnostic::syntax_error(
                code.s1("all"),
                "Illegal prefix 'all' for name"
            ))
        );
    }

    #[test]
    fn test_all_is_illegal_prefix() {
        let code = Code::new("all.foo");
        assert_eq!(
            code.with_partial_stream(parse_name),
            Err(Diagnostic::syntax_error(
                code.s1("all"),
                "Illegal prefix 'all' for name"
            ))
        );
    }

    #[test]
    fn empty_association_list_diagnostic() {
        let code = Code::new("()");
        let (list, diag) = code.with_stream_diagnostics(parse_association_list);
        assert_eq!(
            diag,
            vec![Diagnostic::syntax_error(
                code.pos(),
                "Association list cannot be empty"
            )]
        );
        assert!(list.0.tokens.is_empty());
        assert!(list.0.items.is_empty());
    }

    #[test]
    fn empty_association_list_in_name_diagnostic() {
        let code = Code::new("foo()");
        let res = code.parse(parse_name);
        assert_eq!(
            res.0,
            Err(Diagnostic::syntax_error(
                code.s1("()"),
                "Association list cannot be empty"
            ))
        );
    }

    #[test]
    fn trailing_comma_diagnostic() {
        let code = Code::new("(a => b,)");
        let (list, diag) = code.with_stream_diagnostics(parse_association_list);
        assert_eq!(
            diag,
            vec![Diagnostic::syntax_error(
                code.s1(")").pos(),
                "Expected {expression}"
            )]
        );
        assert_eq!(list.0.items, vec![code.s1("a => b").association_element()]);
    }
}
