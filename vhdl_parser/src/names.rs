// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

/// LRM 8. Names
use crate::ast::{
    to_simple_name, ActualPart, AssociationElement, AttributeName, Designator, Direction,
    DiscreteRange, Expression, ExternalName, ExternalObjectClass, ExternalPath, FunctionCall,
    Ident, Literal, Name, Range, RangeConstraint, SelectedName, Signature,
};
use crate::diagnostic::{Diagnostic, ParseResult};
use crate::expression::{parse_expression, parse_expression_initial_token};
use crate::source::WithPos;
use crate::subprogram::parse_signature;
use crate::subtype_indication::parse_subtype_indication;
use crate::tokenstream::TokenStream;

use crate::tokenizer::{Kind::*, Token};

pub fn parse_designator(stream: &mut TokenStream) -> ParseResult<WithPos<Designator>> {
    let token = stream.expect()?;
    Ok(try_token_kind!(
        token,
        Identifier => token.expect_ident()?.map_into(Designator::Identifier),
        StringLiteral => token.expect_string()?.map_into(Designator::OperatorSymbol),
        Character => token.expect_character()?.map_into(Designator::Character)
    ))
}

pub fn parse_selected_name(stream: &mut TokenStream) -> ParseResult<WithPos<SelectedName>> {
    let mut name = parse_designator(stream)?
        .into_ref()
        .map_into(SelectedName::Designator);
    loop {
        if !stream.skip_if_kind(Dot)? {
            break;
        }
        let suffix = parse_designator(stream)?.into_ref();
        let pos = suffix.pos.combine(&name.pos);
        name = WithPos::from(SelectedName::Selected(Box::new(name), suffix), pos);
    }
    Ok(name)
}

pub fn into_selected_name(name: WithPos<Name>) -> ParseResult<WithPos<SelectedName>> {
    match name.item {
        Name::Selected(prefix, suffix) => {
            let pos = suffix.pos.combine(&prefix.pos);
            Ok(WithPos::from(
                SelectedName::Selected(Box::new(into_selected_name(*prefix.clone())?), suffix),
                pos,
            ))
        }
        Name::Designator(designator) => Ok(WithPos::from(
            SelectedName::Designator(designator),
            name.pos,
        )),
        _ => Err(Diagnostic::error(&name, "Expected selected name")),
    }
}

pub fn expression_to_ident(name: WithPos<Expression>) -> ParseResult<Ident> {
    let name = expression_to_name(name)?;
    to_simple_name(name)
}

pub fn parse_identifier_list(stream: &mut TokenStream) -> ParseResult<Vec<Ident>> {
    let mut idents = Vec::new();
    loop {
        idents.push(stream.expect_ident()?);
        if let Some(token) = stream.peek()? {
            if token.kind == Comma {
                stream.move_after(&token);
                continue;
            }
        }
        break;
    }
    Ok(idents)
}

fn expression_to_name(expr: WithPos<Expression>) -> ParseResult<WithPos<Name>> {
    match expr.item {
        Expression::Name(name) => Ok(WithPos {
            item: *name,
            pos: expr.pos,
        }),
        Expression::Literal(Literal::String(val)) => Ok(WithPos {
            item: Name::Designator(Designator::OperatorSymbol(val).into_ref()),
            pos: expr.pos,
        }),
        Expression::Literal(Literal::Character(val)) => Ok(WithPos {
            item: Name::Designator(Designator::Character(val).into_ref()),
            pos: expr.pos,
        }),
        _ => Err(Diagnostic::error(&expr, "Expected name")),
    }
}

fn actual_to_expression(actual: WithPos<ActualPart>) -> ParseResult<WithPos<Expression>> {
    match actual.item {
        ActualPart::Expression(expr) => Ok(WithPos::from(expr, actual.pos)),
        _ => Err(Diagnostic::error(&actual, "Expected expression")),
    }
}

fn actual_part_to_name(actual: WithPos<ActualPart>) -> ParseResult<WithPos<Name>> {
    match actual.item {
        ActualPart::Expression(expr) => expression_to_name(WithPos::from(expr, actual.pos)),
        _ => Err(Diagnostic::error(&actual, "Expected name")),
    }
}

fn assoc_to_expression(assoc: AssociationElement) -> ParseResult<WithPos<Expression>> {
    match assoc.formal {
        Some(name) => Err(Diagnostic::error(&name, "Expected expression")),
        None => actual_to_expression(assoc.actual),
    }
}

fn parse_actual_part(stream: &mut TokenStream) -> ParseResult<WithPos<ActualPart>> {
    let token = stream.expect()?;
    if token.kind == Open {
        Ok(WithPos::from(ActualPart::Open, token))
    } else {
        Ok(parse_expression_initial_token(stream, token)?.map_into(ActualPart::Expression))
    }
}

fn parse_association_element(stream: &mut TokenStream) -> ParseResult<AssociationElement> {
    let actual = parse_actual_part(stream)?;
    if stream.skip_if_kind(RightArrow)? {
        Ok(AssociationElement {
            formal: Some(actual_part_to_name(actual)?),
            actual: parse_actual_part(stream)?,
        })
    } else {
        Ok(AssociationElement {
            formal: None,
            actual,
        })
    }
}

pub fn parse_association_list(stream: &mut TokenStream) -> ParseResult<Vec<AssociationElement>> {
    stream.expect_kind(LeftPar)?;
    parse_association_list_no_leftpar(stream)
}

pub fn parse_association_list_no_leftpar(
    stream: &mut TokenStream,
) -> ParseResult<Vec<AssociationElement>> {
    let mut association_elements = Vec::with_capacity(1);
    loop {
        association_elements.push(parse_association_element(stream)?);
        let token = stream.expect()?;
        try_token_kind!(
            token,
            Comma => {},
            RightPar => {
                return Ok(association_elements);
            }
        )
    }
}

fn parse_function_call(
    stream: &mut TokenStream,
    prefix: WithPos<Name>,
    first: AssociationElement,
) -> ParseResult<WithPos<Name>> {
    let mut association_elements = Vec::new();
    association_elements.push(first);

    loop {
        association_elements.push(parse_association_element(stream)?);
        let token = stream.expect()?;
        try_token_kind!(
            token,
            Comma => {},
            RightPar => {
                let pos = token.pos.combine_into(&prefix);
                return Ok(WithPos {
                    item: Name::FunctionCall(Box::new(FunctionCall {
                        name: prefix,
                        parameters: association_elements})),
                    pos: pos,
                });
            }
        )
    }
}

fn parse_attribute_name(
    stream: &mut TokenStream,
    name: WithPos<Name>,
    signature: Option<Signature>,
) -> ParseResult<WithPos<Name>> {
    let attr = stream.expect_ident_or_range()?;

    let (expression, pos) = {
        if stream.skip_if_kind(LeftPar)? {
            let ret = Some(parse_expression(stream)?);
            let rpar_token = stream.expect_kind(RightPar)?;
            (ret, rpar_token.pos.combine_into(&name))
        } else {
            (None, attr.pos.combine(&name))
        }
    };

    Ok(WithPos {
        item: Name::Attribute(Box::new(AttributeName {
            name,
            attr,
            signature,
            expr: expression.map(Box::new),
        })),
        pos,
    })
}

enum DesignatorOrAll {
    Designator(Designator),
    All,
}

fn to_suffix(token: Token) -> ParseResult<WithPos<DesignatorOrAll>> {
    let name = {
        try_token_kind!(
            token,
            Identifier => token.expect_ident()?.map_into(|ident| DesignatorOrAll::Designator(Designator::Identifier(ident))),
            Character => token.expect_character()?.map_into(|byte| DesignatorOrAll::Designator(Designator::Character(byte))),
            StringLiteral => token.expect_string()?.map_into(|string| DesignatorOrAll::Designator(Designator::OperatorSymbol(string))),
            All => WithPos::from(DesignatorOrAll::All, token)
        )
    };

    Ok(name)
}

/// LRM 8.7 External names
/// Inside of << >>
fn parse_inner_external_name(stream: &mut TokenStream) -> ParseResult<ExternalName> {
    let token = stream.expect()?;
    let class = try_token_kind!(
        token,
        Signal => ExternalObjectClass::Signal,
        Constant => ExternalObjectClass::Constant,
        Variable => ExternalObjectClass::Variable);

    let token = stream.expect()?;
    let path = try_token_kind!(
        token,
        CommAt => {
            let path_name = parse_name(stream)?;
            let path_pos = path_name.pos.clone().combine_into(&token);
            WithPos::from(ExternalPath::Package(path_name), path_pos)
        },
        Dot => {
            let path_name = parse_name(stream)?;
            let path_pos = path_name.pos.clone().combine_into(&token);
            WithPos::from(ExternalPath::Absolute(path_name), path_pos)
        },
        Circ => {
            stream.expect_kind(Dot)?;
            let path_name = parse_name(stream)?;
            let path_pos = path_name.pos.clone().combine_into(&token);
            WithPos::from(ExternalPath::Relative(path_name), path_pos)
        },
        Identifier => {
            let path_name = parse_name_initial_token(stream, token)?;
            let path_pos = path_name.pos.clone();
            WithPos::from(ExternalPath::Relative(path_name), path_pos)
        }
    );

    stream.expect_kind(Colon)?;
    let subtype = parse_subtype_indication(stream)?;

    Ok(ExternalName {
        class,
        path,
        subtype,
    })
}

/// LRM 8. Names
pub fn parse_name_initial_token(
    stream: &mut TokenStream,
    token: Token,
) -> ParseResult<WithPos<Name>> {
    let mut name = {
        if token.kind == LtLt {
            stream.move_after(&token);
            let external_name = Name::External(Box::new(parse_inner_external_name(stream)?));
            let end_token = stream.expect_kind(GtGt)?;
            WithPos::from(external_name, token.pos.combine_into(&end_token))
        } else {
            let suffix = to_suffix(token)?;
            match suffix.item {
                DesignatorOrAll::Designator(designator) => {
                    WithPos::from(Name::Designator(designator.into_ref()), suffix.pos)
                }
                DesignatorOrAll::All => {
                    return Err(Diagnostic::error(
                        suffix.pos,
                        "Illegal prefix 'all' for name",
                    ));
                }
            }
        }
    };

    while let Some(token) = stream.peek()? {
        match token.kind {
            Dot => {
                stream.move_after(&token);
                let suffix_token = stream.expect()?;
                let pos = name.pos.combine(&suffix_token);
                let suffix = to_suffix(suffix_token)?;

                match suffix.item {
                    DesignatorOrAll::Designator(designator) => {
                        name = WithPos {
                            item: Name::Selected(
                                Box::new(name),
                                WithPos::from(designator.into_ref(), suffix.pos),
                            ),
                            pos,
                        }
                    }
                    DesignatorOrAll::All => {
                        name = WithPos {
                            item: Name::SelectedAll(Box::new(name)),
                            pos,
                        }
                    }
                }
            }
            LeftSquare => {
                let state = stream.state();
                let signature = Some(parse_signature(stream)?);
                if !stream.skip_if_kind(Tick)? {
                    // Alias may have prefix[signature] without tick
                    stream.set_state(state);
                    break;
                }
                name = parse_attribute_name(stream, name, signature)?;
            }
            Tick => {
                if stream.next_kinds_are(&[Tick, LeftPar])? {
                    break;
                }
                stream.move_after(&token);
                let signature = None;
                name = parse_attribute_name(stream, name, signature)?;
            }
            LeftPar => {
                stream.move_after(&token);
                let assoc = parse_association_element(stream)?;
                let sep_token = stream.expect()?;
                try_token_kind!(
                    sep_token,
                    Comma => {
                        name = parse_function_call(stream, name, assoc)?;
                    },
                    To | Downto => {
                        let right_expr = parse_expression(stream)?;
                        let direction = {
                            if sep_token.kind == To {
                                Direction::Ascending
                            } else {
                                Direction::Descending
                            }
                        };
                        let rpar_token = stream.expect_kind(RightPar)?;
                        let pos = rpar_token.pos.combine_into(&name);
                        let discrete_range =
                            DiscreteRange::Range(Range::Range(RangeConstraint {
                                left_expr: Box::new(assoc_to_expression(assoc)?),
                                direction,
                                right_expr: Box::new(right_expr),
                            }));

                        name = WithPos {
                            item: Name::Slice(Box::new(name), discrete_range),
                            pos: pos,
                        };
                    },
                    RightPar => {
                        let pos = sep_token.pos.combine_into(&name);
                        name = WithPos {
                            item: Name::FunctionCall(Box::new(
                                FunctionCall {
                                    name: name,
                                    parameters: vec![assoc]
                                })),
                            pos: pos,
                        };
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

pub fn parse_name(stream: &mut TokenStream) -> ParseResult<WithPos<Name>> {
    let initial_token = stream.expect()?;
    parse_name_initial_token(stream, initial_token)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::latin_1::Latin1String;
    use crate::test_util::Code;

    #[test]
    fn test_parse_selected_name_single() {
        let code = Code::new("foo");
        assert_eq!(
            code.with_stream(parse_selected_name),
            code.s1("foo")
                .ident()
                .map_into(|sym| SelectedName::Designator(Designator::Identifier(sym).into_ref()))
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
            .map_into(SelectedName::Designator);
        let foo_bar = WithPos::from(
            SelectedName::Selected(Box::new(foo), bar),
            code.s1("foo.bar").pos(),
        );
        let foo_bar_baz = WithPos::from(
            SelectedName::Selected(Box::new(foo_bar), baz),
            code.s1("foo.bar.baz").pos(),
        );

        assert_eq!(code.with_stream(parse_selected_name), foo_bar_baz);
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
            WithPos {
                item: Name::Designator(Designator::Identifier(code.symbol("foo")).into_ref()),
                pos: code.s1("foo").pos()
            }
        );
    }

    #[test]
    fn test_characer_name() {
        let code = Code::new("'a'");
        assert_eq!(
            code.with_stream(parse_name),
            WithPos {
                item: Name::Designator(Designator::Character(b'a').into_ref()),
                pos: code.s1("'a'").pos()
            }
        );
    }

    #[test]
    fn test_operator_symbol() {
        let code = Code::new("\"+\"");
        assert_eq!(
            code.with_stream(parse_name),
            WithPos {
                item: Name::Designator(
                    Designator::OperatorSymbol(Latin1String::from_utf8_unchecked("+")).into_ref()
                ),
                pos: code.s1("\"+\"").pos()
            }
        );
    }

    #[test]
    fn test_selected_name() {
        let code = Code::new("foo.bar.baz");

        let foo = WithPos {
            item: Name::Designator(Designator::Identifier(code.symbol("foo")).into_ref()),
            pos: code.s1("foo").pos(),
        };

        let bar = WithPos {
            item: Designator::Identifier(code.symbol("bar")),
            pos: code.s1("bar").pos(),
        };

        let baz = WithPos {
            item: Designator::Identifier(code.symbol("baz")),
            pos: code.s1("baz").pos(),
        };

        let foo_bar = WithPos {
            item: Name::Selected(Box::new(foo), bar.into_ref()),
            pos: code.s1("foo.bar").pos(),
        };

        let foo_bar_baz = WithPos {
            item: Name::Selected(Box::new(foo_bar), baz.into_ref()),
            pos: code.s1("foo.bar.baz").pos(),
        };

        assert_eq!(code.with_stream(parse_name), foo_bar_baz);
    }

    #[test]
    fn test_selected_name_all() {
        let code = Code::new("foo.all");

        let foo = WithPos {
            item: Name::Designator(Designator::Identifier(code.symbol("foo")).into_ref()),
            pos: code.s1("foo").pos(),
        };

        let foo_all = WithPos {
            item: Name::SelectedAll(Box::new(foo)),
            pos: code.s1("foo.all").pos(),
        };

        assert_eq!(code.with_stream(parse_name), foo_all);
    }

    #[test]
    fn test_slice_name_range_to() {
        let code = Code::new("prefix(0 to 3)");
        let prefix = WithPos {
            item: Name::Designator(Designator::Identifier(code.symbol("prefix")).into_ref()),
            pos: code.s1("prefix").pos(),
        };
        let slice = WithPos {
            item: Name::Slice(Box::new(prefix), code.s1("0 to 3").discrete_range()),
            pos: code.s1("prefix(0 to 3)").pos(),
        };
        assert_eq!(code.with_stream(parse_name), slice);
    }

    #[test]
    fn test_slice_name_range_downto() {
        let code = Code::new("prefix(3 downto 0)");
        let prefix = WithPos {
            item: Name::Designator(Designator::Identifier(code.symbol("prefix")).into_ref()),
            pos: code.s1("prefix").pos(),
        };
        let slice = WithPos {
            item: Name::Slice(Box::new(prefix), code.s1("3 downto 0").discrete_range()),
            pos: code.s1("prefix(3 downto 0)").pos(),
        };
        assert_eq!(code.with_stream(parse_name), slice);
    }

    #[test]
    fn test_attribute_name() {
        let code = Code::new("prefix'foo");
        let prefix = WithPos {
            item: Name::Designator(Designator::Identifier(code.symbol("prefix")).into_ref()),
            pos: code.s1("prefix").pos(),
        };
        let attr = WithPos {
            item: Name::Attribute(Box::new(AttributeName {
                name: prefix,
                attr: code.s1("foo").ident(),
                signature: None,
                expr: None,
            })),
            pos: code.s1("prefix'foo").pos(),
        };
        assert_eq!(code.with_stream(parse_name), attr);
    }

    #[test]
    fn test_attribute_name_range() {
        let code = Code::new("prefix'range");
        let prefix = WithPos {
            item: Name::Designator(Designator::Identifier(code.symbol("prefix")).into_ref()),
            pos: code.s1("prefix").pos(),
        };
        let attr = WithPos {
            item: Name::Attribute(Box::new(AttributeName {
                name: prefix,
                attr: WithPos {
                    item: code.symbol("range"),
                    pos: code.s1("range").pos(),
                },
                signature: None,
                expr: None,
            })),
            pos: code.s1("prefix'range").pos(),
        };
        assert_eq!(code.with_stream(parse_name), attr);
    }

    #[test]
    fn test_attribute_name_expression() {
        let code = Code::new("prefix'foo(expr+1)");
        let prefix = WithPos {
            item: Name::Designator(Designator::Identifier(code.symbol("prefix")).into_ref()),
            pos: code.s1("prefix").pos(),
        };
        let attr = WithPos {
            item: Name::Attribute(Box::new(AttributeName {
                name: prefix,
                attr: code.s1("foo").ident(),
                signature: None,
                expr: Some(Box::new(code.s1("expr+1").expr())),
            })),
            pos: code.s1("prefix'foo(expr+1)").pos(),
        };
        assert_eq!(code.with_stream(parse_name), attr);
    }

    #[test]
    fn test_attribute_name_signature_expression() {
        let code = Code::new("prefix[return natural]'foo(expr+1)");
        let prefix = WithPos {
            item: Name::Designator(Designator::Identifier(code.symbol("prefix")).into_ref()),
            pos: code.s1("prefix").pos(),
        };
        let attr = WithPos {
            item: Name::Attribute(Box::new(AttributeName {
                name: prefix,
                attr: code.s1("foo").ident(),
                signature: Some(code.s1("[return natural]").signature()),
                expr: Some(Box::new(code.s1("expr+1").expr())),
            })),
            pos: code.s1("prefix[return natural]'foo(expr+1)").pos(),
        };
        assert_eq!(code.with_stream(parse_name), attr);
    }

    #[test]
    fn test_name_signature_no_attribute_name() {
        // Alias declarations may use name[signature]
        let code = Code::new("prefix[return natural]");
        let name = code.with_partial_stream(|stream| {
            let result = parse_name(stream);
            stream.expect_kind(LeftSquare)?;
            result
        });
        let prefix = WithPos {
            item: Name::Designator(Designator::Identifier(code.symbol("prefix")).into_ref()),
            pos: code.s1("prefix").pos(),
        };
        assert_eq!(name, Ok(prefix));
    }

    #[test]
    fn test_qualified_expression_is_not_name() {
        let code = Code::new("prefix'(");
        let name = code.with_stream(|stream| {
            let result = parse_name(stream);
            stream.expect_kind(Tick)?;
            stream.expect_kind(LeftPar)?;
            result
        });
        let prefix = WithPos {
            item: Name::Designator(Designator::Identifier(code.symbol("prefix")).into_ref()),
            pos: code.s1("prefix").pos(),
        };
        assert_eq!(name, prefix);
    }

    #[test]
    fn test_function_call_no_formal() {
        let code = Code::new("foo(0)");

        let foo = WithPos {
            item: Name::Designator(Designator::Identifier(code.symbol("foo")).into_ref()),
            pos: code.s1("foo").pos(),
        };

        let foo_0 = WithPos {
            item: Name::FunctionCall(Box::new(FunctionCall {
                name: foo,
                parameters: vec![AssociationElement {
                    formal: None,
                    actual: code.s1("0").expr().map_into(ActualPart::Expression),
                }],
            })),
            pos: code.s1("foo(0)").pos(),
        };

        assert_eq!(code.with_stream(parse_name), foo_0);
    }

    #[test]
    fn test_function_call_many() {
        let code = Code::new("prefix(0, 1)(3).suffix");

        let prefix = WithPos {
            item: Name::Designator(Designator::Identifier(code.symbol("prefix")).into_ref()),
            pos: code.s1("prefix").pos(),
        };

        let prefix_index = WithPos {
            item: Name::FunctionCall(Box::new(FunctionCall {
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
            pos: code.s1("prefix(0, 1)").pos(),
        };

        let prefix_index_3 = WithPos {
            item: Name::FunctionCall(Box::new(FunctionCall {
                name: prefix_index,
                parameters: vec![AssociationElement {
                    formal: None,
                    actual: code.s1("3").expr().map_into(ActualPart::Expression),
                }],
            })),
            pos: code.s1("prefix(0, 1)(3)").pos(),
        };

        let suffix = WithPos {
            item: Designator::Identifier(code.symbol("suffix")),
            pos: code.s1("suffix").pos(),
        };

        let prefix_index_3_suffix = WithPos {
            item: Name::Selected(Box::new(prefix_index_3), suffix.into_ref()),
            pos: code.s1("prefix(0, 1)(3).suffix").pos(),
        };

        assert_eq!(code.with_stream(parse_name), prefix_index_3_suffix);
    }

    #[test]
    fn test_function_call() {
        let code = Code::new("foo(arg => 0)");

        let foo = WithPos {
            item: Name::Designator(Designator::Identifier(code.symbol("foo")).into_ref()),
            pos: code.s1("foo").pos(),
        };

        let arg = WithPos {
            item: Name::Designator(Designator::Identifier(code.symbol("arg")).into_ref()),
            pos: code.s1("arg").pos(),
        };

        let assoc_elem = AssociationElement {
            formal: Some(arg),
            actual: code.s1("0").expr().map_into(ActualPart::Expression),
        };

        let foo_call = WithPos {
            item: Name::FunctionCall(Box::new(FunctionCall {
                name: foo,
                parameters: vec![assoc_elem],
            })),
            pos: code.s1("foo(arg => 0)").pos(),
        };

        assert_eq!(code.with_stream(parse_name), foo_call);
    }

    #[test]
    fn test_association_list_actual_part_open() {
        let code = Code::new("(open, arg => open)");
        let elem1 = AssociationElement {
            formal: None,
            actual: WithPos::new(ActualPart::Open, code.s1("open").pos()),
        };
        let elem2 = AssociationElement {
            formal: Some(code.s1("arg").name()),
            actual: WithPos::new(ActualPart::Open, code.s("open", 2)),
        };
        assert_eq!(code.with_stream(parse_association_list), vec![elem1, elem2]);
    }

    #[test]
    fn test_external_name_implicit_relative() {
        let code = Code::new("<< signal dut.foo : std_logic >>");
        let external_name = ExternalName {
            class: ExternalObjectClass::Signal,
            path: WithPos::new(
                ExternalPath::Relative(code.s1("dut.foo").name()),
                code.s1("dut.foo").pos(),
            ),
            subtype: code.s1("std_logic").subtype_indication(),
        };
        assert_eq!(
            code.with_stream(parse_name),
            WithPos::new(Name::External(Box::new(external_name)), code)
        );
    }

    #[test]
    fn test_external_name_explicit_relative() {
        let code = Code::new("<< signal ^.dut.gen(0) : std_logic >>");
        let external_name = ExternalName {
            class: ExternalObjectClass::Signal,
            path: WithPos::new(
                ExternalPath::Relative(code.s1("dut.gen(0)").name()),
                code.s1("^.dut.gen(0)").pos(),
            ),
            subtype: code.s1("std_logic").subtype_indication(),
        };
        assert_eq!(
            code.with_stream(parse_name),
            WithPos::new(Name::External(Box::new(external_name)), code)
        );
    }

    #[test]
    fn test_external_name_absolute() {
        let code = Code::new("<< signal .dut.gen(0) : std_logic >>");
        let external_name = ExternalName {
            class: ExternalObjectClass::Signal,
            path: WithPos::new(
                ExternalPath::Absolute(code.s1("dut.gen(0)").name()),
                code.s1(".dut.gen(0)").pos(),
            ),
            subtype: code.s1("std_logic").subtype_indication(),
        };
        assert_eq!(
            code.with_stream(parse_name),
            WithPos::new(Name::External(Box::new(external_name)), code)
        );
    }

    #[test]
    fn test_external_name_package() {
        let code = Code::new("<< signal @lib.pkg : std_logic >>");
        let external_name = ExternalName {
            class: ExternalObjectClass::Signal,
            path: WithPos::new(
                ExternalPath::Package(code.s1("lib.pkg").name()),
                code.s1("@lib.pkg").pos(),
            ),
            subtype: code.s1("std_logic").subtype_indication(),
        };
        assert_eq!(
            code.with_stream(parse_name),
            WithPos::new(Name::External(Box::new(external_name)), code)
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
            let code = Code::new(&format!("<< {} dut.foo : std_logic >>", string));
            let external_name = ExternalName {
                class,
                path: WithPos::new(
                    ExternalPath::Relative(code.s1("dut.foo").name()),
                    code.s1("dut.foo").pos(),
                ),
                subtype: code.s1("std_logic").subtype_indication(),
            };
            assert_eq!(
                code.with_stream(parse_name),
                WithPos::new(Name::External(Box::new(external_name)), code)
            );
        }
    }

    #[test]
    fn test_simple_all_is_illegal() {
        let code = Code::new("all");
        assert_eq!(
            code.with_partial_stream(parse_name),
            Err(Diagnostic::error(
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
            Err(Diagnostic::error(
                code.s1("all"),
                "Illegal prefix 'all' for name"
            ))
        );
    }
}
