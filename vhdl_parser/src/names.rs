// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

/// LRM 8. Names
use ast::{
    ActualPart, AssociationElement, AttributeName, Direction, DiscreteRange, Expression,
    ExternalName, ExternalObjectClass, ExternalPath, FunctionCall, Ident, Literal, Name, Range,
    RangeConstraint, SelectedName, Signature,
};
use expression::{parse_expression, parse_expression_initial_token};
use message::{error, ParseResult};
use source::WithPos;
use subprogram::parse_signature;
use subtype_indication::parse_subtype_indication;
use tokenstream::TokenStream;

use tokenizer::{Kind::*, Token};

pub fn parse_selected_name(stream: &mut TokenStream) -> ParseResult<SelectedName> {
    let mut result = Vec::new();
    result.push(stream.expect_ident()?);
    loop {
        if None == stream.pop_if_kind(Dot)? {
            break;
        }
        result.push(stream.expect_ident()?);
    }
    Ok(result)
}

pub fn to_selected_name(name: &WithPos<Name>) -> ParseResult<SelectedName> {
    match name.item {
        Name::Selected(ref prefix, ref suffix) => match suffix.item {
            Name::Simple(ref ident) => {
                let mut selected = to_selected_name(&*prefix)?;
                selected.push(WithPos {
                    item: ident.clone(),
                    pos: suffix.pos.clone(),
                });
                Ok(selected)
            }
            _ => Err(error(suffix.as_ref(), "Expected simple name")),
        },
        Name::Simple(ref ident) => Ok(vec![WithPos {
            item: ident.clone(),
            pos: name.pos.clone(),
        }]),
        _ => Err(error(&name, "Expected selected name")),
    }
}

pub fn expression_to_ident(name: WithPos<Expression>) -> ParseResult<Ident> {
    let name = expression_to_name(name)?;
    to_simple_name(name)
}

pub fn to_simple_name(name: WithPos<Name>) -> ParseResult<Ident> {
    match name.item {
        Name::Simple(ident) => Ok(WithPos {
            item: ident,
            pos: name.pos,
        }),
        _ => Err(error(&name, "Expected selected name")),
    }
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
            item: Name::OperatorSymbol(val),
            pos: expr.pos,
        }),
        Expression::Literal(Literal::Character(val)) => Ok(WithPos {
            item: Name::CharacterLiteral(val),
            pos: expr.pos,
        }),
        _ => {
            return Err(error(&expr, "Expected name"));
        }
    }
}

fn actual_to_expression(actual: WithPos<ActualPart>) -> ParseResult<WithPos<Expression>> {
    match actual.item {
        ActualPart::Expression(expr) => Ok(WithPos {
            item: expr,
            pos: actual.pos,
        }),
        _ => Err(error(&actual, "Expected expression")),
    }
}

fn actual_part_to_name(actual: WithPos<ActualPart>) -> ParseResult<WithPos<Name>> {
    match actual.item {
        ActualPart::Expression(expr) => expression_to_name(WithPos::new(expr, actual.pos)),
        _ => Err(error(&actual, "Expected name")),
    }
}

fn assoc_to_expression(assoc: AssociationElement) -> ParseResult<WithPos<Expression>> {
    match assoc.formal {
        Some(name) => Err(error(&name, "Expected expression")),
        None => actual_to_expression(assoc.actual),
    }
}

fn parse_actual_part(stream: &mut TokenStream) -> ParseResult<WithPos<ActualPart>> {
    let token = stream.expect()?;
    if token.kind == Open {
        Ok(WithPos::new(ActualPart::Open, token))
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
    let mut association_elements = Vec::with_capacity(1);
    stream.expect_kind(LeftPar)?;
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
                let pos = prefix.pos.combine(&token.pos);
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
            (ret, name.pos.combine(&rpar_token.pos))
        } else {
            (None, name.pos.combine(&attr.pos))
        }
    };

    Ok(WithPos {
        item: Name::Attribute(Box::new(AttributeName {
            name: name,
            attr: attr,
            signature: signature,
            expr: expression.map(Box::new),
        })),
        pos: pos,
    })
}

fn to_suffix(token: Token) -> ParseResult<WithPos<Name>> {
    let name = {
        try_token_kind!(
            token,
            Identifier => WithPos {
                item: Name::Simple(token.expect_identifier()?),
                pos: token.pos,
            },
            Character => WithPos {
                item: Name::CharacterLiteral(token.expect_character()?),
                pos: token.pos,
            },
            StringLiteral => WithPos {
                item: Name::OperatorSymbol(token.expect_string()?),
                pos: token.pos,
            },
            All => WithPos {
                item: Name::All,
                pos: token.pos,
            }
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
            let path_pos = path_name.pos.clone().combine(&token.pos);
            WithPos::new(ExternalPath::Package(path_name), path_pos)
        },
        Dot => {
            let path_name = parse_name(stream)?;
            let path_pos = path_name.pos.clone().combine(&token.pos);
            WithPos::new(ExternalPath::Absolute(path_name), path_pos)
        },
        Circ => {
            stream.expect_kind(Dot)?;
            let path_name = parse_name(stream)?;
            let path_pos = path_name.pos.clone().combine(&token.pos);
            WithPos::new(ExternalPath::Relative(path_name), path_pos)
        },
        Identifier => {
            let path_name = parse_name_initial_token(stream, token)?;
            let path_pos = path_name.pos.clone();
            WithPos::new(ExternalPath::Relative(path_name), path_pos)
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
            WithPos::new(external_name, token.pos.combine(&end_token.pos))
        } else {
            to_suffix(token)?
        }
    };

    loop {
        if let Some(token) = stream.peek()? {
            match token.kind {
                Dot => {
                    stream.move_after(&token);
                    let suffix_token = stream.expect()?;
                    let pos = name.pos.combine(&suffix_token.pos);
                    name = WithPos {
                        item: Name::Selected(Box::new(name), Box::new(to_suffix(suffix_token)?)),
                        pos,
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
                    if stream.is_peek_kinds(&[Tick, LeftPar])? {
                        break;
                    }
                    stream.move_after(&token);
                    let signature = None;
                    name = parse_attribute_name(stream, name, signature)?;
                }
                LeftPar => {
                    stream.move_after(&token);

                    loop {
                        let assoc = parse_association_element(stream)?;
                        let sep_token = stream.expect()?;
                        try_token_kind!(
                            sep_token,
                            Comma => {
                                name = parse_function_call(stream, name, assoc)?;
                                break;
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
                                let pos = name.pos.combine(&rpar_token.pos);
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
                                break;
                            },
                            RightPar => {
                                let pos = name.pos.combine(&sep_token.pos);
                                name = WithPos {
                                    item: Name::FunctionCall(Box::new(
                                        FunctionCall {
                                            name: name,
                                            parameters: vec![assoc]
                                        })),
                                    pos: pos,
                                };
                                break;
                            }
                        )
                    }
                }
                _ => {
                    break;
                }
            }
        } else {
            break;
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
    use latin_1::Latin1String;
    use test_util::{with_partial_stream, with_stream};

    #[test]
    fn test_parse_selected_name_single() {
        let (util, name) = with_stream(parse_selected_name, "foo");
        assert_eq!(name, vec![util.ident("foo")]);
    }

    #[test]
    fn test_parse_selected_name_multiple() {
        let (util, name) = with_stream(parse_selected_name, "foo.bar.baz");
        assert_eq!(
            name,
            vec![util.ident("foo"), util.ident("bar"), util.ident("baz")]
        );
    }

    #[test]
    fn test_identifier_list() {
        let (util, idents) = with_stream(parse_identifier_list, "foo, bar, baz");
        assert_eq!(
            idents,
            vec![util.ident("foo"), util.ident("bar"), util.ident("baz")]
        );
    }

    #[test]
    fn test_simple_name() {
        let (util, name) = with_stream(parse_name, "foo");
        assert_eq!(
            name,
            WithPos {
                item: Name::Simple(util.symbol("foo")),
                pos: util.first_substr_pos("foo")
            }
        );
    }

    #[test]
    fn test_characer_name() {
        let (util, name) = with_stream(parse_name, "'a'");
        assert_eq!(
            name,
            WithPos {
                item: Name::CharacterLiteral(b'a'),
                pos: util.first_substr_pos("'a'")
            }
        );
    }

    #[test]
    fn test_operator_symbol() {
        let (util, name) = with_stream(parse_name, "\"+\"");
        assert_eq!(
            name,
            WithPos {
                item: Name::OperatorSymbol(Latin1String::from_utf8_unchecked("+")),
                pos: util.first_substr_pos("\"+\"")
            }
        );
    }

    #[test]
    fn test_selected_name() {
        let (util, name) = with_stream(parse_name, "foo.bar.baz");

        let foo = WithPos {
            item: Name::Simple(util.symbol("foo")),
            pos: util.first_substr_pos("foo"),
        };

        let bar = WithPos {
            item: Name::Simple(util.symbol("bar")),
            pos: util.first_substr_pos("bar"),
        };

        let baz = WithPos {
            item: Name::Simple(util.symbol("baz")),
            pos: util.first_substr_pos("baz"),
        };

        let foo_bar = WithPos {
            item: Name::Selected(Box::new(foo), Box::new(bar)),
            pos: util.first_substr_pos("foo.bar"),
        };

        let foo_bar_baz = WithPos {
            item: Name::Selected(Box::new(foo_bar), Box::new(baz)),
            pos: util.first_substr_pos("foo.bar.baz"),
        };

        assert_eq!(name, foo_bar_baz);
    }

    #[test]
    fn test_selected_name_all() {
        let (util, name) = with_stream(parse_name, "foo.all");

        let foo = WithPos {
            item: Name::Simple(util.symbol("foo")),
            pos: util.first_substr_pos("foo"),
        };

        let all = WithPos {
            item: Name::All,
            pos: util.first_substr_pos("all"),
        };

        let foo_all = WithPos {
            item: Name::Selected(Box::new(foo), Box::new(all)),
            pos: util.first_substr_pos("foo.all"),
        };

        assert_eq!(name, foo_all);
    }

    #[test]
    fn test_slice_name_range_to() {
        let (util, name) = with_stream(parse_name, "prefix(0 to 3)");
        let prefix = WithPos {
            item: Name::Simple(util.symbol("prefix")),
            pos: util.first_substr_pos("prefix"),
        };
        let slice = WithPos {
            item: Name::Slice(Box::new(prefix), util.discrete_range("0 to 3")),
            pos: util.first_substr_pos("prefix(0 to 3)"),
        };
        assert_eq!(name, slice);
    }

    #[test]
    fn test_slice_name_range_downto() {
        let (util, name) = with_stream(parse_name, "prefix(3 downto 0)");
        let prefix = WithPos {
            item: Name::Simple(util.symbol("prefix")),
            pos: util.first_substr_pos("prefix"),
        };
        let slice = WithPos {
            item: Name::Slice(Box::new(prefix), util.discrete_range("3 downto 0")),
            pos: util.first_substr_pos("prefix(3 downto 0)"),
        };
        assert_eq!(name, slice);
    }

    #[test]
    fn test_attribute_name() {
        let (util, name) = with_stream(parse_name, "prefix'foo");
        let prefix = WithPos {
            item: Name::Simple(util.symbol("prefix")),
            pos: util.first_substr_pos("prefix"),
        };
        let attr = WithPos {
            item: Name::Attribute(Box::new(AttributeName {
                name: prefix,
                attr: util.ident("foo"),
                signature: None,
                expr: None,
            })),
            pos: util.first_substr_pos("prefix'foo"),
        };
        assert_eq!(name, attr);
    }

    #[test]
    fn test_attribute_name_range() {
        let (util, name) = with_stream(parse_name, "prefix'range");
        let prefix = WithPos {
            item: Name::Simple(util.symbol("prefix")),
            pos: util.first_substr_pos("prefix"),
        };
        let attr = WithPos {
            item: Name::Attribute(Box::new(AttributeName {
                name: prefix,
                attr: WithPos {
                    item: util.symbol("range"),
                    pos: util.first_substr_pos("range"),
                },
                signature: None,
                expr: None,
            })),
            pos: util.first_substr_pos("prefix'range"),
        };
        assert_eq!(name, attr);
    }

    #[test]
    fn test_attribute_name_expression() {
        let (util, name) = with_stream(parse_name, "prefix'foo(expr+1)");
        let prefix = WithPos {
            item: Name::Simple(util.symbol("prefix")),
            pos: util.first_substr_pos("prefix"),
        };
        let attr = WithPos {
            item: Name::Attribute(Box::new(AttributeName {
                name: prefix,
                attr: util.ident("foo"),
                signature: None,
                expr: Some(Box::new(util.expr("expr+1"))),
            })),
            pos: util.first_substr_pos("prefix'foo(expr+1)"),
        };
        assert_eq!(name, attr);
    }

    #[test]
    fn test_attribute_name_signature_expression() {
        let (util, name) = with_stream(parse_name, "prefix[return natural]'foo(expr+1)");
        let prefix = WithPos {
            item: Name::Simple(util.symbol("prefix")),
            pos: util.first_substr_pos("prefix"),
        };
        let attr = WithPos {
            item: Name::Attribute(Box::new(AttributeName {
                name: prefix,
                attr: util.ident("foo"),
                signature: Some(util.signature("[return natural]")),
                expr: Some(Box::new(util.expr("expr+1"))),
            })),
            pos: util.first_substr_pos("prefix[return natural]'foo(expr+1)"),
        };
        assert_eq!(name, attr);
    }

    #[test]
    fn test_name_signature_no_attribute_name() {
        // Alias declarations may use name[signature]
        let (util, name) = with_partial_stream(
            |stream| {
                let result = parse_name(stream);
                stream.expect_kind(LeftSquare)?;
                result
            },
            "prefix[return natural]",
        );
        let prefix = WithPos {
            item: Name::Simple(util.symbol("prefix")),
            pos: util.first_substr_pos("prefix"),
        };
        assert_eq!(name, Ok(prefix));
    }

    #[test]
    fn test_qualified_expression_is_not_name() {
        let (util, name) = with_partial_stream(
            |stream| {
                let result = parse_name(stream);
                stream.expect_kind(Tick)?;
                result
            },
            "prefix'(",
        );
        let prefix = WithPos {
            item: Name::Simple(util.symbol("prefix")),
            pos: util.first_substr_pos("prefix"),
        };
        assert_eq!(name, Ok(prefix));
    }

    #[test]
    fn test_function_call_no_formal() {
        let (util, name) = with_stream(parse_name, "foo(0)");

        let foo = WithPos {
            item: Name::Simple(util.symbol("foo")),
            pos: util.first_substr_pos("foo"),
        };

        let foo_0 = WithPos {
            item: Name::FunctionCall(Box::new(FunctionCall {
                name: foo,
                parameters: vec![AssociationElement {
                    formal: None,
                    actual: util.expr("0").map_into(ActualPart::Expression),
                }],
            })),
            pos: util.first_substr_pos("foo(0)"),
        };

        assert_eq!(name, foo_0);
    }

    #[test]
    fn test_function_call_many() {
        let (util, name) = with_stream(parse_name, "prefix(0, 1)(3).suffix");

        let prefix = WithPos {
            item: Name::Simple(util.symbol("prefix")),
            pos: util.first_substr_pos("prefix"),
        };

        let prefix_index = WithPos {
            item: Name::FunctionCall(Box::new(FunctionCall {
                name: prefix,
                parameters: vec![
                    AssociationElement {
                        formal: None,
                        actual: util.expr("0").map_into(ActualPart::Expression),
                    },
                    AssociationElement {
                        formal: None,
                        actual: util.expr("1").map_into(ActualPart::Expression),
                    },
                ],
            })),
            pos: util.first_substr_pos("prefix(0, 1)"),
        };

        let prefix_index_3 = WithPos {
            item: Name::FunctionCall(Box::new(FunctionCall {
                name: prefix_index,
                parameters: vec![AssociationElement {
                    formal: None,
                    actual: util.expr("3").map_into(ActualPart::Expression),
                }],
            })),
            pos: util.first_substr_pos("prefix(0, 1)(3)"),
        };

        let suffix = WithPos {
            item: Name::Simple(util.symbol("suffix")),
            pos: util.first_substr_pos("suffix"),
        };

        let prefix_index_3_suffix = WithPos {
            item: Name::Selected(Box::new(prefix_index_3), Box::new(suffix)),
            pos: util.first_substr_pos("prefix(0, 1)(3).suffix"),
        };

        assert_eq!(name, prefix_index_3_suffix);
    }

    #[test]
    fn test_function_call() {
        let (util, name) = with_stream(parse_name, "foo(arg => 0)");

        let foo = WithPos {
            item: Name::Simple(util.symbol("foo")),
            pos: util.first_substr_pos("foo"),
        };

        let arg = WithPos {
            item: Name::Simple(util.symbol("arg")),
            pos: util.first_substr_pos("arg"),
        };

        let assoc_elem = AssociationElement {
            formal: Some(arg),
            actual: util.expr("0").map_into(ActualPart::Expression),
        };

        let foo_call = WithPos {
            item: Name::FunctionCall(Box::new(FunctionCall {
                name: foo,
                parameters: vec![assoc_elem],
            })),
            pos: util.first_substr_pos("foo(arg => 0)"),
        };

        assert_eq!(name, foo_call);
    }

    #[test]
    fn test_association_list_actual_part_open() {
        let (util, name) = with_stream(parse_association_list, "(open, arg => open)");
        let elem1 = AssociationElement {
            formal: None,
            actual: WithPos::new(ActualPart::Open, util.first_substr_pos("open")),
        };
        let elem2 = AssociationElement {
            formal: Some(util.name("arg")),
            actual: WithPos::new(ActualPart::Open, util.substr_pos("open", 2)),
        };
        assert_eq!(name, vec![elem1, elem2]);
    }

    #[test]
    fn test_external_name_implicit_relative() {
        let (util, name) = with_stream(parse_name, "<< signal dut.foo : std_logic >>");
        let external_name = ExternalName {
            class: ExternalObjectClass::Signal,
            path: WithPos::new(
                ExternalPath::Relative(util.name("dut.foo")),
                util.first_substr_pos("dut.foo"),
            ),
            subtype: util.subtype_indication("std_logic"),
        };
        assert_eq!(
            name,
            WithPos::new(Name::External(Box::new(external_name)), util.entire_pos())
        );
    }

    #[test]
    fn test_external_name_explicit_relative() {
        let (util, name) = with_stream(parse_name, "<< signal ^.dut.gen(0) : std_logic >>");
        let external_name = ExternalName {
            class: ExternalObjectClass::Signal,
            path: WithPos::new(
                ExternalPath::Relative(util.name("dut.gen(0)")),
                util.first_substr_pos("^.dut.gen(0)"),
            ),
            subtype: util.subtype_indication("std_logic"),
        };
        assert_eq!(
            name,
            WithPos::new(Name::External(Box::new(external_name)), util.entire_pos())
        );
    }

    #[test]
    fn test_external_name_absolute() {
        let (util, name) = with_stream(parse_name, "<< signal .dut.gen(0) : std_logic >>");
        let external_name = ExternalName {
            class: ExternalObjectClass::Signal,
            path: WithPos::new(
                ExternalPath::Absolute(util.name("dut.gen(0)")),
                util.first_substr_pos(".dut.gen(0)"),
            ),
            subtype: util.subtype_indication("std_logic"),
        };
        assert_eq!(
            name,
            WithPos::new(Name::External(Box::new(external_name)), util.entire_pos())
        );
    }

    #[test]
    fn test_external_name_package() {
        let (util, name) = with_stream(parse_name, "<< signal @lib.pkg : std_logic >>");
        let external_name = ExternalName {
            class: ExternalObjectClass::Signal,
            path: WithPos::new(
                ExternalPath::Package(util.name("lib.pkg")),
                util.first_substr_pos("@lib.pkg"),
            ),
            subtype: util.subtype_indication("std_logic"),
        };
        assert_eq!(
            name,
            WithPos::new(Name::External(Box::new(external_name)), util.entire_pos())
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
            let (util, name) =
                with_stream(parse_name, &format!("<< {} dut.foo : std_logic >>", string));
            let external_name = ExternalName {
                class,
                path: WithPos::new(
                    ExternalPath::Relative(util.name("dut.foo")),
                    util.first_substr_pos("dut.foo"),
                ),
                subtype: util.subtype_indication("std_logic"),
            };
            assert_eq!(
                name,
                WithPos::new(Name::External(Box::new(external_name)), util.entire_pos())
            );
        }
    }

}
