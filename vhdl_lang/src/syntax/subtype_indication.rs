// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use super::common::ParseResult;
use super::names::{parse_selected_name, parse_type_mark, parse_type_mark_starting_with_name};
use super::range::{parse_discrete_range, parse_range};
use super::tokens::{kinds_error, Kind::*};
/// LRM 6.3 Subtype declarations
use crate::ast::*;
use crate::data::WithTokenSpan;
use crate::syntax::TokenId;
use crate::TokenSpan;
use vhdl_lang::syntax::parser::ParsingContext;

fn parse_record_element_constraint(ctx: &mut ParsingContext<'_>) -> ParseResult<ElementConstraint> {
    let ident = ctx.stream.expect_ident()?;
    let constraint = Box::new(parse_composite_constraint(ctx)?);
    Ok(ElementConstraint { ident, constraint })
}

fn parse_array_constraint(
    ctx: &mut ParsingContext<'_>,
    leftpar: TokenId,
    // Open is None
    initial: Option<DiscreteRange>,
) -> ParseResult<WithTokenSpan<SubtypeConstraint>> {
    let mut discrete_ranges: Vec<_> = initial.into_iter().collect();

    let mut end_token = loop {
        expect_token!(
            ctx.stream, sep_token, sep_token_id,
            RightPar => break sep_token_id,
            Comma => {}
        );

        discrete_ranges.push(parse_discrete_range(ctx)?);
    };

    // Array element constraint
    let element_constraint = {
        if let Some(elemement_constraint) = parse_subtype_constraint(ctx)? {
            end_token = elemement_constraint.span.end_token;
            Some(Box::new(elemement_constraint))
        } else {
            None
        }
    };

    Ok(WithTokenSpan::from(
        SubtypeConstraint::Array(discrete_ranges, element_constraint),
        TokenSpan::new(leftpar, end_token),
    ))
}

fn parse_composite_constraint(
    ctx: &mut ParsingContext<'_>,
) -> ParseResult<WithTokenSpan<SubtypeConstraint>> {
    // There is no finite lookahead that can differentiate
    // between array and record element constraint
    let leftpar = ctx.stream.expect_kind(LeftPar)?;
    let state = ctx.stream.state();

    let mut initial = {
        if ctx.stream.skip_if_kind(Open) {
            // Array constraint open
            Ok(None)
        } else {
            parse_discrete_range(ctx).map(Some)
        }
    };

    if let Some(token) = ctx.stream.peek() {
        match token.kind {
            RightPar | Comma => {}
            _ => {
                initial = Err(
                    kinds_error(ctx.stream.pos_before(token), &[RightPar, Comma])
                        .when("parsing index constraint"),
                );
            }
        }
    }

    if let Ok(initial) = initial {
        // Array constraint
        parse_array_constraint(ctx, leftpar, initial)
    } else {
        // Record constraint
        ctx.stream.set_state(state);
        let mut constraints = vec![parse_record_element_constraint(ctx)?];

        let rightpar = loop {
            expect_token!(
                ctx.stream,
                sep_token,
                sep_token_id,
                RightPar => break sep_token_id,
                Comma => {}
            );
            constraints.push(parse_record_element_constraint(ctx)?);
        };

        Ok(WithTokenSpan::from(
            SubtypeConstraint::Record(constraints),
            TokenSpan::new(leftpar, rightpar),
        ))
    }
}

pub fn parse_subtype_constraint(
    ctx: &mut ParsingContext<'_>,
) -> ParseResult<Option<WithTokenSpan<SubtypeConstraint>>> {
    if let Some(token) = ctx.stream.peek() {
        let token_id = ctx.stream.get_current_token_id();
        let constraint = match token.kind {
            Range => {
                ctx.stream.skip();
                Some(
                    parse_range(ctx)?
                        .map_into(SubtypeConstraint::Range)
                        .combine_span_with(token_id),
                )
            }
            LeftPar => Some(parse_composite_constraint(ctx)?),
            _ => None,
        };
        Ok(constraint)
    } else {
        Ok(None)
    }
}

pub fn parse_element_resolution_indication(
    ctx: &mut ParsingContext<'_>,
) -> ParseResult<ResolutionIndication> {
    ctx.stream.expect_kind(LeftPar)?;

    let first_ident = ctx.stream.expect_ident()?;

    Ok(peek_token!(
        ctx.stream, token,
        Dot | RightPar => {
            let selected_name = first_ident.map_into_span(|sym| Name::Designator(Designator::Identifier(sym).into_ref()));
            ctx.stream.expect_kind(RightPar)?;
            ResolutionIndication::ArrayElement(selected_name)
        },
        Identifier | LeftPar => {
            // Record

            let mut element_resolutions = Vec::new();
            loop {
                let ident = {
                    if element_resolutions.is_empty() {
                        first_ident.clone()
                    } else {
                        ctx.stream.expect_ident()?
                    }
                };

                let resolution = {
                    if ctx.stream.peek_kind() == Some(LeftPar) {
                        parse_element_resolution_indication(ctx)?
                    } else {
                        ResolutionIndication::FunctionName(parse_selected_name(ctx)?)
                    }
                };

                element_resolutions.push(RecordElementResolution {
                    ident,
                    resolution: Box::new(resolution),
                });

                expect_token!(
                    ctx.stream,
                    token,
                    RightPar => break,
                    Comma => {}
                );

            }

            ResolutionIndication::Record(element_resolutions)
        }
    ))
}

pub fn parse_subtype_indication(ctx: &mut ParsingContext<'_>) -> ParseResult<SubtypeIndication> {
    let (resolution, type_mark) = {
        if ctx.stream.peek_kind() == Some(LeftPar) {
            let resolution = parse_element_resolution_indication(ctx)?;
            let type_mark = parse_type_mark(ctx)?;
            (resolution, type_mark)
        } else {
            let selected_name = parse_selected_name(ctx)?;
            match ctx.stream.peek_kind() {
                Some(Identifier) => (
                    ResolutionIndication::FunctionName(selected_name),
                    parse_type_mark(ctx)?,
                ),
                _ => (
                    ResolutionIndication::Unresolved,
                    parse_type_mark_starting_with_name(ctx, selected_name)?,
                ),
            }
        }
    };

    let constraint = parse_subtype_constraint(ctx)?;

    Ok(SubtypeIndication {
        resolution,
        type_mark,
        constraint,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::syntax::test::Code;

    #[test]
    fn parse_subtype_indication_without_constraint() {
        let code = Code::new("std_logic");
        assert_eq!(
            code.with_stream(parse_subtype_indication),
            SubtypeIndication {
                resolution: ResolutionIndication::Unresolved,
                type_mark: code.s1("std_logic").type_mark(),
                constraint: None
            }
        );
    }

    #[test]
    fn parse_subtype_indication_with_resolution_function() {
        let code = Code::new("resolve std_logic");
        assert_eq!(
            code.with_stream(parse_subtype_indication),
            SubtypeIndication {
                resolution: ResolutionIndication::FunctionName(code.s1("resolve").name()),
                type_mark: code.s1("std_logic").type_mark(),
                constraint: None
            }
        );
    }

    #[test]
    fn parse_subtype_indication_with_array_element_resolution_function() {
        let code = Code::new("(resolve) integer_vector");
        assert_eq!(
            code.with_stream(parse_subtype_indication),
            SubtypeIndication {
                resolution: ResolutionIndication::ArrayElement(code.s1("resolve").name()),
                type_mark: code.s1("integer_vector").type_mark(),
                constraint: None
            }
        );
    }

    #[test]
    fn parse_subtype_indication_with_record_element_resolution_function() {
        let code = Code::new("(elem resolve) rec_t");

        let elem_resolution = RecordElementResolution {
            ident: code.s1("elem").ident(),
            resolution: Box::new(ResolutionIndication::FunctionName(
                code.s1("resolve").name(),
            )),
        };

        assert_eq!(
            code.with_stream(parse_subtype_indication),
            SubtypeIndication {
                resolution: ResolutionIndication::Record(vec![elem_resolution]),
                type_mark: code.s1("rec_t").type_mark(),
                constraint: None
            }
        );
    }

    #[test]
    fn parse_subtype_indication_with_record_element_resolution_function_many() {
        let code =
            Code::new("(elem1 (resolve1), elem2 resolve2, elem3 (sub_elem sub_resolve)) rec_t");

        let elem1_resolution = RecordElementResolution {
            ident: code.s1("elem1").ident(),
            resolution: Box::new(ResolutionIndication::ArrayElement(
                code.s1("resolve1").name(),
            )),
        };

        let elem2_resolution = RecordElementResolution {
            ident: code.s1("elem2").ident(),
            resolution: Box::new(ResolutionIndication::FunctionName(
                code.s1("resolve2").name(),
            )),
        };

        let sub_elem_resolution = RecordElementResolution {
            ident: code.s1("sub_elem").ident(),
            resolution: Box::new(ResolutionIndication::FunctionName(
                code.s1("sub_resolve").name(),
            )),
        };

        let elem3_resolution = RecordElementResolution {
            ident: code.s1("elem3").ident(),
            resolution: Box::new(ResolutionIndication::Record(vec![sub_elem_resolution])),
        };

        assert_eq!(
            code.with_stream(parse_subtype_indication),
            SubtypeIndication {
                resolution: ResolutionIndication::Record(vec![
                    elem1_resolution,
                    elem2_resolution,
                    elem3_resolution
                ]),
                type_mark: code.s1("rec_t").type_mark(),
                constraint: None
            }
        );
    }

    #[test]
    fn parse_subtype_indication_with_resolution_function_selected_name() {
        let code = Code::new("lib.foo.resolve std_logic");
        assert_eq!(
            code.with_stream(parse_subtype_indication),
            SubtypeIndication {
                resolution: ResolutionIndication::FunctionName(code.s1("lib.foo.resolve").name()),
                type_mark: code.s1("std_logic").type_mark(),
                constraint: None
            }
        );
    }

    #[test]
    /// LRM 8. Names
    fn parse_subtype_indication_without_selected_name() {
        let code = Code::new("lib.foo.bar");
        assert_eq!(
            code.with_stream(parse_subtype_indication),
            SubtypeIndication {
                resolution: ResolutionIndication::Unresolved,
                type_mark: code.s1("lib.foo.bar").type_mark(),
                constraint: None
            }
        );
    }

    #[test]
    fn parse_subtype_indication_with_range() {
        let code = Code::new("integer range 0 to 2-1");

        let constraint = WithTokenSpan::new(
            SubtypeConstraint::Range(code.s1("0 to 2-1").range()),
            code.s1("range 0 to 2-1").token_span(),
        );

        assert_eq!(
            code.with_stream(parse_subtype_indication),
            SubtypeIndication {
                resolution: ResolutionIndication::Unresolved,
                type_mark: code.s1("integer").type_mark(),
                constraint: Some(constraint)
            }
        );
    }

    #[test]
    fn parse_subtype_indication_with_range_attribute() {
        let code = Code::new("integer range lib.foo.bar'range");

        let constraint = WithTokenSpan::new(
            SubtypeConstraint::Range(code.s1("lib.foo.bar'range").range()),
            code.s1("range lib.foo.bar'range").token_span(),
        );

        assert_eq!(
            code.with_stream(parse_subtype_indication),
            SubtypeIndication {
                resolution: ResolutionIndication::Unresolved,
                type_mark: code.s1("integer").type_mark(),
                constraint: Some(constraint)
            }
        );
    }

    #[test]
    fn parse_subtype_indication_with_array_constraint_range() {
        let code = Code::new("integer_vector(2-1 downto 0)");

        let constraint = WithTokenSpan::new(
            SubtypeConstraint::Array(vec![code.s1("2-1 downto 0").discrete_range()], None),
            code.s1("(2-1 downto 0)").token_span(),
        );

        assert_eq!(
            code.with_stream(parse_subtype_indication),
            SubtypeIndication {
                resolution: ResolutionIndication::Unresolved,
                type_mark: code.s1("integer_vector").type_mark(),
                constraint: Some(constraint)
            }
        );
    }

    #[test]
    fn parse_subtype_indication_with_array_constraint_discrete() {
        let code = Code::new("integer_vector(lib.foo.bar)");

        let constraint = WithTokenSpan::new(
            SubtypeConstraint::Array(vec![code.s1("lib.foo.bar").discrete_range()], None),
            code.s1("(lib.foo.bar)").token_span(),
        );

        assert_eq!(
            code.with_stream(parse_subtype_indication),
            SubtypeIndication {
                resolution: ResolutionIndication::Unresolved,
                type_mark: code.s1("integer_vector").type_mark(),
                constraint: Some(constraint)
            }
        );
    }

    #[test]
    fn parse_subtype_indication_with_array_constraint_attribute() {
        let code = Code::new("integer_vector(lib.pkg.bar'range)");

        let constraint = WithTokenSpan::new(
            SubtypeConstraint::Array(vec![code.s1("lib.pkg.bar'range").discrete_range()], None),
            code.s1("(lib.pkg.bar'range)").token_span(),
        );

        assert_eq!(
            code.with_stream(parse_subtype_indication),
            SubtypeIndication {
                resolution: ResolutionIndication::Unresolved,
                type_mark: code.s1("integer_vector").type_mark(),
                constraint: Some(constraint)
            }
        );
    }

    #[test]
    fn parse_subtype_indication_with_array_constraint_open() {
        let code = Code::new("integer_vector(open)");

        let constraint = WithTokenSpan::new(
            SubtypeConstraint::Array(vec![], None),
            code.s1("(open)").token_span(),
        );

        assert_eq!(
            code.with_stream(parse_subtype_indication),
            SubtypeIndication {
                resolution: ResolutionIndication::Unresolved,
                type_mark: code.s1("integer_vector").type_mark(),
                constraint: Some(constraint)
            }
        );
    }

    #[test]
    fn parse_subtype_indication_with_multi_dim_array_constraints() {
        let code = Code::new("integer_vector(2-1 downto 0, 11 to 14)");

        let constraint = WithTokenSpan::new(
            SubtypeConstraint::Array(
                vec![
                    code.s1("2-1 downto 0").discrete_range(),
                    code.s1("11 to 14").discrete_range(),
                ],
                None,
            ),
            code.s1("(2-1 downto 0, 11 to 14)").token_span(),
        );

        assert_eq!(
            code.with_stream(parse_subtype_indication),
            SubtypeIndication {
                resolution: ResolutionIndication::Unresolved,
                type_mark: code.s1("integer_vector").type_mark(),
                constraint: Some(constraint)
            }
        );
    }

    #[test]
    fn parse_subtype_indication_with_array_element_constraint() {
        let code = Code::new("integer_vector(2-1 downto 0, 11 to 14)(foo to bar)");

        let element_constraint = WithTokenSpan::new(
            SubtypeConstraint::Array(vec![code.s1("foo to bar").discrete_range()], None),
            code.s1("(foo to bar)").token_span(),
        );

        let constraint = WithTokenSpan::new(
            SubtypeConstraint::Array(
                vec![
                    code.s1("2-1 downto 0").discrete_range(),
                    code.s1("11 to 14").discrete_range(),
                ],
                Some(Box::new(element_constraint)),
            ),
            code.s1("(2-1 downto 0, 11 to 14)(foo to bar)").token_span(),
        );

        assert_eq!(
            code.with_stream(parse_subtype_indication),
            SubtypeIndication {
                resolution: ResolutionIndication::Unresolved,
                type_mark: code.s1("integer_vector").type_mark(),
                constraint: Some(constraint)
            }
        );
    }

    #[test]
    fn parse_subtype_indication_with_record_constraint() {
        let code = Code::new("axi_m2s_t(tdata(2-1 downto 0), tuser(3 to 5))");

        let tdata_constraint = ElementConstraint {
            ident: code.s1("tdata").ident(),
            constraint: Box::new(WithTokenSpan::new(
                SubtypeConstraint::Array(vec![code.s1("2-1 downto 0").discrete_range()], None),
                code.s1("(2-1 downto 0)").token_span(),
            )),
        };

        let tuser_constraint = ElementConstraint {
            ident: code.s1("tuser").ident(),
            constraint: Box::new(WithTokenSpan::new(
                SubtypeConstraint::Array(vec![code.s1("3 to 5").discrete_range()], None),
                code.s1("(3 to 5)").token_span(),
            )),
        };

        assert_eq!(
            code.with_stream(parse_subtype_indication),
            SubtypeIndication {
                resolution: ResolutionIndication::Unresolved,
                type_mark: code.s1("axi_m2s_t").type_mark(),
                constraint: Some(WithTokenSpan::new(
                    SubtypeConstraint::Record(vec![tdata_constraint, tuser_constraint]),
                    code.s1("(tdata(2-1 downto 0), tuser(3 to 5))").token_span()
                ))
            }
        );
    }

    #[test]
    fn test_subtype_indication_with_subtype_attribute() {
        let code = Code::new("obj'subtype");

        assert_eq!(
            code.with_stream(parse_subtype_indication),
            SubtypeIndication {
                resolution: ResolutionIndication::Unresolved,
                type_mark: code.s1("obj'subtype").type_mark(),
                constraint: None
            }
        );

        let code = Code::new("obj.field'subtype");

        assert_eq!(
            code.with_stream(parse_subtype_indication),
            SubtypeIndication {
                resolution: ResolutionIndication::Unresolved,
                type_mark: code.s1("obj.field'subtype").type_mark(),
                constraint: None
            }
        );
    }
}
