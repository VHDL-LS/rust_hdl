// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

/// LRM 6.3 Subtype declarations
use crate::ast::{
    Designator, ElementConstraint, RecordElementResolution, ResolutionIndication, SelectedName,
    SubtypeConstraint, SubtypeIndication,
};
use crate::diagnostic::ParseResult;
use crate::names::parse_selected_name;
use crate::range::{parse_discrete_range, parse_range};
use crate::source::WithPos;
use crate::tokenizer::Kind::*;
use crate::tokenstream::TokenStream;

fn parse_record_element_constraint(stream: &mut TokenStream) -> ParseResult<ElementConstraint> {
    let ident = stream.expect_ident()?;
    let constraint = Box::new(parse_composite_constraint(stream)?);
    Ok(ElementConstraint { ident, constraint })
}

fn parse_composite_constraint(stream: &mut TokenStream) -> ParseResult<WithPos<SubtypeConstraint>> {
    // There is no finite lookahead that can differentiate
    // between array and record element constraint
    let leftpar_pos = stream.expect_kind(LeftPar)?.pos;
    let state = stream.state();
    let mut initial_constraint = parse_discrete_range(stream);
    if let Some(token) = stream.peek()? {
        match token.kind {
            RightPar | Comma => {}
            _ => {
                initial_constraint = Err(token
                    .kinds_error(&[RightPar, Comma])
                    .when("parsing discrete_range"));
            }
        }
    }

    if let Ok(initial_constraint) = initial_constraint {
        // Array constraint
        let mut constraints = vec![initial_constraint];

        let mut end_pos = loop {
            let sep_token = stream.expect()?;
            try_token_kind!(
                sep_token,
                RightPar => break sep_token.pos,
                Comma => {}
            );

            constraints.push(parse_discrete_range(stream)?);
        };

        // Array element constraint
        let element_constraint = {
            if let Some(elemement_constraint) = parse_subtype_constraint(stream)? {
                end_pos = elemement_constraint.pos.clone();
                Some(Box::new(elemement_constraint))
            } else {
                None
            }
        };

        Ok(WithPos::from(
            SubtypeConstraint::Array(constraints, element_constraint),
            leftpar_pos.combine_into(&end_pos),
        ))
    } else {
        // Record constraint
        stream.set_state(state);
        let mut constraints = vec![parse_record_element_constraint(stream)?];

        let rightpar_pos = loop {
            let sep_token = stream.expect()?;
            try_token_kind!(
                sep_token,
                RightPar => break sep_token.pos,
                Comma => {}
            );
            constraints.push(parse_record_element_constraint(stream)?);
        };

        Ok(WithPos::from(
            SubtypeConstraint::Record(constraints),
            leftpar_pos.combine_into(&rightpar_pos),
        ))
    }
}

pub fn parse_subtype_constraint(
    stream: &mut TokenStream,
) -> ParseResult<Option<WithPos<SubtypeConstraint>>> {
    if let Some(token) = stream.peek()? {
        let constraint = match token.kind {
            Range => {
                stream.move_after(&token);
                Some(
                    parse_range(stream)?
                        .map_into(SubtypeConstraint::Range)
                        .combine_pos_with(&token),
                )
            }
            LeftPar => Some(parse_composite_constraint(stream)?),
            _ => None,
        };
        Ok(constraint)
    } else {
        Ok(None)
    }
}

pub fn parse_element_resolution_indication(
    stream: &mut TokenStream,
) -> ParseResult<ResolutionIndication> {
    stream.expect_kind(LeftPar)?;

    let first_ident = stream.expect_ident()?;
    let token = stream.peek_expect()?;

    Ok(try_token_kind!(
        token,
        Dot | RightPar => {
            let selected_name = first_ident.map_into(|sym| SelectedName::Designator(Designator::Identifier(sym).into_ref()));
            stream.expect_kind(RightPar)?;
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
                        stream.expect_ident()?
                    }
                };

                let resolution = {
                    if stream.peek_kind()? == Some(LeftPar) {
                        parse_element_resolution_indication(stream)?
                    } else {
                        ResolutionIndication::FunctionName(parse_selected_name(stream)?)
                    }
                };

                element_resolutions.push(RecordElementResolution {
                    ident: ident,
                    resolution: Box::new(resolution),
                });

                let token = stream.expect()?;
                try_token_kind!(
                    token,
                    RightPar => break,
                    Comma => {}
                );

            }

            ResolutionIndication::Record(element_resolutions)
        }
    ))
}

pub fn parse_subtype_indication(stream: &mut TokenStream) -> ParseResult<SubtypeIndication> {
    let (resolution, type_mark) = {
        if stream.peek_kind()? == Some(LeftPar) {
            let resolution = parse_element_resolution_indication(stream)?;
            let type_mark = parse_selected_name(stream)?;
            (resolution, type_mark)
        } else {
            let selected_name = parse_selected_name(stream)?;
            match stream.peek_kind()? {
                Some(Identifier) => (
                    ResolutionIndication::FunctionName(selected_name),
                    parse_selected_name(stream)?,
                ),
                _ => (ResolutionIndication::Unresolved, selected_name),
            }
        }
    };

    let constraint = parse_subtype_constraint(stream)?;

    Ok(SubtypeIndication {
        resolution,
        type_mark,
        constraint,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::test_util::Code;

    #[test]
    fn parse_subtype_indication_without_constraint() {
        let code = Code::new("std_logic");
        assert_eq!(
            code.with_stream(parse_subtype_indication),
            SubtypeIndication {
                resolution: ResolutionIndication::Unresolved,
                type_mark: code.s1("std_logic").selected_name(),
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
                resolution: ResolutionIndication::FunctionName(code.s1("resolve").selected_name()),
                type_mark: code.s1("std_logic").selected_name(),
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
                resolution: ResolutionIndication::ArrayElement(code.s1("resolve").selected_name()),
                type_mark: code.s1("integer_vector").selected_name(),
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
                code.s1("resolve").selected_name(),
            )),
        };

        assert_eq!(
            code.with_stream(parse_subtype_indication),
            SubtypeIndication {
                resolution: ResolutionIndication::Record(vec![elem_resolution]),
                type_mark: code.s1("rec_t").selected_name(),
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
                code.s1("resolve1").selected_name(),
            )),
        };

        let elem2_resolution = RecordElementResolution {
            ident: code.s1("elem2").ident(),
            resolution: Box::new(ResolutionIndication::FunctionName(
                code.s1("resolve2").selected_name(),
            )),
        };

        let sub_elem_resolution = RecordElementResolution {
            ident: code.s1("sub_elem").ident(),
            resolution: Box::new(ResolutionIndication::FunctionName(
                code.s1("sub_resolve").selected_name(),
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
                type_mark: code.s1("rec_t").selected_name(),
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
                resolution: ResolutionIndication::FunctionName(
                    code.s1("lib.foo.resolve").selected_name()
                ),
                type_mark: code.s1("std_logic").selected_name(),
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
                type_mark: code.s1("lib.foo.bar").selected_name(),
                constraint: None
            }
        );
    }

    #[test]
    fn parse_subtype_indication_with_range() {
        let code = Code::new("integer range 0 to 2-1");

        let constraint = WithPos::new(
            SubtypeConstraint::Range(code.s1("0 to 2-1").range()),
            code.s1("range 0 to 2-1"),
        );

        assert_eq!(
            code.with_stream(parse_subtype_indication),
            SubtypeIndication {
                resolution: ResolutionIndication::Unresolved,
                type_mark: code.s1("integer").selected_name(),
                constraint: Some(constraint)
            }
        );
    }

    #[test]
    fn parse_subtype_indication_with_range_attribute() {
        let code = Code::new("integer range lib.foo.bar'range");

        let constraint = WithPos::new(
            SubtypeConstraint::Range(code.s1("lib.foo.bar'range").range()),
            code.s1("range lib.foo.bar'range"),
        );

        assert_eq!(
            code.with_stream(parse_subtype_indication),
            SubtypeIndication {
                resolution: ResolutionIndication::Unresolved,
                type_mark: code.s1("integer").selected_name(),
                constraint: Some(constraint)
            }
        );
    }

    #[test]
    fn parse_subtype_indication_with_array_constraint_range() {
        let code = Code::new("integer_vector(2-1 downto 0)");

        let constraint = WithPos::new(
            SubtypeConstraint::Array(vec![code.s1("2-1 downto 0").discrete_range()], None),
            code.s1("(2-1 downto 0)"),
        );

        assert_eq!(
            code.with_stream(parse_subtype_indication),
            SubtypeIndication {
                resolution: ResolutionIndication::Unresolved,
                type_mark: code.s1("integer_vector").selected_name(),
                constraint: Some(constraint)
            }
        );
    }

    #[test]
    fn parse_subtype_indication_with_array_constraint_discrete() {
        let code = Code::new("integer_vector(lib.foo.bar)");

        let constraint = WithPos::new(
            SubtypeConstraint::Array(vec![code.s1("lib.foo.bar").discrete_range()], None),
            code.s1("(lib.foo.bar)"),
        );

        assert_eq!(
            code.with_stream(parse_subtype_indication),
            SubtypeIndication {
                resolution: ResolutionIndication::Unresolved,
                type_mark: code.s1("integer_vector").selected_name(),
                constraint: Some(constraint)
            }
        );
    }

    #[test]
    fn parse_subtype_indication_with_array_constraint_attribute() {
        let code = Code::new("integer_vector(lib.pkg.bar'range)");

        let constraint = WithPos::new(
            SubtypeConstraint::Array(vec![code.s1("lib.pkg.bar'range").discrete_range()], None),
            code.s1("(lib.pkg.bar'range)"),
        );

        assert_eq!(
            code.with_stream(parse_subtype_indication),
            SubtypeIndication {
                resolution: ResolutionIndication::Unresolved,
                type_mark: code.s1("integer_vector").selected_name(),
                constraint: Some(constraint)
            }
        );
    }

    #[test]
    fn parse_subtype_indication_with_multi_dim_array_constraints() {
        let code = Code::new("integer_vector(2-1 downto 0, 11 to 14)");

        let constraint = WithPos::new(
            SubtypeConstraint::Array(
                vec![
                    code.s1("2-1 downto 0").discrete_range(),
                    code.s1("11 to 14").discrete_range(),
                ],
                None,
            ),
            code.s1("(2-1 downto 0, 11 to 14)"),
        );

        assert_eq!(
            code.with_stream(parse_subtype_indication),
            SubtypeIndication {
                resolution: ResolutionIndication::Unresolved,
                type_mark: code.s1("integer_vector").selected_name(),
                constraint: Some(constraint)
            }
        );
    }

    #[test]
    fn parse_subtype_indication_with_array_element_constraint() {
        let code = Code::new("integer_vector(2-1 downto 0, 11 to 14)(foo to bar)");

        let element_constraint = WithPos::new(
            SubtypeConstraint::Array(vec![code.s1("foo to bar").discrete_range()], None),
            code.s1("(foo to bar)"),
        );

        let constraint = WithPos::new(
            SubtypeConstraint::Array(
                vec![
                    code.s1("2-1 downto 0").discrete_range(),
                    code.s1("11 to 14").discrete_range(),
                ],
                Some(Box::new(element_constraint)),
            ),
            code.s1("(2-1 downto 0, 11 to 14)(foo to bar)"),
        );

        assert_eq!(
            code.with_stream(parse_subtype_indication),
            SubtypeIndication {
                resolution: ResolutionIndication::Unresolved,
                type_mark: code.s1("integer_vector").selected_name(),
                constraint: Some(constraint)
            }
        );
    }

    #[test]
    fn parse_subtype_indication_with_record_constraint() {
        let code = Code::new("axi_m2s_t(tdata(2-1 downto 0), tuser(3 to 5))");

        let tdata_constraint = ElementConstraint {
            ident: code.s1("tdata").ident(),
            constraint: Box::new(WithPos::new(
                SubtypeConstraint::Array(vec![code.s1("2-1 downto 0").discrete_range()], None),
                code.s1("(2-1 downto 0)"),
            )),
        };

        let tuser_constraint = ElementConstraint {
            ident: code.s1("tuser").ident(),
            constraint: Box::new(WithPos::new(
                SubtypeConstraint::Array(vec![code.s1("3 to 5").discrete_range()], None),
                code.s1("(3 to 5)"),
            )),
        };

        assert_eq!(
            code.with_stream(parse_subtype_indication),
            SubtypeIndication {
                resolution: ResolutionIndication::Unresolved,
                type_mark: code.s1("axi_m2s_t").selected_name(),
                constraint: Some(WithPos::new(
                    SubtypeConstraint::Record(vec![tdata_constraint, tuser_constraint]),
                    code.s1("(tdata(2-1 downto 0), tuser(3 to 5))")
                ))
            }
        );
    }
}
