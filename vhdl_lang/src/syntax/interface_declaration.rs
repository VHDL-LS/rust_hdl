// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use super::common::ParseResult;
use super::names::{
    parse_association_list_no_leftpar, parse_identifier_list, parse_name, parse_selected_name,
};
use super::object_declaration::parse_optional_assignment;
use super::subprogram::parse_subprogram_specification;
use super::subtype_indication::parse_subtype_indication;
use super::tokens::{Kind::*, *};
use crate::ast::token_range::WithToken;
/// LRM 6.5 Interface declarations
use crate::ast::*;
use crate::data::*;
use vhdl_lang::syntax::parser::ParsingContext;
use vhdl_lang::VHDLStandard::VHDL2019;

pub(crate) fn parse_optional_mode(
    ctx: &mut ParsingContext<'_>,
) -> ParseResult<Option<WithToken<Mode>>> {
    let token = ctx.stream.peek_expect()?;
    let id = ctx.stream.get_current_token_id();
    let mode = match token.kind {
        In => Mode::In,
        Out => Mode::Out,
        InOut => Mode::InOut,
        Buffer => Mode::Buffer,
        Linkage => Mode::Linkage,
        _ => return Ok(None),
    };
    ctx.stream.skip();
    Ok(Some(WithToken::new(mode, id)))
}

fn unexpected_object_class_kind(list_type: InterfaceType, token: &Token) -> Diagnostic {
    match list_type {
        InterfaceType::Generic => token.kinds_error(&[Constant, Identifier]),
        InterfaceType::Port => token.kinds_error(&[Signal, Identifier]),
        InterfaceType::Parameter => token.kinds_error(&[Signal, Constant, Variable, Identifier]),
    }
}

fn parse_optional_object_class(
    ctx: &mut ParsingContext<'_>,
    list_type: InterfaceType,
) -> ParseResult<Option<WithToken<ObjectClass>>> {
    let token = ctx.stream.peek_expect()?;
    let id = ctx.stream.get_current_token_id();

    let class = match token.kind {
        Constant => ObjectClass::Constant,
        Variable => ObjectClass::Variable,
        Signal => ObjectClass::Signal,
        Identifier => return Ok(None),
        _ => return Err(unexpected_object_class_kind(list_type, token)),
    };
    ctx.stream.skip();
    Ok(Some(WithToken::new(class, id)))
}

fn parse_interface_file_declaration(
    ctx: &mut ParsingContext<'_>,
) -> ParseResult<Vec<InterfaceDeclaration>> {
    let start_token = ctx.stream.expect_kind(File)?;
    let idents = parse_identifier_list(ctx)?;
    ctx.stream.expect_kind(Colon)?;
    let subtype = parse_subtype_indication(ctx)?;

    if ctx.stream.next_kind_is(Open) {
        if let Some(ident) = idents.first() {
            return Err(Diagnostic::syntax_error(
                ident.pos(ctx),
                "interface_file_declaration may not have file open information",
            ));
        }
    }
    if ctx.stream.next_kind_is(Is) {
        if let Some(ident) = idents.first() {
            return Err(Diagnostic::syntax_error(
                ident.pos(ctx),
                "interface_file_declaration may not have file name",
            ));
        }
    }

    let end_token = ctx.stream.get_last_token_id();

    Ok(idents
        .into_iter()
        .map(|ident| {
            InterfaceDeclaration::File(InterfaceFileDeclaration {
                ident: ident.into(),
                subtype_indication: subtype.clone(),
                span: TokenSpan::new(start_token, end_token),
            })
        })
        .collect())
}

fn parse_interface_object_declaration(
    ctx: &mut ParsingContext<'_>,
    list_type: InterfaceType,
) -> ParseResult<Vec<InterfaceDeclaration>> {
    let start_token = ctx.stream.get_current_token_id();
    let explicit_object_class = parse_optional_object_class(ctx, list_type)?;

    let idents = parse_identifier_list(ctx)?;

    ctx.stream.expect_kind(Colon)?;
    let mode = if ctx.stream.next_kind_is(View) {
        ModeIndication::View(parse_view_mode_indication(ctx)?)
    } else {
        ModeIndication::Simple(parse_simple_mode_indication(
            ctx,
            list_type,
            explicit_object_class.as_ref(),
            &idents,
        )?)
    };

    let end_token = ctx.stream.get_last_token_id();
    Ok(idents
        .into_iter()
        .map(|ident| {
            InterfaceDeclaration::Object(InterfaceObjectDeclaration {
                list_type,
                mode: mode.clone(),
                ident: ident.into(),
                span: TokenSpan::new(start_token, end_token),
            })
        })
        .collect())
}

fn parse_view_mode_indication(ctx: &mut ParsingContext<'_>) -> ParseResult<ModeViewIndication> {
    ctx.stream.expect_kind(View)?;
    let (name, kind) = if ctx.stream.pop_if_kind(LeftPar).is_some() {
        let _name = parse_name(ctx)?;
        ctx.stream.expect_kind(RightPar)?;
        (_name, ModeViewIndicationKind::Array)
    } else {
        (parse_name(ctx)?, ModeViewIndicationKind::Record)
    };
    let subtype_indication = if ctx.stream.pop_if_kind(Of).is_some() {
        Some(parse_subtype_indication(ctx)?)
    } else {
        None
    };
    Ok(ModeViewIndication {
        subtype_indication,
        name,
        kind,
    })
}

fn parse_simple_mode_indication(
    ctx: &mut ParsingContext<'_>,
    list_type: InterfaceType,
    explicit_object_class: Option<&WithToken<ObjectClass>>,
    idents: &[Ident],
) -> ParseResult<SimpleModeIndication> {
    let object_class_tok = explicit_object_class.map(|class| class.token);
    let mode_with_pos = parse_optional_mode(ctx)?;
    let mode = mode_with_pos.as_ref().map(|mode| mode.item);
    let mode_tok = mode_with_pos.as_ref().map(|mode| mode.token);

    let object_class = match (
        list_type,
        explicit_object_class.map(|class| class.item),
        mode,
    ) {
        (_, Some(object_class), _) => object_class,
        (InterfaceType::Port, None, _) => ObjectClass::Signal,
        (InterfaceType::Generic, None, _) => ObjectClass::Constant,
        (InterfaceType::Parameter, None, None | Some(Mode::In)) => ObjectClass::Constant,
        (InterfaceType::Parameter, None, _) => ObjectClass::Variable,
    };

    let subtype = parse_subtype_indication(ctx)?;
    let bus = ctx.stream.pop_if_kind(Bus).is_some();
    let expr = parse_optional_assignment(ctx)?;

    // @TODO maybe move this to a semantic check?
    for ident in idents.iter() {
        if object_class == ObjectClass::Constant && mode.unwrap_or_default() != Mode::In {
            let token_id = mode_tok.unwrap_or(ident.token);
            return Err(Diagnostic::syntax_error(
                ctx.get_pos(token_id),
                "Interface constant declaration may only have mode=in",
            ));
        };

        if list_type == InterfaceType::Port && object_class != ObjectClass::Signal {
            let tok = object_class_tok.unwrap_or(ident.token);
            return Err(Diagnostic::syntax_error(
                ctx.get_pos(tok),
                "Port list only allows signal object class",
            ));
        };

        if list_type == InterfaceType::Generic && object_class != ObjectClass::Constant {
            let tok = object_class_tok.unwrap_or(ident.token);
            return Err(Diagnostic::syntax_error(
                ctx.get_pos(tok),
                "Generic list only allows constant object class",
            ));
        };
    }

    Ok(SimpleModeIndication {
        mode: mode_with_pos,
        class: object_class,
        subtype_indication: subtype,
        expression: expr,
        bus,
    })
}

fn parse_subprogram_default(
    ctx: &mut ParsingContext<'_>,
) -> ParseResult<Option<SubprogramDefault>> {
    if ctx.stream.skip_if_kind(Is) {
        let default = {
            peek_token!(
                ctx.stream, token,
                Identifier => SubprogramDefault::Name(parse_selected_name(ctx)?),
                BOX => {
                    ctx.stream.skip();
                    SubprogramDefault::Box
                }
            )
        };

        Ok(Some(default))
    } else {
        Ok(None)
    }
}

fn parse_interface_package(
    ctx: &mut ParsingContext<'_>,
) -> ParseResult<InterfacePackageDeclaration> {
    let start_token = ctx.stream.expect_kind(Package)?;
    let ident = ctx.stream.expect_ident()?;
    ctx.stream.expect_kind(Is)?;
    ctx.stream.expect_kind(New)?;
    let package_name = parse_selected_name(ctx)?;
    ctx.stream.expect_kind(Generic)?;
    ctx.stream.expect_kind(Map)?;

    let generic_map = {
        let left_par = ctx.stream.expect_kind(LeftPar)?;
        let map_token = ctx.stream.peek_expect()?;
        match map_token.kind {
            BOX => {
                ctx.stream.skip();
                ctx.stream.expect_kind(RightPar)?;
                InterfacePackageGenericMapAspect::Box
            }
            Default => {
                ctx.stream.skip();
                ctx.stream.expect_kind(RightPar)?;
                InterfacePackageGenericMapAspect::Default
            }
            _ => {
                let (list, _) = parse_association_list_no_leftpar(ctx, left_par)?;
                InterfacePackageGenericMapAspect::Map(list)
            }
        }
    };
    let last_token = ctx.stream.get_last_token_id();

    Ok(InterfacePackageDeclaration {
        ident: ident.into(),
        package_name,
        generic_map,
        span: TokenSpan::new(start_token, last_token),
    })
}

fn parse_interface_declaration(
    ctx: &mut ParsingContext,
    list_type: InterfaceType,
) -> ParseResult<Vec<InterfaceDeclaration>> {
    peek_token!(
        ctx.stream, token, start_token,
        Signal | Constant | Variable | Identifier => {
            parse_interface_object_declaration(ctx, list_type)
        },
        File => parse_interface_file_declaration(ctx),
        Type => {
            ctx.stream.skip();
            let ident = ctx.stream.expect_ident()?;
            Ok(vec![InterfaceDeclaration::Type(WithDecl::new(ident))])
        },
        Function | Procedure | Impure | Pure => {
            let spec = parse_subprogram_specification(ctx)?;
            let default = parse_subprogram_default(ctx)?;

            let end_token = ctx.stream.get_last_token_id();

            Ok(vec![InterfaceDeclaration::Subprogram(InterfaceSubprogramDeclaration { specification: spec, default, span: TokenSpan::new(start_token, end_token)})])
        },
        Package => {
            Ok(vec![InterfaceDeclaration::Package (parse_interface_package(ctx)?)])
        }
    )
}

/// Parse ; separator in generic or port lists.
/// Expect ; for all but the last item
fn parse_semicolon_separator(ctx: &mut ParsingContext<'_>) -> ParseResult<()> {
    peek_token!(
        ctx.stream, token,
        SemiColon => {
            ctx.stream.skip();
            if ctx.stream.next_kind_is(RightPar) && ctx.standard < VHDL2019 {
                return Err(Diagnostic::syntax_error(&token.pos,
                        format!("Last interface element may not end with {}",
                        kinds_str(&[SemiColon]))));
            }
        },
        RightPar => {}
    );
    Ok(())
}

fn is_sync_kind(list_type: InterfaceType, kind: Kind) -> bool {
    matches!(
        (list_type, kind),
        (InterfaceType::Generic, Constant)
            | (InterfaceType::Port, Signal)
            | (InterfaceType::Parameter, Constant)
            | (InterfaceType::Parameter, Variable)
            | (InterfaceType::Parameter, Signal)
    )
}

fn parse_interface_list(
    ctx: &mut ParsingContext<'_>,
    list_type: InterfaceType,
) -> ParseResult<InterfaceList> {
    let mut interface_list = Vec::new();

    let left_par = ctx.stream.expect_kind(LeftPar)?;
    let right_par;

    'outer: loop {
        let token = ctx.stream.peek_expect()?;
        match token.kind {
            RightPar => {
                if interface_list.is_empty() {
                    ctx.diagnostics.add(
                        ctx.stream.get_pos(left_par).combine(token),
                        "Interface list must not be empty",
                        ErrorCode::SyntaxError,
                    );
                }
                right_par = ctx.stream.get_current_token_id();
                ctx.stream.skip();
                break;
            }
            _ => {
                let state = ctx.stream.state();

                match parse_interface_declaration(ctx, list_type) {
                    Ok(ref mut decl_list) => {
                        interface_list.append(decl_list);
                    }
                    Err(err) => {
                        ctx.diagnostics.push(err);
                        ctx.stream.set_state(state);
                        if let Some(token) = ctx.stream.peek() {
                            if is_sync_kind(list_type, token.kind) {
                                ctx.stream.skip();
                            }
                        }

                        // Recover
                        while let Some(token) = ctx.stream.peek() {
                            match token.kind {
                                SemiColon => {
                                    ctx.stream.skip();
                                    continue 'outer;
                                }
                                kind if is_sync_kind(list_type, kind) => {
                                    continue 'outer;
                                }
                                RightPar => {
                                    right_par = ctx.stream.get_current_token_id();
                                    ctx.stream.skip();
                                    break 'outer;
                                }
                                _ => {
                                    ctx.stream.skip();
                                }
                            }
                        }
                    }
                }

                if let Err(err) = parse_semicolon_separator(ctx) {
                    ctx.diagnostics.push(err);
                    // Ignore comma when recovering from errors
                    ctx.stream.pop_if_kind(Comma);
                }
            }
        }
    }

    Ok(InterfaceList {
        interface_type: list_type,
        items: interface_list,
        span: TokenSpan::new(left_par, right_par),
    })
}

pub fn parse_generic_interface_list(ctx: &mut ParsingContext<'_>) -> ParseResult<InterfaceList> {
    parse_interface_list(ctx, InterfaceType::Generic)
}

pub fn parse_port_interface_list(ctx: &mut ParsingContext<'_>) -> ParseResult<InterfaceList> {
    parse_interface_list(ctx, InterfaceType::Port)
}

pub fn parse_parameter_interface_list(ctx: &mut ParsingContext<'_>) -> ParseResult<InterfaceList> {
    parse_interface_list(ctx, InterfaceType::Parameter)
}

#[cfg(test)]
fn parse_one_interface_declaration(
    ctx: &mut ParsingContext<'_>,
    list_type: InterfaceType,
) -> ParseResult<InterfaceDeclaration> {
    parse_interface_declaration(ctx, list_type).map(|decls| {
        assert_eq!(decls.len(), 1);
        decls[0].clone()
    })
}

#[cfg(test)]
pub fn parse_parameter(ctx: &mut ParsingContext) -> ParseResult<InterfaceDeclaration> {
    parse_one_interface_declaration(ctx, InterfaceType::Parameter)
}

#[cfg(test)]
pub fn parse_port(ctx: &mut ParsingContext) -> ParseResult<InterfaceDeclaration> {
    parse_one_interface_declaration(ctx, InterfaceType::Port)
}

#[cfg(test)]
pub fn parse_generic(ctx: &mut ParsingContext) -> ParseResult<InterfaceDeclaration> {
    parse_one_interface_declaration(ctx, InterfaceType::Generic)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::analysis::tests::check_diagnostics;
    use crate::syntax::subprogram::parse_subprogram_declaration;
    use crate::syntax::test::Code;
    use crate::syntax::tokens::kinds_error;
    use crate::VHDLStandard::VHDL2019;
    use assert_matches::assert_matches;
    use pretty_assertions::assert_eq;

    #[test]
    fn parses_interface_identifier_list() {
        let code = Code::new("(constant foo, bar : natural)");
        assert_eq!(
            code.with_stream_no_diagnostics(parse_generic_interface_list),
            InterfaceList {
                interface_type: InterfaceType::Generic,
                items: vec![
                    InterfaceDeclaration::Object(InterfaceObjectDeclaration {
                        list_type: InterfaceType::Generic,
                        mode: ModeIndication::Simple(SimpleModeIndication {
                            bus: false,
                            mode: None,
                            class: ObjectClass::Constant,

                            subtype_indication: code.s1("natural").subtype_indication(),
                            expression: None
                        }),
                        ident: code.s1("foo").decl_ident(),
                        span: code.between("constant", "natural").token_span()
                    }),
                    InterfaceDeclaration::Object(InterfaceObjectDeclaration {
                        list_type: InterfaceType::Generic,
                        mode: ModeIndication::Simple(SimpleModeIndication {
                            bus: false,
                            mode: None,
                            class: ObjectClass::Constant,

                            subtype_indication: code.s1("natural").subtype_indication(),
                            expression: None
                        }),
                        ident: code.s1("bar").decl_ident(),
                        span: code.between("constant", "natural").token_span()
                    })
                ],
                span: code.token_span(),
            }
        );
    }

    #[test]
    fn parses_generic() {
        let code = Code::new("foo : std_logic");
        assert_eq!(
            code.with_stream(parse_generic),
            InterfaceDeclaration::Object(InterfaceObjectDeclaration {
                list_type: InterfaceType::Generic,
                mode: ModeIndication::Simple(SimpleModeIndication {
                    bus: false,
                    mode: None,
                    class: ObjectClass::Constant,

                    subtype_indication: code.s1("std_logic").subtype_indication(),
                    expression: None
                }),
                ident: code.s1("foo").decl_ident(),
                span: code.token_span()
            })
        );
    }

    #[test]
    fn parses_interface_file_declaration() {
        let code = Code::new("file foo : text");
        assert_eq!(
            code.with_stream(parse_parameter),
            InterfaceDeclaration::File(InterfaceFileDeclaration {
                ident: code.s1("foo").decl_ident(),
                subtype_indication: code.s1("text").subtype_indication(),
                span: code.token_span()
            })
        );
    }

    #[test]
    fn parses_interface_file_declaration_no_open_info() {
        let code = Code::new("(file foo : text open read_mode)");
        assert_eq!(
            code.with_stream_diagnostics(parse_parameter_interface_list),
            (
                InterfaceList {
                    interface_type: InterfaceType::Parameter,
                    items: vec![],
                    span: code.token_span()
                },
                vec![Diagnostic::syntax_error(
                    code.s1("foo"),
                    "interface_file_declaration may not have file open information"
                )]
            )
        );
    }

    #[test]
    fn parses_interface_file_declaration_no_file_name() {
        let code = Code::new("(file foo : text is \"file_name\")");
        assert_eq!(
            code.with_stream_diagnostics(parse_parameter_interface_list),
            (
                InterfaceList {
                    interface_type: InterfaceType::Parameter,
                    items: vec![],
                    span: code.token_span()
                },
                vec![Diagnostic::syntax_error(
                    code.s1("foo"),
                    "interface_file_declaration may not have file name"
                )]
            )
        );
    }

    #[test]
    fn parses_interface_file_declaration_list_with_errors() {
        let code = Code::new("(file with_name: text is \"file_name\"; file valid : text; file open_info: text open read_mode)");
        assert_eq!(
            code.with_stream_diagnostics(parse_parameter_interface_list),
            (
                InterfaceList {
                    interface_type: InterfaceType::Parameter,
                    items: vec![InterfaceDeclaration::File(InterfaceFileDeclaration {
                        ident: code.s1("valid").decl_ident(),
                        subtype_indication: code.s("text", 2).subtype_indication(),
                        span: code.s1("file valid : text").token_span()
                    })],
                    span: code.token_span()
                },
                vec![
                    Diagnostic::syntax_error(
                        code.s1("with_name"),
                        "interface_file_declaration may not have file name"
                    ),
                    Diagnostic::syntax_error(
                        code.s1("open_info"),
                        "interface_file_declaration may not have file open information"
                    )
                ]
            )
        );
    }

    #[test]
    fn parses_port() {
        let code = Code::new("foo : std_logic");
        assert_eq!(
            code.with_stream(parse_port),
            InterfaceDeclaration::Object(InterfaceObjectDeclaration {
                list_type: InterfaceType::Port,
                mode: ModeIndication::Simple(SimpleModeIndication {
                    bus: false,
                    mode: None,
                    class: ObjectClass::Signal,

                    subtype_indication: code.s1("std_logic").subtype_indication(),
                    expression: None
                }),
                ident: code.s1("foo").decl_ident(),
                span: code.token_span()
            })
        );
    }

    fn to_interface_object(interface_decl: InterfaceDeclaration) -> InterfaceObjectDeclaration {
        match interface_decl {
            InterfaceDeclaration::Object(object) => object,
            _ => panic!("{interface_decl:?}"),
        }
    }

    #[test]
    fn parses_port_without_explicit_class() {
        let code = Code::new("foo : std_logic");
        let result = to_interface_object(code.with_stream(parse_port));
        assert_matches!(
            result.mode,
            ModeIndication::Simple(SimpleModeIndication {
                mode: None,
                class: ObjectClass::Signal,
                ..
            })
        );
    }

    #[test]
    fn parses_generic_without_explicit_class() {
        let code = Code::new("foo : std_logic");
        let result = to_interface_object(code.with_stream(parse_generic));
        assert_matches!(
            result.mode,
            ModeIndication::Simple(SimpleModeIndication {
                mode: None,
                class: ObjectClass::Constant,
                ..
            })
        );
    }

    #[test]
    fn parses_parameter_without_explicit_class() {
        // LRM 4.2.2.1 Formal parameter lists
        // Procedure only allows in, inout, outer
        // in => Constant
        // inout, out => Variable
        // @TODO forbid other modes
        // @TODO forbid mode != in for function
        let code = Code::new("foo : std_logic");
        let result = to_interface_object(code.with_stream(parse_parameter));
        assert_matches!(
            result.mode,
            ModeIndication::Simple(SimpleModeIndication {
                mode: None,
                class: ObjectClass::Constant,
                ..
            })
        );

        let code = Code::new("foo : in std_logic");
        let result = to_interface_object(code.with_stream(parse_parameter));
        assert_matches!(
            result.mode,
            ModeIndication::Simple(SimpleModeIndication {
                mode: Some(WithToken { item: Mode::In, .. }),
                class: ObjectClass::Constant,
                ..
            })
        );

        let code = Code::new("foo : out std_logic");
        let result = to_interface_object(code.with_stream(parse_parameter));
        assert_matches!(
            result.mode,
            ModeIndication::Simple(SimpleModeIndication {
                mode: Some(WithToken {
                    item: Mode::Out,
                    ..
                }),
                class: ObjectClass::Variable,
                ..
            })
        );

        let code = Code::new("foo : inout std_logic");
        let result = to_interface_object(code.with_stream(parse_parameter));
        assert_matches!(
            result.mode,
            ModeIndication::Simple(SimpleModeIndication {
                mode: Some(WithToken {
                    item: Mode::InOut,
                    ..
                }),
                class: ObjectClass::Variable,
                ..
            })
        );
    }

    #[test]
    fn parses_generic_with_optional_keyword() {
        let code = Code::new("constant foo : std_logic");
        assert_eq!(
            code.with_stream(parse_generic),
            InterfaceDeclaration::Object(InterfaceObjectDeclaration {
                list_type: InterfaceType::Generic,
                mode: ModeIndication::Simple(SimpleModeIndication {
                    bus: false,
                    mode: None,
                    class: ObjectClass::Constant,
                    subtype_indication: code.s1("std_logic").subtype_indication(),
                    expression: None
                }),
                ident: code.s1("foo").decl_ident(),
                span: code.token_span()
            })
        );
    }

    #[test]
    fn parses_port_with_optional_keyword() {
        let code = Code::new("signal foo : std_logic");
        assert_eq!(
            code.with_stream(parse_port),
            InterfaceDeclaration::Object(InterfaceObjectDeclaration {
                list_type: InterfaceType::Port,
                mode: ModeIndication::Simple(SimpleModeIndication {
                    bus: false,
                    mode: None,
                    class: ObjectClass::Signal,
                    subtype_indication: code.s1("std_logic").subtype_indication(),
                    expression: None
                }),
                ident: code.s1("foo").decl_ident(),
                span: code.token_span()
            })
        );
    }

    #[test]
    fn parse_generic_non_in_mode_error() {
        let code = Code::new("foo : out boolean");
        assert_eq!(
            code.with_partial_stream(parse_generic),
            Err(Diagnostic::syntax_error(
                &code.s1("out").pos(),
                "Interface constant declaration may only have mode=in"
            ))
        );
    }

    #[test]
    fn test_parse_generic_interface_list() {
        let code = Code::new(
            "\
(constant foo : std_logic;
bar : natural)",
        );

        assert_eq!(
            code.with_stream_no_diagnostics(parse_generic_interface_list),
            InterfaceList {
                interface_type: InterfaceType::Generic,
                items: vec![
                    code.s1("constant foo : std_logic").generic(),
                    code.s1("bar : natural").generic()
                ],
                span: code.token_span()
            }
        );
    }

    #[test]
    fn test_parse_generic_interface_list_error_on_last_semi_colon() {
        let code = Code::new(
            "\
(constant foo : std_logic;
 bar : natural;
)",
        );
        let (result, diagnostics) = code.with_stream_diagnostics(parse_generic_interface_list);

        assert_eq!(
            result,
            InterfaceList {
                interface_type: InterfaceType::Generic,
                items: vec![
                    code.s1("constant foo : std_logic").generic(),
                    code.s1("bar : natural").generic()
                ],
                span: code.token_span()
            }
        );
        assert_eq!(
            diagnostics,
            vec![Diagnostic::syntax_error(
                code.s(";", 2),
                "Last interface element may not end with ';'"
            )]
        );

        let code = Code::with_standard(
            "\
(constant foo : std_logic;
 bar : natural;
)",
            VHDL2019,
        );
        let result = code.parse_ok_no_diagnostics(parse_generic_interface_list);

        assert_eq!(
            result,
            InterfaceList {
                interface_type: InterfaceType::Generic,
                items: vec![
                    code.s1("constant foo : std_logic").generic(),
                    code.s1("bar : natural").generic()
                ],
                span: code.token_span()
            }
        );
    }

    #[test]
    fn test_parse_port_interface_list() {
        let code = Code::new(
            "\
(signal foo : in std_logic;
bar : natural)",
        );

        assert_eq!(
            code.with_stream_no_diagnostics(parse_port_interface_list),
            InterfaceList {
                interface_type: InterfaceType::Port,
                items: vec![
                    code.s1("signal foo : in std_logic").port(),
                    code.s1("bar : natural").port()
                ],
                span: code.token_span()
            }
        );
    }

    #[test]
    fn test_parse_parameter_interface_list() {
        let code = Code::new(
            "\
(signal foo : in std_logic;
 constant bar : natural;
 variable xyz : var)",
        );

        assert_eq!(
            code.with_stream_no_diagnostics(parse_parameter_interface_list),
            InterfaceList {
                interface_type: InterfaceType::Parameter,
                items: vec![
                    code.s1("signal foo : in std_logic").parameter(),
                    code.s1("constant bar : natural").parameter(),
                    code.s1("variable xyz : var").parameter(),
                ],
                span: code.token_span()
            }
        );
    }

    #[test]
    fn test_parse_generic_interface_list_recovery_comma_instead_of_semicolon() {
        let code = Code::new(
            "\
(constant c1 : natural,
 constant c2 : natural)",
        );

        let (result, diagnostics) = code.with_stream_diagnostics(parse_generic_interface_list);
        assert_eq!(
            result,
            InterfaceList {
                interface_type: InterfaceType::Generic,
                items: vec![
                    code.s1("constant c1 : natural").generic(),
                    code.s1("constant c2 : natural").generic(),
                ],
                span: code.token_span()
            }
        );
        assert_eq!(
            diagnostics,
            vec![kinds_error(code.s1(","), &[SemiColon, RightPar])]
        );
    }

    #[test]
    fn test_parse_generic_interface_no_signal() {
        let code = Code::new("(signal c1 : natural)");
        let (_, diagnostics) = code.with_stream_diagnostics(parse_generic_interface_list);
        assert_eq!(
            diagnostics,
            vec![Diagnostic::syntax_error(
                code.s1("signal"),
                "Generic list only allows constant object class"
            )]
        );
    }

    #[test]
    fn test_parse_port_interface_no_constant() {
        let code = Code::new("(constant c1 : natural)");
        let (_, diagnostics) = code.with_stream_diagnostics(parse_port_interface_list);
        assert_eq!(
            diagnostics,
            vec![Diagnostic::syntax_error(
                code.s1("constant"),
                "Port list only allows signal object class"
            )]
        );
    }

    #[test]
    fn test_parse_generic_interface_list_recovery() {
        let code = Code::new(
            "\
(constant c1_err : ;
 -- Recover on previous ;
 constant c2 : natural;
 -- Ignore missing ;
 constant c3 : natural,
 constant c4 : natural;
 constant c5_err
 -- Recover on constant
 constant c6 : natural;
 constant c7_err :)",
        );

        let (result, diagnostics) = code.with_stream_diagnostics(parse_generic_interface_list);
        assert_eq!(
            result,
            InterfaceList {
                interface_type: InterfaceType::Generic,
                items: vec![
                    code.s1("constant c2 : natural").generic(),
                    code.s1("constant c3 : natural").generic(),
                    code.s1("constant c4 : natural").generic(),
                    code.s1("constant c6 : natural").generic()
                ],
                span: code.token_span()
            }
        );
        assert_eq!(diagnostics.len(), 4);
    }

    #[test]
    fn parses_interface_type() {
        let code = Code::new("type name");
        assert_eq!(
            code.with_stream(parse_generic),
            InterfaceDeclaration::Type(code.s1("name").decl_ident())
        );
    }

    #[test]
    fn parses_interface_subprogram() {
        let code = Code::new("function foo return bar");
        assert_eq!(
            code.with_stream(parse_generic),
            InterfaceDeclaration::Subprogram(InterfaceSubprogramDeclaration {
                specification: code
                    .s1("function foo return bar")
                    .subprogram_specification(),
                default: None,
                span: code.token_span()
            })
        );
        let code = Code::new("procedure foo");
        assert_eq!(
            code.with_stream(parse_generic),
            InterfaceDeclaration::Subprogram(InterfaceSubprogramDeclaration {
                specification: code.s1("procedure foo").subprogram_specification(),
                default: None,
                span: code.token_span()
            })
        );
        let code = Code::new("impure function foo return bar");
        assert_eq!(
            code.with_stream(parse_generic),
            InterfaceDeclaration::Subprogram(InterfaceSubprogramDeclaration {
                specification: code
                    .s1("impure function foo return bar")
                    .subprogram_specification(),
                default: None,
                span: code.token_span()
            })
        );
    }

    #[test]
    fn parses_interface_subprogram_default() {
        let code = Code::new("function foo return bar is lib.name");
        assert_eq!(
            code.with_stream(parse_generic),
            InterfaceDeclaration::Subprogram(InterfaceSubprogramDeclaration {
                specification: code
                    .s1("function foo return bar")
                    .subprogram_specification(),
                default: Some(SubprogramDefault::Name(code.s1("lib.name").name())),
                span: code.token_span()
            })
        );

        let code = Code::new("function foo return bar is <>");
        assert_eq!(
            code.with_stream(parse_generic),
            InterfaceDeclaration::Subprogram(InterfaceSubprogramDeclaration {
                specification: code
                    .s1("function foo return bar")
                    .subprogram_specification(),
                default: Some(SubprogramDefault::Box),
                span: code.token_span()
            })
        );
    }

    #[test]
    fn interface_package_generic_map_aspect() {
        let code = Code::new(
            "\
package foo is new lib.pkg
     generic map (foo => bar)",
        );
        assert_eq!(
            code.with_stream(parse_generic),
            InterfaceDeclaration::Package(InterfacePackageDeclaration {
                ident: code.s1("foo").decl_ident(),
                package_name: code.s1("lib.pkg").name(),
                generic_map: InterfacePackageGenericMapAspect::Map(
                    code.s1("(foo => bar)").association_list()
                ),
                span: code.token_span()
            })
        );
    }

    #[test]
    fn interface_package_generic_map_box() {
        let code = Code::new(
            "\
package foo is new lib.pkg
     generic map (<>)",
        );
        assert_eq!(
            code.with_stream(parse_generic),
            InterfaceDeclaration::Package(InterfacePackageDeclaration {
                ident: code.s1("foo").decl_ident(),
                package_name: code.s1("lib.pkg").name(),
                generic_map: InterfacePackageGenericMapAspect::Box,
                span: code.token_span()
            })
        );
    }

    #[test]
    fn interface_package_generic_map_default() {
        let code = Code::new(
            "\
package foo is new lib.pkg
     generic map (default)",
        );
        assert_eq!(
            code.with_stream(parse_generic),
            InterfaceDeclaration::Package(InterfacePackageDeclaration {
                ident: code.s1("foo").decl_ident(),
                package_name: code.s1("lib.pkg").name(),
                generic_map: InterfacePackageGenericMapAspect::Default,
                span: code.token_span()
            })
        );
    }

    #[test]
    fn interface_declaration_cannot_be_empty() {
        let code = Code::new(
            "\
function foo() return bit;
",
        );
        let (res, diag) = code.with_partial_stream_diagnostics(parse_subprogram_declaration);
        check_diagnostics(
            diag,
            vec![Diagnostic::syntax_error(
                code.s1("()"),
                "Interface list must not be empty",
            )],
        );
        assert_eq!(
            res.expect("Expected declaration"),
            SubprogramDeclaration {
                span: code.token_span(),
                specification: SubprogramSpecification::Function(FunctionSpecification {
                    pure: true,
                    designator: code
                        .s1("foo")
                        .ident()
                        .map_into(SubprogramDesignator::Identifier)
                        .into(),
                    header: None,
                    parameter_list: Some(InterfaceList {
                        interface_type: InterfaceType::Parameter,
                        items: vec![],
                        span: code.s1("()").token_span()
                    }),
                    return_type: code.s1("bit").type_mark(),
                    span: code.s1("function foo() return bit").token_span()
                })
            }
        );
    }

    #[test]
    fn parses_bus() {
        let code = Code::new("signal foo : std_logic bus");
        assert_eq!(
            code.with_stream(parse_port),
            InterfaceDeclaration::Object(InterfaceObjectDeclaration {
                list_type: InterfaceType::Port,
                mode: ModeIndication::Simple(SimpleModeIndication {
                    bus: true,
                    mode: None,
                    class: ObjectClass::Signal,
                    subtype_indication: code.s1("std_logic").subtype_indication(),
                    expression: None
                }),
                ident: code.s1("foo").decl_ident(),
                span: code.token_span()
            })
        );
    }

    #[test]
    fn parses_view_interface_declaration() {
        let code = Code::with_standard("signal foo : view bar", VHDL2019);
        assert_eq!(
            code.with_stream(parse_port),
            InterfaceDeclaration::Object(InterfaceObjectDeclaration {
                list_type: InterfaceType::Port,
                mode: ModeIndication::View(ModeViewIndication {
                    name: code.s1("bar").name(),
                    subtype_indication: None,
                    kind: ModeViewIndicationKind::Record
                }),
                ident: code.s1("foo").decl_ident(),
                span: code.token_span()
            })
        );

        let code = Code::with_standard("signal foo : view (bar)", VHDL2019);
        assert_eq!(
            code.with_stream(parse_port),
            InterfaceDeclaration::Object(InterfaceObjectDeclaration {
                list_type: InterfaceType::Port,
                mode: ModeIndication::View(ModeViewIndication {
                    name: code.s1("bar").name(),
                    subtype_indication: None,
                    kind: ModeViewIndicationKind::Array
                }),
                ident: code.s1("foo").decl_ident(),
                span: code.token_span()
            })
        );
        let code = Code::with_standard("signal foo : view bar of baz", VHDL2019);
        assert_eq!(
            code.with_stream(parse_port),
            InterfaceDeclaration::Object(InterfaceObjectDeclaration {
                list_type: InterfaceType::Port,
                mode: ModeIndication::View(ModeViewIndication {
                    name: code.s1("bar").name(),
                    subtype_indication: Some(code.s1("baz").subtype_indication()),
                    kind: ModeViewIndicationKind::Record
                }),
                ident: code.s1("foo").decl_ident(),
                span: code.token_span()
            })
        );
        let code = Code::with_standard("signal foo : view (bar) of baz", VHDL2019);
        assert_eq!(
            code.with_stream(parse_port),
            InterfaceDeclaration::Object(InterfaceObjectDeclaration {
                list_type: InterfaceType::Port,
                mode: ModeIndication::View(ModeViewIndication {
                    name: code.s1("bar").name(),
                    subtype_indication: Some(code.s1("baz").subtype_indication()),
                    kind: ModeViewIndicationKind::Array
                }),
                ident: code.s1("foo").decl_ident(),
                span: code.token_span()
            })
        );
    }
}
