// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use super::common::ParseResult;
use super::names::{parse_association_list_no_leftpar, parse_identifier_list, parse_selected_name};
use super::object_declaration::{parse_file_declaration_no_semi, parse_optional_assignment};
use super::subprogram::parse_subprogram_declaration_no_semi;
use super::subtype_indication::parse_subtype_indication;
use super::tokens::{Kind::*, *};
/// LRM 6.5 Interface declarations
use crate::ast::*;
use crate::data::*;

fn parse_optional_mode(stream: &TokenStream) -> ParseResult<Option<WithPos<Mode>>> {
    let token = stream.peek_expect()?;
    let mode = match token.kind {
        In => Mode::In,
        Out => Mode::Out,
        InOut => Mode::InOut,
        Buffer => Mode::Buffer,
        Linkage => Mode::Linkage,
        _ => return Ok(None),
    };
    stream.skip();
    Ok(Some(WithPos::new(mode, token.pos.clone())))
}

fn unexpected_object_class_kind(list_type: InterfaceType, token: &Token) -> Diagnostic {
    match list_type {
        InterfaceType::Generic => token.kinds_error(&[Constant, Identifier]),
        InterfaceType::Port => token.kinds_error(&[Signal, Identifier]),
        InterfaceType::Parameter => token.kinds_error(&[Signal, Constant, Variable, Identifier]),
    }
}

fn parse_optional_object_class(
    stream: &TokenStream,
    list_type: InterfaceType,
) -> ParseResult<Option<WithPos<ObjectClass>>> {
    let token = stream.peek_expect()?;

    let class = match token.kind {
        Constant => ObjectClass::Constant,
        Variable => ObjectClass::Variable,
        Signal => ObjectClass::Signal,
        Identifier => return Ok(None),
        _ => return Err(unexpected_object_class_kind(list_type, token)),
    };
    stream.skip();
    Ok(Some(WithPos::new(class, token.pos.clone())))
}

fn parse_interface_file_declaration(
    stream: &TokenStream,
) -> ParseResult<Vec<InterfaceDeclaration>> {
    let file_objects = parse_file_declaration_no_semi(stream)?;
    for file_object in file_objects.iter() {
        if file_object.open_info.is_some() {
            return Err(Diagnostic::error(
                &file_object.ident,
                "interface_file_declaration may not have file open information",
            ));
        }
        if file_object.file_name.is_some() {
            return Err(Diagnostic::error(
                &file_object.ident,
                "interface_file_declaration may not have file name",
            ));
        }
    }

    Ok(file_objects
        .into_iter()
        .map(|file_object| {
            InterfaceDeclaration::File(InterfaceFileDeclaration {
                ident: file_object.ident,
                subtype_indication: file_object.subtype_indication,
            })
        })
        .collect())
}

fn parse_interface_object_declaration(
    stream: &TokenStream,
    list_type: InterfaceType,
) -> ParseResult<Vec<InterfaceDeclaration>> {
    let explicit_object_class = parse_optional_object_class(stream, list_type)?;
    let object_class_pos = explicit_object_class.as_ref().map(|class| &class.pos);

    let idents = parse_identifier_list(stream)?;

    stream.expect_kind(Colon)?;

    let mode_with_pos = parse_optional_mode(stream)?;
    let mode = mode_with_pos
        .as_ref()
        .map(|mode| mode.item)
        .unwrap_or(Mode::In);
    let mode_pos = mode_with_pos.map(|mode| mode.pos);

    let object_class = match (
        list_type,
        explicit_object_class.as_ref().map(|class| class.item),
        mode,
    ) {
        (_, Some(object_class), _) => object_class,
        (InterfaceType::Port, None, _) => ObjectClass::Signal,
        (InterfaceType::Generic, None, _) => ObjectClass::Constant,
        (InterfaceType::Parameter, None, Mode::In) => ObjectClass::Constant,
        (InterfaceType::Parameter, None, _) => ObjectClass::Variable,
    };

    let subtype = parse_subtype_indication(stream)?;
    let expr = parse_optional_assignment(stream)?;

    // @TODO maybe move this to a semantic check?
    for ident in idents.iter() {
        if object_class == ObjectClass::Constant && mode != Mode::In {
            let pos = mode_pos.as_ref().unwrap_or(&ident.pos);
            return Err(Diagnostic::error(
                pos,
                "Interface constant declaration may only have mode=in",
            ));
        };

        if list_type == InterfaceType::Port && object_class != ObjectClass::Signal {
            let pos = object_class_pos.unwrap_or(&ident.pos);
            return Err(Diagnostic::error(
                pos,
                "Port list only allows signal object class",
            ));
        };

        if list_type == InterfaceType::Generic && object_class != ObjectClass::Constant {
            let pos = object_class_pos.unwrap_or(&ident.pos);
            return Err(Diagnostic::error(
                pos,
                "Generic list only allows constant object class",
            ));
        };
    }

    Ok(idents
        .into_iter()
        .map(|ident| {
            InterfaceDeclaration::Object(InterfaceObjectDeclaration {
                list_type,
                mode,
                class: object_class,
                ident: ident.into(),
                subtype_indication: subtype.clone(),
                expression: expr.clone(),
            })
        })
        .collect())
}

fn parse_subprogram_default(stream: &TokenStream) -> ParseResult<Option<SubprogramDefault>> {
    if stream.skip_if_kind(Is) {
        let default = {
            peek_token!(
                stream, token,
                Identifier => SubprogramDefault::Name(parse_selected_name(stream)?),
                BOX => {
                    stream.skip();
                    SubprogramDefault::Box
                }
            )
        };

        Ok(Some(default))
    } else {
        Ok(None)
    }
}

fn parse_interface_package(stream: &TokenStream) -> ParseResult<InterfacePackageDeclaration> {
    stream.expect_kind(Package)?;
    let ident = stream.expect_ident()?;
    stream.expect_kind(Is)?;
    stream.expect_kind(New)?;
    let package_name = parse_selected_name(stream)?;
    stream.expect_kind(Generic)?;
    stream.expect_kind(Map)?;

    let generic_map = {
        stream.expect_kind(LeftPar)?;
        let map_token = stream.peek_expect()?;
        match map_token.kind {
            BOX => {
                stream.skip();
                stream.expect_kind(RightPar)?;
                InterfacePackageGenericMapAspect::Box
            }
            Default => {
                stream.skip();
                stream.expect_kind(RightPar)?;
                InterfacePackageGenericMapAspect::Default
            }
            _ => InterfacePackageGenericMapAspect::Map(parse_association_list_no_leftpar(stream)?),
        }
    };

    Ok(InterfacePackageDeclaration {
        ident: ident.into(),
        package_name,
        generic_map,
    })
}

fn parse_interface_declaration(
    stream: &TokenStream,
    diagnostics: &mut dyn DiagnosticHandler,
    list_type: InterfaceType,
) -> ParseResult<Vec<InterfaceDeclaration>> {
    peek_token!(
        stream, token,
        Signal | Constant | Variable | Identifier => {
            parse_interface_object_declaration(stream, list_type)
        },
        File => parse_interface_file_declaration(stream),
        Type => {
            stream.skip();
            let ident = stream.expect_ident()?;
            Ok(vec![InterfaceDeclaration::Type(WithDecl::new(ident))])
        },
        Function | Procedure | Impure | Pure => {
            let decl = parse_subprogram_declaration_no_semi(stream, diagnostics)?;
            let default = parse_subprogram_default(stream)?;

            Ok(vec![InterfaceDeclaration::Subprogram(decl, default)])
        },
        Package => {
            Ok(vec![InterfaceDeclaration::Package (parse_interface_package(stream)?)])
        }
    )
}

/// Parse ; separator in generic or port lists.
/// Expect ; for all but the last item
fn parse_semicolon_separator(stream: &TokenStream) -> ParseResult<()> {
    peek_token!(
        stream, token,
        SemiColon => {
            stream.skip();
            if stream.next_kind_is(RightPar) {
                return Err(Diagnostic::error(&token.pos,
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
    stream: &TokenStream,
    diagnostics: &mut dyn DiagnosticHandler,
    list_type: InterfaceType,
) -> ParseResult<Vec<InterfaceDeclaration>> {
    let mut interface_list = Vec::new();

    stream.expect_kind(LeftPar)?;

    'outer: loop {
        let token = stream.peek_expect()?;
        match token.kind {
            RightPar => {
                stream.skip();
                break;
            }
            _ => {
                let state = stream.state();

                match parse_interface_declaration(stream, diagnostics, list_type) {
                    Ok(ref mut decl_list) => {
                        interface_list.append(decl_list);
                    }
                    Err(err) => {
                        diagnostics.push(err);
                        stream.set_state(state);
                        if let Some(token) = stream.peek() {
                            if is_sync_kind(list_type, token.kind) {
                                stream.skip();
                            }
                        }

                        // Recover
                        while let Some(token) = stream.peek() {
                            match token.kind {
                                SemiColon => {
                                    stream.skip();
                                    continue 'outer;
                                }
                                kind if is_sync_kind(list_type, kind) => {
                                    continue 'outer;
                                }
                                RightPar => {
                                    stream.skip();
                                    break 'outer;
                                }
                                _ => {
                                    stream.skip();
                                }
                            }
                        }
                    }
                }

                if let Err(err) = parse_semicolon_separator(stream) {
                    diagnostics.push(err);
                    // Ignore comma when recovering from errors
                    stream.pop_if_kind(Comma);
                }
            }
        }
    }

    Ok(interface_list)
}

pub fn parse_generic_interface_list(
    stream: &TokenStream,
    diagnostics: &mut dyn DiagnosticHandler,
) -> ParseResult<Vec<InterfaceDeclaration>> {
    parse_interface_list(stream, diagnostics, InterfaceType::Generic)
}

pub fn parse_port_interface_list(
    stream: &TokenStream,
    diagnostics: &mut dyn DiagnosticHandler,
) -> ParseResult<Vec<InterfaceDeclaration>> {
    parse_interface_list(stream, diagnostics, InterfaceType::Port)
}

pub fn parse_parameter_interface_list(
    stream: &TokenStream,
    diagnostics: &mut dyn DiagnosticHandler,
) -> ParseResult<Vec<InterfaceDeclaration>> {
    parse_interface_list(stream, diagnostics, InterfaceType::Parameter)
}

#[cfg(test)]
fn parse_one_interface_declaration(
    stream: &TokenStream,
    list_type: InterfaceType,
) -> ParseResult<InterfaceDeclaration> {
    let mut diagnostics = Vec::new();
    let result = parse_interface_declaration(stream, &mut diagnostics, list_type).map(|decls| {
        assert_eq!(decls.len(), 1);
        decls[0].clone()
    });
    assert_eq!(diagnostics, vec![]);
    result
}

#[cfg(test)]
pub fn parse_parameter(stream: &TokenStream) -> ParseResult<InterfaceDeclaration> {
    parse_one_interface_declaration(stream, InterfaceType::Parameter)
}

#[cfg(test)]
pub fn parse_port(stream: &TokenStream) -> ParseResult<InterfaceDeclaration> {
    parse_one_interface_declaration(stream, InterfaceType::Port)
}

#[cfg(test)]
pub fn parse_generic(stream: &TokenStream) -> ParseResult<InterfaceDeclaration> {
    parse_one_interface_declaration(stream, InterfaceType::Generic)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::test::Code;
    use crate::syntax::tokens::kinds_error;

    #[test]
    fn parses_interface_identifier_list() {
        let code = Code::new("(constant foo, bar : natural)");
        assert_eq!(
            code.with_stream_no_diagnostics(parse_generic_interface_list),
            vec![
                InterfaceDeclaration::Object(InterfaceObjectDeclaration {
                    list_type: InterfaceType::Generic,
                    mode: Mode::In,
                    class: ObjectClass::Constant,
                    ident: code.s1("foo").decl_ident(),
                    subtype_indication: code.s1("natural").subtype_indication(),
                    expression: None
                }),
                InterfaceDeclaration::Object(InterfaceObjectDeclaration {
                    list_type: InterfaceType::Generic,
                    mode: Mode::In,
                    class: ObjectClass::Constant,
                    ident: code.s1("bar").decl_ident(),
                    subtype_indication: code.s1("natural").subtype_indication(),
                    expression: None
                })
            ]
        );
    }

    #[test]
    fn parses_generic() {
        let code = Code::new("foo : std_logic");
        assert_eq!(
            code.with_stream(parse_generic),
            InterfaceDeclaration::Object(InterfaceObjectDeclaration {
                list_type: InterfaceType::Generic,
                mode: Mode::In,
                class: ObjectClass::Constant,
                ident: code.s1("foo").decl_ident(),
                subtype_indication: code.s1("std_logic").subtype_indication(),
                expression: None
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
            })
        );
    }

    #[test]
    fn parses_interface_file_declaration_no_open_info() {
        let code = Code::new("file foo : text open read_mode");
        assert_eq!(
            code.with_stream_err(parse_parameter),
            Diagnostic::error(
                code.s1("foo"),
                "interface_file_declaration may not have file open information"
            )
        );
    }

    #[test]
    fn parses_interface_file_declaration_no_file_name() {
        let code = Code::new("file foo : text is \"file_name\"");
        assert_eq!(
            code.with_stream_err(parse_parameter),
            Diagnostic::error(
                code.s1("foo"),
                "interface_file_declaration may not have file name"
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
                mode: Mode::In,
                class: ObjectClass::Signal,
                ident: code.s1("foo").decl_ident(),
                subtype_indication: code.s1("std_logic").subtype_indication(),
                expression: None
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
        assert_eq!(result.mode, Mode::In);
        assert_eq!(result.class, ObjectClass::Signal);
    }

    #[test]
    fn parses_generic_without_explicit_class() {
        let code = Code::new("foo : std_logic");
        let result = to_interface_object(code.with_stream(parse_generic));
        assert_eq!(result.mode, Mode::In);
        assert_eq!(result.class, ObjectClass::Constant);
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
        assert_eq!(result.mode, Mode::In);
        assert_eq!(result.class, ObjectClass::Constant);

        let code = Code::new("foo : in std_logic");
        let result = to_interface_object(code.with_stream(parse_parameter));
        assert_eq!(result.mode, Mode::In);
        assert_eq!(result.class, ObjectClass::Constant);

        let code = Code::new("foo : out std_logic");
        let result = to_interface_object(code.with_stream(parse_parameter));
        assert_eq!(result.mode, Mode::Out);
        assert_eq!(result.class, ObjectClass::Variable);

        let code = Code::new("foo : inout std_logic");
        let result = to_interface_object(code.with_stream(parse_parameter));
        assert_eq!(result.mode, Mode::InOut);
        assert_eq!(result.class, ObjectClass::Variable);
    }

    #[test]
    fn parses_generic_with_optional_keyword() {
        let code = Code::new("constant foo : std_logic");
        assert_eq!(
            code.with_stream(parse_generic),
            InterfaceDeclaration::Object(InterfaceObjectDeclaration {
                list_type: InterfaceType::Generic,
                mode: Mode::In,
                class: ObjectClass::Constant,
                ident: code.s1("foo").decl_ident(),
                subtype_indication: code.s1("std_logic").subtype_indication(),
                expression: None
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
                mode: Mode::In,
                class: ObjectClass::Signal,
                ident: code.s1("foo").decl_ident(),
                subtype_indication: code.s1("std_logic").subtype_indication(),
                expression: None
            })
        );
    }

    #[test]
    fn parse_generic_non_in_mode_error() {
        let code = Code::new("foo : out boolean");
        assert_eq!(
            code.with_partial_stream(parse_generic),
            Err(Diagnostic::error(
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
            vec![
                code.s1("constant foo : std_logic").generic(),
                code.s1("bar : natural").generic()
            ]
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
            vec![
                code.s1("constant foo : std_logic").generic(),
                code.s1("bar : natural").generic()
            ]
        );
        assert_eq!(
            diagnostics,
            vec![Diagnostic::error(
                code.s(";", 2),
                "Last interface element may not end with ';'"
            )]
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
            vec![
                code.s1("signal foo : in std_logic").port(),
                code.s1("bar : natural").port()
            ]
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
            vec![
                code.s1("signal foo : in std_logic").parameter(),
                code.s1("constant bar : natural").parameter(),
                code.s1("variable xyz : var").parameter(),
            ]
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
            vec![
                code.s1("constant c1 : natural").generic(),
                code.s1("constant c2 : natural").generic(),
            ]
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
            vec![Diagnostic::error(
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
            vec![Diagnostic::error(
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
            vec![
                code.s1("constant c2 : natural").generic(),
                code.s1("constant c3 : natural").generic(),
                code.s1("constant c4 : natural").generic(),
                code.s1("constant c6 : natural").generic()
            ]
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
            InterfaceDeclaration::Subprogram(
                code.s1("function foo return bar").subprogram_decl(),
                None
            )
        );
        let code = Code::new("procedure foo");
        assert_eq!(
            code.with_stream(parse_generic),
            InterfaceDeclaration::Subprogram(code.s1("procedure foo").subprogram_decl(), None)
        );
        let code = Code::new("impure function foo return bar");
        assert_eq!(
            code.with_stream(parse_generic),
            InterfaceDeclaration::Subprogram(
                code.s1("impure function foo return bar").subprogram_decl(),
                None
            )
        );
    }

    #[test]
    fn parses_interface_subprogram_default() {
        let code = Code::new("function foo return bar is lib.name");
        assert_eq!(
            code.with_stream(parse_generic),
            InterfaceDeclaration::Subprogram(
                code.s1("function foo return bar").subprogram_decl(),
                Some(SubprogramDefault::Name(code.s1("lib.name").selected_name()))
            )
        );

        let code = Code::new("function foo return bar is <>");
        assert_eq!(
            code.with_stream(parse_generic),
            InterfaceDeclaration::Subprogram(
                code.s1("function foo return bar").subprogram_decl(),
                Some(SubprogramDefault::Box)
            )
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
                package_name: code.s1("lib.pkg").selected_name(),
                generic_map: InterfacePackageGenericMapAspect::Map(
                    code.s1("(foo => bar)").association_list()
                )
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
                package_name: code.s1("lib.pkg").selected_name(),
                generic_map: InterfacePackageGenericMapAspect::Box
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
                package_name: code.s1("lib.pkg").selected_name(),
                generic_map: InterfacePackageGenericMapAspect::Default
            })
        );
    }
}
