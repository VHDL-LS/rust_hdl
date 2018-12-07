// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

/// LRM 6.5 Interface declarations
use ast::{
    InterfaceDeclaration, InterfaceFileDeclaration, InterfaceObjectDeclaration,
    InterfacePackageDeclaration, InterfacePackageGenericMapAspect, Mode, ObjectClass,
    SubprogramDefault,
};

use message::{push_result, Message, MessageHandler, ParseResult};
use names::{parse_association_list_no_leftpar, parse_identifier_list, parse_selected_name};
use object_declaration::{parse_file_declaration_no_semi, parse_optional_assignment};
use subprogram::parse_subprogram_declaration_no_semi;
use subtype_indication::parse_subtype_indication;
use tokenizer::Kind::*;
use tokenizer::{kinds_str, Kind, Token};
use tokenstream::TokenStream;

fn parse_optional_mode(stream: &mut TokenStream) -> ParseResult<Option<Mode>> {
    Ok(match stream.peek_kind()? {
        Some(In) => Some(Mode::In),
        Some(Out) => Some(Mode::Out),
        Some(InOut) => Some(Mode::InOut),
        Some(Buffer) => Some(Mode::Buffer),
        Some(Linkage) => Some(Mode::Linkage),
        _ => None,
    })
}

fn unexpected_object_class_kind(list_type: InterfaceListType, token: &Token) -> Message {
    match list_type {
        InterfaceListType::Generic => token.kinds_error(&[Constant, Identifier]),
        InterfaceListType::Port => token.kinds_error(&[Signal, Identifier]),
        InterfaceListType::Parameter => {
            token.kinds_error(&[Signal, Constant, Variable, Identifier])
        }
    }
}

fn parse_optional_object_class(
    stream: &mut TokenStream,
    list_type: InterfaceListType,
) -> ParseResult<Option<ObjectClass>> {
    let token = stream.peek_expect()?;

    match token.kind {
        Constant => Ok(Some(ObjectClass::Constant)),
        Variable => Ok(Some(ObjectClass::Variable)),
        Signal => Ok(Some(ObjectClass::Signal)),
        Identifier => Ok(None),
        _ => Err(unexpected_object_class_kind(list_type, &token)),
    }
}

fn parse_interface_file_declaration(
    stream: &mut TokenStream,
) -> ParseResult<Vec<InterfaceDeclaration>> {
    let file_objects = parse_file_declaration_no_semi(stream)?;
    for file_object in file_objects.iter() {
        if file_object.open_info.is_some() {
            return Err(Message::error(
                &file_object.ident,
                "interface_file_declaration may not have file open information",
            ));
        }
        if file_object.file_name.is_some() {
            return Err(Message::error(
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
        }).collect())
}

fn parse_interface_object_declaration(
    stream: &mut TokenStream,
    list_type: InterfaceListType,
) -> ParseResult<Vec<InterfaceDeclaration>> {
    let explicit_object_class = parse_optional_object_class(stream, list_type)?;
    let object_class_pos = match explicit_object_class {
        Some(_) => stream.pop()?.map(|tok| tok.pos.clone()),
        None => None,
    };
    let idents = parse_identifier_list(stream)?;

    stream.expect_kind(Colon)?;

    let mode = parse_optional_mode(stream)?;

    let mode_pos = match mode {
        Some(_) => stream.pop()?.map(|tok| tok.pos.clone()),
        None => None,
    };

    let mode = mode.unwrap_or(Mode::In);

    let object_class = match (list_type, explicit_object_class, mode) {
        (_, Some(object_class), _) => object_class,
        (InterfaceListType::Port, None, _) => ObjectClass::Signal,
        (InterfaceListType::Generic, None, _) => ObjectClass::Constant,
        (InterfaceListType::Parameter, None, Mode::In) => ObjectClass::Constant,
        (InterfaceListType::Parameter, None, _) => ObjectClass::Variable,
    };

    let subtype = parse_subtype_indication(stream)?;
    let expr = parse_optional_assignment(stream)?;

    // @TODO maybe move this to a semantic check?
    for ident in idents.iter() {
        if object_class == ObjectClass::Constant && mode != Mode::In {
            let pos = mode_pos.as_ref().unwrap_or(&ident.pos);
            return Err(Message::error(
                &pos,
                "Interface constant declaration may only have mode=in",
            ));
        };

        if list_type == InterfaceListType::Port && object_class != ObjectClass::Signal {
            let pos = object_class_pos.as_ref().unwrap_or(&ident.pos);
            return Err(Message::error(
                &pos,
                "Port list only allows signal object class",
            ));
        };

        if list_type == InterfaceListType::Generic && object_class != ObjectClass::Constant {
            let pos = object_class_pos.as_ref().unwrap_or(&ident.pos);
            return Err(Message::error(
                &pos,
                "Generic list only allows constant object class",
            ));
        };
    }

    Ok(idents
        .into_iter()
        .map(|ident| {
            InterfaceDeclaration::Object(InterfaceObjectDeclaration {
                mode,
                class: object_class,
                ident,
                subtype_indication: subtype.clone(),
                expression: expr.clone(),
            })
        }).collect())
}

fn parse_subprogram_default(stream: &mut TokenStream) -> ParseResult<Option<SubprogramDefault>> {
    if stream.skip_if_kind(Is)? {
        let token = stream.peek_expect()?;
        let default = {
            try_token_kind!(
                token,
                Identifier => SubprogramDefault::Name(parse_selected_name(stream)?),
                BOX => {
                    stream.move_after(&token);
                    SubprogramDefault::Box
                }
            )
        };

        Ok(Some(default))
    } else {
        Ok(None)
    }
}

fn parse_interface_package_declaration_known_keyword(
    stream: &mut TokenStream,
) -> ParseResult<InterfacePackageDeclaration> {
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
                stream.move_after(&map_token);
                stream.expect_kind(RightPar)?;
                InterfacePackageGenericMapAspect::Box
            }
            Default => {
                stream.move_after(&map_token);
                stream.expect_kind(RightPar)?;
                InterfacePackageGenericMapAspect::Default
            }
            _ => InterfacePackageGenericMapAspect::Map(parse_association_list_no_leftpar(stream)?),
        }
    };

    Ok(InterfacePackageDeclaration {
        ident,
        package_name,
        generic_map,
    })
}

fn parse_interface_declaration(
    stream: &mut TokenStream,
    messages: &mut MessageHandler,
    list_type: InterfaceListType,
) -> ParseResult<Vec<InterfaceDeclaration>> {
    let token = stream.peek_expect()?;

    match_token_kind!(
        token,
        Signal | Constant | Variable | Identifier => {
            parse_interface_object_declaration(stream, list_type)
        },
        File => parse_interface_file_declaration(stream),
        Type => {
            stream.move_after(&token);
            let ident = stream.expect_ident()?;
            Ok(vec![InterfaceDeclaration::Type(ident)])
        },
        Function | Procedure | Impure => {
            let decl = parse_subprogram_declaration_no_semi(stream, messages)?;
            let default = parse_subprogram_default(stream)?;

            Ok(vec![InterfaceDeclaration::Subprogram(decl, default)])
        },
        Package => {
            stream.move_after(&token);
            Ok(vec![InterfaceDeclaration::Package (parse_interface_package_declaration_known_keyword(stream)?)])
        }
    )
}

/// Parse ; separator in generic or port lists.
/// Expect ; for all but the last item
fn parse_semicolon_separator(stream: &mut TokenStream) -> ParseResult<()> {
    let token = stream.peek_expect()?;
    try_token_kind!(token,
                      SemiColon => {
                          stream.move_after(&token);
                          if stream.peek_expect()?.kind == RightPar {
                              return Err(Message::error(&token,
                                                        format!("Last interface element may not end with {}",
                                                    kinds_str(&[SemiColon]))));
                          }
                      },
                      RightPar => {}
    );
    Ok(())
}

#[derive(PartialEq, Clone, Copy)]
enum InterfaceListType {
    Port,
    Generic,
    Parameter,
}

fn is_sync_kind(list_type: InterfaceListType, kind: Kind) -> bool {
    match (list_type, kind) {
        (InterfaceListType::Generic, Constant)
        | (InterfaceListType::Port, Signal)
        | (InterfaceListType::Parameter, Constant)
        | (InterfaceListType::Parameter, Variable)
        | (InterfaceListType::Parameter, Signal) => true,
        _ => false,
    }
}

fn parse_interface_list(
    stream: &mut TokenStream,
    messages: &mut MessageHandler,
    list_type: InterfaceListType,
) -> ParseResult<Vec<InterfaceDeclaration>> {
    let mut interface_list = Vec::new();

    stream.expect_kind(LeftPar)?;

    'outer: loop {
        let token = stream.peek_expect()?;
        match token.kind {
            RightPar => {
                stream.move_after(&token);
                break;
            }
            _ => {
                let state = stream.state();

                match parse_interface_declaration(stream, messages, list_type) {
                    Ok(ref mut decl_list) => {
                        interface_list.append(decl_list);
                    }
                    Err(err) => {
                        messages.push(err);
                        stream.set_state(state);
                        if let Some(token) = stream.peek()? {
                            if is_sync_kind(list_type, token.kind) {
                                stream.move_after(&token);
                            }
                        }

                        // Recover
                        while let Some(token) = stream.peek()? {
                            match token.kind {
                                SemiColon => {
                                    stream.move_after(&token);
                                    continue 'outer;
                                }
                                kind if is_sync_kind(list_type, kind) => {
                                    continue 'outer;
                                }
                                RightPar => {
                                    stream.move_after(&token);
                                    break 'outer;
                                }
                                _ => {
                                    stream.move_after(&token);
                                }
                            }
                        }
                    }
                }

                if let Err(err) = parse_semicolon_separator(stream) {
                    messages.push(err);
                    // Ignore comma when recovering from errors
                    push_result(messages, stream.pop_if_kind(Comma));
                }
            }
        }
    }

    Ok(interface_list)
}

pub fn parse_generic_interface_list(
    stream: &mut TokenStream,
    messages: &mut MessageHandler,
) -> ParseResult<Vec<InterfaceDeclaration>> {
    parse_interface_list(stream, messages, InterfaceListType::Generic)
}

pub fn parse_port_interface_list(
    stream: &mut TokenStream,
    messages: &mut MessageHandler,
) -> ParseResult<Vec<InterfaceDeclaration>> {
    parse_interface_list(stream, messages, InterfaceListType::Port)
}

pub fn parse_parameter_interface_list(
    stream: &mut TokenStream,
    messages: &mut MessageHandler,
) -> ParseResult<Vec<InterfaceDeclaration>> {
    parse_interface_list(stream, messages, InterfaceListType::Parameter)
}

#[cfg(test)]
fn parse_one_interface_declaration(
    stream: &mut TokenStream,
    list_type: InterfaceListType,
) -> ParseResult<InterfaceDeclaration> {
    let mut messages = Vec::new();
    let result = parse_interface_declaration(stream, &mut messages, list_type).map(|decls| {
        assert_eq!(decls.len(), 1);
        decls[0].clone()
    });
    assert_eq!(messages, vec![]);
    result
}

#[cfg(test)]
pub fn parse_parameter(stream: &mut TokenStream) -> ParseResult<InterfaceDeclaration> {
    parse_one_interface_declaration(stream, InterfaceListType::Parameter)
}

#[cfg(test)]
pub fn parse_port(stream: &mut TokenStream) -> ParseResult<InterfaceDeclaration> {
    parse_one_interface_declaration(stream, InterfaceListType::Port)
}

#[cfg(test)]
pub fn parse_generic(stream: &mut TokenStream) -> ParseResult<InterfaceDeclaration> {
    parse_one_interface_declaration(stream, InterfaceListType::Generic)
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_util::Code;
    use tokenizer::kinds_error;

    #[test]
    fn parses_interface_identifier_list() {
        let code = Code::new("(constant foo, bar : natural)");
        assert_eq!(
            code.with_stream_no_messages(parse_generic_interface_list),
            vec![
                InterfaceDeclaration::Object(InterfaceObjectDeclaration {
                    mode: Mode::In,
                    class: ObjectClass::Constant,
                    ident: code.s1("foo").ident(),
                    subtype_indication: code.s1("natural").subtype_indication(),
                    expression: None
                }),
                InterfaceDeclaration::Object(InterfaceObjectDeclaration {
                    mode: Mode::In,
                    class: ObjectClass::Constant,
                    ident: code.s1("bar").ident(),
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
                mode: Mode::In,
                class: ObjectClass::Constant,
                ident: code.s1("foo").ident(),
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
                ident: code.s1("foo").ident(),
                subtype_indication: code.s1("text").subtype_indication(),
            })
        );
    }

    #[test]
    fn parses_interface_file_declaration_no_open_info() {
        let code = Code::new("file foo : text open read_mode");
        assert_eq!(
            code.with_stream_err(parse_parameter),
            Message::error(
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
            Message::error(
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
                mode: Mode::In,
                class: ObjectClass::Signal,
                ident: code.s1("foo").ident(),
                subtype_indication: code.s1("std_logic").subtype_indication(),
                expression: None
            })
        );
    }

    fn to_interface_object(interface_decl: InterfaceDeclaration) -> InterfaceObjectDeclaration {
        match interface_decl {
            InterfaceDeclaration::Object(object) => object,
            _ => panic!("{:?}", interface_decl),
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
                mode: Mode::In,

                class: ObjectClass::Constant,
                ident: code.s1("foo").ident(),
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
                mode: Mode::In,

                class: ObjectClass::Signal,
                ident: code.s1("foo").ident(),
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
            Err(Message::error(
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
            code.with_stream_no_messages(parse_generic_interface_list),
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
        let (result, messages) = code.with_stream_messages(parse_generic_interface_list);

        assert_eq!(
            result,
            vec![
                code.s1("constant foo : std_logic").generic(),
                code.s1("bar : natural").generic()
            ]
        );
        assert_eq!(
            messages,
            vec![Message::error(
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
            code.with_stream_no_messages(parse_port_interface_list),
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
            code.with_stream_no_messages(parse_parameter_interface_list),
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

        let (result, messages) = code.with_stream_messages(parse_generic_interface_list);
        assert_eq!(
            result,
            vec![
                code.s1("constant c1 : natural").generic(),
                code.s1("constant c2 : natural").generic(),
            ]
        );
        assert_eq!(
            messages,
            vec![kinds_error(code.s1(","), &[SemiColon, RightPar])]
        );
    }

    #[test]
    fn test_parse_generic_interface_no_signal() {
        let code = Code::new("(signal c1 : natural)");
        let (_, messages) = code.with_stream_messages(parse_generic_interface_list);
        assert_eq!(
            messages,
            vec![Message::error(
                code.s1("signal"),
                "Generic list only allows constant object class"
            )]
        );
    }

    #[test]
    fn test_parse_port_interface_no_constant() {
        let code = Code::new("(constant c1 : natural)");
        let (_, messages) = code.with_stream_messages(parse_port_interface_list);
        assert_eq!(
            messages,
            vec![Message::error(
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

        let (result, messages) = code.with_stream_messages(parse_generic_interface_list);
        assert_eq!(
            result,
            vec![
                code.s1("constant c2 : natural").generic(),
                code.s1("constant c3 : natural").generic(),
                code.s1("constant c4 : natural").generic(),
                code.s1("constant c6 : natural").generic()
            ]
        );
        assert_eq!(messages.len(), 4);
    }

    #[test]
    fn parses_interface_type() {
        let code = Code::new("type name");
        assert_eq!(
            code.with_stream(parse_generic),
            InterfaceDeclaration::Type(code.s1("name").ident())
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
                ident: code.s1("foo").ident(),
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
                ident: code.s1("foo").ident(),
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
                ident: code.s1("foo").ident(),
                package_name: code.s1("lib.pkg").selected_name(),
                generic_map: InterfacePackageGenericMapAspect::Default
            })
        );
    }

}
