// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

/// LRM 6.5 Interface declarations
use ast::{
    InterfaceDeclaration, InterfaceFileDeclaration, InterfaceObjectDeclaration, Mode, ObjectClass,
    SubprogramDefault,
};

use message::{error, push_result, Message, MessageHandler, ParseResult};
use names::{parse_identifier_list, parse_selected_name};
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
            return Err(error(
                &file_object.ident,
                "interface_file_declaration may not have file open information",
            ));
        }
        if file_object.file_name.is_some() {
            return Err(error(
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
            return Err(error(
                &pos,
                "Interface constant declaration may only have mode=in",
            ));
        };

        if list_type == InterfaceListType::Port && object_class != ObjectClass::Signal {
            let pos = object_class_pos.as_ref().unwrap_or(&ident.pos);
            return Err(error(&pos, "Port list only allows signal object class"));
        };

        if list_type == InterfaceListType::Generic && object_class != ObjectClass::Constant {
            let pos = object_class_pos.as_ref().unwrap_or(&ident.pos);
            return Err(error(
                &pos,
                "Generic list only allows constant object class",
            ));
        };
    }

    Ok(idents
        .into_iter()
        .map(|ident| {
            InterfaceDeclaration::Object(InterfaceObjectDeclaration {
                mode: mode,
                class: object_class,
                ident: ident,
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
        }
    )
}

/// Parse ; separator in generic or port lists.
/// Expect ; for all but the last item
fn parse_semicolon_separator(stream: &mut TokenStream) -> ParseResult<()> {
    let token = stream.peek_expect()?;
    Ok(try_token_kind!(token,
                      SemiColon => {
                          stream.move_after(&token);
                          if stream.peek_expect()?.kind == RightPar {
                              return Err(error(&token,
                                           &format!("Last interface element may not end with {}",
                                                    kinds_str(&[SemiColon]))));
                          }
                      },
                      RightPar => {}
    ))
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
    use test_util::{
        with_partial_stream, with_stream, with_stream_messages, with_stream_no_messages,
    };
    use tokenizer::kinds_error;

    #[test]
    fn parses_interface_identifier_list() {
        let (util, object) = with_stream_no_messages(
            parse_generic_interface_list,
            "(constant foo, bar : natural)",
        );
        assert_eq!(
            object,
            vec![
                InterfaceDeclaration::Object(InterfaceObjectDeclaration {
                    mode: Mode::In,
                    class: ObjectClass::Constant,
                    ident: util.ident("foo"),
                    subtype_indication: util.subtype_indication("natural"),
                    expression: None
                }),
                InterfaceDeclaration::Object(InterfaceObjectDeclaration {
                    mode: Mode::In,
                    class: ObjectClass::Constant,
                    ident: util.ident("bar"),
                    subtype_indication: util.subtype_indication("natural"),
                    expression: None
                })
            ]
        );
    }

    #[test]
    fn parses_generic() {
        let (util, result) = with_stream(parse_generic, "foo : std_logic");
        assert_eq!(
            result,
            InterfaceDeclaration::Object(InterfaceObjectDeclaration {
                mode: Mode::In,
                class: ObjectClass::Constant,
                ident: util.ident("foo"),
                subtype_indication: util.subtype_indication("std_logic"),
                expression: None
            })
        );
    }

    #[test]
    fn parses_interface_file_declaration() {
        let (util, result) = with_stream(parse_parameter, "file foo : text");
        assert_eq!(
            result,
            InterfaceDeclaration::File(InterfaceFileDeclaration {
                ident: util.ident("foo"),
                subtype_indication: util.subtype_indication("text"),
            })
        );
    }

    #[test]
    fn parses_interface_file_declaration_no_open_info() {
        let (util, result) = with_partial_stream(parse_parameter, "file foo : text open read_mode");
        assert_eq!(
            result,
            Err(error(
                &util.first_substr_pos("foo"),
                "interface_file_declaration may not have file open information"
            ))
        );
    }

    #[test]
    fn parses_interface_file_declaration_no_file_name() {
        let (util, result) =
            with_partial_stream(parse_parameter, "file foo : text is \"file_name\"");
        assert_eq!(
            result,
            Err(error(
                &util.first_substr_pos("foo"),
                "interface_file_declaration may not have file name"
            ))
        );
    }

    #[test]
    fn parses_port() {
        let (util, result) = with_stream(parse_port, "foo : std_logic");
        assert_eq!(
            result,
            InterfaceDeclaration::Object(InterfaceObjectDeclaration {
                mode: Mode::In,
                class: ObjectClass::Signal,
                ident: util.ident("foo"),
                subtype_indication: util.subtype_indication("std_logic"),
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
        let (_, result) = with_stream(parse_port, "foo : std_logic");
        let result = to_interface_object(result);
        assert_eq!(result.mode, Mode::In);
        assert_eq!(result.class, ObjectClass::Signal);
    }

    #[test]
    fn parses_generic_without_explicit_class() {
        let (_, result) = with_stream(parse_generic, "foo : std_logic");
        let result = to_interface_object(result);
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
        let (_, result) = with_stream(parse_parameter, "foo : std_logic");
        let result = to_interface_object(result);
        assert_eq!(result.mode, Mode::In);
        assert_eq!(result.class, ObjectClass::Constant);

        let (_, result) = with_stream(parse_parameter, "foo : in std_logic");
        let result = to_interface_object(result);
        assert_eq!(result.mode, Mode::In);
        assert_eq!(result.class, ObjectClass::Constant);

        let (_, result) = with_stream(parse_parameter, "foo : out std_logic");
        let result = to_interface_object(result);
        assert_eq!(result.mode, Mode::Out);
        assert_eq!(result.class, ObjectClass::Variable);

        let (_, result) = with_stream(parse_parameter, "foo : inout std_logic");
        let result = to_interface_object(result);
        assert_eq!(result.mode, Mode::InOut);
        assert_eq!(result.class, ObjectClass::Variable);
    }

    #[test]
    fn parses_generic_with_optional_keyword() {
        let (util, result) = with_stream(parse_generic, "constant foo : std_logic");
        assert_eq!(
            result,
            InterfaceDeclaration::Object(InterfaceObjectDeclaration {
                mode: Mode::In,

                class: ObjectClass::Constant,
                ident: util.ident("foo"),
                subtype_indication: util.subtype_indication("std_logic"),
                expression: None
            })
        );
    }

    #[test]
    fn parses_port_with_optional_keyword() {
        let (util, result) = with_stream(parse_port, "signal foo : std_logic");
        assert_eq!(
            result,
            InterfaceDeclaration::Object(InterfaceObjectDeclaration {
                mode: Mode::In,

                class: ObjectClass::Signal,
                ident: util.ident("foo"),
                subtype_indication: util.subtype_indication("std_logic"),
                expression: None
            })
        );
    }

    #[test]
    fn parse_generic_non_in_mode_error() {
        let (util, result) = with_partial_stream(parse_generic, "foo : out boolean");
        assert_eq!(
            result,
            Err(error(
                &util.first_substr_pos("out"),
                "Interface constant declaration may only have mode=in"
            ))
        );
    }

    #[test]
    fn test_parse_generic_interface_list() {
        let (util, result) = with_stream_no_messages(
            parse_generic_interface_list,
            "\
(constant foo : std_logic;
bar : natural)",
        );

        assert_eq!(
            result,
            vec![
                util.parse_first_ok(parse_generic, "constant foo : std_logic"),
                util.parse_first_ok(parse_generic, "bar : natural")
            ]
        );
    }

    #[test]
    fn test_parse_generic_interface_list_error_on_last_semi_colon() {
        let (util, result, messages) = with_stream_messages(
            parse_generic_interface_list,
            "\
(constant foo : std_logic;
 bar : natural;
)",
        );

        assert_eq!(
            result,
            vec![
                util.parse_first_ok(parse_generic, "constant foo : std_logic"),
                util.parse_first_ok(parse_generic, "bar : natural")
            ]
        );
        assert_eq!(
            messages,
            vec![error(
                &util.substr_pos(";", 2),
                "Last interface element may not end with ';'"
            )]
        );
    }

    #[test]
    fn test_parse_port_interface_list() {
        let (util, result) = with_stream_no_messages(
            parse_port_interface_list,
            "\
(signal foo : in std_logic;
bar : natural)",
        );

        assert_eq!(
            result,
            vec![
                util.parse_first_ok(parse_port, "signal foo : in std_logic"),
                util.parse_first_ok(parse_port, "bar : natural")
            ]
        );
    }

    #[test]
    fn test_parse_parameter_interface_list() {
        let (util, result) = with_stream_no_messages(
            parse_parameter_interface_list,
            "\
(signal foo : in std_logic;
 constant bar : natural;
 variable xyz : var)",
        );

        assert_eq!(
            result,
            vec![
                util.parse_first_ok(parse_parameter, "signal foo : in std_logic"),
                util.parse_first_ok(parse_parameter, "constant bar : natural"),
                util.parse_first_ok(parse_parameter, "variable xyz : var"),
            ]
        );
    }

    #[test]
    fn test_parse_generic_interface_list_recovery_comma_instead_of_semicolon() {
        let (util, result, messages) = with_stream_messages(
            parse_generic_interface_list,
            "\
(constant c1 : natural,
 constant c2 : natural)",
        );

        assert_eq!(
            result,
            vec![
                util.parse_first_ok(parse_generic, "constant c1 : natural"),
                util.parse_first_ok(parse_generic, "constant c2 : natural"),
            ]
        );
        assert_eq!(
            messages,
            vec![kinds_error(
                &util.first_substr_pos(","),
                &[SemiColon, RightPar]
            )]
        );
    }

    #[test]
    fn test_parse_generic_interface_no_signal() {
        let (util, _, messages) =
            with_stream_messages(parse_generic_interface_list, "(signal c1 : natural)");
        assert_eq!(
            messages,
            vec![error(
                &util.first_substr_pos("signal"),
                "Generic list only allows constant object class"
            )]
        );
    }

    #[test]
    fn test_parse_port_interface_no_constant() {
        let (util, _, messages) =
            with_stream_messages(parse_port_interface_list, "(constant c1 : natural)");
        assert_eq!(
            messages,
            vec![error(
                &util.first_substr_pos("constant"),
                "Port list only allows signal object class"
            )]
        );
    }

    #[test]
    fn test_parse_generic_interface_list_recovery() {
        let (util, result, messages) = with_stream_messages(
            parse_generic_interface_list,
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

        assert_eq!(
            result,
            vec![
                util.parse_first_ok(parse_generic, "constant c2 : natural"),
                util.parse_first_ok(parse_generic, "constant c3 : natural"),
                util.parse_first_ok(parse_generic, "constant c4 : natural"),
                util.parse_first_ok(parse_generic, "constant c6 : natural")
            ]
        );
        assert_eq!(messages.len(), 4);
    }

    #[test]
    fn parses_interface_type() {
        let (util, result) = with_stream(parse_generic, "type name");
        assert_eq!(result, InterfaceDeclaration::Type(util.ident("name")));
    }

    #[test]
    fn parses_interface_subprogram() {
        let (util, result) = with_stream(parse_generic, "function foo return bar");
        assert_eq!(
            result,
            InterfaceDeclaration::Subprogram(util.subprogram_decl("function foo return bar"), None)
        );
        let (util, result) = with_stream(parse_generic, "procedure foo");
        assert_eq!(
            result,
            InterfaceDeclaration::Subprogram(util.subprogram_decl("procedure foo"), None)
        );
        let (util, result) = with_stream(parse_generic, "impure function foo return bar");
        assert_eq!(
            result,
            InterfaceDeclaration::Subprogram(
                util.subprogram_decl("impure function foo return bar"),
                None
            )
        );
    }

    #[test]
    fn parses_interface_subprogram_default() {
        let (util, result) = with_stream(parse_generic, "function foo return bar is lib.name");
        assert_eq!(
            result,
            InterfaceDeclaration::Subprogram(
                util.subprogram_decl("function foo return bar"),
                Some(SubprogramDefault::Name(util.selected_name("lib.name")))
            )
        );

        let (util, result) = with_stream(parse_generic, "function foo return bar is <>");
        assert_eq!(
            result,
            InterfaceDeclaration::Subprogram(
                util.subprogram_decl("function foo return bar"),
                Some(SubprogramDefault::Box)
            )
        );
    }

}
