use crate::ast::{IdentList, NameList, SeparatedList, WithRef};
use crate::data::DiagnosticResult;
use crate::syntax::common::ParseResult;
use crate::syntax::names::parse_name;
use crate::syntax::Kind::Comma;
use crate::syntax::{Kind, TokenStream};

/// Parses a list of the form
///   `element { separator element }`
/// where `element` is an AST element and `separator` is a token of some `ast::Kind`.
/// The returned list retains information of the whereabouts of the separator tokens.
pub fn parse_list_with_separator<F, T>(
    stream: &TokenStream,
    separator: Kind,
    parse_fn: F,
) -> DiagnosticResult<SeparatedList<T>>
where
    F: Fn(&TokenStream) -> ParseResult<T>,
{
    let first = parse_fn(stream)?;
    let mut remainder = Vec::new();
    while let Some(separator) = stream.pop_if_kind(separator) {
        remainder.push((separator, parse_fn(stream)?));
    }
    Ok(SeparatedList { first, remainder })
}

pub fn parse_name_list(stream: &TokenStream) -> DiagnosticResult<NameList> {
    parse_list_with_separator(stream, Comma, parse_name)
}

pub fn parse_ident_list(stream: &TokenStream) -> DiagnosticResult<IdentList> {
    parse_list_with_separator(stream, Comma, |stream| {
        stream.expect_ident().map(WithRef::new)
    })
}

#[cfg(test)]
mod test {
    use crate::ast::{IdentList, NameList};
    use crate::syntax::separated_list::{parse_ident_list, parse_name_list};
    use crate::syntax::test::Code;
    use assert_matches::assert_matches;

    #[test]
    pub fn test_error_on_empty_list() {
        let code = Code::new("");
        assert_matches!(code.parse(parse_ident_list), Err(_))
    }

    #[test]
    pub fn parse_single_element_list() {
        let code = Code::new("abc");
        assert_eq!(
            code.parse_ok(parse_ident_list),
            IdentList {
                first: code.s1("abc").ident().into_ref(),
                remainder: vec![]
            }
        )
    }

    #[test]
    pub fn parse_list_with_multiple_elements() {
        let code = Code::new("abc, def, ghi");
        assert_eq!(
            code.parse_ok(parse_ident_list),
            IdentList {
                first: code.s1("abc").ident().into_ref(),
                remainder: vec![
                    (code.s(",", 1).token(), code.s1("def").ident().into_ref()),
                    (code.s(",", 2).token(), code.s1("ghi").ident().into_ref()),
                ]
            }
        )
    }

    #[test]
    fn parse_list_with_many_names() {
        let code = Code::new("work.foo, lib.bar.all");
        assert_eq!(
            code.parse_ok(parse_name_list),
            NameList {
                first: code.s1("work.foo").name(),
                remainder: vec![(code.s1(",").token(), code.s1("lib.bar.all").name())]
            }
        )
    }
}
