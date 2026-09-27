//! Derive macros for `vhdl-lint`.
//!
//! These are re-exported by `vhdl-lint`; depend on that crate instead of this one.

use convert_case::{Case, Casing};
use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::{format_ident, quote};
use syn::{parse_macro_input, DeriveInput, Expr, ExprLit, Lit, Meta};

mod markdown;

/// Implements `vhdl_lint::rule::Documented` from the doc comment on a rule.
///
/// The first paragraph of the doc comment is the rule's summary, and the whole
/// comment its documentation. Every `vhdl` code block is an example, tagged with
/// the construct it parses as and then whether it complies with the rule:
///
/// ````text
/// ```vhdl,design-unit,non-compliant
/// ```
/// ````
///
/// The rule's name is the kebab-case of the type's name.
#[proc_macro_derive(Documented)]
pub fn derive_documented(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    match expand(&input) {
        Ok(tokens) => tokens,
        Err(err) => {
            // Implement the trait regardless, so that the error is the only one reported
            let mut tokens = err.into_compile_error();
            tokens.extend(implementation(&input, "", "", "", Vec::new()));
            tokens
        }
    }
    .into()
}

fn implementation(
    input: &DeriveInput,
    name: &str,
    summary: &str,
    text: &str,
    examples: Vec<proc_macro2::TokenStream>,
) -> proc_macro2::TokenStream {
    let ident = &input.ident;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
    quote! {
        #[automatically_derived]
        impl #impl_generics ::vhdl_lint::rule::doc::Documented for #ident #ty_generics #where_clause {
            const DOCS: &'static ::vhdl_lint::rule::doc::RuleDocs = &::vhdl_lint::rule::doc::RuleDocs {
                name: #name,
                summary: #summary,
                text: #text,
                examples: &[#(#examples),*],
            };
        }
    }
}

fn expand(input: &DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    // Every line of the doc comment, together with the attribute it came from
    let mut lines = Vec::<String>::new();
    let mut spans = Vec::<Span>::new();
    for attr in &input.attrs {
        if !attr.path().is_ident("doc") {
            continue;
        }
        // `#[doc(hidden)]` and friends carry no text
        let Meta::NameValue(meta) = &attr.meta else {
            continue;
        };
        let Expr::Lit(ExprLit {
            lit: Lit::Str(text),
            ..
        }) = &meta.value
        else {
            return Err(syn::Error::new_spanned(
                &meta.value,
                "#[derive(Documented)] needs doc comments written as text",
            ));
        };
        for line in text.value().split('\n') {
            lines.push(line.to_owned());
            spans.push(text.span());
        }
    }

    let docs = markdown::parse(&lines).map_err(|err| {
        let span = err
            .line
            .and_then(|line| spans.get(line).copied())
            .unwrap_or_else(|| input.ident.span());
        syn::Error::new(span, err.message)
    })?;

    let name = input.ident.to_string().to_case(Case::Kebab);
    let examples = docs.examples.iter().map(|example| {
        let kind = format_ident!("{}", example.kind.variant());
        let construct = format_ident!("{}", example.construct.variant);
        let code = &example.code;
        quote! {
            ::vhdl_lint::rule::doc::Example {
                kind: ::vhdl_lint::rule::doc::ExampleKind::#kind,
                construct: ::vhdl_lint::rule::doc::Construct::#construct,
                code: #code,
            }
        }
    });

    Ok(implementation(
        input,
        &name,
        &docs.summary,
        &docs.text,
        examples.collect(),
    ))
}
