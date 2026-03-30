use crate::vhdl_server::{from_lsp_range, uri_to_file_name, VHDLServer};
use lsp_types::*;
use vhdl_lang::ast::ExternalObjectClass;
use vhdl_lang::{AnyEntKind, Concurrent, Object, Overloaded, Type};

// Semantic token type indices — order must match TOKEN_TYPES
const VARIABLE: u32 = 0;
const PARAMETER: u32 = 1;
const PROPERTY: u32 = 2;
const ENUM_MEMBER: u32 = 3;
const FUNCTION: u32 = 4;
const TYPE: u32 = 5;
const CLASS: u32 = 6;
const NAMESPACE: u32 = 7;
const STRUCT: u32 = 8;
const ENUM: u32 = 9;

// Semantic token modifier bits
const MOD_READONLY: u32 = 1 << 0;

pub const TOKEN_TYPES: &[SemanticTokenType] = &[
    SemanticTokenType::VARIABLE,    // 0: signals, variables, constants, files
    SemanticTokenType::PARAMETER,   // 1: subprogram parameters
    SemanticTokenType::PROPERTY,    // 2: attributes, record fields
    SemanticTokenType::ENUM_MEMBER, // 3: enum literals
    SemanticTokenType::FUNCTION,    // 4: functions, procedures
    SemanticTokenType::TYPE,        // 5: types (general)
    SemanticTokenType::CLASS,       // 6: protected types, components
    SemanticTokenType::NAMESPACE,   // 7: libraries, design units, labels
    SemanticTokenType::STRUCT,      // 8: record types
    SemanticTokenType::ENUM,        // 9: enum types
];

pub const TOKEN_MODIFIERS: &[SemanticTokenModifier] = &[
    SemanticTokenModifier::READONLY, // bit 0: constants, generics
];

fn object_token(obj: &Object) -> (u32, u32) {
    if obj.is_param() {
        return (PARAMETER, 0);
    }
    if obj.is_generic() || obj.is_constant() {
        return (VARIABLE, MOD_READONLY);
    }
    (VARIABLE, 0)
}

fn overloaded_token(o: &Overloaded) -> (u32, u32) {
    match o {
        Overloaded::EnumLiteral(_) => (ENUM_MEMBER, 0),
        Overloaded::Alias(inner) => overloaded_token(inner.kind()),
        _ => (FUNCTION, 0),
    }
}

fn type_token(t: &Type) -> (u32, u32) {
    match t {
        Type::Enum(_) => (ENUM, 0),
        Type::Record(_) => (STRUCT, 0),
        Type::Protected(..) => (CLASS, 0),
        Type::Subtype(sub) => type_token(sub.type_mark().kind()),
        Type::Alias(t) => type_token(t.kind()),
        _ => (TYPE, 0),
    }
}

fn to_semantic_token(kind: &AnyEntKind) -> Option<(u32, u32)> {
    let result = match kind {
        AnyEntKind::Object(obj) => object_token(obj),
        AnyEntKind::DeferredConstant(_)
        | AnyEntKind::LoopParameter(_)
        | AnyEntKind::PhysicalLiteral(_) => (VARIABLE, MOD_READONLY),
        AnyEntKind::Overloaded(o) => overloaded_token(o),
        AnyEntKind::Type(t) => type_token(t),
        AnyEntKind::Component(_) => (CLASS, 0),
        AnyEntKind::Attribute(_) | AnyEntKind::ElementDeclaration(_) => (PROPERTY, 0),
        AnyEntKind::Library | AnyEntKind::Design(_) => (NAMESPACE, 0),
        AnyEntKind::View(_) => (TYPE, 0),
        AnyEntKind::File(_) | AnyEntKind::InterfaceFile(_) => (VARIABLE, 0),
        AnyEntKind::ObjectAlias { base_object, .. } => object_token(base_object.object()),
        AnyEntKind::ExternalAlias { class, .. } => match class {
            ExternalObjectClass::Constant => (VARIABLE, MOD_READONLY),
            _ => (VARIABLE, 0),
        },
        AnyEntKind::Concurrent(Some(Concurrent::Instance), _) => (CLASS, 0),
        AnyEntKind::Concurrent(..) | AnyEntKind::Sequential(..) => return None,
    };
    Some(result)
}

/// Check if a token overlaps the filter range by line.
/// Character-level precision is not needed as clients request full-line ranges.
fn in_range(token_range: &vhdl_lang::Range, filter: &vhdl_lang::Range) -> bool {
    token_range.start.line <= filter.end.line && token_range.end.line >= filter.start.line
}

/// Map and sort raw tokens from the AST walk into cacheable form.
fn map_and_sort(
    raw_tokens: Vec<(vhdl_lang::SrcPos, vhdl_lang::EntRef<'_>)>,
) -> Vec<(vhdl_lang::Range, u32, u32)> {
    let mut tokens: Vec<_> = raw_tokens
        .into_iter()
        .filter_map(|(pos, ent)| {
            let (token_type, token_modifiers) = to_semantic_token(ent.kind())?;
            let range = pos.range();
            Some((range, token_type, token_modifiers))
        })
        .collect();

    tokens.sort_by(|a, b| {
        a.0.start
            .line
            .cmp(&b.0.start.line)
            .then(a.0.start.character.cmp(&b.0.start.character))
    });

    tokens
}

/// Delta-encode sorted tokens, optionally filtering to a range.
fn encode(
    tokens: &[(vhdl_lang::Range, u32, u32)],
    range_filter: Option<&vhdl_lang::Range>,
) -> Vec<SemanticToken> {
    let mut semantic_tokens = Vec::with_capacity(tokens.len());
    let mut prev_line = 0u32;
    let mut prev_start = 0u32;

    for (range, token_type, token_modifiers) in tokens {
        if let Some(filter) = range_filter {
            if !in_range(range, filter) {
                continue;
            }
        }

        let line = range.start.line;
        let start = range.start.character;
        if range.start.line != range.end.line {
            continue; // Skip multi-line tokens; identifiers never span lines
        }
        let length = range.end.character - range.start.character;

        let delta_line = line - prev_line;
        let delta_start = if delta_line == 0 {
            start - prev_start
        } else {
            start
        };

        semantic_tokens.push(SemanticToken {
            delta_line,
            delta_start,
            length,
            token_type: *token_type,
            token_modifiers_bitset: *token_modifiers,
        });

        prev_line = line;
        prev_start = start;
    }

    semantic_tokens
}

impl VHDLServer {
    /// Get or compute the cached semantic tokens for a file.
    fn cached_semantic_tokens(&mut self, uri: &Url) -> Option<&[(vhdl_lang::Range, u32, u32)]> {
        if !self.semantic_token_cache.contains_key(uri) {
            let source = self.project.get_source(&uri_to_file_name(uri))?;
            let raw_tokens = self.project.semantic_tokens(&source);
            let tokens = map_and_sort(raw_tokens);
            self.semantic_token_cache.insert(uri.clone(), tokens);
        }
        self.semantic_token_cache.get(uri).map(|v| v.as_slice())
    }

    pub fn semantic_tokens_full(
        &mut self,
        params: &SemanticTokensParams,
    ) -> Option<SemanticTokensResult> {
        let tokens = self.cached_semantic_tokens(&params.text_document.uri)?;
        let data = encode(tokens, None);

        Some(SemanticTokensResult::Tokens(SemanticTokens {
            result_id: None,
            data,
        }))
    }

    pub fn semantic_tokens_range(
        &mut self,
        params: &SemanticTokensRangeParams,
    ) -> Option<SemanticTokensRangeResult> {
        let filter = from_lsp_range(params.range);
        let tokens = self.cached_semantic_tokens(&params.text_document.uri)?;
        let data = encode(tokens, Some(&filter));

        Some(SemanticTokensRangeResult::Tokens(SemanticTokens {
            result_id: None,
            data,
        }))
    }
}
