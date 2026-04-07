use crate::vhdl_server::{from_lsp_range, uri_to_file_name, VHDLServer};
use lsp_types::*;
use vhdl_lang::ast::ExternalObjectClass;
use vhdl_lang::{AnyEntKind, Concurrent, Object, Overloaded, Type};

/// Generates token type index constants and the TOKEN_TYPES legend array
/// from a single declaration, keeping the two in sync automatically.
macro_rules! define_token_types {
    ( $( ($const:ident = $lsp_type:expr) ),+ $(,)? ) => {
        define_token_types!(@consts 0, $( $const, )+);

        pub const TOKEN_TYPES: &[SemanticTokenType] = &[
            $( $lsp_type, )+
        ];
    };

    // Base case
    (@consts $idx:expr, ) => {};
    // Recursive case: assign current index, increment for the rest
    (@consts $idx:expr, $const:ident, $( $rest:ident, )*) => {
        const $const: u32 = $idx;
        define_token_types!(@consts ($idx + 1), $( $rest, )*);
    };
}

define_token_types! {
    (VARIABLE    = SemanticTokenType::VARIABLE),    // signals, variables, constants, files
    (PARAMETER   = SemanticTokenType::PARAMETER),   // subprogram parameters
    (PROPERTY    = SemanticTokenType::PROPERTY),     // attributes, record fields
    (ENUM_MEMBER = SemanticTokenType::ENUM_MEMBER),  // enum literals
    (FUNCTION    = SemanticTokenType::FUNCTION),     // functions, procedures
    (TYPE        = SemanticTokenType::TYPE),          // types (general)
    (CLASS       = SemanticTokenType::CLASS),         // protected types, components
    (NAMESPACE   = SemanticTokenType::NAMESPACE),    // libraries, design units, labels
    (STRUCT      = SemanticTokenType::STRUCT),        // record types
    (ENUM        = SemanticTokenType::ENUM),          // enum types
}

// Semantic token modifier bits
const MOD_READONLY: u32 = 1 << 0;

pub const TOKEN_MODIFIERS: &[SemanticTokenModifier] = &[
    SemanticTokenModifier::READONLY, // bit 0: constants, generics
];

/// Classification of a VHDL entity into an LSP semantic token.
struct TokenClassification {
    token_type: u32,
    modifiers: u32,
}

/// A resolved semantic token ready for caching and encoding.
pub(crate) struct CachedToken {
    pub range: vhdl_lang::Range,
    pub token_type: u32,
    pub modifiers: u32,
}

fn object_token(obj: &Object) -> TokenClassification {
    if obj.is_param() {
        return TokenClassification {
            token_type: PARAMETER,
            modifiers: 0,
        };
    }
    if obj.is_generic() || obj.is_constant() {
        return TokenClassification {
            token_type: VARIABLE,
            modifiers: MOD_READONLY,
        };
    }
    TokenClassification {
        token_type: VARIABLE,
        modifiers: 0,
    }
}

fn overloaded_token(o: &Overloaded) -> TokenClassification {
    match o {
        Overloaded::EnumLiteral(_) => TokenClassification {
            token_type: ENUM_MEMBER,
            modifiers: 0,
        },
        Overloaded::Alias(inner) => overloaded_token(inner.kind()),
        _ => TokenClassification {
            token_type: FUNCTION,
            modifiers: 0,
        },
    }
}

fn type_token(t: &Type) -> TokenClassification {
    match t {
        Type::Enum(_) => TokenClassification {
            token_type: ENUM,
            modifiers: 0,
        },
        Type::Record(_) => TokenClassification {
            token_type: STRUCT,
            modifiers: 0,
        },
        Type::Protected(..) => TokenClassification {
            token_type: CLASS,
            modifiers: 0,
        },
        Type::Subtype(sub) => type_token(sub.type_mark().kind()),
        Type::Alias(t) => type_token(t.kind()),
        _ => TokenClassification {
            token_type: TYPE,
            modifiers: 0,
        },
    }
}

fn classify(kind: &AnyEntKind) -> Option<TokenClassification> {
    let result = match kind {
        AnyEntKind::Object(obj) => object_token(obj),
        AnyEntKind::DeferredConstant(_)
        | AnyEntKind::LoopParameter(_)
        | AnyEntKind::PhysicalLiteral(_) => TokenClassification {
            token_type: VARIABLE,
            modifiers: MOD_READONLY,
        },
        AnyEntKind::Overloaded(o) => overloaded_token(o),
        AnyEntKind::Type(t) => type_token(t),
        AnyEntKind::Component(_) => TokenClassification {
            token_type: CLASS,
            modifiers: 0,
        },
        AnyEntKind::Attribute(_) | AnyEntKind::ElementDeclaration(_) => TokenClassification {
            token_type: PROPERTY,
            modifiers: 0,
        },
        AnyEntKind::Library | AnyEntKind::Design(_) => TokenClassification {
            token_type: NAMESPACE,
            modifiers: 0,
        },
        AnyEntKind::View(_) => TokenClassification {
            token_type: TYPE,
            modifiers: 0,
        },
        AnyEntKind::File(_) | AnyEntKind::InterfaceFile(_) => TokenClassification {
            token_type: VARIABLE,
            modifiers: 0,
        },
        AnyEntKind::ObjectAlias { base_object, .. } => object_token(base_object.object()),
        AnyEntKind::ExternalAlias { class, .. } => match class {
            ExternalObjectClass::Constant => TokenClassification {
                token_type: VARIABLE,
                modifiers: MOD_READONLY,
            },
            _ => TokenClassification {
                token_type: VARIABLE,
                modifiers: 0,
            },
        },
        AnyEntKind::Concurrent(Some(Concurrent::Instance), _) => TokenClassification {
            token_type: CLASS,
            modifiers: 0,
        },
        AnyEntKind::Concurrent(..) | AnyEntKind::Sequential(..) => return None,
    };
    Some(result)
}

/// Map and sort raw tokens from the AST walk into cacheable form.
fn map_and_sort(
    mut raw_tokens: Vec<(vhdl_lang::SrcPos, vhdl_lang::EntRef<'_>)>,
) -> Vec<CachedToken> {
    raw_tokens.sort_by(|(pos_a, _), (pos_b, _)| pos_a.cmp(pos_b));

    raw_tokens
        .into_iter()
        .filter_map(|(pos, ent)| {
            let cls = classify(ent.kind())?;
            Some(CachedToken {
                range: pos.range(),
                token_type: cls.token_type,
                modifiers: cls.modifiers,
            })
        })
        .collect()
}

/// Delta-encode sorted tokens, optionally filtering to a range.
fn encode(tokens: &[CachedToken], range_filter: Option<&vhdl_lang::Range>) -> Vec<SemanticToken> {
    let mut semantic_tokens = Vec::with_capacity(tokens.len());
    let mut prev_line = 0u32;
    let mut prev_start = 0u32;

    for token in tokens {
        if let Some(filter) = range_filter {
            if !token.range.overlaps_lines(filter) {
                continue;
            }
        }

        let line = token.range.start.line;
        let start = token.range.start.character;
        if token.range.start.line != token.range.end.line {
            continue; // Skip multi-line tokens; identifiers never span lines
        }
        let length = token.range.end.character - token.range.start.character;

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
            token_type: token.token_type,
            token_modifiers_bitset: token.modifiers,
        });

        prev_line = line;
        prev_start = start;
    }

    semantic_tokens
}

impl VHDLServer {
    /// Get or compute the cached semantic tokens for a file.
    fn cached_semantic_tokens(&mut self, uri: &Url) -> Option<&[CachedToken]> {
        if !self.semantic_token_cache.contains_key(uri) {
            let source = self.project.get_source(&uri_to_file_name(uri))?;
            let raw_tokens = self.project.find_all_entity_references(&source);
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
