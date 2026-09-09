//! Collects rules and indexes them by the node kinds they apply to.
//!
//! A registry is built once, before any file is read, and then shared immutably
//! across the threads that check files. Nothing here runs a rule; it only answers
//! which rules exist and which of them are interested in a given [`NodeKind`].

use std::collections::HashMap;
use std::fmt;

use vhdl_syntax::syntax::NodeKind;

use crate::error_code::ErrorCode;
use crate::rule::{AstRule, ErasedAstRule};

/// The ways registering a rule can fail.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RegisterError {
    /// A rule with this code is already registered.
    DuplicateCode(ErrorCode),
    /// the error code is not configurable
    NotConfigurable(ErrorCode),
}

impl fmt::Display for RegisterError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RegisterError::DuplicateCode(code) => {
                write!(f, "a rule with code {code} is already registered")
            }
            RegisterError::NotConfigurable(code) => {
                write!(
                    f,
                    "{code} is in category '{}', which is not configurable",
                    code.category()
                )
            }
        }
    }
}

impl std::error::Error for RegisterError {}

/// A set of rules, indexed by the node kinds they apply to.
#[derive(Default)]
pub struct RuleRegistry {
    rules: Vec<Box<dyn ErasedAstRule>>,
    by_kind: HashMap<NodeKind, Vec<usize>>,
    by_code: HashMap<ErrorCode, usize>,
}

impl RuleRegistry {
    pub fn new() -> RuleRegistry {
        RuleRegistry::default()
    }

    /// Add a rule.
    ///
    /// Fails if another rule already uses the same code, or if the
    /// code cannot be configured. In that case the registry is left unchanged.
    pub fn register<R: AstRule>(&mut self, rule: R) -> Result<(), RegisterError> {
        self.register_erased(Box::new(rule))
    }

    /// Add an erased rule.
    pub fn register_erased(&mut self, rule: Box<dyn ErasedAstRule>) -> Result<(), RegisterError> {
        let code = rule.code();

        if !code.category().is_configurable() {
            return Err(RegisterError::NotConfigurable(code));
        }

        if let Some(&existing) = self.by_code.get(&code) {
            debug_assert_eq!(self.rules[existing].code(), code);
            return Err(RegisterError::DuplicateCode(code));
        }

        let index = self.rules.len();
        for kind in rule.applies_to() {
            self.by_kind.entry(*kind).or_default().push(index);
        }
        self.by_code.insert(code, index);
        self.rules.push(rule);
        Ok(())
    }

    /// The rules that apply to `kind`, in registration order.
    pub fn for_kind(&self, kind: NodeKind) -> impl Iterator<Item = &dyn ErasedAstRule> {
        self.by_kind
            .get(&kind)
            .map(Vec::as_slice)
            .unwrap_or_default()
            .iter()
            .map(|&index| self.rules[index].as_ref())
    }

    /// Look a rule up by its code, e.g. `IDM010`.
    pub fn by_code(&self, code: &ErrorCode) -> Option<&dyn ErasedAstRule> {
        self.by_code.get(code).map(|&i| self.rules[i].as_ref())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::error_code::Category;
    use crate::rule::AstRuleCtx;
    use vhdl_syntax::syntax::{
        validate::valid_node::Valid, IfStatementSyntax, SequentialStatementSyntax,
    };

    struct IfRule;

    impl AstRule for IfRule {
        type Node = IfStatementSyntax;
        const CODE: ErrorCode = ErrorCode::new(Category::Idiom, 900);
        fn check(&self, _node: &Valid<Self::Node>, _ctx: &mut AstRuleCtx<'_>) {}
    }

    struct OtherIfRule;

    impl AstRule for OtherIfRule {
        type Node = IfStatementSyntax;
        const CODE: ErrorCode = ErrorCode::new(Category::Idiom, 901);
        fn check(&self, _node: &Valid<Self::Node>, _ctx: &mut AstRuleCtx<'_>) {}
    }

    struct SequentialStatementRule;

    impl AstRule for SequentialStatementRule {
        type Node = SequentialStatementSyntax;
        const CODE: ErrorCode = ErrorCode::new(Category::Idiom, 902);
        fn check(&self, _node: &Valid<Self::Node>, _ctx: &mut AstRuleCtx<'_>) {}
    }

    fn codes_for(registry: &RuleRegistry, kind: NodeKind) -> Vec<ErrorCode> {
        registry.for_kind(kind).map(|rule| rule.code()).collect()
    }

    #[test]
    fn a_registered_rule_is_found_by_code() {
        let mut registry = RuleRegistry::new();
        registry.register(IfRule).unwrap();

        assert_eq!(codes_for(&registry, NodeKind::IfStatement), [IfRule::CODE]);
        assert_eq!(
            registry.by_code(&IfRule::CODE).map(|rule| rule.code()),
            Some(IfRule::CODE)
        );
    }

    #[test]
    fn rules_for_one_kind_come_back_in_registration_order() {
        let mut registry = RuleRegistry::new();
        registry.register(OtherIfRule).unwrap();
        registry.register(IfRule).unwrap();

        assert_eq!(
            codes_for(&registry, NodeKind::IfStatement),
            [OtherIfRule::CODE, IfRule::CODE]
        );
    }

    #[test]
    fn a_choice_rule_is_indexed_under_every_alternative() {
        let mut registry = RuleRegistry::new();
        registry.register(SequentialStatementRule).unwrap();

        for kind in [
            NodeKind::IfStatement,
            NodeKind::CaseStatement,
            NodeKind::NullStatement,
        ] {
            assert_eq!(
                codes_for(&registry, kind),
                [SequentialStatementRule::CODE],
                "{kind:?}"
            );
        }
    }

    #[test]
    fn registering_a_duplicate_code_fails_and_changes_nothing() {
        let mut registry = RuleRegistry::new();
        registry.register(IfRule).unwrap();

        struct Clashing;
        impl AstRule for Clashing {
            type Node = SequentialStatementSyntax;
            const CODE: ErrorCode = IfRule::CODE;
            fn check(&self, _node: &Valid<Self::Node>, _ctx: &mut AstRuleCtx<'_>) {}
        }

        assert_eq!(
            registry.register(Clashing),
            Err(RegisterError::DuplicateCode(IfRule::CODE))
        );
        assert_eq!(codes_for(&registry, NodeKind::IfStatement), [IfRule::CODE]);
    }

    #[test]
    fn a_non_configurable_rule_is_rejected() {
        struct SyntaxCoded;
        impl AstRule for SyntaxCoded {
            type Node = IfStatementSyntax;
            const CODE: ErrorCode = ErrorCode::new(Category::Syntax, 900);
            fn check(&self, _node: &Valid<Self::Node>, _ctx: &mut AstRuleCtx<'_>) {}
        }

        let mut registry = RuleRegistry::new();
        assert_eq!(
            registry.register(SyntaxCoded),
            Err(RegisterError::NotConfigurable(SyntaxCoded::CODE))
        );
    }

    #[test]
    fn register_error_names_the_code() {
        let message = RegisterError::DuplicateCode(ErrorCode::new(Category::Idiom, 1)).to_string();
        assert!(message.contains("IDM001"), "{message}");
    }
}
