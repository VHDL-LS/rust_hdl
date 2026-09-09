pub mod no_parens_around_if;
pub mod registry;
pub mod selection;

pub use registry::{RegisterError, RuleRegistry};

use vhdl_syntax::{
    parser::error::Span,
    syntax::{AstNode, NodeKind, SyntaxNode},
};

use crate::{
    diagnostic::Diagnostic, error_code::ErrorCode, fix::Fix, severity::Severity,
    source_loc::SourceLoc, FileId,
};

/// Holds context for checking a rule
pub struct AstRuleCtx<'a> {
    diagnostics: &'a mut Vec<Diagnostic>,
    // The severity for this error
    severity: Severity,
    // The file that this rule applies to
    // Note that AstRules only refer to their own file, therefore this lives in the context.
    file: FileId,
    // The error-code of this rule
    code: ErrorCode,
}

pub struct DiagnosticBuilder<'a>(&'a mut Diagnostic);

impl<'a> DiagnosticBuilder<'a> {
    pub fn with_fix(self, fix: Fix) -> DiagnosticBuilder<'a> {
        self.0.set_fix(fix);
        self
    }
}

impl<'a> AstRuleCtx<'a> {
    pub(crate) fn new(
        diagnostics: &mut Vec<Diagnostic>,
        severity: Severity,
        file: FileId,
        code: ErrorCode,
    ) -> AstRuleCtx<'_> {
        AstRuleCtx {
            diagnostics,
            severity,
            file,
            code,
        }
    }

    pub fn push(&mut self, span: Span, message: impl Into<String>) -> DiagnosticBuilder<'_> {
        self.diagnostics.push(Diagnostic::new(
            message,
            self.severity,
            SourceLoc::new(self.file, span),
            self.code,
        ));
        DiagnosticBuilder(self.diagnostics.last_mut().unwrap())
    }
}

/// A rule that applies to a specific Abstract Syntax Tree (AST) element.
///
/// This is what authors of custom rules should implement.
// Once an analysis crate exists there should be a second rule-kind applying to
// an analyzed state
pub trait AstRule: Send + Sync + 'static {
    /// The node that this rule applies to.
    /// Note: [DesignFileSyntax](vhdl_syntax::syntax::DesignFileSyntax)
    /// can be used to match on the full file.
    type Node: AstNode;

    const CODE: ErrorCode;

    const SEVERITY: Severity = Severity::Warning;

    const DEFAULT_ENABLED: bool = true;

    fn check(&self, node: &Self::Node, ctx: &mut AstRuleCtx<'_>);
}

/// A type-erased and object-safe view of an [`AstRule`].
pub trait ErasedAstRule: Send + Sync {
    fn code(&self) -> ErrorCode;

    /// Every node kind this rule wants to be called for.
    fn applies_to(&self) -> &'static [NodeKind];

    /// Run the rule against a node whose kind is one of [`Self::applies_to`].
    fn check_raw(&self, node: SyntaxNode, ctx: &mut AstRuleCtx<'_>);

    fn is_enabled_by_default(&self) -> bool;

    fn severity(&self) -> Severity;
}

impl<R: AstRule> ErasedAstRule for R {
    fn code(&self) -> ErrorCode {
        R::CODE
    }

    fn applies_to(&self) -> &'static [NodeKind] {
        <R::Node as AstNode>::META.concrete_kinds()
    }

    fn check_raw(&self, node: SyntaxNode, ctx: &mut AstRuleCtx) {
        self.check(&<R::Node as AstNode>::cast_unchecked(node), ctx)
    }

    fn is_enabled_by_default(&self) -> bool {
        R::DEFAULT_ENABLED
    }

    fn severity(&self) -> Severity {
        R::SEVERITY
    }
}
