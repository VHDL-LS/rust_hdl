// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2024, Lukas Scheller lukasscheller@icloud.com

use crate::parser::error::{SyntaxErr, SyntaxErrKind};
use crate::parser::Parser;
use crate::syntax::NodeKind;
use crate::tokens::{Keyword as Kw, TokenKind};

#[derive(Default)]
pub(crate) struct RecoveryState {
    /// One entry per currently-open node. Each entry is that node's FOLLOW set
    /// (the tokens that may legally appear immediately after the node closes).
    /// Used as the cumulative recovery set during panic-mode skipping.
    pub(crate) sync_stack: Vec<&'static [TokenKind]>,
}

impl RecoveryState {
    pub fn new() -> RecoveryState {
        RecoveryState {
            sync_stack: Vec::new(),
        }
    }

    /// Is `tok` somewhere in the FOLLOW set of any currently-open node?
    /// Used by error recovery to decide when to stop panic-mode skipping.
    pub fn is_recovery_point(&self, tok: TokenKind) -> bool {
        self.sync_stack.iter().any(|follow| follow.contains(&tok))
    }

    pub fn push(&mut self, node: NodeKind) {
        self.sync_stack.push(sync_tokens_for_node_kind(node));
    }

    pub fn pop(&mut self) {
        self.sync_stack.pop();
    }
}

impl Parser {
    /// Is `tok` somewhere in the FOLLOW set of any currently-open node?
    /// Used by error recovery to decide when to stop panic-mode skipping.
    pub(crate) fn is_recovery_point(&self, tok: TokenKind) -> bool {
        self.recovery.is_recovery_point(tok)
    }

    /// Publish diagnostics and recover when expecting one of several tokens.
    pub(crate) fn expect_tokens_recover<const N: usize>(&mut self, expected: [TokenKind; N]) {
        debug_assert!(
            !expected.contains(&self.peek_token()),
            "should only be called on an error path"
        );

        let start = self.builder.current_pos();

        // If recovery was already called at this position without making
        // progress, force-skip a token to prevent infinite loops.
        if self.last_recovery_pos == Some(start) {
            self.last_recovery_pos = None;
            self.skip();
            return;
        }

        // We're at EoF -> emit an EoF diagnostic if we haven't already
        if self.peek_token().is_eof() {
            if self.unexpected_eof {
                return;
            }
            self.unexpected_eof = true;
            self.errors.push(SyntaxErr::eof_err(start..start, expected));
            return;
        }

        let initial_trivia_len = self
            .token_stream
            .peek_next()
            .map(|tok| tok.leading_trivia().byte_len())
            .unwrap_or(0);
        let mut skipped_any = false;
        loop {
            let tok = self.peek_token();
            // Expected contains the token before we hit recovery or EoF -> Assume garbage input
            if expected.contains(&tok) {
                let end = self.builder.current_pos();
                self.errors.push(SyntaxErr::new(
                    start + initial_trivia_len..end,
                    SyntaxErrKind::Unexpected { kind: tok },
                ));
                self.last_recovery_pos = None;
                return;
            }

            // We have hit a recovery or EoF token. This means that it was likely only this token that's erroneous
            //
            // recovery should in theory always include EoF; this is simply a cheap safeguard
            // to avoid infinte looping.
            if self.is_recovery_point(tok) || tok.is_eof() {
                // We found a recovery token at the next position.
                // This means the token is simply missing.
                // Don't consume; simply report diagnostic.
                if !skipped_any {
                    // `start` is the insertion locus (the start of the found
                    // token's leading trivia). The found token's own span is
                    // recovered from the tree at render time, so we only record
                    // its kind here.
                    self.errors.push(SyntaxErr::new(
                        start..start,
                        SyntaxErrKind::Expected {
                            kinds: expected.into(),
                        },
                    ));
                    self.last_recovery_pos = Some(start);
                // skipped tokens: Garbage input before recovery token.
                } else {
                    self.errors.push(SyntaxErr::new(
                        start + initial_trivia_len..self.builder.current_pos(),
                        SyntaxErrKind::Unexpected { kind: tok },
                    ));
                    self.last_recovery_pos = None;
                }
                return;
            }

            // inconclusive. Skip and look at the next token.
            self.skip();
            skipped_any = true;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::error::SyntaxErr;
    use crate::parser::parse_syntax;
    use crate::parser::{parse, Parser};

    fn assert_expected_token(diag: &SyntaxErr, expected_kinds: &[TokenKind]) {
        match diag.err() {
            SyntaxErrKind::Expected { kinds } => {
                assert_eq!(kinds.as_ref(), expected_kinds, "expected kinds mismatch");
            }
            other => panic!("expected ExpectedToken, got {:?}", other),
        }
    }

    #[test]
    fn is_recovery_point_walks_full_sync_stack() {
        let mut state = RecoveryState::new();
        state.push(NodeKind::Package);

        assert!(state.is_recovery_point(TokenKind::Eof));
        state.push(NodeKind::AbsolutePathname);

        assert!(state.is_recovery_point(TokenKind::Colon));
        // EoF should still be recovery point (from previous node)
        assert!(state.is_recovery_point(TokenKind::Eof));

        state.pop();
        assert!(!state.is_recovery_point(TokenKind::Colon));
        assert!(state.is_recovery_point(TokenKind::Eof));

        state.pop();
        assert!(!state.is_recovery_point(TokenKind::Colon));
        assert!(!state.is_recovery_point(TokenKind::Eof));
    }

    /// Missing token: recovery token is sitting at the cursor, so nothing is
    /// skipped and a single ExpectedToken diagnostic is produced.
    #[test]
    fn expect_recover_emits_missing_token_when_recovery_point_is_next() {
        let (_, diags) = parse_syntax(";", |p: &mut Parser| {
            // Assertion's FOLLOW set is `[SemiColon]`, so `;` is a recovery point.
            p.start_node(NodeKind::Assertion);
            p.expect_tokens_recover([TokenKind::Keyword(Kw::Is)]);
            // The recovery token must NOT have been consumed.
            assert_eq!(p.peek_token(), TokenKind::SemiColon);
            p.skip(); // consume so the tree is non-empty
            p.end_node();
        });
        assert_eq!(diags.len(), 1);
        assert_expected_token(&diags[0], &[TokenKind::Keyword(Kw::Is)]);
    }

    /// Garbage before a recovery token: tokens are consumed up to (not
    /// including) the recovery token, and exactly one UnexpectedInput is
    /// reported covering the skipped range.
    #[test]
    fn expect_recover_emits_unexpected_input_after_skipping() {
        let (root, diags) = parse_syntax("foo bar ;", |p: &mut Parser| {
            // Assertion's FOLLOW set is `[SemiColon]`, so `;` is a recovery point.
            p.start_node(NodeKind::Assertion);
            p.expect_tokens_recover([TokenKind::Keyword(Kw::Is)]);
            assert_eq!(
                p.peek_token(),
                TokenKind::SemiColon,
                "recovery token must remain unconsumed"
            );
            p.skip();
            p.end_node();
        });
        assert_eq!(diags.len(), 1, "got: {:?}", diags);
        match &diags[0].err() {
            SyntaxErrKind::Unexpected { kind: _ } => {
                assert!(!diags[0].span_raw().is_empty());
            }
            other => panic!("expected UnexpectedInput, got {:?}", other),
        }
        // The skipped tokens must be attached to the green tree (not dropped).
        let mut buf = Vec::new();
        root.write_to(&mut buf).unwrap();
        let text = String::from_utf8(buf).unwrap();
        assert!(text.contains("foo"));
        assert!(text.contains("bar"));
    }

    /// Expected token shows up after garbage (not a recovery token):
    /// skipping stops once the expected token is reached, an
    /// UnexpectedInput diagnostic is reported, and the expected token
    /// remains unconsumed for the caller.
    #[test]
    fn expect_recover_stops_when_expected_token_appears() {
        let (_, diags) = parse_syntax("garbage is", |p: &mut Parser| {
            p.start_node(NodeKind::Assertion);
            p.expect_tokens_recover([TokenKind::Keyword(Kw::Is)]);
            // expected token kept for the caller to consume.
            assert_eq!(p.peek_token(), TokenKind::Keyword(Kw::Is));
            p.skip();
            p.end_node();
        });
        assert_eq!(diags.len(), 1);
        match diags[0].err() {
            SyntaxErrKind::Unexpected { kind: _ } => {
                // The diagnostic spans the skipped `garbage`, like the
                // garbage-before-recovery-token case above.
                assert!(!diags[0].span_raw().is_empty());
            }
            other => panic!("expected UnexpectedInput, got {:?}", other),
        }
    }

    /// EOF path: a single EOF diagnostic is emitted, and a subsequent call
    /// is idempotent (the `unexpected_eof` flag suppresses duplicates).
    #[test]
    fn expect_recover_at_eof_emits_once() {
        // Non-empty input so the wrapper node has at least one child;
        // we consume the lone token before calling expect_tokens_recover.
        let (_, diags) = parse_syntax("x", |p: &mut Parser| {
            p.start_node(NodeKind::DesignFile);
            p.skip();
            assert_eq!(p.peek_token(), TokenKind::Eof);
            p.expect_tokens_recover([TokenKind::Keyword(Kw::Is)]);
            // Second call must NOT push another diagnostic.
            p.expect_tokens_recover([TokenKind::Keyword(Kw::End)]);
            p.end_node();
        });
        assert_eq!(diags.len(), 1, "expected exactly one EoF diagnostic");
        assert_expected_token(&diags[0], &[TokenKind::Keyword(Kw::Is)]);
    }

    /// The `missing_token` diagnostic records the insertion locus (a zero-width
    /// span at the start of the next token's leading trivia) and the found token
    /// kind. The found token's own text span is no longer stored; it is derived
    /// from the syntax tree at render time via the locus.
    #[test]
    fn missing_token_records_locus_and_found_kind() {
        // Two newlines = two TriviaPiece entries, 2 bytes of trivia.
        let src = "\n\n;";
        let (_, diags) = parse_syntax(src, |p: &mut Parser| {
            // Assertion's FOLLOW set is `[SemiColon]`, so `;` is a recovery point.
            p.start_node(NodeKind::Assertion);
            p.expect_tokens_recover([TokenKind::Keyword(Kw::Is)]);
            p.skip();
            p.end_node();
        });
        assert_eq!(diags.len(), 1);
        // The insertion locus sits before the leading trivia of `;`, i.e. at the
        // start of input here.
        assert!(diags[0].span_raw().is_empty(), "locus must be zero-width");
    }

    // ---- End-to-end smoke tests via the public `parse` entrypoint ----

    /// Top-level garbage triggers `expect_tokens_recover` from `design_unit`;
    /// the DesignFile frame guarantees the parser terminates rather than
    /// looping on the bad input.
    #[test]
    fn parse_garbage_at_top_level_terminates_with_diagnostic() {
        let (_, diags) = parse("xyz");
        assert!(!diags.is_empty(), "expected at least one diagnostic");
    }

    /// Garbage between two design units must not consume the second unit's
    /// starter keyword — recovery should leave `entity` in place so the
    /// next iteration of `design_file` picks it up.
    #[test]
    fn parse_recovers_to_next_design_unit() {
        let src = "garbage entity foo is end entity foo;";
        let (file, diags) = parse(src);
        assert!(!diags.is_empty(), "expected diagnostics for garbage prefix");
        // The recovered tree must still contain the second unit's text.
        let mut buf = Vec::new();
        file.0.write_to(&mut buf).unwrap();
        let text = String::from_utf8(buf).unwrap();
        assert!(text.contains("entity"));
        assert!(text.contains("foo"));
    }
}

pub(crate) fn sync_tokens_for_node_kind(nk: NodeKind) -> &'static [TokenKind] {
    use TokenKind::*;
    match nk {
        NodeKind::AbsolutePathname
        | NodeKind::EntityDesignatorList
        | NodeKind::EntityNameListAll
        | NodeKind::EntityNameListOthers
        | NodeKind::InstantiationListAll
        | NodeKind::InstantiationListList
        | NodeKind::InstantiationListOthers
        | NodeKind::PackagePathname
        | NodeKind::PartialPathname
        | NodeKind::RelativePathname
        | NodeKind::SignalListAll
        | NodeKind::SignalListList
        | NodeKind::SignalListOthers => &[Colon],
        NodeKind::AccessTypeDefinition
        | NodeKind::Assertion
        | NodeKind::BindingIndication
        | NodeKind::ComponentInstantiationItems
        | NodeKind::ConditionalElseItem
        | NodeKind::ConditionalExpressions
        | NodeKind::ConditionalWaveformElseItem
        | NodeKind::ConditionalWaveforms
        | NodeKind::ConstrainedArrayDefinition
        | NodeKind::EnumerationTypeDefinition
        | NodeKind::FileOpenInformation
        | NodeKind::FileTypeDefinition
        | NodeKind::NumericTypeDefinition
        | NodeKind::PhysicalTypeDefinition
        | NodeKind::PhysicalTypeDefinitionEpilogue
        | NodeKind::PortMapAspect
        | NodeKind::ProtectedTypeBody
        | NodeKind::ProtectedTypeBodyEpilogue
        | NodeKind::ProtectedTypeDeclaration
        | NodeKind::ProtectedTypeDeclarationEpilogue
        | NodeKind::RecordTypeDefinition
        | NodeKind::RecordTypeDefinitionEpilogue
        | NodeKind::SelectedExpressions
        | NodeKind::SelectedWaveforms
        | NodeKind::TimeoutClause
        | NodeKind::UnboundedArrayDefinition => &[SemiColon],
        NodeKind::AllSensitivityList
        | NodeKind::AssociationList
        | NodeKind::ElementResolutionResolutionIndication
        | NodeKind::EntityClassEntryList
        | NodeKind::IndexSubtypeDefinitionList
        | NodeKind::InterfaceList
        | NodeKind::InterfacePackageGenericMapAspectAssociations
        | NodeKind::InterfacePackageGenericMapAspectBox
        | NodeKind::InterfacePackageGenericMapAspectDefault
        | NodeKind::RecordResolution
        | NodeKind::RecordResolutionElementResolution
        | NodeKind::ResolutionIndicationElementResolution
        | NodeKind::SensitivityList => &[RightPar],
        NodeKind::ArchitectureBody
        | NodeKind::ArchitectureEpilogue
        | NodeKind::ConfigurationDeclaration
        | NodeKind::ConfigurationDeclarationEpilogue
        | NodeKind::ContextDeclaration
        | NodeKind::ContextDeclarationEpilogue
        | NodeKind::DesignUnit
        | NodeKind::EntityDeclaration
        | NodeKind::EntityDeclarationEpilogue
        | NodeKind::PackageInstantiationDeclarationPrimaryUnit
        | NodeKind::PrimaryUnitPackageDeclaration
        | NodeKind::PslVerificationUnit
        | NodeKind::SecondaryUnitPackageBody => &[
            Eof,
            Keyword(Kw::Library),
            Keyword(Kw::Context),
            Keyword(Kw::Use),
            Keyword(Kw::Entity),
            Keyword(Kw::Configuration),
            Keyword(Kw::Package),
            Keyword(Kw::Architecture),
            Keyword(Kw::Body),
        ],
        NodeKind::BlockConfiguration
        | NodeKind::BlockConfigurationEpilogue
        | NodeKind::BlockConfigurationItem
        | NodeKind::BlockConfigurationItems
        | NodeKind::CaseGenerateAlternative
        | NodeKind::CaseStatementAlternative
        | NodeKind::ComponentConfiguration
        | NodeKind::ComponentConfigurationItems
        | NodeKind::ComponentDeclarationItems
        | NodeKind::CompoundConfigurationSpecificationItems
        | NodeKind::ConfigurationDeclarationItems
        | NodeKind::ElementDeclaration
        | NodeKind::IfGenerateElse
        | NodeKind::IfStatementElse
        | NodeKind::RecordElementDeclarations
        | NodeKind::SecondaryUnitDeclaration
        | NodeKind::UnitDeclarations => &[Keyword(Kw::End)],
        NodeKind::EntitySpecification => &[Keyword(Kw::Is)],
        NodeKind::ForIterationScheme | NodeKind::WhileIterationScheme => &[Keyword(Kw::Loop)],
        NodeKind::GuardedSignalSpecification => &[Keyword(Kw::After)],
        NodeKind::IndexConstraint => &[Keyword(Kw::Of)],
        NodeKind::InterfacePackageDeclarationPreamble => &[Keyword(Kw::New)],
        NodeKind::ActualPart
        | NodeKind::ActualPartExpression
        | NodeKind::ActualPartOpen
        | NodeKind::ActualPartSubtypeIndication
        | NodeKind::AssociationElement
        | NodeKind::ElementAssociation
        | NodeKind::EntityClassEntry
        | NodeKind::IndexSubtypeDefinition
        | NodeKind::RecordElementResolution => &[Comma, RightPar],
        NodeKind::Aggregate
        | NodeKind::AggregateTarget
        | NodeKind::NameTarget
        | NodeKind::CaseStatementPreamble => &[Keyword(Kw::End), Keyword(Kw::When)],
        NodeKind::ConditionClause => &[Keyword(Kw::For), SemiColon],
        NodeKind::ConditionalElseWhenExpression
        | NodeKind::ConditionalExpression
        | NodeKind::ConditionalWaveform
        | NodeKind::ConditionalWaveformElseWhenExpression => &[Keyword(Kw::Else), SemiColon],
        NodeKind::EntityDesignator => &[Colon, Comma],
        NodeKind::FunctionSpecification | NodeKind::ProcedureSpecification => {
            &[Keyword(Kw::Is), SemiColon]
        }
        NodeKind::IdentifierList => &[Colon, SemiColon],
        NodeKind::IfGenerateElsif | NodeKind::IfStatementElsif => {
            &[Keyword(Kw::Else), Keyword(Kw::End)]
        }
        NodeKind::InterfaceConstantDeclaration
        | NodeKind::InterfaceFileDeclaration
        | NodeKind::InterfaceIncompleteTypeDeclaration
        | NodeKind::InterfacePackageDeclaration
        | NodeKind::InterfacePackageGenericMapAspect
        | NodeKind::InterfaceSignalDeclaration
        | NodeKind::InterfaceSubprogramDeclaration
        | NodeKind::InterfaceSubprogramDefaultBox
        | NodeKind::InterfaceSubprogramDefaultName
        | NodeKind::InterfaceVariableDeclaration => &[RightPar, SemiColon],
        NodeKind::PackageInstantiationPreamble
        | NodeKind::SubprogramInstantiationDeclarationPreamble => {
            &[Keyword(Kw::Generic), SemiColon]
        }
        NodeKind::ParameterSpecification => &[Keyword(Kw::Generate), Keyword(Kw::Loop)],
        NodeKind::PrimaryUnitDeclaration => &[Identifier, Keyword(Kw::End)],
        NodeKind::SelectedExpressionItem | NodeKind::SelectedWaveformItem => &[Comma, SemiColon],
        NodeKind::UnaffectedWaveform | NodeKind::WaveformElements => {
            &[Keyword(Kw::When), SemiColon]
        }
        NodeKind::VerificationUnitBindingIndication | NodeKind::VerificationUnitList => {
            &[Keyword(Kw::End), SemiColon]
        }
        NodeKind::AssertionStatement
        | NodeKind::BlockEpilogue
        | NodeKind::BlockStatement
        | NodeKind::CaseGenerateStatement
        | NodeKind::CaseGenerateStatementEpilogue
        | NodeKind::CaseStatement
        | NodeKind::CaseStatementEpilogue
        | NodeKind::ComponentInstantiationStatement
        | NodeKind::ConcurrentAssertionStatement
        | NodeKind::ConcurrentConditionalSignalAssignment
        | NodeKind::ConcurrentProcedureCallOrComponentInstantiationStatement
        | NodeKind::ConcurrentSelectedSignalAssignment
        | NodeKind::ConcurrentSimpleSignalAssignment
        | NodeKind::ConcurrentStatements
        | NodeKind::ConditionalForceAssignment
        | NodeKind::ConditionalVariableAssignment
        | NodeKind::ConditionalWaveformAssignment
        | NodeKind::ExitStatement
        | NodeKind::ForGenerateStatement
        | NodeKind::ForGenerateStatementEpilogue
        | NodeKind::GenerateStatementBody
        | NodeKind::GenerateStatementBodyEpilogue
        | NodeKind::IfGenerateStatement
        | NodeKind::IfGenerateStatementEpilogue
        | NodeKind::IfStatement
        | NodeKind::IfStatementEpilogue
        | NodeKind::LoopStatement
        | NodeKind::LoopStatementEpilogue
        | NodeKind::NextStatement
        | NodeKind::NullStatement
        | NodeKind::ProcedureCallStatement
        | NodeKind::ProcessStatement
        | NodeKind::ProcessStatementEpilogue
        | NodeKind::PslDirective
        | NodeKind::ReportStatement
        | NodeKind::ReturnStatement
        | NodeKind::SelectedForceAssignment
        | NodeKind::SelectedVariableAssignment
        | NodeKind::SelectedWaveformAssignment
        | NodeKind::SequentialStatements
        | NodeKind::SimpleForceAssignment
        | NodeKind::SimpleReleaseAssignment
        | NodeKind::SimpleVariableAssignment
        | NodeKind::SimpleWaveformAssignment
        | NodeKind::WaitStatement => &[Keyword(Kw::Else), Keyword(Kw::Elsif), Keyword(Kw::End)],
        NodeKind::BlockConfigurationPreamble => {
            &[Keyword(Kw::End), Keyword(Kw::For), Keyword(Kw::Use)]
        }
        NodeKind::ComponentDeclarationPreamble => {
            &[Keyword(Kw::End), Keyword(Kw::Generic), Keyword(Kw::Port)]
        }
        NodeKind::ComponentInstantiatedUnit
        | NodeKind::ConfigurationInstantiatedUnit
        | NodeKind::EntityConfigurationAspect
        | NodeKind::EntityEntityAspect
        | NodeKind::EntityInstantiatedUnit
        | NodeKind::EntityOpenAspect => &[Keyword(Kw::Generic), Keyword(Kw::Port), SemiColon],
        NodeKind::NameList | NodeKind::SensitivityClause => {
            &[Keyword(Kw::For), Keyword(Kw::Until), SemiColon]
        }
        NodeKind::RecordTypeDefinitionPreamble => &[Comma, Identifier, Keyword(Kw::End)],
        NodeKind::WaveformElement => &[Comma, Keyword(Kw::When), SemiColon],
        NodeKind::ContextDeclarationPreamble => &[
            Keyword(Kw::Context),
            Keyword(Kw::End),
            Keyword(Kw::Library),
            Keyword(Kw::Use),
        ],
        NodeKind::ConcurrentSelectedSignalAssignmentPreamble
        | NodeKind::SelectedAssignmentPreamble => {
            &[CharacterLiteral, Identifier, LeftPar, LtLt, StringLiteral]
        }
        NodeKind::Signature => &[Colon, Comma, Keyword(Kw::Generic), SemiColon, Tick],
        NodeKind::SubprogramHeader => &[
            Keyword(Kw::Is),
            Keyword(Kw::Parameter),
            Keyword(Kw::Return),
            LeftPar,
            SemiColon,
        ],
        NodeKind::NameResolutionIndication => &[
            CharacterLiteral,
            Comma,
            Identifier,
            LtLt,
            RightPar,
            StringLiteral,
        ],
        NodeKind::SubprogramHeaderGenericClause => &[
            Keyword(Kw::Generic),
            Keyword(Kw::Is),
            Keyword(Kw::Parameter),
            Keyword(Kw::Return),
            LeftPar,
            SemiColon,
        ],
        NodeKind::ContextClause
        | NodeKind::ContextReference
        | NodeKind::LibraryClause
        | NodeKind::UseClauseContextItem => &[
            Keyword(Kw::Architecture),
            Keyword(Kw::Configuration),
            Keyword(Kw::Context),
            Keyword(Kw::End),
            Keyword(Kw::Entity),
            Keyword(Kw::Package),
        ],
        NodeKind::ComponentConfigurationPreamble | NodeKind::ComponentSpecification => &[
            Keyword(Kw::Configuration),
            Keyword(Kw::End),
            Keyword(Kw::Entity),
            Keyword(Kw::For),
            Keyword(Kw::Generic),
            Keyword(Kw::Open),
            Keyword(Kw::Port),
            Keyword(Kw::Use),
        ],
        NodeKind::InterfaceFunctionSpecification | NodeKind::InterfaceProcedureSpecification => &[
            BOX,
            CharacterLiteral,
            Identifier,
            Keyword(Kw::Is),
            LtLt,
            RightPar,
            SemiColon,
            StringLiteral,
        ],
        NodeKind::ParameterList => &[
            BOX,
            CharacterLiteral,
            Identifier,
            Keyword(Kw::Is),
            Keyword(Kw::Return),
            LtLt,
            RightPar,
            SemiColon,
            StringLiteral,
        ],
        NodeKind::CaseStatementAlternativePreamble | NodeKind::LoopStatementPreamble => &[
            CharacterLiteral,
            Identifier,
            Keyword(Kw::Assert),
            Keyword(Kw::Case),
            Keyword(Kw::End),
            Keyword(Kw::Exit),
            Keyword(Kw::If),
            Keyword(Kw::With),
            LeftPar,
            LtLt,
            StringLiteral,
        ],
        NodeKind::GenericClausePreamble | NodeKind::PortClausePreamble => &[
            Comma,
            Identifier,
            Keyword(Kw::Constant),
            Keyword(Kw::File),
            Keyword(Kw::Function),
            Keyword(Kw::Impure),
            Keyword(Kw::Package),
            Keyword(Kw::Procedure),
            Keyword(Kw::Pure),
            Keyword(Kw::Signal),
            Keyword(Kw::Type),
            Keyword(Kw::Variable),
            RightPar,
        ],
        NodeKind::IfStatementPreamble => &[
            CharacterLiteral,
            Identifier,
            Keyword(Kw::Assert),
            Keyword(Kw::Case),
            Keyword(Kw::Else),
            Keyword(Kw::Elsif),
            Keyword(Kw::End),
            Keyword(Kw::Exit),
            Keyword(Kw::If),
            Keyword(Kw::With),
            LeftPar,
            LtLt,
            StringLiteral,
        ],
        NodeKind::AliasDeclaration
        | NodeKind::AttributeDeclaration
        | NodeKind::AttributeSpecification
        | NodeKind::ComponentConfigurationEpilogue
        | NodeKind::ComponentDeclaration
        | NodeKind::ComponentDeclarationEpilogue
        | NodeKind::CompoundConfigurationSpecification
        | NodeKind::ConstantDeclaration
        | NodeKind::Declarations
        | NodeKind::DisconnectionSpecification
        | NodeKind::FileDeclaration
        | NodeKind::FullTypeDeclaration
        | NodeKind::GroupDeclaration
        | NodeKind::GroupTemplateDeclaration
        | NodeKind::IncompleteTypeDeclaration
        | NodeKind::PackageBodyDeclaration
        | NodeKind::PackageDeclaration
        | NodeKind::PackageInstantiationDeclaration
        | NodeKind::PslClockDeclaration
        | NodeKind::PslPropertyDeclaration
        | NodeKind::PslSequenceDeclaration
        | NodeKind::SharedVariableDeclaration
        | NodeKind::SignalDeclaration
        | NodeKind::SimpleConfigurationSpecification
        | NodeKind::SubprogramBody
        | NodeKind::SubprogramBodyEpilogue
        | NodeKind::SubprogramDeclaration
        | NodeKind::SubprogramInstantiationDeclaration
        | NodeKind::SubtypeDeclaration
        | NodeKind::UseClauseDeclaration
        | NodeKind::VariableDeclaration => &[
            CharacterLiteral,
            Identifier,
            Keyword(Kw::Assert),
            Keyword(Kw::Begin),
            Keyword(Kw::Else),
            Keyword(Kw::Elsif),
            Keyword(Kw::End),
            Keyword(Kw::For),
            Keyword(Kw::If),
            Keyword(Kw::Postponed),
            Keyword(Kw::Use),
            Keyword(Kw::With),
            LeftPar,
            LtLt,
            StringLiteral,
        ],
        NodeKind::DeclarationStatementSeparator => &[
            CharacterLiteral,
            Identifier,
            Keyword(Kw::Assert),
            Keyword(Kw::Case),
            Keyword(Kw::Else),
            Keyword(Kw::Elsif),
            Keyword(Kw::End),
            Keyword(Kw::Exit),
            Keyword(Kw::For),
            Keyword(Kw::If),
            Keyword(Kw::Postponed),
            Keyword(Kw::With),
            LeftPar,
            LtLt,
            StringLiteral,
        ],
        NodeKind::Package
        | NodeKind::PackageBody
        | NodeKind::PackageBodyEpilogue
        | NodeKind::PackageEpilogue
        | NodeKind::PackageInstantiation => &[
            CharacterLiteral,
            Eof,
            Identifier,
            Keyword(Kw::Assert),
            Keyword(Kw::Begin),
            Keyword(Kw::Else),
            Keyword(Kw::Elsif),
            Keyword(Kw::End),
            Keyword(Kw::For),
            Keyword(Kw::If),
            Keyword(Kw::Postponed),
            Keyword(Kw::Use),
            Keyword(Kw::With),
            LeftPar,
            LtLt,
            StringLiteral,
        ],
        NodeKind::ConfigurationDeclarationPreamble => &[
            Keyword(Kw::Alias),
            Keyword(Kw::Attribute),
            Keyword(Kw::Component),
            Keyword(Kw::Constant),
            Keyword(Kw::Disconnect),
            Keyword(Kw::File),
            Keyword(Kw::For),
            Keyword(Kw::Function),
            Keyword(Kw::Group),
            Keyword(Kw::Impure),
            Keyword(Kw::Package),
            Keyword(Kw::Procedure),
            Keyword(Kw::Pure),
            Keyword(Kw::Shared),
            Keyword(Kw::Signal),
            Keyword(Kw::Subtype),
            Keyword(Kw::Type),
            Keyword(Kw::Use),
            Keyword(Kw::Variable),
        ],
        NodeKind::ArchitecturePreamble
        | NodeKind::BlockHeader
        | NodeKind::ProcessStatementPreamble
        | NodeKind::SubprogramBodyPreamble => &[
            Keyword(Kw::Alias),
            Keyword(Kw::Attribute),
            Keyword(Kw::Begin),
            Keyword(Kw::Component),
            Keyword(Kw::Constant),
            Keyword(Kw::Disconnect),
            Keyword(Kw::File),
            Keyword(Kw::For),
            Keyword(Kw::Function),
            Keyword(Kw::Group),
            Keyword(Kw::Impure),
            Keyword(Kw::Package),
            Keyword(Kw::Procedure),
            Keyword(Kw::Pure),
            Keyword(Kw::Shared),
            Keyword(Kw::Signal),
            Keyword(Kw::Subtype),
            Keyword(Kw::Type),
            Keyword(Kw::Use),
            Keyword(Kw::Variable),
        ],
        NodeKind::PackageBodyPreamble
        | NodeKind::PackageHeader
        | NodeKind::ProtectedTypeBodyPreamble
        | NodeKind::ProtectedTypeDeclarationPreamble => &[
            Keyword(Kw::Alias),
            Keyword(Kw::Attribute),
            Keyword(Kw::Component),
            Keyword(Kw::Constant),
            Keyword(Kw::Disconnect),
            Keyword(Kw::End),
            Keyword(Kw::File),
            Keyword(Kw::For),
            Keyword(Kw::Function),
            Keyword(Kw::Group),
            Keyword(Kw::Impure),
            Keyword(Kw::Package),
            Keyword(Kw::Procedure),
            Keyword(Kw::Pure),
            Keyword(Kw::Shared),
            Keyword(Kw::Signal),
            Keyword(Kw::Subtype),
            Keyword(Kw::Type),
            Keyword(Kw::Use),
            Keyword(Kw::Variable),
        ],
        NodeKind::UseClause => &[
            CharacterLiteral,
            Identifier,
            Keyword(Kw::Architecture),
            Keyword(Kw::Assert),
            Keyword(Kw::Begin),
            Keyword(Kw::Configuration),
            Keyword(Kw::Context),
            Keyword(Kw::Else),
            Keyword(Kw::Elsif),
            Keyword(Kw::End),
            Keyword(Kw::Entity),
            Keyword(Kw::For),
            Keyword(Kw::If),
            Keyword(Kw::Package),
            Keyword(Kw::Postponed),
            Keyword(Kw::Use),
            Keyword(Kw::With),
            LeftPar,
            LtLt,
            StringLiteral,
        ],
        NodeKind::InertialDelayMechanism | NodeKind::TransportDelayMechanism => &[
            AbstractLiteral,
            BitStringLiteral,
            CharacterLiteral,
            Identifier,
            Keyword(Kw::Abs),
            Keyword(Kw::And),
            Keyword(Kw::Nand),
            Keyword(Kw::New),
            Keyword(Kw::Nor),
            Keyword(Kw::Not),
            Keyword(Kw::Null),
            Keyword(Kw::Or),
            Keyword(Kw::Unaffected),
            Keyword(Kw::Xnor),
            Keyword(Kw::Xor),
            LeftPar,
            LtLt,
            Minus,
            Plus,
            QueQue,
            SemiColon,
            StringLiteral,
        ],
        NodeKind::BlockPreamble => &[
            Keyword(Kw::Alias),
            Keyword(Kw::Attribute),
            Keyword(Kw::Begin),
            Keyword(Kw::Component),
            Keyword(Kw::Constant),
            Keyword(Kw::Disconnect),
            Keyword(Kw::File),
            Keyword(Kw::For),
            Keyword(Kw::Function),
            Keyword(Kw::Generic),
            Keyword(Kw::Group),
            Keyword(Kw::Impure),
            Keyword(Kw::Package),
            Keyword(Kw::Port),
            Keyword(Kw::Procedure),
            Keyword(Kw::Pure),
            Keyword(Kw::Shared),
            Keyword(Kw::Signal),
            Keyword(Kw::Subtype),
            Keyword(Kw::Type),
            Keyword(Kw::Use),
            Keyword(Kw::Variable),
        ],
        NodeKind::Choices => &[
            AbstractLiteral,
            BitStringLiteral,
            CharacterLiteral,
            Comma,
            Identifier,
            Keyword(Kw::Abs),
            Keyword(Kw::And),
            Keyword(Kw::Nand),
            Keyword(Kw::New),
            Keyword(Kw::Nor),
            Keyword(Kw::Not),
            Keyword(Kw::Null),
            Keyword(Kw::Or),
            Keyword(Kw::Xnor),
            Keyword(Kw::Xor),
            LeftPar,
            LtLt,
            Minus,
            Plus,
            QueQue,
            RightArrow,
            SemiColon,
            StringLiteral,
        ],
        NodeKind::FormalPart => &[
            AbstractLiteral,
            BitStringLiteral,
            CharacterLiteral,
            Identifier,
            Keyword(Kw::Abs),
            Keyword(Kw::And),
            Keyword(Kw::Inertial),
            Keyword(Kw::Nand),
            Keyword(Kw::New),
            Keyword(Kw::Nor),
            Keyword(Kw::Not),
            Keyword(Kw::Null),
            Keyword(Kw::Open),
            Keyword(Kw::Or),
            Keyword(Kw::Xnor),
            Keyword(Kw::Xor),
            LeftPar,
            LtLt,
            Minus,
            Plus,
            QueQue,
            RightArrow,
            StringLiteral,
        ],
        NodeKind::PackagePreamble => &[
            Keyword(Kw::Alias),
            Keyword(Kw::Attribute),
            Keyword(Kw::Component),
            Keyword(Kw::Constant),
            Keyword(Kw::Disconnect),
            Keyword(Kw::End),
            Keyword(Kw::File),
            Keyword(Kw::For),
            Keyword(Kw::Function),
            Keyword(Kw::Generic),
            Keyword(Kw::Group),
            Keyword(Kw::Impure),
            Keyword(Kw::Package),
            Keyword(Kw::Procedure),
            Keyword(Kw::Pure),
            Keyword(Kw::Shared),
            Keyword(Kw::Signal),
            Keyword(Kw::Subtype),
            Keyword(Kw::Type),
            Keyword(Kw::Use),
            Keyword(Kw::Variable),
            SemiColon,
        ],
        NodeKind::ExpressionChoice | NodeKind::OthersChoice => &[
            AbstractLiteral,
            Bar,
            BitStringLiteral,
            CharacterLiteral,
            Comma,
            Identifier,
            Keyword(Kw::Abs),
            Keyword(Kw::And),
            Keyword(Kw::Nand),
            Keyword(Kw::New),
            Keyword(Kw::Nor),
            Keyword(Kw::Not),
            Keyword(Kw::Null),
            Keyword(Kw::Or),
            Keyword(Kw::Xnor),
            Keyword(Kw::Xor),
            LeftPar,
            LtLt,
            Minus,
            Plus,
            QueQue,
            RightArrow,
            SemiColon,
            StringLiteral,
        ],
        NodeKind::GenericMapAspect => &[
            Keyword(Kw::Alias),
            Keyword(Kw::Attribute),
            Keyword(Kw::Component),
            Keyword(Kw::Constant),
            Keyword(Kw::Disconnect),
            Keyword(Kw::End),
            Keyword(Kw::File),
            Keyword(Kw::For),
            Keyword(Kw::Function),
            Keyword(Kw::Group),
            Keyword(Kw::Impure),
            Keyword(Kw::Is),
            Keyword(Kw::Package),
            Keyword(Kw::Parameter),
            Keyword(Kw::Port),
            Keyword(Kw::Procedure),
            Keyword(Kw::Pure),
            Keyword(Kw::Return),
            Keyword(Kw::Shared),
            Keyword(Kw::Signal),
            Keyword(Kw::Subtype),
            Keyword(Kw::Type),
            Keyword(Kw::Use),
            Keyword(Kw::Variable),
            LeftPar,
            SemiColon,
        ],
        NodeKind::SubtypeIndication => &[
            AbstractLiteral,
            BitStringLiteral,
            CharacterLiteral,
            ColonEq,
            Comma,
            GtGt,
            Identifier,
            Keyword(Kw::Abs),
            Keyword(Kw::And),
            Keyword(Kw::Bus),
            Keyword(Kw::Is),
            Keyword(Kw::Nand),
            Keyword(Kw::New),
            Keyword(Kw::Nor),
            Keyword(Kw::Not),
            Keyword(Kw::Null),
            Keyword(Kw::Open),
            Keyword(Kw::Or),
            Keyword(Kw::Register),
            Keyword(Kw::Xnor),
            Keyword(Kw::Xor),
            LeftPar,
            LtLt,
            Minus,
            Plus,
            QueQue,
            RightPar,
            SemiColon,
            StringLiteral,
        ],
        NodeKind::EntityHeader | NodeKind::ForGenerateStatementPreamble => &[
            CharacterLiteral,
            Identifier,
            Keyword(Kw::Alias),
            Keyword(Kw::Assert),
            Keyword(Kw::Attribute),
            Keyword(Kw::Begin),
            Keyword(Kw::Component),
            Keyword(Kw::Constant),
            Keyword(Kw::Disconnect),
            Keyword(Kw::End),
            Keyword(Kw::File),
            Keyword(Kw::For),
            Keyword(Kw::Function),
            Keyword(Kw::Group),
            Keyword(Kw::If),
            Keyword(Kw::Impure),
            Keyword(Kw::Package),
            Keyword(Kw::Postponed),
            Keyword(Kw::Procedure),
            Keyword(Kw::Pure),
            Keyword(Kw::Shared),
            Keyword(Kw::Signal),
            Keyword(Kw::Subtype),
            Keyword(Kw::Type),
            Keyword(Kw::Use),
            Keyword(Kw::Variable),
            Keyword(Kw::With),
            LeftPar,
            LtLt,
            StringLiteral,
        ],
        NodeKind::PortClause | NodeKind::PortClauseEpilogue => &[
            CharacterLiteral,
            Identifier,
            Keyword(Kw::Alias),
            Keyword(Kw::Assert),
            Keyword(Kw::Attribute),
            Keyword(Kw::Begin),
            Keyword(Kw::Component),
            Keyword(Kw::Constant),
            Keyword(Kw::Disconnect),
            Keyword(Kw::End),
            Keyword(Kw::File),
            Keyword(Kw::For),
            Keyword(Kw::Function),
            Keyword(Kw::Group),
            Keyword(Kw::If),
            Keyword(Kw::Impure),
            Keyword(Kw::Package),
            Keyword(Kw::Port),
            Keyword(Kw::Postponed),
            Keyword(Kw::Procedure),
            Keyword(Kw::Pure),
            Keyword(Kw::Shared),
            Keyword(Kw::Signal),
            Keyword(Kw::Subtype),
            Keyword(Kw::Type),
            Keyword(Kw::Use),
            Keyword(Kw::Variable),
            Keyword(Kw::With),
            LeftPar,
            LtLt,
            StringLiteral,
        ],
        NodeKind::EntityDeclarationPreamble => &[
            CharacterLiteral,
            Identifier,
            Keyword(Kw::Alias),
            Keyword(Kw::Assert),
            Keyword(Kw::Attribute),
            Keyword(Kw::Begin),
            Keyword(Kw::Component),
            Keyword(Kw::Constant),
            Keyword(Kw::Disconnect),
            Keyword(Kw::End),
            Keyword(Kw::File),
            Keyword(Kw::For),
            Keyword(Kw::Function),
            Keyword(Kw::Generic),
            Keyword(Kw::Group),
            Keyword(Kw::If),
            Keyword(Kw::Impure),
            Keyword(Kw::Package),
            Keyword(Kw::Port),
            Keyword(Kw::Postponed),
            Keyword(Kw::Procedure),
            Keyword(Kw::Pure),
            Keyword(Kw::Shared),
            Keyword(Kw::Signal),
            Keyword(Kw::Subtype),
            Keyword(Kw::Type),
            Keyword(Kw::Use),
            Keyword(Kw::Variable),
            Keyword(Kw::With),
            LeftPar,
            LtLt,
            StringLiteral,
        ],
        NodeKind::IfGenerateStatementPreamble => &[
            CharacterLiteral,
            Identifier,
            Keyword(Kw::Alias),
            Keyword(Kw::Assert),
            Keyword(Kw::Attribute),
            Keyword(Kw::Begin),
            Keyword(Kw::Component),
            Keyword(Kw::Constant),
            Keyword(Kw::Disconnect),
            Keyword(Kw::Else),
            Keyword(Kw::Elsif),
            Keyword(Kw::End),
            Keyword(Kw::File),
            Keyword(Kw::For),
            Keyword(Kw::Function),
            Keyword(Kw::Group),
            Keyword(Kw::If),
            Keyword(Kw::Impure),
            Keyword(Kw::Package),
            Keyword(Kw::Postponed),
            Keyword(Kw::Procedure),
            Keyword(Kw::Pure),
            Keyword(Kw::Shared),
            Keyword(Kw::Signal),
            Keyword(Kw::Subtype),
            Keyword(Kw::Type),
            Keyword(Kw::Use),
            Keyword(Kw::Variable),
            Keyword(Kw::With),
            LeftPar,
            LtLt,
            StringLiteral,
        ],
        NodeKind::GenericClause | NodeKind::GenericClauseEpilogue => &[
            CharacterLiteral,
            Identifier,
            Keyword(Kw::Alias),
            Keyword(Kw::Assert),
            Keyword(Kw::Attribute),
            Keyword(Kw::Begin),
            Keyword(Kw::Component),
            Keyword(Kw::Constant),
            Keyword(Kw::Disconnect),
            Keyword(Kw::End),
            Keyword(Kw::File),
            Keyword(Kw::For),
            Keyword(Kw::Function),
            Keyword(Kw::Generic),
            Keyword(Kw::Group),
            Keyword(Kw::If),
            Keyword(Kw::Impure),
            Keyword(Kw::Package),
            Keyword(Kw::Port),
            Keyword(Kw::Postponed),
            Keyword(Kw::Procedure),
            Keyword(Kw::Pure),
            Keyword(Kw::Shared),
            Keyword(Kw::Signal),
            Keyword(Kw::Subtype),
            Keyword(Kw::Type),
            Keyword(Kw::Use),
            Keyword(Kw::Variable),
            Keyword(Kw::With),
            LeftPar,
            LtLt,
            SemiColon,
            StringLiteral,
        ],
        NodeKind::Label => &[
            AbstractLiteral,
            BitStringLiteral,
            CharacterLiteral,
            Identifier,
            Keyword(Kw::Abs),
            Keyword(Kw::And),
            Keyword(Kw::Assert),
            Keyword(Kw::Block),
            Keyword(Kw::Case),
            Keyword(Kw::Component),
            Keyword(Kw::Configuration),
            Keyword(Kw::Entity),
            Keyword(Kw::Exit),
            Keyword(Kw::For),
            Keyword(Kw::Generate),
            Keyword(Kw::If),
            Keyword(Kw::Loop),
            Keyword(Kw::Nand),
            Keyword(Kw::New),
            Keyword(Kw::Next),
            Keyword(Kw::Nor),
            Keyword(Kw::Not),
            Keyword(Kw::Null),
            Keyword(Kw::Or),
            Keyword(Kw::Others),
            Keyword(Kw::Postponed),
            Keyword(Kw::Process),
            Keyword(Kw::Report),
            Keyword(Kw::Return),
            Keyword(Kw::Wait),
            Keyword(Kw::While),
            Keyword(Kw::With),
            Keyword(Kw::Xnor),
            Keyword(Kw::Xor),
            LeftPar,
            LtLt,
            Minus,
            Plus,
            QueQue,
            RightArrow,
            StringLiteral,
        ],
        NodeKind::Allocator
        | NodeKind::AttributeName
        | NodeKind::BinaryExpression
        | NodeKind::LiteralExpression
        | NodeKind::Name
        | NodeKind::NameExpression
        | NodeKind::ParenthesizedExpressionOrAggregate
        | NodeKind::ParenthesizedName
        | NodeKind::PhysicalLiteral
        | NodeKind::PhysicalLiteralExpression
        | NodeKind::QualifiedExpression
        | NodeKind::RangeConstraint
        | NodeKind::SelectedName
        | NodeKind::UnaryExpression => &[
            AbstractLiteral,
            BOX,
            Bar,
            BitStringLiteral,
            CharacterLiteral,
            Colon,
            ColonEq,
            Comma,
            Concat,
            Div,
            EQ,
            GT,
            GTE,
            GtGt,
            Identifier,
            Keyword(Kw::Abs),
            Keyword(Kw::After),
            Keyword(Kw::And),
            Keyword(Kw::Bus),
            Keyword(Kw::Configuration),
            Keyword(Kw::Downto),
            Keyword(Kw::Else),
            Keyword(Kw::End),
            Keyword(Kw::Entity),
            Keyword(Kw::For),
            Keyword(Kw::Generate),
            Keyword(Kw::Generic),
            Keyword(Kw::Inertial),
            Keyword(Kw::Is),
            Keyword(Kw::Loop),
            Keyword(Kw::Mod),
            Keyword(Kw::Nand),
            Keyword(Kw::New),
            Keyword(Kw::Nor),
            Keyword(Kw::Not),
            Keyword(Kw::Null),
            Keyword(Kw::Open),
            Keyword(Kw::Or),
            Keyword(Kw::Port),
            Keyword(Kw::Range),
            Keyword(Kw::Register),
            Keyword(Kw::Rem),
            Keyword(Kw::Report),
            Keyword(Kw::Return),
            Keyword(Kw::Rol),
            Keyword(Kw::Ror),
            Keyword(Kw::Select),
            Keyword(Kw::Severity),
            Keyword(Kw::Sla),
            Keyword(Kw::Sll),
            Keyword(Kw::Sra),
            Keyword(Kw::Srl),
            Keyword(Kw::Then),
            Keyword(Kw::To),
            Keyword(Kw::Units),
            Keyword(Kw::Until),
            Keyword(Kw::Use),
            Keyword(Kw::When),
            Keyword(Kw::Xnor),
            Keyword(Kw::Xor),
            LT,
            LTE,
            LeftPar,
            LeftSquare,
            LtLt,
            Minus,
            NE,
            Plus,
            Pow,
            QueEQ,
            QueGT,
            QueGTE,
            QueLT,
            QueLTE,
            QueNE,
            QueQue,
            RightArrow,
            RightPar,
            RightSquare,
            SemiColon,
            StringLiteral,
            Tick,
            Times,
        ],
        NodeKind::ExternalConstantName
        | NodeKind::ExternalSignalName
        | NodeKind::ExternalVariableName
        | NodeKind::NameDesignatorPrefix => &[
            AbstractLiteral,
            BOX,
            Bar,
            BitStringLiteral,
            CharacterLiteral,
            Colon,
            ColonEq,
            Comma,
            Concat,
            Div,
            Dot,
            EQ,
            GT,
            GTE,
            GtGt,
            Identifier,
            Keyword(Kw::Abs),
            Keyword(Kw::After),
            Keyword(Kw::And),
            Keyword(Kw::Bus),
            Keyword(Kw::Configuration),
            Keyword(Kw::Downto),
            Keyword(Kw::Else),
            Keyword(Kw::End),
            Keyword(Kw::Entity),
            Keyword(Kw::For),
            Keyword(Kw::Generate),
            Keyword(Kw::Generic),
            Keyword(Kw::Inertial),
            Keyword(Kw::Is),
            Keyword(Kw::Loop),
            Keyword(Kw::Mod),
            Keyword(Kw::Nand),
            Keyword(Kw::New),
            Keyword(Kw::Nor),
            Keyword(Kw::Not),
            Keyword(Kw::Null),
            Keyword(Kw::Open),
            Keyword(Kw::Or),
            Keyword(Kw::Port),
            Keyword(Kw::Range),
            Keyword(Kw::Register),
            Keyword(Kw::Rem),
            Keyword(Kw::Report),
            Keyword(Kw::Return),
            Keyword(Kw::Rol),
            Keyword(Kw::Ror),
            Keyword(Kw::Select),
            Keyword(Kw::Severity),
            Keyword(Kw::Sla),
            Keyword(Kw::Sll),
            Keyword(Kw::Sra),
            Keyword(Kw::Srl),
            Keyword(Kw::Then),
            Keyword(Kw::To),
            Keyword(Kw::Units),
            Keyword(Kw::Until),
            Keyword(Kw::Use),
            Keyword(Kw::When),
            Keyword(Kw::Xnor),
            Keyword(Kw::Xor),
            LT,
            LTE,
            LeftPar,
            LeftSquare,
            LtLt,
            Minus,
            NE,
            Plus,
            Pow,
            QueEQ,
            QueGT,
            QueGTE,
            QueLT,
            QueLTE,
            QueNE,
            QueQue,
            RightArrow,
            RightPar,
            RightSquare,
            SemiColon,
            StringLiteral,
            Tick,
            Times,
        ],
        _ => &[],
    }
}
