// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2026, Lukas Scheller lukasscheller@icloud.com
use super::*;
use crate::syntax::meta::Layout;
use crate::syntax::node_kind::NodeKind;
use crate::syntax::AstNode;
#[doc = r" Return the static [`Layout`] for any concrete node kind."]
pub fn layout_of(kind: NodeKind) -> &'static Layout {
    match kind {
        NodeKind::AbsolutePathname => AbsolutePathnameSyntax::META,
        NodeKind::AccessTypeDefinition => AccessTypeDefinitionSyntax::META,
        NodeKind::ActualPart => ActualPartSyntax::META,
        NodeKind::Aggregate => AggregateSyntax::META,
        NodeKind::AggregateTarget => AggregateTargetSyntax::META,
        NodeKind::AliasDeclaration => AliasDeclarationSyntax::META,
        NodeKind::AllSensitivityList => AllSensitivityListSyntax::META,
        NodeKind::ArchitectureBody => ArchitectureBodySyntax::META,
        NodeKind::ArchitectureEpilogue => ArchitectureEpilogueSyntax::META,
        NodeKind::ArchitecturePreamble => ArchitecturePreambleSyntax::META,
        NodeKind::ArrayConstraint => ArrayConstraintSyntax::META,
        NodeKind::Assertion => AssertionSyntax::META,
        NodeKind::AssertionStatement => AssertionStatementSyntax::META,
        NodeKind::AssociationElement => AssociationElementSyntax::META,
        NodeKind::AssociationList => AssociationListSyntax::META,
        NodeKind::AttributeDeclaration => AttributeDeclarationSyntax::META,
        NodeKind::AttributeName => AttributeNameSyntax::META,
        NodeKind::AttributeRange => AttributeRangeSyntax::META,
        NodeKind::AttributeSpecification => AttributeSpecificationSyntax::META,
        NodeKind::BinaryExpression => BinaryExpressionSyntax::META,
        NodeKind::BindingIndication => BindingIndicationSyntax::META,
        NodeKind::BlockConfiguration => BlockConfigurationSyntax::META,
        NodeKind::BlockConfigurationEpilogue => BlockConfigurationEpilogueSyntax::META,
        NodeKind::BlockConfigurationItem => BlockConfigurationItemSyntax::META,
        NodeKind::BlockConfigurationItems => BlockConfigurationItemsSyntax::META,
        NodeKind::BlockConfigurationPreamble => BlockConfigurationPreambleSyntax::META,
        NodeKind::BlockEpilogue => BlockEpilogueSyntax::META,
        NodeKind::BlockHeader => BlockHeaderSyntax::META,
        NodeKind::BlockPreamble => BlockPreambleSyntax::META,
        NodeKind::BlockStatement => BlockStatementSyntax::META,
        NodeKind::CaseGenerateAlternative => CaseGenerateAlternativeSyntax::META,
        NodeKind::CaseGenerateStatement => CaseGenerateStatementSyntax::META,
        NodeKind::CaseGenerateStatementEpilogue => CaseGenerateStatementEpilogueSyntax::META,
        NodeKind::CaseGenerateStatementPreamble => CaseGenerateStatementPreambleSyntax::META,
        NodeKind::CaseStatement => CaseStatementSyntax::META,
        NodeKind::CaseStatementAlternative => CaseStatementAlternativeSyntax::META,
        NodeKind::CaseStatementAlternativePreamble => CaseStatementAlternativePreambleSyntax::META,
        NodeKind::CaseStatementEpilogue => CaseStatementEpilogueSyntax::META,
        NodeKind::CaseStatementPreamble => CaseStatementPreambleSyntax::META,
        NodeKind::Choices => ChoicesSyntax::META,
        NodeKind::ComponentConfiguration => ComponentConfigurationSyntax::META,
        NodeKind::ComponentConfigurationEpilogue => ComponentConfigurationEpilogueSyntax::META,
        NodeKind::ComponentConfigurationItems => ComponentConfigurationItemsSyntax::META,
        NodeKind::ComponentConfigurationPreamble => ComponentConfigurationPreambleSyntax::META,
        NodeKind::ComponentDeclaration => ComponentDeclarationSyntax::META,
        NodeKind::ComponentDeclarationEpilogue => ComponentDeclarationEpilogueSyntax::META,
        NodeKind::ComponentDeclarationItems => ComponentDeclarationItemsSyntax::META,
        NodeKind::ComponentDeclarationPreamble => ComponentDeclarationPreambleSyntax::META,
        NodeKind::ComponentInstantiatedUnit => ComponentInstantiatedUnitSyntax::META,
        NodeKind::ComponentInstantiationItems => ComponentInstantiationItemsSyntax::META,
        NodeKind::ComponentInstantiationStatement => ComponentInstantiationStatementSyntax::META,
        NodeKind::ComponentSpecification => ComponentSpecificationSyntax::META,
        NodeKind::CompoundConfigurationSpecification => {
            CompoundConfigurationSpecificationSyntax::META
        }
        NodeKind::CompoundConfigurationSpecificationItems => {
            CompoundConfigurationSpecificationItemsSyntax::META
        }
        NodeKind::ConcurrentAssertionStatement => ConcurrentAssertionStatementSyntax::META,
        NodeKind::ConcurrentConditionalSignalAssignment => {
            ConcurrentConditionalSignalAssignmentSyntax::META
        }
        NodeKind::ConcurrentProcedureCallOrComponentInstantiationStatement => {
            ConcurrentProcedureCallOrComponentInstantiationStatementSyntax::META
        }
        NodeKind::ConcurrentSelectedSignalAssignment => {
            ConcurrentSelectedSignalAssignmentSyntax::META
        }
        NodeKind::ConcurrentSelectedSignalAssignmentPreamble => {
            ConcurrentSelectedSignalAssignmentPreambleSyntax::META
        }
        NodeKind::ConcurrentSimpleSignalAssignment => ConcurrentSimpleSignalAssignmentSyntax::META,
        NodeKind::ConcurrentStatements => ConcurrentStatementsSyntax::META,
        NodeKind::ConditionClause => ConditionClauseSyntax::META,
        NodeKind::ConditionalElseItem => ConditionalElseItemSyntax::META,
        NodeKind::ConditionalElseWhenExpression => ConditionalElseWhenExpressionSyntax::META,
        NodeKind::ConditionalExpression => ConditionalExpressionSyntax::META,
        NodeKind::ConditionalExpressions => ConditionalExpressionsSyntax::META,
        NodeKind::ConditionalForceAssignment => ConditionalForceAssignmentSyntax::META,
        NodeKind::ConditionalVariableAssignment => ConditionalVariableAssignmentSyntax::META,
        NodeKind::ConditionalWaveform => ConditionalWaveformSyntax::META,
        NodeKind::ConditionalWaveformAssignment => ConditionalWaveformAssignmentSyntax::META,
        NodeKind::ConditionalWaveformElseItem => ConditionalWaveformElseItemSyntax::META,
        NodeKind::ConditionalWaveformElseWhenExpression => {
            ConditionalWaveformElseWhenExpressionSyntax::META
        }
        NodeKind::ConditionalWaveforms => ConditionalWaveformsSyntax::META,
        NodeKind::ConfigurationDeclaration => ConfigurationDeclarationSyntax::META,
        NodeKind::ConfigurationDeclarationEpilogue => ConfigurationDeclarationEpilogueSyntax::META,
        NodeKind::ConfigurationDeclarationItems => ConfigurationDeclarationItemsSyntax::META,
        NodeKind::ConfigurationDeclarationPreamble => ConfigurationDeclarationPreambleSyntax::META,
        NodeKind::ConfigurationInstantiatedUnit => ConfigurationInstantiatedUnitSyntax::META,
        NodeKind::ConstantDeclaration => ConstantDeclarationSyntax::META,
        NodeKind::ConstrainedArrayDefinition => ConstrainedArrayDefinitionSyntax::META,
        NodeKind::ContextClause => ContextClauseSyntax::META,
        NodeKind::ContextDeclaration => ContextDeclarationSyntax::META,
        NodeKind::ContextDeclarationEpilogue => ContextDeclarationEpilogueSyntax::META,
        NodeKind::ContextDeclarationPreamble => ContextDeclarationPreambleSyntax::META,
        NodeKind::ContextReference => ContextReferenceSyntax::META,
        NodeKind::DeclarationStatementSeparator => DeclarationStatementSeparatorSyntax::META,
        NodeKind::Declarations => DeclarationsSyntax::META,
        NodeKind::DesignFile => DesignFileSyntax::META,
        NodeKind::DesignUnit => DesignUnitSyntax::META,
        NodeKind::DisconnectionSpecification => DisconnectionSpecificationSyntax::META,
        NodeKind::DiscreteRangeChoice => DiscreteRangeChoiceSyntax::META,
        NodeKind::ElementAssociation => ElementAssociationSyntax::META,
        NodeKind::ElementDeclaration => ElementDeclarationSyntax::META,
        NodeKind::ElementResolutionResolutionIndication => {
            ElementResolutionResolutionIndicationSyntax::META
        }
        NodeKind::EntityClassEntry => EntityClassEntrySyntax::META,
        NodeKind::EntityClassEntryList => EntityClassEntryListSyntax::META,
        NodeKind::EntityConfigurationAspect => EntityConfigurationAspectSyntax::META,
        NodeKind::EntityDeclaration => EntityDeclarationSyntax::META,
        NodeKind::EntityDeclarationEpilogue => EntityDeclarationEpilogueSyntax::META,
        NodeKind::EntityDeclarationPreamble => EntityDeclarationPreambleSyntax::META,
        NodeKind::EntityDesignator => EntityDesignatorSyntax::META,
        NodeKind::EntityDesignatorList => EntityDesignatorListSyntax::META,
        NodeKind::EntityEntityAspect => EntityEntityAspectSyntax::META,
        NodeKind::EntityHeader => EntityHeaderSyntax::META,
        NodeKind::EntityInstantiatedUnit => EntityInstantiatedUnitSyntax::META,
        NodeKind::EntityNameListAll => EntityNameListAllSyntax::META,
        NodeKind::EntityNameListOthers => EntityNameListOthersSyntax::META,
        NodeKind::EntityOpenAspect => EntityOpenAspectSyntax::META,
        NodeKind::EntitySpecification => EntitySpecificationSyntax::META,
        NodeKind::EnumerationTypeDefinition => EnumerationTypeDefinitionSyntax::META,
        NodeKind::ExitStatement => ExitStatementSyntax::META,
        NodeKind::ExpressionAllocator => ExpressionAllocatorSyntax::META,
        NodeKind::ExpressionChoice => ExpressionChoiceSyntax::META,
        NodeKind::ExternalConstantName => ExternalConstantNameSyntax::META,
        NodeKind::ExternalSignalName => ExternalSignalNameSyntax::META,
        NodeKind::ExternalVariableName => ExternalVariableNameSyntax::META,
        NodeKind::FileDeclaration => FileDeclarationSyntax::META,
        NodeKind::FileOpenInformation => FileOpenInformationSyntax::META,
        NodeKind::FileTypeDefinition => FileTypeDefinitionSyntax::META,
        NodeKind::ForGenerateStatement => ForGenerateStatementSyntax::META,
        NodeKind::ForGenerateStatementEpilogue => ForGenerateStatementEpilogueSyntax::META,
        NodeKind::ForGenerateStatementPreamble => ForGenerateStatementPreambleSyntax::META,
        NodeKind::ForIterationScheme => ForIterationSchemeSyntax::META,
        NodeKind::FormalPart => FormalPartSyntax::META,
        NodeKind::FullTypeDeclaration => FullTypeDeclarationSyntax::META,
        NodeKind::FunctionSpecification => FunctionSpecificationSyntax::META,
        NodeKind::GenerateStatementBody => GenerateStatementBodySyntax::META,
        NodeKind::GenerateStatementBodyEpilogue => GenerateStatementBodyEpilogueSyntax::META,
        NodeKind::GenericClause => GenericClauseSyntax::META,
        NodeKind::GenericClauseEpilogue => GenericClauseEpilogueSyntax::META,
        NodeKind::GenericClausePreamble => GenericClausePreambleSyntax::META,
        NodeKind::GenericMapAspect => GenericMapAspectSyntax::META,
        NodeKind::GroupConstituentList => GroupConstituentListSyntax::META,
        NodeKind::GroupDeclaration => GroupDeclarationSyntax::META,
        NodeKind::GroupTemplateDeclaration => GroupTemplateDeclarationSyntax::META,
        NodeKind::GuardedSignalSpecification => GuardedSignalSpecificationSyntax::META,
        NodeKind::IdentifierList => IdentifierListSyntax::META,
        NodeKind::IfGenerateElse => IfGenerateElseSyntax::META,
        NodeKind::IfGenerateElsif => IfGenerateElsifSyntax::META,
        NodeKind::IfGenerateStatement => IfGenerateStatementSyntax::META,
        NodeKind::IfGenerateStatementEpilogue => IfGenerateStatementEpilogueSyntax::META,
        NodeKind::IfGenerateStatementPreamble => IfGenerateStatementPreambleSyntax::META,
        NodeKind::IfStatement => IfStatementSyntax::META,
        NodeKind::IfStatementElse => IfStatementElseSyntax::META,
        NodeKind::IfStatementElsif => IfStatementElsifSyntax::META,
        NodeKind::IfStatementEpilogue => IfStatementEpilogueSyntax::META,
        NodeKind::IfStatementPreamble => IfStatementPreambleSyntax::META,
        NodeKind::IncompleteTypeDeclaration => IncompleteTypeDeclarationSyntax::META,
        NodeKind::IndexConstraint => IndexConstraintSyntax::META,
        NodeKind::IndexSubtypeDefinition => IndexSubtypeDefinitionSyntax::META,
        NodeKind::IndexSubtypeDefinitionList => IndexSubtypeDefinitionListSyntax::META,
        NodeKind::InertialDelayMechanism => InertialDelayMechanismSyntax::META,
        NodeKind::InstantiationListAll => InstantiationListAllSyntax::META,
        NodeKind::InstantiationListList => InstantiationListListSyntax::META,
        NodeKind::InstantiationListOthers => InstantiationListOthersSyntax::META,
        NodeKind::InterfaceConstantDeclaration => InterfaceConstantDeclarationSyntax::META,
        NodeKind::InterfaceFileDeclaration => InterfaceFileDeclarationSyntax::META,
        NodeKind::InterfaceFunctionSpecification => InterfaceFunctionSpecificationSyntax::META,
        NodeKind::InterfaceIncompleteTypeDeclaration => {
            InterfaceIncompleteTypeDeclarationSyntax::META
        }
        NodeKind::InterfaceList => InterfaceListSyntax::META,
        NodeKind::InterfacePackageDeclaration => InterfacePackageDeclarationSyntax::META,
        NodeKind::InterfacePackageDeclarationPreamble => {
            InterfacePackageDeclarationPreambleSyntax::META
        }
        NodeKind::InterfacePackageGenericMapAspect => InterfacePackageGenericMapAspectSyntax::META,
        NodeKind::InterfacePackageGenericMapAspectAssociations => {
            InterfacePackageGenericMapAspectAssociationsSyntax::META
        }
        NodeKind::InterfacePackageGenericMapAspectBox => {
            InterfacePackageGenericMapAspectBoxSyntax::META
        }
        NodeKind::InterfacePackageGenericMapAspectDefault => {
            InterfacePackageGenericMapAspectDefaultSyntax::META
        }
        NodeKind::InterfaceProcedureSpecification => InterfaceProcedureSpecificationSyntax::META,
        NodeKind::InterfaceSignalDeclaration => InterfaceSignalDeclarationSyntax::META,
        NodeKind::InterfaceSubprogramDeclaration => InterfaceSubprogramDeclarationSyntax::META,
        NodeKind::InterfaceSubprogramDefaultBox => InterfaceSubprogramDefaultBoxSyntax::META,
        NodeKind::InterfaceSubprogramDefaultName => InterfaceSubprogramDefaultNameSyntax::META,
        NodeKind::InterfaceVariableDeclaration => InterfaceVariableDeclarationSyntax::META,
        NodeKind::Label => LabelSyntax::META,
        NodeKind::LibraryClause => LibraryClauseSyntax::META,
        NodeKind::LiteralExpression => LiteralExpressionSyntax::META,
        NodeKind::LoopStatement => LoopStatementSyntax::META,
        NodeKind::LoopStatementEpilogue => LoopStatementEpilogueSyntax::META,
        NodeKind::LoopStatementPreamble => LoopStatementPreambleSyntax::META,
        NodeKind::Name => NameSyntax::META,
        NodeKind::NameDesignatorPrefix => NameDesignatorPrefixSyntax::META,
        NodeKind::NameExpression => NameExpressionSyntax::META,
        NodeKind::NameList => NameListSyntax::META,
        NodeKind::NameResolutionIndication => NameResolutionIndicationSyntax::META,
        NodeKind::NameTarget => NameTargetSyntax::META,
        NodeKind::NextStatement => NextStatementSyntax::META,
        NodeKind::NullStatement => NullStatementSyntax::META,
        NodeKind::NumericTypeDefinition => NumericTypeDefinitionSyntax::META,
        NodeKind::OpenDiscreteRange => OpenDiscreteRangeSyntax::META,
        NodeKind::OthersChoice => OthersChoiceSyntax::META,
        NodeKind::Package => PackageSyntax::META,
        NodeKind::PackageBody => PackageBodySyntax::META,
        NodeKind::PackageBodyDeclaration => PackageBodyDeclarationSyntax::META,
        NodeKind::PackageBodyEpilogue => PackageBodyEpilogueSyntax::META,
        NodeKind::PackageBodyPreamble => PackageBodyPreambleSyntax::META,
        NodeKind::PackageDeclaration => PackageDeclarationSyntax::META,
        NodeKind::PackageEpilogue => PackageEpilogueSyntax::META,
        NodeKind::PackageHeader => PackageHeaderSyntax::META,
        NodeKind::PackageInstantiation => PackageInstantiationSyntax::META,
        NodeKind::PackageInstantiationDeclaration => PackageInstantiationDeclarationSyntax::META,
        NodeKind::PackageInstantiationDeclarationPrimaryUnit => {
            PackageInstantiationDeclarationPrimaryUnitSyntax::META
        }
        NodeKind::PackageInstantiationPreamble => PackageInstantiationPreambleSyntax::META,
        NodeKind::PackagePathname => PackagePathnameSyntax::META,
        NodeKind::PackagePreamble => PackagePreambleSyntax::META,
        NodeKind::ParameterList => ParameterListSyntax::META,
        NodeKind::ParameterSpecification => ParameterSpecificationSyntax::META,
        NodeKind::ParenthesizedElementResolutionResolutionIndication => {
            ParenthesizedElementResolutionResolutionIndicationSyntax::META
        }
        NodeKind::ParenthesizedExpression => ParenthesizedExpressionSyntax::META,
        NodeKind::ParenthesizedExpressionOrAggregate => {
            ParenthesizedExpressionOrAggregateSyntax::META
        }
        NodeKind::ParenthesizedInterfaceList => ParenthesizedInterfaceListSyntax::META,
        NodeKind::ParenthesizedName => ParenthesizedNameSyntax::META,
        NodeKind::ParenthesizedProcessSensitivityList => {
            ParenthesizedProcessSensitivityListSyntax::META
        }
        NodeKind::PartialPathname => PartialPathnameSyntax::META,
        NodeKind::PhysicalLiteral => PhysicalLiteralSyntax::META,
        NodeKind::PhysicalLiteralExpression => PhysicalLiteralExpressionSyntax::META,
        NodeKind::PhysicalTypeDefinition => PhysicalTypeDefinitionSyntax::META,
        NodeKind::PhysicalTypeDefinitionEpilogue => PhysicalTypeDefinitionEpilogueSyntax::META,
        NodeKind::PortClause => PortClauseSyntax::META,
        NodeKind::PortClauseEpilogue => PortClauseEpilogueSyntax::META,
        NodeKind::PortClausePreamble => PortClausePreambleSyntax::META,
        NodeKind::PortMapAspect => PortMapAspectSyntax::META,
        NodeKind::PrimaryUnitDeclaration => PrimaryUnitDeclarationSyntax::META,
        NodeKind::PrimaryUnitPackageDeclaration => PrimaryUnitPackageDeclarationSyntax::META,
        NodeKind::ProcedureCallStatement => ProcedureCallStatementSyntax::META,
        NodeKind::ProcedureSpecification => ProcedureSpecificationSyntax::META,
        NodeKind::ProcessStatement => ProcessStatementSyntax::META,
        NodeKind::ProcessStatementEpilogue => ProcessStatementEpilogueSyntax::META,
        NodeKind::ProcessStatementPreamble => ProcessStatementPreambleSyntax::META,
        NodeKind::ProtectedTypeBody => ProtectedTypeBodySyntax::META,
        NodeKind::ProtectedTypeBodyEpilogue => ProtectedTypeBodyEpilogueSyntax::META,
        NodeKind::ProtectedTypeBodyPreamble => ProtectedTypeBodyPreambleSyntax::META,
        NodeKind::ProtectedTypeDeclaration => ProtectedTypeDeclarationSyntax::META,
        NodeKind::ProtectedTypeDeclarationEpilogue => ProtectedTypeDeclarationEpilogueSyntax::META,
        NodeKind::ProtectedTypeDeclarationPreamble => ProtectedTypeDeclarationPreambleSyntax::META,
        NodeKind::PslClockDeclaration => PslClockDeclarationSyntax::META,
        NodeKind::PslDirective => PslDirectiveSyntax::META,
        NodeKind::PslPropertyDeclaration => PslPropertyDeclarationSyntax::META,
        NodeKind::PslSequenceDeclaration => PslSequenceDeclarationSyntax::META,
        NodeKind::PslVerificationUnit => PslVerificationUnitSyntax::META,
        NodeKind::QualifiedExpression => QualifiedExpressionSyntax::META,
        NodeKind::RangeConstraint => RangeConstraintSyntax::META,
        NodeKind::RangeConstraintConstraint => RangeConstraintConstraintSyntax::META,
        NodeKind::RangeExpression => RangeExpressionSyntax::META,
        NodeKind::RawTokens => RawTokensSyntax::META,
        NodeKind::RecordConstraint => RecordConstraintSyntax::META,
        NodeKind::RecordElementConstraint => RecordElementConstraintSyntax::META,
        NodeKind::RecordElementDeclarations => RecordElementDeclarationsSyntax::META,
        NodeKind::RecordElementResolution => RecordElementResolutionSyntax::META,
        NodeKind::RecordResolution => RecordResolutionSyntax::META,
        NodeKind::RecordResolutionElementResolution => {
            RecordResolutionElementResolutionSyntax::META
        }
        NodeKind::RecordTypeDefinition => RecordTypeDefinitionSyntax::META,
        NodeKind::RecordTypeDefinitionEpilogue => RecordTypeDefinitionEpilogueSyntax::META,
        NodeKind::RecordTypeDefinitionPreamble => RecordTypeDefinitionPreambleSyntax::META,
        NodeKind::RelativePathname => RelativePathnameSyntax::META,
        NodeKind::ReportStatement => ReportStatementSyntax::META,
        NodeKind::ResolutionIndicationElementResolution => {
            ResolutionIndicationElementResolutionSyntax::META
        }
        NodeKind::ReturnStatement => ReturnStatementSyntax::META,
        NodeKind::SecondaryUnitDeclaration => SecondaryUnitDeclarationSyntax::META,
        NodeKind::SecondaryUnitPackageBody => SecondaryUnitPackageBodySyntax::META,
        NodeKind::SelectedAssignmentPreamble => SelectedAssignmentPreambleSyntax::META,
        NodeKind::SelectedExpressionItem => SelectedExpressionItemSyntax::META,
        NodeKind::SelectedExpressions => SelectedExpressionsSyntax::META,
        NodeKind::SelectedForceAssignment => SelectedForceAssignmentSyntax::META,
        NodeKind::SelectedName => SelectedNameSyntax::META,
        NodeKind::SelectedVariableAssignment => SelectedVariableAssignmentSyntax::META,
        NodeKind::SelectedWaveformAssignment => SelectedWaveformAssignmentSyntax::META,
        NodeKind::SelectedWaveformItem => SelectedWaveformItemSyntax::META,
        NodeKind::SelectedWaveforms => SelectedWaveformsSyntax::META,
        NodeKind::SemiColonTerminatedBindingIndication => {
            SemiColonTerminatedBindingIndicationSyntax::META
        }
        NodeKind::SemiColonTerminatedGenericMapAspect => {
            SemiColonTerminatedGenericMapAspectSyntax::META
        }
        NodeKind::SemiColonTerminatedPortMapAspect => SemiColonTerminatedPortMapAspectSyntax::META,
        NodeKind::SemiColonTerminatedVerificationUnitBindingIndication => {
            SemiColonTerminatedVerificationUnitBindingIndicationSyntax::META
        }
        NodeKind::SensitivityClause => SensitivityClauseSyntax::META,
        NodeKind::SensitivityList => SensitivityListSyntax::META,
        NodeKind::SequentialStatements => SequentialStatementsSyntax::META,
        NodeKind::SharedVariableDeclaration => SharedVariableDeclarationSyntax::META,
        NodeKind::SignalDeclaration => SignalDeclarationSyntax::META,
        NodeKind::SignalListAll => SignalListAllSyntax::META,
        NodeKind::SignalListList => SignalListListSyntax::META,
        NodeKind::SignalListOthers => SignalListOthersSyntax::META,
        NodeKind::Signature => SignatureSyntax::META,
        NodeKind::SimpleConfigurationSpecification => SimpleConfigurationSpecificationSyntax::META,
        NodeKind::SimpleForceAssignment => SimpleForceAssignmentSyntax::META,
        NodeKind::SimpleReleaseAssignment => SimpleReleaseAssignmentSyntax::META,
        NodeKind::SimpleVariableAssignment => SimpleVariableAssignmentSyntax::META,
        NodeKind::SimpleWaveformAssignment => SimpleWaveformAssignmentSyntax::META,
        NodeKind::SubprogramBody => SubprogramBodySyntax::META,
        NodeKind::SubprogramBodyEpilogue => SubprogramBodyEpilogueSyntax::META,
        NodeKind::SubprogramBodyPreamble => SubprogramBodyPreambleSyntax::META,
        NodeKind::SubprogramDeclaration => SubprogramDeclarationSyntax::META,
        NodeKind::SubprogramHeader => SubprogramHeaderSyntax::META,
        NodeKind::SubprogramHeaderGenericClause => SubprogramHeaderGenericClauseSyntax::META,
        NodeKind::SubprogramInstantiationDeclaration => {
            SubprogramInstantiationDeclarationSyntax::META
        }
        NodeKind::SubprogramInstantiationDeclarationPreamble => {
            SubprogramInstantiationDeclarationPreambleSyntax::META
        }
        NodeKind::SubtypeDeclaration => SubtypeDeclarationSyntax::META,
        NodeKind::SubtypeIndication => SubtypeIndicationSyntax::META,
        NodeKind::SubtypeIndicationAllocator => SubtypeIndicationAllocatorSyntax::META,
        NodeKind::SubtypeIndicationDiscreteDiscreteRange => {
            SubtypeIndicationDiscreteDiscreteRangeSyntax::META
        }
        NodeKind::SubtypeIndicationDiscreteRange => SubtypeIndicationDiscreteRangeSyntax::META,
        NodeKind::TimeoutClause => TimeoutClauseSyntax::META,
        NodeKind::TransportDelayMechanism => TransportDelayMechanismSyntax::META,
        NodeKind::TypeConversion => TypeConversionSyntax::META,
        NodeKind::UnaffectedWaveform => UnaffectedWaveformSyntax::META,
        NodeKind::UnaryExpression => UnaryExpressionSyntax::META,
        NodeKind::UnboundedArrayDefinition => UnboundedArrayDefinitionSyntax::META,
        NodeKind::UnitDeclarations => UnitDeclarationsSyntax::META,
        NodeKind::UseClause => UseClauseSyntax::META,
        NodeKind::UseClauseContextItem => UseClauseContextItemSyntax::META,
        NodeKind::UseClauseDeclaration => UseClauseDeclarationSyntax::META,
        NodeKind::VariableDeclaration => VariableDeclarationSyntax::META,
        NodeKind::VerificationUnitBindingIndication => {
            VerificationUnitBindingIndicationSyntax::META
        }
        NodeKind::VerificationUnitList => VerificationUnitListSyntax::META,
        NodeKind::WaitStatement => WaitStatementSyntax::META,
        NodeKind::WaveformElement => WaveformElementSyntax::META,
        NodeKind::WaveformElements => WaveformElementsSyntax::META,
        NodeKind::WhileIterationScheme => WhileIterationSchemeSyntax::META,
    }
}
