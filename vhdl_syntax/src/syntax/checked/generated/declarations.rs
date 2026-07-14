// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2026, Lukas Scheller lukasscheller@icloud.com
use super::*;
use crate::syntax::checked::CheckedNode;
use crate::syntax::node::SyntaxToken;
use crate::syntax::*;
#[derive(Debug, Clone)]
pub enum CheckedDeclaration {
    SubprogramDeclaration(CheckedSubprogramDeclaration),
    SubprogramBody(CheckedSubprogramBody),
    SubprogramInstantiationDeclaration(CheckedSubprogramInstantiationDeclaration),
    PackageDeclaration(CheckedPackageDeclaration),
    PackageBodyDeclaration(CheckedPackageBodyDeclaration),
    PackageInstantiationDeclaration(CheckedPackageInstantiationDeclaration),
    TypeDeclaration(CheckedTypeDeclaration),
    SubtypeDeclaration(CheckedSubtypeDeclaration),
    FileDeclaration(CheckedFileDeclaration),
    AliasDeclaration(CheckedAliasDeclaration),
    ComponentDeclaration(CheckedComponentDeclaration),
    AttributeDeclaration(CheckedAttributeDeclaration),
    AttributeSpecification(CheckedAttributeSpecification),
    ConfigurationSpecification(CheckedConfigurationSpecification),
    DisconnectionSpecification(CheckedDisconnectionSpecification),
    UseClauseDeclaration(CheckedUseClauseDeclaration),
    GroupTemplateDeclaration(CheckedGroupTemplateDeclaration),
    GroupDeclaration(CheckedGroupDeclaration),
    ConstantDeclaration(CheckedConstantDeclaration),
    SignalDeclaration(CheckedSignalDeclaration),
    VariableDeclaration(CheckedVariableDeclaration),
    SharedVariableDeclaration(CheckedSharedVariableDeclaration),
    PslPropertyDeclaration(CheckedPslPropertyDeclaration),
    PslSequenceDeclaration(CheckedPslSequenceDeclaration),
    PslClockDeclaration(CheckedPslClockDeclaration),
}
impl CheckedNode for CheckedDeclaration {
    type Syntax = DeclarationSyntax;
    fn cast_unchecked(syntax: DeclarationSyntax) -> Self {
        match syntax {
            DeclarationSyntax::SubprogramDeclaration(inner) => {
                CheckedDeclaration::SubprogramDeclaration(
                    CheckedSubprogramDeclaration::cast_unchecked(inner),
                )
            }
            DeclarationSyntax::SubprogramBody(inner) => {
                CheckedDeclaration::SubprogramBody(CheckedSubprogramBody::cast_unchecked(inner))
            }
            DeclarationSyntax::SubprogramInstantiationDeclaration(inner) => {
                CheckedDeclaration::SubprogramInstantiationDeclaration(
                    CheckedSubprogramInstantiationDeclaration::cast_unchecked(inner),
                )
            }
            DeclarationSyntax::PackageDeclaration(inner) => CheckedDeclaration::PackageDeclaration(
                CheckedPackageDeclaration::cast_unchecked(inner),
            ),
            DeclarationSyntax::PackageBodyDeclaration(inner) => {
                CheckedDeclaration::PackageBodyDeclaration(
                    CheckedPackageBodyDeclaration::cast_unchecked(inner),
                )
            }
            DeclarationSyntax::PackageInstantiationDeclaration(inner) => {
                CheckedDeclaration::PackageInstantiationDeclaration(
                    CheckedPackageInstantiationDeclaration::cast_unchecked(inner),
                )
            }
            DeclarationSyntax::TypeDeclaration(inner) => {
                CheckedDeclaration::TypeDeclaration(CheckedTypeDeclaration::cast_unchecked(inner))
            }
            DeclarationSyntax::SubtypeDeclaration(inner) => CheckedDeclaration::SubtypeDeclaration(
                CheckedSubtypeDeclaration::cast_unchecked(inner),
            ),
            DeclarationSyntax::FileDeclaration(inner) => {
                CheckedDeclaration::FileDeclaration(CheckedFileDeclaration::cast_unchecked(inner))
            }
            DeclarationSyntax::AliasDeclaration(inner) => {
                CheckedDeclaration::AliasDeclaration(CheckedAliasDeclaration::cast_unchecked(inner))
            }
            DeclarationSyntax::ComponentDeclaration(inner) => {
                CheckedDeclaration::ComponentDeclaration(
                    CheckedComponentDeclaration::cast_unchecked(inner),
                )
            }
            DeclarationSyntax::AttributeDeclaration(inner) => {
                CheckedDeclaration::AttributeDeclaration(
                    CheckedAttributeDeclaration::cast_unchecked(inner),
                )
            }
            DeclarationSyntax::AttributeSpecification(inner) => {
                CheckedDeclaration::AttributeSpecification(
                    CheckedAttributeSpecification::cast_unchecked(inner),
                )
            }
            DeclarationSyntax::ConfigurationSpecification(inner) => {
                CheckedDeclaration::ConfigurationSpecification(
                    CheckedConfigurationSpecification::cast_unchecked(inner),
                )
            }
            DeclarationSyntax::DisconnectionSpecification(inner) => {
                CheckedDeclaration::DisconnectionSpecification(
                    CheckedDisconnectionSpecification::cast_unchecked(inner),
                )
            }
            DeclarationSyntax::UseClauseDeclaration(inner) => {
                CheckedDeclaration::UseClauseDeclaration(
                    CheckedUseClauseDeclaration::cast_unchecked(inner),
                )
            }
            DeclarationSyntax::GroupTemplateDeclaration(inner) => {
                CheckedDeclaration::GroupTemplateDeclaration(
                    CheckedGroupTemplateDeclaration::cast_unchecked(inner),
                )
            }
            DeclarationSyntax::GroupDeclaration(inner) => {
                CheckedDeclaration::GroupDeclaration(CheckedGroupDeclaration::cast_unchecked(inner))
            }
            DeclarationSyntax::ConstantDeclaration(inner) => {
                CheckedDeclaration::ConstantDeclaration(CheckedConstantDeclaration::cast_unchecked(
                    inner,
                ))
            }
            DeclarationSyntax::SignalDeclaration(inner) => CheckedDeclaration::SignalDeclaration(
                CheckedSignalDeclaration::cast_unchecked(inner),
            ),
            DeclarationSyntax::VariableDeclaration(inner) => {
                CheckedDeclaration::VariableDeclaration(CheckedVariableDeclaration::cast_unchecked(
                    inner,
                ))
            }
            DeclarationSyntax::SharedVariableDeclaration(inner) => {
                CheckedDeclaration::SharedVariableDeclaration(
                    CheckedSharedVariableDeclaration::cast_unchecked(inner),
                )
            }
            DeclarationSyntax::PslPropertyDeclaration(inner) => {
                CheckedDeclaration::PslPropertyDeclaration(
                    CheckedPslPropertyDeclaration::cast_unchecked(inner),
                )
            }
            DeclarationSyntax::PslSequenceDeclaration(inner) => {
                CheckedDeclaration::PslSequenceDeclaration(
                    CheckedPslSequenceDeclaration::cast_unchecked(inner),
                )
            }
            DeclarationSyntax::PslClockDeclaration(inner) => {
                CheckedDeclaration::PslClockDeclaration(CheckedPslClockDeclaration::cast_unchecked(
                    inner,
                ))
            }
        }
    }
    fn raw(&self) -> Self::Syntax {
        match self {
            CheckedDeclaration::SubprogramDeclaration(inner) => {
                DeclarationSyntax::SubprogramDeclaration(inner.raw())
            }
            CheckedDeclaration::SubprogramBody(inner) => {
                DeclarationSyntax::SubprogramBody(inner.raw())
            }
            CheckedDeclaration::SubprogramInstantiationDeclaration(inner) => {
                DeclarationSyntax::SubprogramInstantiationDeclaration(inner.raw())
            }
            CheckedDeclaration::PackageDeclaration(inner) => {
                DeclarationSyntax::PackageDeclaration(inner.raw())
            }
            CheckedDeclaration::PackageBodyDeclaration(inner) => {
                DeclarationSyntax::PackageBodyDeclaration(inner.raw())
            }
            CheckedDeclaration::PackageInstantiationDeclaration(inner) => {
                DeclarationSyntax::PackageInstantiationDeclaration(inner.raw())
            }
            CheckedDeclaration::TypeDeclaration(inner) => {
                DeclarationSyntax::TypeDeclaration(inner.raw())
            }
            CheckedDeclaration::SubtypeDeclaration(inner) => {
                DeclarationSyntax::SubtypeDeclaration(inner.raw())
            }
            CheckedDeclaration::FileDeclaration(inner) => {
                DeclarationSyntax::FileDeclaration(inner.raw())
            }
            CheckedDeclaration::AliasDeclaration(inner) => {
                DeclarationSyntax::AliasDeclaration(inner.raw())
            }
            CheckedDeclaration::ComponentDeclaration(inner) => {
                DeclarationSyntax::ComponentDeclaration(inner.raw())
            }
            CheckedDeclaration::AttributeDeclaration(inner) => {
                DeclarationSyntax::AttributeDeclaration(inner.raw())
            }
            CheckedDeclaration::AttributeSpecification(inner) => {
                DeclarationSyntax::AttributeSpecification(inner.raw())
            }
            CheckedDeclaration::ConfigurationSpecification(inner) => {
                DeclarationSyntax::ConfigurationSpecification(inner.raw())
            }
            CheckedDeclaration::DisconnectionSpecification(inner) => {
                DeclarationSyntax::DisconnectionSpecification(inner.raw())
            }
            CheckedDeclaration::UseClauseDeclaration(inner) => {
                DeclarationSyntax::UseClauseDeclaration(inner.raw())
            }
            CheckedDeclaration::GroupTemplateDeclaration(inner) => {
                DeclarationSyntax::GroupTemplateDeclaration(inner.raw())
            }
            CheckedDeclaration::GroupDeclaration(inner) => {
                DeclarationSyntax::GroupDeclaration(inner.raw())
            }
            CheckedDeclaration::ConstantDeclaration(inner) => {
                DeclarationSyntax::ConstantDeclaration(inner.raw())
            }
            CheckedDeclaration::SignalDeclaration(inner) => {
                DeclarationSyntax::SignalDeclaration(inner.raw())
            }
            CheckedDeclaration::VariableDeclaration(inner) => {
                DeclarationSyntax::VariableDeclaration(inner.raw())
            }
            CheckedDeclaration::SharedVariableDeclaration(inner) => {
                DeclarationSyntax::SharedVariableDeclaration(inner.raw())
            }
            CheckedDeclaration::PslPropertyDeclaration(inner) => {
                DeclarationSyntax::PslPropertyDeclaration(inner.raw())
            }
            CheckedDeclaration::PslSequenceDeclaration(inner) => {
                DeclarationSyntax::PslSequenceDeclaration(inner.raw())
            }
            CheckedDeclaration::PslClockDeclaration(inner) => {
                DeclarationSyntax::PslClockDeclaration(inner.raw())
            }
        }
    }
}
#[derive(Debug, Clone)]
pub struct CheckedPackageDeclaration(PackageDeclarationSyntax);
impl CheckedNode for CheckedPackageDeclaration {
    type Syntax = PackageDeclarationSyntax;
    fn cast_unchecked(syntax: PackageDeclarationSyntax) -> Self {
        CheckedPackageDeclaration(syntax)
    }
    fn raw(&self) -> PackageDeclarationSyntax {
        self.0.clone()
    }
}
impl CheckedPackageDeclaration {
    pub fn package(&self) -> CheckedPackage {
        CheckedPackage::cast_unchecked(self.0.package().unwrap())
    }
}
#[derive(Debug, Clone)]
pub struct CheckedPackageInstantiationDeclaration(PackageInstantiationDeclarationSyntax);
impl CheckedNode for CheckedPackageInstantiationDeclaration {
    type Syntax = PackageInstantiationDeclarationSyntax;
    fn cast_unchecked(syntax: PackageInstantiationDeclarationSyntax) -> Self {
        CheckedPackageInstantiationDeclaration(syntax)
    }
    fn raw(&self) -> PackageInstantiationDeclarationSyntax {
        self.0.clone()
    }
}
impl CheckedPackageInstantiationDeclaration {
    pub fn package_instantiation(&self) -> CheckedPackageInstantiation {
        CheckedPackageInstantiation::cast_unchecked(self.0.package_instantiation().unwrap())
    }
}
#[derive(Debug, Clone)]
pub struct CheckedPackageBodyDeclaration(PackageBodyDeclarationSyntax);
impl CheckedNode for CheckedPackageBodyDeclaration {
    type Syntax = PackageBodyDeclarationSyntax;
    fn cast_unchecked(syntax: PackageBodyDeclarationSyntax) -> Self {
        CheckedPackageBodyDeclaration(syntax)
    }
    fn raw(&self) -> PackageBodyDeclarationSyntax {
        self.0.clone()
    }
}
impl CheckedPackageBodyDeclaration {
    pub fn package_body(&self) -> CheckedPackageBody {
        CheckedPackageBody::cast_unchecked(self.0.package_body().unwrap())
    }
}
#[derive(Debug, Clone)]
pub struct CheckedUseClauseDeclaration(UseClauseDeclarationSyntax);
impl CheckedNode for CheckedUseClauseDeclaration {
    type Syntax = UseClauseDeclarationSyntax;
    fn cast_unchecked(syntax: UseClauseDeclarationSyntax) -> Self {
        CheckedUseClauseDeclaration(syntax)
    }
    fn raw(&self) -> UseClauseDeclarationSyntax {
        self.0.clone()
    }
}
impl CheckedUseClauseDeclaration {
    pub fn use_clause(&self) -> CheckedUseClause {
        CheckedUseClause::cast_unchecked(self.0.use_clause().unwrap())
    }
}
#[derive(Debug, Clone)]
pub struct CheckedActualPart(ActualPartSyntax);
impl CheckedNode for CheckedActualPart {
    type Syntax = ActualPartSyntax;
    fn cast_unchecked(syntax: ActualPartSyntax) -> Self {
        CheckedActualPart(syntax)
    }
    fn raw(&self) -> ActualPartSyntax {
        self.0.clone()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedAliasDeclaration(AliasDeclarationSyntax);
impl CheckedNode for CheckedAliasDeclaration {
    type Syntax = AliasDeclarationSyntax;
    fn cast_unchecked(syntax: AliasDeclarationSyntax) -> Self {
        CheckedAliasDeclaration(syntax)
    }
    fn raw(&self) -> AliasDeclarationSyntax {
        self.0.clone()
    }
}
impl CheckedAliasDeclaration {
    pub fn alias_token(&self) -> SyntaxToken {
        self.0.alias_token().unwrap()
    }
    pub fn alias_designator(&self) -> AliasDesignatorSyntax {
        self.0.alias_designator().unwrap()
    }
    pub fn colon_token(&self) -> Option<SyntaxToken> {
        self.0.colon_token()
    }
    pub fn subtype_indication(&self) -> Option<CheckedSubtypeIndication> {
        self.0
            .subtype_indication()
            .map(CheckedSubtypeIndication::cast_unchecked)
    }
    pub fn is_token(&self) -> SyntaxToken {
        self.0.is_token().unwrap()
    }
    pub fn name(&self) -> CheckedName {
        CheckedName::cast_unchecked(self.0.name().unwrap())
    }
    pub fn signature(&self) -> Option<CheckedSignature> {
        self.0.signature().map(CheckedSignature::cast_unchecked)
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.0.semi_colon_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedAssociationElement(AssociationElementSyntax);
impl CheckedNode for CheckedAssociationElement {
    type Syntax = AssociationElementSyntax;
    fn cast_unchecked(syntax: AssociationElementSyntax) -> Self {
        CheckedAssociationElement(syntax)
    }
    fn raw(&self) -> AssociationElementSyntax {
        self.0.clone()
    }
}
impl CheckedAssociationElement {
    pub fn formal_part(&self) -> Option<CheckedFormalPart> {
        self.0.formal_part().map(CheckedFormalPart::cast_unchecked)
    }
    pub fn right_arrow_token(&self) -> Option<SyntaxToken> {
        self.0.right_arrow_token()
    }
    pub fn actual_part(&self) -> CheckedActualPart {
        CheckedActualPart::cast_unchecked(self.0.actual_part().unwrap())
    }
}
#[derive(Debug, Clone)]
pub struct CheckedAssociationList(AssociationListSyntax);
impl CheckedNode for CheckedAssociationList {
    type Syntax = AssociationListSyntax;
    fn cast_unchecked(syntax: AssociationListSyntax) -> Self {
        CheckedAssociationList(syntax)
    }
    fn raw(&self) -> AssociationListSyntax {
        self.0.clone()
    }
}
impl CheckedAssociationList {
    pub fn association_elements(
        &self,
    ) -> impl Iterator<Item = CheckedAssociationElement> + use<'_> {
        self.0
            .association_elements()
            .map(CheckedAssociationElement::cast_unchecked)
    }
    pub fn comma_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0.comma_token()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedAttributeDeclaration(AttributeDeclarationSyntax);
impl CheckedNode for CheckedAttributeDeclaration {
    type Syntax = AttributeDeclarationSyntax;
    fn cast_unchecked(syntax: AttributeDeclarationSyntax) -> Self {
        CheckedAttributeDeclaration(syntax)
    }
    fn raw(&self) -> AttributeDeclarationSyntax {
        self.0.clone()
    }
}
impl CheckedAttributeDeclaration {
    pub fn attribute_token(&self) -> SyntaxToken {
        self.0.attribute_token().unwrap()
    }
    pub fn identifier_token(&self) -> SyntaxToken {
        self.0.identifier_token().unwrap()
    }
    pub fn colon_token(&self) -> SyntaxToken {
        self.0.colon_token().unwrap()
    }
    pub fn name(&self) -> CheckedName {
        CheckedName::cast_unchecked(self.0.name().unwrap())
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.0.semi_colon_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedComponentDeclaration(ComponentDeclarationSyntax);
impl CheckedNode for CheckedComponentDeclaration {
    type Syntax = ComponentDeclarationSyntax;
    fn cast_unchecked(syntax: ComponentDeclarationSyntax) -> Self {
        CheckedComponentDeclaration(syntax)
    }
    fn raw(&self) -> ComponentDeclarationSyntax {
        self.0.clone()
    }
}
impl CheckedComponentDeclaration {
    pub fn component_declaration_preamble(&self) -> CheckedComponentDeclarationPreamble {
        CheckedComponentDeclarationPreamble::cast_unchecked(
            self.0.component_declaration_preamble().unwrap(),
        )
    }
    pub fn component_declaration_items(&self) -> Option<CheckedComponentDeclarationItems> {
        self.0
            .component_declaration_items()
            .map(CheckedComponentDeclarationItems::cast_unchecked)
    }
    pub fn component_declaration_epilogue(&self) -> CheckedComponentDeclarationEpilogue {
        CheckedComponentDeclarationEpilogue::cast_unchecked(
            self.0.component_declaration_epilogue().unwrap(),
        )
    }
}
#[derive(Debug, Clone)]
pub struct CheckedComponentDeclarationItems(ComponentDeclarationItemsSyntax);
impl CheckedNode for CheckedComponentDeclarationItems {
    type Syntax = ComponentDeclarationItemsSyntax;
    fn cast_unchecked(syntax: ComponentDeclarationItemsSyntax) -> Self {
        CheckedComponentDeclarationItems(syntax)
    }
    fn raw(&self) -> ComponentDeclarationItemsSyntax {
        self.0.clone()
    }
}
impl CheckedComponentDeclarationItems {
    pub fn generic_clause(&self) -> Option<CheckedGenericClause> {
        self.0
            .generic_clause()
            .map(CheckedGenericClause::cast_unchecked)
    }
    pub fn port_clause(&self) -> Option<CheckedPortClause> {
        self.0.port_clause().map(CheckedPortClause::cast_unchecked)
    }
}
#[derive(Debug, Clone)]
pub struct CheckedComponentDeclarationPreamble(ComponentDeclarationPreambleSyntax);
impl CheckedNode for CheckedComponentDeclarationPreamble {
    type Syntax = ComponentDeclarationPreambleSyntax;
    fn cast_unchecked(syntax: ComponentDeclarationPreambleSyntax) -> Self {
        CheckedComponentDeclarationPreamble(syntax)
    }
    fn raw(&self) -> ComponentDeclarationPreambleSyntax {
        self.0.clone()
    }
}
impl CheckedComponentDeclarationPreamble {
    pub fn component_token(&self) -> SyntaxToken {
        self.0.component_token().unwrap()
    }
    pub fn name_token(&self) -> SyntaxToken {
        self.0.name_token().unwrap()
    }
    pub fn is_token(&self) -> Option<SyntaxToken> {
        self.0.is_token()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedComponentDeclarationEpilogue(ComponentDeclarationEpilogueSyntax);
impl CheckedNode for CheckedComponentDeclarationEpilogue {
    type Syntax = ComponentDeclarationEpilogueSyntax;
    fn cast_unchecked(syntax: ComponentDeclarationEpilogueSyntax) -> Self {
        CheckedComponentDeclarationEpilogue(syntax)
    }
    fn raw(&self) -> ComponentDeclarationEpilogueSyntax {
        self.0.clone()
    }
}
impl CheckedComponentDeclarationEpilogue {
    pub fn end_token(&self) -> SyntaxToken {
        self.0.end_token().unwrap()
    }
    pub fn component_token(&self) -> SyntaxToken {
        self.0.component_token().unwrap()
    }
    pub fn identifier_token(&self) -> Option<SyntaxToken> {
        self.0.identifier_token()
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.0.semi_colon_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedConstantDeclaration(ConstantDeclarationSyntax);
impl CheckedNode for CheckedConstantDeclaration {
    type Syntax = ConstantDeclarationSyntax;
    fn cast_unchecked(syntax: ConstantDeclarationSyntax) -> Self {
        CheckedConstantDeclaration(syntax)
    }
    fn raw(&self) -> ConstantDeclarationSyntax {
        self.0.clone()
    }
}
impl CheckedConstantDeclaration {
    pub fn constant_token(&self) -> SyntaxToken {
        self.0.constant_token().unwrap()
    }
    pub fn identifier_list(&self) -> CheckedIdentifierList {
        CheckedIdentifierList::cast_unchecked(self.0.identifier_list().unwrap())
    }
    pub fn colon_token(&self) -> SyntaxToken {
        self.0.colon_token().unwrap()
    }
    pub fn subtype_indication(&self) -> CheckedSubtypeIndication {
        CheckedSubtypeIndication::cast_unchecked(self.0.subtype_indication().unwrap())
    }
    pub fn colon_eq_token(&self) -> Option<SyntaxToken> {
        self.0.colon_eq_token()
    }
    pub fn expression(&self) -> Option<CheckedExpression> {
        self.0.expression().map(CheckedExpression::cast_unchecked)
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.0.semi_colon_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedRangeConstraintConstraint(RangeConstraintConstraintSyntax);
impl CheckedNode for CheckedRangeConstraintConstraint {
    type Syntax = RangeConstraintConstraintSyntax;
    fn cast_unchecked(syntax: RangeConstraintConstraintSyntax) -> Self {
        CheckedRangeConstraintConstraint(syntax)
    }
    fn raw(&self) -> RangeConstraintConstraintSyntax {
        self.0.clone()
    }
}
impl CheckedRangeConstraintConstraint {
    pub fn range_constraint(&self) -> CheckedRangeConstraint {
        CheckedRangeConstraint::cast_unchecked(self.0.range_constraint().unwrap())
    }
}
#[derive(Debug, Clone)]
pub enum CheckedConstraint {
    RangeConstraintConstraint(CheckedRangeConstraintConstraint),
    ArrayConstraint(CheckedArrayConstraint),
    RecordConstraint(CheckedRecordConstraint),
}
impl CheckedNode for CheckedConstraint {
    type Syntax = ConstraintSyntax;
    fn cast_unchecked(syntax: ConstraintSyntax) -> Self {
        match syntax {
            ConstraintSyntax::RangeConstraintConstraint(inner) => {
                CheckedConstraint::RangeConstraintConstraint(
                    CheckedRangeConstraintConstraint::cast_unchecked(inner),
                )
            }
            ConstraintSyntax::ArrayConstraint(inner) => {
                CheckedConstraint::ArrayConstraint(CheckedArrayConstraint::cast_unchecked(inner))
            }
            ConstraintSyntax::RecordConstraint(inner) => {
                CheckedConstraint::RecordConstraint(CheckedRecordConstraint::cast_unchecked(inner))
            }
        }
    }
    fn raw(&self) -> Self::Syntax {
        match self {
            CheckedConstraint::RangeConstraintConstraint(inner) => {
                ConstraintSyntax::RangeConstraintConstraint(inner.raw())
            }
            CheckedConstraint::ArrayConstraint(inner) => {
                ConstraintSyntax::ArrayConstraint(inner.raw())
            }
            CheckedConstraint::RecordConstraint(inner) => {
                ConstraintSyntax::RecordConstraint(inner.raw())
            }
        }
    }
}
#[derive(Debug, Clone)]
pub struct CheckedResolutionIndicationElementResolution(
    ResolutionIndicationElementResolutionSyntax,
);
impl CheckedNode for CheckedResolutionIndicationElementResolution {
    type Syntax = ResolutionIndicationElementResolutionSyntax;
    fn cast_unchecked(syntax: ResolutionIndicationElementResolutionSyntax) -> Self {
        CheckedResolutionIndicationElementResolution(syntax)
    }
    fn raw(&self) -> ResolutionIndicationElementResolutionSyntax {
        self.0.clone()
    }
}
impl CheckedResolutionIndicationElementResolution {
    pub fn resolution_indication(&self) -> CheckedResolutionIndication {
        CheckedResolutionIndication::cast_unchecked(self.0.resolution_indication().unwrap())
    }
}
#[derive(Debug, Clone)]
pub struct CheckedRecordResolutionElementResolution(RecordResolutionElementResolutionSyntax);
impl CheckedNode for CheckedRecordResolutionElementResolution {
    type Syntax = RecordResolutionElementResolutionSyntax;
    fn cast_unchecked(syntax: RecordResolutionElementResolutionSyntax) -> Self {
        CheckedRecordResolutionElementResolution(syntax)
    }
    fn raw(&self) -> RecordResolutionElementResolutionSyntax {
        self.0.clone()
    }
}
impl CheckedRecordResolutionElementResolution {
    pub fn record_resolution(&self) -> Option<CheckedRecordResolution> {
        self.0
            .record_resolution()
            .map(CheckedRecordResolution::cast_unchecked)
    }
}
#[derive(Debug, Clone)]
pub enum CheckedElementResolution {
    ResolutionIndicationElementResolution(CheckedResolutionIndicationElementResolution),
    RecordResolutionElementResolution(CheckedRecordResolutionElementResolution),
}
impl CheckedNode for CheckedElementResolution {
    type Syntax = ElementResolutionSyntax;
    fn cast_unchecked(syntax: ElementResolutionSyntax) -> Self {
        match syntax {
            ElementResolutionSyntax::ResolutionIndicationElementResolution(inner) => {
                CheckedElementResolution::ResolutionIndicationElementResolution(
                    CheckedResolutionIndicationElementResolution::cast_unchecked(inner),
                )
            }
            ElementResolutionSyntax::RecordResolutionElementResolution(inner) => {
                CheckedElementResolution::RecordResolutionElementResolution(
                    CheckedRecordResolutionElementResolution::cast_unchecked(inner),
                )
            }
        }
    }
    fn raw(&self) -> Self::Syntax {
        match self {
            CheckedElementResolution::ResolutionIndicationElementResolution(inner) => {
                ElementResolutionSyntax::ResolutionIndicationElementResolution(inner.raw())
            }
            CheckedElementResolution::RecordResolutionElementResolution(inner) => {
                ElementResolutionSyntax::RecordResolutionElementResolution(inner.raw())
            }
        }
    }
}
#[derive(Debug, Clone)]
pub struct CheckedEntityClassEntry(EntityClassEntrySyntax);
impl CheckedNode for CheckedEntityClassEntry {
    type Syntax = EntityClassEntrySyntax;
    fn cast_unchecked(syntax: EntityClassEntrySyntax) -> Self {
        CheckedEntityClassEntry(syntax)
    }
    fn raw(&self) -> EntityClassEntrySyntax {
        self.0.clone()
    }
}
impl CheckedEntityClassEntry {
    pub fn entity_class(&self) -> EntityClassSyntax {
        self.0.entity_class().unwrap()
    }
    pub fn box_token(&self) -> Option<SyntaxToken> {
        self.0.box_token()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedEntityClassEntryList(EntityClassEntryListSyntax);
impl CheckedNode for CheckedEntityClassEntryList {
    type Syntax = EntityClassEntryListSyntax;
    fn cast_unchecked(syntax: EntityClassEntryListSyntax) -> Self {
        CheckedEntityClassEntryList(syntax)
    }
    fn raw(&self) -> EntityClassEntryListSyntax {
        self.0.clone()
    }
}
impl CheckedEntityClassEntryList {
    pub fn entity_class_entrys(&self) -> impl Iterator<Item = CheckedEntityClassEntry> + use<'_> {
        self.0
            .entity_class_entrys()
            .map(CheckedEntityClassEntry::cast_unchecked)
    }
    pub fn comma_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0.comma_token()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedFileDeclaration(FileDeclarationSyntax);
impl CheckedNode for CheckedFileDeclaration {
    type Syntax = FileDeclarationSyntax;
    fn cast_unchecked(syntax: FileDeclarationSyntax) -> Self {
        CheckedFileDeclaration(syntax)
    }
    fn raw(&self) -> FileDeclarationSyntax {
        self.0.clone()
    }
}
impl CheckedFileDeclaration {
    pub fn file_token(&self) -> SyntaxToken {
        self.0.file_token().unwrap()
    }
    pub fn identifier_list(&self) -> CheckedIdentifierList {
        CheckedIdentifierList::cast_unchecked(self.0.identifier_list().unwrap())
    }
    pub fn colon_token(&self) -> SyntaxToken {
        self.0.colon_token().unwrap()
    }
    pub fn subtype_indication(&self) -> CheckedSubtypeIndication {
        CheckedSubtypeIndication::cast_unchecked(self.0.subtype_indication().unwrap())
    }
    pub fn file_open_information(&self) -> CheckedFileOpenInformation {
        CheckedFileOpenInformation::cast_unchecked(self.0.file_open_information().unwrap())
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.0.semi_colon_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedFileOpenInformation(FileOpenInformationSyntax);
impl CheckedNode for CheckedFileOpenInformation {
    type Syntax = FileOpenInformationSyntax;
    fn cast_unchecked(syntax: FileOpenInformationSyntax) -> Self {
        CheckedFileOpenInformation(syntax)
    }
    fn raw(&self) -> FileOpenInformationSyntax {
        self.0.clone()
    }
}
impl CheckedFileOpenInformation {
    pub fn open_token(&self) -> SyntaxToken {
        self.0.open_token().unwrap()
    }
    pub fn file_open_kind(&self) -> CheckedExpression {
        CheckedExpression::cast_unchecked(self.0.file_open_kind().unwrap())
    }
    pub fn is_token(&self) -> SyntaxToken {
        self.0.is_token().unwrap()
    }
    pub fn file_logical_name(&self) -> CheckedExpression {
        CheckedExpression::cast_unchecked(self.0.file_logical_name().unwrap())
    }
}
#[derive(Debug, Clone)]
pub struct CheckedParenthesizedName(ParenthesizedNameSyntax);
impl CheckedNode for CheckedParenthesizedName {
    type Syntax = ParenthesizedNameSyntax;
    fn cast_unchecked(syntax: ParenthesizedNameSyntax) -> Self {
        CheckedParenthesizedName(syntax)
    }
    fn raw(&self) -> ParenthesizedNameSyntax {
        self.0.clone()
    }
}
impl CheckedParenthesizedName {
    pub fn left_par_token(&self) -> SyntaxToken {
        self.0.left_par_token().unwrap()
    }
    pub fn name(&self) -> CheckedName {
        CheckedName::cast_unchecked(self.0.name().unwrap())
    }
    pub fn right_par_token(&self) -> SyntaxToken {
        self.0.right_par_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedFormalPart(FormalPartSyntax);
impl CheckedNode for CheckedFormalPart {
    type Syntax = FormalPartSyntax;
    fn cast_unchecked(syntax: FormalPartSyntax) -> Self {
        CheckedFormalPart(syntax)
    }
    fn raw(&self) -> FormalPartSyntax {
        self.0.clone()
    }
}
impl CheckedFormalPart {
    pub fn name(&self) -> CheckedName {
        CheckedName::cast_unchecked(self.0.name().unwrap())
    }
    pub fn parenthesized_name(&self) -> CheckedParenthesizedName {
        CheckedParenthesizedName::cast_unchecked(self.0.parenthesized_name().unwrap())
    }
}
#[derive(Debug, Clone)]
pub struct CheckedFullTypeDeclaration(FullTypeDeclarationSyntax);
impl CheckedNode for CheckedFullTypeDeclaration {
    type Syntax = FullTypeDeclarationSyntax;
    fn cast_unchecked(syntax: FullTypeDeclarationSyntax) -> Self {
        CheckedFullTypeDeclaration(syntax)
    }
    fn raw(&self) -> FullTypeDeclarationSyntax {
        self.0.clone()
    }
}
impl CheckedFullTypeDeclaration {
    pub fn type_token(&self) -> SyntaxToken {
        self.0.type_token().unwrap()
    }
    pub fn identifier_token(&self) -> SyntaxToken {
        self.0.identifier_token().unwrap()
    }
    pub fn is_token(&self) -> SyntaxToken {
        self.0.is_token().unwrap()
    }
    pub fn type_definition(&self) -> CheckedTypeDefinition {
        CheckedTypeDefinition::cast_unchecked(self.0.type_definition().unwrap())
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.0.semi_colon_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedGenericClause(GenericClauseSyntax);
impl CheckedNode for CheckedGenericClause {
    type Syntax = GenericClauseSyntax;
    fn cast_unchecked(syntax: GenericClauseSyntax) -> Self {
        CheckedGenericClause(syntax)
    }
    fn raw(&self) -> GenericClauseSyntax {
        self.0.clone()
    }
}
impl CheckedGenericClause {
    pub fn generic_clause_preamble(&self) -> CheckedGenericClausePreamble {
        CheckedGenericClausePreamble::cast_unchecked(self.0.generic_clause_preamble().unwrap())
    }
    pub fn interface_list(&self) -> Option<CheckedInterfaceList> {
        self.0
            .interface_list()
            .map(CheckedInterfaceList::cast_unchecked)
    }
    pub fn generic_clause_epilogue(&self) -> CheckedGenericClauseEpilogue {
        CheckedGenericClauseEpilogue::cast_unchecked(self.0.generic_clause_epilogue().unwrap())
    }
}
#[derive(Debug, Clone)]
pub struct CheckedGenericClausePreamble(GenericClausePreambleSyntax);
impl CheckedNode for CheckedGenericClausePreamble {
    type Syntax = GenericClausePreambleSyntax;
    fn cast_unchecked(syntax: GenericClausePreambleSyntax) -> Self {
        CheckedGenericClausePreamble(syntax)
    }
    fn raw(&self) -> GenericClausePreambleSyntax {
        self.0.clone()
    }
}
impl CheckedGenericClausePreamble {
    pub fn generic_token(&self) -> SyntaxToken {
        self.0.generic_token().unwrap()
    }
    pub fn left_par_token(&self) -> SyntaxToken {
        self.0.left_par_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedGenericClauseEpilogue(GenericClauseEpilogueSyntax);
impl CheckedNode for CheckedGenericClauseEpilogue {
    type Syntax = GenericClauseEpilogueSyntax;
    fn cast_unchecked(syntax: GenericClauseEpilogueSyntax) -> Self {
        CheckedGenericClauseEpilogue(syntax)
    }
    fn raw(&self) -> GenericClauseEpilogueSyntax {
        self.0.clone()
    }
}
impl CheckedGenericClauseEpilogue {
    pub fn right_par_token(&self) -> SyntaxToken {
        self.0.right_par_token().unwrap()
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.0.semi_colon_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedGenericMapAspect(GenericMapAspectSyntax);
impl CheckedNode for CheckedGenericMapAspect {
    type Syntax = GenericMapAspectSyntax;
    fn cast_unchecked(syntax: GenericMapAspectSyntax) -> Self {
        CheckedGenericMapAspect(syntax)
    }
    fn raw(&self) -> GenericMapAspectSyntax {
        self.0.clone()
    }
}
impl CheckedGenericMapAspect {
    pub fn generic_token(&self) -> SyntaxToken {
        self.0.generic_token().unwrap()
    }
    pub fn map_token(&self) -> SyntaxToken {
        self.0.map_token().unwrap()
    }
    pub fn left_par_token(&self) -> SyntaxToken {
        self.0.left_par_token().unwrap()
    }
    pub fn association_list(&self) -> Option<CheckedAssociationList> {
        self.0
            .association_list()
            .map(CheckedAssociationList::cast_unchecked)
    }
    pub fn right_par_token(&self) -> SyntaxToken {
        self.0.right_par_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedGroupConstituentList(GroupConstituentListSyntax);
impl CheckedNode for CheckedGroupConstituentList {
    type Syntax = GroupConstituentListSyntax;
    fn cast_unchecked(syntax: GroupConstituentListSyntax) -> Self {
        CheckedGroupConstituentList(syntax)
    }
    fn raw(&self) -> GroupConstituentListSyntax {
        self.0.clone()
    }
}
impl CheckedGroupConstituentList {
    pub fn names(&self) -> impl Iterator<Item = CheckedName> + use<'_> {
        self.0.names().map(CheckedName::cast_unchecked)
    }
    pub fn comma_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0.comma_token()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedGroupDeclaration(GroupDeclarationSyntax);
impl CheckedNode for CheckedGroupDeclaration {
    type Syntax = GroupDeclarationSyntax;
    fn cast_unchecked(syntax: GroupDeclarationSyntax) -> Self {
        CheckedGroupDeclaration(syntax)
    }
    fn raw(&self) -> GroupDeclarationSyntax {
        self.0.clone()
    }
}
impl CheckedGroupDeclaration {
    pub fn group_token(&self) -> SyntaxToken {
        self.0.group_token().unwrap()
    }
    pub fn identifier_token(&self) -> SyntaxToken {
        self.0.identifier_token().unwrap()
    }
    pub fn colon_token(&self) -> SyntaxToken {
        self.0.colon_token().unwrap()
    }
    pub fn name(&self) -> CheckedName {
        CheckedName::cast_unchecked(self.0.name().unwrap())
    }
    pub fn left_par_token(&self) -> SyntaxToken {
        self.0.left_par_token().unwrap()
    }
    pub fn group_constituent_list(&self) -> Option<CheckedGroupConstituentList> {
        self.0
            .group_constituent_list()
            .map(CheckedGroupConstituentList::cast_unchecked)
    }
    pub fn right_par_token(&self) -> SyntaxToken {
        self.0.right_par_token().unwrap()
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.0.semi_colon_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedGroupTemplateDeclaration(GroupTemplateDeclarationSyntax);
impl CheckedNode for CheckedGroupTemplateDeclaration {
    type Syntax = GroupTemplateDeclarationSyntax;
    fn cast_unchecked(syntax: GroupTemplateDeclarationSyntax) -> Self {
        CheckedGroupTemplateDeclaration(syntax)
    }
    fn raw(&self) -> GroupTemplateDeclarationSyntax {
        self.0.clone()
    }
}
impl CheckedGroupTemplateDeclaration {
    pub fn group_token(&self) -> SyntaxToken {
        self.0.group_token().unwrap()
    }
    pub fn identifier_token(&self) -> SyntaxToken {
        self.0.identifier_token().unwrap()
    }
    pub fn is_token(&self) -> SyntaxToken {
        self.0.is_token().unwrap()
    }
    pub fn left_par_token(&self) -> SyntaxToken {
        self.0.left_par_token().unwrap()
    }
    pub fn entity_class_entry_list(&self) -> Option<CheckedEntityClassEntryList> {
        self.0
            .entity_class_entry_list()
            .map(CheckedEntityClassEntryList::cast_unchecked)
    }
    pub fn right_par_token(&self) -> SyntaxToken {
        self.0.right_par_token().unwrap()
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.0.semi_colon_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedInterfaceConstantDeclaration(InterfaceConstantDeclarationSyntax);
impl CheckedNode for CheckedInterfaceConstantDeclaration {
    type Syntax = InterfaceConstantDeclarationSyntax;
    fn cast_unchecked(syntax: InterfaceConstantDeclarationSyntax) -> Self {
        CheckedInterfaceConstantDeclaration(syntax)
    }
    fn raw(&self) -> InterfaceConstantDeclarationSyntax {
        self.0.clone()
    }
}
impl CheckedInterfaceConstantDeclaration {
    pub fn constant_token(&self) -> SyntaxToken {
        self.0.constant_token().unwrap()
    }
    pub fn identifier_list(&self) -> CheckedIdentifierList {
        CheckedIdentifierList::cast_unchecked(self.0.identifier_list().unwrap())
    }
    pub fn colon_token(&self) -> SyntaxToken {
        self.0.colon_token().unwrap()
    }
    pub fn in_token(&self) -> SyntaxToken {
        self.0.in_token().unwrap()
    }
    pub fn subtype_indication(&self) -> CheckedSubtypeIndication {
        CheckedSubtypeIndication::cast_unchecked(self.0.subtype_indication().unwrap())
    }
    pub fn colon_eq_token(&self) -> SyntaxToken {
        self.0.colon_eq_token().unwrap()
    }
    pub fn expression(&self) -> CheckedExpression {
        CheckedExpression::cast_unchecked(self.0.expression().unwrap())
    }
}
#[derive(Debug, Clone)]
pub enum CheckedInterfaceDeclaration {
    InterfaceObjectDeclaration(CheckedInterfaceObjectDeclaration),
    InterfaceIncompleteTypeDeclaration(CheckedInterfaceIncompleteTypeDeclaration),
    InterfaceSubprogramDeclaration(CheckedInterfaceSubprogramDeclaration),
    InterfacePackageDeclaration(CheckedInterfacePackageDeclaration),
}
impl CheckedNode for CheckedInterfaceDeclaration {
    type Syntax = InterfaceDeclarationSyntax;
    fn cast_unchecked(syntax: InterfaceDeclarationSyntax) -> Self {
        match syntax {
            InterfaceDeclarationSyntax::InterfaceObjectDeclaration(inner) => {
                CheckedInterfaceDeclaration::InterfaceObjectDeclaration(
                    CheckedInterfaceObjectDeclaration::cast_unchecked(inner),
                )
            }
            InterfaceDeclarationSyntax::InterfaceIncompleteTypeDeclaration(inner) => {
                CheckedInterfaceDeclaration::InterfaceIncompleteTypeDeclaration(
                    CheckedInterfaceIncompleteTypeDeclaration::cast_unchecked(inner),
                )
            }
            InterfaceDeclarationSyntax::InterfaceSubprogramDeclaration(inner) => {
                CheckedInterfaceDeclaration::InterfaceSubprogramDeclaration(
                    CheckedInterfaceSubprogramDeclaration::cast_unchecked(inner),
                )
            }
            InterfaceDeclarationSyntax::InterfacePackageDeclaration(inner) => {
                CheckedInterfaceDeclaration::InterfacePackageDeclaration(
                    CheckedInterfacePackageDeclaration::cast_unchecked(inner),
                )
            }
        }
    }
    fn raw(&self) -> Self::Syntax {
        match self {
            CheckedInterfaceDeclaration::InterfaceObjectDeclaration(inner) => {
                InterfaceDeclarationSyntax::InterfaceObjectDeclaration(inner.raw())
            }
            CheckedInterfaceDeclaration::InterfaceIncompleteTypeDeclaration(inner) => {
                InterfaceDeclarationSyntax::InterfaceIncompleteTypeDeclaration(inner.raw())
            }
            CheckedInterfaceDeclaration::InterfaceSubprogramDeclaration(inner) => {
                InterfaceDeclarationSyntax::InterfaceSubprogramDeclaration(inner.raw())
            }
            CheckedInterfaceDeclaration::InterfacePackageDeclaration(inner) => {
                InterfaceDeclarationSyntax::InterfacePackageDeclaration(inner.raw())
            }
        }
    }
}
#[derive(Debug, Clone)]
pub struct CheckedInterfaceFileDeclaration(InterfaceFileDeclarationSyntax);
impl CheckedNode for CheckedInterfaceFileDeclaration {
    type Syntax = InterfaceFileDeclarationSyntax;
    fn cast_unchecked(syntax: InterfaceFileDeclarationSyntax) -> Self {
        CheckedInterfaceFileDeclaration(syntax)
    }
    fn raw(&self) -> InterfaceFileDeclarationSyntax {
        self.0.clone()
    }
}
impl CheckedInterfaceFileDeclaration {
    pub fn file_token(&self) -> SyntaxToken {
        self.0.file_token().unwrap()
    }
    pub fn identifier_list(&self) -> CheckedIdentifierList {
        CheckedIdentifierList::cast_unchecked(self.0.identifier_list().unwrap())
    }
    pub fn colon_token(&self) -> SyntaxToken {
        self.0.colon_token().unwrap()
    }
    pub fn subtype_indication(&self) -> CheckedSubtypeIndication {
        CheckedSubtypeIndication::cast_unchecked(self.0.subtype_indication().unwrap())
    }
}
#[derive(Debug, Clone)]
pub struct CheckedInterfaceFunctionSpecification(InterfaceFunctionSpecificationSyntax);
impl CheckedNode for CheckedInterfaceFunctionSpecification {
    type Syntax = InterfaceFunctionSpecificationSyntax;
    fn cast_unchecked(syntax: InterfaceFunctionSpecificationSyntax) -> Self {
        CheckedInterfaceFunctionSpecification(syntax)
    }
    fn raw(&self) -> InterfaceFunctionSpecificationSyntax {
        self.0.clone()
    }
}
impl CheckedInterfaceFunctionSpecification {
    pub fn function_purity(&self) -> Option<FunctionPuritySyntax> {
        self.0.function_purity()
    }
    pub fn function_token(&self) -> SyntaxToken {
        self.0.function_token().unwrap()
    }
    pub fn designator(&self) -> DesignatorSyntax {
        self.0.designator().unwrap()
    }
    pub fn parameter_list(&self) -> Option<CheckedParameterList> {
        self.0
            .parameter_list()
            .map(CheckedParameterList::cast_unchecked)
    }
    pub fn return_token(&self) -> SyntaxToken {
        self.0.return_token().unwrap()
    }
    pub fn name(&self) -> CheckedName {
        CheckedName::cast_unchecked(self.0.name().unwrap())
    }
}
#[derive(Debug, Clone)]
pub struct CheckedInterfaceIncompleteTypeDeclaration(InterfaceIncompleteTypeDeclarationSyntax);
impl CheckedNode for CheckedInterfaceIncompleteTypeDeclaration {
    type Syntax = InterfaceIncompleteTypeDeclarationSyntax;
    fn cast_unchecked(syntax: InterfaceIncompleteTypeDeclarationSyntax) -> Self {
        CheckedInterfaceIncompleteTypeDeclaration(syntax)
    }
    fn raw(&self) -> InterfaceIncompleteTypeDeclarationSyntax {
        self.0.clone()
    }
}
impl CheckedInterfaceIncompleteTypeDeclaration {
    pub fn type_token(&self) -> SyntaxToken {
        self.0.type_token().unwrap()
    }
    pub fn identifier_token(&self) -> SyntaxToken {
        self.0.identifier_token().unwrap()
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.0.semi_colon_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedInterfaceList(InterfaceListSyntax);
impl CheckedNode for CheckedInterfaceList {
    type Syntax = InterfaceListSyntax;
    fn cast_unchecked(syntax: InterfaceListSyntax) -> Self {
        CheckedInterfaceList(syntax)
    }
    fn raw(&self) -> InterfaceListSyntax {
        self.0.clone()
    }
}
impl CheckedInterfaceList {
    pub fn interface_declarations(
        &self,
    ) -> impl Iterator<Item = CheckedInterfaceDeclaration> + use<'_> {
        self.0
            .interface_declarations()
            .map(CheckedInterfaceDeclaration::cast_unchecked)
    }
    pub fn semi_colon_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0.semi_colon_token()
    }
}
#[derive(Debug, Clone)]
pub enum CheckedInterfaceObjectDeclaration {
    InterfaceConstantDeclaration(CheckedInterfaceConstantDeclaration),
    InterfaceSignalDeclaration(CheckedInterfaceSignalDeclaration),
    InterfaceVariableDeclaration(CheckedInterfaceVariableDeclaration),
    InterfaceFileDeclaration(CheckedInterfaceFileDeclaration),
}
impl CheckedNode for CheckedInterfaceObjectDeclaration {
    type Syntax = InterfaceObjectDeclarationSyntax;
    fn cast_unchecked(syntax: InterfaceObjectDeclarationSyntax) -> Self {
        match syntax {
            InterfaceObjectDeclarationSyntax::InterfaceConstantDeclaration(inner) => {
                CheckedInterfaceObjectDeclaration::InterfaceConstantDeclaration(
                    CheckedInterfaceConstantDeclaration::cast_unchecked(inner),
                )
            }
            InterfaceObjectDeclarationSyntax::InterfaceSignalDeclaration(inner) => {
                CheckedInterfaceObjectDeclaration::InterfaceSignalDeclaration(
                    CheckedInterfaceSignalDeclaration::cast_unchecked(inner),
                )
            }
            InterfaceObjectDeclarationSyntax::InterfaceVariableDeclaration(inner) => {
                CheckedInterfaceObjectDeclaration::InterfaceVariableDeclaration(
                    CheckedInterfaceVariableDeclaration::cast_unchecked(inner),
                )
            }
            InterfaceObjectDeclarationSyntax::InterfaceFileDeclaration(inner) => {
                CheckedInterfaceObjectDeclaration::InterfaceFileDeclaration(
                    CheckedInterfaceFileDeclaration::cast_unchecked(inner),
                )
            }
        }
    }
    fn raw(&self) -> Self::Syntax {
        match self {
            CheckedInterfaceObjectDeclaration::InterfaceConstantDeclaration(inner) => {
                InterfaceObjectDeclarationSyntax::InterfaceConstantDeclaration(inner.raw())
            }
            CheckedInterfaceObjectDeclaration::InterfaceSignalDeclaration(inner) => {
                InterfaceObjectDeclarationSyntax::InterfaceSignalDeclaration(inner.raw())
            }
            CheckedInterfaceObjectDeclaration::InterfaceVariableDeclaration(inner) => {
                InterfaceObjectDeclarationSyntax::InterfaceVariableDeclaration(inner.raw())
            }
            CheckedInterfaceObjectDeclaration::InterfaceFileDeclaration(inner) => {
                InterfaceObjectDeclarationSyntax::InterfaceFileDeclaration(inner.raw())
            }
        }
    }
}
#[derive(Debug, Clone)]
pub struct CheckedInterfacePackageDeclaration(InterfacePackageDeclarationSyntax);
impl CheckedNode for CheckedInterfacePackageDeclaration {
    type Syntax = InterfacePackageDeclarationSyntax;
    fn cast_unchecked(syntax: InterfacePackageDeclarationSyntax) -> Self {
        CheckedInterfacePackageDeclaration(syntax)
    }
    fn raw(&self) -> InterfacePackageDeclarationSyntax {
        self.0.clone()
    }
}
impl CheckedInterfacePackageDeclaration {
    pub fn interface_package_declaration_preamble(
        &self,
    ) -> CheckedInterfacePackageDeclarationPreamble {
        CheckedInterfacePackageDeclarationPreamble::cast_unchecked(
            self.0.interface_package_declaration_preamble().unwrap(),
        )
    }
    pub fn new_token(&self) -> SyntaxToken {
        self.0.new_token().unwrap()
    }
    pub fn name(&self) -> CheckedName {
        CheckedName::cast_unchecked(self.0.name().unwrap())
    }
    pub fn interface_package_generic_map_aspect(&self) -> CheckedInterfacePackageGenericMapAspect {
        CheckedInterfacePackageGenericMapAspect::cast_unchecked(
            self.0.interface_package_generic_map_aspect().unwrap(),
        )
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.0.semi_colon_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedInterfacePackageDeclarationPreamble(InterfacePackageDeclarationPreambleSyntax);
impl CheckedNode for CheckedInterfacePackageDeclarationPreamble {
    type Syntax = InterfacePackageDeclarationPreambleSyntax;
    fn cast_unchecked(syntax: InterfacePackageDeclarationPreambleSyntax) -> Self {
        CheckedInterfacePackageDeclarationPreamble(syntax)
    }
    fn raw(&self) -> InterfacePackageDeclarationPreambleSyntax {
        self.0.clone()
    }
}
impl CheckedInterfacePackageDeclarationPreamble {
    pub fn package_token(&self) -> SyntaxToken {
        self.0.package_token().unwrap()
    }
    pub fn identifier_token(&self) -> SyntaxToken {
        self.0.identifier_token().unwrap()
    }
    pub fn is_token(&self) -> SyntaxToken {
        self.0.is_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedInterfacePackageGenericMapAspect(InterfacePackageGenericMapAspectSyntax);
impl CheckedNode for CheckedInterfacePackageGenericMapAspect {
    type Syntax = InterfacePackageGenericMapAspectSyntax;
    fn cast_unchecked(syntax: InterfacePackageGenericMapAspectSyntax) -> Self {
        CheckedInterfacePackageGenericMapAspect(syntax)
    }
    fn raw(&self) -> InterfacePackageGenericMapAspectSyntax {
        self.0.clone()
    }
}
impl CheckedInterfacePackageGenericMapAspect {
    pub fn generic_token(&self) -> SyntaxToken {
        self.0.generic_token().unwrap()
    }
    pub fn map_token(&self) -> SyntaxToken {
        self.0.map_token().unwrap()
    }
    pub fn left_par_token(&self) -> SyntaxToken {
        self.0.left_par_token().unwrap()
    }
    pub fn interface_package_generic_map_aspect_inner(
        &self,
    ) -> CheckedInterfacePackageGenericMapAspectInner {
        CheckedInterfacePackageGenericMapAspectInner::cast_unchecked(
            self.0.interface_package_generic_map_aspect_inner().unwrap(),
        )
    }
    pub fn right_par_token(&self) -> SyntaxToken {
        self.0.right_par_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedInterfacePackageGenericMapAspectBox(InterfacePackageGenericMapAspectBoxSyntax);
impl CheckedNode for CheckedInterfacePackageGenericMapAspectBox {
    type Syntax = InterfacePackageGenericMapAspectBoxSyntax;
    fn cast_unchecked(syntax: InterfacePackageGenericMapAspectBoxSyntax) -> Self {
        CheckedInterfacePackageGenericMapAspectBox(syntax)
    }
    fn raw(&self) -> InterfacePackageGenericMapAspectBoxSyntax {
        self.0.clone()
    }
}
impl CheckedInterfacePackageGenericMapAspectBox {
    pub fn box_token(&self) -> SyntaxToken {
        self.0.box_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedInterfacePackageGenericMapAspectDefault(
    InterfacePackageGenericMapAspectDefaultSyntax,
);
impl CheckedNode for CheckedInterfacePackageGenericMapAspectDefault {
    type Syntax = InterfacePackageGenericMapAspectDefaultSyntax;
    fn cast_unchecked(syntax: InterfacePackageGenericMapAspectDefaultSyntax) -> Self {
        CheckedInterfacePackageGenericMapAspectDefault(syntax)
    }
    fn raw(&self) -> InterfacePackageGenericMapAspectDefaultSyntax {
        self.0.clone()
    }
}
impl CheckedInterfacePackageGenericMapAspectDefault {
    pub fn default_token(&self) -> SyntaxToken {
        self.0.default_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedInterfacePackageGenericMapAspectAssociations(
    InterfacePackageGenericMapAspectAssociationsSyntax,
);
impl CheckedNode for CheckedInterfacePackageGenericMapAspectAssociations {
    type Syntax = InterfacePackageGenericMapAspectAssociationsSyntax;
    fn cast_unchecked(syntax: InterfacePackageGenericMapAspectAssociationsSyntax) -> Self {
        CheckedInterfacePackageGenericMapAspectAssociations(syntax)
    }
    fn raw(&self) -> InterfacePackageGenericMapAspectAssociationsSyntax {
        self.0.clone()
    }
}
impl CheckedInterfacePackageGenericMapAspectAssociations {
    pub fn association_list(&self) -> Option<CheckedAssociationList> {
        self.0
            .association_list()
            .map(CheckedAssociationList::cast_unchecked)
    }
}
#[derive(Debug, Clone)]
pub enum CheckedInterfacePackageGenericMapAspectInner {
    InterfacePackageGenericMapAspectBox(CheckedInterfacePackageGenericMapAspectBox),
    InterfacePackageGenericMapAspectDefault(CheckedInterfacePackageGenericMapAspectDefault),
    InterfacePackageGenericMapAspectAssociations(
        CheckedInterfacePackageGenericMapAspectAssociations,
    ),
}
impl CheckedNode for CheckedInterfacePackageGenericMapAspectInner {
    type Syntax = InterfacePackageGenericMapAspectInnerSyntax;
    fn cast_unchecked(syntax: InterfacePackageGenericMapAspectInnerSyntax) -> Self {
        match syntax { InterfacePackageGenericMapAspectInnerSyntax :: InterfacePackageGenericMapAspectBox (inner) => CheckedInterfacePackageGenericMapAspectInner :: InterfacePackageGenericMapAspectBox (CheckedInterfacePackageGenericMapAspectBox :: cast_unchecked (inner)) , InterfacePackageGenericMapAspectInnerSyntax :: InterfacePackageGenericMapAspectDefault (inner) => CheckedInterfacePackageGenericMapAspectInner :: InterfacePackageGenericMapAspectDefault (CheckedInterfacePackageGenericMapAspectDefault :: cast_unchecked (inner)) , InterfacePackageGenericMapAspectInnerSyntax :: InterfacePackageGenericMapAspectAssociations (inner) => CheckedInterfacePackageGenericMapAspectInner :: InterfacePackageGenericMapAspectAssociations (CheckedInterfacePackageGenericMapAspectAssociations :: cast_unchecked (inner)) , }
    }
    fn raw(&self) -> Self::Syntax {
        match self { CheckedInterfacePackageGenericMapAspectInner :: InterfacePackageGenericMapAspectBox (inner) => InterfacePackageGenericMapAspectInnerSyntax :: InterfacePackageGenericMapAspectBox (inner . raw ()) , CheckedInterfacePackageGenericMapAspectInner :: InterfacePackageGenericMapAspectDefault (inner) => InterfacePackageGenericMapAspectInnerSyntax :: InterfacePackageGenericMapAspectDefault (inner . raw ()) , CheckedInterfacePackageGenericMapAspectInner :: InterfacePackageGenericMapAspectAssociations (inner) => InterfacePackageGenericMapAspectInnerSyntax :: InterfacePackageGenericMapAspectAssociations (inner . raw ()) , }
    }
}
#[derive(Debug, Clone)]
pub struct CheckedInterfaceProcedureSpecification(InterfaceProcedureSpecificationSyntax);
impl CheckedNode for CheckedInterfaceProcedureSpecification {
    type Syntax = InterfaceProcedureSpecificationSyntax;
    fn cast_unchecked(syntax: InterfaceProcedureSpecificationSyntax) -> Self {
        CheckedInterfaceProcedureSpecification(syntax)
    }
    fn raw(&self) -> InterfaceProcedureSpecificationSyntax {
        self.0.clone()
    }
}
impl CheckedInterfaceProcedureSpecification {
    pub fn procedure_token(&self) -> SyntaxToken {
        self.0.procedure_token().unwrap()
    }
    pub fn designator(&self) -> DesignatorSyntax {
        self.0.designator().unwrap()
    }
    pub fn parameter_token(&self) -> SyntaxToken {
        self.0.parameter_token().unwrap()
    }
    pub fn left_par_token(&self) -> SyntaxToken {
        self.0.left_par_token().unwrap()
    }
    pub fn interface_list(&self) -> Option<CheckedInterfaceList> {
        self.0
            .interface_list()
            .map(CheckedInterfaceList::cast_unchecked)
    }
    pub fn right_par_token(&self) -> SyntaxToken {
        self.0.right_par_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedInterfaceSignalDeclaration(InterfaceSignalDeclarationSyntax);
impl CheckedNode for CheckedInterfaceSignalDeclaration {
    type Syntax = InterfaceSignalDeclarationSyntax;
    fn cast_unchecked(syntax: InterfaceSignalDeclarationSyntax) -> Self {
        CheckedInterfaceSignalDeclaration(syntax)
    }
    fn raw(&self) -> InterfaceSignalDeclarationSyntax {
        self.0.clone()
    }
}
impl CheckedInterfaceSignalDeclaration {
    pub fn signal_token(&self) -> Option<SyntaxToken> {
        self.0.signal_token()
    }
    pub fn identifier_list(&self) -> CheckedIdentifierList {
        CheckedIdentifierList::cast_unchecked(self.0.identifier_list().unwrap())
    }
    pub fn colon_token(&self) -> SyntaxToken {
        self.0.colon_token().unwrap()
    }
    pub fn mode(&self) -> Option<ModeSyntax> {
        self.0.mode()
    }
    pub fn subtype_indication(&self) -> CheckedSubtypeIndication {
        CheckedSubtypeIndication::cast_unchecked(self.0.subtype_indication().unwrap())
    }
    pub fn bus_token(&self) -> Option<SyntaxToken> {
        self.0.bus_token()
    }
    pub fn colon_eq_token(&self) -> Option<SyntaxToken> {
        self.0.colon_eq_token()
    }
    pub fn expression(&self) -> Option<CheckedExpression> {
        self.0.expression().map(CheckedExpression::cast_unchecked)
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.0.semi_colon_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedInterfaceSubprogramDeclaration(InterfaceSubprogramDeclarationSyntax);
impl CheckedNode for CheckedInterfaceSubprogramDeclaration {
    type Syntax = InterfaceSubprogramDeclarationSyntax;
    fn cast_unchecked(syntax: InterfaceSubprogramDeclarationSyntax) -> Self {
        CheckedInterfaceSubprogramDeclaration(syntax)
    }
    fn raw(&self) -> InterfaceSubprogramDeclarationSyntax {
        self.0.clone()
    }
}
impl CheckedInterfaceSubprogramDeclaration {
    pub fn interface_subprogram_specification(&self) -> CheckedInterfaceSubprogramSpecification {
        CheckedInterfaceSubprogramSpecification::cast_unchecked(
            self.0.interface_subprogram_specification().unwrap(),
        )
    }
    pub fn is_token(&self) -> Option<SyntaxToken> {
        self.0.is_token()
    }
    pub fn interface_subprogram_default(&self) -> Option<CheckedInterfaceSubprogramDefault> {
        self.0
            .interface_subprogram_default()
            .map(CheckedInterfaceSubprogramDefault::cast_unchecked)
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.0.semi_colon_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedInterfaceSubprogramDefaultName(InterfaceSubprogramDefaultNameSyntax);
impl CheckedNode for CheckedInterfaceSubprogramDefaultName {
    type Syntax = InterfaceSubprogramDefaultNameSyntax;
    fn cast_unchecked(syntax: InterfaceSubprogramDefaultNameSyntax) -> Self {
        CheckedInterfaceSubprogramDefaultName(syntax)
    }
    fn raw(&self) -> InterfaceSubprogramDefaultNameSyntax {
        self.0.clone()
    }
}
impl CheckedInterfaceSubprogramDefaultName {
    pub fn name(&self) -> CheckedName {
        CheckedName::cast_unchecked(self.0.name().unwrap())
    }
}
#[derive(Debug, Clone)]
pub struct CheckedInterfaceSubprogramDefaultBox(InterfaceSubprogramDefaultBoxSyntax);
impl CheckedNode for CheckedInterfaceSubprogramDefaultBox {
    type Syntax = InterfaceSubprogramDefaultBoxSyntax;
    fn cast_unchecked(syntax: InterfaceSubprogramDefaultBoxSyntax) -> Self {
        CheckedInterfaceSubprogramDefaultBox(syntax)
    }
    fn raw(&self) -> InterfaceSubprogramDefaultBoxSyntax {
        self.0.clone()
    }
}
impl CheckedInterfaceSubprogramDefaultBox {
    pub fn box_token(&self) -> SyntaxToken {
        self.0.box_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub enum CheckedInterfaceSubprogramDefault {
    InterfaceSubprogramDefaultName(CheckedInterfaceSubprogramDefaultName),
    InterfaceSubprogramDefaultBox(CheckedInterfaceSubprogramDefaultBox),
}
impl CheckedNode for CheckedInterfaceSubprogramDefault {
    type Syntax = InterfaceSubprogramDefaultSyntax;
    fn cast_unchecked(syntax: InterfaceSubprogramDefaultSyntax) -> Self {
        match syntax {
            InterfaceSubprogramDefaultSyntax::InterfaceSubprogramDefaultName(inner) => {
                CheckedInterfaceSubprogramDefault::InterfaceSubprogramDefaultName(
                    CheckedInterfaceSubprogramDefaultName::cast_unchecked(inner),
                )
            }
            InterfaceSubprogramDefaultSyntax::InterfaceSubprogramDefaultBox(inner) => {
                CheckedInterfaceSubprogramDefault::InterfaceSubprogramDefaultBox(
                    CheckedInterfaceSubprogramDefaultBox::cast_unchecked(inner),
                )
            }
        }
    }
    fn raw(&self) -> Self::Syntax {
        match self {
            CheckedInterfaceSubprogramDefault::InterfaceSubprogramDefaultName(inner) => {
                InterfaceSubprogramDefaultSyntax::InterfaceSubprogramDefaultName(inner.raw())
            }
            CheckedInterfaceSubprogramDefault::InterfaceSubprogramDefaultBox(inner) => {
                InterfaceSubprogramDefaultSyntax::InterfaceSubprogramDefaultBox(inner.raw())
            }
        }
    }
}
#[derive(Debug, Clone)]
pub enum CheckedInterfaceSubprogramSpecification {
    InterfaceProcedureSpecification(CheckedInterfaceProcedureSpecification),
    InterfaceFunctionSpecification(CheckedInterfaceFunctionSpecification),
}
impl CheckedNode for CheckedInterfaceSubprogramSpecification {
    type Syntax = InterfaceSubprogramSpecificationSyntax;
    fn cast_unchecked(syntax: InterfaceSubprogramSpecificationSyntax) -> Self {
        match syntax {
            InterfaceSubprogramSpecificationSyntax::InterfaceProcedureSpecification(inner) => {
                CheckedInterfaceSubprogramSpecification::InterfaceProcedureSpecification(
                    CheckedInterfaceProcedureSpecification::cast_unchecked(inner),
                )
            }
            InterfaceSubprogramSpecificationSyntax::InterfaceFunctionSpecification(inner) => {
                CheckedInterfaceSubprogramSpecification::InterfaceFunctionSpecification(
                    CheckedInterfaceFunctionSpecification::cast_unchecked(inner),
                )
            }
        }
    }
    fn raw(&self) -> Self::Syntax {
        match self {
            CheckedInterfaceSubprogramSpecification::InterfaceProcedureSpecification(inner) => {
                InterfaceSubprogramSpecificationSyntax::InterfaceProcedureSpecification(inner.raw())
            }
            CheckedInterfaceSubprogramSpecification::InterfaceFunctionSpecification(inner) => {
                InterfaceSubprogramSpecificationSyntax::InterfaceFunctionSpecification(inner.raw())
            }
        }
    }
}
#[derive(Debug, Clone)]
pub struct CheckedInterfaceVariableDeclaration(InterfaceVariableDeclarationSyntax);
impl CheckedNode for CheckedInterfaceVariableDeclaration {
    type Syntax = InterfaceVariableDeclarationSyntax;
    fn cast_unchecked(syntax: InterfaceVariableDeclarationSyntax) -> Self {
        CheckedInterfaceVariableDeclaration(syntax)
    }
    fn raw(&self) -> InterfaceVariableDeclarationSyntax {
        self.0.clone()
    }
}
impl CheckedInterfaceVariableDeclaration {
    pub fn variable_token(&self) -> Option<SyntaxToken> {
        self.0.variable_token()
    }
    pub fn identifier_list(&self) -> CheckedIdentifierList {
        CheckedIdentifierList::cast_unchecked(self.0.identifier_list().unwrap())
    }
    pub fn colon_token(&self) -> SyntaxToken {
        self.0.colon_token().unwrap()
    }
    pub fn mode(&self) -> Option<ModeSyntax> {
        self.0.mode()
    }
    pub fn subtype_indication(&self) -> CheckedSubtypeIndication {
        CheckedSubtypeIndication::cast_unchecked(self.0.subtype_indication().unwrap())
    }
    pub fn colon_eq_token(&self) -> Option<SyntaxToken> {
        self.0.colon_eq_token()
    }
    pub fn expression(&self) -> Option<CheckedExpression> {
        self.0.expression().map(CheckedExpression::cast_unchecked)
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.0.semi_colon_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedPortClause(PortClauseSyntax);
impl CheckedNode for CheckedPortClause {
    type Syntax = PortClauseSyntax;
    fn cast_unchecked(syntax: PortClauseSyntax) -> Self {
        CheckedPortClause(syntax)
    }
    fn raw(&self) -> PortClauseSyntax {
        self.0.clone()
    }
}
impl CheckedPortClause {
    pub fn port_clause_preamble(&self) -> CheckedPortClausePreamble {
        CheckedPortClausePreamble::cast_unchecked(self.0.port_clause_preamble().unwrap())
    }
    pub fn interface_list(&self) -> Option<CheckedInterfaceList> {
        self.0
            .interface_list()
            .map(CheckedInterfaceList::cast_unchecked)
    }
    pub fn port_clause_epilogue(&self) -> CheckedPortClauseEpilogue {
        CheckedPortClauseEpilogue::cast_unchecked(self.0.port_clause_epilogue().unwrap())
    }
}
#[derive(Debug, Clone)]
pub struct CheckedPortClausePreamble(PortClausePreambleSyntax);
impl CheckedNode for CheckedPortClausePreamble {
    type Syntax = PortClausePreambleSyntax;
    fn cast_unchecked(syntax: PortClausePreambleSyntax) -> Self {
        CheckedPortClausePreamble(syntax)
    }
    fn raw(&self) -> PortClausePreambleSyntax {
        self.0.clone()
    }
}
impl CheckedPortClausePreamble {
    pub fn port_token(&self) -> SyntaxToken {
        self.0.port_token().unwrap()
    }
    pub fn left_par_token(&self) -> SyntaxToken {
        self.0.left_par_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedPortClauseEpilogue(PortClauseEpilogueSyntax);
impl CheckedNode for CheckedPortClauseEpilogue {
    type Syntax = PortClauseEpilogueSyntax;
    fn cast_unchecked(syntax: PortClauseEpilogueSyntax) -> Self {
        CheckedPortClauseEpilogue(syntax)
    }
    fn raw(&self) -> PortClauseEpilogueSyntax {
        self.0.clone()
    }
}
impl CheckedPortClauseEpilogue {
    pub fn right_par_token(&self) -> SyntaxToken {
        self.0.right_par_token().unwrap()
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.0.semi_colon_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedPortMapAspect(PortMapAspectSyntax);
impl CheckedNode for CheckedPortMapAspect {
    type Syntax = PortMapAspectSyntax;
    fn cast_unchecked(syntax: PortMapAspectSyntax) -> Self {
        CheckedPortMapAspect(syntax)
    }
    fn raw(&self) -> PortMapAspectSyntax {
        self.0.clone()
    }
}
impl CheckedPortMapAspect {
    pub fn port_token(&self) -> SyntaxToken {
        self.0.port_token().unwrap()
    }
    pub fn map_token(&self) -> SyntaxToken {
        self.0.map_token().unwrap()
    }
    pub fn left_par_token(&self) -> SyntaxToken {
        self.0.left_par_token().unwrap()
    }
    pub fn association_list(&self) -> Option<CheckedAssociationList> {
        self.0
            .association_list()
            .map(CheckedAssociationList::cast_unchecked)
    }
    pub fn right_par_token(&self) -> SyntaxToken {
        self.0.right_par_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedRecordElementResolution(RecordElementResolutionSyntax);
impl CheckedNode for CheckedRecordElementResolution {
    type Syntax = RecordElementResolutionSyntax;
    fn cast_unchecked(syntax: RecordElementResolutionSyntax) -> Self {
        CheckedRecordElementResolution(syntax)
    }
    fn raw(&self) -> RecordElementResolutionSyntax {
        self.0.clone()
    }
}
impl CheckedRecordElementResolution {
    pub fn name_token(&self) -> SyntaxToken {
        self.0.name_token().unwrap()
    }
    pub fn resolution_indication(&self) -> CheckedResolutionIndication {
        CheckedResolutionIndication::cast_unchecked(self.0.resolution_indication().unwrap())
    }
}
#[derive(Debug, Clone)]
pub struct CheckedRecordResolution(RecordResolutionSyntax);
impl CheckedNode for CheckedRecordResolution {
    type Syntax = RecordResolutionSyntax;
    fn cast_unchecked(syntax: RecordResolutionSyntax) -> Self {
        CheckedRecordResolution(syntax)
    }
    fn raw(&self) -> RecordResolutionSyntax {
        self.0.clone()
    }
}
impl CheckedRecordResolution {
    pub fn record_element_resolutions(
        &self,
    ) -> impl Iterator<Item = CheckedRecordElementResolution> + use<'_> {
        self.0
            .record_element_resolutions()
            .map(CheckedRecordElementResolution::cast_unchecked)
    }
    pub fn comma_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0.comma_token()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedNameResolutionIndication(NameResolutionIndicationSyntax);
impl CheckedNode for CheckedNameResolutionIndication {
    type Syntax = NameResolutionIndicationSyntax;
    fn cast_unchecked(syntax: NameResolutionIndicationSyntax) -> Self {
        CheckedNameResolutionIndication(syntax)
    }
    fn raw(&self) -> NameResolutionIndicationSyntax {
        self.0.clone()
    }
}
impl CheckedNameResolutionIndication {
    pub fn name(&self) -> CheckedName {
        CheckedName::cast_unchecked(self.0.name().unwrap())
    }
}
#[derive(Debug, Clone)]
pub struct CheckedElementResolutionResolutionIndication(
    ElementResolutionResolutionIndicationSyntax,
);
impl CheckedNode for CheckedElementResolutionResolutionIndication {
    type Syntax = ElementResolutionResolutionIndicationSyntax;
    fn cast_unchecked(syntax: ElementResolutionResolutionIndicationSyntax) -> Self {
        CheckedElementResolutionResolutionIndication(syntax)
    }
    fn raw(&self) -> ElementResolutionResolutionIndicationSyntax {
        self.0.clone()
    }
}
impl CheckedElementResolutionResolutionIndication {
    pub fn element_resolution(&self) -> CheckedElementResolution {
        CheckedElementResolution::cast_unchecked(self.0.element_resolution().unwrap())
    }
}
#[derive(Debug, Clone)]
pub struct CheckedParenthesizedElementResolutionResolutionIndication(
    ParenthesizedElementResolutionResolutionIndicationSyntax,
);
impl CheckedNode for CheckedParenthesizedElementResolutionResolutionIndication {
    type Syntax = ParenthesizedElementResolutionResolutionIndicationSyntax;
    fn cast_unchecked(syntax: ParenthesizedElementResolutionResolutionIndicationSyntax) -> Self {
        CheckedParenthesizedElementResolutionResolutionIndication(syntax)
    }
    fn raw(&self) -> ParenthesizedElementResolutionResolutionIndicationSyntax {
        self.0.clone()
    }
}
impl CheckedParenthesizedElementResolutionResolutionIndication {
    pub fn left_par_token(&self) -> SyntaxToken {
        self.0.left_par_token().unwrap()
    }
    pub fn element_resolution_resolution_indication(
        &self,
    ) -> CheckedElementResolutionResolutionIndication {
        CheckedElementResolutionResolutionIndication::cast_unchecked(
            self.0.element_resolution_resolution_indication().unwrap(),
        )
    }
    pub fn right_par_token(&self) -> SyntaxToken {
        self.0.right_par_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub enum CheckedResolutionIndication {
    NameResolutionIndication(CheckedNameResolutionIndication),
    ParenthesizedElementResolutionResolutionIndication(
        CheckedParenthesizedElementResolutionResolutionIndication,
    ),
}
impl CheckedNode for CheckedResolutionIndication {
    type Syntax = ResolutionIndicationSyntax;
    fn cast_unchecked(syntax: ResolutionIndicationSyntax) -> Self {
        match syntax {
            ResolutionIndicationSyntax::NameResolutionIndication(inner) => {
                CheckedResolutionIndication::NameResolutionIndication(
                    CheckedNameResolutionIndication::cast_unchecked(inner),
                )
            }
            ResolutionIndicationSyntax::ParenthesizedElementResolutionResolutionIndication(
                inner,
            ) => CheckedResolutionIndication::ParenthesizedElementResolutionResolutionIndication(
                CheckedParenthesizedElementResolutionResolutionIndication::cast_unchecked(inner),
            ),
        }
    }
    fn raw(&self) -> Self::Syntax {
        match self {
            CheckedResolutionIndication::NameResolutionIndication(inner) => {
                ResolutionIndicationSyntax::NameResolutionIndication(inner.raw())
            }
            CheckedResolutionIndication::ParenthesizedElementResolutionResolutionIndication(
                inner,
            ) => ResolutionIndicationSyntax::ParenthesizedElementResolutionResolutionIndication(
                inner.raw(),
            ),
        }
    }
}
#[derive(Debug, Clone)]
pub struct CheckedSignalDeclaration(SignalDeclarationSyntax);
impl CheckedNode for CheckedSignalDeclaration {
    type Syntax = SignalDeclarationSyntax;
    fn cast_unchecked(syntax: SignalDeclarationSyntax) -> Self {
        CheckedSignalDeclaration(syntax)
    }
    fn raw(&self) -> SignalDeclarationSyntax {
        self.0.clone()
    }
}
impl CheckedSignalDeclaration {
    pub fn signal_token(&self) -> SyntaxToken {
        self.0.signal_token().unwrap()
    }
    pub fn identifier_list(&self) -> CheckedIdentifierList {
        CheckedIdentifierList::cast_unchecked(self.0.identifier_list().unwrap())
    }
    pub fn colon_token(&self) -> SyntaxToken {
        self.0.colon_token().unwrap()
    }
    pub fn subtype_indication(&self) -> CheckedSubtypeIndication {
        CheckedSubtypeIndication::cast_unchecked(self.0.subtype_indication().unwrap())
    }
    pub fn signal_kind(&self) -> Option<SignalKindSyntax> {
        self.0.signal_kind()
    }
    pub fn colon_eq_token(&self) -> Option<SyntaxToken> {
        self.0.colon_eq_token()
    }
    pub fn expression(&self) -> Option<CheckedExpression> {
        self.0.expression().map(CheckedExpression::cast_unchecked)
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.0.semi_colon_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedSubtypeDeclaration(SubtypeDeclarationSyntax);
impl CheckedNode for CheckedSubtypeDeclaration {
    type Syntax = SubtypeDeclarationSyntax;
    fn cast_unchecked(syntax: SubtypeDeclarationSyntax) -> Self {
        CheckedSubtypeDeclaration(syntax)
    }
    fn raw(&self) -> SubtypeDeclarationSyntax {
        self.0.clone()
    }
}
impl CheckedSubtypeDeclaration {
    pub fn subtype_token(&self) -> SyntaxToken {
        self.0.subtype_token().unwrap()
    }
    pub fn identifier_token(&self) -> SyntaxToken {
        self.0.identifier_token().unwrap()
    }
    pub fn is_token(&self) -> SyntaxToken {
        self.0.is_token().unwrap()
    }
    pub fn subtype_indication(&self) -> CheckedSubtypeIndication {
        CheckedSubtypeIndication::cast_unchecked(self.0.subtype_indication().unwrap())
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.0.semi_colon_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedSubtypeIndication(SubtypeIndicationSyntax);
impl CheckedNode for CheckedSubtypeIndication {
    type Syntax = SubtypeIndicationSyntax;
    fn cast_unchecked(syntax: SubtypeIndicationSyntax) -> Self {
        CheckedSubtypeIndication(syntax)
    }
    fn raw(&self) -> SubtypeIndicationSyntax {
        self.0.clone()
    }
}
impl CheckedSubtypeIndication {
    pub fn resolution_indication(&self) -> CheckedResolutionIndication {
        CheckedResolutionIndication::cast_unchecked(self.0.resolution_indication().unwrap())
    }
    pub fn name(&self) -> CheckedName {
        CheckedName::cast_unchecked(self.0.name().unwrap())
    }
}
#[derive(Debug, Clone)]
pub enum CheckedTypeDeclaration {
    FullTypeDeclaration(CheckedFullTypeDeclaration),
    IncompleteTypeDeclaration(CheckedIncompleteTypeDeclaration),
}
impl CheckedNode for CheckedTypeDeclaration {
    type Syntax = TypeDeclarationSyntax;
    fn cast_unchecked(syntax: TypeDeclarationSyntax) -> Self {
        match syntax {
            TypeDeclarationSyntax::FullTypeDeclaration(inner) => {
                CheckedTypeDeclaration::FullTypeDeclaration(
                    CheckedFullTypeDeclaration::cast_unchecked(inner),
                )
            }
            TypeDeclarationSyntax::IncompleteTypeDeclaration(inner) => {
                CheckedTypeDeclaration::IncompleteTypeDeclaration(
                    CheckedIncompleteTypeDeclaration::cast_unchecked(inner),
                )
            }
        }
    }
    fn raw(&self) -> Self::Syntax {
        match self {
            CheckedTypeDeclaration::FullTypeDeclaration(inner) => {
                TypeDeclarationSyntax::FullTypeDeclaration(inner.raw())
            }
            CheckedTypeDeclaration::IncompleteTypeDeclaration(inner) => {
                TypeDeclarationSyntax::IncompleteTypeDeclaration(inner.raw())
            }
        }
    }
}
#[derive(Debug, Clone)]
pub enum CheckedTypeDefinition {
    ScalarTypeDefinition(CheckedScalarTypeDefinition),
    CompositeTypeDefinition(CheckedCompositeTypeDefinition),
    AccessTypeDefinition(CheckedAccessTypeDefinition),
    FileTypeDefinition(CheckedFileTypeDefinition),
    ProtectedTypeDefinition(CheckedProtectedTypeDefinition),
}
impl CheckedNode for CheckedTypeDefinition {
    type Syntax = TypeDefinitionSyntax;
    fn cast_unchecked(syntax: TypeDefinitionSyntax) -> Self {
        match syntax {
            TypeDefinitionSyntax::ScalarTypeDefinition(inner) => {
                CheckedTypeDefinition::ScalarTypeDefinition(
                    CheckedScalarTypeDefinition::cast_unchecked(inner),
                )
            }
            TypeDefinitionSyntax::CompositeTypeDefinition(inner) => {
                CheckedTypeDefinition::CompositeTypeDefinition(
                    CheckedCompositeTypeDefinition::cast_unchecked(inner),
                )
            }
            TypeDefinitionSyntax::AccessTypeDefinition(inner) => {
                CheckedTypeDefinition::AccessTypeDefinition(
                    CheckedAccessTypeDefinition::cast_unchecked(inner),
                )
            }
            TypeDefinitionSyntax::FileTypeDefinition(inner) => {
                CheckedTypeDefinition::FileTypeDefinition(
                    CheckedFileTypeDefinition::cast_unchecked(inner),
                )
            }
            TypeDefinitionSyntax::ProtectedTypeDefinition(inner) => {
                CheckedTypeDefinition::ProtectedTypeDefinition(
                    CheckedProtectedTypeDefinition::cast_unchecked(inner),
                )
            }
        }
    }
    fn raw(&self) -> Self::Syntax {
        match self {
            CheckedTypeDefinition::ScalarTypeDefinition(inner) => {
                TypeDefinitionSyntax::ScalarTypeDefinition(inner.raw())
            }
            CheckedTypeDefinition::CompositeTypeDefinition(inner) => {
                TypeDefinitionSyntax::CompositeTypeDefinition(inner.raw())
            }
            CheckedTypeDefinition::AccessTypeDefinition(inner) => {
                TypeDefinitionSyntax::AccessTypeDefinition(inner.raw())
            }
            CheckedTypeDefinition::FileTypeDefinition(inner) => {
                TypeDefinitionSyntax::FileTypeDefinition(inner.raw())
            }
            CheckedTypeDefinition::ProtectedTypeDefinition(inner) => {
                TypeDefinitionSyntax::ProtectedTypeDefinition(inner.raw())
            }
        }
    }
}
#[derive(Debug, Clone)]
pub struct CheckedVariableDeclaration(VariableDeclarationSyntax);
impl CheckedNode for CheckedVariableDeclaration {
    type Syntax = VariableDeclarationSyntax;
    fn cast_unchecked(syntax: VariableDeclarationSyntax) -> Self {
        CheckedVariableDeclaration(syntax)
    }
    fn raw(&self) -> VariableDeclarationSyntax {
        self.0.clone()
    }
}
impl CheckedVariableDeclaration {
    pub fn variable_token(&self) -> SyntaxToken {
        self.0.variable_token().unwrap()
    }
    pub fn identifier_list(&self) -> CheckedIdentifierList {
        CheckedIdentifierList::cast_unchecked(self.0.identifier_list().unwrap())
    }
    pub fn colon_token(&self) -> SyntaxToken {
        self.0.colon_token().unwrap()
    }
    pub fn subtype_indication(&self) -> CheckedSubtypeIndication {
        CheckedSubtypeIndication::cast_unchecked(self.0.subtype_indication().unwrap())
    }
    pub fn colon_eq_token(&self) -> Option<SyntaxToken> {
        self.0.colon_eq_token()
    }
    pub fn expression(&self) -> Option<CheckedExpression> {
        self.0.expression().map(CheckedExpression::cast_unchecked)
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.0.semi_colon_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedSharedVariableDeclaration(SharedVariableDeclarationSyntax);
impl CheckedNode for CheckedSharedVariableDeclaration {
    type Syntax = SharedVariableDeclarationSyntax;
    fn cast_unchecked(syntax: SharedVariableDeclarationSyntax) -> Self {
        CheckedSharedVariableDeclaration(syntax)
    }
    fn raw(&self) -> SharedVariableDeclarationSyntax {
        self.0.clone()
    }
}
impl CheckedSharedVariableDeclaration {
    pub fn shared_token(&self) -> SyntaxToken {
        self.0.shared_token().unwrap()
    }
    pub fn variable_token(&self) -> SyntaxToken {
        self.0.variable_token().unwrap()
    }
    pub fn identifier_list(&self) -> CheckedIdentifierList {
        CheckedIdentifierList::cast_unchecked(self.0.identifier_list().unwrap())
    }
    pub fn colon_token(&self) -> SyntaxToken {
        self.0.colon_token().unwrap()
    }
    pub fn subtype_indication(&self) -> CheckedSubtypeIndication {
        CheckedSubtypeIndication::cast_unchecked(self.0.subtype_indication().unwrap())
    }
    pub fn colon_eq_token(&self) -> Option<SyntaxToken> {
        self.0.colon_eq_token()
    }
    pub fn expression(&self) -> Option<CheckedExpression> {
        self.0.expression().map(CheckedExpression::cast_unchecked)
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.0.semi_colon_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedDeclarations(DeclarationsSyntax);
impl CheckedNode for CheckedDeclarations {
    type Syntax = DeclarationsSyntax;
    fn cast_unchecked(syntax: DeclarationsSyntax) -> Self {
        CheckedDeclarations(syntax)
    }
    fn raw(&self) -> DeclarationsSyntax {
        self.0.clone()
    }
}
impl CheckedDeclarations {
    pub fn declarations(&self) -> impl Iterator<Item = CheckedDeclaration> + use<'_> {
        self.0
            .declarations()
            .map(CheckedDeclaration::cast_unchecked)
    }
}
