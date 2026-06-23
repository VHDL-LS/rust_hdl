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
pub struct CheckedAccessTypeDefinition(AccessTypeDefinitionSyntax);
impl CheckedNode for CheckedAccessTypeDefinition {
    type Syntax = AccessTypeDefinitionSyntax;
    fn cast_unchecked(syntax: AccessTypeDefinitionSyntax) -> Self {
        CheckedAccessTypeDefinition(syntax)
    }
    fn raw(&self) -> AccessTypeDefinitionSyntax {
        self.0.clone()
    }
}
impl CheckedAccessTypeDefinition {
    pub fn access_token(&self) -> SyntaxToken {
        self.0.access_token().unwrap()
    }
    pub fn subtype_indication(&self) -> CheckedSubtypeIndication {
        CheckedSubtypeIndication::cast_unchecked(self.0.subtype_indication().unwrap())
    }
}
#[derive(Debug, Clone)]
pub struct CheckedArrayConstraint(ArrayConstraintSyntax);
impl CheckedNode for CheckedArrayConstraint {
    type Syntax = ArrayConstraintSyntax;
    fn cast_unchecked(syntax: ArrayConstraintSyntax) -> Self {
        CheckedArrayConstraint(syntax)
    }
    fn raw(&self) -> ArrayConstraintSyntax {
        self.0.clone()
    }
}
impl CheckedArrayConstraint {
    pub fn index_constraint(&self) -> CheckedIndexConstraint {
        CheckedIndexConstraint::cast_unchecked(self.0.index_constraint().unwrap())
    }
    pub fn constraint(&self) -> CheckedConstraint {
        CheckedConstraint::cast_unchecked(self.0.constraint().unwrap())
    }
}
#[derive(Debug, Clone)]
pub enum CheckedArrayTypeDefinition {
    UnboundedArrayDefinition(CheckedUnboundedArrayDefinition),
    ConstrainedArrayDefinition(CheckedConstrainedArrayDefinition),
}
impl CheckedNode for CheckedArrayTypeDefinition {
    type Syntax = ArrayTypeDefinitionSyntax;
    fn cast_unchecked(syntax: ArrayTypeDefinitionSyntax) -> Self {
        match syntax {
            ArrayTypeDefinitionSyntax::UnboundedArrayDefinition(inner) => {
                CheckedArrayTypeDefinition::UnboundedArrayDefinition(
                    CheckedUnboundedArrayDefinition::cast_unchecked(inner),
                )
            }
            ArrayTypeDefinitionSyntax::ConstrainedArrayDefinition(inner) => {
                CheckedArrayTypeDefinition::ConstrainedArrayDefinition(
                    CheckedConstrainedArrayDefinition::cast_unchecked(inner),
                )
            }
        }
    }
    fn raw(&self) -> Self::Syntax {
        match self {
            CheckedArrayTypeDefinition::UnboundedArrayDefinition(inner) => {
                ArrayTypeDefinitionSyntax::UnboundedArrayDefinition(inner.raw())
            }
            CheckedArrayTypeDefinition::ConstrainedArrayDefinition(inner) => {
                ArrayTypeDefinitionSyntax::ConstrainedArrayDefinition(inner.raw())
            }
        }
    }
}
#[derive(Debug, Clone)]
pub enum CheckedCompositeTypeDefinition {
    ArrayTypeDefinition(CheckedArrayTypeDefinition),
    RecordTypeDefinition(CheckedRecordTypeDefinition),
}
impl CheckedNode for CheckedCompositeTypeDefinition {
    type Syntax = CompositeTypeDefinitionSyntax;
    fn cast_unchecked(syntax: CompositeTypeDefinitionSyntax) -> Self {
        match syntax {
            CompositeTypeDefinitionSyntax::ArrayTypeDefinition(inner) => {
                CheckedCompositeTypeDefinition::ArrayTypeDefinition(
                    CheckedArrayTypeDefinition::cast_unchecked(inner),
                )
            }
            CompositeTypeDefinitionSyntax::RecordTypeDefinition(inner) => {
                CheckedCompositeTypeDefinition::RecordTypeDefinition(
                    CheckedRecordTypeDefinition::cast_unchecked(inner),
                )
            }
        }
    }
    fn raw(&self) -> Self::Syntax {
        match self {
            CheckedCompositeTypeDefinition::ArrayTypeDefinition(inner) => {
                CompositeTypeDefinitionSyntax::ArrayTypeDefinition(inner.raw())
            }
            CheckedCompositeTypeDefinition::RecordTypeDefinition(inner) => {
                CompositeTypeDefinitionSyntax::RecordTypeDefinition(inner.raw())
            }
        }
    }
}
#[derive(Debug, Clone)]
pub struct CheckedConstrainedArrayDefinition(ConstrainedArrayDefinitionSyntax);
impl CheckedNode for CheckedConstrainedArrayDefinition {
    type Syntax = ConstrainedArrayDefinitionSyntax;
    fn cast_unchecked(syntax: ConstrainedArrayDefinitionSyntax) -> Self {
        CheckedConstrainedArrayDefinition(syntax)
    }
    fn raw(&self) -> ConstrainedArrayDefinitionSyntax {
        self.0.clone()
    }
}
impl CheckedConstrainedArrayDefinition {
    pub fn array_token(&self) -> SyntaxToken {
        self.0.array_token().unwrap()
    }
    pub fn index_constraint(&self) -> CheckedIndexConstraint {
        CheckedIndexConstraint::cast_unchecked(self.0.index_constraint().unwrap())
    }
    pub fn of_token(&self) -> SyntaxToken {
        self.0.of_token().unwrap()
    }
    pub fn subtype_indication(&self) -> CheckedSubtypeIndication {
        CheckedSubtypeIndication::cast_unchecked(self.0.subtype_indication().unwrap())
    }
}
#[derive(Debug, Clone)]
pub struct CheckedSubtypeIndicationDiscreteDiscreteRange(
    SubtypeIndicationDiscreteDiscreteRangeSyntax,
);
impl CheckedNode for CheckedSubtypeIndicationDiscreteDiscreteRange {
    type Syntax = SubtypeIndicationDiscreteDiscreteRangeSyntax;
    fn cast_unchecked(syntax: SubtypeIndicationDiscreteDiscreteRangeSyntax) -> Self {
        CheckedSubtypeIndicationDiscreteDiscreteRange(syntax)
    }
    fn raw(&self) -> SubtypeIndicationDiscreteDiscreteRangeSyntax {
        self.0.clone()
    }
}
impl CheckedSubtypeIndicationDiscreteDiscreteRange {
    pub fn subtype_indication(&self) -> CheckedSubtypeIndication {
        CheckedSubtypeIndication::cast_unchecked(self.0.subtype_indication().unwrap())
    }
}
#[derive(Debug, Clone)]
pub struct CheckedSubtypeIndicationDiscreteRange(SubtypeIndicationDiscreteRangeSyntax);
impl CheckedNode for CheckedSubtypeIndicationDiscreteRange {
    type Syntax = SubtypeIndicationDiscreteRangeSyntax;
    fn cast_unchecked(syntax: SubtypeIndicationDiscreteRangeSyntax) -> Self {
        CheckedSubtypeIndicationDiscreteRange(syntax)
    }
    fn raw(&self) -> SubtypeIndicationDiscreteRangeSyntax {
        self.0.clone()
    }
}
impl CheckedSubtypeIndicationDiscreteRange {
    pub fn range(&self) -> CheckedRange {
        CheckedRange::cast_unchecked(self.0.range().unwrap())
    }
}
#[derive(Debug, Clone)]
pub struct CheckedOpenDiscreteRange(OpenDiscreteRangeSyntax);
impl CheckedNode for CheckedOpenDiscreteRange {
    type Syntax = OpenDiscreteRangeSyntax;
    fn cast_unchecked(syntax: OpenDiscreteRangeSyntax) -> Self {
        CheckedOpenDiscreteRange(syntax)
    }
    fn raw(&self) -> OpenDiscreteRangeSyntax {
        self.0.clone()
    }
}
impl CheckedOpenDiscreteRange {
    pub fn open_token(&self) -> SyntaxToken {
        self.0.open_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub enum CheckedDiscreteRange {
    SubtypeIndicationDiscreteDiscreteRange(CheckedSubtypeIndicationDiscreteDiscreteRange),
    SubtypeIndicationDiscreteRange(CheckedSubtypeIndicationDiscreteRange),
    OpenDiscreteRange(CheckedOpenDiscreteRange),
}
impl CheckedNode for CheckedDiscreteRange {
    type Syntax = DiscreteRangeSyntax;
    fn cast_unchecked(syntax: DiscreteRangeSyntax) -> Self {
        match syntax {
            DiscreteRangeSyntax::SubtypeIndicationDiscreteDiscreteRange(inner) => {
                CheckedDiscreteRange::SubtypeIndicationDiscreteDiscreteRange(
                    CheckedSubtypeIndicationDiscreteDiscreteRange::cast_unchecked(inner),
                )
            }
            DiscreteRangeSyntax::SubtypeIndicationDiscreteRange(inner) => {
                CheckedDiscreteRange::SubtypeIndicationDiscreteRange(
                    CheckedSubtypeIndicationDiscreteRange::cast_unchecked(inner),
                )
            }
            DiscreteRangeSyntax::OpenDiscreteRange(inner) => {
                CheckedDiscreteRange::OpenDiscreteRange(CheckedOpenDiscreteRange::cast_unchecked(
                    inner,
                ))
            }
        }
    }
    fn raw(&self) -> Self::Syntax {
        match self {
            CheckedDiscreteRange::SubtypeIndicationDiscreteDiscreteRange(inner) => {
                DiscreteRangeSyntax::SubtypeIndicationDiscreteDiscreteRange(inner.raw())
            }
            CheckedDiscreteRange::SubtypeIndicationDiscreteRange(inner) => {
                DiscreteRangeSyntax::SubtypeIndicationDiscreteRange(inner.raw())
            }
            CheckedDiscreteRange::OpenDiscreteRange(inner) => {
                DiscreteRangeSyntax::OpenDiscreteRange(inner.raw())
            }
        }
    }
}
#[derive(Debug, Clone)]
pub struct CheckedElementDeclaration(ElementDeclarationSyntax);
impl CheckedNode for CheckedElementDeclaration {
    type Syntax = ElementDeclarationSyntax;
    fn cast_unchecked(syntax: ElementDeclarationSyntax) -> Self {
        CheckedElementDeclaration(syntax)
    }
    fn raw(&self) -> ElementDeclarationSyntax {
        self.0.clone()
    }
}
impl CheckedElementDeclaration {
    pub fn identifier_list(&self) -> CheckedIdentifierList {
        CheckedIdentifierList::cast_unchecked(self.0.identifier_list().unwrap())
    }
    pub fn colon_token(&self) -> SyntaxToken {
        self.0.colon_token().unwrap()
    }
    pub fn subtype_indication(&self) -> CheckedSubtypeIndication {
        CheckedSubtypeIndication::cast_unchecked(self.0.subtype_indication().unwrap())
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.0.semi_colon_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedEnumerationTypeDefinition(EnumerationTypeDefinitionSyntax);
impl CheckedNode for CheckedEnumerationTypeDefinition {
    type Syntax = EnumerationTypeDefinitionSyntax;
    fn cast_unchecked(syntax: EnumerationTypeDefinitionSyntax) -> Self {
        CheckedEnumerationTypeDefinition(syntax)
    }
    fn raw(&self) -> EnumerationTypeDefinitionSyntax {
        self.0.clone()
    }
}
impl CheckedEnumerationTypeDefinition {
    pub fn left_par_token(&self) -> SyntaxToken {
        self.0.left_par_token().unwrap()
    }
    pub fn discrete_ranges(&self) -> impl Iterator<Item = CheckedDiscreteRange> + use<'_> {
        self.0
            .discrete_ranges()
            .map(CheckedDiscreteRange::cast_unchecked)
    }
    pub fn comma_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0.comma_token()
    }
    pub fn right_par_token(&self) -> SyntaxToken {
        self.0.right_par_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedFileTypeDefinition(FileTypeDefinitionSyntax);
impl CheckedNode for CheckedFileTypeDefinition {
    type Syntax = FileTypeDefinitionSyntax;
    fn cast_unchecked(syntax: FileTypeDefinitionSyntax) -> Self {
        CheckedFileTypeDefinition(syntax)
    }
    fn raw(&self) -> FileTypeDefinitionSyntax {
        self.0.clone()
    }
}
impl CheckedFileTypeDefinition {
    pub fn file_token(&self) -> SyntaxToken {
        self.0.file_token().unwrap()
    }
    pub fn of_token(&self) -> SyntaxToken {
        self.0.of_token().unwrap()
    }
    pub fn name(&self) -> CheckedName {
        CheckedName::cast_unchecked(self.0.name().unwrap())
    }
}
#[derive(Debug, Clone)]
pub struct CheckedIdentifierList(IdentifierListSyntax);
impl CheckedNode for CheckedIdentifierList {
    type Syntax = IdentifierListSyntax;
    fn cast_unchecked(syntax: IdentifierListSyntax) -> Self {
        CheckedIdentifierList(syntax)
    }
    fn raw(&self) -> IdentifierListSyntax {
        self.0.clone()
    }
}
impl CheckedIdentifierList {
    pub fn identifier_token(&self) -> SyntaxToken {
        self.0.identifier_token().unwrap()
    }
    pub fn comma_token(&self) -> SyntaxToken {
        self.0.comma_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedIncompleteTypeDeclaration(IncompleteTypeDeclarationSyntax);
impl CheckedNode for CheckedIncompleteTypeDeclaration {
    type Syntax = IncompleteTypeDeclarationSyntax;
    fn cast_unchecked(syntax: IncompleteTypeDeclarationSyntax) -> Self {
        CheckedIncompleteTypeDeclaration(syntax)
    }
    fn raw(&self) -> IncompleteTypeDeclarationSyntax {
        self.0.clone()
    }
}
impl CheckedIncompleteTypeDeclaration {
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
pub struct CheckedIndexSubtypeDefinition(IndexSubtypeDefinitionSyntax);
impl CheckedNode for CheckedIndexSubtypeDefinition {
    type Syntax = IndexSubtypeDefinitionSyntax;
    fn cast_unchecked(syntax: IndexSubtypeDefinitionSyntax) -> Self {
        CheckedIndexSubtypeDefinition(syntax)
    }
    fn raw(&self) -> IndexSubtypeDefinitionSyntax {
        self.0.clone()
    }
}
impl CheckedIndexSubtypeDefinition {
    pub fn name(&self) -> CheckedName {
        CheckedName::cast_unchecked(self.0.name().unwrap())
    }
    pub fn range_token(&self) -> SyntaxToken {
        self.0.range_token().unwrap()
    }
    pub fn box_token(&self) -> SyntaxToken {
        self.0.box_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedPhysicalLiteral(PhysicalLiteralSyntax);
impl CheckedNode for CheckedPhysicalLiteral {
    type Syntax = PhysicalLiteralSyntax;
    fn cast_unchecked(syntax: PhysicalLiteralSyntax) -> Self {
        CheckedPhysicalLiteral(syntax)
    }
    fn raw(&self) -> PhysicalLiteralSyntax {
        self.0.clone()
    }
}
impl CheckedPhysicalLiteral {
    pub fn abstract_literal_token(&self) -> SyntaxToken {
        self.0.abstract_literal_token().unwrap()
    }
    pub fn name(&self) -> CheckedName {
        CheckedName::cast_unchecked(self.0.name().unwrap())
    }
}
#[derive(Debug, Clone)]
pub struct CheckedPhysicalTypeDefinition(PhysicalTypeDefinitionSyntax);
impl CheckedNode for CheckedPhysicalTypeDefinition {
    type Syntax = PhysicalTypeDefinitionSyntax;
    fn cast_unchecked(syntax: PhysicalTypeDefinitionSyntax) -> Self {
        CheckedPhysicalTypeDefinition(syntax)
    }
    fn raw(&self) -> PhysicalTypeDefinitionSyntax {
        self.0.clone()
    }
}
impl CheckedPhysicalTypeDefinition {
    pub fn range_constraint(&self) -> CheckedRangeConstraint {
        CheckedRangeConstraint::cast_unchecked(self.0.range_constraint().unwrap())
    }
    pub fn unit_declarations(&self) -> CheckedUnitDeclarations {
        CheckedUnitDeclarations::cast_unchecked(self.0.unit_declarations().unwrap())
    }
    pub fn physical_type_definition_epilogue(&self) -> CheckedPhysicalTypeDefinitionEpilogue {
        CheckedPhysicalTypeDefinitionEpilogue::cast_unchecked(
            self.0.physical_type_definition_epilogue().unwrap(),
        )
    }
}
#[derive(Debug, Clone)]
pub struct CheckedUnitDeclarations(UnitDeclarationsSyntax);
impl CheckedNode for CheckedUnitDeclarations {
    type Syntax = UnitDeclarationsSyntax;
    fn cast_unchecked(syntax: UnitDeclarationsSyntax) -> Self {
        CheckedUnitDeclarations(syntax)
    }
    fn raw(&self) -> UnitDeclarationsSyntax {
        self.0.clone()
    }
}
impl CheckedUnitDeclarations {
    pub fn units_token(&self) -> SyntaxToken {
        self.0.units_token().unwrap()
    }
    pub fn primary_unit_declaration(&self) -> CheckedPrimaryUnitDeclaration {
        CheckedPrimaryUnitDeclaration::cast_unchecked(self.0.primary_unit_declaration().unwrap())
    }
    pub fn secondary_unit_declarations(
        &self,
    ) -> impl Iterator<Item = CheckedSecondaryUnitDeclaration> + use<'_> {
        self.0
            .secondary_unit_declarations()
            .map(CheckedSecondaryUnitDeclaration::cast_unchecked)
    }
}
#[derive(Debug, Clone)]
pub struct CheckedPhysicalTypeDefinitionEpilogue(PhysicalTypeDefinitionEpilogueSyntax);
impl CheckedNode for CheckedPhysicalTypeDefinitionEpilogue {
    type Syntax = PhysicalTypeDefinitionEpilogueSyntax;
    fn cast_unchecked(syntax: PhysicalTypeDefinitionEpilogueSyntax) -> Self {
        CheckedPhysicalTypeDefinitionEpilogue(syntax)
    }
    fn raw(&self) -> PhysicalTypeDefinitionEpilogueSyntax {
        self.0.clone()
    }
}
impl CheckedPhysicalTypeDefinitionEpilogue {
    pub fn end_token(&self) -> SyntaxToken {
        self.0.end_token().unwrap()
    }
    pub fn units_token(&self) -> SyntaxToken {
        self.0.units_token().unwrap()
    }
    pub fn name_token(&self) -> SyntaxToken {
        self.0.name_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedPrimaryUnitDeclaration(PrimaryUnitDeclarationSyntax);
impl CheckedNode for CheckedPrimaryUnitDeclaration {
    type Syntax = PrimaryUnitDeclarationSyntax;
    fn cast_unchecked(syntax: PrimaryUnitDeclarationSyntax) -> Self {
        CheckedPrimaryUnitDeclaration(syntax)
    }
    fn raw(&self) -> PrimaryUnitDeclarationSyntax {
        self.0.clone()
    }
}
impl CheckedPrimaryUnitDeclaration {
    pub fn identifier_token(&self) -> SyntaxToken {
        self.0.identifier_token().unwrap()
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.0.semi_colon_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedProtectedTypeBody(ProtectedTypeBodySyntax);
impl CheckedNode for CheckedProtectedTypeBody {
    type Syntax = ProtectedTypeBodySyntax;
    fn cast_unchecked(syntax: ProtectedTypeBodySyntax) -> Self {
        CheckedProtectedTypeBody(syntax)
    }
    fn raw(&self) -> ProtectedTypeBodySyntax {
        self.0.clone()
    }
}
impl CheckedProtectedTypeBody {
    pub fn protected_type_body_preamble(&self) -> CheckedProtectedTypeBodyPreamble {
        CheckedProtectedTypeBodyPreamble::cast_unchecked(
            self.0.protected_type_body_preamble().unwrap(),
        )
    }
    pub fn declarations(&self) -> Option<CheckedDeclarations> {
        self.0
            .declarations()
            .map(CheckedDeclarations::cast_unchecked)
    }
    pub fn protected_type_body_epilogue(&self) -> CheckedProtectedTypeBodyEpilogue {
        CheckedProtectedTypeBodyEpilogue::cast_unchecked(
            self.0.protected_type_body_epilogue().unwrap(),
        )
    }
}
#[derive(Debug, Clone)]
pub struct CheckedProtectedTypeBodyPreamble(ProtectedTypeBodyPreambleSyntax);
impl CheckedNode for CheckedProtectedTypeBodyPreamble {
    type Syntax = ProtectedTypeBodyPreambleSyntax;
    fn cast_unchecked(syntax: ProtectedTypeBodyPreambleSyntax) -> Self {
        CheckedProtectedTypeBodyPreamble(syntax)
    }
    fn raw(&self) -> ProtectedTypeBodyPreambleSyntax {
        self.0.clone()
    }
}
impl CheckedProtectedTypeBodyPreamble {
    pub fn protected_token(&self) -> SyntaxToken {
        self.0.protected_token().unwrap()
    }
    pub fn body_token(&self) -> SyntaxToken {
        self.0.body_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedProtectedTypeBodyEpilogue(ProtectedTypeBodyEpilogueSyntax);
impl CheckedNode for CheckedProtectedTypeBodyEpilogue {
    type Syntax = ProtectedTypeBodyEpilogueSyntax;
    fn cast_unchecked(syntax: ProtectedTypeBodyEpilogueSyntax) -> Self {
        CheckedProtectedTypeBodyEpilogue(syntax)
    }
    fn raw(&self) -> ProtectedTypeBodyEpilogueSyntax {
        self.0.clone()
    }
}
impl CheckedProtectedTypeBodyEpilogue {
    pub fn end_token(&self) -> SyntaxToken {
        self.0.end_token().unwrap()
    }
    pub fn protected_token(&self) -> SyntaxToken {
        self.0.protected_token().unwrap()
    }
    pub fn body_token(&self) -> SyntaxToken {
        self.0.body_token().unwrap()
    }
    pub fn name_token(&self) -> SyntaxToken {
        self.0.name_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedProtectedTypeDeclaration(ProtectedTypeDeclarationSyntax);
impl CheckedNode for CheckedProtectedTypeDeclaration {
    type Syntax = ProtectedTypeDeclarationSyntax;
    fn cast_unchecked(syntax: ProtectedTypeDeclarationSyntax) -> Self {
        CheckedProtectedTypeDeclaration(syntax)
    }
    fn raw(&self) -> ProtectedTypeDeclarationSyntax {
        self.0.clone()
    }
}
impl CheckedProtectedTypeDeclaration {
    pub fn protected_type_declaration_preamble(&self) -> CheckedProtectedTypeDeclarationPreamble {
        CheckedProtectedTypeDeclarationPreamble::cast_unchecked(
            self.0.protected_type_declaration_preamble().unwrap(),
        )
    }
    pub fn declarations(&self) -> Option<CheckedDeclarations> {
        self.0
            .declarations()
            .map(CheckedDeclarations::cast_unchecked)
    }
    pub fn protected_type_declaration_epilogue(&self) -> CheckedProtectedTypeDeclarationEpilogue {
        CheckedProtectedTypeDeclarationEpilogue::cast_unchecked(
            self.0.protected_type_declaration_epilogue().unwrap(),
        )
    }
}
#[derive(Debug, Clone)]
pub struct CheckedProtectedTypeDeclarationPreamble(ProtectedTypeDeclarationPreambleSyntax);
impl CheckedNode for CheckedProtectedTypeDeclarationPreamble {
    type Syntax = ProtectedTypeDeclarationPreambleSyntax;
    fn cast_unchecked(syntax: ProtectedTypeDeclarationPreambleSyntax) -> Self {
        CheckedProtectedTypeDeclarationPreamble(syntax)
    }
    fn raw(&self) -> ProtectedTypeDeclarationPreambleSyntax {
        self.0.clone()
    }
}
impl CheckedProtectedTypeDeclarationPreamble {
    pub fn protected_token(&self) -> SyntaxToken {
        self.0.protected_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedProtectedTypeDeclarationEpilogue(ProtectedTypeDeclarationEpilogueSyntax);
impl CheckedNode for CheckedProtectedTypeDeclarationEpilogue {
    type Syntax = ProtectedTypeDeclarationEpilogueSyntax;
    fn cast_unchecked(syntax: ProtectedTypeDeclarationEpilogueSyntax) -> Self {
        CheckedProtectedTypeDeclarationEpilogue(syntax)
    }
    fn raw(&self) -> ProtectedTypeDeclarationEpilogueSyntax {
        self.0.clone()
    }
}
impl CheckedProtectedTypeDeclarationEpilogue {
    pub fn end_token(&self) -> SyntaxToken {
        self.0.end_token().unwrap()
    }
    pub fn protected_token(&self) -> SyntaxToken {
        self.0.protected_token().unwrap()
    }
    pub fn name_token(&self) -> SyntaxToken {
        self.0.name_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub enum CheckedProtectedTypeDefinition {
    ProtectedTypeDeclaration(CheckedProtectedTypeDeclaration),
    ProtectedTypeBody(CheckedProtectedTypeBody),
}
impl CheckedNode for CheckedProtectedTypeDefinition {
    type Syntax = ProtectedTypeDefinitionSyntax;
    fn cast_unchecked(syntax: ProtectedTypeDefinitionSyntax) -> Self {
        match syntax {
            ProtectedTypeDefinitionSyntax::ProtectedTypeDeclaration(inner) => {
                CheckedProtectedTypeDefinition::ProtectedTypeDeclaration(
                    CheckedProtectedTypeDeclaration::cast_unchecked(inner),
                )
            }
            ProtectedTypeDefinitionSyntax::ProtectedTypeBody(inner) => {
                CheckedProtectedTypeDefinition::ProtectedTypeBody(
                    CheckedProtectedTypeBody::cast_unchecked(inner),
                )
            }
        }
    }
    fn raw(&self) -> Self::Syntax {
        match self {
            CheckedProtectedTypeDefinition::ProtectedTypeDeclaration(inner) => {
                ProtectedTypeDefinitionSyntax::ProtectedTypeDeclaration(inner.raw())
            }
            CheckedProtectedTypeDefinition::ProtectedTypeBody(inner) => {
                ProtectedTypeDefinitionSyntax::ProtectedTypeBody(inner.raw())
            }
        }
    }
}
#[derive(Debug, Clone)]
pub struct CheckedRangeExpression(RangeExpressionSyntax);
impl CheckedNode for CheckedRangeExpression {
    type Syntax = RangeExpressionSyntax;
    fn cast_unchecked(syntax: RangeExpressionSyntax) -> Self {
        CheckedRangeExpression(syntax)
    }
    fn raw(&self) -> RangeExpressionSyntax {
        self.0.clone()
    }
}
impl CheckedRangeExpression {
    pub fn lhs(&self) -> CheckedExpression {
        CheckedExpression::cast_unchecked(self.0.lhs().unwrap())
    }
    pub fn direction(&self) -> DirectionSyntax {
        self.0.direction().unwrap()
    }
    pub fn rhs(&self) -> CheckedExpression {
        CheckedExpression::cast_unchecked(self.0.rhs().unwrap())
    }
}
#[derive(Debug, Clone)]
pub struct CheckedAttributeRange(AttributeRangeSyntax);
impl CheckedNode for CheckedAttributeRange {
    type Syntax = AttributeRangeSyntax;
    fn cast_unchecked(syntax: AttributeRangeSyntax) -> Self {
        CheckedAttributeRange(syntax)
    }
    fn raw(&self) -> AttributeRangeSyntax {
        self.0.clone()
    }
}
impl CheckedAttributeRange {
    pub fn name(&self) -> CheckedName {
        CheckedName::cast_unchecked(self.0.name().unwrap())
    }
}
#[derive(Debug, Clone)]
pub enum CheckedRange {
    AttributeRange(CheckedAttributeRange),
    RangeExpression(CheckedRangeExpression),
}
impl CheckedNode for CheckedRange {
    type Syntax = RangeSyntax;
    fn cast_unchecked(syntax: RangeSyntax) -> Self {
        match syntax {
            RangeSyntax::AttributeRange(inner) => {
                CheckedRange::AttributeRange(CheckedAttributeRange::cast_unchecked(inner))
            }
            RangeSyntax::RangeExpression(inner) => {
                CheckedRange::RangeExpression(CheckedRangeExpression::cast_unchecked(inner))
            }
        }
    }
    fn raw(&self) -> Self::Syntax {
        match self {
            CheckedRange::AttributeRange(inner) => RangeSyntax::AttributeRange(inner.raw()),
            CheckedRange::RangeExpression(inner) => RangeSyntax::RangeExpression(inner.raw()),
        }
    }
}
#[derive(Debug, Clone)]
pub struct CheckedRangeConstraint(RangeConstraintSyntax);
impl CheckedNode for CheckedRangeConstraint {
    type Syntax = RangeConstraintSyntax;
    fn cast_unchecked(syntax: RangeConstraintSyntax) -> Self {
        CheckedRangeConstraint(syntax)
    }
    fn raw(&self) -> RangeConstraintSyntax {
        self.0.clone()
    }
}
impl CheckedRangeConstraint {
    pub fn range_token(&self) -> SyntaxToken {
        self.0.range_token().unwrap()
    }
    pub fn range(&self) -> CheckedRange {
        CheckedRange::cast_unchecked(self.0.range().unwrap())
    }
}
#[derive(Debug, Clone)]
pub struct CheckedRecordConstraint(RecordConstraintSyntax);
impl CheckedNode for CheckedRecordConstraint {
    type Syntax = RecordConstraintSyntax;
    fn cast_unchecked(syntax: RecordConstraintSyntax) -> Self {
        CheckedRecordConstraint(syntax)
    }
    fn raw(&self) -> RecordConstraintSyntax {
        self.0.clone()
    }
}
impl CheckedRecordConstraint {
    pub fn left_par_token(&self) -> SyntaxToken {
        self.0.left_par_token().unwrap()
    }
    pub fn record_element_constraints(
        &self,
    ) -> impl Iterator<Item = CheckedRecordElementConstraint> + use<'_> {
        self.0
            .record_element_constraints()
            .map(CheckedRecordElementConstraint::cast_unchecked)
    }
    pub fn comma_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0.comma_token()
    }
    pub fn right_par_token(&self) -> SyntaxToken {
        self.0.right_par_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedRecordElementConstraint(RecordElementConstraintSyntax);
impl CheckedNode for CheckedRecordElementConstraint {
    type Syntax = RecordElementConstraintSyntax;
    fn cast_unchecked(syntax: RecordElementConstraintSyntax) -> Self {
        CheckedRecordElementConstraint(syntax)
    }
    fn raw(&self) -> RecordElementConstraintSyntax {
        self.0.clone()
    }
}
impl CheckedRecordElementConstraint {
    pub fn name(&self) -> CheckedName {
        CheckedName::cast_unchecked(self.0.name().unwrap())
    }
    pub fn constraint(&self) -> CheckedConstraint {
        CheckedConstraint::cast_unchecked(self.0.constraint().unwrap())
    }
}
#[derive(Debug, Clone)]
pub struct CheckedRecordTypeDefinition(RecordTypeDefinitionSyntax);
impl CheckedNode for CheckedRecordTypeDefinition {
    type Syntax = RecordTypeDefinitionSyntax;
    fn cast_unchecked(syntax: RecordTypeDefinitionSyntax) -> Self {
        CheckedRecordTypeDefinition(syntax)
    }
    fn raw(&self) -> RecordTypeDefinitionSyntax {
        self.0.clone()
    }
}
impl CheckedRecordTypeDefinition {
    pub fn record_type_definition_preamble(&self) -> CheckedRecordTypeDefinitionPreamble {
        CheckedRecordTypeDefinitionPreamble::cast_unchecked(
            self.0.record_type_definition_preamble().unwrap(),
        )
    }
    pub fn record_element_declarations(&self) -> Option<CheckedRecordElementDeclarations> {
        self.0
            .record_element_declarations()
            .map(CheckedRecordElementDeclarations::cast_unchecked)
    }
    pub fn record_type_definition_epilogue(&self) -> CheckedRecordTypeDefinitionEpilogue {
        CheckedRecordTypeDefinitionEpilogue::cast_unchecked(
            self.0.record_type_definition_epilogue().unwrap(),
        )
    }
}
#[derive(Debug, Clone)]
pub struct CheckedRecordElementDeclarations(RecordElementDeclarationsSyntax);
impl CheckedNode for CheckedRecordElementDeclarations {
    type Syntax = RecordElementDeclarationsSyntax;
    fn cast_unchecked(syntax: RecordElementDeclarationsSyntax) -> Self {
        CheckedRecordElementDeclarations(syntax)
    }
    fn raw(&self) -> RecordElementDeclarationsSyntax {
        self.0.clone()
    }
}
impl CheckedRecordElementDeclarations {
    pub fn element_declarations(
        &self,
    ) -> impl Iterator<Item = CheckedElementDeclaration> + use<'_> {
        self.0
            .element_declarations()
            .map(CheckedElementDeclaration::cast_unchecked)
    }
}
#[derive(Debug, Clone)]
pub struct CheckedRecordTypeDefinitionPreamble(RecordTypeDefinitionPreambleSyntax);
impl CheckedNode for CheckedRecordTypeDefinitionPreamble {
    type Syntax = RecordTypeDefinitionPreambleSyntax;
    fn cast_unchecked(syntax: RecordTypeDefinitionPreambleSyntax) -> Self {
        CheckedRecordTypeDefinitionPreamble(syntax)
    }
    fn raw(&self) -> RecordTypeDefinitionPreambleSyntax {
        self.0.clone()
    }
}
impl CheckedRecordTypeDefinitionPreamble {
    pub fn record_token(&self) -> SyntaxToken {
        self.0.record_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedRecordTypeDefinitionEpilogue(RecordTypeDefinitionEpilogueSyntax);
impl CheckedNode for CheckedRecordTypeDefinitionEpilogue {
    type Syntax = RecordTypeDefinitionEpilogueSyntax;
    fn cast_unchecked(syntax: RecordTypeDefinitionEpilogueSyntax) -> Self {
        CheckedRecordTypeDefinitionEpilogue(syntax)
    }
    fn raw(&self) -> RecordTypeDefinitionEpilogueSyntax {
        self.0.clone()
    }
}
impl CheckedRecordTypeDefinitionEpilogue {
    pub fn end_token(&self) -> SyntaxToken {
        self.0.end_token().unwrap()
    }
    pub fn record_token(&self) -> SyntaxToken {
        self.0.record_token().unwrap()
    }
    pub fn identifier_token(&self) -> SyntaxToken {
        self.0.identifier_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedNumericTypeDefinition(NumericTypeDefinitionSyntax);
impl CheckedNode for CheckedNumericTypeDefinition {
    type Syntax = NumericTypeDefinitionSyntax;
    fn cast_unchecked(syntax: NumericTypeDefinitionSyntax) -> Self {
        CheckedNumericTypeDefinition(syntax)
    }
    fn raw(&self) -> NumericTypeDefinitionSyntax {
        self.0.clone()
    }
}
impl CheckedNumericTypeDefinition {
    pub fn range_constraint(&self) -> CheckedRangeConstraint {
        CheckedRangeConstraint::cast_unchecked(self.0.range_constraint().unwrap())
    }
}
#[derive(Debug, Clone)]
pub enum CheckedScalarTypeDefinition {
    EnumerationTypeDefinition(CheckedEnumerationTypeDefinition),
    NumericTypeDefinition(CheckedNumericTypeDefinition),
    PhysicalTypeDefinition(CheckedPhysicalTypeDefinition),
}
impl CheckedNode for CheckedScalarTypeDefinition {
    type Syntax = ScalarTypeDefinitionSyntax;
    fn cast_unchecked(syntax: ScalarTypeDefinitionSyntax) -> Self {
        match syntax {
            ScalarTypeDefinitionSyntax::EnumerationTypeDefinition(inner) => {
                CheckedScalarTypeDefinition::EnumerationTypeDefinition(
                    CheckedEnumerationTypeDefinition::cast_unchecked(inner),
                )
            }
            ScalarTypeDefinitionSyntax::NumericTypeDefinition(inner) => {
                CheckedScalarTypeDefinition::NumericTypeDefinition(
                    CheckedNumericTypeDefinition::cast_unchecked(inner),
                )
            }
            ScalarTypeDefinitionSyntax::PhysicalTypeDefinition(inner) => {
                CheckedScalarTypeDefinition::PhysicalTypeDefinition(
                    CheckedPhysicalTypeDefinition::cast_unchecked(inner),
                )
            }
        }
    }
    fn raw(&self) -> Self::Syntax {
        match self {
            CheckedScalarTypeDefinition::EnumerationTypeDefinition(inner) => {
                ScalarTypeDefinitionSyntax::EnumerationTypeDefinition(inner.raw())
            }
            CheckedScalarTypeDefinition::NumericTypeDefinition(inner) => {
                ScalarTypeDefinitionSyntax::NumericTypeDefinition(inner.raw())
            }
            CheckedScalarTypeDefinition::PhysicalTypeDefinition(inner) => {
                ScalarTypeDefinitionSyntax::PhysicalTypeDefinition(inner.raw())
            }
        }
    }
}
#[derive(Debug, Clone)]
pub struct CheckedSecondaryUnitDeclaration(SecondaryUnitDeclarationSyntax);
impl CheckedNode for CheckedSecondaryUnitDeclaration {
    type Syntax = SecondaryUnitDeclarationSyntax;
    fn cast_unchecked(syntax: SecondaryUnitDeclarationSyntax) -> Self {
        CheckedSecondaryUnitDeclaration(syntax)
    }
    fn raw(&self) -> SecondaryUnitDeclarationSyntax {
        self.0.clone()
    }
}
impl CheckedSecondaryUnitDeclaration {
    pub fn identifier_token(&self) -> SyntaxToken {
        self.0.identifier_token().unwrap()
    }
    pub fn eq_token(&self) -> SyntaxToken {
        self.0.eq_token().unwrap()
    }
    pub fn physical_literal(&self) -> CheckedPhysicalLiteral {
        CheckedPhysicalLiteral::cast_unchecked(self.0.physical_literal().unwrap())
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.0.semi_colon_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedIndexConstraint(IndexConstraintSyntax);
impl CheckedNode for CheckedIndexConstraint {
    type Syntax = IndexConstraintSyntax;
    fn cast_unchecked(syntax: IndexConstraintSyntax) -> Self {
        CheckedIndexConstraint(syntax)
    }
    fn raw(&self) -> IndexConstraintSyntax {
        self.0.clone()
    }
}
impl CheckedIndexConstraint {
    pub fn left_par_token(&self) -> SyntaxToken {
        self.0.left_par_token().unwrap()
    }
    pub fn discrete_ranges(&self) -> impl Iterator<Item = CheckedDiscreteRange> + use<'_> {
        self.0
            .discrete_ranges()
            .map(CheckedDiscreteRange::cast_unchecked)
    }
    pub fn comma_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0.comma_token()
    }
    pub fn right_par_token(&self) -> SyntaxToken {
        self.0.right_par_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedIndexSubtypeDefinitionList(IndexSubtypeDefinitionListSyntax);
impl CheckedNode for CheckedIndexSubtypeDefinitionList {
    type Syntax = IndexSubtypeDefinitionListSyntax;
    fn cast_unchecked(syntax: IndexSubtypeDefinitionListSyntax) -> Self {
        CheckedIndexSubtypeDefinitionList(syntax)
    }
    fn raw(&self) -> IndexSubtypeDefinitionListSyntax {
        self.0.clone()
    }
}
impl CheckedIndexSubtypeDefinitionList {
    pub fn index_subtype_definitions(
        &self,
    ) -> impl Iterator<Item = CheckedIndexSubtypeDefinition> + use<'_> {
        self.0
            .index_subtype_definitions()
            .map(CheckedIndexSubtypeDefinition::cast_unchecked)
    }
    pub fn comma_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0.comma_token()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedUnboundedArrayDefinition(UnboundedArrayDefinitionSyntax);
impl CheckedNode for CheckedUnboundedArrayDefinition {
    type Syntax = UnboundedArrayDefinitionSyntax;
    fn cast_unchecked(syntax: UnboundedArrayDefinitionSyntax) -> Self {
        CheckedUnboundedArrayDefinition(syntax)
    }
    fn raw(&self) -> UnboundedArrayDefinitionSyntax {
        self.0.clone()
    }
}
impl CheckedUnboundedArrayDefinition {
    pub fn array_token(&self) -> SyntaxToken {
        self.0.array_token().unwrap()
    }
    pub fn left_par_token(&self) -> SyntaxToken {
        self.0.left_par_token().unwrap()
    }
    pub fn index_subtype_definition_list(&self) -> Option<CheckedIndexSubtypeDefinitionList> {
        self.0
            .index_subtype_definition_list()
            .map(CheckedIndexSubtypeDefinitionList::cast_unchecked)
    }
    pub fn right_par_token(&self) -> SyntaxToken {
        self.0.right_par_token().unwrap()
    }
    pub fn of_token(&self) -> SyntaxToken {
        self.0.of_token().unwrap()
    }
    pub fn subtype_indication(&self) -> CheckedSubtypeIndication {
        CheckedSubtypeIndication::cast_unchecked(self.0.subtype_indication().unwrap())
    }
}
