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
pub struct CheckedSubprogramDeclaration(SubprogramDeclarationSyntax);
impl CheckedNode for CheckedSubprogramDeclaration {
    type Syntax = SubprogramDeclarationSyntax;
    fn cast_unchecked(syntax: SubprogramDeclarationSyntax) -> Self {
        CheckedSubprogramDeclaration(syntax)
    }
    fn raw(&self) -> SubprogramDeclarationSyntax {
        self.0.clone()
    }
}
impl CheckedSubprogramDeclaration {
    pub fn subprogram_specification(&self) -> CheckedSubprogramSpecification {
        CheckedSubprogramSpecification::cast_unchecked(self.0.subprogram_specification().unwrap())
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.0.semi_colon_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub enum CheckedSubprogramSpecification {
    ProcedureSpecification(CheckedProcedureSpecification),
    FunctionSpecification(CheckedFunctionSpecification),
}
impl CheckedNode for CheckedSubprogramSpecification {
    type Syntax = SubprogramSpecificationSyntax;
    fn cast_unchecked(syntax: SubprogramSpecificationSyntax) -> Self {
        match syntax {
            SubprogramSpecificationSyntax::ProcedureSpecification(inner) => {
                CheckedSubprogramSpecification::ProcedureSpecification(
                    CheckedProcedureSpecification::cast_unchecked(inner),
                )
            }
            SubprogramSpecificationSyntax::FunctionSpecification(inner) => {
                CheckedSubprogramSpecification::FunctionSpecification(
                    CheckedFunctionSpecification::cast_unchecked(inner),
                )
            }
        }
    }
    fn raw(&self) -> Self::Syntax {
        match self {
            CheckedSubprogramSpecification::ProcedureSpecification(inner) => {
                SubprogramSpecificationSyntax::ProcedureSpecification(inner.raw())
            }
            CheckedSubprogramSpecification::FunctionSpecification(inner) => {
                SubprogramSpecificationSyntax::FunctionSpecification(inner.raw())
            }
        }
    }
}
#[derive(Debug, Clone)]
pub struct CheckedProcedureSpecification(ProcedureSpecificationSyntax);
impl CheckedNode for CheckedProcedureSpecification {
    type Syntax = ProcedureSpecificationSyntax;
    fn cast_unchecked(syntax: ProcedureSpecificationSyntax) -> Self {
        CheckedProcedureSpecification(syntax)
    }
    fn raw(&self) -> ProcedureSpecificationSyntax {
        self.0.clone()
    }
}
impl CheckedProcedureSpecification {
    pub fn procedure_token(&self) -> SyntaxToken {
        self.0.procedure_token().unwrap()
    }
    pub fn designator(&self) -> DesignatorSyntax {
        self.0.designator().unwrap()
    }
    pub fn subprogram_header(&self) -> Option<CheckedSubprogramHeader> {
        self.0
            .subprogram_header()
            .map(CheckedSubprogramHeader::cast_unchecked)
    }
    pub fn parameter_list(&self) -> Option<CheckedParameterList> {
        self.0
            .parameter_list()
            .map(CheckedParameterList::cast_unchecked)
    }
}
#[derive(Debug, Clone)]
pub struct CheckedFunctionSpecification(FunctionSpecificationSyntax);
impl CheckedNode for CheckedFunctionSpecification {
    type Syntax = FunctionSpecificationSyntax;
    fn cast_unchecked(syntax: FunctionSpecificationSyntax) -> Self {
        CheckedFunctionSpecification(syntax)
    }
    fn raw(&self) -> FunctionSpecificationSyntax {
        self.0.clone()
    }
}
impl CheckedFunctionSpecification {
    pub fn function_purity(&self) -> Option<FunctionPuritySyntax> {
        self.0.function_purity()
    }
    pub fn function_token(&self) -> SyntaxToken {
        self.0.function_token().unwrap()
    }
    pub fn designator(&self) -> DesignatorSyntax {
        self.0.designator().unwrap()
    }
    pub fn subprogram_header(&self) -> Option<CheckedSubprogramHeader> {
        self.0
            .subprogram_header()
            .map(CheckedSubprogramHeader::cast_unchecked)
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
pub struct CheckedParenthesizedInterfaceList(ParenthesizedInterfaceListSyntax);
impl CheckedNode for CheckedParenthesizedInterfaceList {
    type Syntax = ParenthesizedInterfaceListSyntax;
    fn cast_unchecked(syntax: ParenthesizedInterfaceListSyntax) -> Self {
        CheckedParenthesizedInterfaceList(syntax)
    }
    fn raw(&self) -> ParenthesizedInterfaceListSyntax {
        self.0.clone()
    }
}
impl CheckedParenthesizedInterfaceList {
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
pub struct CheckedParameterList(ParameterListSyntax);
impl CheckedNode for CheckedParameterList {
    type Syntax = ParameterListSyntax;
    fn cast_unchecked(syntax: ParameterListSyntax) -> Self {
        CheckedParameterList(syntax)
    }
    fn raw(&self) -> ParameterListSyntax {
        self.0.clone()
    }
}
impl CheckedParameterList {
    pub fn parameter_token(&self) -> Option<SyntaxToken> {
        self.0.parameter_token()
    }
    pub fn parenthesized_interface_list(&self) -> Option<CheckedParenthesizedInterfaceList> {
        self.0
            .parenthesized_interface_list()
            .map(CheckedParenthesizedInterfaceList::cast_unchecked)
    }
}
#[derive(Debug, Clone)]
pub struct CheckedSubprogramHeader(SubprogramHeaderSyntax);
impl CheckedNode for CheckedSubprogramHeader {
    type Syntax = SubprogramHeaderSyntax;
    fn cast_unchecked(syntax: SubprogramHeaderSyntax) -> Self {
        CheckedSubprogramHeader(syntax)
    }
    fn raw(&self) -> SubprogramHeaderSyntax {
        self.0.clone()
    }
}
impl CheckedSubprogramHeader {
    pub fn subprogram_header_generic_clause(&self) -> Option<CheckedSubprogramHeaderGenericClause> {
        self.0
            .subprogram_header_generic_clause()
            .map(CheckedSubprogramHeaderGenericClause::cast_unchecked)
    }
    pub fn generic_map_aspect(&self) -> Option<CheckedGenericMapAspect> {
        self.0
            .generic_map_aspect()
            .map(CheckedGenericMapAspect::cast_unchecked)
    }
}
#[derive(Debug, Clone)]
pub struct CheckedSubprogramHeaderGenericClause(SubprogramHeaderGenericClauseSyntax);
impl CheckedNode for CheckedSubprogramHeaderGenericClause {
    type Syntax = SubprogramHeaderGenericClauseSyntax;
    fn cast_unchecked(syntax: SubprogramHeaderGenericClauseSyntax) -> Self {
        CheckedSubprogramHeaderGenericClause(syntax)
    }
    fn raw(&self) -> SubprogramHeaderGenericClauseSyntax {
        self.0.clone()
    }
}
impl CheckedSubprogramHeaderGenericClause {
    pub fn generic_token(&self) -> SyntaxToken {
        self.0.generic_token().unwrap()
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
pub struct CheckedSubprogramBody(SubprogramBodySyntax);
impl CheckedNode for CheckedSubprogramBody {
    type Syntax = SubprogramBodySyntax;
    fn cast_unchecked(syntax: SubprogramBodySyntax) -> Self {
        CheckedSubprogramBody(syntax)
    }
    fn raw(&self) -> SubprogramBodySyntax {
        self.0.clone()
    }
}
impl CheckedSubprogramBody {
    pub fn subprogram_body_preamble(&self) -> CheckedSubprogramBodyPreamble {
        CheckedSubprogramBodyPreamble::cast_unchecked(self.0.subprogram_body_preamble().unwrap())
    }
    pub fn declarations(&self) -> Option<CheckedDeclarations> {
        self.0
            .declarations()
            .map(CheckedDeclarations::cast_unchecked)
    }
    pub fn declaration_statement_separator(&self) -> CheckedDeclarationStatementSeparator {
        CheckedDeclarationStatementSeparator::cast_unchecked(
            self.0.declaration_statement_separator().unwrap(),
        )
    }
    pub fn concurrent_statements(&self) -> Option<CheckedConcurrentStatements> {
        self.0
            .concurrent_statements()
            .map(CheckedConcurrentStatements::cast_unchecked)
    }
    pub fn subprogram_body_epilogue(&self) -> CheckedSubprogramBodyEpilogue {
        CheckedSubprogramBodyEpilogue::cast_unchecked(self.0.subprogram_body_epilogue().unwrap())
    }
}
#[derive(Debug, Clone)]
pub struct CheckedSubprogramBodyPreamble(SubprogramBodyPreambleSyntax);
impl CheckedNode for CheckedSubprogramBodyPreamble {
    type Syntax = SubprogramBodyPreambleSyntax;
    fn cast_unchecked(syntax: SubprogramBodyPreambleSyntax) -> Self {
        CheckedSubprogramBodyPreamble(syntax)
    }
    fn raw(&self) -> SubprogramBodyPreambleSyntax {
        self.0.clone()
    }
}
impl CheckedSubprogramBodyPreamble {
    pub fn subprogram_specification(&self) -> CheckedSubprogramSpecification {
        CheckedSubprogramSpecification::cast_unchecked(self.0.subprogram_specification().unwrap())
    }
    pub fn is_token(&self) -> SyntaxToken {
        self.0.is_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedSubprogramBodyEpilogue(SubprogramBodyEpilogueSyntax);
impl CheckedNode for CheckedSubprogramBodyEpilogue {
    type Syntax = SubprogramBodyEpilogueSyntax;
    fn cast_unchecked(syntax: SubprogramBodyEpilogueSyntax) -> Self {
        CheckedSubprogramBodyEpilogue(syntax)
    }
    fn raw(&self) -> SubprogramBodyEpilogueSyntax {
        self.0.clone()
    }
}
impl CheckedSubprogramBodyEpilogue {
    pub fn end_token(&self) -> SyntaxToken {
        self.0.end_token().unwrap()
    }
    pub fn subprogram_kind(&self) -> Option<SubprogramKindSyntax> {
        self.0.subprogram_kind()
    }
    pub fn designator(&self) -> Option<DesignatorSyntax> {
        self.0.designator()
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.0.semi_colon_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedSubprogramInstantiationDeclaration(SubprogramInstantiationDeclarationSyntax);
impl CheckedNode for CheckedSubprogramInstantiationDeclaration {
    type Syntax = SubprogramInstantiationDeclarationSyntax;
    fn cast_unchecked(syntax: SubprogramInstantiationDeclarationSyntax) -> Self {
        CheckedSubprogramInstantiationDeclaration(syntax)
    }
    fn raw(&self) -> SubprogramInstantiationDeclarationSyntax {
        self.0.clone()
    }
}
impl CheckedSubprogramInstantiationDeclaration {
    pub fn subprogram_instantiation_declaration_preamble(
        &self,
    ) -> CheckedSubprogramInstantiationDeclarationPreamble {
        CheckedSubprogramInstantiationDeclarationPreamble::cast_unchecked(
            self.0
                .subprogram_instantiation_declaration_preamble()
                .unwrap(),
        )
    }
    pub fn generic_map_aspect(&self) -> Option<CheckedGenericMapAspect> {
        self.0
            .generic_map_aspect()
            .map(CheckedGenericMapAspect::cast_unchecked)
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.0.semi_colon_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedSubprogramInstantiationDeclarationPreamble(
    SubprogramInstantiationDeclarationPreambleSyntax,
);
impl CheckedNode for CheckedSubprogramInstantiationDeclarationPreamble {
    type Syntax = SubprogramInstantiationDeclarationPreambleSyntax;
    fn cast_unchecked(syntax: SubprogramInstantiationDeclarationPreambleSyntax) -> Self {
        CheckedSubprogramInstantiationDeclarationPreamble(syntax)
    }
    fn raw(&self) -> SubprogramInstantiationDeclarationPreambleSyntax {
        self.0.clone()
    }
}
impl CheckedSubprogramInstantiationDeclarationPreamble {
    pub fn subprogram_kind(&self) -> SubprogramKindSyntax {
        self.0.subprogram_kind().unwrap()
    }
    pub fn name_token(&self) -> SyntaxToken {
        self.0.name_token().unwrap()
    }
    pub fn is_token(&self) -> SyntaxToken {
        self.0.is_token().unwrap()
    }
    pub fn new_token(&self) -> SyntaxToken {
        self.0.new_token().unwrap()
    }
    pub fn name(&self) -> CheckedName {
        CheckedName::cast_unchecked(self.0.name().unwrap())
    }
    pub fn signature(&self) -> Option<CheckedSignature> {
        self.0.signature().map(CheckedSignature::cast_unchecked)
    }
}
#[derive(Debug, Clone)]
pub struct CheckedPackage(PackageSyntax);
impl CheckedNode for CheckedPackage {
    type Syntax = PackageSyntax;
    fn cast_unchecked(syntax: PackageSyntax) -> Self {
        CheckedPackage(syntax)
    }
    fn raw(&self) -> PackageSyntax {
        self.0.clone()
    }
}
impl CheckedPackage {
    pub fn package_preamble(&self) -> CheckedPackagePreamble {
        CheckedPackagePreamble::cast_unchecked(self.0.package_preamble().unwrap())
    }
    pub fn package_header(&self) -> Option<CheckedPackageHeader> {
        self.0
            .package_header()
            .map(CheckedPackageHeader::cast_unchecked)
    }
    pub fn declarations(&self) -> Option<CheckedDeclarations> {
        self.0
            .declarations()
            .map(CheckedDeclarations::cast_unchecked)
    }
    pub fn package_epilogue(&self) -> CheckedPackageEpilogue {
        CheckedPackageEpilogue::cast_unchecked(self.0.package_epilogue().unwrap())
    }
}
#[derive(Debug, Clone)]
pub struct CheckedPackagePreamble(PackagePreambleSyntax);
impl CheckedNode for CheckedPackagePreamble {
    type Syntax = PackagePreambleSyntax;
    fn cast_unchecked(syntax: PackagePreambleSyntax) -> Self {
        CheckedPackagePreamble(syntax)
    }
    fn raw(&self) -> PackagePreambleSyntax {
        self.0.clone()
    }
}
impl CheckedPackagePreamble {
    pub fn package_token(&self) -> SyntaxToken {
        self.0.package_token().unwrap()
    }
    pub fn name_token(&self) -> SyntaxToken {
        self.0.name_token().unwrap()
    }
    pub fn is_token(&self) -> SyntaxToken {
        self.0.is_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedPackageEpilogue(PackageEpilogueSyntax);
impl CheckedNode for CheckedPackageEpilogue {
    type Syntax = PackageEpilogueSyntax;
    fn cast_unchecked(syntax: PackageEpilogueSyntax) -> Self {
        CheckedPackageEpilogue(syntax)
    }
    fn raw(&self) -> PackageEpilogueSyntax {
        self.0.clone()
    }
}
impl CheckedPackageEpilogue {
    pub fn end_token(&self) -> SyntaxToken {
        self.0.end_token().unwrap()
    }
    pub fn package_token(&self) -> Option<SyntaxToken> {
        self.0.package_token()
    }
    pub fn identifier_token(&self) -> Option<SyntaxToken> {
        self.0.identifier_token()
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.0.semi_colon_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedPackageHeader(PackageHeaderSyntax);
impl CheckedNode for CheckedPackageHeader {
    type Syntax = PackageHeaderSyntax;
    fn cast_unchecked(syntax: PackageHeaderSyntax) -> Self {
        CheckedPackageHeader(syntax)
    }
    fn raw(&self) -> PackageHeaderSyntax {
        self.0.clone()
    }
}
impl CheckedPackageHeader {
    pub fn generic_clause(&self) -> Option<CheckedGenericClause> {
        self.0
            .generic_clause()
            .map(CheckedGenericClause::cast_unchecked)
    }
    pub fn generic_map_aspect(&self) -> Option<CheckedGenericMapAspect> {
        self.0
            .generic_map_aspect()
            .map(CheckedGenericMapAspect::cast_unchecked)
    }
    pub fn semi_colon_token(&self) -> Option<SyntaxToken> {
        self.0.semi_colon_token()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedPackageBody(PackageBodySyntax);
impl CheckedNode for CheckedPackageBody {
    type Syntax = PackageBodySyntax;
    fn cast_unchecked(syntax: PackageBodySyntax) -> Self {
        CheckedPackageBody(syntax)
    }
    fn raw(&self) -> PackageBodySyntax {
        self.0.clone()
    }
}
impl CheckedPackageBody {
    pub fn package_body_preamble(&self) -> CheckedPackageBodyPreamble {
        CheckedPackageBodyPreamble::cast_unchecked(self.0.package_body_preamble().unwrap())
    }
    pub fn declarations(&self) -> Option<CheckedDeclarations> {
        self.0
            .declarations()
            .map(CheckedDeclarations::cast_unchecked)
    }
    pub fn package_body_epilogue(&self) -> CheckedPackageBodyEpilogue {
        CheckedPackageBodyEpilogue::cast_unchecked(self.0.package_body_epilogue().unwrap())
    }
}
#[derive(Debug, Clone)]
pub struct CheckedPackageBodyPreamble(PackageBodyPreambleSyntax);
impl CheckedNode for CheckedPackageBodyPreamble {
    type Syntax = PackageBodyPreambleSyntax;
    fn cast_unchecked(syntax: PackageBodyPreambleSyntax) -> Self {
        CheckedPackageBodyPreamble(syntax)
    }
    fn raw(&self) -> PackageBodyPreambleSyntax {
        self.0.clone()
    }
}
impl CheckedPackageBodyPreamble {
    pub fn package_token(&self) -> SyntaxToken {
        self.0.package_token().unwrap()
    }
    pub fn body_token(&self) -> SyntaxToken {
        self.0.body_token().unwrap()
    }
    pub fn name_token(&self) -> SyntaxToken {
        self.0.name_token().unwrap()
    }
    pub fn is_token(&self) -> SyntaxToken {
        self.0.is_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedPackageBodyEpilogue(PackageBodyEpilogueSyntax);
impl CheckedNode for CheckedPackageBodyEpilogue {
    type Syntax = PackageBodyEpilogueSyntax;
    fn cast_unchecked(syntax: PackageBodyEpilogueSyntax) -> Self {
        CheckedPackageBodyEpilogue(syntax)
    }
    fn raw(&self) -> PackageBodyEpilogueSyntax {
        self.0.clone()
    }
}
impl CheckedPackageBodyEpilogue {
    pub fn end_token(&self) -> SyntaxToken {
        self.0.end_token().unwrap()
    }
    pub fn package_token(&self) -> Option<SyntaxToken> {
        self.0.package_token()
    }
    pub fn body_token(&self) -> Option<SyntaxToken> {
        self.0.body_token()
    }
    pub fn identifier_token(&self) -> Option<SyntaxToken> {
        self.0.identifier_token()
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.0.semi_colon_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedPackageInstantiation(PackageInstantiationSyntax);
impl CheckedNode for CheckedPackageInstantiation {
    type Syntax = PackageInstantiationSyntax;
    fn cast_unchecked(syntax: PackageInstantiationSyntax) -> Self {
        CheckedPackageInstantiation(syntax)
    }
    fn raw(&self) -> PackageInstantiationSyntax {
        self.0.clone()
    }
}
impl CheckedPackageInstantiation {
    pub fn package_instantiation_preamble(&self) -> CheckedPackageInstantiationPreamble {
        CheckedPackageInstantiationPreamble::cast_unchecked(
            self.0.package_instantiation_preamble().unwrap(),
        )
    }
    pub fn generic_map_aspect(&self) -> Option<CheckedGenericMapAspect> {
        self.0
            .generic_map_aspect()
            .map(CheckedGenericMapAspect::cast_unchecked)
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.0.semi_colon_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedPackageInstantiationPreamble(PackageInstantiationPreambleSyntax);
impl CheckedNode for CheckedPackageInstantiationPreamble {
    type Syntax = PackageInstantiationPreambleSyntax;
    fn cast_unchecked(syntax: PackageInstantiationPreambleSyntax) -> Self {
        CheckedPackageInstantiationPreamble(syntax)
    }
    fn raw(&self) -> PackageInstantiationPreambleSyntax {
        self.0.clone()
    }
}
impl CheckedPackageInstantiationPreamble {
    pub fn package_token(&self) -> SyntaxToken {
        self.0.package_token().unwrap()
    }
    pub fn name_token(&self) -> SyntaxToken {
        self.0.name_token().unwrap()
    }
    pub fn is_token(&self) -> SyntaxToken {
        self.0.is_token().unwrap()
    }
    pub fn new_token(&self) -> SyntaxToken {
        self.0.new_token().unwrap()
    }
    pub fn name(&self) -> CheckedName {
        CheckedName::cast_unchecked(self.0.name().unwrap())
    }
}
#[derive(Debug, Clone)]
pub struct CheckedSignature(SignatureSyntax);
impl CheckedNode for CheckedSignature {
    type Syntax = SignatureSyntax;
    fn cast_unchecked(syntax: SignatureSyntax) -> Self {
        CheckedSignature(syntax)
    }
    fn raw(&self) -> SignatureSyntax {
        self.0.clone()
    }
}
impl CheckedSignature {
    pub fn left_square_token(&self) -> SyntaxToken {
        self.0.left_square_token().unwrap()
    }
    pub fn names(&self) -> impl Iterator<Item = CheckedName> + use<'_> {
        self.0.names().map(CheckedName::cast_unchecked)
    }
    pub fn return_token(&self) -> SyntaxToken {
        self.0.return_token().unwrap()
    }
    pub fn return_type(&self) -> CheckedName {
        CheckedName::cast_unchecked(self.0.return_type().unwrap())
    }
    pub fn right_square_token(&self) -> SyntaxToken {
        self.0.right_square_token().unwrap()
    }
}
