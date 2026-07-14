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
pub struct CheckedContextClause(ContextClauseSyntax);
impl CheckedNode for CheckedContextClause {
    type Syntax = ContextClauseSyntax;
    fn cast_unchecked(syntax: ContextClauseSyntax) -> Self {
        CheckedContextClause(syntax)
    }
    fn raw(&self) -> ContextClauseSyntax {
        self.0.clone()
    }
}
impl CheckedContextClause {
    pub fn context_items(&self) -> impl Iterator<Item = CheckedContextItem> + use<'_> {
        self.0
            .context_items()
            .map(CheckedContextItem::cast_unchecked)
    }
}
#[derive(Debug, Clone)]
pub struct CheckedContextDeclaration(ContextDeclarationSyntax);
impl CheckedNode for CheckedContextDeclaration {
    type Syntax = ContextDeclarationSyntax;
    fn cast_unchecked(syntax: ContextDeclarationSyntax) -> Self {
        CheckedContextDeclaration(syntax)
    }
    fn raw(&self) -> ContextDeclarationSyntax {
        self.0.clone()
    }
}
impl CheckedContextDeclaration {
    pub fn context_declaration_preamble(&self) -> CheckedContextDeclarationPreamble {
        CheckedContextDeclarationPreamble::cast_unchecked(
            self.0.context_declaration_preamble().unwrap(),
        )
    }
    pub fn context_clause(&self) -> Option<CheckedContextClause> {
        self.0
            .context_clause()
            .map(CheckedContextClause::cast_unchecked)
    }
    pub fn context_declaration_epilogue(&self) -> CheckedContextDeclarationEpilogue {
        CheckedContextDeclarationEpilogue::cast_unchecked(
            self.0.context_declaration_epilogue().unwrap(),
        )
    }
}
#[derive(Debug, Clone)]
pub struct CheckedContextDeclarationPreamble(ContextDeclarationPreambleSyntax);
impl CheckedNode for CheckedContextDeclarationPreamble {
    type Syntax = ContextDeclarationPreambleSyntax;
    fn cast_unchecked(syntax: ContextDeclarationPreambleSyntax) -> Self {
        CheckedContextDeclarationPreamble(syntax)
    }
    fn raw(&self) -> ContextDeclarationPreambleSyntax {
        self.0.clone()
    }
}
impl CheckedContextDeclarationPreamble {
    pub fn context_token(&self) -> SyntaxToken {
        self.0.context_token().unwrap()
    }
    pub fn name_token(&self) -> SyntaxToken {
        self.0.name_token().unwrap()
    }
    pub fn is_token(&self) -> SyntaxToken {
        self.0.is_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedContextDeclarationEpilogue(ContextDeclarationEpilogueSyntax);
impl CheckedNode for CheckedContextDeclarationEpilogue {
    type Syntax = ContextDeclarationEpilogueSyntax;
    fn cast_unchecked(syntax: ContextDeclarationEpilogueSyntax) -> Self {
        CheckedContextDeclarationEpilogue(syntax)
    }
    fn raw(&self) -> ContextDeclarationEpilogueSyntax {
        self.0.clone()
    }
}
impl CheckedContextDeclarationEpilogue {
    pub fn end_token(&self) -> SyntaxToken {
        self.0.end_token().unwrap()
    }
    pub fn context_token(&self) -> Option<SyntaxToken> {
        self.0.context_token()
    }
    pub fn identifier_token(&self) -> Option<SyntaxToken> {
        self.0.identifier_token()
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.0.semi_colon_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedUseClauseContextItem(UseClauseContextItemSyntax);
impl CheckedNode for CheckedUseClauseContextItem {
    type Syntax = UseClauseContextItemSyntax;
    fn cast_unchecked(syntax: UseClauseContextItemSyntax) -> Self {
        CheckedUseClauseContextItem(syntax)
    }
    fn raw(&self) -> UseClauseContextItemSyntax {
        self.0.clone()
    }
}
impl CheckedUseClauseContextItem {
    pub fn use_clause(&self) -> CheckedUseClause {
        CheckedUseClause::cast_unchecked(self.0.use_clause().unwrap())
    }
}
#[derive(Debug, Clone)]
pub struct CheckedContextReference(ContextReferenceSyntax);
impl CheckedNode for CheckedContextReference {
    type Syntax = ContextReferenceSyntax;
    fn cast_unchecked(syntax: ContextReferenceSyntax) -> Self {
        CheckedContextReference(syntax)
    }
    fn raw(&self) -> ContextReferenceSyntax {
        self.0.clone()
    }
}
impl CheckedContextReference {
    pub fn context_token(&self) -> SyntaxToken {
        self.0.context_token().unwrap()
    }
    pub fn name_list(&self) -> Option<CheckedNameList> {
        self.0.name_list().map(CheckedNameList::cast_unchecked)
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.0.semi_colon_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub enum CheckedContextItem {
    LibraryClause(CheckedLibraryClause),
    UseClauseContextItem(CheckedUseClauseContextItem),
    ContextReference(CheckedContextReference),
}
impl CheckedNode for CheckedContextItem {
    type Syntax = ContextItemSyntax;
    fn cast_unchecked(syntax: ContextItemSyntax) -> Self {
        match syntax {
            ContextItemSyntax::LibraryClause(inner) => {
                CheckedContextItem::LibraryClause(CheckedLibraryClause::cast_unchecked(inner))
            }
            ContextItemSyntax::UseClauseContextItem(inner) => {
                CheckedContextItem::UseClauseContextItem(
                    CheckedUseClauseContextItem::cast_unchecked(inner),
                )
            }
            ContextItemSyntax::ContextReference(inner) => {
                CheckedContextItem::ContextReference(CheckedContextReference::cast_unchecked(inner))
            }
        }
    }
    fn raw(&self) -> Self::Syntax {
        match self {
            CheckedContextItem::LibraryClause(inner) => {
                ContextItemSyntax::LibraryClause(inner.raw())
            }
            CheckedContextItem::UseClauseContextItem(inner) => {
                ContextItemSyntax::UseClauseContextItem(inner.raw())
            }
            CheckedContextItem::ContextReference(inner) => {
                ContextItemSyntax::ContextReference(inner.raw())
            }
        }
    }
}
#[derive(Debug, Clone)]
pub struct CheckedDesignFile(DesignFileSyntax);
impl CheckedNode for CheckedDesignFile {
    type Syntax = DesignFileSyntax;
    fn cast_unchecked(syntax: DesignFileSyntax) -> Self {
        CheckedDesignFile(syntax)
    }
    fn raw(&self) -> DesignFileSyntax {
        self.0.clone()
    }
}
impl CheckedDesignFile {
    pub fn design_units(&self) -> impl Iterator<Item = CheckedDesignUnit> + use<'_> {
        self.0.design_units().map(CheckedDesignUnit::cast_unchecked)
    }
    pub fn eof_token(&self) -> SyntaxToken {
        self.0.eof_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedDesignUnit(DesignUnitSyntax);
impl CheckedNode for CheckedDesignUnit {
    type Syntax = DesignUnitSyntax;
    fn cast_unchecked(syntax: DesignUnitSyntax) -> Self {
        CheckedDesignUnit(syntax)
    }
    fn raw(&self) -> DesignUnitSyntax {
        self.0.clone()
    }
}
impl CheckedDesignUnit {
    pub fn context_clause(&self) -> Option<CheckedContextClause> {
        self.0
            .context_clause()
            .map(CheckedContextClause::cast_unchecked)
    }
    pub fn library_unit(&self) -> CheckedLibraryUnit {
        CheckedLibraryUnit::cast_unchecked(self.0.library_unit().unwrap())
    }
}
#[derive(Debug, Clone)]
pub struct CheckedLibraryClause(LibraryClauseSyntax);
impl CheckedNode for CheckedLibraryClause {
    type Syntax = LibraryClauseSyntax;
    fn cast_unchecked(syntax: LibraryClauseSyntax) -> Self {
        CheckedLibraryClause(syntax)
    }
    fn raw(&self) -> LibraryClauseSyntax {
        self.0.clone()
    }
}
impl CheckedLibraryClause {
    pub fn library_token(&self) -> SyntaxToken {
        self.0.library_token().unwrap()
    }
    pub fn identifier_list(&self) -> CheckedIdentifierList {
        CheckedIdentifierList::cast_unchecked(self.0.identifier_list().unwrap())
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.0.semi_colon_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub enum CheckedLibraryUnit {
    PrimaryUnit(CheckedPrimaryUnit),
    SecondaryUnit(CheckedSecondaryUnit),
}
impl CheckedNode for CheckedLibraryUnit {
    type Syntax = LibraryUnitSyntax;
    fn cast_unchecked(syntax: LibraryUnitSyntax) -> Self {
        match syntax {
            LibraryUnitSyntax::PrimaryUnit(inner) => {
                CheckedLibraryUnit::PrimaryUnit(CheckedPrimaryUnit::cast_unchecked(inner))
            }
            LibraryUnitSyntax::SecondaryUnit(inner) => {
                CheckedLibraryUnit::SecondaryUnit(CheckedSecondaryUnit::cast_unchecked(inner))
            }
        }
    }
    fn raw(&self) -> Self::Syntax {
        match self {
            CheckedLibraryUnit::PrimaryUnit(inner) => LibraryUnitSyntax::PrimaryUnit(inner.raw()),
            CheckedLibraryUnit::SecondaryUnit(inner) => {
                LibraryUnitSyntax::SecondaryUnit(inner.raw())
            }
        }
    }
}
#[derive(Debug, Clone)]
pub struct CheckedPrimaryUnitPackageDeclaration(PrimaryUnitPackageDeclarationSyntax);
impl CheckedNode for CheckedPrimaryUnitPackageDeclaration {
    type Syntax = PrimaryUnitPackageDeclarationSyntax;
    fn cast_unchecked(syntax: PrimaryUnitPackageDeclarationSyntax) -> Self {
        CheckedPrimaryUnitPackageDeclaration(syntax)
    }
    fn raw(&self) -> PrimaryUnitPackageDeclarationSyntax {
        self.0.clone()
    }
}
impl CheckedPrimaryUnitPackageDeclaration {
    pub fn package(&self) -> CheckedPackage {
        CheckedPackage::cast_unchecked(self.0.package().unwrap())
    }
}
#[derive(Debug, Clone)]
pub struct CheckedPackageInstantiationDeclarationPrimaryUnit(
    PackageInstantiationDeclarationPrimaryUnitSyntax,
);
impl CheckedNode for CheckedPackageInstantiationDeclarationPrimaryUnit {
    type Syntax = PackageInstantiationDeclarationPrimaryUnitSyntax;
    fn cast_unchecked(syntax: PackageInstantiationDeclarationPrimaryUnitSyntax) -> Self {
        CheckedPackageInstantiationDeclarationPrimaryUnit(syntax)
    }
    fn raw(&self) -> PackageInstantiationDeclarationPrimaryUnitSyntax {
        self.0.clone()
    }
}
impl CheckedPackageInstantiationDeclarationPrimaryUnit {
    pub fn package_instantiation(&self) -> CheckedPackageInstantiation {
        CheckedPackageInstantiation::cast_unchecked(self.0.package_instantiation().unwrap())
    }
}
#[derive(Debug, Clone)]
pub enum CheckedPrimaryUnit {
    EntityDeclaration(CheckedEntityDeclaration),
    ConfigurationDeclaration(CheckedConfigurationDeclaration),
    PrimaryUnitPackageDeclaration(CheckedPrimaryUnitPackageDeclaration),
    PackageInstantiationDeclarationPrimaryUnit(CheckedPackageInstantiationDeclarationPrimaryUnit),
    ContextDeclaration(CheckedContextDeclaration),
    PslVerificationUnit(CheckedPslVerificationUnit),
}
impl CheckedNode for CheckedPrimaryUnit {
    type Syntax = PrimaryUnitSyntax;
    fn cast_unchecked(syntax: PrimaryUnitSyntax) -> Self {
        match syntax {
            PrimaryUnitSyntax::EntityDeclaration(inner) => CheckedPrimaryUnit::EntityDeclaration(
                CheckedEntityDeclaration::cast_unchecked(inner),
            ),
            PrimaryUnitSyntax::ConfigurationDeclaration(inner) => {
                CheckedPrimaryUnit::ConfigurationDeclaration(
                    CheckedConfigurationDeclaration::cast_unchecked(inner),
                )
            }
            PrimaryUnitSyntax::PrimaryUnitPackageDeclaration(inner) => {
                CheckedPrimaryUnit::PrimaryUnitPackageDeclaration(
                    CheckedPrimaryUnitPackageDeclaration::cast_unchecked(inner),
                )
            }
            PrimaryUnitSyntax::PackageInstantiationDeclarationPrimaryUnit(inner) => {
                CheckedPrimaryUnit::PackageInstantiationDeclarationPrimaryUnit(
                    CheckedPackageInstantiationDeclarationPrimaryUnit::cast_unchecked(inner),
                )
            }
            PrimaryUnitSyntax::ContextDeclaration(inner) => CheckedPrimaryUnit::ContextDeclaration(
                CheckedContextDeclaration::cast_unchecked(inner),
            ),
            PrimaryUnitSyntax::PslVerificationUnit(inner) => {
                CheckedPrimaryUnit::PslVerificationUnit(CheckedPslVerificationUnit::cast_unchecked(
                    inner,
                ))
            }
        }
    }
    fn raw(&self) -> Self::Syntax {
        match self {
            CheckedPrimaryUnit::EntityDeclaration(inner) => {
                PrimaryUnitSyntax::EntityDeclaration(inner.raw())
            }
            CheckedPrimaryUnit::ConfigurationDeclaration(inner) => {
                PrimaryUnitSyntax::ConfigurationDeclaration(inner.raw())
            }
            CheckedPrimaryUnit::PrimaryUnitPackageDeclaration(inner) => {
                PrimaryUnitSyntax::PrimaryUnitPackageDeclaration(inner.raw())
            }
            CheckedPrimaryUnit::PackageInstantiationDeclarationPrimaryUnit(inner) => {
                PrimaryUnitSyntax::PackageInstantiationDeclarationPrimaryUnit(inner.raw())
            }
            CheckedPrimaryUnit::ContextDeclaration(inner) => {
                PrimaryUnitSyntax::ContextDeclaration(inner.raw())
            }
            CheckedPrimaryUnit::PslVerificationUnit(inner) => {
                PrimaryUnitSyntax::PslVerificationUnit(inner.raw())
            }
        }
    }
}
#[derive(Debug, Clone)]
pub struct CheckedSecondaryUnitPackageBody(SecondaryUnitPackageBodySyntax);
impl CheckedNode for CheckedSecondaryUnitPackageBody {
    type Syntax = SecondaryUnitPackageBodySyntax;
    fn cast_unchecked(syntax: SecondaryUnitPackageBodySyntax) -> Self {
        CheckedSecondaryUnitPackageBody(syntax)
    }
    fn raw(&self) -> SecondaryUnitPackageBodySyntax {
        self.0.clone()
    }
}
impl CheckedSecondaryUnitPackageBody {
    pub fn package_body(&self) -> CheckedPackageBody {
        CheckedPackageBody::cast_unchecked(self.0.package_body().unwrap())
    }
}
#[derive(Debug, Clone)]
pub enum CheckedSecondaryUnit {
    ArchitectureBody(CheckedArchitectureBody),
    SecondaryUnitPackageBody(CheckedSecondaryUnitPackageBody),
}
impl CheckedNode for CheckedSecondaryUnit {
    type Syntax = SecondaryUnitSyntax;
    fn cast_unchecked(syntax: SecondaryUnitSyntax) -> Self {
        match syntax {
            SecondaryUnitSyntax::ArchitectureBody(inner) => CheckedSecondaryUnit::ArchitectureBody(
                CheckedArchitectureBody::cast_unchecked(inner),
            ),
            SecondaryUnitSyntax::SecondaryUnitPackageBody(inner) => {
                CheckedSecondaryUnit::SecondaryUnitPackageBody(
                    CheckedSecondaryUnitPackageBody::cast_unchecked(inner),
                )
            }
        }
    }
    fn raw(&self) -> Self::Syntax {
        match self {
            CheckedSecondaryUnit::ArchitectureBody(inner) => {
                SecondaryUnitSyntax::ArchitectureBody(inner.raw())
            }
            CheckedSecondaryUnit::SecondaryUnitPackageBody(inner) => {
                SecondaryUnitSyntax::SecondaryUnitPackageBody(inner.raw())
            }
        }
    }
}
