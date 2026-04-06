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
pub struct CheckedAbsolutePathname(AbsolutePathnameSyntax);
impl CheckedNode for CheckedAbsolutePathname {
    type Syntax = AbsolutePathnameSyntax;
    fn cast_unchecked(syntax: AbsolutePathnameSyntax) -> Self {
        CheckedAbsolutePathname(syntax)
    }
    fn raw(&self) -> AbsolutePathnameSyntax {
        self.0.clone()
    }
}
impl CheckedAbsolutePathname {
    pub fn dot_token(&self) -> SyntaxToken {
        self.0.dot_token().unwrap()
    }
    pub fn partial_pathname(&self) -> Option<CheckedPartialPathname> {
        self.0
            .partial_pathname()
            .map(CheckedPartialPathname::cast_unchecked)
    }
}
#[derive(Debug, Clone)]
pub struct CheckedAttributeName(AttributeNameSyntax);
impl CheckedNode for CheckedAttributeName {
    type Syntax = AttributeNameSyntax;
    fn cast_unchecked(syntax: AttributeNameSyntax) -> Self {
        CheckedAttributeName(syntax)
    }
    fn raw(&self) -> AttributeNameSyntax {
        self.0.clone()
    }
}
impl CheckedAttributeName {
    pub fn left_square_token(&self) -> SyntaxToken {
        self.0.left_square_token().unwrap()
    }
    pub fn signature(&self) -> CheckedSignature {
        CheckedSignature::cast_unchecked(self.0.signature().unwrap())
    }
    pub fn right_square_token(&self) -> SyntaxToken {
        self.0.right_square_token().unwrap()
    }
    pub fn tick_token(&self) -> SyntaxToken {
        self.0.tick_token().unwrap()
    }
    pub fn attribute_designator_token_token(&self) -> SyntaxToken {
        self.0.attribute_designator_token_token().unwrap()
    }
    pub fn left_par_token(&self) -> SyntaxToken {
        self.0.left_par_token().unwrap()
    }
    pub fn expression(&self) -> CheckedExpression {
        CheckedExpression::cast_unchecked(self.0.expression().unwrap())
    }
    pub fn right_par_token(&self) -> SyntaxToken {
        self.0.right_par_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub enum CheckedExternalName {
    ExternalConstantName(CheckedExternalConstantName),
    ExternalSignalName(CheckedExternalSignalName),
    ExternalVariableName(CheckedExternalVariableName),
}
impl CheckedNode for CheckedExternalName {
    type Syntax = ExternalNameSyntax;
    fn cast_unchecked(syntax: ExternalNameSyntax) -> Self {
        match syntax {
            ExternalNameSyntax::ExternalConstantName(inner) => {
                CheckedExternalName::ExternalConstantName(
                    CheckedExternalConstantName::cast_unchecked(inner),
                )
            }
            ExternalNameSyntax::ExternalSignalName(inner) => {
                CheckedExternalName::ExternalSignalName(CheckedExternalSignalName::cast_unchecked(
                    inner,
                ))
            }
            ExternalNameSyntax::ExternalVariableName(inner) => {
                CheckedExternalName::ExternalVariableName(
                    CheckedExternalVariableName::cast_unchecked(inner),
                )
            }
        }
    }
    fn raw(&self) -> Self::Syntax {
        match self {
            CheckedExternalName::ExternalConstantName(inner) => {
                ExternalNameSyntax::ExternalConstantName(inner.raw())
            }
            CheckedExternalName::ExternalSignalName(inner) => {
                ExternalNameSyntax::ExternalSignalName(inner.raw())
            }
            CheckedExternalName::ExternalVariableName(inner) => {
                ExternalNameSyntax::ExternalVariableName(inner.raw())
            }
        }
    }
}
#[derive(Debug, Clone)]
pub struct CheckedExternalConstantName(ExternalConstantNameSyntax);
impl CheckedNode for CheckedExternalConstantName {
    type Syntax = ExternalConstantNameSyntax;
    fn cast_unchecked(syntax: ExternalConstantNameSyntax) -> Self {
        CheckedExternalConstantName(syntax)
    }
    fn raw(&self) -> ExternalConstantNameSyntax {
        self.0.clone()
    }
}
impl CheckedExternalConstantName {
    pub fn lt_lt_token(&self) -> SyntaxToken {
        self.0.lt_lt_token().unwrap()
    }
    pub fn constant_token(&self) -> SyntaxToken {
        self.0.constant_token().unwrap()
    }
    pub fn external_path_name(&self) -> CheckedExternalPathName {
        CheckedExternalPathName::cast_unchecked(self.0.external_path_name().unwrap())
    }
    pub fn colon_token(&self) -> SyntaxToken {
        self.0.colon_token().unwrap()
    }
    pub fn subtype_indication(&self) -> CheckedSubtypeIndication {
        CheckedSubtypeIndication::cast_unchecked(self.0.subtype_indication().unwrap())
    }
    pub fn gt_gt_token(&self) -> SyntaxToken {
        self.0.gt_gt_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedExternalSignalName(ExternalSignalNameSyntax);
impl CheckedNode for CheckedExternalSignalName {
    type Syntax = ExternalSignalNameSyntax;
    fn cast_unchecked(syntax: ExternalSignalNameSyntax) -> Self {
        CheckedExternalSignalName(syntax)
    }
    fn raw(&self) -> ExternalSignalNameSyntax {
        self.0.clone()
    }
}
impl CheckedExternalSignalName {
    pub fn lt_lt_token(&self) -> SyntaxToken {
        self.0.lt_lt_token().unwrap()
    }
    pub fn signal_token(&self) -> SyntaxToken {
        self.0.signal_token().unwrap()
    }
    pub fn external_path_name(&self) -> CheckedExternalPathName {
        CheckedExternalPathName::cast_unchecked(self.0.external_path_name().unwrap())
    }
    pub fn colon_token(&self) -> SyntaxToken {
        self.0.colon_token().unwrap()
    }
    pub fn subtype_indication(&self) -> CheckedSubtypeIndication {
        CheckedSubtypeIndication::cast_unchecked(self.0.subtype_indication().unwrap())
    }
    pub fn gt_gt_token(&self) -> SyntaxToken {
        self.0.gt_gt_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedExternalVariableName(ExternalVariableNameSyntax);
impl CheckedNode for CheckedExternalVariableName {
    type Syntax = ExternalVariableNameSyntax;
    fn cast_unchecked(syntax: ExternalVariableNameSyntax) -> Self {
        CheckedExternalVariableName(syntax)
    }
    fn raw(&self) -> ExternalVariableNameSyntax {
        self.0.clone()
    }
}
impl CheckedExternalVariableName {
    pub fn lt_lt_token(&self) -> SyntaxToken {
        self.0.lt_lt_token().unwrap()
    }
    pub fn variable_token(&self) -> SyntaxToken {
        self.0.variable_token().unwrap()
    }
    pub fn external_path_name(&self) -> CheckedExternalPathName {
        CheckedExternalPathName::cast_unchecked(self.0.external_path_name().unwrap())
    }
    pub fn colon_token(&self) -> SyntaxToken {
        self.0.colon_token().unwrap()
    }
    pub fn subtype_indication(&self) -> CheckedSubtypeIndication {
        CheckedSubtypeIndication::cast_unchecked(self.0.subtype_indication().unwrap())
    }
    pub fn gt_gt_token(&self) -> SyntaxToken {
        self.0.gt_gt_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub enum CheckedExternalPathName {
    PackagePathname(CheckedPackagePathname),
    AbsolutePathname(CheckedAbsolutePathname),
    RelativePathname(CheckedRelativePathname),
}
impl CheckedNode for CheckedExternalPathName {
    type Syntax = ExternalPathNameSyntax;
    fn cast_unchecked(syntax: ExternalPathNameSyntax) -> Self {
        match syntax {
            ExternalPathNameSyntax::PackagePathname(inner) => {
                CheckedExternalPathName::PackagePathname(CheckedPackagePathname::cast_unchecked(
                    inner,
                ))
            }
            ExternalPathNameSyntax::AbsolutePathname(inner) => {
                CheckedExternalPathName::AbsolutePathname(CheckedAbsolutePathname::cast_unchecked(
                    inner,
                ))
            }
            ExternalPathNameSyntax::RelativePathname(inner) => {
                CheckedExternalPathName::RelativePathname(CheckedRelativePathname::cast_unchecked(
                    inner,
                ))
            }
        }
    }
    fn raw(&self) -> Self::Syntax {
        match self {
            CheckedExternalPathName::PackagePathname(inner) => {
                ExternalPathNameSyntax::PackagePathname(inner.raw())
            }
            CheckedExternalPathName::AbsolutePathname(inner) => {
                ExternalPathNameSyntax::AbsolutePathname(inner.raw())
            }
            CheckedExternalPathName::RelativePathname(inner) => {
                ExternalPathNameSyntax::RelativePathname(inner.raw())
            }
        }
    }
}
#[derive(Debug, Clone)]
pub struct CheckedPackagePathname(PackagePathnameSyntax);
impl CheckedNode for CheckedPackagePathname {
    type Syntax = PackagePathnameSyntax;
    fn cast_unchecked(syntax: PackagePathnameSyntax) -> Self {
        CheckedPackagePathname(syntax)
    }
    fn raw(&self) -> PackagePathnameSyntax {
        self.0.clone()
    }
}
impl CheckedPackagePathname {
    pub fn comm_at_token(&self) -> SyntaxToken {
        self.0.comm_at_token().unwrap()
    }
    pub fn dot_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0.dot_token()
    }
    pub fn simple_name_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0.simple_name_token()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedRelativePathname(RelativePathnameSyntax);
impl CheckedNode for CheckedRelativePathname {
    type Syntax = RelativePathnameSyntax;
    fn cast_unchecked(syntax: RelativePathnameSyntax) -> Self {
        CheckedRelativePathname(syntax)
    }
    fn raw(&self) -> RelativePathnameSyntax {
        self.0.clone()
    }
}
impl CheckedRelativePathname {
    pub fn circ_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0.circ_token()
    }
    pub fn dot_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0.dot_token()
    }
    pub fn partial_pathname(&self) -> Option<CheckedPartialPathname> {
        self.0
            .partial_pathname()
            .map(CheckedPartialPathname::cast_unchecked)
    }
}
#[derive(Debug, Clone)]
pub struct CheckedPartialPathname(PartialPathnameSyntax);
impl CheckedNode for CheckedPartialPathname {
    type Syntax = PartialPathnameSyntax;
    fn cast_unchecked(syntax: PartialPathnameSyntax) -> Self {
        CheckedPartialPathname(syntax)
    }
    fn raw(&self) -> PartialPathnameSyntax {
        self.0.clone()
    }
}
impl CheckedPartialPathname {
    pub fn identifier_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0.identifier_token()
    }
    pub fn dot_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0.dot_token()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedNameList(NameListSyntax);
impl CheckedNode for CheckedNameList {
    type Syntax = NameListSyntax;
    fn cast_unchecked(syntax: NameListSyntax) -> Self {
        CheckedNameList(syntax)
    }
    fn raw(&self) -> NameListSyntax {
        self.0.clone()
    }
}
impl CheckedNameList {
    pub fn names(&self) -> impl Iterator<Item = CheckedName> + use<'_> {
        self.0.names().map(CheckedName::cast_unchecked)
    }
    pub fn comma_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0.comma_token()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedLabel(LabelSyntax);
impl CheckedNode for CheckedLabel {
    type Syntax = LabelSyntax;
    fn cast_unchecked(syntax: LabelSyntax) -> Self {
        CheckedLabel(syntax)
    }
    fn raw(&self) -> LabelSyntax {
        self.0.clone()
    }
}
impl CheckedLabel {
    pub fn identifier_token(&self) -> SyntaxToken {
        self.0.identifier_token().unwrap()
    }
    pub fn colon_token(&self) -> SyntaxToken {
        self.0.colon_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedName(NameSyntax);
impl CheckedNode for CheckedName {
    type Syntax = NameSyntax;
    fn cast_unchecked(syntax: NameSyntax) -> Self {
        CheckedName(syntax)
    }
    fn raw(&self) -> NameSyntax {
        self.0.clone()
    }
}
impl CheckedName {
    pub fn name_prefix(&self) -> CheckedNamePrefix {
        CheckedNamePrefix::cast_unchecked(self.0.name_prefix().unwrap())
    }
    pub fn name_tails(&self) -> impl Iterator<Item = CheckedNameTail> + use<'_> {
        self.0.name_tails().map(CheckedNameTail::cast_unchecked)
    }
}
#[derive(Debug, Clone)]
pub struct CheckedNameDesignatorPrefix(NameDesignatorPrefixSyntax);
impl CheckedNode for CheckedNameDesignatorPrefix {
    type Syntax = NameDesignatorPrefixSyntax;
    fn cast_unchecked(syntax: NameDesignatorPrefixSyntax) -> Self {
        CheckedNameDesignatorPrefix(syntax)
    }
    fn raw(&self) -> NameDesignatorPrefixSyntax {
        self.0.clone()
    }
}
impl CheckedNameDesignatorPrefix {
    pub fn name_designator(&self) -> NameDesignatorSyntax {
        self.0.name_designator().unwrap()
    }
}
#[derive(Debug, Clone)]
pub enum CheckedNamePrefix {
    ExternalName(CheckedExternalName),
    NameDesignatorPrefix(CheckedNameDesignatorPrefix),
}
impl CheckedNode for CheckedNamePrefix {
    type Syntax = NamePrefixSyntax;
    fn cast_unchecked(syntax: NamePrefixSyntax) -> Self {
        match syntax {
            NamePrefixSyntax::ExternalName(inner) => {
                CheckedNamePrefix::ExternalName(CheckedExternalName::cast_unchecked(inner))
            }
            NamePrefixSyntax::NameDesignatorPrefix(inner) => {
                CheckedNamePrefix::NameDesignatorPrefix(
                    CheckedNameDesignatorPrefix::cast_unchecked(inner),
                )
            }
        }
    }
    fn raw(&self) -> Self::Syntax {
        match self {
            CheckedNamePrefix::ExternalName(inner) => NamePrefixSyntax::ExternalName(inner.raw()),
            CheckedNamePrefix::NameDesignatorPrefix(inner) => {
                NamePrefixSyntax::NameDesignatorPrefix(inner.raw())
            }
        }
    }
}
#[derive(Debug, Clone)]
pub struct CheckedSelectedName(SelectedNameSyntax);
impl CheckedNode for CheckedSelectedName {
    type Syntax = SelectedNameSyntax;
    fn cast_unchecked(syntax: SelectedNameSyntax) -> Self {
        CheckedSelectedName(syntax)
    }
    fn raw(&self) -> SelectedNameSyntax {
        self.0.clone()
    }
}
impl CheckedSelectedName {
    pub fn dot_token(&self) -> SyntaxToken {
        self.0.dot_token().unwrap()
    }
    pub fn suffix(&self) -> SuffixSyntax {
        self.0.suffix().unwrap()
    }
}
#[derive(Debug, Clone)]
pub enum CheckedNameTail {
    SelectedName(CheckedSelectedName),
    RawTokens(CheckedRawTokens),
    AttributeName(CheckedAttributeName),
}
impl CheckedNode for CheckedNameTail {
    type Syntax = NameTailSyntax;
    fn cast_unchecked(syntax: NameTailSyntax) -> Self {
        match syntax {
            NameTailSyntax::SelectedName(inner) => {
                CheckedNameTail::SelectedName(CheckedSelectedName::cast_unchecked(inner))
            }
            NameTailSyntax::RawTokens(inner) => {
                CheckedNameTail::RawTokens(CheckedRawTokens::cast_unchecked(inner))
            }
            NameTailSyntax::AttributeName(inner) => {
                CheckedNameTail::AttributeName(CheckedAttributeName::cast_unchecked(inner))
            }
        }
    }
    fn raw(&self) -> Self::Syntax {
        match self {
            CheckedNameTail::SelectedName(inner) => NameTailSyntax::SelectedName(inner.raw()),
            CheckedNameTail::RawTokens(inner) => NameTailSyntax::RawTokens(inner.raw()),
            CheckedNameTail::AttributeName(inner) => NameTailSyntax::AttributeName(inner.raw()),
        }
    }
}
#[derive(Debug, Clone)]
pub struct CheckedRawTokens(RawTokensSyntax);
impl CheckedNode for CheckedRawTokens {
    type Syntax = RawTokensSyntax;
    fn cast_unchecked(syntax: RawTokensSyntax) -> Self {
        CheckedRawTokens(syntax)
    }
    fn raw(&self) -> RawTokensSyntax {
        self.0.clone()
    }
}
