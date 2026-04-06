// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2026, Lukas Scheller lukasscheller@icloud.com
use crate::syntax::checked::CheckedNode;
use crate::syntax::*;
#[derive(Debug, Clone)]
pub struct CheckedPslDirective(PslDirectiveSyntax);
impl CheckedNode for CheckedPslDirective {
    type Syntax = PslDirectiveSyntax;
    fn cast_unchecked(syntax: PslDirectiveSyntax) -> Self {
        CheckedPslDirective(syntax)
    }
    fn raw(&self) -> PslDirectiveSyntax {
        self.0.clone()
    }
}
impl CheckedPslDirective {}
#[derive(Debug, Clone)]
pub struct CheckedPslPropertyDeclaration(PslPropertyDeclarationSyntax);
impl CheckedNode for CheckedPslPropertyDeclaration {
    type Syntax = PslPropertyDeclarationSyntax;
    fn cast_unchecked(syntax: PslPropertyDeclarationSyntax) -> Self {
        CheckedPslPropertyDeclaration(syntax)
    }
    fn raw(&self) -> PslPropertyDeclarationSyntax {
        self.0.clone()
    }
}
impl CheckedPslPropertyDeclaration {}
#[derive(Debug, Clone)]
pub struct CheckedPslSequenceDeclaration(PslSequenceDeclarationSyntax);
impl CheckedNode for CheckedPslSequenceDeclaration {
    type Syntax = PslSequenceDeclarationSyntax;
    fn cast_unchecked(syntax: PslSequenceDeclarationSyntax) -> Self {
        CheckedPslSequenceDeclaration(syntax)
    }
    fn raw(&self) -> PslSequenceDeclarationSyntax {
        self.0.clone()
    }
}
impl CheckedPslSequenceDeclaration {}
#[derive(Debug, Clone)]
pub struct CheckedPslClockDeclaration(PslClockDeclarationSyntax);
impl CheckedNode for CheckedPslClockDeclaration {
    type Syntax = PslClockDeclarationSyntax;
    fn cast_unchecked(syntax: PslClockDeclarationSyntax) -> Self {
        CheckedPslClockDeclaration(syntax)
    }
    fn raw(&self) -> PslClockDeclarationSyntax {
        self.0.clone()
    }
}
impl CheckedPslClockDeclaration {}
#[derive(Debug, Clone)]
pub struct CheckedPslVerificationUnit(PslVerificationUnitSyntax);
impl CheckedNode for CheckedPslVerificationUnit {
    type Syntax = PslVerificationUnitSyntax;
    fn cast_unchecked(syntax: PslVerificationUnitSyntax) -> Self {
        CheckedPslVerificationUnit(syntax)
    }
    fn raw(&self) -> PslVerificationUnitSyntax {
        self.0.clone()
    }
}
impl CheckedPslVerificationUnit {}
