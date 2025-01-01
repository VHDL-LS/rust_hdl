// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com

#[derive(PartialEq, Eq, Copy, Clone, Debug)]
pub enum NodeKind {
    EntityDeclaration,
    ConfigurationDeclaration,
    PackageDeclaration,
    PackageInstantiationDeclaration,
    ContextDeclaration,
    ArchitectureBody,
    PackageBody,
    EntityHeader,
    DesignUnit,
    DesignFile,
    ContextClause,
    // ...
}
