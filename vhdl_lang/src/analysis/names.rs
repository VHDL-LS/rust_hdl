// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2022, Olof Kraigher olof.kraigher@gmail.com

use super::analyze::*;
use super::region::*;
use crate::ast::*;
use crate::data::*;
use std::sync::Arc;

// Represent a resolved name which can be either a
// 1. Design such as a library or design unit
// 2. Object such a direct reference to an object or some kind of index, slice or selected name
#[derive(Debug)]
pub enum ResolvedName {
    Library(Symbol),
    Design(DesignEnt),
    Type(TypeEnt),
    Overloaded(OverloadedName),
    ObjectSelection {
        base_object: ObjectEnt,
        type_mark: TypeEnt,
    },
    ExternalName {
        class: ExternalObjectClass,
        type_mark: TypeEnt,
    },
}

impl ResolvedName {
    fn new(ent: Arc<AnyEnt>) -> Option<Self> {
        let name = match ent.kind() {
            AnyEntKind::Object(object) => {
                let type_mark = object.subtype.type_mark().to_owned();
                Self::ObjectSelection {
                    base_object: ObjectEnt::new(ent),
                    type_mark,
                }
            }
            AnyEntKind::ObjectAlias {
                base_object,
                type_mark,
            } => Self::ObjectSelection {
                base_object: base_object.clone(),
                type_mark: type_mark.to_owned(),
            },
            AnyEntKind::Type(_) => ResolvedName::Type(TypeEnt::from_any(ent).unwrap()),
            AnyEntKind::Design(_) => ResolvedName::Design(DesignEnt::from_any(ent).unwrap()),
            AnyEntKind::Library => {
                ResolvedName::Library(ent.designator().as_identifier().cloned().unwrap())
            }
            _ => {
                return None;
            }
        };

        Some(name)
    }

    fn with_suffix(self, ent: Arc<AnyEnt>) -> Result<Option<Self>, String> {
        match ent.kind() {
            AnyEntKind::Object(..) => {
                debug_assert!(matches!(self, Self::Design(_)));
                Ok(Self::new(ent))
            }
            AnyEntKind::ObjectAlias { .. } => {
                debug_assert!(matches!(self, Self::Design(_)));
                Ok(Self::new(ent))
            }
            _ => {
                match self {
                    Self::Design(_) => Ok(Self::new(ent)),
                    Self::Type(_) => {
                        Err("Type may not be the prefix of a selected name".to_owned())
                    }
                    Self::ObjectSelection { base_object, .. } => match ent.actual_kind() {
                        AnyEntKind::ElementDeclaration(subtype) => {
                            Ok(Some(Self::ObjectSelection {
                                base_object,
                                type_mark: subtype.type_mark().to_owned(),
                            }))
                        }
                        // @TODO protected type method
                        _ => Ok(None),
                    },
                    Self::ExternalName { class, .. } => match ent.actual_kind() {
                        AnyEntKind::ElementDeclaration(subtype) => Ok(Some(Self::ExternalName {
                            class,
                            type_mark: subtype.type_mark().to_owned(),
                        })),
                        // @TODO this is probably an error
                        _ => Ok(None),
                    },
                    Self::Overloaded(..) => Err("Overloaded suffix of overloaded name".to_owned()),
                    Self::Library(_) => {
                        Err("Library may not be suffix of selected name".to_owned())
                    }
                }
            }
        }
    }
}

impl<'a> AnalyzeContext<'a> {
    // Helper function:
    // Resolve a name that must be some kind of object selection, index or slice
    // Such names occur as assignment targets and aliases
    // Takes an error message as an argument to be re-usable
    pub fn resolve_object_prefix(
        &self,
        scope: &Scope<'_>,
        name_pos: &SrcPos,
        name: &mut Name,
        err_msg: &'static str,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> AnalysisResult<Option<ResolvedName>> {
        match name {
            Name::Selected(prefix, suffix) => {
                suffix.clear_reference();

                let resolved = self.resolve_object_prefix(
                    scope,
                    &prefix.pos,
                    &mut prefix.item,
                    err_msg,
                    diagnostics,
                )?;

                let resolved = if let Some(resolved) = resolved {
                    resolved
                } else {
                    return Ok(None);
                };

                match resolved {
                    ResolvedName::Library(ref library_name) => Ok(Some(ResolvedName::Design(
                        self.lookup_in_library(library_name, &suffix.pos, suffix.designator())?,
                    ))),
                    ResolvedName::Design(ref ent) => {
                        match self.lookup_selected(&prefix.pos, ent, suffix)? {
                            NamedEntities::Single(named_entity) => {
                                suffix.set_unique_reference(&named_entity);
                                resolved
                                    .with_suffix(named_entity)
                                    .map_err(|e| Diagnostic::error(name_pos, e).into())
                            }
                            NamedEntities::Overloaded(overloaded) => {
                                // Could be used for an alias of a subprogram
                                Ok(Some(ResolvedName::Overloaded(overloaded)))
                            }
                        }
                    }
                    ResolvedName::Type(..) => Err(Diagnostic::error(name_pos, err_msg).into()),
                    ResolvedName::ObjectSelection { ref type_mark, .. } => {
                        match type_mark.selected(&prefix.pos, suffix)? {
                            NamedEntities::Single(named_entity) => {
                                suffix.set_unique_reference(&named_entity);
                                resolved
                                    .with_suffix(named_entity)
                                    .map_err(|e| Diagnostic::error(name_pos, e).into())
                            }
                            NamedEntities::Overloaded(..) => {
                                // Probably a protected type method, this can never be aliased or a target
                                Err(Diagnostic::error(name_pos, err_msg).into())
                            }
                        }
                    }
                    ResolvedName::ExternalName { ref type_mark, .. } => {
                        match type_mark.selected(&prefix.pos, suffix)? {
                            NamedEntities::Single(named_entity) => {
                                suffix.set_unique_reference(&named_entity);
                                resolved
                                    .with_suffix(named_entity)
                                    .map_err(|e| Diagnostic::error(name_pos, e).into())
                            }
                            NamedEntities::Overloaded(..) => {
                                // Probably a protected type method, this can never be aliased or a target
                                // Likely a user error
                                Err(Diagnostic::error(name_pos, err_msg).into())
                            }
                        }
                    }
                    ResolvedName::Overloaded(..) => {
                        // Overloaded suffix of overloaded name is not possible
                        Err(Diagnostic::error(name_pos, err_msg).into())
                    }
                }
            }
            Name::SelectedAll(prefix) => self.resolve_object_prefix(
                scope,
                &prefix.pos,
                &mut prefix.item,
                err_msg,
                diagnostics,
            ),
            Name::Designator(designator) => {
                designator.clear_reference();

                match scope.lookup(name_pos, designator.designator())? {
                    NamedEntities::Single(named_entity) => {
                        designator.set_unique_reference(&named_entity);
                        Ok(ResolvedName::new(named_entity))
                    }
                    NamedEntities::Overloaded(overloaded) => {
                        // Could be used for an alias of a subprogram
                        Ok(Some(ResolvedName::Overloaded(overloaded)))
                    }
                }
            }
            Name::Indexed(ref mut prefix, ref mut indexes) => {
                let resolved = self.resolve_object_prefix(
                    scope,
                    &prefix.pos,
                    &mut prefix.item,
                    err_msg,
                    diagnostics,
                );

                if let Ok(Some(ResolvedName::ObjectSelection {
                    base_object,
                    type_mark,
                })) = resolved
                {
                    let elem_type = self.analyze_indexed_name(
                        scope,
                        name_pos,
                        prefix.suffix_pos(),
                        &type_mark,
                        indexes,
                        diagnostics,
                    )?;

                    Ok(Some(ResolvedName::ObjectSelection {
                        base_object,
                        type_mark: elem_type,
                    }))
                } else {
                    for expr in indexes.iter_mut() {
                        self.analyze_expression(scope, expr, diagnostics)?;
                    }
                    Err(Diagnostic::error(&prefix.pos, err_msg).into())
                }
            }

            Name::Slice(ref mut prefix, ref mut drange) => {
                let res = self.resolve_object_prefix(
                    scope,
                    &prefix.pos,
                    &mut prefix.item,
                    err_msg,
                    diagnostics,
                );

                if let Ok(Some(ResolvedName::ObjectSelection { ref type_mark, .. })) = res {
                    self.analyze_sliced_name(prefix.suffix_pos(), type_mark, diagnostics)?;
                }

                self.analyze_discrete_range(scope, drange.as_mut(), diagnostics)?;
                res
            }
            Name::Attribute(..) => Err(Diagnostic::error(name_pos, err_msg).into()),

            Name::FunctionCall(ref mut fcall) => {
                if let Some((prefix, indexes)) = fcall.to_indexed() {
                    *name = Name::Indexed(prefix, indexes);
                    self.resolve_object_prefix(scope, name_pos, name, err_msg, diagnostics)
                } else {
                    Err(Diagnostic::error(name_pos, err_msg).into())
                }
            }
            Name::External(ref mut ename) => {
                let ExternalName { subtype, class, .. } = ename.as_mut();
                let subtype = self.resolve_subtype_indication(scope, subtype, diagnostics)?;
                Ok(Some(ResolvedName::ExternalName {
                    class: *class,
                    type_mark: subtype.type_mark().to_owned(),
                }))
            }
        }
    }
}
