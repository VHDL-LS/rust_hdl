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

macro_rules! try_unknown {
    ($expr:expr) => {
        if let Some(value) = $expr? {
            value
        } else {
            // Unknown
            return Ok(None);
        }
    };
}

#[derive(Debug)]
pub enum ObjectBase {
    Object(ObjectEnt),
    DeferredConstant,
    ExternalName(ExternalObjectClass),
}

impl ObjectBase {
    pub fn mode(&self) -> Option<Mode> {
        match self {
            ObjectBase::Object(object) => object.mode(),
            ObjectBase::DeferredConstant => None,
            ObjectBase::ExternalName(_) => None,
        }
    }

    pub fn class(&self) -> ObjectClass {
        match self {
            ObjectBase::Object(object) => object.class(),
            ObjectBase::DeferredConstant => ObjectClass::Constant,
            ObjectBase::ExternalName(class) => (*class).into(),
        }
    }

    pub fn describe_class(&self) -> String {
        if let Some(mode) = self.mode() {
            if self.class() == ObjectClass::Constant {
                format!("interface {}", self.class())
            } else {
                format!("interface {} of mode {}", self.class(), mode)
            }
        } else {
            format!("{}", self.class())
        }
    }
}

#[derive(Debug)]
pub enum ResolvedName {
    Library(Symbol),
    Design(DesignEnt),
    Type(TypeEnt),
    Overloaded(OverloadedName),
    ObjectSelection {
        base: ObjectBase,
        type_mark: TypeEnt,
    },
    // Something that cannot be further selected
    Final(Arc<AnyEnt>),
}

impl ResolvedName {
    /// The name was selected out of a design unit
    fn from_design(ent: Arc<AnyEnt>) -> Result<Self, String> {
        let name = match ent.kind() {
            AnyEntKind::Object(object) => {
                let type_mark = object.subtype.type_mark().to_owned();
                ResolvedName::ObjectSelection {
                    base: ObjectBase::Object(ObjectEnt::new(ent)),
                    type_mark,
                }
            }
            AnyEntKind::ObjectAlias {
                base_object,
                type_mark,
            } => ResolvedName::ObjectSelection {
                base: ObjectBase::Object(base_object.clone()),
                type_mark: type_mark.to_owned(),
            },
            AnyEntKind::ExternalAlias { class, type_mark } => ResolvedName::ObjectSelection {
                base: ObjectBase::ExternalName(*class),
                type_mark: type_mark.clone(),
            },
            AnyEntKind::DeferredConstant(subtype) => ResolvedName::ObjectSelection {
                base: ObjectBase::DeferredConstant,
                type_mark: subtype.type_mark().clone(),
            },
            AnyEntKind::Type(_) => ResolvedName::Type(TypeEnt::from_any(ent).unwrap()),
            AnyEntKind::Overloaded(_) => ResolvedName::Overloaded(OverloadedName::single(
                OverloadedEnt::from_any(ent).unwrap(),
            )),
            AnyEntKind::File(_)
            | AnyEntKind::InterfaceFile(_)
            | AnyEntKind::Component(_)
            | AnyEntKind::PhysicalLiteral(_) => ResolvedName::Final(ent.clone()),
            AnyEntKind::Design(_)
            | AnyEntKind::Library
            | AnyEntKind::Attribute
            | AnyEntKind::ElementDeclaration(_)
            | AnyEntKind::Label
            | AnyEntKind::LoopParameter => {
                return Err(format!(
                    "{} cannot be selected from design unit",
                    ent.kind().describe()
                ))
            }
        };

        Ok(name)
    }

    /// The name was looked up from the current scope
    fn from_scope(ent: Arc<AnyEnt>) -> Result<Self, String> {
        let name = match ent.kind() {
            AnyEntKind::Object(object) => {
                let type_mark = object.subtype.type_mark().to_owned();
                ResolvedName::ObjectSelection {
                    base: ObjectBase::Object(ObjectEnt::new(ent)),
                    type_mark,
                }
            }
            AnyEntKind::ObjectAlias {
                base_object,
                type_mark,
            } => ResolvedName::ObjectSelection {
                base: ObjectBase::Object(base_object.clone()),
                type_mark: type_mark.to_owned(),
            },
            AnyEntKind::ExternalAlias { class, type_mark } => ResolvedName::ObjectSelection {
                base: ObjectBase::ExternalName(*class),
                type_mark: type_mark.clone(),
            },
            AnyEntKind::DeferredConstant(subtype) => ResolvedName::ObjectSelection {
                base: ObjectBase::DeferredConstant,
                type_mark: subtype.type_mark().clone(),
            },
            AnyEntKind::Type(_) => ResolvedName::Type(TypeEnt::from_any(ent).unwrap()),
            AnyEntKind::Design(_) => ResolvedName::Design(DesignEnt::from_any(ent).unwrap()),
            AnyEntKind::Library => {
                ResolvedName::Library(ent.designator().as_identifier().cloned().unwrap())
            }
            AnyEntKind::Overloaded(_) => ResolvedName::Overloaded(OverloadedName::single(
                OverloadedEnt::from_any(ent).unwrap(),
            )),
            AnyEntKind::File(_)
            | AnyEntKind::InterfaceFile(_)
            | AnyEntKind::Component(_)
            | AnyEntKind::Label
            | AnyEntKind::LoopParameter
            | AnyEntKind::PhysicalLiteral(_) => ResolvedName::Final(ent.clone()),
            AnyEntKind::Attribute | AnyEntKind::ElementDeclaration(_) => {
                return Err(format!(
                    "{} should never be looked up from the current scope",
                    ent.kind().describe()
                ))
            }
        };

        Ok(name)
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

                let resolved = try_unknown!(self.resolve_object_prefix(
                    scope,
                    &prefix.pos,
                    &mut prefix.item,
                    err_msg,
                    diagnostics,
                ));

                match resolved {
                    ResolvedName::Library(ref library_name) => {
                        Ok(Some(ResolvedName::Design(self.lookup_in_library(
                            library_name,
                            &suffix.pos,
                            &suffix.item.item,
                            &mut suffix.item.reference,
                        )?)))
                    }
                    ResolvedName::Design(ref ent) => {
                        let name = ent.selected(&prefix.pos, suffix)?;
                        suffix.set_reference(&name);
                        match name {
                            NamedEntities::Single(named_entity) => Ok(Some(
                                ResolvedName::from_design(named_entity)
                                    .map_err(|e| Diagnostic::error(&suffix.pos, e))?,
                            )),
                            NamedEntities::Overloaded(overloaded) => {
                                // Could be used for an alias of a subprogram
                                Ok(Some(ResolvedName::Overloaded(overloaded)))
                            }
                        }
                    }
                    ResolvedName::Type(..) => Err(Diagnostic::error(name_pos, err_msg).into()),
                    ResolvedName::ObjectSelection { base, type_mark } => {
                        match type_mark.selected(&prefix.pos, suffix)? {
                            TypedSelection::RecordElement(elem) => {
                                suffix.set_unique_reference(elem.as_ref());
                                Ok(Some(ResolvedName::ObjectSelection {
                                    base,
                                    type_mark: elem.type_mark().to_owned(),
                                }))
                            }
                            TypedSelection::ProtectedMethod(overloaded) => {
                                suffix.set_reference(&overloaded);
                                // Probably a protected type method, this can never be aliased or a target
                                Err(Diagnostic::error(name_pos, err_msg).into())
                            }
                        }
                    }
                    ResolvedName::Overloaded(..) => {
                        // Overloaded suffix of overloaded name is not possible
                        Err(Diagnostic::error(name_pos, err_msg).into())
                    }
                    ResolvedName::Final(..) => Err(Diagnostic::error(name_pos, err_msg).into()),
                }
            }
            Name::SelectedAll(prefix) => {
                let resolved = try_unknown!(self.resolve_object_prefix(
                    scope,
                    &prefix.pos,
                    &mut prefix.item,
                    err_msg,
                    diagnostics,
                ));

                if let ResolvedName::ObjectSelection { base, type_mark } = resolved {
                    if let Some(type_mark) = type_mark.accessed_type() {
                        return Ok(Some(ResolvedName::ObjectSelection {
                            base,
                            type_mark: type_mark.clone(),
                        }));
                    }
                }

                Err(Diagnostic::error(&prefix.pos, "Cannot be the prefix of .all").into())
            }
            Name::Designator(designator) => {
                designator.clear_reference();
                let name = scope.lookup(name_pos, designator.designator())?;
                designator.set_reference(&name);

                match name {
                    NamedEntities::Single(named_entity) => Ok(Some(
                        ResolvedName::from_scope(named_entity)
                            .map_err(|e| Diagnostic::error(name_pos, e))?,
                    )),
                    NamedEntities::Overloaded(overloaded) => {
                        // Could be used for an alias of a subprogram
                        Ok(Some(ResolvedName::Overloaded(overloaded)))
                    }
                }
            }
            Name::Indexed(ref mut prefix, ref mut indexes) => {
                let resolved = try_unknown!(self.resolve_object_prefix(
                    scope,
                    &prefix.pos,
                    &mut prefix.item,
                    err_msg,
                    diagnostics,
                ));

                if let ResolvedName::ObjectSelection { base, type_mark } = resolved {
                    let elem_type = self.analyze_indexed_name(
                        scope,
                        name_pos,
                        prefix.suffix_pos(),
                        &type_mark,
                        indexes,
                        diagnostics,
                    )?;

                    Ok(Some(ResolvedName::ObjectSelection {
                        base,
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
                let resolved = try_unknown!(self.resolve_object_prefix(
                    scope,
                    &prefix.pos,
                    &mut prefix.item,
                    err_msg,
                    diagnostics,
                ));

                if let ResolvedName::ObjectSelection { ref type_mark, .. } = resolved {
                    self.analyze_sliced_name(prefix.suffix_pos(), type_mark, diagnostics)?;
                }

                self.analyze_discrete_range(scope, drange.as_mut(), diagnostics)?;
                Ok(Some(resolved))
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
                Ok(Some(ResolvedName::ObjectSelection {
                    base: ObjectBase::ExternalName(*class),
                    type_mark: subtype.type_mark().to_owned(),
                }))
            }
        }
    }
}
