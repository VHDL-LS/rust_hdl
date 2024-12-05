//! This Source Code Form is subject to the terms of the Mozilla Public
//! License, v. 2.0. If a copy of the MPL was not distributed with this file,
//! You can obtain one at http://mozilla.org/MPL/2.0/.
//!
//! Copyright (c) 2023, Olof Kraigher olof.kraigher@gmail.com

use fnv::FnvHashMap;
use vhdl_lang::SrcPos;

use super::analyze::*;
use super::names::ResolvedName;
use super::scope::*;
use crate::ast::AssociationElement;
use crate::ast::Expression;
use crate::ast::Literal;
use crate::ast::Name;
use crate::ast::Operator;
use crate::ast::PackageInstantiation;
use crate::ast::{ActualPart, MapAspect};
use crate::data::error_codes::ErrorCode;
use crate::data::DiagnosticHandler;
use crate::named_entity::*;
use crate::Diagnostic;
use crate::NullDiagnostics;

impl<'a> AnalyzeContext<'a, '_> {
    pub fn generic_package_instance(
        &self,
        scope: &Scope<'a>,
        package_ent: EntRef<'a>,
        unit: &mut PackageInstantiation,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> EvalResult<Region<'a>> {
        let PackageInstantiation {
            package_name,
            generic_map,
            ..
        } = unit;

        let package_region =
            self.analyze_package_instance_name(scope, package_name, diagnostics)?;
        self.generic_instance(
            package_ent,
            scope,
            unit.ident.tree.pos(self.ctx),
            &package_region,
            generic_map,
            diagnostics,
        )
        .map(|(region, _)| region)
    }

    pub fn generic_map(
        &self,
        scope: &Scope<'a>,
        generics: GpkgRegion<'a>,
        generic_map: &mut [AssociationElement],
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> EvalResult<FnvHashMap<EntityId, TypeEnt<'a>>> {
        let mut mapping = FnvHashMap::default();

        // @TODO check missing associations
        for (idx, assoc) in generic_map.iter_mut().enumerate() {
            let formal = if let Some(formal) = &mut assoc.formal {
                let formal_pos = formal.pos(self.ctx);
                if let Name::Designator(des) = &mut formal.item {
                    match generics.lookup(&formal_pos, &des.item) {
                        Ok((_, ent)) => {
                            des.set_unique_reference(&ent);
                            ent
                        }
                        Err(err) => {
                            diagnostics.push(err);
                            continue;
                        }
                    }
                } else {
                    diagnostics.add(
                        formal.pos(self.ctx),
                        "Expected simple name for package generic formal",
                        ErrorCode::MismatchedKinds,
                    );
                    continue;
                }
            } else if let Some(ent) = generics.nth(idx) {
                ent
            } else {
                diagnostics.add(
                    assoc.actual.pos(self.ctx),
                    "Extra actual for generic map",
                    ErrorCode::TooManyArguments,
                );
                continue;
            };

            match &mut assoc.actual.item {
                ActualPart::Expression(expr) => match formal {
                    GpkgInterfaceEnt::Type(uninst_typ) => {
                        let typ = if let Expression::Name(name) = expr {
                            match name.as_mut() {
                                // Could be an array constraint such as integer_vector(0 to 3)
                                // @TODO we ignore the suffix for now
                                Name::Slice(prefix, drange) => {
                                    let typ = self.type_name(
                                        scope,
                                        prefix.span,
                                        &mut prefix.item,
                                        diagnostics,
                                    )?;
                                    if let Type::Array { indexes, .. } = typ.base().kind() {
                                        if let Some(Some(idx_typ)) = indexes.first() {
                                            self.drange_with_ttyp(
                                                scope,
                                                (*idx_typ).into(),
                                                drange,
                                                diagnostics,
                                            )?;
                                        }
                                    } else {
                                        diagnostics.add(
                                            assoc.actual.pos(self.ctx),
                                            format!(
                                                "Array constraint cannot be used for {}",
                                                typ.describe()
                                            ),
                                            ErrorCode::TypeMismatch,
                                        );
                                    }
                                    typ
                                }
                                // Could be a record constraint such as rec_t(field(0 to 3))
                                // @TODO we ignore the suffix for now
                                Name::CallOrIndexed(call) if call.could_be_indexed_name() => self
                                    .type_name(
                                    scope,
                                    call.name.span,
                                    &mut call.name.item,
                                    diagnostics,
                                )?,
                                _ => self.type_name(scope, assoc.actual.span, name, diagnostics)?,
                            }
                        } else {
                            diagnostics.add(
                                assoc.actual.pos(self.ctx),
                                "Cannot map expression to type generic",
                                ErrorCode::MismatchedKinds,
                            );
                            continue;
                        };

                        mapping.insert(uninst_typ.id(), typ);
                    }
                    GpkgInterfaceEnt::Constant(obj) => self.expr_pos_with_ttyp(
                        scope,
                        self.map_type_ent(&mapping, obj.type_mark(), scope),
                        assoc.actual.span,
                        expr,
                        diagnostics,
                    )?,
                    GpkgInterfaceEnt::Subprogram(target) => match expr {
                        Expression::Name(name) => {
                            let resolved =
                                self.name_resolve(scope, assoc.actual.span, name, diagnostics)?;
                            if let ResolvedName::Overloaded(des, overloaded) = resolved {
                                let signature = target.subprogram_key().map(|base_type| {
                                    mapping
                                        .get(&base_type.id())
                                        .map(|ent| ent.base())
                                        .unwrap_or(base_type)
                                });
                                if let Some(ent) = overloaded.get(&signature) {
                                    name.set_unique_reference(&ent);
                                } else {
                                    let mut diag = Diagnostic::mismatched_kinds(
                                        assoc.actual.pos(self.ctx),
                                        format!(
                                            "Cannot map '{des}' to subprogram generic {}{}",
                                            target.designator(),
                                            signature.key().describe()
                                        ),
                                    );

                                    diag.add_subprogram_candidates(
                                        "Does not match",
                                        overloaded.entities(),
                                    );

                                    diagnostics.push(diag)
                                }
                            } else {
                                diagnostics.add(
                                    assoc.actual.pos(self.ctx),
                                    format!(
                                        "Cannot map {} to subprogram generic",
                                        resolved.describe()
                                    ),
                                    ErrorCode::MismatchedKinds,
                                )
                            }
                        }
                        Expression::Literal(Literal::String(string)) => {
                            if Operator::from_latin1(string.clone()).is_none() {
                                diagnostics.add(
                                    assoc.actual.pos(self.ctx),
                                    "Invalid operator symbol",
                                    ErrorCode::InvalidOperatorSymbol,
                                );
                            }
                        }
                        _ => diagnostics.add(
                            assoc.actual.pos(self.ctx),
                            "Cannot map expression to subprogram generic",
                            ErrorCode::MismatchedKinds,
                        ),
                    },
                    GpkgInterfaceEnt::Package(_) => match expr {
                        Expression::Name(name) => {
                            self.name_resolve(scope, assoc.actual.span, name, diagnostics)?;
                        }
                        _ => diagnostics.add(
                            assoc.actual.pos(self.ctx),
                            "Cannot map expression to package generic",
                            ErrorCode::MismatchedKinds,
                        ),
                    },
                },
                ActualPart::Open => {
                    // @TODO
                }
            }
        }
        Ok(mapping)
    }

    pub fn generic_instance(
        &self,
        ent: EntRef<'a>,
        scope: &Scope<'a>,
        decl_pos: &SrcPos,
        uninst_region: &Region<'a>,
        generic_map: &mut Option<MapAspect>,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> EvalResult<(Region<'a>, FnvHashMap<EntityId, TypeEnt<'a>>)> {
        let nested = scope.nested().in_package_declaration();
        let (generics, other) = uninst_region.to_package_generic();

        let mapping = if let Some(generic_map) = generic_map {
            self.generic_map(
                &nested,
                generics,
                generic_map.list.items.as_mut_slice(),
                diagnostics,
            )?
        } else {
            FnvHashMap::default()
        };

        for uninst in other {
            match self.instantiate(Some(ent), &mapping, uninst, &nested) {
                Ok(inst) => {
                    // We ignore diagnostics here, for example when adding implicit operators EQ and NE for interface types
                    // They can collide if there are more than one interface type that map to the same actual type
                    nested.add(inst, &mut NullDiagnostics);
                }
                Err((err, code)) => {
                    let mut diag = Diagnostic::new(decl_pos, err, code);
                    if let Some(pos) = uninst.decl_pos() {
                        diag.add_related(pos, "When instantiating this declaration");
                    }
                    diagnostics.push(diag);
                }
            }
        }

        Ok((nested.into_region(), mapping))
    }

    pub(crate) fn instantiate(
        &self,
        parent: Option<EntRef<'a>>,
        mapping: &FnvHashMap<EntityId, TypeEnt<'a>>,
        uninst: EntRef<'a>,
        scope: &Scope<'a>,
    ) -> Result<EntRef<'a>, (String, ErrorCode)> {
        let designator = uninst.designator().clone();

        let decl_pos = uninst.decl_pos().cloned();

        let inst = self.arena.alloc(
            designator,
            parent.or(uninst.parent),
            Related::InstanceOf(uninst),
            AnyEntKind::Library, // Will be immediately overwritten below
            decl_pos,
            uninst.src_span,
            Some(self.source()),
        );
        let kind = self.map_kind(Some(inst), mapping, uninst.kind(), scope)?;
        unsafe {
            inst.set_kind(kind);
        }

        for implicit_uninst in uninst.implicits.iter() {
            unsafe {
                self.arena.add_implicit(
                    inst.id(),
                    self.instantiate(Some(inst), mapping, implicit_uninst, scope)?,
                );
            }
        }

        Ok(inst)
    }

    fn map_kind(
        &self,
        parent: Option<EntRef<'a>>,
        mapping: &FnvHashMap<EntityId, TypeEnt<'a>>,
        kind: &'a AnyEntKind<'a>,
        scope: &Scope<'a>,
    ) -> Result<AnyEntKind<'a>, (String, ErrorCode)> {
        Ok(match kind {
            AnyEntKind::ExternalAlias { class, type_mark } => AnyEntKind::ExternalAlias {
                class: *class,
                type_mark: self.map_type_ent(mapping, *type_mark, scope),
            },
            AnyEntKind::ObjectAlias {
                base_object,
                type_mark,
            } => AnyEntKind::ObjectAlias {
                base_object: if let Some(obj) =
                    ObjectEnt::from_any(self.instantiate(None, mapping, base_object, scope)?)
                {
                    obj
                } else {
                    return Err((
                        "Internal error, expected instantiated object to be object".to_owned(),
                        ErrorCode::Internal,
                    ));
                },
                type_mark: self.map_type_ent(mapping, *type_mark, scope),
            },
            AnyEntKind::File(subtype) => {
                AnyEntKind::File(self.map_subtype(mapping, *subtype, scope))
            }
            AnyEntKind::InterfaceFile(typ) => {
                AnyEntKind::InterfaceFile(self.map_type_ent(mapping, *typ, scope))
            }
            AnyEntKind::Component(region) => {
                AnyEntKind::Component(self.map_region(parent, mapping, region, scope)?)
            }
            AnyEntKind::Attribute(typ) => {
                AnyEntKind::Attribute(self.map_type_ent(mapping, *typ, scope))
            }
            AnyEntKind::Overloaded(overloaded) => {
                AnyEntKind::Overloaded(self.map_overloaded(parent, mapping, overloaded, scope)?)
            }
            AnyEntKind::Type(typ) => AnyEntKind::Type(self.map_type(parent, mapping, typ, scope)?),
            AnyEntKind::ElementDeclaration(subtype) => {
                AnyEntKind::ElementDeclaration(self.map_subtype(mapping, *subtype, scope))
            }
            AnyEntKind::Sequential(s) => AnyEntKind::Sequential(*s),
            AnyEntKind::Concurrent(c) => AnyEntKind::Concurrent(*c),
            AnyEntKind::Object(obj) => AnyEntKind::Object(self.map_object(mapping, obj, scope)),
            AnyEntKind::LoopParameter(typ) => AnyEntKind::LoopParameter(
                typ.map(|typ| self.map_type_ent(mapping, typ.into(), scope).base()),
            ),
            AnyEntKind::PhysicalLiteral(typ) => {
                AnyEntKind::PhysicalLiteral(self.map_type_ent(mapping, *typ, scope))
            }
            AnyEntKind::DeferredConstant(subtype) => {
                AnyEntKind::DeferredConstant(self.map_subtype(mapping, *subtype, scope))
            }
            AnyEntKind::Library => AnyEntKind::Library,
            AnyEntKind::Design(design) => match design {
                Design::PackageInstance(region) => AnyEntKind::Design(Design::PackageInstance(
                    self.map_region(parent, mapping, region, scope)?,
                )),
                Design::InterfacePackageInstance(region) => {
                    AnyEntKind::Design(Design::InterfacePackageInstance(
                        self.map_region(parent, mapping, region, scope)?,
                    ))
                }
                _ => {
                    return Err((
                        format!(
                            "Internal error, did not expect to instantiate {}",
                            design.describe()
                        ),
                        ErrorCode::Internal,
                    ));
                }
            },
            AnyEntKind::View(typ) => AnyEntKind::View(self.map_subtype(mapping, *typ, scope)),
        })
    }

    fn map_overloaded(
        &self,
        parent: Option<EntRef<'a>>,
        mapping: &FnvHashMap<EntityId, TypeEnt<'a>>,
        overloaded: &'a Overloaded<'a>,
        scope: &Scope<'a>,
    ) -> Result<Overloaded<'a>, (String, ErrorCode)> {
        Ok(match overloaded {
            Overloaded::SubprogramDecl(signature) => {
                Overloaded::SubprogramDecl(self.map_signature(parent, mapping, signature, scope)?)
            }
            Overloaded::Subprogram(signature) => {
                Overloaded::Subprogram(self.map_signature(parent, mapping, signature, scope)?)
            }
            Overloaded::UninstSubprogramDecl(signature, generic_map) => {
                Overloaded::UninstSubprogramDecl(
                    self.map_signature(parent, mapping, signature, scope)?,
                    generic_map.clone(),
                )
            }
            Overloaded::UninstSubprogram(signature, generic_map) => Overloaded::UninstSubprogram(
                self.map_signature(parent, mapping, signature, scope)?,
                generic_map.clone(),
            ),
            Overloaded::InterfaceSubprogram(signature) => Overloaded::InterfaceSubprogram(
                self.map_signature(parent, mapping, signature, scope)?,
            ),
            Overloaded::EnumLiteral(signature) => {
                Overloaded::EnumLiteral(self.map_signature(parent, mapping, signature, scope)?)
            }
            Overloaded::Alias(alias) => {
                let alias_inst = self.instantiate(parent, mapping, alias, scope)?;

                if let Some(overloaded) = OverloadedEnt::from_any(alias_inst) {
                    Overloaded::Alias(overloaded)
                } else {
                    return Err((
                        "Internal error, expected overloaded when instantiating overloaded entity"
                            .to_owned(),
                        ErrorCode::Internal,
                    ));
                }
            }
        })
    }

    pub(crate) fn map_signature(
        &self,
        parent: Option<EntRef<'a>>,
        mapping: &FnvHashMap<EntityId, TypeEnt<'a>>,
        signature: &'a Signature<'a>,
        scope: &Scope<'a>,
    ) -> Result<Signature<'a>, (String, ErrorCode)> {
        let Signature {
            formals,
            return_type,
        } = signature;

        let FormalRegion {
            typ,
            entities: uninst_entities,
        } = formals;

        let mut inst_entities = Vec::with_capacity(uninst_entities.len());
        for uninst in uninst_entities {
            let inst = self.instantiate(parent, mapping, uninst, scope)?;

            if let Some(inst) = InterfaceEnt::from_any(inst) {
                inst_entities.push(inst);
            } else {
                return Err((
                    "Internal error, expected interface to be instantiated as interface".to_owned(),
                    ErrorCode::Internal,
                ));
            }
        }

        Ok(Signature {
            formals: FormalRegion {
                typ: *typ,
                entities: inst_entities,
            },
            return_type: return_type.map(|typ| self.map_type_ent(mapping, typ, scope)),
        })
    }

    fn map_region(
        &self,
        parent: Option<EntRef<'a>>,
        mapping: &FnvHashMap<EntityId, TypeEnt<'a>>,
        region: &'a Region<'a>,
        scope: &Scope<'a>,
    ) -> Result<Region<'a>, (String, ErrorCode)> {
        let Region {
            entities: uninst_entities,
            kind,
            ..
        } = region;

        let mut inst_region = Region {
            kind: *kind,
            ..Region::default()
        };

        for uninst in uninst_entities.values() {
            match uninst {
                NamedEntities::Single(uninst) => {
                    let inst = self.instantiate(parent, mapping, uninst, scope)?;
                    inst_region.add(inst, &mut NullDiagnostics);
                }
                NamedEntities::Overloaded(overloaded) => {
                    for uninst in overloaded.entities() {
                        let inst = self.instantiate(parent, mapping, uninst.into(), scope)?;
                        inst_region.add(inst, &mut NullDiagnostics);
                    }
                }
            }
        }

        Ok(inst_region)
    }

    fn map_type(
        &self,
        parent: Option<EntRef<'a>>,
        mapping: &FnvHashMap<EntityId, TypeEnt<'a>>,
        typ: &'a Type<'a>,
        scope: &Scope<'a>,
    ) -> Result<Type<'a>, (String, ErrorCode)> {
        Ok(match typ {
            Type::Array { indexes, elem_type } => {
                let mut mapped_indexes = Vec::with_capacity(indexes.len());
                for index_typ in indexes.iter() {
                    mapped_indexes.push(index_typ.map(|index_typ| {
                        self.map_type_ent(mapping, index_typ.into(), scope).base()
                    }))
                }

                Type::Array {
                    indexes: mapped_indexes,
                    elem_type: self.map_type_ent(mapping, *elem_type, scope),
                }
            }
            Type::Enum(symbols) => Type::Enum(symbols.clone()),
            Type::Integer => Type::Integer,
            Type::Real => Type::Real,
            Type::Physical => Type::Physical,
            Type::Access(subtype) => Type::Access(self.map_subtype(mapping, *subtype, scope)),
            Type::Record(record_region) => {
                let mut elems = Vec::with_capacity(record_region.elems.len());
                for uninst in record_region.elems.iter() {
                    let inst = self.instantiate(parent, mapping, uninst, scope)?;

                    if let Some(inst) = RecordElement::from_any(inst) {
                        elems.push(inst);
                    } else {
                        return Err(("Internal error, expected instantiated record element to be record element".to_owned(), ErrorCode::Internal));
                    }
                }
                Type::Record(RecordRegion { elems })
            }
            Type::Subtype(subtype) => Type::Subtype(self.map_subtype(mapping, *subtype, scope)),
            Type::Protected(region, is_body) => {
                Type::Protected(self.map_region(parent, mapping, region, scope)?, *is_body)
            }
            Type::File => Type::File,
            Type::Alias(typ) => Type::Alias(self.map_type_ent(mapping, *typ, scope)),
            Type::Universal(utyp) => Type::Universal(*utyp),
            Type::Incomplete => Type::Incomplete,
            Type::Interface => Type::Interface,
        })
    }

    fn map_object(
        &self,
        mapping: &FnvHashMap<EntityId, TypeEnt<'a>>,
        obj: &Object<'a>,
        scope: &Scope<'a>,
    ) -> Object<'a> {
        let Object {
            class,
            iface,
            subtype,
            has_default,
        } = obj;

        Object {
            class: *class,
            iface: iface.clone(),
            subtype: self.map_subtype(mapping, *subtype, scope),
            has_default: *has_default,
        }
    }

    fn map_type_ent(
        &self,
        mapping: &FnvHashMap<EntityId, TypeEnt<'a>>,
        typ: TypeEnt<'a>,
        scope: &Scope<'a>,
    ) -> TypeEnt<'a> {
        match mapping.get(&typ.id()) {
            None => {
                if let Some(entity) = scope
                    .lookup(&typ.designator)
                    .ok()
                    .and_then(|result| result.into_non_overloaded().ok())
                    .and_then(TypeEnt::from_any)
                {
                    entity
                } else {
                    typ
                }
            }
            Some(typ) => *typ,
        }
    }

    fn map_subtype(
        &self,
        mapping: &FnvHashMap<EntityId, TypeEnt<'a>>,
        subtype: Subtype<'a>,
        scope: &Scope<'a>,
    ) -> Subtype<'a> {
        let Subtype { type_mark } = subtype;

        Subtype {
            type_mark: self.map_type_ent(mapping, type_mark, scope),
        }
    }
}
