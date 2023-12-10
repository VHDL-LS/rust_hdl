// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2019, Olof Kraigher olof.kraigher@gmail.com

use super::names::*;
use super::*;
use crate::ast::*;
use crate::data::*;
use crate::named_entity::{Signature, *};
use crate::{ast, named_entity, HasTokenSpan};
use analyze::*;
use fnv::FnvHashMap;
use std::collections::hash_map::Entry;

impl Declaration {
    pub fn is_allowed_in_context(&self, parent: &AnyEntKind) -> bool {
        use Declaration::*;
        use ObjectClass::*;
        match parent {
            AnyEntKind::Design(Design::Architecture(..))
            | AnyEntKind::Concurrent(Some(Concurrent::Block | Concurrent::Generate)) => matches!(
                self,
                Object(ObjectDeclaration {
                    class: Constant | Signal | SharedVariable,
                    ..
                }) | File(_)
                    | Type(_)
                    | Component(_)
                    | Attribute(_)
                    | Alias(_)
                    | SubprogramDeclaration(_)
                    | SubprogramInstantiation(_)
                    | SubprogramBody(_)
                    | Use(_)
                    | Package(_)
                    | Configuration(_)
            ),
            AnyEntKind::Design(Design::Configuration) => {
                matches!(self, Use(_) | Attribute(ast::Attribute::Specification(_)))
            }
            AnyEntKind::Design(Design::Entity(..)) => matches!(
                self,
                Object(_)
                    | File(_)
                    | Type(_)
                    | Attribute(_)
                    | Alias(_)
                    | SubprogramDeclaration(_)
                    | SubprogramInstantiation(_)
                    | SubprogramBody(_)
                    | Use(_)
                    | Package(_)
            ),
            AnyEntKind::Design(Design::PackageBody | Design::UninstPackage(..))
            | AnyEntKind::Overloaded(
                Overloaded::SubprogramDecl(_)
                | Overloaded::Subprogram(_)
                | Overloaded::UninstSubprogramDecl(..)
                | Overloaded::UninstSubprogram(..),
            )
            | AnyEntKind::Concurrent(Some(Concurrent::Process))
            | AnyEntKind::Type(named_entity::Type::Protected(..)) => matches!(
                self,
                Object(ObjectDeclaration {
                    class: Constant | Variable | SharedVariable,
                    ..
                }) | File(_)
                    | Type(_)
                    | Attribute(_)
                    | Alias(_)
                    | SubprogramDeclaration(_)
                    | SubprogramInstantiation(_)
                    | SubprogramBody(_)
                    | Use(_)
                    | Package(_)
            ),
            AnyEntKind::Design(Design::Package(..)) => matches!(
                self,
                Object(_)
                    | File(_)
                    | Type(_)
                    | Component(_)
                    | Attribute(_)
                    | Alias(_)
                    | SubprogramDeclaration(_)
                    | SubprogramInstantiation(_)
                    | Use(_)
                    | Package(_)
            ),
            _ => {
                // AnyEntKind::Library is used in tests for a generic declarative region
                if !(cfg!(test) && matches!(parent, AnyEntKind::Library)) {
                    debug_assert!(false, "Parent should be a declarative region");
                }
                true
            }
        }
    }
}

impl<'a> AnalyzeContext<'a> {
    pub fn analyze_declarative_part(
        &self,
        scope: &Scope<'a>,
        parent: EntRef<'a>,
        declarations: &mut [Declaration],
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        let mut incomplete_types: FnvHashMap<Symbol, (EntRef<'a>, SrcPos)> = FnvHashMap::default();

        for i in 0..declarations.len() {
            // Handle incomplete types

            let (decl, remaining) = declarations[i..].split_first_mut().unwrap();

            if !decl.is_allowed_in_context(parent.kind()) {
                diagnostics.error(
                    decl.get_pos(self.ctx),
                    format!("{} declaration not allowed here", decl.describe(),),
                )
            }

            match decl {
                Declaration::Type(type_decl) => match type_decl.def {
                    TypeDefinition::Incomplete(ref mut reference) => {
                        match incomplete_types.entry(type_decl.ident.name().clone()) {
                            Entry::Vacant(entry) => {
                                let full_definiton =
                                    find_full_type_definition(type_decl.ident.name(), remaining);

                                let decl_pos = match full_definiton {
                                    Some(full_decl) => full_decl.ident.pos(),
                                    None => {
                                        let mut error = Diagnostic::error(
                                            type_decl.ident.pos(),
                                            format!(
                                            "Missing full type declaration of incomplete type '{}'",
                                            type_decl.ident.name()
                                        ),
                                        );
                                        error.add_related(type_decl.ident.pos(), "The full type declaration shall occur immediately within the same declarative part");
                                        diagnostics.push(error);
                                        type_decl.ident.pos()
                                    }
                                };

                                let designator =
                                    Designator::Identifier(type_decl.ident.name().clone());

                                // Set incomplete type defintion to position of full declaration
                                let ent = self.arena.explicit(
                                    designator,
                                    parent,
                                    AnyEntKind::Type(Type::Incomplete),
                                    Some(decl_pos),
                                );
                                reference.set_unique_reference(ent);

                                entry.insert((ent, type_decl.ident.pos().clone()));
                                scope.add(ent, diagnostics);
                            }
                            Entry::Occupied(entry) => {
                                let (_, decl_pos) = entry.get();

                                diagnostics.push(Diagnostic::duplicate_error(
                                    &type_decl.ident,
                                    type_decl.ident.pos(),
                                    Some(decl_pos),
                                ));
                            }
                        }
                    }
                    _ => {
                        let incomplete_type = incomplete_types.get(type_decl.ident.name());
                        if let Some((incomplete_type, _)) = incomplete_type {
                            self.analyze_type_declaration(
                                scope,
                                parent,
                                type_decl,
                                Some(incomplete_type.id()),
                                diagnostics,
                            )?;
                        } else {
                            self.analyze_type_declaration(
                                scope,
                                parent,
                                type_decl,
                                None,
                                diagnostics,
                            )?;
                        }
                    }
                },
                _ => {
                    self.analyze_declaration(scope, parent, &mut declarations[i], diagnostics)?;
                }
            }
        }
        Ok(())
    }

    fn analyze_alias_declaration(
        &self,
        scope: &Scope<'a>,
        parent: EntRef<'a>,
        alias: &mut AliasDeclaration,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> EvalResult<EntRef<'a>> {
        let AliasDeclaration {
            designator,
            name,
            subtype_indication,
            signature,
            span: _,
        } = alias;

        let resolved_name = self.name_resolve(scope, &name.pos, &mut name.item, diagnostics);

        if let Some(ref mut subtype_indication) = subtype_indication {
            // Object alias
            self.analyze_subtype_indication(scope, subtype_indication, diagnostics)?;
        }

        let resolved_name = resolved_name?;

        let kind = {
            match resolved_name {
                ResolvedName::ObjectName(oname) => {
                    if let Some(ref signature) = signature {
                        diagnostics.push(Diagnostic::should_not_have_signature("Alias", signature));
                    }
                    match oname.base {
                        ObjectBase::Object(base_object) => AnyEntKind::ObjectAlias {
                            base_object,
                            type_mark: oname.type_mark(),
                        },
                        ObjectBase::ObjectAlias(base_object, _) => AnyEntKind::ObjectAlias {
                            base_object,
                            type_mark: oname.type_mark(),
                        },
                        ObjectBase::ExternalName(class) => AnyEntKind::ExternalAlias {
                            class,
                            type_mark: oname.type_mark(),
                        },
                        ObjectBase::DeferredConstant(_) => {
                            // @TODO handle
                            return Err(EvalError::Unknown);
                        }
                    }
                }
                ResolvedName::Library(_)
                | ResolvedName::Design(_)
                | ResolvedName::Expression(_) => {
                    if let Some(ref signature) = signature {
                        diagnostics.push(Diagnostic::should_not_have_signature("Alias", signature));
                    }
                    diagnostics.error(
                        &name.pos,
                        format!("{} cannot be aliased", resolved_name.describe_type()),
                    );
                    return Err(EvalError::Unknown);
                }
                ResolvedName::Type(typ) => {
                    if let Some(ref signature) = signature {
                        diagnostics.push(Diagnostic::should_not_have_signature("Alias", signature));
                    }
                    AnyEntKind::Type(Type::Alias(typ))
                }
                ResolvedName::Overloaded(des, overloaded) => {
                    if let Some(ref mut signature) = signature {
                        // TODO: Uninstantiated subprogram in aliases
                        let signature_key =
                            self.resolve_signature(scope, signature, diagnostics)?;
                        if let Some(ent) = overloaded.get(&SubprogramKey::Normal(signature_key)) {
                            if let Some(reference) = name.item.suffix_reference_mut() {
                                reference.set_unique_reference(&ent);
                            }
                            AnyEntKind::Overloaded(Overloaded::Alias(ent))
                        } else {
                            diagnostics.push(Diagnostic::no_overloaded_with_signature(
                                &des.pos,
                                &des.item,
                                &overloaded,
                            ));
                            return Err(EvalError::Unknown);
                        }
                    } else {
                        diagnostics.push(Diagnostic::signature_required(name));
                        return Err(EvalError::Unknown);
                    }
                }
                ResolvedName::Final(_) => {
                    // @TODO some of these can probably be aliased
                    return Err(EvalError::Unknown);
                }
            }
        };

        Ok(designator.define(self.arena, parent, kind))
    }

    pub(crate) fn analyze_declaration(
        &self,
        scope: &Scope<'a>,
        parent: EntRef<'a>,
        decl: &mut Declaration,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        match decl {
            Declaration::Alias(alias) => {
                if let Some(ent) =
                    as_fatal(self.analyze_alias_declaration(scope, parent, alias, diagnostics))?
                {
                    scope.add(ent, diagnostics);

                    for implicit in ent.as_actual().implicits.iter() {
                        match OverloadedEnt::from_any(implicit) {
                            Some(implicit) => {
                                let impicit_alias = self.arena.implicit(
                                    ent,
                                    implicit.designator().clone(),
                                    AnyEntKind::Overloaded(Overloaded::Alias(implicit)),
                                    ent.decl_pos(),
                                );
                                scope.add(impicit_alias, diagnostics);
                            }
                            None => {
                                eprintln!(
                                    "Expect implicit declaration to be overloaded, got: {}",
                                    implicit.describe()
                                )
                            }
                        }
                    }
                }
            }
            Declaration::Object(ref mut object_decl) => {
                let subtype = self.resolve_subtype_indication(
                    scope,
                    &mut object_decl.subtype_indication,
                    diagnostics,
                );

                if let Some(ref mut expr) = object_decl.expression {
                    if let Ok(ref subtype) = subtype {
                        self.expr_pos_with_ttyp(
                            scope,
                            subtype.type_mark(),
                            &expr.pos,
                            &mut expr.item,
                            diagnostics,
                        )?;
                    } else {
                        self.expr_unknown_ttyp(scope, expr, diagnostics)?;
                    }
                }

                if let Some(subtype) = as_fatal(subtype)? {
                    let kind = if object_decl.class == ObjectClass::Constant
                        && object_decl.expression.is_none()
                    {
                        AnyEntKind::DeferredConstant(subtype)
                    } else {
                        AnyEntKind::Object(Object {
                            class: object_decl.class,
                            iface: None,
                            has_default: object_decl.expression.is_some(),
                            subtype,
                        })
                    };

                    let declared_by = if object_decl.class == ObjectClass::Constant
                        && object_decl.expression.is_some()
                    {
                        self.find_deferred_constant_declaration(scope, &object_decl.ident.tree.item)
                    } else {
                        None
                    };

                    let object_ent = self.arena.alloc(
                        object_decl.ident.tree.item.clone().into(),
                        Some(parent),
                        if let Some(declared_by) = declared_by {
                            Related::DeclaredBy(declared_by)
                        } else {
                            Related::None
                        },
                        kind,
                        Some(object_decl.ident.tree.pos().clone()),
                    );
                    object_decl.ident.decl.set(object_ent.id());

                    scope.add(object_ent, diagnostics);
                }
            }
            Declaration::File(ref mut file) => {
                let FileDeclaration {
                    ident,
                    subtype_indication,
                    open_info,
                    file_name,
                    span: _,
                } = file;

                let subtype = as_fatal(self.resolve_subtype_indication(
                    scope,
                    subtype_indication,
                    diagnostics,
                ))?;

                if let Some(ref mut expr) = open_info {
                    self.expr_unknown_ttyp(scope, expr, diagnostics)?;
                }
                if let Some(ref mut expr) = file_name {
                    self.expr_unknown_ttyp(scope, expr, diagnostics)?;
                }

                if let Some(subtype) = subtype {
                    scope.add(
                        self.arena.define(ident, parent, AnyEntKind::File(subtype)),
                        diagnostics,
                    );
                }
            }
            Declaration::Component(ref mut component) => {
                let nested = scope.nested();
                let ent = self.arena.define(
                    &mut component.ident,
                    parent,
                    AnyEntKind::Component(Region::default()),
                );
                self.analyze_interface_list(
                    &nested,
                    ent,
                    &mut component.generic_list,
                    diagnostics,
                )?;
                self.analyze_interface_list(&nested, ent, &mut component.port_list, diagnostics)?;

                let kind = AnyEntKind::Component(nested.into_region());
                unsafe {
                    ent.set_kind(kind);
                }

                scope.add(ent, diagnostics);
            }
            Declaration::Attribute(ref mut attr) => match attr {
                Attribute::Declaration(ref mut attr_decl) => {
                    if let Some(typ) = as_fatal(self.resolve_type_mark(
                        scope,
                        &mut attr_decl.type_mark,
                        diagnostics,
                    ))? {
                        scope.add(
                            self.arena.define(
                                &mut attr_decl.ident,
                                parent,
                                AnyEntKind::Attribute(typ),
                            ),
                            diagnostics,
                        );
                    }
                }
                Attribute::Specification(ref mut attr_spec) => {
                    self.attribute_specification(scope, parent, attr_spec, diagnostics)?;
                }
            },
            Declaration::SubprogramBody(ref mut body) => {
                let (subpgm_region, subpgm_ent) = match as_fatal(self.subprogram_specification(
                    scope,
                    parent,
                    &mut body.specification,
                    Overloaded::Subprogram,
                    diagnostics,
                ))? {
                    Some(r) => r,
                    None => {
                        return Ok(());
                    }
                };

                scope.add(subpgm_ent.into(), diagnostics);

                self.define_labels_for_sequential_part(
                    &subpgm_region,
                    subpgm_ent.into(),
                    &mut body.statements,
                    diagnostics,
                )?;
                self.analyze_declarative_part(
                    &subpgm_region,
                    subpgm_ent.into(),
                    &mut body.declarations,
                    diagnostics,
                )?;

                self.analyze_sequential_part(
                    &subpgm_region,
                    subpgm_ent.into(),
                    &mut body.statements,
                    diagnostics,
                )?;
            }
            Declaration::SubprogramDeclaration(ref mut subdecl) => {
                match as_fatal(self.subprogram_specification(
                    scope,
                    parent,
                    &mut subdecl.specification,
                    Overloaded::SubprogramDecl,
                    diagnostics,
                ))? {
                    Some((_, ent)) => {
                        scope.add(ent.into(), diagnostics);
                    }
                    None => {
                        return Ok(());
                    }
                }
            }
            Declaration::SubprogramInstantiation(ref mut instance) => {
                let subpgm_ent = self.arena.define(
                    &mut instance.ident,
                    parent,
                    AnyEntKind::Overloaded(Overloaded::Subprogram(Signature::new(
                        FormalRegion::new_params(),
                        None,
                    ))),
                );
                let referenced_name = &mut instance.subprogram_name;
                if let Some(name) = as_fatal(self.name_resolve(
                    scope,
                    &referenced_name.pos,
                    &mut referenced_name.item,
                    diagnostics,
                ))? {
                    if let Some(signature) = as_fatal(self.generic_subprogram_instance(
                        scope,
                        &subpgm_ent,
                        &name,
                        instance,
                        diagnostics,
                    ))? {
                        unsafe {
                            subpgm_ent
                                .set_kind(AnyEntKind::Overloaded(Overloaded::Subprogram(signature)))
                        }
                        scope.add(subpgm_ent, diagnostics)
                    }
                }
            }
            Declaration::Use(ref mut use_clause) => {
                self.analyze_use_clause(scope, use_clause, diagnostics)?;
            }

            Declaration::Package(ref mut instance) => {
                let ent = self.arena.define(
                    &mut instance.ident,
                    parent,
                    AnyEntKind::Design(Design::PackageInstance(Region::default())),
                );

                if let Some(pkg_region) =
                    as_fatal(self.generic_package_instance(scope, ent, instance, diagnostics))?
                {
                    let kind = AnyEntKind::Design(Design::PackageInstance(pkg_region));
                    unsafe {
                        ent.set_kind(kind);
                    }
                    scope.add(ent, diagnostics);
                }
            }
            Declaration::Configuration(..) => {}
            Declaration::Type(..) => unreachable!("Handled elsewhere"),
        };

        Ok(())
    }

    fn find_deferred_constant_declaration(
        &self,
        scope: &Scope<'a>,
        ident: &Symbol,
    ) -> Option<EntRef<'a>> {
        if let Some(NamedEntities::Single(ent)) = scope.lookup_immediate(&ident.into()) {
            if ent.kind().is_deferred_constant() {
                return Some(ent);
            }
        }
        None
    }

    fn attribute_specification(
        &self,
        scope: &Scope<'a>,
        parent: EntRef<'a>,
        attr_spec: &mut AttributeSpecification,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        let AttributeSpecification {
            ident,
            entity_name,
            entity_class,
            expr,
            span: _,
        } = attr_spec;

        let attr_ent = match scope.lookup(
            &ident.item.pos,
            &Designator::Identifier(ident.item.name().clone()),
        ) {
            Ok(NamedEntities::Single(ent)) => {
                ident.set_unique_reference(ent);
                if let Some(attr_ent) = AttributeEnt::from_any(ent) {
                    self.expr_pos_with_ttyp(
                        scope,
                        attr_ent.typ(),
                        &expr.pos,
                        &mut expr.item,
                        diagnostics,
                    )?;
                    attr_ent
                } else {
                    diagnostics.error(
                        &ident.item.pos,
                        format!("{} is not an attribute", ent.describe()),
                    );
                    return Ok(());
                }
            }
            Ok(NamedEntities::Overloaded(_)) => {
                diagnostics.error(
                    &ident.item.pos,
                    format!("Overloaded name '{}' is not an attribute", ident.item),
                );
                return Ok(());
            }
            Err(err) => {
                diagnostics.push(err);
                return Ok(());
            }
        };

        if let EntityName::Name(EntityTag {
            designator,
            signature,
        }) = entity_name
        {
            let ent: EntRef = match scope.lookup(&designator.pos, &designator.item.item) {
                Ok(NamedEntities::Single(ent)) => {
                    designator.set_unique_reference(ent);

                    if let Some(signature) = signature {
                        diagnostics.push(Diagnostic::should_not_have_signature(
                            "Attribute specification",
                            &signature.pos,
                        ));
                    }
                    ent
                }
                Ok(NamedEntities::Overloaded(overloaded)) => {
                    if let Some(signature) = signature {
                        match as_fatal(self.resolve_signature(scope, signature, diagnostics))? {
                            Some(signature_key) => {
                                if let Some(ent) =
                                    overloaded.get(&SubprogramKey::Normal(signature_key))
                                {
                                    designator.set_unique_reference(&ent);
                                    ent.into()
                                } else {
                                    diagnostics.push(Diagnostic::no_overloaded_with_signature(
                                        &designator.pos,
                                        &designator.item.item,
                                        &overloaded,
                                    ));
                                    return Ok(());
                                }
                            }
                            None => {
                                return Ok(());
                            }
                        }
                    } else if let Some(ent) = overloaded.as_unique() {
                        designator.set_unique_reference(ent);
                        ent
                    } else {
                        diagnostics.push(Diagnostic::signature_required(designator));
                        return Ok(());
                    }
                }
                Err(err) => {
                    diagnostics.push(err);
                    return Ok(());
                }
            };

            // Attributes affect the underlying entity and cannot be set directly on aliases
            let ent = ent.as_actual();

            match entity_class {
                EntityClass::Architecture
                | EntityClass::Entity
                | EntityClass::Package
                | EntityClass::Configuration => {
                    if ent != parent {
                        diagnostics.push(Diagnostic::error(
                            designator,
                            "Attribute specification must be in the immediate declarative part",
                        ));
                        return Ok(());
                    }
                }
                EntityClass::Signal
                | EntityClass::Variable
                | EntityClass::Procedure
                | EntityClass::Function
                | EntityClass::Component
                | EntityClass::Constant
                | EntityClass::Type
                | EntityClass::Subtype
                | EntityClass::Literal
                | EntityClass::Units
                | EntityClass::File
                | EntityClass::Label => {
                    if ent.parent != Some(parent) {
                        diagnostics.push(Diagnostic::error(
                            designator,
                            "Attribute specification must be in the immediate declarative part",
                        ));
                        return Ok(());
                    }
                }
            }

            if Some(*entity_class) != get_entity_class(ent) {
                diagnostics.push(Diagnostic::error(
                    designator,
                    format!("{} is not of class {}", ent.describe(), entity_class),
                ));
                return Ok(());
            }

            let res = unsafe { self.arena.add_attr(ent.id(), &designator.pos, attr_ent) };

            if let Err(diagnostic) = res {
                diagnostics.push(diagnostic);
            }
        }

        Ok(())
    }

    pub fn analyze_interface_declaration(
        &self,
        scope: &Scope<'a>,
        parent: EntRef<'a>,
        decl: &mut InterfaceDeclaration,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> EvalResult<EntRef<'a>> {
        let ent = match decl {
            InterfaceDeclaration::File(ref mut file_decl) => {
                let file_type = self.resolve_subtype_indication(
                    scope,
                    &mut file_decl.subtype_indication,
                    diagnostics,
                )?;
                self.arena.define(
                    &mut file_decl.ident,
                    parent,
                    AnyEntKind::InterfaceFile(file_type.type_mark().to_owned()),
                )
            }
            InterfaceDeclaration::Object(ref mut object_decl) => {
                let subtype = self.resolve_subtype_indication(
                    scope,
                    &mut object_decl.subtype_indication,
                    diagnostics,
                );

                if let Some(ref mut expression) = object_decl.expression {
                    if let Ok(ref subtype) = subtype {
                        self.expr_pos_with_ttyp(
                            scope,
                            subtype.type_mark(),
                            &expression.pos,
                            &mut expression.item,
                            diagnostics,
                        )?;
                    } else {
                        self.expr_unknown_ttyp(scope, expression, diagnostics)?
                    }
                }

                let subtype = subtype?;
                self.arena.define(
                    &mut object_decl.ident,
                    parent,
                    AnyEntKind::Object(Object {
                        class: object_decl.class,
                        iface: Some(ObjectInterface::new(
                            object_decl.list_type,
                            object_decl.mode,
                        )),
                        subtype,
                        has_default: object_decl.expression.is_some(),
                    }),
                )
            }
            InterfaceDeclaration::Type(ref mut ident) => {
                let typ = TypeEnt::from_any(self.arena.define(
                    ident,
                    parent,
                    AnyEntKind::Type(Type::Interface),
                ))
                .unwrap();

                let implicit = [
                    self.comparison(Operator::EQ, typ),
                    self.comparison(Operator::NE, typ),
                ];

                for ent in implicit {
                    unsafe {
                        self.arena.add_implicit(typ.id(), ent);
                    }

                    scope.add(ent, diagnostics);
                }

                typ.into()
            }
            InterfaceDeclaration::Subprogram(ref mut subpgm, ..) => {
                let (_, ent) = self.subprogram_specification(
                    scope,
                    parent,
                    subpgm,
                    Overloaded::InterfaceSubprogram,
                    diagnostics,
                )?;
                ent.into()
            }
            InterfaceDeclaration::Package(ref mut instance) => {
                let package_region = catch_analysis_err(
                    self.analyze_package_instance_name(scope, &mut instance.package_name),
                    diagnostics,
                )?;

                self.arena.define(
                    &mut instance.ident,
                    parent,
                    AnyEntKind::Design(Design::PackageInstance(package_region.clone())),
                )
            }
        };
        Ok(ent)
    }

    pub fn analyze_interface_list(
        &self,
        scope: &Scope<'a>,
        parent: EntRef<'a>,
        declarations: &mut [InterfaceDeclaration],
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        for decl in declarations.iter_mut() {
            if let Some(ent) =
                as_fatal(self.analyze_interface_declaration(scope, parent, decl, diagnostics))?
            {
                scope.add(ent, diagnostics);
            }
        }
        Ok(())
    }

    pub fn analyze_parameter_list(
        &self,
        scope: &Scope<'a>,
        parent: EntRef<'a>,
        declarations: &mut [InterfaceDeclaration],
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult<FormalRegion<'a>> {
        let mut params = FormalRegion::new(InterfaceType::Parameter);

        for decl in declarations.iter_mut() {
            if let Some(ent) =
                as_fatal(self.analyze_interface_declaration(scope, parent, decl, diagnostics))?
            {
                scope.add(ent, diagnostics);
                params.add(ent);
            }
        }
        Ok(params)
    }

    pub(crate) fn analyze_array_index(
        &self,
        scope: &Scope<'a>,
        array_index: &mut ArrayIndex,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> EvalResult<BaseType<'a>> {
        match array_index {
            ArrayIndex::IndexSubtypeDefintion(ref mut type_mark) => self
                .resolve_type_mark(scope, type_mark, diagnostics)
                .map(|typ| typ.base()),
            ArrayIndex::Discrete(ref mut drange) => self.drange_type(scope, drange, diagnostics),
        }
    }
}

impl Diagnostic {
    fn no_overloaded_with_signature(
        pos: &SrcPos,
        des: &Designator,
        overloaded: &OverloadedName,
    ) -> Diagnostic {
        let mut diagnostic = Diagnostic::error(
            pos,
            format!(
                "Could not find declaration of {} with given signature",
                des.describe()
            ),
        );
        diagnostic.add_subprogram_candidates("Found", overloaded.entities());
        diagnostic
    }

    fn should_not_have_signature(prefix: &str, pos: impl AsRef<SrcPos>) -> Diagnostic {
        Diagnostic::error(
            pos,
            format!("{prefix} should only have a signature for subprograms and enum literals"),
        )
    }

    fn signature_required(pos: impl AsRef<SrcPos>) -> Diagnostic {
        Diagnostic::error(
            pos,
            "Signature required for alias of subprogram and enum literals",
        )
    }
}

fn get_entity_class(ent: EntRef) -> Option<EntityClass> {
    match ent.actual_kind() {
        // Alias is never the direct target of attribute
        AnyEntKind::ExternalAlias { .. } => None,
        // Alias is never the direct target of attribute
        AnyEntKind::ObjectAlias { .. } => None,
        AnyEntKind::File(_) => Some(EntityClass::File),
        AnyEntKind::InterfaceFile(_) => Some(EntityClass::File),
        AnyEntKind::Component(_) => Some(EntityClass::Component),
        AnyEntKind::Attribute(_) => None,
        AnyEntKind::Overloaded(ent) => match ent {
            Overloaded::SubprogramDecl(s)
            | Overloaded::Subprogram(s)
            | Overloaded::UninstSubprogramDecl(s, _)
            | Overloaded::UninstSubprogram(s, _)
            | Overloaded::InterfaceSubprogram(s) => {
                if s.return_type.is_some() {
                    Some(EntityClass::Function)
                } else {
                    Some(EntityClass::Procedure)
                }
            }
            Overloaded::EnumLiteral(_) => Some(EntityClass::Literal),
            // Alias is never the direct target of attribute
            Overloaded::Alias(_) => None,
        },
        AnyEntKind::Type(Type::Subtype(_)) => Some(EntityClass::Subtype),
        AnyEntKind::Type(_) => Some(EntityClass::Type),
        AnyEntKind::ElementDeclaration(_) => None,
        AnyEntKind::Concurrent(_) => Some(EntityClass::Label),
        AnyEntKind::Sequential(_) => Some(EntityClass::Label),
        AnyEntKind::Object(obj) => match obj.class {
            ObjectClass::Signal => Some(EntityClass::Signal),
            ObjectClass::Constant => Some(EntityClass::Constant),
            ObjectClass::Variable => Some(EntityClass::Variable),
            ObjectClass::SharedVariable => Some(EntityClass::Variable),
        },
        AnyEntKind::LoopParameter(_) => None, // @TODO is it allowed?
        AnyEntKind::PhysicalLiteral(_) => None, // @TODO maybe Units?
        AnyEntKind::DeferredConstant(_) => Some(EntityClass::Constant),
        AnyEntKind::Library => None,
        AnyEntKind::Design(des) => match des {
            Design::Entity(_, _) => Some(EntityClass::Entity),
            Design::Architecture(_) => Some(EntityClass::Architecture),
            Design::Configuration => Some(EntityClass::Configuration),
            Design::Package(_, _) => Some(EntityClass::Package),
            // Should never be target of attribute
            Design::PackageBody => None,
            Design::UninstPackage(_, _) => None,
            Design::PackageInstance(_) => None,
            Design::Context(_) => None,
        },
    }
}

fn find_full_type_definition<'a>(
    name: &Symbol,
    decls: &'a [Declaration],
) -> Option<&'a TypeDeclaration> {
    for decl in decls.iter() {
        if let Declaration::Type(type_decl) = decl {
            match type_decl.def {
                TypeDefinition::Incomplete(..) => {
                    // ignored
                }
                _ => {
                    if type_decl.ident.name() == name {
                        return Some(type_decl);
                    }
                }
            }
        }
    }
    None
}
