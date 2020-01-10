// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2019, Olof Kraigher olof.kraigher@gmail.com

use super::*;
use crate::ast::*;
use crate::data::*;
use analyze::*;
use fnv::FnvHashMap;
use region::*;
use std::collections::hash_map::Entry;
use std::sync::Arc;

impl<'a> AnalyzeContext<'a> {
    pub fn analyze_declarative_part(
        &self,
        region: &mut Region<'_>,
        declarations: &mut [Declaration],
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        let mut incomplete_types: FnvHashMap<Symbol, (EntityId, SrcPos)> = FnvHashMap::default();

        for i in 0..declarations.len() {
            // Handle incomplete types

            let (decl, remaining) = declarations[i..].split_first_mut().unwrap();

            match decl {
                Declaration::Type(type_decl) => match type_decl.def {
                    TypeDefinition::Incomplete(ref mut reference) => {
                        reference.clear_reference();

                        let full_definiton =
                            find_full_type_definition(type_decl.ident.name(), remaining);

                        let decl_pos = match full_definiton {
                            Some(full_decl) => {
                                reference.set_reference_pos(Some(full_decl.ident.pos()));
                                full_decl.ident.pos()
                            }
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

                        match incomplete_types.entry(type_decl.ident.name().clone()) {
                            Entry::Vacant(entry) => {
                                let designator =
                                    Designator::Identifier(type_decl.ident.name().clone());
                                // Set incomplete type defintion to position of full declaration
                                let ent = NamedEntity::new(
                                    designator,
                                    NamedEntityKind::IncompleteType,
                                    Some(decl_pos),
                                );
                                entry.insert((ent.id(), type_decl.ident.pos().clone()));
                                region.add_named_entity(ent, diagnostics);
                            }
                            Entry::Occupied(entry) => {
                                let (_, decl_pos) = entry.get();

                                diagnostics.push(duplicate_error(
                                    &type_decl.ident,
                                    type_decl.ident.pos(),
                                    Some(decl_pos),
                                ));
                            }
                        }
                    }
                    _ => {
                        let incomplete_type = incomplete_types.get(type_decl.ident.name());
                        let id = incomplete_type.map(|(id, _)| *id);
                        self.analyze_type_declaration(region, type_decl, id, diagnostics)?;
                    }
                },
                _ => {
                    self.analyze_declaration(region, &mut declarations[i], diagnostics)?;
                }
            }
        }
        Ok(())
    }

    fn analyze_declaration(
        &self,
        region: &mut Region<'_>,
        decl: &mut Declaration,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        match decl {
            Declaration::Alias(alias) => {
                let AliasDeclaration {
                    designator,
                    name,
                    subtype_indication,
                    signature,
                } = alias;

                let resolved_name =
                    self.resolve_name(region, &name.pos, &mut name.item, diagnostics)?;

                if let Some(ref mut subtype_indication) = subtype_indication {
                    // Object alias
                    self.analyze_subtype_indication(region, subtype_indication, diagnostics)?;
                }

                if let Some(ref mut signature) = signature {
                    // Subprogram or enum literal alias
                    self.analyze_signature(region, signature, diagnostics)?;
                }

                let kind = {
                    if signature.is_some() {
                        NamedEntityKind::Overloaded
                    } else if subtype_indication.is_some() {
                        NamedEntityKind::OtherAlias
                    } else {
                        let named_entity =
                            resolved_name.and_then(|resolved| resolved.into_non_overloaded());

                        if let Some(named_entity) = named_entity {
                            region.add_implicit_declarations(
                                Some(&designator.pos),
                                &named_entity,
                                diagnostics,
                            );
                            NamedEntityKind::AliasOf(named_entity)
                        } else {
                            NamedEntityKind::OtherAlias
                        }
                    }
                };

                region.add(designator.clone(), kind, diagnostics);
            }
            Declaration::Object(ref mut object_decl) => {
                self.analyze_subtype_indication(
                    region,
                    &mut object_decl.subtype_indication,
                    diagnostics,
                )?;
                if let Some(ref mut expr) = object_decl.expression {
                    self.analyze_expression(region, expr, diagnostics)?;
                }
                region.add(
                    &object_decl.ident,
                    NamedEntityKind::from_object_declaration(object_decl),
                    diagnostics,
                );
            }
            Declaration::File(ref mut file) => {
                let FileDeclaration {
                    ident,
                    subtype_indication,
                    open_info,
                    file_name,
                } = file;
                self.analyze_subtype_indication(region, subtype_indication, diagnostics)?;
                if let Some(ref mut expr) = open_info {
                    self.analyze_expression(region, expr, diagnostics)?;
                }
                if let Some(ref mut expr) = file_name {
                    self.analyze_expression(region, expr, diagnostics)?;
                }
                region.add(ident.clone(), NamedEntityKind::File, diagnostics);
            }
            Declaration::Component(ref mut component) => {
                region.add(&component.ident, NamedEntityKind::Component, diagnostics);
                let mut region = region.nested();
                self.analyze_interface_list(&mut region, &mut component.generic_list, diagnostics)?;
                self.analyze_interface_list(&mut region, &mut component.port_list, diagnostics)?;
                region.close(diagnostics);
            }
            Declaration::Attribute(ref mut attr) => match attr {
                Attribute::Declaration(ref mut attr_decl) => {
                    if let Err(err) = self.resolve_type_mark(region, &mut attr_decl.type_mark) {
                        err.add_to(diagnostics)?;
                    }
                    region.add(&attr_decl.ident, NamedEntityKind::Attribute, diagnostics);
                }
                // @TODO Ignored for now
                Attribute::Specification(..) => {}
            },
            Declaration::SubprogramBody(ref mut body) => {
                region.add(
                    body.specification.designator(),
                    NamedEntityKind::Overloaded,
                    diagnostics,
                );
                let mut spec_region = self.analyze_subprogram_declaration(
                    region,
                    &mut body.specification,
                    diagnostics,
                )?;
                self.analyze_declarative_part(
                    &mut spec_region,
                    &mut body.declarations,
                    diagnostics,
                )?;
                self.analyze_sequential_part(&mut spec_region, &mut body.statements, diagnostics)?;
            }
            Declaration::SubprogramDeclaration(ref mut subdecl) => {
                region.add(
                    subdecl.designator(),
                    NamedEntityKind::Overloaded,
                    diagnostics,
                );
                self.analyze_subprogram_declaration(region, subdecl, diagnostics)?;
            }

            Declaration::Use(ref mut use_clause) => {
                self.analyze_use_clause(region, &mut use_clause.item, diagnostics)?;
            }

            Declaration::Package(ref mut instance) => {
                match self.analyze_package_instance_name(region, &mut instance.package_name) {
                    Ok(package_region) => region.add(
                        &instance.ident,
                        NamedEntityKind::LocalPackageInstance(package_region),
                        diagnostics,
                    ),
                    Err(err) => err.add_to(diagnostics)?,
                }
            }
            Declaration::Configuration(..) => {}
            Declaration::Type(..) => unreachable!("Handled elsewhere"),
        };

        Ok(())
    }

    fn analyze_type_declaration(
        &self,
        parent: &mut Region<'_>,
        type_decl: &mut TypeDeclaration,
        // Is the full type declaration of an incomplete type
        // Overwrite id when defining full type
        overwrite_id: Option<EntityId>,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        match type_decl.def {
            TypeDefinition::Enumeration(ref enumeration) => {
                for literal in enumeration.iter() {
                    parent.add(
                        literal.clone().map_into(|lit| lit.into_designator()),
                        NamedEntityKind::Overloaded,
                        diagnostics,
                    )
                }

                let mut enum_region = Region::default();
                let mut ignored = Vec::new();
                for literal in enumeration.iter() {
                    enum_region.add(
                        literal.clone().map_into(|lit| lit.into_designator()),
                        NamedEntityKind::Overloaded,
                        // Ignore diagnostics as they will be given above
                        &mut ignored,
                    )
                }

                add_or_overwrite(
                    parent,
                    &type_decl.ident,
                    NamedEntityKind::TypeDeclaration(Some(Arc::new(enum_region))),
                    overwrite_id,
                    diagnostics,
                );
            }
            TypeDefinition::ProtectedBody(ref mut body) => {
                body.type_reference.clear_reference();

                match parent.lookup_extended(&type_decl.ident.item.clone().into()) {
                    Some(visible) => {
                        let is_ok = match visible.clone().into_non_overloaded() {
                            Ok(ent) => {
                                if let NamedEntityKind::ProtectedType(ptype_region) = ent.kind() {
                                    body.type_reference.set_unique_reference(&ent);
                                    let mut region = Region::extend(&ptype_region, Some(parent));
                                    self.analyze_declarative_part(
                                        &mut region,
                                        &mut body.decl,
                                        diagnostics,
                                    )?;
                                    parent.add_protected_body(type_decl.ident.clone(), diagnostics);
                                    true
                                } else {
                                    false
                                }
                            }
                            _ => false,
                        };

                        if !is_ok {
                            diagnostics.push(Diagnostic::error(
                                type_decl.ident.pos(),
                                format!("'{}' is not a protected type", &type_decl.ident.item),
                            ));
                        }
                    }
                    None => {
                        diagnostics.push(Diagnostic::error(
                            type_decl.ident.pos(),
                            format!(
                                "No declaration of protected type '{}'",
                                &type_decl.ident.item
                            ),
                        ));
                    }
                };
            }
            TypeDefinition::Protected(ref mut prot_decl) => {
                // Protected type name is visible inside its declarative region
                // This will be overwritten later when the protected type region is finished
                let id = add_or_overwrite(
                    parent,
                    &type_decl.ident,
                    NamedEntityKind::TypeDeclaration(None),
                    overwrite_id,
                    diagnostics,
                );

                let mut region = parent.nested();
                for item in prot_decl.items.iter_mut() {
                    match item {
                        ProtectedTypeDeclarativeItem::Subprogram(ref mut subprogram) => {
                            region.add(
                                subprogram.designator(),
                                NamedEntityKind::Overloaded,
                                diagnostics,
                            );
                            self.analyze_subprogram_declaration(&region, subprogram, diagnostics)?;
                        }
                    }
                }
                let region = region.without_parent();

                parent.overwrite(
                    type_decl.ident.name().clone(),
                    NamedEntity::new_with_id(
                        id,
                        type_decl.ident.name().clone(),
                        NamedEntityKind::ProtectedType(Arc::new(region)),
                        Some(type_decl.ident.pos()),
                    ),
                );
            }
            TypeDefinition::Record(ref mut element_decls) => {
                let mut region = Region::default();
                for elem_decl in element_decls.iter_mut() {
                    self.analyze_subtype_indication(parent, &mut elem_decl.subtype, diagnostics)?;
                    region.add(&elem_decl.ident, NamedEntityKind::RecordField, diagnostics);
                }
                region.close(diagnostics);

                add_or_overwrite(
                    parent,
                    &type_decl.ident,
                    NamedEntityKind::TypeDeclaration(None),
                    overwrite_id,
                    diagnostics,
                );
            }
            TypeDefinition::Access(ref mut subtype_indication) => {
                self.analyze_subtype_indication(parent, subtype_indication, diagnostics)?;

                add_or_overwrite(
                    parent,
                    &type_decl.ident,
                    NamedEntityKind::TypeDeclaration(None),
                    overwrite_id,
                    diagnostics,
                );
            }
            TypeDefinition::Array(ref mut array_indexes, ref mut subtype_indication) => {
                for index in array_indexes.iter_mut() {
                    self.analyze_array_index(parent, index, diagnostics)?;
                }
                self.analyze_subtype_indication(parent, subtype_indication, diagnostics)?;

                add_or_overwrite(
                    parent,
                    &type_decl.ident,
                    NamedEntityKind::TypeDeclaration(None),
                    overwrite_id,
                    diagnostics,
                );
            }
            TypeDefinition::Subtype(ref mut subtype_indication) => {
                self.analyze_subtype_indication(parent, subtype_indication, diagnostics)?;

                add_or_overwrite(
                    parent,
                    &type_decl.ident,
                    NamedEntityKind::TypeDeclaration(None),
                    overwrite_id,
                    diagnostics,
                );
            }
            TypeDefinition::Physical(ref mut physical) => {
                parent.add(
                    physical.primary_unit.clone(),
                    NamedEntityKind::PhysicalLiteral,
                    diagnostics,
                );
                for (secondary_unit_name, _) in physical.secondary_units.iter_mut() {
                    parent.add(
                        secondary_unit_name.clone(),
                        NamedEntityKind::PhysicalLiteral,
                        diagnostics,
                    )
                }

                add_or_overwrite(
                    parent,
                    &type_decl.ident,
                    NamedEntityKind::TypeDeclaration(None),
                    overwrite_id,
                    diagnostics,
                );
            }
            TypeDefinition::Incomplete(..) => {
                unreachable!("Handled elsewhere");
            }

            TypeDefinition::Integer(ref mut range) => {
                self.analyze_range(parent, range, diagnostics)?;
                add_or_overwrite(
                    parent,
                    &type_decl.ident,
                    NamedEntityKind::TypeDeclaration(None),
                    overwrite_id,
                    diagnostics,
                );
            }

            TypeDefinition::File(ref mut type_mark) => {
                if let Err(err) = self.resolve_type_mark(parent, type_mark) {
                    err.add_to(diagnostics)?;
                }

                let mut implicit = Region::default();
                for name in ["file_open", "file_close", "endfile"].iter() {
                    implicit.add_implicit(
                        Designator::Identifier(self.symbol_utf8(name)),
                        None,
                        NamedEntityKind::Overloaded,
                        diagnostics,
                    );
                }
                add_or_overwrite(
                    parent,
                    &type_decl.ident,
                    NamedEntityKind::TypeDeclaration(Some(Arc::new(implicit))),
                    overwrite_id,
                    diagnostics,
                );
            }
        }

        Ok(())
    }

    pub fn analyze_signature(
        &self,
        region: &Region<'_>,
        signature: &mut Signature,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        match signature {
            Signature::Function(ref mut args, ref mut ret) => {
                for arg in args.iter_mut() {
                    if let Err(err) = self.resolve_selected_name(region, arg) {
                        err.add_to(diagnostics)?;
                    }
                }
                if let Err(err) = self.resolve_selected_name(region, ret) {
                    err.add_to(diagnostics)?;
                }
            }
            Signature::Procedure(args) => {
                for arg in args.iter_mut() {
                    if let Err(err) = self.resolve_selected_name(region, arg) {
                        err.add_to(diagnostics)?;
                    }
                }
            }
        }
        Ok(())
    }

    fn analyze_interface_declaration(
        &self,
        region: &mut Region<'_>,
        decl: &mut InterfaceDeclaration,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        match decl {
            InterfaceDeclaration::File(ref mut file_decl) => {
                self.analyze_subtype_indication(
                    region,
                    &mut file_decl.subtype_indication,
                    diagnostics,
                )?;
                region.add(
                    &file_decl.ident,
                    NamedEntityKind::InterfaceFile,
                    diagnostics,
                );
            }
            InterfaceDeclaration::Object(ref mut object_decl) => {
                self.analyze_subtype_indication(
                    region,
                    &mut object_decl.subtype_indication,
                    diagnostics,
                )?;
                if let Some(ref mut expression) = object_decl.expression {
                    self.analyze_expression(region, expression, diagnostics)?
                }
                region.add(
                    &object_decl.ident,
                    NamedEntityKind::InterfaceObject(object_decl.class),
                    diagnostics,
                );
            }
            InterfaceDeclaration::Type(ref ident) => {
                region.add(ident, NamedEntityKind::InterfaceType, diagnostics);
            }
            InterfaceDeclaration::Subprogram(ref mut subpgm, ..) => {
                self.analyze_subprogram_declaration(region, subpgm, diagnostics)?;
                region.add(
                    subpgm.designator(),
                    NamedEntityKind::Overloaded,
                    diagnostics,
                );
            }
            InterfaceDeclaration::Package(ref mut instance) => {
                match self.analyze_package_instance_name(region, &mut instance.package_name) {
                    Ok(package_region) => region.add(
                        &instance.ident,
                        NamedEntityKind::LocalPackageInstance(package_region.clone()),
                        diagnostics,
                    ),
                    Err(err) => {
                        err.add_to(diagnostics)?;
                    }
                }
            }
        };
        Ok(())
    }

    pub fn analyze_interface_list(
        &self,
        region: &mut Region<'_>,
        declarations: &mut [InterfaceDeclaration],
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        for decl in declarations.iter_mut() {
            self.analyze_interface_declaration(region, decl, diagnostics)?;
        }
        Ok(())
    }

    fn analyze_array_index(
        &self,
        region: &mut Region<'_>,
        array_index: &mut ArrayIndex,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        match array_index {
            ArrayIndex::IndexSubtypeDefintion(ref mut type_mark) => {
                if let Err(err) = self.resolve_type_mark(region, type_mark) {
                    err.add_to(diagnostics)?;
                }
            }
            ArrayIndex::Discrete(ref mut drange) => {
                self.analyze_discrete_range(region, drange, diagnostics)?;
            }
        }
        Ok(())
    }
    fn analyze_element_constraint(
        &self,
        region: &Region<'_>,
        constraint: &mut ElementConstraint,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        // @TODO more
        let ElementConstraint { constraint, .. } = constraint;
        self.analyze_subtype_constraint(region, &mut constraint.item, diagnostics)
    }

    fn analyze_subtype_constraint(
        &self,
        region: &Region<'_>,
        constraint: &mut SubtypeConstraint,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        match constraint {
            SubtypeConstraint::Array(ref mut dranges, ref mut constraint) => {
                for drange in dranges.iter_mut() {
                    self.analyze_discrete_range(region, drange, diagnostics)?;
                }
                if let Some(constraint) = constraint {
                    self.analyze_subtype_constraint(region, &mut constraint.item, diagnostics)?;
                }
            }
            SubtypeConstraint::Range(ref mut range) => {
                self.analyze_range(region, range, diagnostics)?;
            }
            SubtypeConstraint::Record(ref mut constraints) => {
                for constraint in constraints.iter_mut() {
                    self.analyze_element_constraint(region, constraint, diagnostics)?;
                }
            }
        }
        Ok(())
    }

    pub fn analyze_subtype_indication(
        &self,
        region: &Region<'_>,
        subtype_indication: &mut SubtypeIndication,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        // @TODO more
        let SubtypeIndication {
            type_mark,
            constraint,
            ..
        } = subtype_indication;

        if let Err(err) = self.resolve_type_mark(region, type_mark) {
            err.add_to(diagnostics)?
        }

        if let Some(constraint) = constraint {
            self.analyze_subtype_constraint(region, &mut constraint.item, diagnostics)?;
        }

        Ok(())
    }

    fn analyze_subprogram_declaration<'r>(
        &self,
        parent: &'r Region<'_>,
        subprogram: &mut SubprogramDeclaration,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult<Region<'r>> {
        let mut region = parent.nested();

        match subprogram {
            SubprogramDeclaration::Function(fun) => {
                self.analyze_interface_list(&mut region, &mut fun.parameter_list, diagnostics)?;
                if let Err(err) = self.resolve_type_mark(&parent, &mut fun.return_type) {
                    err.add_to(diagnostics)?
                }
            }
            SubprogramDeclaration::Procedure(procedure) => {
                self.analyze_interface_list(
                    &mut region,
                    &mut procedure.parameter_list,
                    diagnostics,
                )?;
            }
        }
        region.close(diagnostics);
        Ok(region)
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

fn add_or_overwrite(
    region: &mut Region,
    name: &Ident,
    kind: NamedEntityKind,
    old_id: Option<EntityId>,
    diagnostics: &mut dyn DiagnosticHandler,
) -> EntityId {
    if let Some(id) = old_id {
        let ent = NamedEntity::new_with_id(id, name.name().clone(), kind, Some(name.pos()));
        region.overwrite(name.name().clone(), ent);
        id
    } else {
        let ent = NamedEntity::new(name.name().clone(), kind, Some(&name.pos));
        let id = ent.id();
        region.add_named_entity(ent, diagnostics);
        id
    }
}
