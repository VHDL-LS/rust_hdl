// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2019, Olof Kraigher olof.kraigher@gmail.com

use super::formal_region::FormalRegion;
use super::formal_region::RecordRegion;
use super::implicits::ImplicitVecBuilder;
use super::names::*;
use super::*;
use crate::ast;
use crate::ast::*;
use crate::data::*;
use analyze::*;
use arc_swap::ArcSwapOption;
use arc_swap::ArcSwapWeak;
use fnv::FnvHashMap;
use named_entity::Signature;
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
        let mut incomplete_types: FnvHashMap<Symbol, (Arc<NamedEntity>, SrcPos)> =
            FnvHashMap::default();

        for i in 0..declarations.len() {
            // Handle incomplete types

            let (decl, remaining) = declarations[i..].split_first_mut().unwrap();

            match decl {
                Declaration::Type(type_decl) => match type_decl.def {
                    TypeDefinition::Incomplete(ref mut reference) => {
                        reference.clear_reference();

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
                                let ent = Arc::new(NamedEntity::new(
                                    designator,
                                    NamedEntityKind::Type(Type::Incomplete(ArcSwapWeak::default())),
                                    Some(decl_pos),
                                ));
                                reference.set_unique_reference(&ent);

                                entry.insert((ent.clone(), type_decl.ident.pos().clone()));
                                region.add(ent, diagnostics);
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
                        if let Some((incomplete_type, _)) = incomplete_type {
                            self.analyze_type_declaration(
                                region,
                                type_decl,
                                Some(incomplete_type.id()),
                                diagnostics,
                            )?;

                            // Lookup the newly analyzed type and set it as the full definition of
                            // the incomplete type
                            if let Some(NamedEntities::Single(full_type)) =
                                region.lookup_immediate(incomplete_type.designator())
                            {
                                if let NamedEntityKind::Type(Type::Incomplete(full_ref)) =
                                    incomplete_type.kind()
                                {
                                    if full_type.kind().is_type() {
                                        full_ref.store(Arc::downgrade(full_type));
                                    }
                                }
                            }
                        } else {
                            self.analyze_type_declaration(region, type_decl, None, diagnostics)?;
                        }
                    }
                },
                _ => {
                    self.analyze_declaration(region, &mut declarations[i], diagnostics)?;
                }
            }
        }
        Ok(())
    }

    fn analyze_alias_declaration(
        &self,
        region: &Region<'_>,
        alias: &mut AliasDeclaration,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult<Option<Arc<NamedEntity>>> {
        let AliasDeclaration {
            designator,
            name,
            subtype_indication,
            signature,
        } = alias;

        let resolved_name = self.resolve_object_prefix(
            region,
            &name.pos,
            &mut name.item,
            "Invalid alias name",
            diagnostics,
        );

        if let Some(ref mut subtype_indication) = subtype_indication {
            // Object alias
            self.analyze_subtype_indication(region, subtype_indication, diagnostics)?;
        }

        let resolved_name = match resolved_name {
            Ok(resolved_name) => resolved_name,
            Err(err) => {
                err.add_to(diagnostics)?;
                return Ok(None);
            }
        };

        let kind = {
            match resolved_name {
                ResolvedName::ObjectSelection {
                    base_object,
                    type_mark,
                } => {
                    if let Some(ref signature) = signature {
                        diagnostics.push(signature_error(signature));
                    }
                    NamedEntityKind::ObjectAlias {
                        base_object,
                        type_mark,
                    }
                }
                ResolvedName::ExternalName { class, type_mark } => {
                    if let Some(ref signature) = signature {
                        diagnostics.push(signature_error(signature));
                    }
                    NamedEntityKind::ExternalAlias { class, type_mark }
                }
                ResolvedName::NonObject(ent) => {
                    if let Some(ref signature) = signature {
                        diagnostics.push(signature_error(signature));
                    }
                    NamedEntityKind::NonObjectAlias(ent)
                }
                ResolvedName::Type(typ) => {
                    if let Some(ref signature) = signature {
                        diagnostics.push(signature_error(signature));
                    }
                    NamedEntityKind::Type(Type::Alias(typ))
                }
                ResolvedName::Overloaded(overloaded) => {
                    if let Some(ref mut signature) = signature {
                        match self.resolve_signature(region, signature) {
                            Ok(signature_key) => {
                                if let Some(ent) = overloaded.get(&signature_key) {
                                    if let Some(reference) = name.item.suffix_reference_mut() {
                                        reference.set_unique_reference(ent.inner());
                                    }
                                    NamedEntityKind::NonObjectAlias(ent.into())
                                } else {
                                    let mut diagnostic = Diagnostic::error(
                                        name,
                                        "Could not find declaration with given signature",
                                    );
                                    for ent in overloaded.entities() {
                                        if let Some(pos) = ent.decl_pos() {
                                            diagnostic.add_related(
                                                pos,
                                                format!("Found {}", ent.describe()),
                                            );
                                        }
                                    }
                                    diagnostics.push(diagnostic);
                                    return Ok(None);
                                }
                            }
                            Err(err) => {
                                err.add_to(diagnostics)?;
                                return Ok(None);
                            }
                        }
                    } else {
                        diagnostics.error(
                            name,
                            "Signature required for alias of subprogram and enum literals",
                        );
                        return Ok(None);
                    }
                }
            }
        };

        Ok(Some(designator.define(kind)))
    }

    fn analyze_declaration(
        &self,
        region: &mut Region<'_>,
        decl: &mut Declaration,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        match decl {
            Declaration::Alias(alias) => {
                if let Some(ent) = self.analyze_alias_declaration(region, alias, diagnostics)? {
                    region.add(ent.clone(), diagnostics);
                    region.add_implicit_declaration_aliases(ent, diagnostics);
                }
            }
            Declaration::Object(ref mut object_decl) => {
                let subtype = self.resolve_subtype_indication(
                    region,
                    &mut object_decl.subtype_indication,
                    diagnostics,
                );

                if let Some(ref mut expr) = object_decl.expression {
                    if let Ok(ref subtype) = subtype {
                        self.analyze_expression_with_target_type(
                            region,
                            subtype.type_mark(),
                            &expr.pos,
                            &mut expr.item,
                            diagnostics,
                        )?;
                    } else {
                        self.analyze_expression(region, expr, diagnostics)?;
                    }
                }

                match subtype {
                    Ok(subtype) => {
                        let kind = if object_decl.class == ObjectClass::Constant
                            && object_decl.expression.is_none()
                        {
                            NamedEntityKind::DeferredConstant(subtype)
                        } else {
                            NamedEntityKind::Object(Object {
                                class: object_decl.class,
                                mode: None,
                                has_default: object_decl.expression.is_some(),
                                subtype,
                            })
                        };
                        region.add(object_decl.ident.define(kind), diagnostics);
                    }
                    Err(err) => err.add_to(diagnostics)?,
                }
            }
            Declaration::File(ref mut file) => {
                let FileDeclaration {
                    ident,
                    subtype_indication,
                    open_info,
                    file_name,
                } = file;

                let subtype = match self.resolve_subtype_indication(
                    region,
                    subtype_indication,
                    diagnostics,
                ) {
                    Ok(subtype) => Some(subtype),
                    Err(err) => {
                        err.add_to(diagnostics)?;
                        None
                    }
                };

                if let Some(ref mut expr) = open_info {
                    self.analyze_expression(region, expr, diagnostics)?;
                }
                if let Some(ref mut expr) = file_name {
                    self.analyze_expression(region, expr, diagnostics)?;
                }

                if let Some(subtype) = subtype {
                    region.add(ident.define(NamedEntityKind::File(subtype)), diagnostics);
                }
            }
            Declaration::Component(ref mut component) => {
                let mut component_region = region.nested();
                self.analyze_interface_list(
                    &mut component_region,
                    &mut component.generic_list,
                    diagnostics,
                )?;
                self.analyze_interface_list(
                    &mut component_region,
                    &mut component.port_list,
                    diagnostics,
                )?;
                component_region.close(diagnostics);
                region.add(
                    component.ident.define(NamedEntityKind::Component(
                        component_region.without_parent(),
                    )),
                    diagnostics,
                );
            }
            Declaration::Attribute(ref mut attr) => match attr {
                Attribute::Declaration(ref mut attr_decl) => {
                    if let Err(err) = self.resolve_type_mark(region, &mut attr_decl.type_mark) {
                        err.add_to(diagnostics)?;
                    }
                    region.add(
                        attr_decl.ident.define(NamedEntityKind::Attribute),
                        diagnostics,
                    );
                }
                // @TODO Ignored for now
                Attribute::Specification(..) => {}
            },
            Declaration::SubprogramBody(ref mut body) => {
                let mut subpgm_region = region.nested();

                let signature = self.analyze_subprogram_declaration(
                    &mut subpgm_region,
                    &mut body.specification,
                    diagnostics,
                );

                // End mutable borrow of parent
                let subpgm_region = subpgm_region.without_parent();

                // Overwrite subprogram definition with full signature
                match signature {
                    Ok(signature) => {
                        let subpgm_ent = body
                            .specification
                            .define(NamedEntityKind::Subprogram(signature));
                        region.add(subpgm_ent, diagnostics);
                    }
                    Err(err) => err.add_to(diagnostics)?,
                }
                let mut subpgm_region = subpgm_region.with_parent(region);

                self.analyze_declarative_part(
                    &mut subpgm_region,
                    &mut body.declarations,
                    diagnostics,
                )?;
                subpgm_region.close(diagnostics);

                self.analyze_sequential_part(
                    &mut subpgm_region,
                    &mut body.statements,
                    diagnostics,
                )?;
            }
            Declaration::SubprogramDeclaration(ref mut subdecl) => {
                let mut subpgm_region = region.nested();
                let signature =
                    self.analyze_subprogram_declaration(&mut subpgm_region, subdecl, diagnostics);
                subpgm_region.close(diagnostics);
                drop(subpgm_region);

                match signature {
                    Ok(signature) => {
                        region.add(
                            subdecl.define(NamedEntityKind::SubprogramDecl(signature)),
                            diagnostics,
                        );
                    }
                    Err(err) => err.add_to(diagnostics)?,
                }
            }

            Declaration::Use(ref mut use_clause) => {
                self.analyze_use_clause(region, &mut use_clause.item, diagnostics)?;
            }

            Declaration::Package(ref mut instance) => {
                match self.analyze_package_instance_name(region, &mut instance.package_name) {
                    Ok(package_region) => region.add(
                        instance
                            .ident
                            .define(NamedEntityKind::LocalPackageInstance(package_region)),
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
            TypeDefinition::Enumeration(ref mut enumeration) => {
                let implicit = ImplicitVecBuilder::default();
                let enum_type = TypeEnt::define_with_opt_id(
                    overwrite_id,
                    &mut type_decl.ident,
                    Type::Enum(
                        implicit.inner(),
                        enumeration
                            .iter()
                            .map(|literal| literal.tree.item.clone().into_designator())
                            .collect(),
                    ),
                );

                let signature = Signature::new(
                    FormalRegion::new(InterfaceListType::Parameter),
                    Some(enum_type.clone()),
                );

                for literal in enumeration.iter_mut() {
                    let literal_ent = Arc::new(NamedEntity::new(
                        literal.tree.item.clone().into_designator(),
                        NamedEntityKind::EnumLiteral(signature.clone()),
                        Some(&literal.tree.pos),
                    ));
                    literal.decl = Some(literal_ent.clone());
                    implicit.push(&literal_ent);
                    parent.add(literal_ent, diagnostics);
                }

                parent.add(enum_type.clone().into(), diagnostics);

                if let Some(standard) = self.standard_package() {
                    for ent in standard.enum_implicits(enum_type) {
                        implicit.push(&ent);
                        parent.add(ent, diagnostics);
                    }
                }
            }
            TypeDefinition::ProtectedBody(ref mut body) => {
                body.type_reference.clear_reference();

                match parent.lookup_immediate(&type_decl.ident.tree.item.clone().into()) {
                    Some(visible) => {
                        let is_ok = match visible.clone().into_non_overloaded() {
                            Ok(ent) => {
                                if let NamedEntityKind::Type(Type::Protected(
                                    ptype_region,
                                    body_pos,
                                )) = ent.kind()
                                {
                                    body.type_reference.set_unique_reference(&ent);
                                    let mut region = Region::extend(ptype_region, Some(parent));
                                    self.analyze_declarative_part(
                                        &mut region,
                                        &mut body.decl,
                                        diagnostics,
                                    )?;

                                    if let Some(prev_pos) = body_pos
                                        .swap(Some(Arc::new(type_decl.ident.tree.pos.clone())))
                                    {
                                        diagnostics.push(duplicate_error(
                                            &type_decl.ident.tree,
                                            &type_decl.ident.tree.pos,
                                            Some(&prev_pos),
                                        ))
                                    }

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
                                format!("'{}' is not a protected type", &type_decl.ident),
                            ));
                        }
                    }
                    None => {
                        diagnostics.push(Diagnostic::error(
                            type_decl.ident.pos(),
                            format!("No declaration of protected type '{}'", &type_decl.ident),
                        ));
                    }
                };
            }
            TypeDefinition::Protected(ref mut prot_decl) => {
                // Protected type name is visible inside its declarative region
                // This will be overwritten later when the protected type region is finished
                // @TODO mutate region
                let ptype: Arc<NamedEntity> = TypeEnt::define_with_opt_id(
                    overwrite_id,
                    &mut type_decl.ident,
                    Type::Protected(Region::default(), ArcSwapOption::default()),
                )
                .into();

                parent.add(ptype.clone(), diagnostics);

                let mut region = parent.nested();
                for item in prot_decl.items.iter_mut() {
                    match item {
                        ProtectedTypeDeclarativeItem::Subprogram(ref mut subprogram) => {
                            let mut subpgm_region = region.nested();
                            let signature = self.analyze_subprogram_declaration(
                                &mut subpgm_region,
                                subprogram,
                                diagnostics,
                            );
                            subpgm_region.close(diagnostics);
                            drop(subpgm_region);

                            match signature {
                                Ok(signature) => {
                                    region.add(
                                        subprogram
                                            .define(NamedEntityKind::SubprogramDecl(signature)),
                                        diagnostics,
                                    );
                                }
                                Err(err) => {
                                    err.add_to(diagnostics)?;
                                }
                            }
                        }
                    }
                }

                // This is safe since we are in a single thread and no other reference can exist yes
                // Also the region is stored inside an Arc which cannot move
                {
                    let NamedEntityKind::Type(Type::Protected(region_ptr, _)) = ptype.kind() else {
                        unreachable!();
                    };

                    let region_ptr = unsafe {
                        let region_ptr = region_ptr as *const Region<'static>;
                        let region_ptr = region_ptr as *mut Region<'static>;
                        &mut *region_ptr as &mut Region<'static>
                    };
                    *region_ptr = region.without_parent();
                }
            }
            TypeDefinition::Record(ref mut element_decls) => {
                let mut elems = RecordRegion::default();
                let mut region = Region::default();
                for elem_decl in element_decls.iter_mut() {
                    let subtype = self.resolve_subtype_indication(
                        parent,
                        &mut elem_decl.subtype,
                        diagnostics,
                    );
                    match subtype {
                        Ok(subtype) => {
                            let elem = elem_decl
                                .ident
                                .define(NamedEntityKind::ElementDeclaration(subtype));
                            region.add(elem.clone(), diagnostics);
                            elems.add(elem);
                        }
                        Err(err) => {
                            err.add_to(diagnostics)?;
                        }
                    }
                }
                region.close(diagnostics);

                let implicit = ImplicitVecBuilder::default();
                let type_ent = TypeEnt::define_with_opt_id(
                    overwrite_id,
                    &mut type_decl.ident,
                    Type::Record(elems, implicit.inner()),
                );
                parent.add(type_ent.clone().into(), diagnostics);

                if let Some(standard) = self.standard_package() {
                    for ent in standard.record_implicits(type_ent) {
                        implicit.push(&ent);
                        parent.add(ent, diagnostics);
                    }
                }
            }
            TypeDefinition::Access(ref mut subtype_indication) => {
                let subtype =
                    self.resolve_subtype_indication(parent, subtype_indication, diagnostics);
                match subtype {
                    Ok(subtype) => {
                        let implicit = ImplicitVecBuilder::default();
                        let type_ent = TypeEnt::define_with_opt_id(
                            overwrite_id,
                            &mut type_decl.ident,
                            Type::Access(subtype, implicit.inner()),
                        );

                        parent.add(type_ent.clone().into(), diagnostics);

                        if let Some(standard) = self.standard_package() {
                            for ent in standard.access_implicits(type_ent) {
                                implicit.push(&ent);
                                parent.add(ent, diagnostics);
                            }
                        }
                    }
                    Err(err) => err.add_to(diagnostics)?,
                }
            }
            TypeDefinition::Array(ref mut array_indexes, ref mut subtype_indication) => {
                let mut indexes: Vec<Option<TypeEnt>> = Vec::with_capacity(array_indexes.len());
                for index in array_indexes.iter_mut() {
                    indexes.push(self.analyze_array_index(parent, index, diagnostics)?);
                }

                let elem_type = match self.resolve_subtype_indication(
                    parent,
                    subtype_indication,
                    diagnostics,
                ) {
                    Ok(subtype) => subtype.type_mark().to_owned(),
                    Err(err) => {
                        err.add_to(diagnostics)?;
                        return Ok(());
                    }
                };

                let implicits = ImplicitVecBuilder::default();
                let array_ent = TypeEnt::define_with_opt_id(
                    overwrite_id,
                    &mut type_decl.ident,
                    Type::Array {
                        implicit: implicits.inner(),
                        indexes,
                        elem_type,
                    },
                );

                parent.add(array_ent.clone().into(), diagnostics);

                if let Some(standard) = self.standard_package() {
                    for ent in standard.array_implicits(array_ent) {
                        implicits.push(&ent);
                        parent.add(ent, diagnostics);
                    }
                }
            }
            TypeDefinition::Subtype(ref mut subtype_indication) => {
                match self.resolve_subtype_indication(parent, subtype_indication, diagnostics) {
                    Ok(subtype) => {
                        let type_ent = TypeEnt::define_with_opt_id(
                            overwrite_id,
                            &mut type_decl.ident,
                            Type::Subtype(subtype),
                        );
                        parent.add(type_ent.into(), diagnostics);
                    }
                    Err(err) => {
                        err.add_to(diagnostics)?;
                    }
                }
            }
            TypeDefinition::Physical(ref mut physical) => {
                let implicits = ImplicitVecBuilder::default();

                let phys_type = TypeEnt::define_with_opt_id(
                    overwrite_id,
                    &mut type_decl.ident,
                    Type::Physical(implicits.inner()),
                );
                parent.add(phys_type.clone().into(), diagnostics);

                let primary = physical
                    .primary_unit
                    .define(NamedEntityKind::PhysicalLiteral(phys_type.clone()));

                implicits.push(&primary);
                parent.add(primary, diagnostics);

                for (secondary_unit_name, value) in physical.secondary_units.iter_mut() {
                    match self.resolve_physical_unit(parent, &mut value.unit) {
                        Ok(secondary_unit_type) => {
                            if secondary_unit_type.base_type() != &phys_type {
                                diagnostics.error(
                                    &value.unit.item.pos,
                                    format!(
                                        "Physical unit of type '{}' does not match {}",
                                        secondary_unit_type.designator(),
                                        phys_type.describe()
                                    ),
                                )
                            }
                        }
                        Err(err) => diagnostics.push(err),
                    }

                    let secondary_unit = secondary_unit_name
                        .define(NamedEntityKind::PhysicalLiteral(phys_type.clone()));
                    implicits.push(&secondary_unit);
                    parent.add(secondary_unit, diagnostics)
                }

                if let Some(standard) = self.standard_package() {
                    for ent in standard.physical_implicits(phys_type) {
                        implicits.push(&ent);
                        parent.add(ent, diagnostics);
                    }
                }
            }
            TypeDefinition::Incomplete(..) => {
                unreachable!("Handled elsewhere");
            }

            TypeDefinition::Integer(ref mut range) => {
                self.analyze_range(parent, range, diagnostics)?;
                let implicit = ImplicitVecBuilder::default();

                let kind = match integer_or_real_range(range) {
                    ScalarType::Integer => Type::Integer(implicit.inner()),
                    ScalarType::Real => Type::Real(implicit.inner()),
                };

                let type_ent =
                    TypeEnt::define_with_opt_id(overwrite_id, &mut type_decl.ident, kind);
                parent.add(type_ent.clone().into(), diagnostics);

                if let Some(standard) = self.standard_package() {
                    for ent in standard.integer_implicits(type_ent) {
                        implicit.push(&ent);
                        parent.add(ent, diagnostics);
                    }
                }
            }

            TypeDefinition::File(ref mut type_mark) => {
                let implicit = ImplicitVecBuilder::default();

                let file_type = TypeEnt::define_with_opt_id(
                    overwrite_id,
                    &mut type_decl.ident,
                    Type::File(implicit.inner()),
                );

                match self.resolve_type_mark_name(parent, type_mark) {
                    Ok(type_mark) => {
                        if let Some(standard) = self.standard_package() {
                            for ent in standard
                                .create_implicit_file_type_subprograms(&file_type, &type_mark)
                            {
                                implicit.push(&ent);
                                parent.add(ent, diagnostics);
                            }
                        }
                    }
                    Err(err) => {
                        err.add_to(diagnostics)?;
                    }
                }

                parent.add(file_type.into(), diagnostics);
            }
        }

        Ok(())
    }

    pub fn resolve_signature(
        &self,
        region: &Region<'_>,
        signature: &mut WithPos<ast::Signature>,
    ) -> AnalysisResult<SignatureKey> {
        let (args, return_type) = match &mut signature.item {
            ast::Signature::Function(ref mut args, ref mut ret) => {
                let args: Vec<_> = args
                    .iter_mut()
                    .map(|arg| self.resolve_type_mark_name(region, arg))
                    .collect();
                let return_type = self.resolve_type_mark_name(region, ret);
                (args, Some(return_type))
            }
            ast::Signature::Procedure(args) => {
                let args: Vec<_> = args
                    .iter_mut()
                    .map(|arg| self.resolve_type_mark_name(region, arg))
                    .collect();
                (args, None)
            }
        };

        let mut params = Vec::with_capacity(args.len());
        for arg in args {
            params.push(arg?.base_type().id());
        }

        if let Some(return_type) = return_type {
            Ok(SignatureKey::new(
                params,
                Some(return_type?.base_type().id()),
            ))
        } else {
            Ok(SignatureKey::new(params, None))
        }
    }

    fn analyze_interface_declaration(
        &self,
        region: &Region<'_>,
        decl: &mut InterfaceDeclaration,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> AnalysisResult<Arc<NamedEntity>> {
        let ent = match decl {
            InterfaceDeclaration::File(ref mut file_decl) => {
                let file_type = self.resolve_subtype_indication(
                    region,
                    &mut file_decl.subtype_indication,
                    diagnostics,
                )?;
                file_decl.ident.define(NamedEntityKind::InterfaceFile(
                    file_type.type_mark().to_owned(),
                ))
            }
            InterfaceDeclaration::Object(ref mut object_decl) => {
                let subtype = self.resolve_subtype_indication(
                    region,
                    &mut object_decl.subtype_indication,
                    diagnostics,
                );

                if let Some(ref mut expression) = object_decl.expression {
                    if let Ok(ref subtype) = subtype {
                        self.analyze_expression_with_target_type(
                            region,
                            subtype.type_mark(),
                            &expression.pos,
                            &mut expression.item,
                            diagnostics,
                        )?;
                    } else {
                        self.analyze_expression(region, expression, diagnostics)?
                    }
                }

                let subtype = subtype?;
                object_decl.ident.define(NamedEntityKind::Object(Object {
                    class: object_decl.class,
                    mode: Some(object_decl.mode),
                    subtype,
                    has_default: object_decl.expression.is_some(),
                }))
            }
            InterfaceDeclaration::Type(ref mut ident) => {
                ident.define(NamedEntityKind::Type(Type::Interface))
            }
            InterfaceDeclaration::Subprogram(ref mut subpgm, ..) => {
                let mut subpgm_region = region.nested();
                let signature =
                    self.analyze_subprogram_declaration(&mut subpgm_region, subpgm, diagnostics);
                subpgm_region.close(diagnostics);
                drop(subpgm_region);

                subpgm.define(NamedEntityKind::Subprogram(signature?))
            }
            InterfaceDeclaration::Package(ref mut instance) => {
                let package_region =
                    self.analyze_package_instance_name(region, &mut instance.package_name)?;

                instance.ident.define(NamedEntityKind::LocalPackageInstance(
                    package_region.clone(),
                ))
            }
        };
        Ok(ent)
    }

    pub fn analyze_interface_list(
        &self,
        region: &mut Region<'_>,
        declarations: &mut [InterfaceDeclaration],
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        for decl in declarations.iter_mut() {
            match self.analyze_interface_declaration(region, decl, diagnostics) {
                Ok(ent) => {
                    region.add(ent, diagnostics);
                }
                Err(err) => {
                    err.add_to(diagnostics)?;
                }
            }
        }
        Ok(())
    }

    pub fn analyze_parameter_list(
        &self,
        region: &mut Region<'_>,
        declarations: &mut [InterfaceDeclaration],
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult<FormalRegion> {
        let mut params = FormalRegion::new(InterfaceListType::Parameter);

        for decl in declarations.iter_mut() {
            match self.analyze_interface_declaration(region, decl, diagnostics) {
                Ok(ent) => {
                    region.add(ent.clone(), diagnostics);
                    params.add(ent);
                }
                Err(err) => {
                    err.add_to(diagnostics)?;
                }
            }
        }
        Ok(params)
    }

    fn analyze_array_index(
        &self,
        region: &mut Region<'_>,
        array_index: &mut ArrayIndex,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult<Option<TypeEnt>> {
        match array_index {
            ArrayIndex::IndexSubtypeDefintion(ref mut type_mark) => {
                match self.resolve_type_mark_name(region, type_mark) {
                    Ok(typ) => Ok(Some(typ)),
                    Err(err) => {
                        err.add_to(diagnostics)?;
                        Ok(None)
                    }
                }
            }
            ArrayIndex::Discrete(ref mut drange) => {
                self.analyze_discrete_range(region, drange, diagnostics)?;
                Ok(None)
            }
        }
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

    pub fn resolve_subtype_indication(
        &self,
        region: &Region<'_>,
        subtype_indication: &mut SubtypeIndication,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> AnalysisResult<Subtype> {
        // @TODO more
        let SubtypeIndication {
            type_mark,
            constraint,
            ..
        } = subtype_indication;

        let base_type = self.resolve_type_mark(region, type_mark)?;

        if let Some(constraint) = constraint {
            self.analyze_subtype_constraint(region, &mut constraint.item, diagnostics)?;
        }

        Ok(Subtype::new(base_type))
    }

    pub fn analyze_subtype_indication(
        &self,
        region: &Region<'_>,
        subtype_indication: &mut SubtypeIndication,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        if let Err(err) = self.resolve_subtype_indication(region, subtype_indication, diagnostics) {
            err.add_to(diagnostics)?;
        }
        Ok(())
    }

    fn analyze_subprogram_declaration(
        &self,
        region: &mut Region<'_>,
        subprogram: &mut SubprogramDeclaration,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> AnalysisResult<Signature> {
        match subprogram {
            SubprogramDeclaration::Function(fun) => {
                let params =
                    self.analyze_parameter_list(region, &mut fun.parameter_list, diagnostics);
                let return_type = self.resolve_type_mark_name(region, &mut fun.return_type);
                Ok(Signature::new(params?, Some(return_type?)))
            }
            SubprogramDeclaration::Procedure(procedure) => {
                let params =
                    self.analyze_parameter_list(region, &mut procedure.parameter_list, diagnostics);
                Ok(Signature::new(params?, None))
            }
        }
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

fn signature_error(pos: impl AsRef<SrcPos>) -> Diagnostic {
    Diagnostic::error(
        pos,
        "Alias should only have a signature for subprograms and enum literals",
    )
}

enum ScalarType {
    Integer,
    Real,
}

/// @TODO A simple and incomplete way to disambiguate integer and real in standard.vhd
fn integer_or_real_range(range: &ast::Range) -> ScalarType {
    if let ast::Range::Range(RangeConstraint { left_expr, .. }) = range {
        let expr = if let Expression::Unary(_, ref expr) = left_expr.item {
            &expr.item
        } else {
            &left_expr.item
        };

        if let Expression::Literal(Literal::AbstractLiteral(AbstractLiteral::Real(_))) = expr {
            return ScalarType::Real;
        }
    }

    ScalarType::Integer
}
