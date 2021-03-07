// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2019, Olof Kraigher olof.kraigher@gmail.com

use super::*;
use crate::ast;
use crate::ast::*;
use crate::data::*;
use analyze::*;
use arc_swap::ArcSwapWeak;
use fnv::FnvHashMap;
use named_entity::Signature;
use region::*;
use std::sync::Arc;
use std::{collections::hash_map::Entry, sync::Weak};

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
                                    NamedEntityKind::IncompleteType(ArcSwapWeak::new(Weak::new())),
                                    Some(decl_pos),
                                ));
                                reference.set_unique_reference(&ent);

                                entry.insert((ent.clone(), type_decl.ident.pos().clone()));
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
                                if let NamedEntityKind::IncompleteType(full_ref) =
                                    incomplete_type.kind()
                                {
                                    full_ref.store(Arc::downgrade(full_type));
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

        let resolved_name = self.resolve_name(region, &name.pos, &mut name.item, diagnostics)?;

        if let Some(ref mut subtype_indication) = subtype_indication {
            // Object alias
            self.analyze_subtype_indication(region, subtype_indication, diagnostics)?;
        }

        let kind = {
            if subtype_indication.is_some() {
                NamedEntityKind::OtherAlias
            } else if let Some(named_entities) = resolved_name {
                let ent = match named_entities {
                    NamedEntities::Single(ent) => {
                        if let Some(ref signature) = signature {
                            diagnostics.error(signature, "Alias should only have a signature for subprograms and enum literals");
                        }
                        ent
                    }
                    NamedEntities::Overloaded(overloaded) => {
                        if let Some(ref mut signature) = signature {
                            match self.resolve_signature(region, signature) {
                                Ok(signature_key) => {
                                    if let Some(ent) = overloaded.get(&signature_key) {
                                        if let Some(reference) = name.item.suffix_reference_mut() {
                                            reference.set_unique_reference(&ent);
                                        }
                                        ent
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
                };
                NamedEntityKind::AliasOf(ent)
            } else {
                // Found but not known, likely some kind of sliced or indexed name
                NamedEntityKind::OtherAlias
            }
        };

        Ok(Some(Arc::new(NamedEntity::new(
            designator.item.clone(),
            kind,
            Some(&designator.pos),
        ))))
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
                    region.add_named_entity(ent.clone(), diagnostics);
                    region.add_implicit_declaration_aliases(&ent, diagnostics);
                }
            }
            Declaration::Object(ref mut object_decl) => {
                let subtype = self.resolve_subtype_indication(
                    region,
                    &mut object_decl.subtype_indication,
                    diagnostics,
                );

                if let Some(ref mut expr) = object_decl.expression {
                    self.analyze_expression(region, expr, diagnostics)?;
                }

                match subtype {
                    Ok(subtype) => {
                        let ent = if object_decl.class == ObjectClass::Constant
                            && object_decl.expression.is_none()
                        {
                            NamedEntityKind::DeferredConstant
                        } else {
                            NamedEntityKind::Object(Object {
                                class: object_decl.class,
                                mode: None,
                                has_default: object_decl.expression.is_some(),
                                subtype,
                            })
                        };
                        region.add(&object_decl.ident, ent, diagnostics);
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
                        let kind = NamedEntityKind::Subprogram(signature);
                        let designator = body.specification.designator();
                        let subpgm_ent =
                            NamedEntity::new(designator.item, kind, Some(&designator.pos));
                        region.add_named_entity(Arc::new(subpgm_ent), diagnostics);
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
                            subdecl.designator(),
                            NamedEntityKind::SubprogramDecl(signature),
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
                let enum_type = Arc::new(NamedEntity::new_with_opt_id(
                    overwrite_id,
                    type_decl.ident.name().clone(),
                    NamedEntityKind::TypeDeclaration(Vec::new()),
                    Some(&type_decl.ident.pos),
                ));

                let signature = Signature::new(ParameterList::default(), Some(enum_type.clone()));

                let mut implicit = Vec::with_capacity(enumeration.len());

                for literal in enumeration.iter() {
                    let literal_ent = NamedEntity::new(
                        literal.item.clone().into_designator(),
                        NamedEntityKind::EnumLiteral(signature.clone()),
                        Some(&literal.pos),
                    );
                    let literal_ent = Arc::new(literal_ent);
                    implicit.push(Arc::downgrade(&literal_ent));
                    parent.add_named_entity(literal_ent, diagnostics);
                }

                // Overwrite enum type with one that contains the implicit declarations
                // @TODO investigate get_mut_unchecked to change the original enum_type
                //       this will create a new struct instance and thus the signature of
                //       the enum literals will not contain the full type declaration of the
                //       enum type
                parent.add_named_entity(
                    Arc::new(enum_type.clone_with_kind(NamedEntityKind::TypeDeclaration(implicit))),
                    diagnostics,
                );
            }
            TypeDefinition::ProtectedBody(ref mut body) => {
                body.type_reference.clear_reference();

                match parent.lookup_immediate(&type_decl.ident.item.clone().into()) {
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
                let ptype = Arc::new(NamedEntity::new_with_opt_id(
                    overwrite_id,
                    type_decl.ident.name().clone(),
                    NamedEntityKind::TypeDeclaration(Vec::new()),
                    Some(&type_decl.ident.pos),
                ));
                parent.add_named_entity(ptype.clone(), diagnostics);

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
                                        subprogram.designator(),
                                        NamedEntityKind::SubprogramDecl(signature),
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
                let region = region.without_parent();

                parent.add_named_entity(
                    Arc::new(
                        ptype.clone_with_kind(NamedEntityKind::ProtectedType(Arc::new(region))),
                    ),
                    &mut Vec::new(),
                );
            }
            TypeDefinition::Record(ref mut element_decls) => {
                let mut region = Region::default();
                for elem_decl in element_decls.iter_mut() {
                    let subtype = self.resolve_subtype_indication(
                        parent,
                        &mut elem_decl.subtype,
                        diagnostics,
                    );
                    match subtype {
                        Ok(subtype) => {
                            region.add(
                                &elem_decl.ident,
                                NamedEntityKind::ElementDeclaration(subtype),
                                diagnostics,
                            );
                        }
                        Err(err) => {
                            err.add_to(diagnostics)?;
                        }
                    }
                }
                region.close(diagnostics);

                let type_ent = Arc::new(NamedEntity::new_with_opt_id(
                    overwrite_id,
                    type_decl.ident.name().clone(),
                    NamedEntityKind::RecordType(Arc::new(region)),
                    Some(&type_decl.ident.pos),
                ));
                parent.add_named_entity(type_ent, diagnostics);
            }
            TypeDefinition::Access(ref mut subtype_indication) => {
                let subtype =
                    self.resolve_subtype_indication(parent, subtype_indication, diagnostics);
                match subtype {
                    Ok(subtype) => {
                        let type_ent = Arc::new(NamedEntity::new_with_opt_id(
                            overwrite_id,
                            type_decl.ident.name().clone(),
                            NamedEntityKind::AccessType(subtype),
                            Some(&type_decl.ident.pos),
                        ));
                        parent.add_named_entity(type_ent, diagnostics);
                    }
                    Err(err) => err.add_to(diagnostics)?,
                }
            }
            TypeDefinition::Array(ref mut array_indexes, ref mut subtype_indication) => {
                for index in array_indexes.iter_mut() {
                    self.analyze_array_index(parent, index, diagnostics)?;
                }
                self.analyze_subtype_indication(parent, subtype_indication, diagnostics)?;

                let type_ent = Arc::new(NamedEntity::new_with_opt_id(
                    overwrite_id,
                    type_decl.ident.name().clone(),
                    NamedEntityKind::TypeDeclaration(Vec::new()),
                    Some(&type_decl.ident.pos),
                ));

                let mut implicit = Vec::new();
                if !self.is_standard_package() {
                    // @TODO analyze standard package separately
                    let to_string = Arc::new(self.create_to_string(type_ent.clone()));
                    parent.add_named_entity(to_string.clone(), diagnostics);
                    implicit.push(Arc::downgrade(&to_string));
                }
                parent.add_named_entity(
                    Arc::new(type_ent.clone_with_kind(NamedEntityKind::TypeDeclaration(implicit))),
                    diagnostics,
                );
            }
            TypeDefinition::Subtype(ref mut subtype_indication) => {
                match self.resolve_subtype_indication(parent, subtype_indication, diagnostics) {
                    Ok(subtype) => {
                        let type_ent = Arc::new(NamedEntity::new_with_opt_id(
                            overwrite_id,
                            type_decl.ident.name().clone(),
                            NamedEntityKind::Subtype(subtype),
                            Some(&type_decl.ident.pos),
                        ));
                        parent.add_named_entity(type_ent, diagnostics);
                    }
                    Err(err) => {
                        err.add_to(diagnostics)?;
                    }
                }
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
                    NamedEntityKind::TypeDeclaration(Vec::new()),
                    overwrite_id,
                    diagnostics,
                );
            }
            TypeDefinition::Incomplete(..) => {
                unreachable!("Handled elsewhere");
            }

            TypeDefinition::Integer(ref mut range) => {
                self.analyze_range(parent, range, diagnostics)?;
                let type_ent = Arc::new(NamedEntity::new_with_opt_id(
                    overwrite_id,
                    type_decl.ident.name().clone(),
                    NamedEntityKind::TypeDeclaration(Vec::new()),
                    Some(&type_decl.ident.pos),
                ));

                let mut implicit = Vec::new();
                if !self.is_standard_package() {
                    // @TODO analyze standard package separately
                    let to_string = Arc::new(self.create_to_string(type_ent.clone()));
                    parent.add_named_entity(to_string.clone(), diagnostics);
                    implicit.push(Arc::downgrade(&to_string));
                }
                parent.add_named_entity(
                    Arc::new(type_ent.clone_with_kind(NamedEntityKind::TypeDeclaration(implicit))),
                    diagnostics,
                );
            }

            TypeDefinition::File(ref mut type_mark) => {
                if let Err(err) = self.resolve_type_mark(parent, type_mark) {
                    err.add_to(diagnostics)?;
                }

                let file_type = Arc::new(NamedEntity::new_with_opt_id(
                    overwrite_id,
                    type_decl.ident.name().clone(),
                    NamedEntityKind::TypeDeclaration(Vec::new()),
                    Some(&type_decl.ident.pos),
                ));

                let implicit = self.create_implicit_file_type_subprograms(file_type.clone());

                for ent in implicit.iter() {
                    parent.add_named_entity(ent.clone(), diagnostics);
                }

                // We need to overwrite the type due to circular pointer relations between implicit subprograms
                // and type declarations
                let implicit = implicit.iter().map(|ent| Arc::downgrade(ent)).collect();
                let file_type =
                    file_type.clone_with_kind(NamedEntityKind::TypeDeclaration(implicit));
                parent.add_named_entity(Arc::new(file_type), diagnostics);
            }
        }

        Ok(())
    }

    pub fn create_implicit_file_type_subprograms(
        &self,
        file_type: Arc<NamedEntity>,
    ) -> Vec<Arc<NamedEntity>> {
        let mut implicit = Vec::new();

        let standard = self.expect_standard_package_analysis().unwrap();
        let standard_region = &standard.result().region;

        let string = standard_region
            .lookup_immediate(&self.symbol_utf8("STRING").into())
            .unwrap()
            .clone()
            .into_non_overloaded()
            .unwrap();

        let boolean = standard_region
            .lookup_immediate(&self.symbol_utf8("BOOLEAN").into())
            .unwrap()
            .clone()
            .into_non_overloaded()
            .unwrap();

        // procedure FILE_OPEN (file F: FT; External_Name: in STRING);
        {
            let mut params = ParameterList::default();
            params.add_param(Arc::new(NamedEntity::new(
                self.symbol_utf8("F"),
                NamedEntityKind::InterfaceFile(file_type.clone()),
                file_type.decl_pos(),
            )));
            params.add_param(Arc::new(NamedEntity::new(
                self.symbol_utf8("External_Name"),
                NamedEntityKind::Object(Object {
                    class: ObjectClass::Constant,
                    mode: Some(Mode::In),
                    subtype: Subtype::new(string),
                    has_default: false,
                }),
                file_type.decl_pos(),
            )));

            implicit.push(Arc::new(NamedEntity::implicit(
                self.symbol_utf8("FILE_OPEN"),
                NamedEntityKind::Subprogram(Signature::new(params, None)),
                file_type.decl_pos(),
            )));
        }

        // procedure FILE_CLOSE (file F: FT);
        {
            let mut params = ParameterList::default();
            params.add_param(Arc::new(NamedEntity::new(
                self.symbol_utf8("F"),
                NamedEntityKind::InterfaceFile(file_type.clone()),
                file_type.decl_pos(),
            )));

            implicit.push(Arc::new(NamedEntity::implicit(
                self.symbol_utf8("FILE_CLOSE"),
                NamedEntityKind::Subprogram(Signature::new(params, None)),
                file_type.decl_pos(),
            )));
        }

        // function ENDFILE (file F: FT) return BOOLEAN;
        {
            let mut params = ParameterList::default();
            params.add_param(Arc::new(NamedEntity::new(
                self.symbol_utf8("F"),
                NamedEntityKind::InterfaceFile(file_type.clone()),
                file_type.decl_pos(),
            )));

            implicit.push(Arc::new(NamedEntity::implicit(
                self.symbol_utf8("ENDFILE"),
                NamedEntityKind::Subprogram(Signature::new(params, Some(boolean))),
                file_type.decl_pos(),
            )));
        }

        implicit
    }

    /// Create implicit TO_STRING
    /// function TO_STRING (VALUE: T) return STRING;
    pub fn create_to_string(&self, type_ent: Arc<NamedEntity>) -> NamedEntity {
        let standard = self.expect_standard_package_analysis().unwrap();
        let region = &standard.result().region;

        let string = region
            .lookup_immediate(&self.symbol_utf8("STRING").into())
            .unwrap()
            .clone()
            .into_non_overloaded()
            .unwrap();

        let mut params = ParameterList::default();
        params.add_param(Arc::new(NamedEntity::new(
            self.symbol_utf8("VALUE"),
            NamedEntityKind::Object(Object {
                class: ObjectClass::Constant,
                mode: Some(Mode::In),
                subtype: Subtype::new(type_ent.clone()),
                has_default: false,
            }),
            type_ent.decl_pos(),
        )));

        NamedEntity::implicit(
            self.symbol_utf8("TO_STRING"),
            NamedEntityKind::Subprogram(Signature::new(params, Some(string))),
            type_ent.decl_pos(),
        )
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
                    .map(|arg| self.resolve_type_mark(region, arg))
                    .collect();
                let return_type = self.resolve_type_mark(region, ret);
                (args, Some(return_type))
            }
            ast::Signature::Procedure(args) => {
                let args: Vec<_> = args
                    .iter_mut()
                    .map(|arg| self.resolve_type_mark(region, arg))
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
    ) -> AnalysisResult<NamedEntity> {
        let ent = match decl {
            InterfaceDeclaration::File(ref mut file_decl) => {
                let file_type = self.resolve_subtype_indication(
                    region,
                    &mut file_decl.subtype_indication,
                    diagnostics,
                )?;
                NamedEntity::new(
                    file_decl.ident.name().clone(),
                    NamedEntityKind::InterfaceFile(file_type.base().clone()),
                    Some(&file_decl.ident.pos),
                )
            }
            InterfaceDeclaration::Object(ref mut object_decl) => {
                let subtype = self.resolve_subtype_indication(
                    region,
                    &mut object_decl.subtype_indication,
                    diagnostics,
                );

                if let Some(ref mut expression) = object_decl.expression {
                    self.analyze_expression(region, expression, diagnostics)?
                }

                let subtype = subtype?;

                NamedEntity::new(
                    object_decl.ident.name().clone(),
                    NamedEntityKind::Object(Object {
                        class: object_decl.class,
                        mode: Some(object_decl.mode),
                        subtype,
                        has_default: object_decl.expression.is_some(),
                    }),
                    Some(&object_decl.ident.pos),
                )
            }
            InterfaceDeclaration::Type(ref ident) => NamedEntity::new(
                ident.name().clone(),
                NamedEntityKind::InterfaceType,
                Some(&ident.pos),
            ),
            InterfaceDeclaration::Subprogram(ref mut subpgm, ..) => {
                let mut subpgm_region = region.nested();
                let signature =
                    self.analyze_subprogram_declaration(&mut subpgm_region, subpgm, diagnostics);
                subpgm_region.close(diagnostics);
                drop(subpgm_region);

                let kind = NamedEntityKind::Subprogram(signature?);

                let designator = subpgm.designator();
                NamedEntity::new(designator.item, kind, Some(&designator.pos))
            }
            InterfaceDeclaration::Package(ref mut instance) => {
                let package_region =
                    self.analyze_package_instance_name(region, &mut instance.package_name)?;

                NamedEntity::new(
                    instance.ident.name().clone(),
                    NamedEntityKind::LocalPackageInstance(package_region.clone()),
                    Some(&instance.ident.pos),
                )
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
                    let ent = Arc::new(ent);
                    region.add_named_entity(ent.clone(), diagnostics);
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
    ) -> FatalResult<ParameterList> {
        let mut params = ParameterList::default();

        for decl in declarations.iter_mut() {
            match self.analyze_interface_declaration(region, decl, diagnostics) {
                Ok(ent) => {
                    let ent = Arc::new(ent);
                    region.add_named_entity(ent.clone(), diagnostics);
                    params.add_param(ent);
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
                let return_type = self.resolve_type_mark(region, &mut fun.return_type);
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

fn add_or_overwrite(
    region: &mut Region,
    name: &Ident,
    kind: NamedEntityKind,
    old_id: Option<EntityId>,
    diagnostics: &mut dyn DiagnosticHandler,
) -> Arc<NamedEntity> {
    let ent = Arc::new(NamedEntity::new_with_opt_id(
        old_id,
        name.name().clone(),
        kind,
        Some(&name.pos),
    ));
    region.add_named_entity(ent.clone(), diagnostics);
    ent
}
