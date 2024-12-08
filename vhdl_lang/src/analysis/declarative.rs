// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2019, Olof Kraigher olof.kraigher@gmail.com

use super::names::*;
use super::*;
use crate::ast::token_range::WithTokenSpan;
use crate::ast::*;
use crate::data::error_codes::ErrorCode;
use crate::data::*;
use crate::named_entity::{Signature, *};
use crate::{ast, named_entity, HasTokenSpan};
use analyze::*;
use fnv::FnvHashMap;
use itertools::Itertools;
use std::collections::hash_map::Entry;
use std::collections::HashSet;
use vhdl_lang::TokenSpan;

impl Declaration {
    /// Returns whether the declaration denoted by `self` is allowed in the given context.
    /// For example, within an architecture, only constants, signals and shared variables are allowed,
    /// variables are not.
    ///
    /// ### Conforming example:
    /// ```vhdl
    /// architecture arch of ent is
    ///     signal foo : bit;
    /// begin
    /// end arch;
    /// ```
    ///
    /// ### Non-Conforming example:
    /// ```vhdl
    /// architecture arch of ent is
    ///     variable foo : bit;
    /// begin
    /// end arch;
    /// ```
    ///
    /// The context is given by the parent element of the declaration.
    pub fn is_allowed_in_context(&self, parent: &AnyEntKind<'_>) -> bool {
        use Declaration::*;
        use ObjectClass::*;
        match parent {
            // LRM: block_declarative_item
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
                    | View(_)
            ),
            // LRM: configuration_declarative_item
            AnyEntKind::Design(Design::Configuration) => {
                matches!(self, Use(_) | Attribute(ast::Attribute::Specification(_)))
            }
            // LRM: entity_declarative_item
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
                    | View(_)
            ),
            // LRM: package_body_declarative_item
            AnyEntKind::Design(Design::PackageBody(..) | Design::UninstPackage(..))
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
            // LRM: package_declarative_item
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
                    | View(_)
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

impl<'a> AnalyzeContext<'a, '_> {
    pub fn analyze_declarative_part(
        &self,
        scope: &Scope<'a>,
        parent: EntRef<'a>,
        declarations: &mut [WithTokenSpan<Declaration>],
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        let mut incomplete_types: FnvHashMap<Symbol, (EntRef<'a>, SrcPos)> = FnvHashMap::default();

        for i in 0..declarations.len() {
            let (WithTokenSpan { item: decl, span }, remaining) =
                declarations[i..].split_first_mut().unwrap();

            if !decl.is_allowed_in_context(parent.kind()) {
                diagnostics.add(
                    span.pos(self.ctx),
                    format!("{} declaration not allowed here", decl.describe()),
                    ErrorCode::DeclarationNotAllowed,
                )
            }

            // Handle incomplete types
            match decl {
                Declaration::Type(type_decl) => match type_decl.def {
                    TypeDefinition::Incomplete(ref mut reference) => {
                        match incomplete_types.entry(type_decl.ident.name().clone()) {
                            Entry::Vacant(entry) => {
                                let full_definiton =
                                    find_full_type_definition(type_decl.ident.name(), remaining);

                                let (decl_pos, span) = match full_definiton {
                                    Some(full_decl) => (
                                        self.ctx.get_pos(full_decl.ident.tree.token),
                                        full_decl.span,
                                    ),
                                    None => {
                                        let error = Diagnostic::new(
                                            type_decl.ident.pos(self.ctx),
                                            format!(
                                                "Missing full type declaration of incomplete type '{}'",
                                                type_decl.ident.name()
                                            ),
                                            ErrorCode::MissingFullTypeDeclaration,
                                        ).related(type_decl.ident.pos(self.ctx), "The full type declaration shall occur immediately within the same declarative part");
                                        diagnostics.push(error);
                                        (type_decl.ident.pos(self.ctx), type_decl.span)
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
                                    span,
                                    Some(self.source()),
                                );
                                reference.set_unique_reference(ent);

                                entry.insert((ent, type_decl.ident.pos(self.ctx).clone()));
                                scope.add(ent, diagnostics);
                            }
                            Entry::Occupied(entry) => {
                                let (_, decl_pos) = entry.get();

                                diagnostics.push(Diagnostic::duplicate_error(
                                    &type_decl.ident,
                                    type_decl.ident.pos(self.ctx),
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
        src_span: TokenSpan,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> EvalResult<EntRef<'a>> {
        let AliasDeclaration {
            designator,
            name,
            subtype_indication,
            signature,
            is_token: _,
        } = alias;

        let resolved_name = self.name_resolve(scope, name.span, &mut name.item, diagnostics);

        if let Some(ref mut subtype_indication) = subtype_indication {
            // Object alias
            self.analyze_subtype_indication(scope, subtype_indication, diagnostics)?;
        }

        let resolved_name = resolved_name?;

        let kind = {
            match resolved_name {
                ResolvedName::ObjectName(oname) => {
                    if let Some(ref signature) = signature {
                        diagnostics.push(Diagnostic::should_not_have_signature(
                            "Alias",
                            signature.pos(self.ctx),
                        ));
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
                        diagnostics.push(Diagnostic::should_not_have_signature(
                            "Alias",
                            signature.pos(self.ctx),
                        ));
                    }
                    diagnostics.add(
                        name.pos(self.ctx),
                        format!("{} cannot be aliased", resolved_name.describe_type()),
                        ErrorCode::MismatchedKinds,
                    );
                    return Err(EvalError::Unknown);
                }
                ResolvedName::Type(typ) => {
                    if let Some(ref signature) = signature {
                        diagnostics.push(Diagnostic::should_not_have_signature(
                            "Alias",
                            signature.pos(self.ctx),
                        ));
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
                                des.pos(self.ctx),
                                &des.item,
                                &overloaded,
                            ));
                            return Err(EvalError::Unknown);
                        }
                    } else {
                        diagnostics.push(Diagnostic::signature_required(name.pos(self.ctx)));
                        return Err(EvalError::Unknown);
                    }
                }
                ResolvedName::Final(ent) => {
                    if let Some(ent) = ViewEnt::from_any(ent) {
                        AnyEntKind::View(*ent.subtype())
                    } else {
                        // @TODO some of these can probably be aliased
                        return Err(EvalError::Unknown);
                    }
                }
            }
        };

        Ok(designator.define(
            self.ctx,
            self.arena,
            parent,
            kind,
            src_span,
            Some(self.source()),
        ))
    }

    pub(crate) fn analyze_declaration(
        &self,
        scope: &Scope<'a>,
        parent: EntRef<'a>,
        decl: &mut WithTokenSpan<Declaration>,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        let src_span = decl.span();
        match &mut decl.item {
            Declaration::Alias(alias) => {
                if let Some(ent) = as_fatal(self.analyze_alias_declaration(
                    scope,
                    parent,
                    alias,
                    decl.span,
                    diagnostics,
                ))? {
                    scope.add(ent, diagnostics);

                    for implicit in ent.as_actual().implicits.iter() {
                        match OverloadedEnt::from_any(implicit) {
                            Some(implicit) => {
                                let impicit_alias = self.arena.implicit(
                                    ent,
                                    implicit.designator().clone(),
                                    AnyEntKind::Overloaded(Overloaded::Alias(implicit)),
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
                            expr.span,
                            &mut expr.item,
                            diagnostics,
                        )?;
                    } else {
                        self.expr_unknown_ttyp(scope, expr, diagnostics)?;
                    }
                }

                if let Some(subtype) = as_fatal(subtype)? {
                    for ident in &mut object_decl.idents {
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
                            self.find_deferred_constant_declaration(scope, &ident.tree.item)
                        } else {
                            None
                        };
                        let object_ent = self.arena.alloc(
                            ident.tree.item.clone().into(),
                            Some(parent),
                            if let Some(declared_by) = declared_by {
                                Related::DeclaredBy(declared_by)
                            } else {
                                Related::None
                            },
                            kind,
                            Some(ident.pos(self.ctx).clone()),
                            src_span,
                            Some(self.source()),
                        );
                        ident.decl.set(object_ent.id());

                        scope.add(object_ent, diagnostics);
                    }
                }
            }
            Declaration::File(ref mut file) => {
                let FileDeclaration {
                    idents,
                    colon_token: _,
                    subtype_indication,
                    open_info,
                    file_name,
                } = file;

                let subtype = as_fatal(self.resolve_subtype_indication(
                    scope,
                    subtype_indication,
                    diagnostics,
                ))?;

                if let Some((_, ref mut expr)) = open_info {
                    self.expr_unknown_ttyp(scope, expr, diagnostics)?;
                }
                if let Some((_, ref mut expr)) = file_name {
                    self.expr_unknown_ttyp(scope, expr, diagnostics)?;
                }

                if let Some(subtype) = subtype {
                    for ident in idents {
                        scope.add(
                            self.define(ident, parent, AnyEntKind::File(subtype), src_span),
                            diagnostics,
                        );
                    }
                }
            }
            Declaration::Component(ref mut component) => {
                let nested = scope.nested();
                let ent = self.define(
                    &mut component.ident,
                    parent,
                    AnyEntKind::Component(Region::default()),
                    src_span,
                );
                if let Some(generic_list) = &mut component.generic_list {
                    self.analyze_interface_list(&nested, ent, generic_list, diagnostics)?;
                }
                if let Some(port_list) = &mut component.port_list {
                    self.analyze_interface_list(&nested, ent, port_list, diagnostics)?;
                }

                let kind = AnyEntKind::Component(nested.into_region());
                unsafe {
                    ent.set_kind(kind);
                }

                scope.add(ent, diagnostics);
            }
            Declaration::Attribute(ref mut attr) => match attr {
                Attribute::Declaration(ref mut attr_decl) => {
                    if let Some(typ) = as_fatal(self.type_name(
                        scope,
                        attr_decl.type_mark.span,
                        &mut attr_decl.type_mark.item,
                        diagnostics,
                    ))? {
                        scope.add(
                            self.define(
                                &mut attr_decl.ident,
                                parent,
                                AnyEntKind::Attribute(typ),
                                src_span,
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
                return self.subprogram_body(scope, parent, body, diagnostics);
            }
            Declaration::SubprogramDeclaration(ref mut subdecl) => {
                match as_fatal(self.subprogram_specification(
                    scope,
                    parent,
                    &mut subdecl.specification,
                    subdecl.span,
                    Overloaded::SubprogramDecl,
                    diagnostics,
                ))? {
                    Some((_, ent)) => {
                        scope.add(ent, diagnostics);
                    }
                    None => {
                        return Ok(());
                    }
                }
            }
            Declaration::SubprogramInstantiation(ref mut instance) => {
                let subpgm_ent = self.define(
                    &mut instance.ident,
                    parent,
                    AnyEntKind::Overloaded(Overloaded::Subprogram(Signature::new(
                        FormalRegion::new_params(),
                        None,
                    ))),
                    src_span,
                );
                let referenced_name = &mut instance.subprogram_name;
                if let Some(name) = as_fatal(self.name_resolve(
                    scope,
                    referenced_name.span,
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
                let ent = self.define(
                    &mut instance.ident,
                    parent,
                    AnyEntKind::Design(Design::PackageInstance(Region::default())),
                    src_span,
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
            Declaration::View(view) => {
                if let Some(view) = as_fatal(self.analyze_view_declaration(
                    scope,
                    parent,
                    view,
                    decl.span,
                    diagnostics,
                ))? {
                    scope.add(view, diagnostics);
                }
            }
            Declaration::Type(..) => unreachable!("Handled elsewhere"),
        };

        Ok(())
    }

    /// Analyzes a mode view declaration.
    /// * Checks that the type of the view declaration is a record type
    /// * Checks that all elements are associated in the view
    fn analyze_view_declaration(
        &self,
        scope: &Scope<'a>,
        parent: EntRef<'a>,
        view: &mut ModeViewDeclaration,
        src_span: TokenSpan,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> EvalResult<EntRef<'a>> {
        let typ = self.resolve_subtype_indication(scope, &mut view.typ, diagnostics)?;
        let record_region = match typ.type_mark().kind() {
            Type::Record(region) => region,
            _ => {
                let diag = Diagnostic::new(
                    view.typ.type_mark.pos(self.ctx),
                    format!(
                        "The type of a view must be a record type, not {}",
                        typ.type_mark().describe()
                    ),
                    ErrorCode::TypeMismatch,
                )
                .opt_related(
                    typ.type_mark().decl_pos.as_ref(),
                    format!("{} declared here", typ.type_mark().describe()),
                );
                bail!(diagnostics, diag);
            }
        };
        let mut unassociated: HashSet<_> = record_region.elems.iter().collect();
        for element in view.elements.iter_mut() {
            for name in element.names.iter_mut() {
                let desi = Designator::Identifier(name.tree.item.clone());
                let Some(record_element) = record_region.lookup(&desi) else {
                    diagnostics.push(Diagnostic::new(
                        name.pos(self.ctx),
                        format!("Not a part of {}", typ.type_mark().describe()),
                        ErrorCode::Unresolved,
                    ));
                    continue;
                };
                name.decl.set_unique_reference(&record_element);
                unassociated.remove(&record_element);
            }
        }
        if !unassociated.is_empty() {
            diagnostics.add(
                view.ident.pos(self.ctx),
                pretty_format_unassociated_message(&unassociated),
                ErrorCode::Unassociated,
            );
        }
        Ok(self.define(&mut view.ident, parent, AnyEntKind::View(typ), src_span))
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
            colon_token: _,
        } = attr_spec;

        let attr_ent = match scope.lookup(&Designator::Identifier(ident.item.name().clone())) {
            Ok(NamedEntities::Single(ent)) => {
                ident.set_unique_reference(ent);
                if let Some(attr_ent) = AttributeEnt::from_any(ent) {
                    self.expr_pos_with_ttyp(
                        scope,
                        attr_ent.typ(),
                        expr.span,
                        &mut expr.item,
                        diagnostics,
                    )?;
                    attr_ent
                } else {
                    diagnostics.add(
                        ident.item.pos(self.ctx),
                        format!("{} is not an attribute", ent.describe()),
                        ErrorCode::MismatchedKinds,
                    );
                    return Ok(());
                }
            }
            Ok(NamedEntities::Overloaded(_)) => {
                diagnostics.add(
                    ident.item.pos(self.ctx),
                    format!("Overloaded name '{}' is not an attribute", ident.item),
                    ErrorCode::MismatchedKinds,
                );
                return Ok(());
            }
            Err(err) => {
                diagnostics.push(err.into_diagnostic(self.ctx, ident.item.token));
                return Ok(());
            }
        };

        if let EntityName::Name(EntityTag {
            designator,
            signature,
        }) = entity_name
        {
            let ent: EntRef<'_> = match scope.lookup(&designator.item.item) {
                Ok(NamedEntities::Single(ent)) => {
                    designator.set_unique_reference(ent);

                    if let Some(signature) = signature {
                        diagnostics.push(Diagnostic::should_not_have_signature(
                            "Attribute specification",
                            signature.pos(self.ctx),
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
                                        designator.pos(self.ctx),
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
                        diagnostics.push(Diagnostic::signature_required(designator.pos(self.ctx)));
                        return Ok(());
                    }
                }
                Err(err) => {
                    diagnostics.push(err.into_diagnostic(self.ctx, designator.token));
                    return Ok(());
                }
            };

            // Attributes affect the underlying entity and cannot be set directly on aliases
            let ent = ent.as_actual();

            if Some(*entity_class) != get_entity_class(ent) {
                diagnostics.add(
                    designator.pos(self.ctx),
                    format!("{} is not of class {}", ent.describe(), entity_class),
                    ErrorCode::MismatchedEntityClass,
                );
                return Ok(());
            }

            match entity_class {
                EntityClass::Architecture
                | EntityClass::Entity
                | EntityClass::Package
                | EntityClass::Configuration => {
                    if ent != parent {
                        diagnostics.add(
                            designator.pos(self.ctx),
                            "Attribute specification must be in the immediate declarative part",
                            ErrorCode::MisplacedAttributeSpec,
                        );
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
                        diagnostics.add(
                            designator.pos(self.ctx),
                            "Attribute specification must be in the immediate declarative part",
                            ErrorCode::MisplacedAttributeSpec,
                        );
                        return Ok(());
                    }
                }
            }

            let res = unsafe {
                self.arena
                    .add_attr(ent.id(), designator.pos(self.ctx), attr_ent)
            };

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
    ) -> EvalResult<Vec<EntRef<'a>>> {
        let span = decl.span();
        let ent = match decl {
            InterfaceDeclaration::File(ref mut file_decl) => {
                let file_type = self.resolve_subtype_indication(
                    scope,
                    &mut file_decl.subtype_indication,
                    diagnostics,
                )?;
                file_decl
                    .idents
                    .iter_mut()
                    .map(|ident| {
                        self.define(
                            ident,
                            parent,
                            AnyEntKind::InterfaceFile(file_type.type_mark().to_owned()),
                            span,
                        )
                    })
                    .collect()
            }
            InterfaceDeclaration::Object(ref mut object_decl) => {
                self.analyze_interface_object_declaration(scope, parent, object_decl, diagnostics)?
            }
            InterfaceDeclaration::Type(ref mut ident) => {
                let typ = TypeEnt::from_any(self.define(
                    ident,
                    parent,
                    AnyEntKind::Type(Type::Interface),
                    span,
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

                vec![typ.into()]
            }
            InterfaceDeclaration::Subprogram(ref mut subpgm) => {
                let (_, ent) = self.subprogram_specification(
                    scope,
                    parent,
                    &mut subpgm.specification,
                    subpgm.span,
                    Overloaded::InterfaceSubprogram,
                    diagnostics,
                )?;
                vec![ent]
            }
            InterfaceDeclaration::Package(ref mut instance) => {
                let package_region = self.analyze_package_instance_name(
                    scope,
                    &mut instance.package_name,
                    diagnostics,
                )?;

                vec![self.define(
                    &mut instance.ident,
                    parent,
                    AnyEntKind::Design(Design::InterfacePackageInstance(package_region)),
                    span,
                )]
            }
        };
        Ok(ent)
    }

    pub fn analyze_interface_object_declaration(
        &self,
        scope: &Scope<'a>,
        parent: EntRef<'a>,
        object_decl: &mut InterfaceObjectDeclaration,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> EvalResult<Vec<EntRef<'a>>> {
        let objects = object_decl
            .idents
            .iter_mut()
            .map(|ident| {
                self.define(
                    ident,
                    parent,
                    AnyEntKind::Object(Object {
                        class: ObjectClass::Signal,
                        iface: None,
                        subtype: Subtype::new(self.universal_integer().into()),
                        has_default: false,
                    }),
                    object_decl.span,
                )
            })
            .collect_vec();
        for object in &objects {
            let actual_object = match &mut object_decl.mode {
                ModeIndication::Simple(mode) => {
                    let (subtype, class) =
                        self.analyze_simple_mode_indication(scope, mode, diagnostics)?;
                    Object {
                        class,
                        iface: Some(ObjectInterface::simple(
                            object_decl.list_type,
                            mode.mode.as_ref().map(|mode| mode.item).unwrap_or_default(),
                        )),
                        subtype,
                        has_default: mode.expression.is_some(),
                    }
                }
                ModeIndication::View(view) => {
                    let (view_ent, subtype) =
                        self.analyze_mode_indication(scope, object, view, diagnostics)?;
                    Object {
                        class: ObjectClass::Signal,
                        iface: Some(ObjectInterface::Port(InterfaceMode::View(view_ent))),
                        subtype,
                        has_default: false,
                    }
                }
            };
            unsafe {
                object.set_kind(AnyEntKind::Object(actual_object));
            }
        }

        Ok(objects)
    }

    /// Analyzes a mode view indication of the form
    /// ```vhdl
    /// foo : view s_axis of axi_stream
    /// ```
    ///
    /// This function resolves all used types and verifies them.
    /// If the provided view describes an array but no actual array type is given, i.e.:
    /// ```vhdl
    /// multiple_foos : view (s_axis)
    /// ```
    /// this function will declare an anonymous array indication with a single index and the
    /// view's subtype as element, similar as if the view was declared like so:
    /// ```vhdl
    /// -- pseudo code
    /// type anonymous is array (integer range <>) of axi_stream;
    /// multiple_foos : view (s_axis) of anonymous
    /// ```
    /// The anonymous array type will be marked as being linked to the interface declaration
    /// (in the example above: the `anonymous` type is implicitly declared by `multiple_foos`)
    fn analyze_mode_indication(
        &self,
        scope: &Scope<'a>,
        object_ent: EntRef<'a>,
        view: &mut ModeViewIndication,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> EvalResult<(ViewEnt<'a>, Subtype<'a>)> {
        let resolved =
            self.name_resolve(scope, view.name.span, &mut view.name.item, diagnostics)?;
        let view_ent = self.resolve_view_ent(&resolved, diagnostics, view.name.span)?;
        let subtype = if let Some((_, ast_declared_subtype)) = &mut view.subtype_indication {
            let declared_subtype =
                self.resolve_subtype_indication(scope, ast_declared_subtype, diagnostics)?;
            match view.kind {
                ModeViewIndicationKind::Array => {
                    let Type::Array {
                        indexes: _,
                        elem_type,
                    } = declared_subtype.type_mark().kind()
                    else {
                        bail!(
                            diagnostics,
                            Diagnostic::new(
                                ast_declared_subtype.type_mark.pos(self.ctx),
                                "Subtype must be an array",
                                ErrorCode::TypeMismatch
                            )
                        );
                    };
                    if *elem_type != view_ent.subtype().type_mark() {
                        bail!(
                            diagnostics,
                            Diagnostic::new(
                                ast_declared_subtype.type_mark.pos(self.ctx),
                                format!(
                                    "Array element {} must match {} declared for the view",
                                    elem_type.describe(),
                                    view_ent.subtype().type_mark().describe()
                                ),
                                ErrorCode::TypeMismatch
                            )
                        );
                    }
                }
                ModeViewIndicationKind::Record => {
                    if declared_subtype.type_mark() != view_ent.subtype().type_mark() {
                        bail!(
                            diagnostics,
                            Diagnostic::new(
                                ast_declared_subtype.type_mark.pos(self.ctx),
                                "Specified subtype must match the subtype declared for the view",
                                ErrorCode::TypeMismatch
                            )
                        );
                    }
                }
            }
            declared_subtype
        } else {
            match view.kind {
                ModeViewIndicationKind::Array => {
                    let typ = Type::Array {
                        indexes: vec![Some(self.universal_integer())],
                        elem_type: view_ent.subtype().type_mark(),
                    };
                    let typ = self.arena.implicit(
                        object_ent,
                        scope.anonymous_designator(),
                        AnyEntKind::Type(typ),
                    );
                    Subtype::new(TypeEnt::from_any(typ).unwrap())
                }
                ModeViewIndicationKind::Record => *view_ent.subtype(),
            }
        };
        Ok((view_ent, subtype))
    }

    pub fn analyze_simple_mode_indication(
        &self,
        scope: &Scope<'a>,
        mode: &mut SimpleModeIndication,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> EvalResult<(Subtype<'a>, ObjectClass)> {
        let subtype =
            self.resolve_subtype_indication(scope, &mut mode.subtype_indication, diagnostics);

        if let Some(ref mut expression) = mode.expression {
            if let Ok(ref subtype) = subtype {
                self.expr_pos_with_ttyp(
                    scope,
                    subtype.type_mark(),
                    expression.span,
                    &mut expression.item,
                    diagnostics,
                )?;
            } else {
                self.expr_unknown_ttyp(scope, expression, diagnostics)?
            }
        }

        Ok((subtype?, mode.class))
    }

    pub fn analyze_interface_list(
        &self,
        scope: &Scope<'a>,
        parent: EntRef<'a>,
        interface_list: &mut InterfaceList,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult<FormalRegion<'a>> {
        let mut params = FormalRegion::new(interface_list.interface_type);

        for decl in interface_list.items.iter_mut() {
            if let Some(ents) =
                as_fatal(self.analyze_interface_declaration(scope, parent, decl, diagnostics))?
            {
                for ent in ents {
                    scope.add(ent, diagnostics);
                    params.add(ent);
                }
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
                .type_name(scope, type_mark.span, &mut type_mark.item, diagnostics)
                .map(|typ| typ.base()),
            ArrayIndex::Discrete(ref mut drange) => {
                self.drange_type(scope, &mut drange.item, diagnostics)
            }
        }
    }
}

impl Diagnostic {
    fn no_overloaded_with_signature(
        pos: &SrcPos,
        des: &Designator,
        overloaded: &OverloadedName<'_>,
    ) -> Diagnostic {
        let mut diagnostic = Diagnostic::new(
            pos,
            format!(
                "Could not find declaration of {} with given signature",
                des.describe()
            ),
            ErrorCode::NoOverloadedWithSignature,
        );
        diagnostic.add_subprogram_candidates("Found", overloaded.entities());
        diagnostic
    }

    fn should_not_have_signature(prefix: &str, pos: impl AsRef<SrcPos>) -> Diagnostic {
        Diagnostic::new(
            pos,
            format!("{prefix} should only have a signature for subprograms and enum literals"),
            ErrorCode::IllegalSignature,
        )
    }

    fn signature_required(pos: impl AsRef<SrcPos>) -> Diagnostic {
        Diagnostic::new(
            pos,
            "Signature required for alias of subprogram and enum literals",
            ErrorCode::SignatureRequired,
        )
    }
}

fn get_entity_class(ent: EntRef<'_>) -> Option<EntityClass> {
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
            Design::Architecture(..) => Some(EntityClass::Architecture),
            Design::Configuration => Some(EntityClass::Configuration),
            Design::Package(_, _) => Some(EntityClass::Package),
            // Should never be target of attribute
            Design::PackageBody(..) => None,
            Design::UninstPackage(_, _) => None,
            Design::PackageInstance(_) => None,
            Design::InterfacePackageInstance(_) => None,
            Design::Context(_) => None,
        },
        AnyEntKind::View(_) => None,
    }
}

fn find_full_type_definition<'a>(
    name: &Symbol,
    decls: &'a [WithTokenSpan<Declaration>],
) -> Option<&'a TypeDeclaration> {
    for decl in decls.iter() {
        if let Declaration::Type(type_decl) = &decl.item {
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

const UNASSOCIATED_DISPLAY_THRESHOLD: usize = 3;

/// Pretty formats a hash set with unassociated record elements.
/// This is for an improved user experience.
/// The returned message has the format "Missing association of x".
/// * If there is only one element, the message becomes "Missing association of element the_element"
/// * If there are more elements, the message becomes
///     "Missing association of element the_element1, the_element2 and the_element3"
/// * If there are more elements than [UNASSOCIATED_DISPLAY_THRESHOLD], the message will be truncated
///     to "Missing association of element the_element1, the_element2, the_element3 and 17 more"
fn pretty_format_unassociated_message(unassociated: &HashSet<&RecordElement<'_>>) -> String {
    assert!(
        !unassociated.is_empty(),
        "Should never be called with an empty set"
    );
    let mut as_string_vec = unassociated
        .iter()
        .sorted_by_key(|el| el.decl_pos())
        .map(|el| el.designator().describe())
        .collect_vec();
    let description = if as_string_vec.len() == 1 {
        as_string_vec.pop().unwrap()
    } else if as_string_vec.len() > UNASSOCIATED_DISPLAY_THRESHOLD {
        let mut desc = as_string_vec[..UNASSOCIATED_DISPLAY_THRESHOLD].join(", ");
        desc.push_str(" and ");
        desc.push_str(&(as_string_vec.len() - UNASSOCIATED_DISPLAY_THRESHOLD).to_string());
        desc.push_str(" more");
        desc
    } else {
        // len > 1, therefore we always have a last element
        let last = as_string_vec.pop().unwrap();
        let mut desc = as_string_vec.join(", ");
        desc.push_str(" and ");
        desc.push_str(&last);
        desc
    };
    if unassociated.len() == 1 {
        format!("Missing association of element {}", description)
    } else {
        format!("Missing association of elements {}", description)
    }
}
