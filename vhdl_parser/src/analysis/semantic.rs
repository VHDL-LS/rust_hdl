// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use super::declarative_region::{AnyDeclaration, DeclarativeRegion, VisibleDeclaration};
use super::library::{
    AnalysisUnit, Context, DesignRoot, EntityDesignUnit, Library, LockedUnit, PackageDesignUnit,
};
use crate::ast::{HasIdent, *};
use crate::diagnostic::{Diagnostic, DiagnosticHandler};
use crate::latin_1::Latin1String;
use crate::source::{SrcPos, WithPos};
use crate::symbol_table::{Symbol, SymbolTable};
use std::sync::Arc;

use super::lock::{AnalysisEntry, AnalysisLock, CircularDependencyError, ReadGuard};

enum AnalysisError {
    Fatal(CircularDependencyError),
    NotFatal(Diagnostic),
}

type AnalysisResult<T> = Result<T, AnalysisError>;
type FatalResult<T> = Result<T, CircularDependencyError>;
type FatalNullResult = FatalResult<()>;

impl From<CircularDependencyError> for AnalysisError {
    fn from(err: CircularDependencyError) -> AnalysisError {
        AnalysisError::Fatal(err)
    }
}

impl From<Diagnostic> for AnalysisError {
    fn from(diagnostic: Diagnostic) -> AnalysisError {
        AnalysisError::NotFatal(diagnostic)
    }
}

impl AnalysisError {
    // Add Non-fatal error to diagnostics or return fatal error
    fn add_to(self, diagnostics: &mut dyn DiagnosticHandler) -> FatalNullResult {
        match self {
            AnalysisError::Fatal(err) => Err(err),
            AnalysisError::NotFatal(diag) => {
                diagnostics.push(diag);
                Ok(())
            }
        }
    }

    /// Return self to enforce #[must_use]
    fn add_circular_reference(self, location: &SrcPos) -> AnalysisError {
        if let AnalysisError::Fatal(err) = self {
            AnalysisError::Fatal(err.add_reference(location))
        } else {
            self
        }
    }
}

trait HandleFatal<T> {
    fn push_fatal_error(&mut self, error: FatalResult<T>);
}

impl<'a, T> HandleFatal<T> for dyn DiagnosticHandler + 'a {
    fn push_fatal_error(&mut self, error: FatalResult<T>) {
        if let Err(err) = error {
            err.push_into(self);
        }
    }
}

enum LookupResult<'a, T> {
    /// A single name was selected
    Single(VisibleDeclaration),
    /// All names within was selected
    AllWithin(&'a WithPos<T>, VisibleDeclaration),
    /// The name to lookup (or some part thereof was not a selected name)
    NotSelected,
}

pub struct Analyzer<'a> {
    work_sym: Symbol,
    std_sym: Symbol,
    standard_designator: Designator,
    standard_sym: Symbol,
    root: &'a DesignRoot,
}

impl<'a> Analyzer<'a> {
    pub fn new(root: &'a DesignRoot, symtab: &Arc<SymbolTable>) -> Analyzer<'a> {
        let standard_sym = symtab.insert(&Latin1String::new(b"standard"));

        Analyzer {
            work_sym: symtab.insert(&Latin1String::new(b"work")),
            std_sym: symtab.insert(&Latin1String::new(b"std")),
            standard_designator: Designator::Identifier(standard_sym.clone()),
            standard_sym,
            root,
        }
    }

    fn lookup_within(
        &self,
        prefix_pos: &SrcPos,
        prefix: &VisibleDeclaration,
        suffix: &WithPos<WithRef<Designator>>,
    ) -> AnalysisResult<VisibleDeclaration> {
        match prefix.first() {
            AnyDeclaration::Library(ref library_name) => {
                if let Some(decl) = self
                    .root
                    .expect_library(library_name)
                    .region
                    .lookup(suffix.designator(), false)
                {
                    Ok(decl.clone())
                } else {
                    Err(Diagnostic::error(
                        suffix.as_ref(),
                        format!(
                            "No primary unit '{}' within '{}'",
                            suffix.item, library_name
                        ),
                    ))?
                }
            }

            AnyDeclaration::UninstPackage(ref library_sym, ref package_sym) => Err(
                uninstantiated_package_prefix_error(prefix_pos, library_sym, package_sym),
            )?,

            AnyDeclaration::Package(ref library_name, ref package_name) => {
                let package = self.get_package_declaration_analysis(library_name, package_name)?;

                if let Some(decl) = package.region.lookup(suffix.designator(), false) {
                    Ok(decl.clone())
                } else {
                    Err(Diagnostic::error(
                        suffix.as_ref(),
                        format!(
                            "No declaration of '{}' within package '{}.{}'",
                            suffix.item, library_name, package_name
                        ),
                    ))?
                }
            }

            AnyDeclaration::PackageInstance(ref library_name, ref instance_name) => {
                let instance = self.get_package_instance_analysis(library_name, instance_name)?;
                if let Some(decl) = instance.region.lookup(suffix.designator(), false) {
                    Ok(decl.clone())
                } else {
                    Err(Diagnostic::error(
                        suffix.as_ref(),
                        format!(
                            "No declaration of '{}' within package instance '{}.{}'",
                            suffix.item, library_name, instance_name
                        ),
                    ))?
                }
            }

            AnyDeclaration::LocalPackageInstance(ref instance_name, ref instance_region) => {
                if let Some(decl) = instance_region.lookup(suffix.designator(), false) {
                    Ok(decl.clone())
                } else {
                    Err(Diagnostic::error(
                        suffix.as_ref(),
                        format!(
                            "No declaration of '{}' within package instance '{}'",
                            suffix.item, &instance_name
                        ),
                    ))?
                }
            }
            _ => Err(Diagnostic::error(
                prefix_pos,
                "Invalid prefix for selected name",
            ))?,
        }
    }

    fn lookup_selected_name<'n>(
        &self,
        region: &DeclarativeRegion<'_>,
        name: &'n WithPos<SelectedName>,
    ) -> AnalysisResult<VisibleDeclaration> {
        match self.lookup_selected_name_ref(region, name)? {
            LookupResult::Single(decl) => Ok(decl),
            _ => {
                panic!("Can only lookup single within SelectedName");
            }
        }
    }

    /// Lookup the prefix of a selected name
    fn lookup_prefix<'n, T: AsSelectedNameRef<'n, T>>(
        &self,
        region: &DeclarativeRegion<'_>,
        prefix: &'n WithPos<T>,
    ) -> AnalysisResult<VisibleDeclaration> {
        match self.lookup_selected_name_ref(region, prefix)? {
            LookupResult::Single(decl) => Ok(decl),
            LookupResult::AllWithin(..) => Err(Diagnostic::error(
                prefix.as_ref(),
                "'.all' may not be the prefix of a selected name",
            ))?,
            LookupResult::NotSelected => Err(Diagnostic::error(
                prefix.as_ref(),
                "may not be the prefix of a selected name",
            ))?,
        }
    }

    /// Returns the VisibleDeclaration or None if it was not a selected name
    /// Returns error message if a name was not declared
    /// @TODO We only lookup selected names since other names such as slice and index require typechecking
    fn lookup_selected_name_ref<'n, T: AsSelectedNameRef<'n, T>>(
        &self,
        region: &DeclarativeRegion<'_>,
        name: &'n WithPos<T>,
    ) -> AnalysisResult<LookupResult<'n, T>> {
        match name.item.as_selected_name_ref() {
            SelectedNameRef::Selected(ref prefix, ref suffix) => {
                let decl = self.lookup_prefix(region, prefix)?;
                match self.lookup_within(&prefix.pos, &decl, suffix) {
                    Ok(decl) => Ok(LookupResult::Single(decl)),
                    Err(err) => Err(err.add_circular_reference(&name.pos)),
                }
            }

            SelectedNameRef::SelectedAll(ref prefix) => {
                let decl = self.lookup_prefix(region, prefix)?;
                Ok(LookupResult::AllWithin(prefix, decl))
            }
            SelectedNameRef::Designator(ref designator) => {
                if let Some(visible_item) = region.lookup(designator.designator(), true) {
                    Ok(LookupResult::Single(visible_item.clone()))
                } else {
                    Err(Diagnostic::error(
                        &name.pos,
                        format!("No declaration of '{}'", designator),
                    ))?
                }
            }
            SelectedNameRef::Other => {
                // Not a selected name
                // @TODO at least lookup prefix for now
                Ok(LookupResult::NotSelected)
            }
        }
    }

    fn analyze_interface_declaration(
        &self,
        region: &mut DeclarativeRegion<'_>,
        decl: &mut InterfaceDeclaration,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        match decl {
            InterfaceDeclaration::File(ref mut file_decl) => {
                self.analyze_subtype_indicaton(
                    region,
                    &mut file_decl.subtype_indication,
                    diagnostics,
                )?;
                region.add(&file_decl.ident, AnyDeclaration::Other, diagnostics);
            }
            InterfaceDeclaration::Object(ref mut object_decl) => {
                self.analyze_subtype_indicaton(
                    region,
                    &mut object_decl.subtype_indication,
                    diagnostics,
                )?;
                region.add(&object_decl.ident, AnyDeclaration::Other, diagnostics);
            }
            InterfaceDeclaration::Type(ref ident) => {
                region.add(ident, AnyDeclaration::Other, diagnostics);
            }
            InterfaceDeclaration::Subprogram(ref mut subpgm, ..) => {
                self.analyze_subprogram_declaration(region, subpgm, diagnostics)?;
                region.add(subpgm.designator(), AnyDeclaration::Overloaded, diagnostics);
            }
            InterfaceDeclaration::Package(ref instance) => {
                match self.analyze_package_instance_name(region, &instance.package_name) {
                    Ok(package) => region.add(
                        &instance.ident,
                        AnyDeclaration::LocalPackageInstance(
                            instance.ident.item.clone(),
                            Arc::new(package.region.clone()),
                        ),
                        diagnostics,
                    ),
                    Err(err) => {
                        let err = err.add_circular_reference(&instance.package_name.pos);
                        err.add_to(diagnostics)?;
                    }
                }
            }
        };
        Ok(())
    }

    fn analyze_interface_list(
        &self,
        region: &mut DeclarativeRegion<'_>,
        declarations: &mut [InterfaceDeclaration],
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        for decl in declarations.iter_mut() {
            self.analyze_interface_declaration(region, decl, diagnostics)?;
        }
        Ok(())
    }

    fn lookup_type_mark(
        &self,
        region: &DeclarativeRegion<'_>,
        type_mark: &mut WithPos<SelectedName>,
    ) -> AnalysisResult<VisibleDeclaration> {
        self.lookup_selected_name(region, &type_mark)
    }

    fn analyze_subtype_indicaton(
        &self,
        region: &mut DeclarativeRegion<'_>,
        subtype_indication: &mut SubtypeIndication,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        if let Err(err) = self.lookup_type_mark(region, &mut subtype_indication.type_mark) {
            err.add_to(diagnostics)
        } else {
            Ok(())
        }
    }

    fn analyze_subprogram_declaration(
        &self,
        parent: &DeclarativeRegion<'_>,
        subprogram: &mut SubprogramDeclaration,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        let mut region = DeclarativeRegion::new_borrowed_parent(parent);

        match subprogram {
            SubprogramDeclaration::Function(fun) => {
                self.analyze_interface_list(&mut region, &mut fun.parameter_list, diagnostics)?;
                if let Err(err) = self.lookup_type_mark(&parent, &mut fun.return_type) {
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
        region.close_both(diagnostics);
        Ok(())
    }

    fn analyze_declaration(
        &self,
        region: &mut DeclarativeRegion<'_>,
        decl: &mut Declaration,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        match decl {
            Declaration::Alias(alias) => {
                if let Some(ref mut subtype_indication) = alias.subtype_indication {
                    self.analyze_subtype_indicaton(region, subtype_indication, diagnostics)?;
                }
                region.add(
                    alias.designator.clone(),
                    if alias.signature.is_some() {
                        AnyDeclaration::Overloaded
                    } else {
                        AnyDeclaration::Other
                    },
                    diagnostics,
                );
            }
            Declaration::Object(ref mut object_decl) => {
                self.analyze_subtype_indicaton(
                    region,
                    &mut object_decl.subtype_indication,
                    diagnostics,
                )?;
                region.add(
                    &object_decl.ident,
                    AnyDeclaration::from_object_declaration(object_decl),
                    diagnostics,
                );
            }
            Declaration::File(ref mut file_decl) => {
                self.analyze_subtype_indicaton(
                    region,
                    &mut file_decl.subtype_indication,
                    diagnostics,
                )?;
                region.add(&file_decl.ident, AnyDeclaration::Other, diagnostics);
            }
            Declaration::Component(ref mut component) => {
                region.add(&component.ident, AnyDeclaration::Other, diagnostics);

                {
                    let mut region = DeclarativeRegion::new_borrowed_parent(region);
                    self.analyze_interface_list(
                        &mut region,
                        &mut component.generic_list,
                        diagnostics,
                    )?;
                    region.close_both(diagnostics);
                }

                {
                    let mut region = DeclarativeRegion::new_borrowed_parent(region);
                    self.analyze_interface_list(
                        &mut region,
                        &mut component.port_list,
                        diagnostics,
                    )?;
                    region.close_both(diagnostics);
                }
            }
            Declaration::Attribute(ref mut attr) => match attr {
                Attribute::Declaration(ref mut attr_decl) => {
                    if let Err(err) = self.lookup_type_mark(region, &mut attr_decl.type_mark) {
                        err.add_to(diagnostics)?;
                    }
                    region.add(&attr_decl.ident, AnyDeclaration::Other, diagnostics);
                }
                // @TODO Ignored for now
                Attribute::Specification(..) => {}
            },
            Declaration::SubprogramBody(ref mut body) => {
                region.add(
                    body.specification.designator(),
                    AnyDeclaration::Overloaded,
                    diagnostics,
                );
                self.analyze_subprogram_declaration(region, &mut body.specification, diagnostics)?;
                let mut region = DeclarativeRegion::new_borrowed_parent(region);
                self.analyze_declarative_part(&mut region, &mut body.declarations, diagnostics)?;
            }
            Declaration::SubprogramDeclaration(ref mut subdecl) => {
                region.add(
                    subdecl.designator(),
                    AnyDeclaration::Overloaded,
                    diagnostics,
                );
                self.analyze_subprogram_declaration(region, subdecl, diagnostics)?;
            }

            Declaration::Use(ref mut use_clause) => {
                self.analyze_use_clause(
                    region,
                    &mut use_clause.item,
                    &use_clause.pos,
                    diagnostics,
                )?;
            }

            Declaration::Package(ref instance) => {
                match self.analyze_package_instance_name(region, &instance.package_name) {
                    Ok(data) => region.add(
                        &instance.ident,
                        AnyDeclaration::LocalPackageInstance(
                            instance.ident.item.clone(),
                            Arc::new(data.region.clone()),
                        ),
                        diagnostics,
                    ),
                    Err(err) => {
                        let err = err.add_circular_reference(&instance.package_name.pos);
                        err.add_to(diagnostics)?
                    }
                }
            }
            Declaration::Configuration(..) => {}
            Declaration::Type(ref mut type_decl) => {
                // Protected types are visible inside their declaration
                region.add(
                    &type_decl.ident,
                    AnyDeclaration::from_type_declaration(type_decl),
                    diagnostics,
                );

                match type_decl.def {
                    TypeDefinition::Enumeration(ref enumeration) => {
                        for literal in enumeration.iter() {
                            region.add(
                                literal.clone().map_into(|lit| lit.into_designator()),
                                AnyDeclaration::Overloaded,
                                diagnostics,
                            )
                        }
                    }
                    TypeDefinition::ProtectedBody(ref mut body) => {
                        let mut region = DeclarativeRegion::new_borrowed_parent(region);
                        self.analyze_declarative_part(&mut region, &mut body.decl, diagnostics)?;
                    }
                    TypeDefinition::Protected(ref mut prot_decl) => {
                        for item in prot_decl.items.iter_mut() {
                            match item {
                                ProtectedTypeDeclarativeItem::Subprogram(ref mut subprogram) => {
                                    self.analyze_subprogram_declaration(
                                        region,
                                        subprogram,
                                        diagnostics,
                                    )?;
                                }
                            }
                        }
                    }
                    TypeDefinition::Record(ref mut element_decls) => {
                        let mut record_region = DeclarativeRegion::default();
                        for elem_decl in element_decls.iter_mut() {
                            self.analyze_subtype_indicaton(
                                region,
                                &mut elem_decl.subtype,
                                diagnostics,
                            )?;
                            record_region.add(&elem_decl.ident, AnyDeclaration::Other, diagnostics);
                        }
                        record_region.close_both(diagnostics);
                    }
                    TypeDefinition::Access(ref mut subtype_indication) => {
                        self.analyze_subtype_indicaton(region, subtype_indication, diagnostics)?;
                    }
                    TypeDefinition::Array(.., ref mut subtype_indication) => {
                        self.analyze_subtype_indicaton(region, subtype_indication, diagnostics)?;
                    }
                    TypeDefinition::Subtype(ref mut subtype_indication) => {
                        self.analyze_subtype_indicaton(region, subtype_indication, diagnostics)?;
                    }
                    _ => {}
                }
            }
        };

        Ok(())
    }

    fn analyze_declarative_part(
        &self,
        region: &mut DeclarativeRegion<'_>,
        declarations: &mut [Declaration],
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        for decl in declarations.iter_mut() {
            self.analyze_declaration(region, decl, diagnostics)?;
        }
        Ok(())
    }

    fn analyze_use_clause(
        &self,
        region: &mut DeclarativeRegion<'_>,
        use_clause: &mut UseClause,
        use_pos: &SrcPos,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        for name in use_clause.name_list.iter() {
            match name.item {
                Name::Selected(..) => {}
                Name::SelectedAll(..) => {}
                _ => {
                    diagnostics.push(Diagnostic::error(
                        &use_pos,
                        "Use clause must be a selected name",
                    ));
                    continue;
                }
            }

            match self.lookup_selected_name_ref(&region, &name) {
                Ok(LookupResult::Single(visible_decl)) => {
                    region.make_potentially_visible(visible_decl);
                }
                Ok(LookupResult::AllWithin(prefix, visible_decl)) => {
                    match visible_decl.first() {
                        AnyDeclaration::Library(ref library_name) => {
                            region.make_all_potentially_visible(
                                &self.root.expect_library(library_name).region,
                            );
                        }
                        AnyDeclaration::UninstPackage(ref library_sym, ref package_sym) => {
                            diagnostics.push(uninstantiated_package_prefix_error(
                                &prefix.pos,
                                library_sym,
                                package_sym,
                            ));
                        }
                        AnyDeclaration::Package(ref library_name, ref package_name) => {
                            match self.get_package_declaration_analysis(library_name, package_name)
                            {
                                Ok(data) => {
                                    region.make_all_potentially_visible(&data.region);
                                }
                                Err(err) => {
                                    return Err(err.add_reference(&name.pos));
                                }
                            }
                        }
                        AnyDeclaration::PackageInstance(ref library_name, ref package_name) => {
                            match self.get_package_instance_analysis(library_name, package_name) {
                                Ok(data) => {
                                    region.make_all_potentially_visible(&data.region);
                                }
                                Err(err) => {
                                    return Err(err.add_reference(&name.pos));
                                }
                            }
                        }
                        AnyDeclaration::LocalPackageInstance(_, ref instance_region) => {
                            region.make_all_potentially_visible(&instance_region);
                        }
                        // @TODO handle others
                        _ => {}
                    }
                }
                Ok(LookupResult::NotSelected) => {
                    diagnostics.push(Diagnostic::error(
                        &use_pos,
                        "Use clause must be a selected name",
                    ));
                }
                Err(err) => {
                    err.add_to(diagnostics)?;
                }
            }
        }

        Ok(())
    }

    fn analyze_context_clause(
        &self,
        region: &mut DeclarativeRegion<'_>,
        context_clause: &mut [WithPos<ContextItem>],
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        for context_item in context_clause.iter_mut() {
            match context_item.item {
                ContextItem::Library(LibraryClause { ref name_list }) => {
                    for library_name in name_list.iter() {
                        if self.work_sym == library_name.item {
                            diagnostics.push(Diagnostic::hint(
                                &library_name,
                                "Library clause not necessary for current working library",
                            ))
                        } else if let Some(library) = self.root.get_library(&library_name.item) {
                            region.make_library_visible(&library.name, library);
                        } else {
                            diagnostics.push(Diagnostic::error(
                                &library_name,
                                format!("No such library '{}'", library_name.item),
                            ));
                        }
                    }
                }
                ContextItem::Use(ref mut use_clause) => {
                    self.analyze_use_clause(region, use_clause, &context_item.pos, diagnostics)?;
                }
                ContextItem::Context(ContextReference { ref name_list }) => {
                    for name in name_list {
                        match name.item {
                            Name::Selected(..) => {}
                            _ => {
                                diagnostics.push(Diagnostic::error(
                                    &context_item,
                                    "Context reference must be a selected name",
                                ));
                                continue;
                            }
                        }

                        match self.lookup_selected_name_ref(&region, &name) {
                            Ok(LookupResult::Single(visible_decl)) => {
                                match visible_decl.first() {
                                    // OK
                                    AnyDeclaration::Context(ref library_name, ref context_name) => {
                                        let library = self.root.expect_library(library_name);

                                        let context_lock = library
                                            .context(context_name)
                                            .expect("Assume context exists if made visible");
                                        match self.get_context_analysis(library, context_lock) {
                                            Ok(context) => {
                                                region.copy_visibility_from(&context.region);
                                            }
                                            Err(fatal_err) => {
                                                return Err(fatal_err.add_reference(&name.pos));
                                            }
                                        }
                                    }
                                    _ => {
                                        // @TODO maybe lookup should return the source position of the suffix
                                        if let Name::Selected(_, ref suffix) = name.item {
                                            diagnostics.push(Diagnostic::error(
                                                &suffix,
                                                format!(
                                                    "'{}' does not denote a context declaration",
                                                    suffix.designator()
                                                ),
                                            ));
                                        }
                                    }
                                }
                            }
                            Ok(LookupResult::AllWithin(..)) => {
                                // @TODO
                            }
                            Ok(LookupResult::NotSelected) => {
                                diagnostics.push(Diagnostic::error(
                                    &context_item,
                                    "Context reference must be a selected name",
                                ));
                            }
                            Err(err) => {
                                err.add_to(diagnostics)?;
                            }
                        }
                    }
                }
            }
        }

        Ok(())
    }

    fn analyze_generate_body(
        &self,
        parent: &DeclarativeRegion<'_>,
        body: &mut GenerateBody,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        let mut region = DeclarativeRegion::new_borrowed_parent(parent);

        if let Some(ref mut decl) = body.decl {
            self.analyze_declarative_part(&mut region, decl, diagnostics)?;
        }
        self.analyze_concurrent_part(&region, &mut body.statements, diagnostics)?;

        Ok(())
    }

    fn analyze_concurrent_statement(
        &self,
        parent: &DeclarativeRegion<'_>,
        statement: &mut LabeledConcurrentStatement,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        match statement.statement {
            ConcurrentStatement::Block(ref mut block) => {
                let mut region = DeclarativeRegion::new_borrowed_parent(parent);
                self.analyze_declarative_part(&mut region, &mut block.decl, diagnostics)?;
                self.analyze_concurrent_part(&region, &mut block.statements, diagnostics)?;
            }
            ConcurrentStatement::Process(ref mut process) => {
                let mut region = DeclarativeRegion::new_borrowed_parent(parent);
                self.analyze_declarative_part(&mut region, &mut process.decl, diagnostics)?;
            }
            ConcurrentStatement::ForGenerate(ref mut gen) => {
                self.analyze_generate_body(parent, &mut gen.body, diagnostics)?;
            }
            ConcurrentStatement::IfGenerate(ref mut gen) => {
                for conditional in gen.conditionals.iter_mut() {
                    self.analyze_generate_body(parent, &mut conditional.item, diagnostics)?;
                }
                if let Some(ref mut else_item) = gen.else_item {
                    self.analyze_generate_body(parent, else_item, diagnostics)?;
                }
            }
            ConcurrentStatement::CaseGenerate(ref mut gen) => {
                for alternative in gen.alternatives.iter_mut() {
                    self.analyze_generate_body(parent, &mut alternative.item, diagnostics)?;
                }
            }
            _ => {}
        };
        Ok(())
    }

    fn analyze_concurrent_part(
        &self,
        parent: &DeclarativeRegion<'_>,
        statements: &mut [LabeledConcurrentStatement],
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        for statement in statements.iter_mut() {
            self.analyze_concurrent_statement(parent, statement, diagnostics)?;
        }

        Ok(())
    }

    /// Add implicit context clause for all packages except STD.STANDARD
    /// library STD, WORK;
    /// use STD.STANDARD.all;
    fn add_implicit_context_clause(&self, region: &mut DeclarativeRegion<'_>, work: &Library) {
        region.make_library_visible(&self.work_sym, work);

        // @TODO maybe add warning if standard library is missing
        if let Some(library) = self.root.get_library(&self.std_sym) {
            region.make_library_visible(&self.std_sym, library);

            let decl = library.region.lookup(&self.standard_designator, false);

            if let Some(AnyDeclaration::Package(.., ref standard_pkg_name)) =
                decl.map(|decl| decl.first())
            {
                let standard_pkg_region = &self
                    .get_package_declaration_analysis(&library.name, standard_pkg_name)
                    .expect("Found fatal error when using STD.STANDARD package")
                    .region;
                region.make_all_potentially_visible(standard_pkg_region);
            } else {
                panic!("Could not find package standard");
            }
        }
    }

    fn analyze_package_declaration(
        &self,
        library: &Library,
        has_body: bool,
        package: &mut AnalysisUnit<PackageDeclaration>,
    ) -> FatalNullResult {
        let mut diagnostics = Vec::new();
        let mut root_region = Box::new(DeclarativeRegion::default());
        if !(library.name == self.std_sym && *package.name() == self.standard_sym) {
            self.add_implicit_context_clause(&mut root_region, library);
        }

        self.analyze_context_clause(
            &mut root_region,
            &mut package.context_clause,
            &mut diagnostics,
        )?;

        let mut region = DeclarativeRegion::new_owned_parent(root_region).in_package_declaration();

        if let Some(ref mut list) = package.unit.generic_clause {
            self.analyze_interface_list(&mut region, list, &mut diagnostics)?;
        }
        self.analyze_declarative_part(&mut region, &mut package.unit.decl, &mut diagnostics)?;

        if has_body {
            region.close_immediate(&mut diagnostics);
        } else {
            region.close_both(&mut diagnostics);
        }

        package.diagnostics = diagnostics;
        package.region = region;
        Ok(())
    }

    fn get_package_declaration_analysis(
        &self,
        library_name: &Symbol,
        package_name: &Symbol,
    ) -> FatalResult<ReadGuard<AnalysisUnit<PackageDeclaration>>> {
        let library = self.root.expect_library(library_name);
        let package_unit = library.expect_any_package(package_name);
        match package_unit.package.entry()? {
            AnalysisEntry::Vacant(mut package) => {
                self.analyze_package_declaration(
                    library,
                    package_unit.body.is_some(),
                    &mut package,
                )?;
                drop(package);
                Ok(package_unit.package.expect_analyzed())
            }
            AnalysisEntry::Occupied(package) => Ok(package),
        }
    }

    fn analyze_package_body_unit(
        &self,
        primary_region: &DeclarativeRegion<'_>,
        body: &mut AnalysisUnit<PackageBody>,
    ) -> FatalNullResult {
        let mut diagnostics = Vec::new();
        // @TODO make pattern of primary/secondary extension
        let mut root_region = primary_region.get_parent().unwrap().extend(None);
        self.analyze_context_clause(&mut root_region, &mut body.context_clause, &mut diagnostics)?;
        let mut region = primary_region.extend(Some(&root_region));
        self.analyze_declarative_part(&mut region, &mut body.unit.decl, &mut diagnostics)?;
        region.close_both(&mut diagnostics);
        // Body does not need region
        body.diagnostics = diagnostics;
        Ok(())
    }

    fn analyze_package(
        &self,
        library: &Library,
        package_unit: &PackageDesignUnit,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        let package = self.get_package_declaration_analysis(&library.name, package_unit.name())?;
        diagnostics.append(package.diagnostics.clone());
        if let Some(ref body_lock) = package_unit.body {
            match body_lock.entry()? {
                AnalysisEntry::Vacant(mut body) => {
                    self.analyze_package_body_unit(&package.region, &mut body)?;
                    diagnostics.append(body.diagnostics.clone());
                }
                AnalysisEntry::Occupied(body) => {
                    diagnostics.append(body.diagnostics.clone());
                }
            }
        }
        Ok(())
    }

    fn analyze_architecture(
        &self,
        entity: &AnalysisUnit<EntityDeclaration>,
        arch: &mut AnalysisUnit<ArchitectureBody>,
    ) -> FatalNullResult {
        let mut diagnostics = Vec::new();
        let mut root_region = entity.region.get_parent().unwrap().extend(None);
        self.analyze_context_clause(&mut root_region, &mut arch.context_clause, &mut diagnostics)?;
        let mut region = entity.region.extend(Some(&root_region));
        self.analyze_declarative_part(&mut region, &mut arch.unit.decl, &mut diagnostics)?;
        self.analyze_concurrent_part(&mut region, &mut arch.unit.statements, &mut diagnostics)?;
        region.close_both(&mut diagnostics);
        // @TODO architecture does not need a region
        arch.diagnostics = diagnostics;
        Ok(())
    }

    fn analyze_entity(
        &self,
        library: &Library,
        entity_unit: &EntityDesignUnit,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        let entity = self.get_entity_declaration_analysis(library, &entity_unit.entity)?;
        diagnostics.append(entity.diagnostics.clone());

        for arch_lock in entity_unit.architectures.values() {
            match arch_lock.entry()? {
                AnalysisEntry::Vacant(mut arch) => {
                    self.analyze_architecture(&entity, &mut arch)?;
                    diagnostics.append(arch.diagnostics.clone());
                }
                AnalysisEntry::Occupied(arch) => {
                    diagnostics.append(arch.diagnostics.clone());
                }
            };
        }
        Ok(())
    }

    fn analyze_entity_declaration(
        &self,
        library: &Library,
        entity: &mut AnalysisUnit<EntityDeclaration>,
    ) -> FatalNullResult {
        let mut diagnostics = Vec::new();
        let mut root_region = DeclarativeRegion::default();
        self.add_implicit_context_clause(&mut root_region, library);
        self.analyze_context_clause(
            &mut root_region,
            &mut entity.context_clause,
            &mut diagnostics,
        )?;

        let mut region = DeclarativeRegion::new_owned_parent(Box::new(root_region));

        if let Some(ref mut list) = entity.unit.generic_clause {
            self.analyze_interface_list(&mut region, list, &mut diagnostics)?;
        }
        if let Some(ref mut list) = entity.unit.port_clause {
            self.analyze_interface_list(&mut region, list, &mut diagnostics)?;
        }
        self.analyze_declarative_part(&mut region, &mut entity.unit.decl, &mut diagnostics)?;
        self.analyze_concurrent_part(&region, &mut entity.unit.statements, &mut diagnostics)?;

        region.close_immediate(&mut diagnostics);

        entity.region = region;
        entity.diagnostics = diagnostics;
        Ok(())
    }

    fn get_entity_declaration_analysis(
        &self,
        library: &Library,
        entity_lock: &'a LockedUnit<EntityDeclaration>,
    ) -> FatalResult<ReadGuard<AnalysisUnit<EntityDeclaration>>> {
        match entity_lock.entry()? {
            AnalysisEntry::Vacant(mut entity) => {
                self.analyze_entity_declaration(library, &mut entity)?;
                drop(entity);
                Ok(entity_lock.expect_analyzed())
            }
            AnalysisEntry::Occupied(entity) => Ok(entity),
        }
    }

    fn lookup_package_instance_name(
        &self,
        region: &DeclarativeRegion<'_>,
        package_name: &WithPos<SelectedName>,
    ) -> AnalysisResult<(Symbol, Symbol)> {
        let decl = self.lookup_selected_name(region, &package_name)?;

        if let AnyDeclaration::UninstPackage(ref library_name, ref package_name) = decl.first() {
            Ok((library_name.clone(), package_name.clone()))
        } else {
            Err(Diagnostic::error(
                &package_name.pos,
                format!(
                    "'{}' is not an uninstantiated generic package",
                    &decl.designator
                ),
            ))?
        }
    }
    /// Returns a reference to the the uninstantiated package
    fn analyze_package_instance_name(
        &self,
        region: &DeclarativeRegion<'_>,
        package_name: &WithPos<SelectedName>,
    ) -> AnalysisResult<ReadGuard<AnalysisUnit<PackageDeclaration>>> {
        let (library_name, package_name) =
            self.lookup_package_instance_name(region, package_name)?;

        match self.get_package_declaration_analysis(&library_name, &package_name) {
            Ok(package) => Ok(package),
            Err(err) => Err(AnalysisError::Fatal(err)),
        }
    }

    fn analyze_package_instance(
        &self,
        library: &Library,
        instance: &mut AnalysisUnit<PackageInstantiation>,
    ) -> FatalNullResult {
        let mut diagnostics = Vec::new();
        let mut region = DeclarativeRegion::default();
        self.add_implicit_context_clause(&mut region, library);
        self.analyze_context_clause(&mut region, &mut instance.context_clause, &mut diagnostics)?;

        match self.analyze_package_instance_name(&region, &instance.unit.package_name) {
            Ok(package) => {
                instance.diagnostics = diagnostics;
                instance.region = package.region.clone();
                Ok(())
            }
            Err(AnalysisError::NotFatal(diagnostic)) => {
                diagnostics.push(diagnostic);
                // Failed to analyze, add empty region with diagnostics
                instance.diagnostics = diagnostics;
                Ok(())
            }
            Err(AnalysisError::Fatal(err)) => {
                Err(err.add_reference(&instance.unit.package_name.pos))
            }
        }
    }

    fn get_package_instance_analysis(
        &self,
        library_name: &Symbol,
        instance_name: &Symbol,
    ) -> FatalResult<ReadGuard<AnalysisUnit<PackageInstantiation>>> {
        let library = self.root.expect_library(library_name);
        let instance = library.expect_package_instance(instance_name);

        match instance.instance.entry()? {
            AnalysisEntry::Vacant(mut unit) => {
                self.analyze_package_instance(library, &mut unit)?;
                drop(unit);
                Ok(instance.instance.expect_analyzed())
            }
            AnalysisEntry::Occupied(entry) => Ok(entry),
        }
    }

    fn analyze_context(&self, library: &Library, context: &mut Context) -> FatalNullResult {
        let mut root_region = DeclarativeRegion::default();
        self.add_implicit_context_clause(&mut root_region, library);
        let mut diagnostics = Vec::new();
        let mut region = DeclarativeRegion::new_owned_parent(Box::new(root_region));
        self.analyze_context_clause(&mut region, &mut context.decl.items, &mut diagnostics)?;
        context.region = region;
        context.diagnostics = diagnostics;
        Ok(())
    }

    fn get_context_analysis(
        &self,
        library: &Library,
        lock: &'a AnalysisLock<Context>,
    ) -> FatalResult<ReadGuard<'a, Context>> {
        match lock.entry()? {
            AnalysisEntry::Vacant(mut context) => {
                self.analyze_context(library, &mut context)?;
                drop(context);
                Ok(lock.expect_analyzed())
            }
            AnalysisEntry::Occupied(context) => Ok(context),
        }
    }

    fn lookup_entity_for_configuration<'l>(
        &self,
        library: &'l Library,
        region: &DeclarativeRegion<'_>,
        config: &ConfigurationDeclaration,
    ) -> AnalysisResult<&'l EntityDesignUnit> {
        let ref ent_name = config.entity_name;

        let decl = {
            match ent_name.item {
                // Entitities are implicitly defined for configurations
                // configuration cfg of ent
                SelectedName::Designator(_) => self.lookup_selected_name(&library.region, ent_name),

                // configuration cfg of lib.ent
                _ => self.lookup_selected_name(&region, &ent_name),
            }
        }?;

        match decl.first() {
            AnyDeclaration::Entity(ref libsym, ref entsym) => {
                if libsym != &library.name {
                    Err(Diagnostic::error(
                                    &ent_name,
                                    format!("Configuration must be within the same library '{}' as the corresponding entity", &library.name),
                                ))?
                } else {
                    Ok(&library.entity(entsym).expect("Expect entity is available"))
                }
            }
            _ => Err(Diagnostic::error(&ent_name, "does not denote an entity"))?,
        }
    }

    fn analyze_configuration(
        &self,
        library: &Library,
        config: &mut AnalysisUnit<ConfigurationDeclaration>,
    ) -> FatalNullResult {
        let mut diagnostics = Vec::new();
        let mut region = DeclarativeRegion::default();
        self.add_implicit_context_clause(&mut region, library);
        self.analyze_context_clause(&mut region, &mut config.context_clause, &mut diagnostics)?;

        match self.lookup_entity_for_configuration(library, &region, &config.unit) {
            Ok(entity_unit) => {
                let primary_pos = entity_unit.pos();
                let secondary_pos = config.pos();
                if primary_pos.source == secondary_pos.source
                    && primary_pos.start > secondary_pos.start
                {
                    diagnostics.push(Diagnostic::error(
                        secondary_pos,
                        format!(
                            "Configuration '{}' declared before entity '{}'",
                            &config.name(),
                            entity_unit.name()
                        ),
                    ));
                }
            }
            Err(err) => {
                err.add_to(&mut diagnostics)?;
            }
        };

        config.region = region;
        config.diagnostics = diagnostics;
        Ok(())
    }

    fn get_configuration_analysis(
        &self,
        library: &Library,
        config_lock: &'a LockedUnit<ConfigurationDeclaration>,
    ) -> FatalResult<ReadGuard<AnalysisUnit<ConfigurationDeclaration>>> {
        match config_lock.entry()? {
            AnalysisEntry::Vacant(mut config) => {
                self.analyze_configuration(library, &mut config)?;
                drop(config);
                Ok(config_lock.expect_analyzed())
            }
            AnalysisEntry::Occupied(config) => Ok(config),
        }
    }

    fn analyze_library(&self, library: &Library, diagnostics: &mut dyn DiagnosticHandler) {
        for package in library.packages() {
            let fatal_err = self.analyze_package(library, package, diagnostics);
            diagnostics.push_fatal_error(fatal_err);
        }

        for package in library.uninst_packages() {
            let fatal_err = self.analyze_package(library, package, diagnostics);
            diagnostics.push_fatal_error(fatal_err);
        }

        for package_instance in library.package_instances() {
            match self.get_package_instance_analysis(&library.name, package_instance.name()) {
                Ok(instance) => {
                    diagnostics.append(instance.diagnostics.clone());
                }
                Err(fatal_err) => {
                    fatal_err.push_into(diagnostics);
                }
            }
        }

        for context in library.contexts() {
            match self.get_context_analysis(library, context) {
                Ok(context) => diagnostics.append(context.diagnostics.clone()),
                Err(fatal_err) => {
                    fatal_err.push_into(diagnostics);
                }
            }
        }

        for entity in library.entities() {
            let fatal_err = self.analyze_entity(library, entity, diagnostics);
            diagnostics.push_fatal_error(fatal_err);
        }

        for config in library.configurations() {
            match self.get_configuration_analysis(library, config) {
                Ok(config) => diagnostics.append(config.diagnostics.clone()),
                Err(fatal_err) => {
                    fatal_err.push_into(diagnostics);
                }
            }
        }
    }

    pub fn analyze(&self, diagnostics: &mut dyn DiagnosticHandler) {
        if let Some(library) = self.root.get_library(&self.std_sym) {
            // Analyze standard library first
            self.get_package_declaration_analysis(&self.std_sym, &self.standard_sym)
                .expect("Expect no fatal error when STD.STANDARD package");

            for package in library.packages() {
                if *package.name() != self.standard_sym {
                    self.analyze_package(library, package, diagnostics)
                        .expect("Expect no circular error when analyzing packages in STD library");
                }
            }
        }

        for library in self.root.iter_libraries() {
            // Standard library already analyzed
            if library.name == self.std_sym {
                continue;
            }

            self.analyze_library(library, diagnostics);
        }
    }
}

fn uninstantiated_package_prefix_error(
    prefix: &SrcPos,
    library_sym: &Symbol,
    package_sym: &Symbol,
) -> Diagnostic {
    Diagnostic::error(
        prefix,
        format!(
            "Uninstantiated generic package '{}.{}' may not be the prefix of a selected name",
            library_sym, package_sym
        ),
    )
}
