// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use super::declarative_region::{
    AnyDeclaration, DeclarativeRegion, PrimaryUnitData, VisibleDeclaration,
};
use super::library::{DesignRoot, Library, PackageDesignUnit};
use crate::ast::{HasIdent, *};
use crate::diagnostic::{Diagnostic, DiagnosticHandler};
use crate::latin_1::Latin1String;
use crate::source::{SrcPos, WithPos};
use crate::symbol_table::{Symbol, SymbolTable};
use std::cell::RefCell;
use std::collections::hash_map::Entry;
use std::sync::Arc;

use self::fnv::FnvHashMap;
use fnv;

enum LookupResult<'a> {
    /// A single name was selected
    Single(VisibleDeclaration),
    /// A single name was selected
    AllWithin(&'a WithPos<Name>, VisibleDeclaration),
    /// The name to lookup (or some part thereof was not a selected name)
    NotSelected,
    /// A prefix but found but lookup was not implemented yet
    Unfinished,
}

#[derive(Clone, Debug)]
struct Dependency {
    library_name: Symbol,
    primary_unit_name: Symbol,
    location: SrcPos,
}

#[derive(Clone, Debug)]
struct CircularDependencyError {
    path: Vec<Dependency>,
}

impl CircularDependencyError {
    fn push_into(self, diagnostics: &mut dyn DiagnosticHandler) {
        for dependency in self.path {
            diagnostics.push(Diagnostic::error(
                dependency.location,
                format!(
                    "Found circular dependency when referencing '{}.{}'",
                    dependency.library_name, dependency.primary_unit_name
                ),
            ));
        }
    }
}

type AnalysisResult = Result<Arc<PrimaryUnitData>, CircularDependencyError>;

enum StartAnalysisResult<'a> {
    AlreadyAnalyzed(AnalysisResult),
    NotYetAnalyzed(PendingAnalysis<'a>),
}

struct PendingAnalysis<'a> {
    context: &'a AnalysisContext,
    key: (Symbol, Symbol),
}

impl<'a> PendingAnalysis<'a> {
    fn new(context: &'a AnalysisContext, key: (Symbol, Symbol)) -> PendingAnalysis<'a> {
        PendingAnalysis { context, key }
    }

    fn end_analysis(self, data: PrimaryUnitData) -> AnalysisResult {
        match self
            .context
            .primary_unit_data
            .borrow_mut()
            .entry(self.key.clone())
        {
            // Analysis already done, keep old value
            Entry::Occupied(entry) => {
                // Will always be a circular dependency error
                assert!(entry.get().is_err());
                entry.get().clone()
            }
            Entry::Vacant(entry) => {
                let result = Ok(Arc::new(data));
                entry.insert(result.clone());
                result
            }
        }
    }
}

impl Drop for PendingAnalysis<'_> {
    fn drop(&mut self) {
        self.context.locked.borrow_mut().remove(&self.key);
        self.context.path.borrow_mut().pop();
    }
}

struct AnalysisContext {
    primary_unit_data: RefCell<FnvHashMap<(Symbol, Symbol), AnalysisResult>>,
    locked: RefCell<FnvHashMap<(Symbol, Symbol), ()>>,
    path: RefCell<Vec<Option<Dependency>>>,
}

impl<'a> AnalysisContext {
    fn new() -> AnalysisContext {
        AnalysisContext {
            primary_unit_data: RefCell::new(FnvHashMap::default()),
            locked: RefCell::new(FnvHashMap::default()),
            path: RefCell::new(Vec::new()),
        }
    }

    fn start_analysis(
        &'a self,
        // The optional location where the design unit was used
        entry_point: Option<SrcPos>,
        library_name: &Symbol,
        primary_unit_name: &Symbol,
    ) -> StartAnalysisResult<'a> {
        if let Some(result) = self.get_result(library_name, primary_unit_name) {
            return StartAnalysisResult::AlreadyAnalyzed(result);
        }

        let key = (library_name.clone(), primary_unit_name.clone());

        self.path
            .borrow_mut()
            .push(entry_point.map(|location| Dependency {
                library_name: library_name.clone(),
                primary_unit_name: primary_unit_name.clone(),
                location,
            }));

        if self.locked.borrow_mut().insert(key.clone(), ()).is_some() {
            let path: Vec<Dependency> = self
                .path
                .borrow()
                .iter()
                .cloned()
                .filter_map(|d| d)
                .collect();
            let path_result = Err(CircularDependencyError { path });

            // All locked units will have circular dependencies
            for other_key in self.locked.borrow().keys() {
                let result = {
                    if *other_key != key {
                        Err(CircularDependencyError { path: Vec::new() })
                    } else {
                        path_result.clone()
                    }
                };

                self.primary_unit_data
                    .borrow_mut()
                    .insert(other_key.clone(), result);
            }

            // Only provide full path error to one unit so that duplicate diagnostics are not created
            StartAnalysisResult::AlreadyAnalyzed(path_result)
        } else {
            StartAnalysisResult::NotYetAnalyzed(PendingAnalysis::new(self, key))
        }
    }

    fn get_result(
        &self,
        library_name: &Symbol,
        primary_unit_name: &Symbol,
    ) -> Option<AnalysisResult> {
        self.primary_unit_data
            .borrow()
            .get(&(library_name.clone(), primary_unit_name.clone()))
            .cloned()
    }
}

pub struct Analyzer<'a> {
    work_sym: Symbol,
    std_sym: Symbol,
    standard_designator: Designator,
    standard_sym: Symbol,
    root: &'a DesignRoot,

    /// DeclarativeRegion for each library containing the primary units
    library_regions: FnvHashMap<Symbol, DeclarativeRegion<'static>>,
    analysis_context: AnalysisContext,
}

impl<'a> Analyzer<'a> {
    pub fn new(root: &'a DesignRoot, symtab: &Arc<SymbolTable>) -> Analyzer<'a> {
        let mut library_regions = FnvHashMap::default();
        let mut diagnostics = Vec::new();

        for library in root.iter_libraries() {
            let mut region = DeclarativeRegion::new(None);

            for package in library.packages() {
                let package_sym = package.package.unit.ident.item.clone();

                region.add(
                    &package.package.unit.ident,
                    AnyDeclaration::Package(library.name.clone(), package_sym),
                    &mut diagnostics,
                );
            }

            for context in library.contexts() {
                let context_sym = context.ident.item.clone();

                region.add(
                    &context.ident,
                    AnyDeclaration::Context(library.name.clone(), context_sym),
                    &mut diagnostics,
                );
            }

            for entity in library.entities() {
                region.add(
                    &entity.entity.unit.ident,
                    AnyDeclaration::Other,
                    &mut diagnostics,
                );
            }

            for configuration in library.configurations() {
                region.add(
                    configuration.ident(),
                    AnyDeclaration::Other,
                    &mut diagnostics,
                );
            }

            for instance in library.package_instances() {
                let instance_sym = instance.ident().item.clone();
                region.add(
                    instance.ident(),
                    AnyDeclaration::PackageInstance(library.name.clone(), instance_sym),
                    &mut diagnostics,
                );
            }

            library_regions.insert(library.name.clone(), region);
        }

        assert!(diagnostics.is_empty());

        let standard_sym = symtab.insert(&Latin1String::new(b"standard"));
        Analyzer {
            work_sym: symtab.insert(&Latin1String::new(b"work")),
            std_sym: symtab.insert(&Latin1String::new(b"std")),
            standard_designator: Designator::Identifier(standard_sym.clone()),
            standard_sym,
            root,
            library_regions,
            analysis_context: AnalysisContext::new(),
        }
    }

    /// Returns the VisibleDeclaration or None if it was not a selected name
    /// Returns error message if a name was not declared
    /// @TODO We only lookup selected names since other names such as slice and index require typechecking
    fn lookup_selected_name<'n>(
        &self,
        region: &DeclarativeRegion<'_>,
        name: &'n WithPos<Name>,
    ) -> Result<LookupResult<'n>, Diagnostic> {
        match name.item {
            Name::Selected(ref prefix, ref suffix) => {
                let visible_decl = {
                    match self.lookup_selected_name(region, prefix)? {
                        LookupResult::Single(visible_decl) => visible_decl,
                        LookupResult::AllWithin(..) => {
                            return Err(Diagnostic::error(
                                prefix.as_ref(),
                                "'.all' may not be the prefix of a selected name",
                            ));
                        }
                        others => return Ok(others),
                    }
                };

                match visible_decl.first() {
                    AnyDeclaration::Library(ref library_name) => {
                        if let Some(visible_decl) =
                            self.library_regions[library_name].lookup(&suffix.item, false)
                        {
                            Ok(LookupResult::Single(visible_decl.clone()))
                        } else {
                            Err(Diagnostic::error(
                                suffix.as_ref(),
                                format!(
                                    "No primary unit '{}' within '{}'",
                                    suffix.item, library_name
                                ),
                            ))
                        }
                    }

                    AnyDeclaration::Package(ref library_name, ref package_name) => {
                        let library = self
                            .root
                            .get_library(library_name)
                            .expect("Assume library exists if made visible");

                        let package = library
                            .package(package_name)
                            .expect("Assume package exists if made visible");

                        if package.is_generic() {
                            Err(uninstantiated_package_prefix_error(
                                &prefix.pos,
                                library,
                                package,
                            ))
                        } else if let Ok(data) =
                            self.get_package_result(Some(prefix.pos.clone()), library, package)
                        {
                            if let Some(visible_decl) = data.region.lookup(&suffix.item, false) {
                                Ok(LookupResult::Single(visible_decl.clone()))
                            } else {
                                Err(Diagnostic::error(
                                    suffix.as_ref(),
                                    format!(
                                        "No declaration of '{}' within package '{}.{}'",
                                        suffix.item,
                                        &library.name,
                                        package.package.name()
                                    ),
                                ))
                            }
                        } else {
                            // Circular dependency, diagnostic will never be used
                            Err(Diagnostic::error(&prefix.pos, ""))
                        }
                    }

                    AnyDeclaration::PackageInstance(ref library_name, ref instance_name) => {
                        let library = self
                            .root
                            .get_library(library_name)
                            .expect("Assume library exists if made visible");

                        let instance = library
                            .package_instance(instance_name)
                            .expect("Assume package instance exists if made visible");

                        if let Ok(data) = self.analyze_package_instance_unit(
                            Some(prefix.pos.clone()),
                            library,
                            instance,
                        ) {
                            if let Some(visible_decl) = data.region.lookup(&suffix.item, false) {
                                Ok(LookupResult::Single(visible_decl.clone()))
                            } else {
                                Err(Diagnostic::error(
                                    suffix.as_ref(),
                                    format!(
                                        "No declaration of '{}' within package instance '{}.{}'",
                                        suffix.item,
                                        &library.name,
                                        instance.unit.name()
                                    ),
                                ))
                            }
                        } else {
                            // Circular dependency, diagnostic will never be used
                            Err(Diagnostic::error(&prefix.pos, ""))
                        }
                    }

                    AnyDeclaration::LocalPackageInstance(ref instance_name, ref data) => {
                        if let Some(visible_decl) = data.region.lookup(&suffix.item, false) {
                            Ok(LookupResult::Single(visible_decl.clone()))
                        } else {
                            Err(Diagnostic::error(
                                suffix.as_ref(),
                                format!(
                                    "No declaration of '{}' within package instance '{}'",
                                    suffix.item, &instance_name
                                ),
                            ))
                        }
                    }
                    // @TODO ignore other declarations for now
                    _ => Ok(LookupResult::Unfinished),
                }
            }

            Name::SelectedAll(ref prefix) => match self.lookup_selected_name(region, prefix)? {
                LookupResult::Single(visible_decl) => {
                    Ok(LookupResult::AllWithin(prefix, visible_decl))
                }
                LookupResult::AllWithin(..) => Err(Diagnostic::error(
                    prefix.as_ref(),
                    "'.all' may not be the prefix of a selected name",
                )),
                others => Ok(others),
            },
            Name::Designator(ref designator) => {
                if let Some(visible_item) = region.lookup(&designator, true) {
                    Ok(LookupResult::Single(visible_item.clone()))
                } else {
                    Err(Diagnostic::error(
                        &name.pos,
                        format!("No declaration of '{}'", designator),
                    ))
                }
            }
            _ => {
                // Not a selected name
                // @TODO at least lookup prefix for now
                Ok(LookupResult::NotSelected)
            }
        }
    }

    fn analyze_interface_declaration(
        &self,
        region: &mut DeclarativeRegion<'_>,
        decl: &InterfaceDeclaration,
        diagnostics: &mut dyn DiagnosticHandler,
    ) {
        match decl {
            InterfaceDeclaration::File(ref file_decl) => {
                self.analyze_subtype_indicaton(region, &file_decl.subtype_indication, diagnostics);
                region.add(&file_decl.ident, AnyDeclaration::Other, diagnostics);
            }
            InterfaceDeclaration::Object(ref object_decl) => {
                self.analyze_subtype_indicaton(
                    region,
                    &object_decl.subtype_indication,
                    diagnostics,
                );
                region.add(&object_decl.ident, AnyDeclaration::Other, diagnostics);
            }
            InterfaceDeclaration::Type(ref ident) => {
                region.add(ident, AnyDeclaration::Other, diagnostics);
            }
            InterfaceDeclaration::Subprogram(subpgm, ..) => {
                self.analyze_subprogram_declaration(region, subpgm, diagnostics);
                region.add(subpgm.designator(), AnyDeclaration::Overloaded, diagnostics);
            }
            InterfaceDeclaration::Package(ref instance) => {
                match self.analyze_package_instance_name(region, &instance.package_name) {
                    Ok(package_region) => region.add(
                        &instance.ident,
                        AnyDeclaration::LocalPackageInstance(
                            instance.ident.item.clone(),
                            package_region,
                        ),
                        diagnostics,
                    ),
                    Err(diagnostic) => {
                        diagnostics.push(diagnostic);
                    }
                }
            }
        }
    }

    fn analyze_interface_list(
        &self,
        region: &mut DeclarativeRegion<'_>,
        declarations: &[InterfaceDeclaration],
        diagnostics: &mut dyn DiagnosticHandler,
    ) {
        for decl in declarations.iter() {
            self.analyze_interface_declaration(region, decl, diagnostics);
        }
    }

    #[allow(clippy::ptr_arg)]
    fn lookup_type_mark(
        &self,
        region: &DeclarativeRegion<'_>,
        type_mark: &WithPos<SelectedName>,
    ) -> Result<VisibleDeclaration, Diagnostic> {
        let type_mark_name = type_mark.clone().into();
        match self.lookup_selected_name(region, &type_mark_name)? {
            LookupResult::Single(visible_decl) => Ok(visible_decl),
            _ => {
                // Cannot really happen with SelectedName but refactoring might change it...
                Err(Diagnostic::error(
                    &type_mark_name.pos,
                    "Invalid name for type mark",
                ))
            }
        }
    }

    fn analyze_subtype_indicaton(
        &self,
        region: &mut DeclarativeRegion<'_>,
        subtype_indication: &SubtypeIndication,
        diagnostics: &mut dyn DiagnosticHandler,
    ) {
        if let Err(diagnostic) = self.lookup_type_mark(region, &subtype_indication.type_mark) {
            diagnostics.push(diagnostic);
        }
    }

    fn analyze_subprogram_declaration(
        &self,
        parent: &DeclarativeRegion<'_>,
        subprogram: &SubprogramDeclaration,
        diagnostics: &mut dyn DiagnosticHandler,
    ) {
        let mut region = DeclarativeRegion::new(Some(parent));

        match subprogram {
            SubprogramDeclaration::Function(fun) => {
                self.analyze_interface_list(&mut region, &fun.parameter_list, diagnostics);
                if let Err(diagnostic) = self.lookup_type_mark(&parent, &fun.return_type) {
                    diagnostics.push(diagnostic);
                }
            }
            SubprogramDeclaration::Procedure(proc) => {
                self.analyze_interface_list(&mut region, &proc.parameter_list, diagnostics);
            }
        }
        region.close_both(diagnostics);
    }

    fn analyze_declaration(
        &self,
        region: &mut DeclarativeRegion<'_>,
        decl: &'a Declaration,
        diagnostics: &mut dyn DiagnosticHandler,
    ) {
        match decl {
            Declaration::Alias(alias) => {
                if let Some(ref subtype_indication) = alias.subtype_indication {
                    self.analyze_subtype_indicaton(region, subtype_indication, diagnostics);
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
            Declaration::Object(ref object_decl) => {
                self.analyze_subtype_indicaton(
                    region,
                    &object_decl.subtype_indication,
                    diagnostics,
                );
                region.add(
                    &object_decl.ident,
                    AnyDeclaration::from_object_declaration(object_decl),
                    diagnostics,
                );
            }
            Declaration::File(ref file_decl) => {
                self.analyze_subtype_indicaton(region, &file_decl.subtype_indication, diagnostics);
                region.add(&file_decl.ident, AnyDeclaration::Other, diagnostics);
            }
            Declaration::Component(ref component) => {
                region.add(&component.ident, AnyDeclaration::Other, diagnostics);

                {
                    let mut region = DeclarativeRegion::new(Some(region));
                    self.analyze_interface_list(&mut region, &component.generic_list, diagnostics);
                    region.close_both(diagnostics);
                }

                {
                    let mut region = DeclarativeRegion::new(Some(region));
                    self.analyze_interface_list(&mut region, &component.port_list, diagnostics);
                    region.close_both(diagnostics);
                }
            }
            Declaration::Attribute(ref attr) => match attr {
                Attribute::Declaration(ref attr_decl) => {
                    if let Err(diagnostic) = self.lookup_type_mark(region, &attr_decl.type_mark) {
                        diagnostics.push(diagnostic);
                    }
                    region.add(&attr_decl.ident, AnyDeclaration::Other, diagnostics);
                }
                // @TODO Ignored for now
                Attribute::Specification(..) => {}
            },
            Declaration::SubprogramBody(body) => {
                region.add(
                    body.specification.designator(),
                    AnyDeclaration::Overloaded,
                    diagnostics,
                );
                self.analyze_subprogram_declaration(region, &body.specification, diagnostics);
                let mut region = DeclarativeRegion::new(Some(region));
                self.analyze_declarative_part(&mut region, &body.declarations, diagnostics);
            }
            Declaration::SubprogramDeclaration(subdecl) => {
                region.add(
                    subdecl.designator(),
                    AnyDeclaration::Overloaded,
                    diagnostics,
                );
                self.analyze_subprogram_declaration(region, &subdecl, diagnostics);
            }

            // @TODO Ignored for now
            Declaration::Use(ref use_clause) => {
                self.analyze_use_clause(region, &use_clause.item, &use_clause.pos, diagnostics);
            }
            Declaration::Package(ref instance) => {
                match self.analyze_package_instance(region, instance) {
                    Ok(package_region) => region.add(
                        &instance.ident,
                        AnyDeclaration::LocalPackageInstance(
                            instance.ident.item.clone(),
                            package_region,
                        ),
                        diagnostics,
                    ),
                    Err(diagnostic) => {
                        diagnostics.push(diagnostic);
                    }
                }
            }
            Declaration::Configuration(..) => {}
            Declaration::Type(ref type_decl) => {
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
                    TypeDefinition::ProtectedBody(ref body) => {
                        let mut region = DeclarativeRegion::new(Some(region));
                        self.analyze_declarative_part(&mut region, &body.decl, diagnostics);
                    }
                    TypeDefinition::Protected(ref prot_decl) => {
                        for item in prot_decl.items.iter() {
                            match item {
                                ProtectedTypeDeclarativeItem::Subprogram(subprogram) => {
                                    self.analyze_subprogram_declaration(
                                        region,
                                        subprogram,
                                        diagnostics,
                                    );
                                }
                            }
                        }
                    }
                    TypeDefinition::Record(ref element_decls) => {
                        let mut record_region = DeclarativeRegion::new(None);
                        for elem_decl in element_decls.iter() {
                            self.analyze_subtype_indicaton(region, &elem_decl.subtype, diagnostics);
                            record_region.add(&elem_decl.ident, AnyDeclaration::Other, diagnostics);
                        }
                        record_region.close_both(diagnostics);
                    }
                    TypeDefinition::Access(ref subtype_indication) => {
                        self.analyze_subtype_indicaton(region, subtype_indication, diagnostics);
                    }
                    TypeDefinition::Array(.., ref subtype_indication) => {
                        self.analyze_subtype_indicaton(region, subtype_indication, diagnostics);
                    }
                    TypeDefinition::Subtype(ref subtype_indication) => {
                        self.analyze_subtype_indicaton(region, subtype_indication, diagnostics);
                    }
                    _ => {}
                }
            }
        }
    }

    fn analyze_declarative_part(
        &self,
        region: &mut DeclarativeRegion<'_>,
        declarations: &[Declaration],
        diagnostics: &mut dyn DiagnosticHandler,
    ) {
        for decl in declarations.iter() {
            self.analyze_declaration(region, decl, diagnostics);
        }
    }

    fn analyze_use_clause(
        &self,
        region: &mut DeclarativeRegion<'_>,
        use_clause: &UseClause,
        use_pos: &SrcPos,
        diagnostics: &mut dyn DiagnosticHandler,
    ) {
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

            match self.lookup_selected_name(&region, &name) {
                Ok(LookupResult::Single(visible_decl)) => {
                    region.make_potentially_visible(visible_decl);
                }
                Ok(LookupResult::AllWithin(prefix, visible_decl)) => {
                    match visible_decl.first() {
                        AnyDeclaration::Library(ref library_name) => {
                            region
                                .make_all_potentially_visible(&self.library_regions[library_name]);
                        }
                        AnyDeclaration::Package(ref library_name, ref package_name) => {
                            let library = self
                                .root
                                .get_library(library_name)
                                .expect("Assume library exists if made visible");

                            let package = library
                                .package(package_name)
                                .expect("Assume package exists if made visible");

                            if package.is_generic() {
                                diagnostics.push(uninstantiated_package_prefix_error(
                                    &prefix.pos,
                                    library,
                                    package,
                                ));
                            } else if let Ok(data) =
                                self.get_package_result(Some(prefix.pos.clone()), library, package)
                            {
                                region.make_all_potentially_visible(&data.region);
                            } else {
                                // Circular dependency, return
                                return;
                            }
                        }
                        AnyDeclaration::PackageInstance(ref library_name, ref package_name) => {
                            let library = self
                                .root
                                .get_library(library_name)
                                .expect("Assume library exists if made visible");

                            let package = library
                                .package_instance(package_name)
                                .expect("Assume package exists if made visible");

                            if let Ok(data) = self.analyze_package_instance_unit(
                                Some(prefix.pos.clone()),
                                library,
                                package,
                            ) {
                                region.make_all_potentially_visible(&data.region);
                            } else {
                                // Circular dependency, return
                                return;
                            }
                        }
                        AnyDeclaration::LocalPackageInstance(_, ref data) => {
                            region.make_all_potentially_visible(&data.region);
                        }
                        // @TODO handle others
                        _ => {}
                    }
                }
                Ok(LookupResult::Unfinished) => {}
                Ok(LookupResult::NotSelected) => {
                    diagnostics.push(Diagnostic::error(
                        &use_pos,
                        "Use clause must be a selected name",
                    ));
                }
                Err(diagnostic) => {
                    diagnostics.push(diagnostic);
                }
            }
        }
    }

    fn analyze_context_clause(
        &self,
        region: &mut DeclarativeRegion<'_>,
        context_clause: &[WithPos<ContextItem>],
        diagnostics: &mut dyn DiagnosticHandler,
    ) {
        for context_item in context_clause.iter() {
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
                ContextItem::Use(ref use_clause) => {
                    self.analyze_use_clause(region, use_clause, &context_item.pos, diagnostics);
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

                        match self.lookup_selected_name(&region, &name) {
                            Ok(LookupResult::Single(visible_decl)) => {
                                match visible_decl.first() {
                                    // OK
                                    AnyDeclaration::Context(ref library_name, ref context_name) => {
                                        let library = self
                                            .root
                                            .get_library(library_name)
                                            .expect("Assume library exists if made visible");

                                        let context = library
                                            .context(context_name)
                                            .expect("Assume context exists if made visible");

                                        // Error will be given when
                                        // analyzing the context
                                        // clause specifically and
                                        // shall not be duplicated
                                        // here
                                        let mut ignore_diagnostics = Vec::new();
                                        self.analyze_context_clause(
                                            region,
                                            &context.items,
                                            &mut ignore_diagnostics,
                                        );
                                    }
                                    _ => {
                                        // @TODO maybe lookup should return the source position of the suffix
                                        if let Name::Selected(_, ref suffix) = name.item {
                                            diagnostics.push(Diagnostic::error(
                                                &suffix,
                                                format!(
                                                    "'{}' does not denote a context declaration",
                                                    &suffix.item
                                                ),
                                            ));
                                        }
                                    }
                                }
                            }
                            Ok(LookupResult::AllWithin(..)) => {
                                // @TODO
                            }
                            Ok(LookupResult::Unfinished) => {}
                            Ok(LookupResult::NotSelected) => {
                                diagnostics.push(Diagnostic::error(
                                    &context_item,
                                    "Context reference must be a selected name",
                                ));
                            }
                            Err(diagnostic) => {
                                diagnostics.push(diagnostic);
                            }
                        }
                    }
                }
            }
        }
    }

    fn get_package_result(
        &self,
        entry_point: Option<SrcPos>,
        library: &Library,
        package: &PackageDesignUnit,
    ) -> AnalysisResult {
        self.analyze_package_declaration_unit(entry_point, library, package)
    }

    fn analyze_generate_body(
        &self,
        parent: &DeclarativeRegion<'_>,
        body: &GenerateBody,
        diagnostics: &mut dyn DiagnosticHandler,
    ) {
        let mut region = DeclarativeRegion::new(Some(parent));

        if let Some(ref decl) = body.decl {
            self.analyze_declarative_part(&mut region, &decl, diagnostics);
        }
        self.analyze_concurrent_part(&region, &body.statements, diagnostics);
    }

    fn analyze_concurrent_statement(
        &self,
        parent: &DeclarativeRegion<'_>,
        statement: &LabeledConcurrentStatement,
        diagnostics: &mut dyn DiagnosticHandler,
    ) {
        match statement.statement {
            ConcurrentStatement::Block(ref block) => {
                let mut region = DeclarativeRegion::new(Some(parent));
                self.analyze_declarative_part(&mut region, &block.decl, diagnostics);
                self.analyze_concurrent_part(&region, &block.statements, diagnostics);
            }
            ConcurrentStatement::Process(ref process) => {
                let mut region = DeclarativeRegion::new(Some(parent));
                self.analyze_declarative_part(&mut region, &process.decl, diagnostics);
            }
            ConcurrentStatement::ForGenerate(ref gen) => {
                self.analyze_generate_body(parent, &gen.body, diagnostics);
            }
            ConcurrentStatement::IfGenerate(ref gen) => {
                for conditional in gen.conditionals.iter() {
                    self.analyze_generate_body(parent, &conditional.item, diagnostics);
                }
                if let Some(ref else_item) = gen.else_item {
                    self.analyze_generate_body(parent, else_item, diagnostics);
                }
            }
            ConcurrentStatement::CaseGenerate(ref gen) => {
                for alternative in gen.alternatives.iter() {
                    self.analyze_generate_body(parent, &alternative.item, diagnostics);
                }
            }
            _ => {}
        }
    }

    fn analyze_concurrent_part(
        &self,
        parent: &DeclarativeRegion<'_>,
        statements: &[LabeledConcurrentStatement],
        diagnostics: &mut dyn DiagnosticHandler,
    ) {
        for statement in statements.iter() {
            self.analyze_concurrent_statement(parent, statement, diagnostics);
        }
    }

    fn analyze_architecture_body(
        &self,
        entity_region: &mut DeclarativeRegion<'_>,
        architecture: &ArchitectureBody,
        diagnostics: &mut dyn DiagnosticHandler,
    ) {
        self.analyze_declarative_part(entity_region, &architecture.decl, diagnostics);
        self.analyze_concurrent_part(entity_region, &architecture.statements, diagnostics);
    }

    fn analyze_entity_declaration(
        &self,
        region: &mut DeclarativeRegion<'_>,
        entity: &EntityDeclaration,
        diagnostics: &mut dyn DiagnosticHandler,
    ) {
        if let Some(ref list) = entity.generic_clause {
            self.analyze_interface_list(region, list, diagnostics);
        }
        if let Some(ref list) = entity.port_clause {
            self.analyze_interface_list(region, list, diagnostics);
        }
        self.analyze_declarative_part(region, &entity.decl, diagnostics);
        self.analyze_concurrent_part(region, &entity.statements, diagnostics);
    }

    /// Add implicit context clause for all packages except STD.STANDARD
    /// library STD, WORK;
    /// use STD.STANDARD.all;
    fn add_implicit_context_clause(&self, region: &mut DeclarativeRegion<'_>, work: &Library) {
        region.make_library_visible(&self.work_sym, work);

        // @TODO maybe add warning if standard library is missing
        if let Some(library) = self.root.get_library(&self.std_sym) {
            region.make_library_visible(&self.std_sym, library);

            let decl = self.library_regions[&library.name].lookup(&self.standard_designator, false);

            if let Some(AnyDeclaration::Package(.., ref standard_pkg_name)) =
                decl.map(|decl| decl.first())
            {
                let standard_pkg_region = &self
                    .analysis_context
                    .get_result(&library.name, standard_pkg_name)
                    .expect("STD.STANDARD package must be analyzed at this point")
                    .expect("Found circular dependency when using STD.STANDARD package")
                    .region;
                region.make_all_potentially_visible(standard_pkg_region);
            } else {
                panic!("Could not find package standard");
            }
        }
    }
    fn analyze_package_declaration(
        &self,
        region: &mut DeclarativeRegion,
        package: &PackageDeclaration,
        diagnostics: &mut dyn DiagnosticHandler,
    ) {
        if let Some(ref list) = package.generic_clause {
            self.analyze_interface_list(region, list, diagnostics);
        }
        self.analyze_declarative_part(region, &package.decl, diagnostics);
    }

    fn analyze_package_declaration_unit(
        &self,
        // The optional entry point where the package declarartion was used
        // None if the package was directly analyzed and not due to a use clause
        entry_point: Option<SrcPos>,
        library: &Library,
        package: &PackageDesignUnit,
    ) -> AnalysisResult {
        let mut diagnostics = Vec::new();

        match self.analysis_context.start_analysis(
            entry_point,
            &library.name,
            package.package.name(),
        ) {
            StartAnalysisResult::NotYetAnalyzed(pending) => {
                let mut root_region = Box::new(DeclarativeRegion::new(None));
                if !(library.name == self.std_sym && *package.package.name() == self.standard_sym) {
                    self.add_implicit_context_clause(&mut root_region, library);
                }

                self.analyze_context_clause(
                    &mut root_region,
                    &package.package.context_clause,
                    &mut diagnostics,
                );

                let mut region =
                    DeclarativeRegion::new_owned_parent(root_region).in_package_declaration();
                self.analyze_package_declaration(
                    &mut region,
                    &package.package.unit,
                    &mut diagnostics,
                );

                if package.body.is_some() {
                    region.close_immediate(&mut diagnostics);
                } else {
                    region.close_both(&mut diagnostics);
                }

                pending.end_analysis(PrimaryUnitData::new(diagnostics, region))
            }

            StartAnalysisResult::AlreadyAnalyzed(result) => result,
        }
    }

    fn analyze_package_body_unit(
        &self,
        primary_region: &DeclarativeRegion<'_>,
        package: &PackageDesignUnit,
        diagnostics: &mut dyn DiagnosticHandler,
    ) {
        if let Some(ref body) = package.body {
            // @TODO make pattern of primary/secondary extension
            let mut root_region = primary_region.get_parent().unwrap().extend(None);
            self.analyze_context_clause(&mut root_region, &body.context_clause, diagnostics);
            let mut region = primary_region.extend(Some(&root_region));
            self.analyze_declarative_part(&mut region, &body.unit.decl, diagnostics);
            region.close_both(diagnostics);
        }
    }

    pub fn analyze_package(
        &self,
        library: &Library,
        package: &PackageDesignUnit,
        diagnostics: &mut dyn DiagnosticHandler,
    ) {
        match self.analyze_package_declaration_unit(None, library, package) {
            Ok(data) => {
                data.push_to(diagnostics);
                self.analyze_package_body_unit(&data.region, &package, diagnostics);
            }
            Err(circular_dependency) => {
                circular_dependency.push_into(diagnostics);
            }
        };
    }

    /// Returns a reference to the the uninstantiated package
    pub fn analyze_package_instance(
        &self,
        parent: &DeclarativeRegion<'_>,
        package_instance: &PackageInstantiation,
    ) -> Result<Arc<PrimaryUnitData>, Diagnostic> {
        self.analyze_package_instance_name(parent, &package_instance.package_name)
    }

    /// Returns a reference to the the uninstantiated package
    #[allow(clippy::ptr_arg)]
    pub fn analyze_package_instance_name(
        &self,
        parent: &DeclarativeRegion<'_>,
        package_name: &WithPos<SelectedName>,
    ) -> Result<Arc<PrimaryUnitData>, Diagnostic> {
        let entry_point = package_name.pos.clone();
        let package_name = package_name.clone().into();

        match self.lookup_selected_name(parent, &package_name)? {
            LookupResult::Single(visible_decl) => {
                if let AnyDeclaration::Package(ref library_name, ref package_name) =
                    visible_decl.first()
                {
                    let library = self
                        .root
                        .get_library(library_name)
                        .expect("Assume library exists if made visible");

                    let package = library
                        .package(package_name)
                        .expect("Assume package exists if made visible");

                    if package.is_generic() {
                        if let Ok(data) =
                            self.get_package_result(Some(entry_point.clone()), library, package)
                        {
                            return Ok(data.clone());
                        } else {
                            return Err(Diagnostic::error(
                                &entry_point,
                                format!(
                                    "'Could not instantiate package '{}.{}' with circular dependency'",
                                    &library.name, package.package.name()
                                ),
                            ));
                        }
                    }
                }
                Err(Diagnostic::error(
                    &package_name.pos,
                    format!(
                        "'{}' is not an uninstantiated generic package",
                        &visible_decl.designator
                    ),
                ))
            }
            _ => {
                // Cannot really happen as package_name is a SelectedName so cannot test it
                // Leave here in case of future refactoring changes the type
                Err(Diagnostic::error(
                    &package_name.pos,
                    "Invalid selected name for generic package",
                ))
            }
        }
    }

    fn analyze_package_instance_unit(
        &self,
        entry_point: Option<SrcPos>,
        library: &Library,
        package_instance: &DesignUnit<PackageInstantiation>,
    ) -> AnalysisResult {
        let mut diagnostics = Vec::new();

        match self.analysis_context.start_analysis(
            entry_point,
            &library.name,
            package_instance.unit.name(),
        ) {
            StartAnalysisResult::NotYetAnalyzed(pending) => {
                let mut region = DeclarativeRegion::new(None);
                self.add_implicit_context_clause(&mut region, library);
                self.analyze_context_clause(
                    &mut region,
                    &package_instance.context_clause,
                    &mut diagnostics,
                );

                match self.analyze_package_instance(&region, &package_instance.unit) {
                    Ok(data) => {
                        // @TODO avoid clone?
                        pending.end_analysis(PrimaryUnitData::new(diagnostics, data.region.clone()))
                    }
                    Err(diagnostic) => {
                        diagnostics.push(diagnostic);
                        // Failed to analyze, add empty region
                        pending.end_analysis(PrimaryUnitData::new(
                            diagnostics,
                            DeclarativeRegion::new(None),
                        ))
                    }
                }
            }
            StartAnalysisResult::AlreadyAnalyzed(result) => result,
        }
    }

    pub fn analyze_library(&self, library: &Library, diagnostics: &mut dyn DiagnosticHandler) {
        for package in library.packages() {
            self.analyze_package(library, package, diagnostics);
        }

        for package_instance in library.package_instances() {
            match self.analyze_package_instance_unit(None, library, package_instance) {
                Ok(data) => {
                    data.push_to(diagnostics);
                }
                Err(circular_dependency) => {
                    circular_dependency.push_into(diagnostics);
                }
            }
        }

        for context in library.contexts() {
            let mut root_region = DeclarativeRegion::new(None);
            self.add_implicit_context_clause(&mut root_region, library);
            self.analyze_context_clause(&mut root_region, &context.items, diagnostics);
        }

        for entity in library.entities() {
            let mut primary_root_region = DeclarativeRegion::new(None);
            self.add_implicit_context_clause(&mut primary_root_region, library);
            self.analyze_context_clause(
                &mut primary_root_region,
                &entity.entity.context_clause,
                diagnostics,
            );

            let mut primary_region = DeclarativeRegion::new(Some(&primary_root_region));
            self.analyze_entity_declaration(&mut primary_region, &entity.entity.unit, diagnostics);
            primary_region.close_immediate(diagnostics);

            for architecture in entity.architectures.values() {
                let mut root_region = primary_root_region.extend(None);
                self.analyze_context_clause(
                    &mut root_region,
                    &architecture.context_clause,
                    diagnostics,
                );
                let mut region = primary_region.extend(Some(&root_region));
                self.analyze_architecture_body(&mut region, &architecture.unit, diagnostics);
                region.close_both(diagnostics);
            }
        }
    }

    pub fn analyze(&self, diagnostics: &mut dyn DiagnosticHandler) {
        // Analyze standard library first
        if let Some(library) = self.root.get_library(&self.std_sym) {
            let standard_package = library
                .package(&self.standard_sym)
                .expect("Failed to find package STD.STANDARD");
            self.analyze_package(library, standard_package, diagnostics);
            for package in library.packages() {
                if *package.package.name() != self.standard_sym {
                    self.analyze_package(library, package, diagnostics);
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
    library: &Library,
    package: &PackageDesignUnit,
) -> Diagnostic {
    Diagnostic::error(
        prefix,
        format!(
            "Uninstantiated generic package '{}.{}' may not be the prefix of a selected name",
            &library.name,
            package.package.name()
        ),
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::diagnostic::Diagnostic;
    use crate::test_util::{check_diagnostics, check_no_diagnostics, Code, CodeBuilder};

    fn expected_message(code: &Code, name: &str, occ1: usize, occ2: usize) -> Diagnostic {
        Diagnostic::error(
            code.s(&name, occ2),
            format!("Duplicate declaration of '{}'", &name),
        )
        .related(code.s(&name, occ1), "Previously defined here")
    }

    fn expected_diagnostics(code: &Code, names: &[&str]) -> Vec<Diagnostic> {
        let mut diagnostics = Vec::new();
        for name in names {
            diagnostics.push(expected_message(code, name, 1, 2));
        }
        diagnostics
    }

    fn expected_diagnostics_multi(code1: &Code, code2: &Code, names: &[&str]) -> Vec<Diagnostic> {
        let mut diagnostics = Vec::new();
        for name in names {
            diagnostics.push(
                Diagnostic::error(
                    code2.s1(&name),
                    format!("Duplicate declaration of '{}'", &name),
                )
                .related(code1.s1(&name), "Previously defined here"),
            )
        }
        diagnostics
    }

    #[test]
    fn allows_unique_names() {
        let mut builder = LibraryBuilder::new();
        builder.code(
            "libname",
            "
package pkg is
  constant a : natural := 0;
  constant b : natural := 0;
  constant c : natural := 0;
end package;
",
        );

        let diagnostics = builder.analyze();
        check_no_diagnostics(&diagnostics);
    }

    #[test]
    fn allows_deferred_constant() {
        let mut builder = LibraryBuilder::new();
        builder.code(
            "libname",
            "
package pkg is
  constant a : natural;
end package;

package body pkg is
  constant a : natural := 0;
end package body;
",
        );

        let diagnostics = builder.analyze();
        check_no_diagnostics(&diagnostics);
    }

    #[test]
    fn forbid_deferred_constant_after_constant() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package pkg is
  constant a1 : natural := 0;
  constant a1 : natural;
end package;
",
        );

        let diagnostics = builder.analyze();
        check_diagnostics(diagnostics, expected_diagnostics(&code, &["a1"]));
    }

    #[test]
    fn forbid_deferred_constant_outside_of_package_declaration() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package pkg is
end package;

package body pkg is
  constant a1 : natural;
  constant a1 : natural := 0;
end package body;
",
        );

        let diagnostics = builder.analyze();
        check_diagnostics(
            diagnostics,
            vec![Diagnostic::error(
                &code.s1("a1"),
                "Deferred constants are only allowed in package declarations (not body)",
            )],
        );
    }

    #[test]
    fn forbid_full_declaration_of_deferred_constant_outside_of_package_body() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package pkg is
  constant a1 : natural;
  constant a1 : natural := 0;
end package;
",
        );

        let diagnostics = builder.analyze();
        check_diagnostics(
            diagnostics,
            vec![Diagnostic::error(
                &code.s("a1", 1),
                "Deferred constant 'a1' lacks corresponding full constant declaration in package body",
            ),Diagnostic::error(
                &code.s("a1", 2),
                "Full declaration of deferred constant is only allowed in a package body",
            )],
        );
    }

    #[test]
    fn error_on_missing_full_constant_declaration() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package pkg_no_body is
  constant a1 : natural;
end package;

package pkg is
  constant b1 : natural;
end package;

package body pkg is
end package body;
",
        );

        let diagnostics = builder.analyze();
        check_diagnostics(
            diagnostics,
            vec![
                Diagnostic::error(
                    &code.s1("a1"),
                    "Deferred constant 'a1' lacks corresponding full constant declaration in package body",
                ),
                Diagnostic::error(
                    &code.s1("b1"),
                    "Deferred constant 'b1' lacks corresponding full constant declaration in package body",
                ),
            ],
        );
    }

    #[test]
    fn error_on_missing_protected_body() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package pkg_no_body is
  type a1 is protected
  end protected;
end package;

package pkg is
  type b1 is protected
  end protected;
end package;

package body pkg is
end package body;
",
        );

        let diagnostics = builder.analyze();
        check_diagnostics(
            diagnostics,
            vec![
                Diagnostic::error(&code.s1("a1"), "Missing body for protected type 'a1'"),
                Diagnostic::error(&code.s1("b1"), "Missing body for protected type 'b1'"),
            ],
        );
    }

    #[test]
    fn error_on_missing_protected_type_for_body() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package pkg_no_body is
  type a1 is protected body
  end protected body;
end package;

package pkg is
end package;

package body pkg is
  type b1 is protected body
  end protected body;

  type b1 is protected
  end protected;
end package body;
",
        );

        let diagnostics = builder.analyze();
        check_diagnostics(
            diagnostics,
            vec![
                Diagnostic::error(&code.s1("a1"), "No declaration of protected type 'a1'"),
                Diagnostic::error(&code.s1("b1"), "No declaration of protected type 'b1'"),
                Diagnostic::error(&code.s("b1", 2), "Missing body for protected type 'b1'"),
            ],
        );
    }

    #[test]
    fn forbid_multiple_constant_after_deferred_constant() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package pkg is
  constant a1 : natural;
end package;

package body pkg is
  constant a1 : natural := 0;
  constant a1 : natural := 0;
end package body;
",
        );

        let diagnostics = builder.analyze();
        check_diagnostics(diagnostics, vec![expected_message(&code, "a1", 2, 3)]);
    }

    #[test]
    fn forbid_homographs() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package pkg is
  constant a1 : natural := 0;
  constant a : natural := 0;
  constant a1 : natural := 0;
end package;
",
        );

        let diagnostics = builder.analyze();
        check_diagnostics(diagnostics, expected_diagnostics(&code, &["a1"]));
    }

    #[test]
    fn allows_protected_type_and_body_with_same_name() {
        let mut builder = LibraryBuilder::new();
        builder.code(
            "libname",
            "
package pkg is
  type prot_t is protected
  end protected;

  type prot_t is protected body
  end protected body;
end package;
",
        );

        let diagnostics = builder.analyze();
        check_no_diagnostics(&diagnostics);
    }

    #[test]
    fn forbid_duplicate_protected_type() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package pkg is
  type prot_t is protected
  end protected;

  type prot_t is protected
  end protected;

  type prot_t is protected body
  end protected body;
end package;
",
        );

        let diagnostics = builder.analyze();
        check_diagnostics(diagnostics, expected_diagnostics(&code, &["prot_t"]));
    }

    #[test]
    fn forbid_duplicate_protected_type_body() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package pkg is
  type prot_t is protected
  end protected;

  type prot_t is protected body
  end protected body;

  type prot_t is protected body
  end protected body;
end package;
",
        );

        let diagnostics = builder.analyze();
        check_diagnostics(diagnostics, vec![expected_message(&code, "prot_t", 2, 3)]);
    }

    #[test]
    fn forbid_incompatible_deferred_items() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package pkg is

  -- Protected type vs constant
  type a1 is protected
  end protected;
  constant a1 : natural := 0;

  -- Just to avoid missing body error
  type a1 is protected body
  end protected body;

  -- Deferred constant vs protected body
  constant b1 : natural;
  type b1 is protected body
  end protected body;

end package;

package body pkg is
  constant b1 : natural := 0;
end package body;
",
        );

        let diagnostics = builder.analyze();
        let mut expected = vec![Diagnostic::error(
            &code.s("b1", 2),
            "No declaration of protected type 'b1'",
        )];
        expected.append(&mut expected_diagnostics(&code, &["a1", "b1"]));
        check_diagnostics(diagnostics, expected);
    }

    #[test]
    fn allows_incomplete_type_definition() {
        let mut builder = LibraryBuilder::new();
        builder.code(
            "libname",
            "
package pkg is
  type rec_t;
  type rec_t is record
  end record;
end package;
",
        );

        let diagnostics = builder.analyze();
        check_no_diagnostics(&diagnostics);
    }

    #[test]
    fn error_on_duplicate_incomplete_type_definition() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package pkg is
  type rec_t;
  type rec_t;
  type rec_t is record
  end record;
end package;
",
        );

        let diagnostics = builder.analyze();
        check_diagnostics(diagnostics, expected_diagnostics(&code, &["rec_t"]));
    }

    #[test]
    fn error_on_missing_full_type_definition_for_incomplete() {
        let mut builder = LibraryBuilder::new();
        let code_pkg = builder.code(
            "libname",
            "
package pkg is
  type rec_t;
end package;

package body pkg is
  -- Must appear in the same immediate declarative region
  type rec_t is record
  end record;
end package body;
",
        );

        let code_ent = builder.code(
            "libname",
            "
entity ent is
end entity;

architecture rtl of ent is
  type rec_t;
begin
  blk : block
    -- Must appear in the same immediate declarative region
    type rec_t is record
    end record;
  begin
  end block;
end architecture;
",
        );

        let code_pkg2 = builder.code(
            "libname",
            "
-- To check that no duplicate errors are made when closing the immediate and extended regions
package pkg2 is
  type rec_t;
end package;

package body pkg2 is
end package body;
",
        );

        let mut expected_diagnostics = Vec::new();
        for code in [code_pkg, code_ent, code_pkg2].iter() {
            expected_diagnostics.push(Diagnostic::error(
                code.s1("rec_t"),
                "Missing full type declaration of incomplete type 'rec_t'",
            ));
            expected_diagnostics.push(
                Diagnostic::hint(
                    code.s1("rec_t"),
                    "The full type declaration shall occur immediately within the same declarative part",
                ));
        }

        let diagnostics = builder.analyze();
        check_diagnostics(diagnostics, expected_diagnostics);
    }

    #[test]
    fn forbid_homographs_in_subprogram_bodies() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package pkg is
end package;

package body pkg is
  procedure proc(a1, a, a1 : natural) is
    constant b1 : natural := 0;
    constant b : natural := 0;
    constant b1 : natural := 0;

    procedure nested_proc(c1, c, c1 : natural) is
      constant d1 : natural := 0;
      constant d : natural := 0;
      constant d1 : natural := 0;
    begin
    end;

  begin
  end;
end package body;
",
        );

        let diagnostics = builder.analyze();
        check_diagnostics(
            diagnostics,
            expected_diagnostics(&code, &["a1", "b1", "c1", "d1"]),
        );
    }

    #[test]
    fn forbid_homographs_in_component_declarations() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package pkg is
  component comp is
    generic (
      a1 : natural;
      a : natural;
      a1 : natural
    );
    port (
      b1 : natural;
      b : natural;
      b1 : natural
    );
  end component;
end package;
",
        );

        let diagnostics = builder.analyze();
        check_diagnostics(diagnostics, expected_diagnostics(&code, &["a1", "b1"]));
    }

    #[test]
    fn forbid_homographs_in_record_type_declarations() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package pkg is
  type rec_t is record
    a1 : natural;
    a : natural;
    a1 : natural;
  end record;
end package;
",
        );

        let diagnostics = builder.analyze();
        check_diagnostics(diagnostics, expected_diagnostics(&code, &["a1"]));
    }

    #[test]
    fn forbid_homographs_in_proteced_type_declarations() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package pkg is
  type prot_t is protected
    procedure proc(a1, a, a1 : natural);
  end protected;

  type prot_t is protected body
    constant b1 : natural := 0;
    constant b : natural := 0;
    constant b1 : natural := 0;
  end protected body;
end package;
",
        );

        let diagnostics = builder.analyze();
        check_diagnostics(diagnostics, expected_diagnostics(&code, &["a1", "b1"]));
    }

    #[test]
    fn forbid_homographs_in_subprogram_declarations() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package pkg is
  procedure proc(a1, a, a1 : natural);
  function fun(b1, a, b1 : natural) return natural;
end package;
",
        );

        let diagnostics = builder.analyze();
        check_diagnostics(diagnostics, expected_diagnostics(&code, &["a1", "b1"]));
    }

    #[test]
    fn forbid_homographs_in_block() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
entity ent is
begin
  blk : block
    constant a1 : natural := 0;
    constant a : natural := 0;
    constant a1 : natural := 0;
  begin
    process
      constant b1 : natural := 0;
      constant b : natural := 0;
      constant b1 : natural := 0;
    begin
    end process;
  end block;
end entity;
",
        );

        let diagnostics = builder.analyze();
        check_diagnostics(diagnostics, expected_diagnostics(&code, &["a1", "b1"]));
    }

    #[test]
    fn forbid_homographs_in_process() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
entity ent is
begin
  process
    constant a1 : natural := 0;
    constant a : natural := 0;
    constant a1 : natural := 0;
  begin
  end process;
end entity;
",
        );

        let diagnostics = builder.analyze();
        check_diagnostics(diagnostics, expected_diagnostics(&code, &["a1"]));
    }

    #[test]
    fn forbid_homographs_for_generate() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
entity ent is
begin
  gen_for: for i in 0 to 3 generate
    constant a1 : natural := 0;
    constant a : natural := 0;
    constant a1 : natural := 0;
  begin
    process
      constant b1 : natural := 0;
      constant b : natural := 0;
      constant b1 : natural := 0;
    begin
    end process;
  end generate;
end entity;
",
        );

        let diagnostics = builder.analyze();
        check_diagnostics(diagnostics, expected_diagnostics(&code, &["a1", "b1"]));
    }

    #[test]
    fn forbid_homographs_if_generate() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
entity ent is
begin
  gen_if: if true generate
    constant a1 : natural := 0;
    constant a : natural := 0;
    constant a1 : natural := 0;
  begin

    prcss : process
      constant b1 : natural := 0;
      constant b : natural := 0;
      constant b1 : natural := 0;
    begin
    end process;

  else generate
    constant c1 : natural := 0;
    constant c: natural := 0;
    constant c1 : natural := 0;
  begin
    prcss : process
      constant d1 : natural := 0;
      constant d : natural := 0;
      constant d1 : natural := 0;
    begin
    end process;
  end generate;
end entity;
",
        );

        let diagnostics = builder.analyze();
        check_diagnostics(
            diagnostics,
            expected_diagnostics(&code, &["a1", "b1", "c1", "d1"]),
        );
    }

    #[test]
    fn forbid_homographs_case_generate() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
entity ent is
begin
  gen_case: case 0 generate
    when others =>
      constant a1 : natural := 0;
      constant a : natural := 0;
      constant a1 : natural := 0;
    begin
      process
        constant b1 : natural := 0;
        constant b : natural := 0;
        constant b1 : natural := 0;
      begin
      end process;
  end generate;
end entity;
",
        );

        let diagnostics = builder.analyze();
        check_diagnostics(diagnostics, expected_diagnostics(&code, &["a1", "b1"]));
    }

    #[test]
    fn forbid_homographs_in_entity_declarations() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
entity ent is
  generic (
    a1 : natural;
    a : natural;
    a1 : natural
  );
  port (
    b1 : natural;
    b : natural;
    b1 : natural
  );
  constant c1 : natural := 0;
  constant c : natural := 0;
  constant c1 : natural := 0;
begin

  blk : block
    constant d1 : natural := 0;
    constant d : natural := 0;
    constant d1 : natural := 0;
  begin

  end block;

end entity;
",
        );

        let diagnostics = builder.analyze();
        check_diagnostics(
            diagnostics,
            expected_diagnostics(&code, &["a1", "b1", "c1", "d1"]),
        );
    }

    #[test]
    fn forbid_homographs_in_architecture_bodies() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
entity ent is
end entity;

architecture arch of ent is
  constant a1 : natural := 0;
  constant a : natural := 0;
  constant a1 : natural := 0;
begin

  blk : block
    constant b1 : natural := 0;
    constant b : natural := 0;
    constant b1 : natural := 0;
  begin
  end block;

end architecture;
",
        );

        let diagnostics = builder.analyze();
        check_diagnostics(diagnostics, expected_diagnostics(&code, &["a1", "b1"]));
    }

    #[test]
    fn forbid_homographs_of_type_declarations() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package pkg is
  constant a1 : natural := 0;
  type a1 is (foo, bar);
end package;
",
        );

        let diagnostics = builder.analyze();
        check_diagnostics(diagnostics, expected_diagnostics(&code, &["a1"]));
    }

    #[test]
    fn forbid_homographs_of_component_declarations() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package pkg is
  constant a1 : natural := 0;
  component a1 is
    port (clk : bit);
  end component;
end package;
",
        );

        let diagnostics = builder.analyze();
        check_diagnostics(diagnostics, expected_diagnostics(&code, &["a1"]));
    }

    #[test]
    fn forbid_homographs_of_file_declarations() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package pkg is
  constant a1 : natural := 0;
  file a1 : std.textio.text;
end package;
",
        );

        let diagnostics = builder.analyze();
        check_diagnostics(diagnostics, expected_diagnostics(&code, &["a1"]));
    }

    #[test]
    fn forbid_homographs_in_package_declarations() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package gpkg is
  generic (foo : natural);
end package;

package pkg is
  package a1 is new work.gpkg generic map (foo => bar);
  package a1 is new work.gpkg generic map (foo => bar);
end package;
",
        );

        let diagnostics = builder.analyze();
        check_diagnostics(diagnostics, expected_diagnostics(&code, &["a1"]));
    }

    #[test]
    fn forbid_homographs_in_attribute_declarations() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package pkg is
  attribute a1 : string;
  attribute a1 : string;
end package;
",
        );

        let diagnostics = builder.analyze();
        check_diagnostics(diagnostics, expected_diagnostics(&code, &["a1"]));
    }

    #[test]
    fn forbid_homographs_in_alias_declarations() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package pkg is
  alias a1 is foo;
  alias a1 is bar;

  -- Legal since subprograms are overloaded
  alias b1 is foo[return natural];
  alias b1 is bar[return boolean];
end package pkg;
",
        );

        let diagnostics = builder.analyze();
        check_diagnostics(diagnostics, expected_diagnostics(&code, &["a1"]));
    }

    #[test]
    fn forbid_homographs_for_overloaded_vs_non_overloaded() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package pkg is
  alias a1 is foo;
  alias a1 is bar[return boolean];

  function b1 return natural;
  constant b1 : natural := 0;
end package;
",
        );

        let diagnostics = builder.analyze();
        check_diagnostics(diagnostics, expected_diagnostics(&code, &["a1", "b1"]));
    }

    #[test]
    fn enum_literals_may_overload() {
        let mut builder = LibraryBuilder::new();
        builder.code(
            "libname",
            "
package pkg is
  type enum_t is (a1, b1);

  -- Ok since enumerations may overload
  type enum2_t is (a1, b1);
end package;
",
        );

        let diagnostics = builder.analyze();
        check_no_diagnostics(&diagnostics);
    }

    #[test]
    fn forbid_homograph_to_enum_literals() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package pkg is
  type enum_t is (a1, b1);
  constant a1 : natural := 0;
  function b1 return natural;
end package pkg;
",
        );

        let diagnostics = builder.analyze();
        check_diagnostics(diagnostics, expected_diagnostics(&code, &["a1"]));
    }

    #[test]
    fn forbid_homographs_in_interface_file_declarations() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package pkg is
  procedure proc(file a1, a, a1 : std.textio.text);
end package;
",
        );

        let diagnostics = builder.analyze();
        check_diagnostics(diagnostics, expected_diagnostics(&code, &["a1"]));
    }

    #[test]
    fn forbid_homographs_in_interface_type_declarations() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
entity ent is
  generic (
    type a1;
    type a1
  );
end entity;
",
        );

        let diagnostics = builder.analyze();
        check_diagnostics(diagnostics, expected_diagnostics(&code, &["a1"]));
    }

    #[test]
    fn forbid_homographs_in_interface_package_declarations() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package gpkg is
  generic (const : natural);
end package;

entity ent is
  generic (
    package a1 is new work.gpkg generic map (const => 0);
    package a1 is new work.gpkg generic map (const => 0)
  );
end entity;
",
        );

        let diagnostics = builder.analyze();
        check_diagnostics(diagnostics, expected_diagnostics(&code, &["a1"]));
    }

    #[test]
    fn forbid_homographs_in_entity_extended_declarative_regions() {
        let mut builder = LibraryBuilder::new();
        let ent = builder.code(
            "libname",
            "
entity ent is
  generic (
    constant g1 : natural;
    constant g2 : natural;
    constant g3 : natural;
    constant g4 : natural
  );
  port (
    signal g1 : natural;
    signal p1 : natural;
    signal p2 : natural;
    signal p3 : natural
  );
  constant g2 : natural := 0;
  constant p1 : natural := 0;
  constant e1 : natural := 0;
  constant e2 : natural := 0;
end entity;",
        );

        let arch1 = builder.code(
            "libname",
            "
architecture rtl of ent is
  constant g3 : natural := 0;
  constant p2 : natural := 0;
  constant e1 : natural := 0;
  constant a1 : natural := 0;
begin
end architecture;",
        );

        let arch2 = builder.code(
            "libname",
            "
architecture rtl2 of ent is
  constant a1 : natural := 0;
  constant e2 : natural := 0;
begin
end architecture;
",
        );

        let diagnostics = builder.analyze();
        let mut expected = expected_diagnostics(&ent, &["g1", "g2", "p1"]);
        expected.append(&mut expected_diagnostics_multi(
            &ent,
            &arch1,
            &["g3", "p2", "e1"],
        ));
        expected.append(&mut expected_diagnostics_multi(&ent, &arch2, &["e2"]));
        check_diagnostics(diagnostics, expected);
    }

    #[test]
    fn forbid_homographs_in_package_extended_declarative_regions() {
        let mut builder = LibraryBuilder::new();
        let pkg = builder.code(
            "libname",
            "
package pkg is
  generic (
    constant g1 : natural;
    constant g2 : natural
  );
  constant g1 : natural := 0;
end package;",
        );

        let body = builder.code(
            "libname",
            "
package body pkg is
  constant g1 : natural := 0;
  constant g2 : natural := 0;
  constant p1 : natural := 0;
end package body;",
        );

        let diagnostics = builder.analyze();
        let mut expected = expected_diagnostics(&pkg, &["g1"]);
        expected.append(&mut expected_diagnostics_multi(&pkg, &body, &["g1", "g2"]));
        check_diagnostics(diagnostics, expected);
    }

    #[test]
    fn check_library_clause_library_exists() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
library missing_lib;

entity ent is
end entity;
            ",
        );

        let diagnostics = builder.analyze();

        check_diagnostics(
            diagnostics,
            vec![Diagnostic::error(
                code.s1("missing_lib"),
                "No such library 'missing_lib'",
            )],
        )
    }

    #[test]
    fn library_clause_extends_into_secondary_units() {
        let mut builder = LibraryBuilder::new();
        builder.code(
            "libname",
            "
-- Package will be used for testing
package usepkg is
  constant const : natural := 0;
end package;

-- This should be visible also in architectures
library libname;

entity ent is
end entity;

use libname.usepkg;

architecture rtl of ent is
begin
end architecture;

-- This should be visible also in package body
library libname;
use libname.usepkg;

package pkg is
end package;

use usepkg.const;

package body pkg is
end package body;
            ",
        );

        let diagnostics = builder.analyze();

        check_no_diagnostics(&diagnostics);
    }

    /// Check that context clause in secondary units work
    #[test]
    fn context_clause_in_secondary_units() {
        let mut builder = LibraryBuilder::new();
        builder.code(
            "libname",
            "
package usepkg is
  constant const : natural := 0;
end package;

entity ent is
end entity;

library libname;

architecture rtl of ent is
  use libname.usepkg;
begin
end architecture;

package pkg is
end package;

library libname;

package body pkg is
  use libname.usepkg;
end package body;
            ",
        );

        let diagnostics = builder.analyze();

        check_no_diagnostics(&diagnostics);
    }

    #[test]
    fn secondary_units_share_only_root_region() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package pkg2 is
  constant const : natural := 0;
end package;

package pkg is
  use work.pkg2;
end package;

-- Does not work
use pkg2.const;

package body pkg is
  -- Does work
  use pkg2.const;
end package body;
",
        );
        let diagnostics = builder.analyze();
        check_diagnostics(
            diagnostics,
            vec![Diagnostic::error(
                code.s("pkg2", 3),
                "No declaration of 'pkg2'",
            )],
        )
    }

    #[test]
    fn check_library_clause_library_exists_in_context_declarations() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
context ctx is
  library missing_lib;
end context;
            ",
        );

        let diagnostics = builder.analyze();

        check_diagnostics(
            diagnostics,
            vec![Diagnostic::error(
                code.s1("missing_lib"),
                "No such library 'missing_lib'",
            )],
        )
    }

    #[test]
    fn context_clause_makes_names_visible() {
        let mut builder = LibraryBuilder::new();
        builder.code(
            "libname",
            "
-- Package will be used for testing
package usepkg is
  constant const : natural := 0;
end package;

context ctx is
  library libname;
  use libname.usepkg;
end context;


context work.ctx;
use usepkg.const;

package pkg is
end package;
            ",
        );

        let diagnostics = builder.analyze();

        check_no_diagnostics(&diagnostics);
    }

    #[test]
    fn library_std_is_pre_defined() {
        let mut builder = LibraryBuilder::new();
        builder.code(
            "libname",
            "
use std.textio.all;

entity ent is
end entity;
            ",
        );

        let diagnostics = builder.analyze();
        check_no_diagnostics(&diagnostics);
    }

    #[test]
    fn work_library_not_necessary_hint() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
library work;

entity ent is
end entity;
            ",
        );

        let diagnostics = builder.analyze();

        check_diagnostics(
            diagnostics,
            vec![Diagnostic::hint(
                code.s1("work"),
                "Library clause not necessary for current working library",
            )],
        )
    }

    use crate::source::Source;
    use std::collections::{hash_map::Entry, HashMap};

    struct LibraryBuilder {
        code_builder: CodeBuilder,
        libraries: HashMap<Symbol, Vec<Code>>,
    }

    impl LibraryBuilder {
        fn new_no_std() -> LibraryBuilder {
            LibraryBuilder {
                code_builder: CodeBuilder::new(),
                libraries: HashMap::default(),
            }
        }

        fn new() -> LibraryBuilder {
            let mut library = LibraryBuilder::new_no_std();
            library.code_from_source(
                "std",
                Source::inline(
                    "standard.vhd",
                    Arc::new(Latin1String::new(include_bytes!(
                        "../../../example_project/vhdl_libraries/2008/std/standard.vhd"
                    ))),
                ),
            );
            library.code_from_source(
                "std",
                Source::inline(
                    "textio.vhd",
                    Arc::new(Latin1String::new(include_bytes!(
                        "../../../example_project/vhdl_libraries/2008/std/textio.vhd"
                    ))),
                ),
            );
            library.code_from_source(
                "std",
                Source::inline(
                    "env.vhd",
                    Arc::new(Latin1String::new(include_bytes!(
                        "../../../example_project/vhdl_libraries/2008/std/env.vhd"
                    ))),
                ),
            );
            library
        }

        fn add_code(&mut self, library_name: &str, code: Code) {
            let library_name = self.code_builder.symbol(library_name);
            match self.libraries.entry(library_name) {
                Entry::Occupied(mut entry) => {
                    entry.get_mut().push(code.clone());
                }
                Entry::Vacant(entry) => {
                    entry.insert(vec![code.clone()]);
                }
            }
        }

        fn code(&mut self, library_name: &str, code: &str) -> Code {
            let code = self.code_builder.code(code);
            self.add_code(library_name, code.clone());
            code
        }

        fn code_from_source(&mut self, library_name: &str, source: Source) -> Code {
            let code = self.code_builder.code_from_source(source);
            self.add_code(library_name, code.clone());
            code
        }

        fn analyze(&self) -> Vec<Diagnostic> {
            let mut root = DesignRoot::new();
            let mut diagnostics = Vec::new();

            for (library_name, codes) in self.libraries.iter() {
                let design_files = codes.iter().map(|code| code.design_file()).collect();
                let library = Library::new(
                    library_name.clone(),
                    &self.code_builder.symbol("work"),
                    design_files,
                    &mut diagnostics,
                );
                root.add_library(library);
            }

            Analyzer::new(&root, &self.code_builder.symtab.clone()).analyze(&mut diagnostics);

            diagnostics
        }
    }

    #[test]
    fn check_use_clause_for_missing_design_unit() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package pkg is
end package;

package gpkg is
  generic (const : natural);
end package;

entity ent is
end entity;

architecture rtl of ent is
begin
end architecture;

configuration cfg of ent is
  for rtl
  end for;
end configuration;

package ipkg is new work.gpkg
  generic map (
    const => 1
  );

library libname;

-- Should work
use work.pkg;
use libname.pkg.all;
use libname.ent;
use libname.ipkg;
use libname.cfg;

use work.missing_pkg;
use libname.missing_pkg.all;


entity dummy is
end entity;
            ",
        );

        let diagnostics = builder.analyze();

        check_diagnostics(
            diagnostics,
            vec![
                Diagnostic::error(
                    code.s("missing_pkg", 1),
                    "No primary unit 'missing_pkg' within 'libname'",
                ),
                Diagnostic::error(
                    code.s("missing_pkg", 2),
                    "No primary unit 'missing_pkg' within 'libname'",
                ),
            ],
        )
    }

    #[test]
    fn check_use_clause_for_missing_library_clause() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package pkg is
end package;

use libname.pkg;

entity dummy is
end entity;
            ",
        );

        let diagnostics = builder.analyze();

        check_diagnostics(
            diagnostics,
            vec![Diagnostic::error(
                code.s("libname", 1),
                "No declaration of 'libname'",
            )],
        )
    }

    #[test]
    fn nested_use_clause_missing() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package pkg is
  constant const : natural := 0;
end package;

library libname;

entity ent is
end entity;

architecture rtl of ent is
  use libname.pkg; -- Works
  use libname.pkg1; -- Error
begin
  process
    use pkg.const; -- Works
    use libname.pkg1; -- Error
  begin
  end process;

  blk : block
    use pkg.const; -- Works
    use libname.pkg1; -- Error
  begin
  end block;

end architecture;
            ",
        );

        let diagnostics = builder.analyze();

        check_diagnostics(
            diagnostics,
            vec![
                Diagnostic::error(code.s("pkg1", 1), "No primary unit 'pkg1' within 'libname'"),
                Diagnostic::error(code.s("pkg1", 2), "No primary unit 'pkg1' within 'libname'"),
                Diagnostic::error(code.s("pkg1", 3), "No primary unit 'pkg1' within 'libname'"),
            ],
        )
    }

    #[test]
    fn check_context_reference_for_missing_context() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
context ctx is
end context;

context work.ctx;
context work.missing_ctx;

entity dummy is
end entity;
            ",
        );

        let diagnostics = builder.analyze();

        check_diagnostics(
            diagnostics,
            vec![Diagnostic::error(
                code.s1("missing_ctx"),
                "No primary unit 'missing_ctx' within 'libname'",
            )],
        )
    }

    #[test]
    fn check_context_reference_for_non_context() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package pkg is
end package;

context work.pkg;

entity dummy is
end entity;
            ",
        );

        let diagnostics = builder.analyze();

        check_diagnostics(
            diagnostics,
            vec![Diagnostic::error(
                code.s("pkg", 2),
                "'pkg' does not denote a context declaration",
            )],
        )
    }

    #[test]
    fn check_use_clause_and_context_clause_must_be_selected_name() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
library libname;

context libname;
use work;
use libname;

use work.pkg(0);
context work.ctx'range;

entity dummy is
end entity;
            ",
        );

        let diagnostics = builder.analyze();

        check_diagnostics(
            diagnostics,
            vec![
                Diagnostic::error(
                    code.s1("context libname;"),
                    "Context reference must be a selected name",
                ),
                Diagnostic::error(code.s1("use work;"), "Use clause must be a selected name"),
                Diagnostic::error(
                    code.s1("use libname;"),
                    "Use clause must be a selected name",
                ),
                Diagnostic::error(
                    code.s1("use work.pkg(0);"),
                    "Use clause must be a selected name",
                ),
                Diagnostic::error(
                    code.s1("context work.ctx'range;"),
                    "Context reference must be a selected name",
                ),
            ],
        );
    }

    #[test]
    fn check_two_stage_use_clause_for_missing_name() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package pkg is
  type enum_t is (alpha, beta);
  constant const : enum_t := alpha;
end package;

use work.pkg;
use pkg.const;
use pkg.const2;

package pkg2 is
end package;
            ",
        );
        let diagnostics = builder.analyze();
        check_diagnostics(
            diagnostics,
            vec![Diagnostic::error(
                code.s1("const2"),
                "No declaration of 'const2' within package 'libname.pkg'",
            )],
        );
    }
    #[test]
    fn check_use_clause_for_missing_name_in_package() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package pkg is
  type enum_t is (alpha, beta);
  constant const : enum_t := alpha;
end package;

use work.pkg.const;
use work.pkg.const2;

package pkg2 is
end package;
            ",
        );
        let diagnostics = builder.analyze();
        check_diagnostics(
            diagnostics,
            vec![Diagnostic::error(
                code.s1("const2"),
                "No declaration of 'const2' within package 'libname.pkg'",
            )],
        );
    }

    #[test]
    fn check_use_clause_for_missing_name_in_package_instance() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package gpkg is
  generic (constant gconst : natural);
  constant const : natural := 0;
end package;

package ipkg is new work.gpkg generic map (gconst => 0);

use work.ipkg.const;
use work.ipkg.const2;

-- @TODO should probably not be visible #19
-- use work.ipkg.gconst;

package pkg is
end package;
            ",
        );
        let diagnostics = builder.analyze();
        check_diagnostics(
            diagnostics,
            vec![
                // @TODO add use instance path in error diagnostic
                Diagnostic::error(
                    code.s1("const2"),
                    "No declaration of 'const2' within package instance 'libname.ipkg'",
                ),
            ],
        );
    }

    #[test]
    fn use_clause_cannot_reference_potentially_visible_name() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package pkg2 is
  constant const1 : natural := 0;
  constant const2 : natural := 0;
end package;


use work.pkg2.const1;

package pkg is
  use work.pkg2.const2;
  constant const3 : natural := 0;
end package;

use work.pkg.const1;
use work.pkg.const2;
use work.pkg.const3;

entity ent is
end entity;
            ",
        );
        let diagnostics = builder.analyze();
        check_diagnostics(
            diagnostics,
            vec![
                Diagnostic::error(
                    code.s("const1", 3),
                    "No declaration of 'const1' within package 'libname.pkg'",
                ),
                Diagnostic::error(
                    code.s("const2", 3),
                    "No declaration of 'const2' within package 'libname.pkg'",
                ),
            ],
        );
    }

    #[test]
    fn error_on_use_clause_with_double_all() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package pkg1 is
  constant const1 : natural := 0;
end package;

use work.all.all;
use work.all.foo;

entity ent is
end entity;
            ",
        );
        let diagnostics = builder.analyze();
        check_diagnostics(
            diagnostics,
            vec![
                Diagnostic::error(
                    code.s("work.all", 1),
                    "'.all' may not be the prefix of a selected name",
                ),
                Diagnostic::error(
                    code.s("work.all", 2),
                    "'.all' may not be the prefix of a selected name",
                ),
            ],
        );
    }

    #[test]
    fn an_uninstantiated_package_may_not_be_prefix_of_selected_name() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package gpkg is
  generic (const : natural);
end package;

use work.gpkg.all;

package pkg is
  use work.gpkg.const;
end package;
            ",
        );
        let diagnostics = builder.analyze();
        check_diagnostics(
            diagnostics,
            vec![
                Diagnostic::error(
                    code.s("work.gpkg", 1),
                    "Uninstantiated generic package 'libname.gpkg' may not be the prefix of a selected name",
                ),
                Diagnostic::error(
                    code.s("work.gpkg", 2),
                    "Uninstantiated generic package 'libname.gpkg' may not be the prefix of a selected name",
                ),
            ],
        );
    }

    #[test]
    fn package_name_must_be_visible_in_package_instance() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package gpkg is
  generic (const : natural);
end package;

package ipkg_err is new gpkg generic map (const => 0);
package ipkg_ok is new work.gpkg generic map (const => 0);

package nested is
  package ipkg_err is new gpkg generic map (const => 0);
  package ipkg_ok is new work.gpkg generic map (const => 0);
end package;
            ",
        );
        let diagnostics = builder.analyze();
        check_diagnostics(
            diagnostics,
            vec![
                Diagnostic::error(code.s("gpkg", 2), "No declaration of 'gpkg'"),
                Diagnostic::error(code.s("gpkg", 4), "No declaration of 'gpkg'"),
            ],
        );
    }

    #[test]
    fn package_name_must_be_an_uninstantiated_package_in_package_instance() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package pkg is
  constant const : natural := 0;
end package;

package ipkg is new work.pkg generic map (const => 0);

package nested is
  package ipkg2 is new work.pkg.const generic map (const => 0);
end package;
            ",
        );
        let diagnostics = builder.analyze();
        check_diagnostics(
            diagnostics,
            vec![
                Diagnostic::error(
                    code.s1("work.pkg"),
                    "'pkg' is not an uninstantiated generic package",
                ),
                Diagnostic::error(
                    code.s1("work.pkg.const"),
                    "'const' is not an uninstantiated generic package",
                ),
            ],
        );
    }

    #[test]
    fn use_clause_with_selected_all_design_units() {
        let mut builder = LibraryBuilder::new();
        builder.code(
            "libname",
            "
package pkg1 is
  constant const1 : natural := 0;
end package;

package pkg2 is
  constant const2 : natural := 0;
end package;

use work.all;
use pkg1.const1;
use pkg2.const2;

entity ent is
end entity;
            ",
        );
        let diagnostics = builder.analyze();
        check_no_diagnostics(&diagnostics);
    }

    #[test]
    fn use_clause_with_selected_all_names() {
        let mut builder = LibraryBuilder::new();
        builder.code(
            "libname",
            "
package pkg1 is
  type enum_t is (alpha, beta);
end package;

use work.pkg1.all;

entity ent is
end entity;

architecture rtl of ent is
  signal foo : enum_t;
begin
end architecture;
            ",
        );
        let diagnostics = builder.analyze();
        check_no_diagnostics(&diagnostics);
    }

    #[test]
    fn detects_circular_dependencies() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
use work.pkg2.const;

package pkg1 is
  constant const : natural := 0;
end package;

use work.pkg1.const;

package pkg2 is
  constant const : natural := 0;
end package;",
        );
        let diagnostics = builder.analyze();
        check_diagnostics(
            diagnostics,
            vec![
                Diagnostic::error(
                    code.s1("work.pkg1"),
                    "Found circular dependency when referencing 'libname.pkg1'",
                ),
                Diagnostic::error(
                    code.s1("work.pkg2"),
                    "Found circular dependency when referencing 'libname.pkg2'",
                ),
            ],
        );
    }

    #[test]
    fn detects_circular_dependencies_all() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
use work.pkg2.all;

package pkg1 is
  constant const : natural := 0;
end package;

use work.pkg1.all;

package pkg2 is
  constant const : natural := 0;
end package;",
        );
        let diagnostics = builder.analyze();
        check_diagnostics(
            diagnostics,
            vec![
                Diagnostic::error(
                    code.s1("work.pkg1"),
                    "Found circular dependency when referencing 'libname.pkg1'",
                ),
                Diagnostic::error(
                    code.s1("work.pkg2"),
                    "Found circular dependency when referencing 'libname.pkg2'",
                ),
            ],
        );
    }

    #[test]
    fn detects_circular_dependencies_only_when_used() {
        let mut builder = LibraryBuilder::new();
        builder.code(
            "libname",
            "
use work.all;

package pkg1 is
  constant const : natural := 0;
end package;

use work.pkg1.const;

package pkg2 is
  constant const : natural := 0;
end package;",
        );
        let diagnostics = builder.analyze();
        check_no_diagnostics(&diagnostics);
    }

    #[test]
    fn resolves_type_mark_in_subtype_indications() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package pkg1 is
  -- Object declaration
  constant const : natural := 0;
  constant const2 : missing := 0;

  -- File declaration
  file fil : std.textio.text;
  file fil2 : missing;

  -- Alias declaration
  alias foo : natural is const;
  alias foo2 : missing is const;

  -- Array type definiton
  type arr_t is array (natural range <>) of natural;
  type arr_t2 is array (natural range <>) of missing;

  -- Access type definiton
  type acc_t is access natural;
  type acc_t2 is access missing;

  -- Subtype definiton
  subtype sub_t is natural range 0 to 1;
  subtype sub_t2 is missing range 0 to 1;

  -- Record definition
  type rec_t is record
     f1 : natural;
     f2 : missing;
  end record;

  -- Interface file
  procedure p1 (fil : std.textio.text);
  procedure p2 (fil : missing);

  -- Interface object
  function f1 (const : natural) return natural;
  function f2 (const : missing) return natural;
end package;",
        );

        let expected = (0..9)
            .map(|idx| Diagnostic::error(code.s("missing", 1 + idx), "No declaration of 'missing'"))
            .collect();

        let diagnostics = builder.analyze();
        check_diagnostics(diagnostics, expected);
    }

    #[test]
    fn resolves_return_type() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package pkg is
  function f1 (const : natural) return natural;
  function f2 (const : natural) return missing;
end package;",
        );

        let diagnostics = builder.analyze();
        check_diagnostics(
            diagnostics,
            vec![Diagnostic::error(
                code.s1("missing"),
                "No declaration of 'missing'",
            )],
        );
    }

    #[test]
    fn resolves_attribute_declaration_type_mark() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package pkg is
  attribute attr : string;
  attribute attr2 : missing;
end package;",
        );

        let diagnostics = builder.analyze();
        check_diagnostics(
            diagnostics,
            vec![Diagnostic::error(
                code.s1("missing"),
                "No declaration of 'missing'",
            )],
        );
    }

    #[test]
    fn protected_type_is_visible_in_declaration() {
        let mut builder = LibraryBuilder::new();
        builder.code(
            "libname",
            "
package pkg1 is
  type prot_t is protected
     procedure proc(val : inout prot_t);
  end protected;

  type prot_t is protected body
     procedure proc(val : inout prot_t) is
     begin
     end;
  end protected body;
end package;",
        );

        let diagnostics = builder.analyze();
        check_no_diagnostics(&diagnostics);
    }

    #[test]
    fn use_all_in_package() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package pkg1 is
  subtype typ is natural range 0 to 1;
end package;

use work.pkg1.all;

package pkg2 is
  constant const : typ := 0;
  constant const2 : missing := 0;
end package;

",
        );
        let diagnostics = builder.analyze();
        check_diagnostics(
            diagnostics,
            vec![Diagnostic::error(
                code.s1("missing"),
                "No declaration of 'missing'",
            )],
        );
    }

    #[test]
    fn use_all_in_primary_package_instance() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package gpkg is
  generic (const : natural);
  subtype typ is natural range 0 to 1;
end package;

package ipkg is new work.gpkg generic map (const => 0);

use work.ipkg.all;

package pkg is
  constant const : typ := 0;
  constant const2 : missing := 0;
end package;

",
        );
        let diagnostics = builder.analyze();
        check_diagnostics(
            diagnostics,
            vec![Diagnostic::error(
                code.s1("missing"),
                "No declaration of 'missing'",
            )],
        );
    }

    #[test]
    fn use_of_interface_package_declaration() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package gpkg1 is
  generic (const : natural);
  subtype typ is natural range 0 to 1;
end package;

package gpkg2 is
  generic (package ipkg is new work.gpkg1 generic map (const => 1));
  use ipkg.typ;
  use ipkg.missing;
end package;
",
        );

        let diagnostics = builder.analyze();
        check_diagnostics(
            diagnostics,
            vec![Diagnostic::error(
                code.s1("missing"),
                "No declaration of 'missing' within package instance 'ipkg'",
            )],
        );
    }
    #[test]
    fn use_in_local_package_instance() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package gpkg is
  generic (const : natural);
  subtype typ is natural range 0 to 1;
end package;


package pkg is
  package ipkg is new work.gpkg generic map (const => 0);
  use ipkg.typ;

  constant const : typ := 0;
  constant const2 : missing := 0;
end package;

",
        );
        let diagnostics = builder.analyze();
        check_diagnostics(
            diagnostics,
            vec![Diagnostic::error(
                code.s1("missing"),
                "No declaration of 'missing'",
            )],
        );
    }

    #[test]
    fn use_all_in_local_package_instance() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package gpkg is
  generic (const : natural);
  subtype typ is natural range 0 to 1;
end package;


package pkg is
  package ipkg is new work.gpkg generic map (const => 0);
  use ipkg.all;

  constant const : typ := 0;
  constant const2 : missing := 0;
end package;

",
        );
        let diagnostics = builder.analyze();
        check_diagnostics(
            diagnostics,
            vec![Diagnostic::error(
                code.s1("missing"),
                "No declaration of 'missing'",
            )],
        );
    }
}
