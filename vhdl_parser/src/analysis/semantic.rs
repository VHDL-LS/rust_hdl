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
use crate::latin_1::Latin1String;
use crate::message::{Message, MessageHandler};
use crate::source::{SrcPos, WithPos};
use crate::symbol_table::{Symbol, SymbolTable};
use std::cell::RefCell;
use std::collections::hash_map::Entry;
use std::sync::Arc;

use self::fnv::FnvHashMap;
use fnv;

enum LookupResult<'n, 'a> {
    /// A single name was selected
    Single(VisibleDeclaration<'a>),
    /// A single name was selected
    AllWithin(&'n WithPos<Name>, VisibleDeclaration<'a>),
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
    fn push_into(self, messages: &mut dyn MessageHandler) {
        for dependency in self.path {
            messages.push(Message::error(
                dependency.location,
                format!(
                    "Found circular dependency when referencing '{}.{}'",
                    dependency.library_name, dependency.primary_unit_name
                ),
            ));
        }
    }
}

type AnalysisResult<'a> = Result<Arc<PrimaryUnitData<'a>>, CircularDependencyError>;

enum StartAnalysisResult<'s, 'a: 's> {
    AlreadyAnalyzed(AnalysisResult<'a>),
    NotYetAnalyzed(PendingAnalysis<'s, 'a>),
}

struct PendingAnalysis<'s, 'a: 's> {
    context: &'s AnalysisContext<'a>,
    key: (Symbol, Symbol),
}

impl<'s, 'a: 's> PendingAnalysis<'s, 'a> {
    fn new(context: &'s AnalysisContext<'a>, key: (Symbol, Symbol)) -> PendingAnalysis<'s, 'a> {
        PendingAnalysis { context, key }
    }

    fn end_analysis(self, data: PrimaryUnitData<'a>) -> AnalysisResult<'a> {
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

impl<'s, 'a: 's> Drop for PendingAnalysis<'s, 'a> {
    fn drop(&mut self) {
        self.context.locked.borrow_mut().remove(&self.key);
        self.context.path.borrow_mut().pop();
    }
}

struct AnalysisContext<'a> {
    primary_unit_data: RefCell<FnvHashMap<(Symbol, Symbol), AnalysisResult<'a>>>,
    locked: RefCell<FnvHashMap<(Symbol, Symbol), ()>>,
    path: RefCell<Vec<Option<Dependency>>>,
}

impl<'a> AnalysisContext<'a> {
    fn new() -> AnalysisContext<'a> {
        AnalysisContext {
            primary_unit_data: RefCell::new(FnvHashMap::default()),
            locked: RefCell::new(FnvHashMap::default()),
            path: RefCell::new(Vec::new()),
        }
    }

    fn start_analysis<'s>(
        &'s self,
        // The optional location where the design unit was used
        entry_point: Option<SrcPos>,
        library_name: &Symbol,
        primary_unit_name: &Symbol,
    ) -> StartAnalysisResult<'s, 'a> {
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

            // Only provide full path error to one unit so that duplicate messages are not created
            StartAnalysisResult::AlreadyAnalyzed(path_result)
        } else {
            StartAnalysisResult::NotYetAnalyzed(PendingAnalysis::new(self, key))
        }
    }

    fn get_result(
        &self,
        library_name: &Symbol,
        primary_unit_name: &Symbol,
    ) -> Option<AnalysisResult<'a>> {
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
    library_regions: FnvHashMap<Symbol, DeclarativeRegion<'a, 'a>>,
    analysis_context: AnalysisContext<'a>,
}

impl<'r, 'a: 'r> Analyzer<'a> {
    pub fn new(root: &'a DesignRoot, symtab: &Arc<SymbolTable>) -> Analyzer<'a> {
        let mut library_regions = FnvHashMap::default();
        let mut messages = Vec::new();

        for library in root.iter_libraries() {
            let mut region = DeclarativeRegion::new(None);

            for package in library.packages() {
                let decl = VisibleDeclaration {
                    designator: Designator::Identifier(package.package.unit.ident.item.clone()),
                    decl: AnyDeclaration::Package(library, package),
                    decl_pos: Some(package.package.unit.ident.pos.clone()),
                    may_overload: false,
                };
                region.add(decl, &mut messages);
            }

            for context in library.contexts() {
                let decl = VisibleDeclaration {
                    designator: Designator::Identifier(context.ident.item.clone()),
                    decl: AnyDeclaration::Context(context),
                    decl_pos: Some(context.ident.pos.clone()),
                    may_overload: false,
                };
                region.add(decl, &mut messages);
            }

            for entity in library.entities() {
                let decl = VisibleDeclaration {
                    designator: Designator::Identifier(entity.entity.unit.ident.item.clone()),
                    decl: AnyDeclaration::Entity(entity),
                    decl_pos: Some(entity.entity.unit.ident.pos.clone()),
                    may_overload: false,
                };
                region.add(decl, &mut messages);

                for configuration in entity.configurations() {
                    let decl = VisibleDeclaration {
                        designator: Designator::Identifier(configuration.ident().item.clone()),
                        decl: AnyDeclaration::Configuration(configuration),
                        decl_pos: Some(configuration.ident().pos.clone()),
                        may_overload: false,
                    };
                    region.add(decl, &mut messages);
                }
            }

            for instance in library.package_instances() {
                let decl = VisibleDeclaration {
                    designator: Designator::Identifier(instance.ident().item.clone()),
                    decl: AnyDeclaration::PackageInstance(library, instance),
                    decl_pos: Some(instance.ident().pos.clone()),
                    may_overload: false,
                };
                region.add(decl, &mut messages);
            }

            library_regions.insert(library.name.clone(), region);
        }

        assert!(messages.is_empty());

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
        region: &DeclarativeRegion<'_, 'a>,
        name: &'n WithPos<Name>,
    ) -> Result<LookupResult<'n, 'a>, Message> {
        match name.item {
            Name::Selected(ref prefix, ref suffix) => {
                let visible_decl = {
                    match self.lookup_selected_name(region, prefix)? {
                        LookupResult::Single(visible_decl) => visible_decl,
                        LookupResult::AllWithin(..) => {
                            return Err(Message::error(
                                prefix.as_ref(),
                                "'.all' may not be the prefix of a selected name",
                            ));
                        }
                        others => return Ok(others),
                    }
                };

                match visible_decl.decl {
                    AnyDeclaration::Library(ref library) => {
                        if let Some(visible_decl) =
                            self.library_regions[&library.name].lookup(&suffix.item, false)
                        {
                            Ok(LookupResult::Single(visible_decl.clone()))
                        } else {
                            Err(Message::error(
                                suffix.as_ref(),
                                format!(
                                    "No primary unit '{}' within '{}'",
                                    suffix.item, &library.name
                                ),
                            ))
                        }
                    }

                    AnyDeclaration::Package(ref library, ref package) => {
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
                                Err(Message::error(
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
                            // Circular dependency, message will never be used
                            Err(Message::error(&prefix.pos, ""))
                        }
                    }

                    AnyDeclaration::PackageInstance(ref library, ref instance) => {
                        if let Ok(data) = self.analyze_package_instance_unit(
                            Some(prefix.pos.clone()),
                            library,
                            instance,
                        ) {
                            if let Some(visible_decl) = data.region.lookup(&suffix.item, false) {
                                Ok(LookupResult::Single(visible_decl.clone()))
                            } else {
                                Err(Message::error(
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
                            // Circular dependency, message will never be used
                            Err(Message::error(&prefix.pos, ""))
                        }
                    }

                    AnyDeclaration::LocalPackageInstance(ref instance_name, ref data) => {
                        if let Some(visible_decl) = data.region.lookup(&suffix.item, false) {
                            Ok(LookupResult::Single(visible_decl.clone()))
                        } else {
                            Err(Message::error(
                                suffix.as_ref(),
                                format!(
                                    "No declaration of '{}' within package instance '{}'",
                                    suffix.item, &instance_name.item
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
                LookupResult::AllWithin(..) => Err(Message::error(
                    prefix.as_ref(),
                    "'.all' may not be the prefix of a selected name",
                )),
                others => Ok(others),
            },
            Name::Designator(ref designator) => {
                if let Some(visible_item) = region.lookup(&designator, true) {
                    Ok(LookupResult::Single(visible_item.clone()))
                } else {
                    Err(Message::error(
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
        region: &mut DeclarativeRegion<'_, 'a>,
        decl: &'a InterfaceDeclaration,
        messages: &mut dyn MessageHandler,
    ) {
        match decl {
            InterfaceDeclaration::File(ref file_decl) => {
                self.analyze_subtype_indicaton(region, &file_decl.subtype_indication, messages);
                region.add(
                    VisibleDeclaration::new(&file_decl.ident, AnyDeclaration::Interface(decl)),
                    messages,
                );
            }
            InterfaceDeclaration::Object(ref object_decl) => {
                self.analyze_subtype_indicaton(region, &object_decl.subtype_indication, messages);
                region.add(
                    VisibleDeclaration::new(&object_decl.ident, AnyDeclaration::Interface(decl)),
                    messages,
                );
            }
            InterfaceDeclaration::Type(ref ident) => {
                region.add(
                    VisibleDeclaration::new(ident, AnyDeclaration::Interface(decl)),
                    messages,
                );
            }
            InterfaceDeclaration::Subprogram(subpgm, ..) => {
                self.analyze_subprogram_declaration(region, subpgm, messages);
                region.add(
                    VisibleDeclaration::new(subpgm.designator(), AnyDeclaration::Interface(decl))
                        .with_overload(true),
                    messages,
                );
            }
            InterfaceDeclaration::Package(ref instance) => {
                match self.analyze_package_instance_name(region, &instance.package_name) {
                    Ok(package_region) => region.add(
                        VisibleDeclaration::new(
                            &instance.ident,
                            AnyDeclaration::LocalPackageInstance(&instance.ident, package_region),
                        ),
                        messages,
                    ),
                    Err(msg) => {
                        messages.push(msg);
                    }
                }
            }
        }
    }

    fn analyze_interface_list(
        &self,
        region: &mut DeclarativeRegion<'_, 'a>,
        declarations: &'a [InterfaceDeclaration],
        messages: &mut dyn MessageHandler,
    ) {
        for decl in declarations.iter() {
            self.analyze_interface_declaration(region, decl, messages);
        }
    }

    #[allow(clippy::ptr_arg)]
    fn lookup_type_mark(
        &self,
        region: &DeclarativeRegion<'_, 'a>,
        type_mark: &WithPos<SelectedName>,
    ) -> Result<VisibleDeclaration<'a>, Message> {
        let type_mark_name = type_mark.clone().into();
        match self.lookup_selected_name(region, &type_mark_name)? {
            LookupResult::Single(visible_decl) => Ok(visible_decl),
            _ => {
                // Cannot really happen with SelectedName but refactoring might change it...
                Err(Message::error(
                    &type_mark_name.pos,
                    "Invalid name for type mark",
                ))
            }
        }
    }

    fn analyze_subtype_indicaton(
        &self,
        region: &mut DeclarativeRegion<'_, 'a>,
        subtype_indication: &'a SubtypeIndication,
        messages: &mut dyn MessageHandler,
    ) {
        if let Err(msg) = self.lookup_type_mark(region, &subtype_indication.type_mark) {
            messages.push(msg);
        }
    }

    fn analyze_subprogram_declaration(
        &self,
        parent: &DeclarativeRegion<'_, 'a>,
        subprogram: &'a SubprogramDeclaration,
        messages: &mut dyn MessageHandler,
    ) {
        let mut region = DeclarativeRegion::new(Some(parent));

        match subprogram {
            SubprogramDeclaration::Function(fun) => {
                self.analyze_interface_list(&mut region, &fun.parameter_list, messages);
                if let Err(msg) = self.lookup_type_mark(&parent, &fun.return_type) {
                    messages.push(msg);
                }
            }
            SubprogramDeclaration::Procedure(proc) => {
                self.analyze_interface_list(&mut region, &proc.parameter_list, messages);
            }
        }
        region.close_both(messages);
    }

    fn analyze_declaration(
        &self,
        region: &mut DeclarativeRegion<'_, 'a>,
        decl: &'a Declaration,
        messages: &mut dyn MessageHandler,
    ) {
        match decl {
            Declaration::Alias(alias) => {
                if let Some(ref subtype_indication) = alias.subtype_indication {
                    self.analyze_subtype_indicaton(region, subtype_indication, messages);
                }
                region.add(
                    VisibleDeclaration::new(
                        alias.designator.clone(),
                        AnyDeclaration::Declaration(decl),
                    )
                    .with_overload(alias.signature.is_some()),
                    messages,
                );
            }
            Declaration::Object(ref object_decl) => {
                self.analyze_subtype_indicaton(region, &object_decl.subtype_indication, messages);
                region.add(
                    VisibleDeclaration::new(&object_decl.ident, AnyDeclaration::Declaration(decl)),
                    messages,
                );
            }
            Declaration::File(ref file_decl) => {
                self.analyze_subtype_indicaton(region, &file_decl.subtype_indication, messages);
                region.add(
                    VisibleDeclaration::new(&file_decl.ident, AnyDeclaration::Declaration(decl)),
                    messages,
                );
            }
            Declaration::Component(ref component) => {
                region.add(
                    VisibleDeclaration::new(&component.ident, AnyDeclaration::Declaration(decl)),
                    messages,
                );

                {
                    let mut region = DeclarativeRegion::new(Some(region));
                    self.analyze_interface_list(&mut region, &component.generic_list, messages);
                    region.close_both(messages);
                }

                {
                    let mut region = DeclarativeRegion::new(Some(region));
                    self.analyze_interface_list(&mut region, &component.port_list, messages);
                    region.close_both(messages);
                }
            }
            Declaration::Attribute(ref attr) => match attr {
                Attribute::Declaration(ref attr_decl) => {
                    if let Err(msg) = self.lookup_type_mark(region, &attr_decl.type_mark) {
                        messages.push(msg);
                    }
                    region.add(
                        VisibleDeclaration::new(
                            &attr_decl.ident,
                            AnyDeclaration::Declaration(decl),
                        ),
                        messages,
                    );
                }
                // @TODO Ignored for now
                Attribute::Specification(..) => {}
            },
            Declaration::SubprogramBody(body) => {
                region.add(
                    VisibleDeclaration::new(
                        body.specification.designator(),
                        AnyDeclaration::Declaration(decl),
                    )
                    .with_overload(true),
                    messages,
                );
                self.analyze_subprogram_declaration(region, &body.specification, messages);
                let mut region = DeclarativeRegion::new(Some(region));
                self.analyze_declarative_part(&mut region, &body.declarations, messages);
            }
            Declaration::SubprogramDeclaration(subdecl) => {
                region.add(
                    VisibleDeclaration::new(
                        subdecl.designator(),
                        AnyDeclaration::Declaration(decl),
                    )
                    .with_overload(true),
                    messages,
                );
                self.analyze_subprogram_declaration(region, &subdecl, messages);
            }

            // @TODO Ignored for now
            Declaration::Use(ref use_clause) => {
                self.analyze_use_clause(region, &use_clause.item, &use_clause.pos, messages);
            }
            Declaration::Package(ref instance) => {
                match self.analyze_package_instance(region, instance) {
                    Ok(package_region) => region.add(
                        VisibleDeclaration::new(
                            &instance.ident,
                            AnyDeclaration::LocalPackageInstance(&instance.ident, package_region),
                        ),
                        messages,
                    ),
                    Err(msg) => {
                        messages.push(msg);
                    }
                }
            }
            Declaration::Configuration(..) => {}
            Declaration::Type(TypeDeclaration {
                ref ident,
                def: TypeDefinition::Enumeration(ref enumeration),
            }) => {
                region.add(
                    VisibleDeclaration::new(ident, AnyDeclaration::Declaration(decl)),
                    messages,
                );
                for literal in enumeration.iter() {
                    region.add(
                        VisibleDeclaration::new(
                            literal.clone().map_into(|lit| lit.into_designator()),
                            AnyDeclaration::Enum(literal),
                        )
                        .with_overload(true),
                        messages,
                    )
                }
            }
            Declaration::Type(ref type_decl) => {
                // Protected types are visible inside their declaration
                region.add(
                    VisibleDeclaration::new(&type_decl.ident, AnyDeclaration::Declaration(decl)),
                    messages,
                );

                match type_decl.def {
                    TypeDefinition::ProtectedBody(ref body) => {
                        let mut region = DeclarativeRegion::new(Some(region));
                        self.analyze_declarative_part(&mut region, &body.decl, messages);
                    }
                    TypeDefinition::Protected(ref prot_decl) => {
                        for item in prot_decl.items.iter() {
                            match item {
                                ProtectedTypeDeclarativeItem::Subprogram(subprogram) => {
                                    self.analyze_subprogram_declaration(
                                        region, subprogram, messages,
                                    );
                                }
                            }
                        }
                    }
                    TypeDefinition::Record(ref element_decls) => {
                        let mut record_region = DeclarativeRegion::new(None);
                        for elem_decl in element_decls.iter() {
                            self.analyze_subtype_indicaton(region, &elem_decl.subtype, messages);
                            record_region.add(
                                VisibleDeclaration::new(
                                    &elem_decl.ident,
                                    AnyDeclaration::Element(elem_decl),
                                ),
                                messages,
                            );
                        }
                        record_region.close_both(messages);
                    }
                    TypeDefinition::Access(ref subtype_indication) => {
                        self.analyze_subtype_indicaton(region, subtype_indication, messages);
                    }
                    TypeDefinition::Array(.., ref subtype_indication) => {
                        self.analyze_subtype_indicaton(region, subtype_indication, messages);
                    }
                    TypeDefinition::Subtype(ref subtype_indication) => {
                        self.analyze_subtype_indicaton(region, subtype_indication, messages);
                    }
                    _ => {}
                }
            }
        }
    }

    fn analyze_declarative_part(
        &self,
        region: &mut DeclarativeRegion<'_, 'a>,
        declarations: &'a [Declaration],
        messages: &mut dyn MessageHandler,
    ) {
        for decl in declarations.iter() {
            self.analyze_declaration(region, decl, messages);
        }
    }

    fn analyze_use_clause(
        &self,
        region: &mut DeclarativeRegion<'_, 'a>,
        use_clause: &UseClause,
        use_pos: &SrcPos,
        messages: &mut dyn MessageHandler,
    ) {
        for name in use_clause.name_list.iter() {
            match name.item {
                Name::Selected(..) => {}
                Name::SelectedAll(..) => {}
                _ => {
                    messages.push(Message::error(
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
                    match visible_decl.decl {
                        AnyDeclaration::Library(ref library) => {
                            region
                                .make_all_potentially_visible(&self.library_regions[&library.name]);
                        }
                        AnyDeclaration::Package(ref library, ref package) => {
                            if package.is_generic() {
                                messages.push(uninstantiated_package_prefix_error(
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
                        AnyDeclaration::PackageInstance(ref library, ref package) => {
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
                    messages.push(Message::error(
                        &use_pos,
                        "Use clause must be a selected name",
                    ));
                }
                Err(msg) => {
                    messages.push(msg);
                }
            }
        }
    }

    fn analyze_context_clause(
        &self,
        region: &mut DeclarativeRegion<'_, 'a>,
        context_clause: &[WithPos<ContextItem>],
        messages: &mut dyn MessageHandler,
    ) {
        for context_item in context_clause.iter() {
            match context_item.item {
                ContextItem::Library(LibraryClause { ref name_list }) => {
                    for library_name in name_list.iter() {
                        if self.work_sym == library_name.item {
                            messages.push(Message::hint(
                                &library_name,
                                "Library clause not necessary for current working library",
                            ))
                        } else if let Some(library) = self.root.get_library(&library_name.item) {
                            region.make_library_visible(&library.name, library);
                        } else {
                            messages.push(Message::error(
                                &library_name,
                                format!("No such library '{}'", library_name.item),
                            ));
                        }
                    }
                }
                ContextItem::Use(ref use_clause) => {
                    self.analyze_use_clause(region, use_clause, &context_item.pos, messages);
                }
                ContextItem::Context(ContextReference { ref name_list }) => {
                    for name in name_list {
                        match name.item {
                            Name::Selected(..) => {}
                            _ => {
                                messages.push(Message::error(
                                    &context_item,
                                    "Context reference must be a selected name",
                                ));
                                continue;
                            }
                        }

                        match self.lookup_selected_name(&region, &name) {
                            Ok(LookupResult::Single(visible_decl)) => {
                                match visible_decl.decl {
                                    // OK
                                    AnyDeclaration::Context(ref context) => {
                                        // Error will be given when
                                        // analyzing the context
                                        // clause specifically and
                                        // shall not be duplicated
                                        // here
                                        let mut ignore_messages = Vec::new();
                                        self.analyze_context_clause(
                                            region,
                                            &context.items,
                                            &mut ignore_messages,
                                        );
                                    }
                                    _ => {
                                        // @TODO maybe lookup should return the source position of the suffix
                                        if let Name::Selected(_, ref suffix) = name.item {
                                            messages.push(Message::error(
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
                                messages.push(Message::error(
                                    &context_item,
                                    "Context reference must be a selected name",
                                ));
                            }
                            Err(msg) => {
                                messages.push(msg);
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
        library: &'a Library,
        package: &'a PackageDesignUnit,
    ) -> AnalysisResult<'a> {
        self.analyze_package_declaration_unit(entry_point, library, package)
    }

    fn analyze_generate_body(
        &self,
        parent: &DeclarativeRegion<'_, 'a>,
        body: &'a GenerateBody,
        messages: &mut dyn MessageHandler,
    ) {
        let mut region = DeclarativeRegion::new(Some(parent));

        if let Some(ref decl) = body.decl {
            self.analyze_declarative_part(&mut region, &decl, messages);
        }
        self.analyze_concurrent_part(&region, &body.statements, messages);
    }

    fn analyze_concurrent_statement(
        &self,
        parent: &DeclarativeRegion<'_, 'a>,
        statement: &'a LabeledConcurrentStatement,
        messages: &mut dyn MessageHandler,
    ) {
        match statement.statement {
            ConcurrentStatement::Block(ref block) => {
                let mut region = DeclarativeRegion::new(Some(parent));
                self.analyze_declarative_part(&mut region, &block.decl, messages);
                self.analyze_concurrent_part(&region, &block.statements, messages);
            }
            ConcurrentStatement::Process(ref process) => {
                let mut region = DeclarativeRegion::new(Some(parent));
                self.analyze_declarative_part(&mut region, &process.decl, messages);
            }
            ConcurrentStatement::ForGenerate(ref gen) => {
                self.analyze_generate_body(parent, &gen.body, messages);
            }
            ConcurrentStatement::IfGenerate(ref gen) => {
                for conditional in gen.conditionals.iter() {
                    self.analyze_generate_body(parent, &conditional.item, messages);
                }
                if let Some(ref else_item) = gen.else_item {
                    self.analyze_generate_body(parent, else_item, messages);
                }
            }
            ConcurrentStatement::CaseGenerate(ref gen) => {
                for alternative in gen.alternatives.iter() {
                    self.analyze_generate_body(parent, &alternative.item, messages);
                }
            }
            _ => {}
        }
    }

    fn analyze_concurrent_part(
        &self,
        parent: &DeclarativeRegion<'_, 'a>,
        statements: &'a [LabeledConcurrentStatement],
        messages: &mut dyn MessageHandler,
    ) {
        for statement in statements.iter() {
            self.analyze_concurrent_statement(parent, statement, messages);
        }
    }

    fn analyze_architecture_body(
        &self,
        entity_region: &mut DeclarativeRegion<'_, 'a>,
        architecture: &'a ArchitectureBody,
        messages: &mut dyn MessageHandler,
    ) {
        self.analyze_declarative_part(entity_region, &architecture.decl, messages);
        self.analyze_concurrent_part(entity_region, &architecture.statements, messages);
    }

    fn analyze_entity_declaration(
        &self,
        region: &mut DeclarativeRegion<'_, 'a>,
        entity: &'a EntityDeclaration,
        messages: &mut dyn MessageHandler,
    ) {
        if let Some(ref list) = entity.generic_clause {
            self.analyze_interface_list(region, list, messages);
        }
        if let Some(ref list) = entity.port_clause {
            self.analyze_interface_list(region, list, messages);
        }
        self.analyze_declarative_part(region, &entity.decl, messages);
        self.analyze_concurrent_part(region, &entity.statements, messages);
    }

    /// Add implicit context clause for all packages except STD.STANDARD
    /// library STD, WORK;
    /// use STD.STANDARD.all;
    fn add_implicit_context_clause(
        &self,
        region: &mut DeclarativeRegion<'a, 'a>,
        work: &'a Library,
    ) {
        region.make_library_visible(&self.work_sym, work);

        // @TODO maybe add warning if standard library is missing
        if let Some(library) = self.root.get_library(&self.std_sym) {
            region.make_library_visible(&self.std_sym, library);

            if let Some(VisibleDeclaration {
                decl: AnyDeclaration::Package(.., standard_pkg),
                ..
            }) = self.library_regions[&library.name].lookup(&self.standard_designator, false)
            {
                let standard_pkg_region = &self
                    .analysis_context
                    .get_result(&library.name, standard_pkg.package.name())
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
        parent: &'r DeclarativeRegion<'r, 'a>,
        package: &'a PackageDeclaration,
        messages: &mut dyn MessageHandler,
    ) -> DeclarativeRegion<'r, 'a> {
        let mut region = DeclarativeRegion::new(Some(parent)).in_package_declaration();
        if let Some(ref list) = package.generic_clause {
            self.analyze_interface_list(&mut region, list, messages);
        }
        self.analyze_declarative_part(&mut region, &package.decl, messages);
        region
    }

    fn analyze_package_declaration_unit(
        &self,
        // The optional entry point where the package declarartion was used
        // None if the package was directly analyzed and not due to a use clause
        entry_point: Option<SrcPos>,
        library: &'a Library,
        package: &'a PackageDesignUnit,
    ) -> AnalysisResult<'a> {
        let mut messages = Vec::new();

        match self.analysis_context.start_analysis(
            entry_point,
            &library.name,
            package.package.name(),
        ) {
            StartAnalysisResult::NotYetAnalyzed(pending) => {
                let mut root_region = DeclarativeRegion::new(None);
                if !(library.name == self.std_sym && *package.package.name() == self.standard_sym) {
                    self.add_implicit_context_clause(&mut root_region, library);
                }

                self.analyze_context_clause(
                    &mut root_region,
                    &package.package.context_clause,
                    &mut messages,
                );

                let mut region = self.analyze_package_declaration(
                    &root_region,
                    &package.package.unit,
                    &mut messages,
                );

                if package.body.is_some() {
                    region.close_immediate(&mut messages);
                } else {
                    region.close_both(&mut messages);
                }

                pending.end_analysis(PrimaryUnitData::new(messages, region.into_owned_parent()))
            }

            StartAnalysisResult::AlreadyAnalyzed(result) => result,
        }
    }

    fn analyze_package_body_unit(
        &self,
        primary_region: &DeclarativeRegion<'_, 'a>,
        package: &'a PackageDesignUnit,
        messages: &mut dyn MessageHandler,
    ) {
        if let Some(ref body) = package.body {
            let mut root_region = primary_region
                .clone_parent()
                .expect("Expected parent region");;
            self.analyze_context_clause(&mut root_region, &body.context_clause, messages);
            let mut region = primary_region.clone().into_extended(&root_region);
            self.analyze_declarative_part(&mut region, &body.unit.decl, messages);
            region.close_both(messages);
        }
    }

    pub fn analyze_package(
        &self,
        library: &'a Library,
        package: &'a PackageDesignUnit,
        messages: &mut dyn MessageHandler,
    ) {
        match self.analyze_package_declaration_unit(None, library, package) {
            Ok(data) => {
                data.push_to(messages);
                self.analyze_package_body_unit(&data.region, &package, messages);
            }
            Err(circular_dependency) => {
                circular_dependency.push_into(messages);
            }
        };
    }

    /// Returns a reference to the the uninstantiated package
    pub fn analyze_package_instance(
        &self,
        parent: &'r DeclarativeRegion<'r, 'a>,
        package_instance: &'a PackageInstantiation,
    ) -> Result<Arc<PrimaryUnitData<'a>>, Message> {
        self.analyze_package_instance_name(parent, &package_instance.package_name)
    }

    /// Returns a reference to the the uninstantiated package
    #[allow(clippy::ptr_arg)]
    pub fn analyze_package_instance_name(
        &self,
        parent: &'r DeclarativeRegion<'r, 'a>,
        package_name: &WithPos<SelectedName>,
    ) -> Result<Arc<PrimaryUnitData<'a>>, Message> {
        let entry_point = package_name.pos.clone();
        let package_name = package_name.clone().into();

        match self.lookup_selected_name(parent, &package_name)? {
            LookupResult::Single(visible_decl) => {
                if let AnyDeclaration::Package(ref library, ref package) = visible_decl.decl {
                    if package.is_generic() {
                        if let Ok(data) =
                            self.get_package_result(Some(entry_point.clone()), library, package)
                        {
                            return Ok(data.clone());
                        } else {
                            return Err(Message::error(
                                &entry_point,
                                format!(
                                    "'Could not instantiate package '{}.{}' with circular dependency'",
                                    &library.name, package.package.name()
                                ),
                            ));
                        }
                    }
                }
                Err(Message::error(
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
                Err(Message::error(
                    &package_name.pos,
                    "Invalid selected name for generic package",
                ))
            }
        }
    }

    fn analyze_package_instance_unit(
        &self,
        entry_point: Option<SrcPos>,
        library: &'a Library,
        package_instance: &'a DesignUnit<PackageInstantiation>,
    ) -> AnalysisResult<'a> {
        let mut messages = Vec::new();

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
                    &mut messages,
                );

                match self.analyze_package_instance(&region, &package_instance.unit) {
                    Ok(data) => {
                        // @TODO avoid clone?
                        pending.end_analysis(PrimaryUnitData::new(messages, data.region.clone()))
                    }
                    Err(msg) => {
                        messages.push(msg);
                        // Failed to analyze, add empty region
                        pending.end_analysis(PrimaryUnitData::new(
                            messages,
                            DeclarativeRegion::new(None),
                        ))
                    }
                }
            }
            StartAnalysisResult::AlreadyAnalyzed(result) => result,
        }
    }

    pub fn analyze_library(&self, library: &'a Library, messages: &mut dyn MessageHandler) {
        for package in library.packages() {
            self.analyze_package(library, package, messages);
        }

        for package_instance in library.package_instances() {
            match self.analyze_package_instance_unit(None, library, package_instance) {
                Ok(data) => {
                    data.push_to(messages);
                }
                Err(circular_dependency) => {
                    circular_dependency.push_into(messages);
                }
            }
        }

        for context in library.contexts() {
            let mut root_region = DeclarativeRegion::new(None);
            self.add_implicit_context_clause(&mut root_region, library);
            self.analyze_context_clause(&mut root_region, &context.items, messages);
        }

        for entity in library.entities() {
            let mut root_region = DeclarativeRegion::new(None);
            self.add_implicit_context_clause(&mut root_region, library);
            self.analyze_context_clause(&mut root_region, &entity.entity.context_clause, messages);
            let mut region = DeclarativeRegion::new(Some(&root_region));
            self.analyze_entity_declaration(&mut region, &entity.entity.unit, messages);
            region.close_immediate(messages);
            for architecture in entity.architectures.values() {
                let mut root_region = region.clone();
                self.analyze_context_clause(
                    &mut root_region,
                    &architecture.context_clause,
                    messages,
                );
                let mut region = region.clone().into_extended(&root_region);
                self.analyze_architecture_body(&mut region, &architecture.unit, messages);
                region.close_both(messages);
            }
        }
    }

    pub fn analyze(&self, messages: &mut dyn MessageHandler) {
        // Analyze standard library first
        if let Some(library) = self.root.get_library(&self.std_sym) {
            let standard_package = library
                .package(&self.standard_sym)
                .expect("Failed to find package STD.STANDARD");
            self.analyze_package(library, standard_package, messages);
            for package in library.packages() {
                if *package.package.name() != self.standard_sym {
                    self.analyze_package(library, package, messages);
                }
            }
        }

        for library in self.root.iter_libraries() {
            // Standard library already analyzed
            if library.name == self.std_sym {
                continue;
            }

            self.analyze_library(library, messages);
        }
    }
}

fn uninstantiated_package_prefix_error(
    prefix: &SrcPos,
    library: &Library,
    package: &PackageDesignUnit,
) -> Message {
    Message::error(
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
    use crate::message::Message;
    use crate::test_util::{check_messages, check_no_messages, Code, CodeBuilder};

    fn expected_message(code: &Code, name: &str, occ1: usize, occ2: usize) -> Message {
        Message::error(
            code.s(&name, occ2),
            format!("Duplicate declaration of '{}'", &name),
        )
        .related(code.s(&name, occ1), "Previously defined here")
    }

    fn expected_messages(code: &Code, names: &[&str]) -> Vec<Message> {
        let mut messages = Vec::new();
        for name in names {
            messages.push(expected_message(code, name, 1, 2));
        }
        messages
    }

    fn expected_messages_multi(code1: &Code, code2: &Code, names: &[&str]) -> Vec<Message> {
        let mut messages = Vec::new();
        for name in names {
            messages.push(
                Message::error(
                    code2.s1(&name),
                    format!("Duplicate declaration of '{}'", &name),
                )
                .related(code1.s1(&name), "Previously defined here"),
            )
        }
        messages
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

        let messages = builder.analyze();
        check_no_messages(&messages);
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

        let messages = builder.analyze();
        check_no_messages(&messages);
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

        let messages = builder.analyze();
        check_messages(messages, expected_messages(&code, &["a1"]));
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

        let messages = builder.analyze();
        check_messages(
            messages,
            vec![Message::error(
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

        let messages = builder.analyze();
        check_messages(
            messages,
            vec![Message::error(
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

        let messages = builder.analyze();
        check_messages(
            messages,
            vec![
                Message::error(
                    &code.s1("a1"),
                    "Deferred constant 'a1' lacks corresponding full constant declaration in package body",
                ),
                Message::error(
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

        let messages = builder.analyze();
        check_messages(
            messages,
            vec![
                Message::error(&code.s1("a1"), "Missing body for protected type 'a1'"),
                Message::error(&code.s1("b1"), "Missing body for protected type 'b1'"),
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

        let messages = builder.analyze();
        check_messages(
            messages,
            vec![
                Message::error(&code.s1("a1"), "No declaration of protected type 'a1'"),
                Message::error(&code.s1("b1"), "No declaration of protected type 'b1'"),
                Message::error(&code.s("b1", 2), "Missing body for protected type 'b1'"),
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

        let messages = builder.analyze();
        check_messages(messages, vec![expected_message(&code, "a1", 2, 3)]);
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

        let messages = builder.analyze();
        check_messages(messages, expected_messages(&code, &["a1"]));
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

        let messages = builder.analyze();
        check_no_messages(&messages);
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

        let messages = builder.analyze();
        check_messages(messages, expected_messages(&code, &["prot_t"]));
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

        let messages = builder.analyze();
        check_messages(messages, vec![expected_message(&code, "prot_t", 2, 3)]);
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

        let messages = builder.analyze();
        check_messages(messages, expected_messages(&code, &["a1", "b1"]));
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

        let messages = builder.analyze();
        check_no_messages(&messages);
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

        let messages = builder.analyze();
        check_messages(messages, expected_messages(&code, &["rec_t"]));
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

        let mut expected_messages = Vec::new();
        for code in [code_pkg, code_ent, code_pkg2].iter() {
            expected_messages.push(Message::error(
                code.s1("rec_t"),
                "Missing full type declaration of incomplete type 'rec_t'",
            ));
            expected_messages.push(
                Message::hint(
                    code.s1("rec_t"),
                    "The full type declaration shall occur immediately within the same declarative part",
                ));
        }

        let messages = builder.analyze();
        check_messages(messages, expected_messages);
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

        let messages = builder.analyze();
        check_messages(
            messages,
            expected_messages(&code, &["a1", "b1", "c1", "d1"]),
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

        let messages = builder.analyze();
        check_messages(messages, expected_messages(&code, &["a1", "b1"]));
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

        let messages = builder.analyze();
        check_messages(messages, expected_messages(&code, &["a1"]));
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

        let messages = builder.analyze();
        check_messages(messages, expected_messages(&code, &["a1", "b1"]));
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

        let messages = builder.analyze();
        check_messages(messages, expected_messages(&code, &["a1", "b1"]));
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

        let messages = builder.analyze();
        check_messages(messages, expected_messages(&code, &["a1", "b1"]));
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

        let messages = builder.analyze();
        check_messages(messages, expected_messages(&code, &["a1"]));
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

        let messages = builder.analyze();
        check_messages(messages, expected_messages(&code, &["a1", "b1"]));
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

        let messages = builder.analyze();
        check_messages(
            messages,
            expected_messages(&code, &["a1", "b1", "c1", "d1"]),
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

        let messages = builder.analyze();
        check_messages(messages, expected_messages(&code, &["a1", "b1"]));
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

        let messages = builder.analyze();
        check_messages(
            messages,
            expected_messages(&code, &["a1", "b1", "c1", "d1"]),
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

        let messages = builder.analyze();
        check_messages(messages, expected_messages(&code, &["a1", "b1"]));
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

        let messages = builder.analyze();
        check_messages(messages, expected_messages(&code, &["a1"]));
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

        let messages = builder.analyze();
        check_messages(messages, expected_messages(&code, &["a1"]));
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

        let messages = builder.analyze();
        check_messages(messages, expected_messages(&code, &["a1"]));
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

        let messages = builder.analyze();
        check_messages(messages, expected_messages(&code, &["a1"]));
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

        let messages = builder.analyze();
        check_messages(messages, expected_messages(&code, &["a1"]));
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

        let messages = builder.analyze();
        check_messages(messages, expected_messages(&code, &["a1"]));
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

        let messages = builder.analyze();
        check_messages(messages, expected_messages(&code, &["a1", "b1"]));
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

        let messages = builder.analyze();
        check_no_messages(&messages);
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

        let messages = builder.analyze();
        check_messages(messages, expected_messages(&code, &["a1"]));
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

        let messages = builder.analyze();
        check_messages(messages, expected_messages(&code, &["a1"]));
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

        let messages = builder.analyze();
        check_messages(messages, expected_messages(&code, &["a1"]));
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

        let messages = builder.analyze();
        check_messages(messages, expected_messages(&code, &["a1"]));
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

        let messages = builder.analyze();
        let mut expected = expected_messages(&ent, &["g1", "g2", "p1"]);
        expected.append(&mut expected_messages_multi(
            &ent,
            &arch1,
            &["g3", "p2", "e1"],
        ));
        expected.append(&mut expected_messages_multi(&ent, &arch2, &["e2"]));
        check_messages(messages, expected);
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

        let messages = builder.analyze();
        let mut expected = expected_messages(&pkg, &["g1"]);
        expected.append(&mut expected_messages_multi(&pkg, &body, &["g1", "g2"]));
        check_messages(messages, expected);
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

        let messages = builder.analyze();

        check_messages(
            messages,
            vec![Message::error(
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

        let messages = builder.analyze();

        check_no_messages(&messages);
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

        let messages = builder.analyze();

        check_no_messages(&messages);
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
        let messages = builder.analyze();
        check_messages(
            messages,
            vec![Message::error(
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

        let messages = builder.analyze();

        check_messages(
            messages,
            vec![Message::error(
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

        let messages = builder.analyze();

        check_no_messages(&messages);
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

        let messages = builder.analyze();
        check_no_messages(&messages);
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

        let messages = builder.analyze();

        check_messages(
            messages,
            vec![Message::hint(
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
            use crate::latin_1::Latin1String;

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

        fn analyze(&self) -> Vec<Message> {
            let mut root = DesignRoot::new();
            let mut messages = Vec::new();

            for (library_name, codes) in self.libraries.iter() {
                let design_files = codes.iter().map(|code| code.design_file()).collect();
                let library = Library::new(
                    library_name.clone(),
                    &self.code_builder.symbol("work"),
                    design_files,
                    &mut messages,
                );
                root.add_library(library);
            }

            Analyzer::new(&root, &self.code_builder.symtab.clone()).analyze(&mut messages);

            messages
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

        let messages = builder.analyze();

        check_messages(
            messages,
            vec![
                Message::error(
                    code.s("missing_pkg", 1),
                    "No primary unit 'missing_pkg' within 'libname'",
                ),
                Message::error(
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

        let messages = builder.analyze();

        check_messages(
            messages,
            vec![Message::error(
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

        let messages = builder.analyze();

        check_messages(
            messages,
            vec![
                Message::error(code.s("pkg1", 1), "No primary unit 'pkg1' within 'libname'"),
                Message::error(code.s("pkg1", 2), "No primary unit 'pkg1' within 'libname'"),
                Message::error(code.s("pkg1", 3), "No primary unit 'pkg1' within 'libname'"),
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

        let messages = builder.analyze();

        check_messages(
            messages,
            vec![Message::error(
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

        let messages = builder.analyze();

        check_messages(
            messages,
            vec![Message::error(
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

        let messages = builder.analyze();

        check_messages(
            messages,
            vec![
                Message::error(
                    code.s1("context libname;"),
                    "Context reference must be a selected name",
                ),
                Message::error(code.s1("use work;"), "Use clause must be a selected name"),
                Message::error(
                    code.s1("use libname;"),
                    "Use clause must be a selected name",
                ),
                Message::error(
                    code.s1("use work.pkg(0);"),
                    "Use clause must be a selected name",
                ),
                Message::error(
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
        let messages = builder.analyze();
        check_messages(
            messages,
            vec![Message::error(
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
        let messages = builder.analyze();
        check_messages(
            messages,
            vec![Message::error(
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
        let messages = builder.analyze();
        check_messages(
            messages,
            vec![
                // @TODO add use instance path in error message
                Message::error(
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
        let messages = builder.analyze();
        check_messages(
            messages,
            vec![
                Message::error(
                    code.s("const1", 3),
                    "No declaration of 'const1' within package 'libname.pkg'",
                ),
                Message::error(
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
        let messages = builder.analyze();
        check_messages(
            messages,
            vec![
                Message::error(
                    code.s("work.all", 1),
                    "'.all' may not be the prefix of a selected name",
                ),
                Message::error(
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
        let messages = builder.analyze();
        check_messages(
            messages,
            vec![
                Message::error(
                    code.s("work.gpkg", 1),
                    "Uninstantiated generic package 'libname.gpkg' may not be the prefix of a selected name",
                ),
                Message::error(
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
        let messages = builder.analyze();
        check_messages(
            messages,
            vec![
                Message::error(code.s("gpkg", 2), "No declaration of 'gpkg'"),
                Message::error(code.s("gpkg", 4), "No declaration of 'gpkg'"),
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
        let messages = builder.analyze();
        check_messages(
            messages,
            vec![
                Message::error(
                    code.s1("work.pkg"),
                    "'pkg' is not an uninstantiated generic package",
                ),
                Message::error(
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
        let messages = builder.analyze();
        check_no_messages(&messages);
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
        let messages = builder.analyze();
        check_no_messages(&messages);
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
        let messages = builder.analyze();
        check_messages(
            messages,
            vec![
                Message::error(
                    code.s1("work.pkg1"),
                    "Found circular dependency when referencing 'libname.pkg1'",
                ),
                Message::error(
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
        let messages = builder.analyze();
        check_messages(
            messages,
            vec![
                Message::error(
                    code.s1("work.pkg1"),
                    "Found circular dependency when referencing 'libname.pkg1'",
                ),
                Message::error(
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
        let messages = builder.analyze();
        check_no_messages(&messages);
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
            .map(|idx| Message::error(code.s("missing", 1 + idx), "No declaration of 'missing'"))
            .collect();

        let messages = builder.analyze();
        check_messages(messages, expected);
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

        let messages = builder.analyze();
        check_messages(
            messages,
            vec![Message::error(
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

        let messages = builder.analyze();
        check_messages(
            messages,
            vec![Message::error(
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

        let messages = builder.analyze();
        check_no_messages(&messages);
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
        let messages = builder.analyze();
        check_messages(
            messages,
            vec![Message::error(
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
        let messages = builder.analyze();
        check_messages(
            messages,
            vec![Message::error(
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

        let messages = builder.analyze();
        check_messages(
            messages,
            vec![Message::error(
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
        let messages = builder.analyze();
        check_messages(
            messages,
            vec![Message::error(
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
        let messages = builder.analyze();
        check_messages(
            messages,
            vec![Message::error(
                code.s1("missing"),
                "No declaration of 'missing'",
            )],
        );
    }

}
