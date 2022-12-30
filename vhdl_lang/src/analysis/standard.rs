// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2022, Olof Kraigher olof.kraigher@gmail.com

use std::sync::Arc;

use crate::ast::Mode;
use crate::ast::ObjectClass;
use crate::data::Symbol;
use crate::syntax::Symbols;

use super::formal_region::FormalRegion;
use super::named_entity::Signature;
use super::region::NamedEntityKind;
use super::region::Object;
use super::region::Region;
use super::region::Subtype;
use super::region::TypeEnt;
use super::NamedEntity;

pub struct StandardRegion<'a> {
    // Only for symbol table
    symbols: &'a Symbols,
    region: &'a Region<'a>,
}

impl<'a> StandardRegion<'a> {
    pub fn new(symbols: &'a Symbols, region: &'a Region<'a>) -> Self {
        Self { symbols, region }
    }

    fn symbol(&self, name: &str) -> Symbol {
        self.symbols.symtab().insert_utf8(name)
    }

    fn lookup_type(&self, name: &str) -> TypeEnt {
        TypeEnt::from_any(
            self.region
                .lookup_immediate(&self.symbol(name).into())
                .unwrap()
                .clone()
                .into_non_overloaded()
                .unwrap(),
        )
        .unwrap()
    }

    fn string(&self) -> TypeEnt {
        self.lookup_type("STRING")
    }

    fn boolean(&self) -> TypeEnt {
        self.lookup_type("BOOLEAN")
    }

    fn file_open_kind(&self) -> TypeEnt {
        self.lookup_type("FILE_OPEN_KIND")
    }
    fn file_open_status(&self) -> TypeEnt {
        self.lookup_type("FILE_OPEN_STATUS")
    }

    pub fn create_implicit_file_type_subprograms(
        &self,
        file_type: &TypeEnt,
        type_mark: &TypeEnt,
    ) -> Vec<Arc<NamedEntity>> {
        let mut implicit = Vec::new();

        let string = self.string();
        let boolean = self.boolean();
        let file_open_kind = self.file_open_kind();
        let file_open_status = self.file_open_status();

        // procedure FILE_OPEN (file F: FT; External_Name: in STRING; Open_Kind: in FILE_OPEN_KIND := READ_MODE);
        {
            let mut params = FormalRegion::default();
            params.add(Arc::new(NamedEntity::new(
                self.symbol("F"),
                NamedEntityKind::InterfaceFile(file_type.to_owned()),
                file_type.decl_pos(),
            )));
            params.add(Arc::new(NamedEntity::new(
                self.symbol("External_Name"),
                NamedEntityKind::Object(Object {
                    class: ObjectClass::Constant,
                    mode: Some(Mode::In),
                    subtype: Subtype::new(string.clone()),
                    has_default: false,
                }),
                file_type.decl_pos(),
            )));

            params.add(Arc::new(NamedEntity::new(
                self.symbol("Open_Kind"),
                NamedEntityKind::Object(Object {
                    class: ObjectClass::Constant,
                    mode: Some(Mode::In),
                    subtype: Subtype::new(file_open_kind.clone()),
                    has_default: false,
                }),
                file_type.decl_pos(),
            )));
            implicit.push(Arc::new(NamedEntity::implicit(
                self.symbol("FILE_OPEN"),
                NamedEntityKind::Subprogram(Signature::new(params, None)),
                file_type.decl_pos(),
            )));
        }

        // procedure FILE_OPEN (Status: out FILE_OPEN_STATUS; file F: FT; External_Name: in STRING; Open_Kind: in FILE_OPEN_KIND := READ_MODE);
        {
            let mut params = FormalRegion::default();
            params.add(Arc::new(NamedEntity::new(
                self.symbol("Status"),
                NamedEntityKind::Object(Object {
                    class: ObjectClass::Variable,
                    mode: Some(Mode::Out),
                    subtype: Subtype::new(file_open_status),
                    has_default: false,
                }),
                file_type.decl_pos(),
            )));
            params.add(Arc::new(NamedEntity::new(
                self.symbol("F"),
                NamedEntityKind::InterfaceFile(file_type.to_owned()),
                file_type.decl_pos(),
            )));
            params.add(Arc::new(NamedEntity::new(
                self.symbol("External_Name"),
                NamedEntityKind::Object(Object {
                    class: ObjectClass::Constant,
                    mode: Some(Mode::In),
                    subtype: Subtype::new(string),
                    has_default: false,
                }),
                file_type.decl_pos(),
            )));

            params.add(Arc::new(NamedEntity::new(
                self.symbol("Open_Kind"),
                NamedEntityKind::Object(Object {
                    class: ObjectClass::Constant,
                    mode: Some(Mode::In),
                    subtype: Subtype::new(file_open_kind),
                    has_default: false,
                }),
                file_type.decl_pos(),
            )));
            implicit.push(Arc::new(NamedEntity::implicit(
                self.symbol("FILE_OPEN"),
                NamedEntityKind::Subprogram(Signature::new(params, None)),
                file_type.decl_pos(),
            )));
        }

        // procedure FILE_CLOSE (file F: FT);
        {
            let mut params = FormalRegion::default();
            params.add(Arc::new(NamedEntity::new(
                self.symbol("F"),
                NamedEntityKind::InterfaceFile(file_type.to_owned()),
                file_type.decl_pos(),
            )));

            implicit.push(Arc::new(NamedEntity::implicit(
                self.symbol("FILE_CLOSE"),
                NamedEntityKind::Subprogram(Signature::new(params, None)),
                file_type.decl_pos(),
            )));
        }

        // procedure READ (file F: FT; VALUE: out TM);
        {
            let mut params = FormalRegion::default();
            params.add(Arc::new(NamedEntity::new(
                self.symbol("F"),
                NamedEntityKind::InterfaceFile(file_type.to_owned()),
                file_type.decl_pos(),
            )));

            params.add(Arc::new(NamedEntity::new(
                self.symbol("VALUE"),
                NamedEntityKind::Object(Object {
                    class: ObjectClass::Variable,
                    mode: Some(Mode::Out),
                    subtype: Subtype::new(type_mark.clone()),
                    has_default: false,
                }),
                file_type.decl_pos(),
            )));

            implicit.push(Arc::new(NamedEntity::implicit(
                self.symbol("READ"),
                NamedEntityKind::Subprogram(Signature::new(params, None)),
                file_type.decl_pos(),
            )));
        }

        // procedure WRITE (file F: FT; VALUE: in TM);
        {
            let mut params = FormalRegion::default();
            params.add(Arc::new(NamedEntity::new(
                self.symbol("F"),
                NamedEntityKind::InterfaceFile(file_type.to_owned()),
                file_type.decl_pos(),
            )));

            params.add(Arc::new(NamedEntity::new(
                self.symbol("VALUE"),
                NamedEntityKind::Object(Object {
                    class: ObjectClass::Constant,
                    mode: Some(Mode::In),
                    subtype: Subtype::new(type_mark.clone()),
                    has_default: false,
                }),
                file_type.decl_pos(),
            )));

            implicit.push(Arc::new(NamedEntity::implicit(
                self.symbol("WRITE"),
                NamedEntityKind::Subprogram(Signature::new(params, None)),
                file_type.decl_pos(),
            )));
        }

        // procedure FLUSH (file F: FT);
        {
            let mut params = FormalRegion::default();
            params.add(Arc::new(NamedEntity::new(
                self.symbol("F"),
                NamedEntityKind::InterfaceFile(file_type.to_owned()),
                file_type.decl_pos(),
            )));

            implicit.push(Arc::new(NamedEntity::implicit(
                self.symbol("FLUSH"),
                NamedEntityKind::Subprogram(Signature::new(params, None)),
                file_type.decl_pos(),
            )));
        }

        // function ENDFILE (file F: FT) return BOOLEAN;
        {
            let mut params = FormalRegion::default();
            params.add(Arc::new(NamedEntity::new(
                self.symbol("F"),
                NamedEntityKind::InterfaceFile(file_type.to_owned()),
                file_type.decl_pos(),
            )));

            implicit.push(Arc::new(NamedEntity::implicit(
                self.symbol("ENDFILE"),
                NamedEntityKind::Subprogram(Signature::new(params, Some(boolean))),
                file_type.decl_pos(),
            )));
        }

        implicit
    }

    /// Create implicit TO_STRING
    /// function TO_STRING (VALUE: T) return STRING;
    pub fn create_to_string(&self, type_ent: TypeEnt) -> Arc<NamedEntity> {
        let mut params = FormalRegion::default();
        params.add(Arc::new(NamedEntity::new(
            self.symbol("VALUE"),
            NamedEntityKind::Object(Object {
                class: ObjectClass::Constant,
                mode: Some(Mode::In),
                subtype: Subtype::new(type_ent.to_owned()),
                has_default: false,
            }),
            type_ent.decl_pos(),
        )));

        Arc::new(NamedEntity::implicit(
            self.symbol("TO_STRING"),
            NamedEntityKind::Subprogram(Signature::new(params, Some(self.string()))),
            type_ent.decl_pos(),
        ))
    }

    /// Create implicit MAXIMUM/MINIMUM
    // function MINIMUM (L, R: T) return T;
    // function MAXIMUM (L, R: T) return T;
    fn create_min_or_maximum(&self, name: &str, type_ent: TypeEnt) -> Arc<NamedEntity> {
        let mut params = FormalRegion::default();
        params.add(Arc::new(NamedEntity::new(
            self.symbol("L"),
            NamedEntityKind::Object(Object {
                class: ObjectClass::Constant,
                mode: Some(Mode::In),
                subtype: Subtype::new(type_ent.to_owned()),
                has_default: false,
            }),
            type_ent.decl_pos(),
        )));

        params.add(Arc::new(NamedEntity::new(
            self.symbol("R"),
            NamedEntityKind::Object(Object {
                class: ObjectClass::Constant,
                mode: Some(Mode::In),
                subtype: Subtype::new(type_ent.to_owned()),
                has_default: false,
            }),
            type_ent.decl_pos(),
        )));

        Arc::new(NamedEntity::implicit(
            self.symbol(name),
            NamedEntityKind::Subprogram(Signature::new(params, Some(type_ent.clone()))),
            type_ent.decl_pos(),
        ))
    }

    pub fn minimum(&self, type_ent: TypeEnt) -> Arc<NamedEntity> {
        self.create_min_or_maximum("MINMUM", type_ent)
    }

    pub fn maximum(&self, type_ent: TypeEnt) -> Arc<NamedEntity> {
        self.create_min_or_maximum("MAXIMUM", type_ent)
    }

    /// Create implicit DEALLOCATE
    /// procedure DEALLOCATE (P: inout AT);
    pub fn create_deallocate(&self, type_ent: &TypeEnt) -> Arc<NamedEntity> {
        let mut params = FormalRegion::default();
        params.add(Arc::new(NamedEntity::new(
            self.symbol("P"),
            NamedEntityKind::Object(Object {
                class: ObjectClass::Variable,
                mode: Some(Mode::InOut),
                subtype: Subtype::new(type_ent.to_owned()),
                has_default: false,
            }),
            type_ent.decl_pos(),
        )));

        Arc::new(NamedEntity::implicit(
            self.symbol("DEALLOCATE"),
            NamedEntityKind::Subprogram(Signature::new(params, None)),
            type_ent.decl_pos(),
        ))
    }

    // Return the implicit things defined at the end of the standard packge
    pub fn end_of_package_implicits(&self) -> Vec<Arc<NamedEntity>> {
        let mut res = Vec::new();

        // Predefined TO_STRING operations on scalar types
        // function TO_STRING (VALUE: BOOLEAN) return STRING;
        // function TO_STRING (VALUE: BIT) return STRING;
        // function TO_STRING (VALUE: CHARACTER) return STRING;
        // function TO_STRING (VALUE: SEVERITY_LEVEL) return STRING;
        // function TO_STRING (VALUE: universal_integer) return STRING;
        // function TO_STRING (VALUE: universal_real) return STRING;
        // function TO_STRING (VALUE: INTEGER) return STRING;
        // function TO_STRING (VALUE: REAL) return STRING;
        // function TO_STRING (VALUE: TIME) return STRING;
        // function TO_STRING (VALUE: FILE_OPEN_KIND) return STRING;
        // function TO_STRING (VALUE: FILE_OPEN_STATUS) return STRING;

        // @TODO Predefined overloaded TO_STRING operations
        // function TO_STRING (VALUE: REAL; DIGITS: NATURAL) return STRING;
        // function TO_STRING (VALUE: REAL; FORMAT: STRING) return STRING;
        // function TO_STRING (VALUE: TIME; UNIT: TIME) return STRING

        for name in [
            "BOOLEAN",
            "BIT",
            "CHARACTER",
            "SEVERITY_LEVEL",
            "INTEGER",
            "REAL",
            "TIME",
            "FILE_OPEN_KIND",
            "FILE_OPEN_STATUS",
        ] {
            let typ = self.lookup_type(name);
            res.extend([
                self.create_to_string(typ.clone()),
                self.minimum(typ.clone()),
                self.maximum(typ),
            ]);
        }

        res
    }
}
