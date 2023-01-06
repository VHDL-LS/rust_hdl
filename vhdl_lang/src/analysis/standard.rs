// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2022, Olof Kraigher olof.kraigher@gmail.com

use std::sync::Arc;

use crate::ast::Designator;
use crate::ast::Mode;
use crate::ast::ObjectClass;
use crate::ast::Operator;
use crate::data::Symbol;
use crate::syntax::Symbols;

use super::formal_region::FormalRegion;
use super::named_entity::Signature;
use super::region::NamedEntityKind;
use super::region::Object;
use super::region::Region;
use super::region::Subtype;
use super::region::TypeEnt;
use super::root::UnitReadGuard;
use super::NamedEntity;

pub(super) enum RegionRef<'a> {
    // Inside the standard package itself we can hold a reference
    Inside(&'a Region<'a>),
    // Outside we need an analysis result
    Outside(UnitReadGuard<'a>),
}

impl<'a> std::ops::Deref for RegionRef<'a> {
    type Target = Region<'a>;
    fn deref(&self) -> &Self::Target {
        match self {
            RegionRef::Inside(r) => r,
            RegionRef::Outside(r) => &r.result().region,
        }
    }
}

pub(super) struct StandardRegion<'a> {
    // Only for symbol table
    symbols: &'a Symbols,
    region: RegionRef<'a>,
}

impl<'a> StandardRegion<'a> {
    pub(super) fn new(symbols: &'a Symbols, region: RegionRef<'a>) -> Self {
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

    fn natural(&self) -> TypeEnt {
        self.lookup_type("NATURAL")
    }

    fn real(&self) -> TypeEnt {
        self.lookup_type("REAL")
    }

    fn time(&self) -> TypeEnt {
        self.lookup_type("TIME")
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
            let mut params = FormalRegion::new_params();
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
                    has_default: true,
                }),
                file_type.decl_pos(),
            )));
            implicit.push(Arc::new(NamedEntity::implicit(
                file_type.clone().into(),
                self.symbol("FILE_OPEN"),
                NamedEntityKind::Subprogram(Signature::new(params, None)),
                file_type.decl_pos(),
            )));
        }

        // procedure FILE_OPEN (Status: out FILE_OPEN_STATUS; file F: FT; External_Name: in STRING; Open_Kind: in FILE_OPEN_KIND := READ_MODE);
        {
            let mut params = FormalRegion::new_params();
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
                    has_default: true,
                }),
                file_type.decl_pos(),
            )));
            implicit.push(Arc::new(NamedEntity::implicit(
                file_type.clone().into(),
                self.symbol("FILE_OPEN"),
                NamedEntityKind::Subprogram(Signature::new(params, None)),
                file_type.decl_pos(),
            )));
        }

        // procedure FILE_CLOSE (file F: FT);
        {
            let mut params = FormalRegion::new_params();
            params.add(Arc::new(NamedEntity::new(
                self.symbol("F"),
                NamedEntityKind::InterfaceFile(file_type.to_owned()),
                file_type.decl_pos(),
            )));

            implicit.push(Arc::new(NamedEntity::implicit(
                file_type.clone().into(),
                self.symbol("FILE_CLOSE"),
                NamedEntityKind::Subprogram(Signature::new(params, None)),
                file_type.decl_pos(),
            )));
        }

        // procedure READ (file F: FT; VALUE: out TM);
        {
            let mut params = FormalRegion::new_params();
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
                file_type.clone().into(),
                self.symbol("READ"),
                NamedEntityKind::Subprogram(Signature::new(params, None)),
                file_type.decl_pos(),
            )));
        }

        // procedure WRITE (file F: FT; VALUE: in TM);
        {
            let mut params = FormalRegion::new_params();
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
                file_type.clone().into(),
                self.symbol("WRITE"),
                NamedEntityKind::Subprogram(Signature::new(params, None)),
                file_type.decl_pos(),
            )));
        }

        // procedure FLUSH (file F: FT);
        {
            let mut params = FormalRegion::new_params();
            params.add(Arc::new(NamedEntity::new(
                self.symbol("F"),
                NamedEntityKind::InterfaceFile(file_type.to_owned()),
                file_type.decl_pos(),
            )));

            implicit.push(Arc::new(NamedEntity::implicit(
                file_type.clone().into(),
                self.symbol("FLUSH"),
                NamedEntityKind::Subprogram(Signature::new(params, None)),
                file_type.decl_pos(),
            )));
        }

        // function ENDFILE (file F: FT) return BOOLEAN;
        {
            let mut params = FormalRegion::new_params();
            params.add(Arc::new(NamedEntity::new(
                self.symbol("F"),
                NamedEntityKind::InterfaceFile(file_type.to_owned()),
                file_type.decl_pos(),
            )));

            implicit.push(Arc::new(NamedEntity::implicit(
                file_type.clone().into(),
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
        let mut params = FormalRegion::new_params();
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
            type_ent.clone().into(),
            self.symbol("TO_STRING"),
            NamedEntityKind::Subprogram(Signature::new(params, Some(self.string()))),
            type_ent.decl_pos(),
        ))
    }

    /// Create implicit MAXIMUM/MINIMUM
    // function MINIMUM (L, R: T) return T;
    // function MAXIMUM (L, R: T) return T;
    fn create_min_or_maximum(&self, name: &str, type_ent: TypeEnt) -> Arc<NamedEntity> {
        let mut params = FormalRegion::new_params();
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
            type_ent.clone().into(),
            self.symbol(name),
            NamedEntityKind::Subprogram(Signature::new(params, Some(type_ent.clone()))),
            type_ent.decl_pos(),
        ))
    }

    fn unary(&self, op: Operator, typ: TypeEnt, return_type: TypeEnt) -> Arc<NamedEntity> {
        let mut params = FormalRegion::new_params();
        params.add(Arc::new(NamedEntity::new(
            // @TODO anonymous
            self.symbol("V"),
            NamedEntityKind::Object(Object {
                class: ObjectClass::Constant,
                mode: Some(Mode::In),
                subtype: Subtype::new(typ.to_owned()),
                has_default: false,
            }),
            typ.decl_pos(),
        )));

        Arc::new(NamedEntity::implicit(
            typ.clone().into(),
            Designator::OperatorSymbol(op),
            NamedEntityKind::Subprogram(Signature::new(params, Some(return_type))),
            typ.decl_pos(),
        ))
    }

    fn symmetric_unary(&self, op: Operator, typ: TypeEnt) -> Arc<NamedEntity> {
        self.unary(op, typ.clone(), typ)
    }

    fn binary(
        &self,
        op: Operator,
        implicit_of: TypeEnt,
        left: TypeEnt,
        right: TypeEnt,
        return_type: TypeEnt,
    ) -> Arc<NamedEntity> {
        let mut params = FormalRegion::new_params();
        params.add(Arc::new(NamedEntity::new(
            // @TODO anonymous
            self.symbol("L"),
            NamedEntityKind::Object(Object {
                class: ObjectClass::Constant,
                mode: Some(Mode::In),
                subtype: Subtype::new(left),
                has_default: false,
            }),
            implicit_of.decl_pos(),
        )));

        params.add(Arc::new(NamedEntity::new(
            // @TODO anonymous
            self.symbol("R"),
            NamedEntityKind::Object(Object {
                class: ObjectClass::Constant,
                mode: Some(Mode::In),
                subtype: Subtype::new(right),
                has_default: false,
            }),
            implicit_of.decl_pos(),
        )));

        Arc::new(NamedEntity::implicit(
            implicit_of.clone().into(),
            Designator::OperatorSymbol(op),
            NamedEntityKind::Subprogram(Signature::new(params, Some(return_type))),
            implicit_of.decl_pos(),
        ))
    }

    fn symmetric_binary(&self, op: Operator, typ: TypeEnt) -> Arc<NamedEntity> {
        self.binary(op, typ.clone(), typ.clone(), typ.clone(), typ)
    }

    pub fn minimum(&self, type_ent: TypeEnt) -> Arc<NamedEntity> {
        self.create_min_or_maximum("MINIMUM", type_ent)
    }

    pub fn maximum(&self, type_ent: TypeEnt) -> Arc<NamedEntity> {
        self.create_min_or_maximum("MAXIMUM", type_ent)
    }

    /// Create implicit DEALLOCATE
    /// procedure DEALLOCATE (P: inout AT);
    pub fn create_deallocate(&self, type_ent: TypeEnt) -> Arc<NamedEntity> {
        let mut params = FormalRegion::new_params();
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
            type_ent.clone().into(),
            self.symbol("DEALLOCATE"),
            NamedEntityKind::Subprogram(Signature::new(params, None)),
            type_ent.decl_pos(),
        ))
    }

    pub fn implicits_of_integer_type(&self, typ: TypeEnt) -> Vec<Arc<NamedEntity>> {
        vec![
            self.minimum(typ.clone()),
            self.maximum(typ.clone()),
            self.create_to_string(typ.clone()),
            self.symmetric_unary(Operator::Minus, typ.clone()),
            self.symmetric_unary(Operator::Plus, typ.clone()),
            self.symmetric_unary(Operator::Abs, typ.clone()),
            self.symmetric_binary(Operator::Plus, typ.clone()),
            self.symmetric_binary(Operator::Minus, typ),
        ]
    }

    pub fn implicits_of_physical_type(&self, typ: TypeEnt) -> Vec<Arc<NamedEntity>> {
        vec![
            self.minimum(typ.clone()),
            self.maximum(typ.clone()),
            self.symmetric_unary(Operator::Minus, typ.clone()),
            self.symmetric_unary(Operator::Plus, typ.clone()),
            self.symmetric_unary(Operator::Abs, typ.clone()),
            self.symmetric_binary(Operator::Plus, typ.clone()),
            self.symmetric_binary(Operator::Minus, typ),
        ]
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

        for name in [
            "BOOLEAN",
            "BIT",
            "CHARACTER",
            "SEVERITY_LEVEL",
            "FILE_OPEN_KIND",
            "FILE_OPEN_STATUS",
        ] {
            let typ = self.lookup_type(name);
            let implicits = vec![
                self.create_to_string(typ.clone()),
                self.minimum(typ.clone()),
                self.maximum(typ.clone()),
            ];

            for ent in implicits.iter() {
                if let Some(implicit) = typ.kind().implicits() {
                    // This is safe because the standard package is analyzed in a single thread
                    unsafe { implicit.push(ent) };
                }
            }

            res.extend(implicits);
        }

        for name in ["INTEGER", "REAL", "TIME"] {
            let typ = self.lookup_type(name);
            for ent in self.implicits_of_integer_type(typ.clone()) {
                if let Some(implicit) = typ.kind().implicits() {
                    // This is safe because the standard package is analyzed in a single thread
                    unsafe { implicit.push(&ent) };
                }
                res.push(ent);
            }
        }

        for name in ["BOOLEAN", "BIT"] {
            let typ = self.lookup_type(name);
            let implicits = [self.symmetric_unary(Operator::Not, typ.clone())];

            for ent in implicits {
                if let Some(implicit) = typ.kind().implicits() {
                    // This is safe because the standard package is analyzed in a single thread
                    unsafe { implicit.push(&ent) };
                }
                res.push(ent);
            }
        }

        for name in ["BOOLEAN_VECTOR", "BIT_VECTOR"] {
            let typ = self.lookup_type(name);
            let return_typ = self.lookup_type(name.strip_suffix("_VECTOR").unwrap());
            let implicits = [self.unary(Operator::Not, typ.clone(), return_typ.clone())];

            for ent in implicits {
                if let Some(implicit) = typ.kind().implicits() {
                    // This is safe because the standard package is analyzed in a single thread
                    unsafe { implicit.push(&ent) };
                }
                res.push(ent);
            }
        }

        // Predefined overloaded TO_STRING operations
        // function TO_STRING (VALUE: REAL; DIGITS: NATURAL) return STRING;
        {
            let real = self.real();
            let natural = self.natural();
            let string = self.string();

            let mut params = FormalRegion::new_params();
            params.add(Arc::new(NamedEntity::new(
                self.symbol("VALUE"),
                NamedEntityKind::Object(Object {
                    class: ObjectClass::Constant,
                    mode: Some(Mode::In),
                    subtype: Subtype::new(real.to_owned()),
                    has_default: false,
                }),
                real.decl_pos(),
            )));

            params.add(Arc::new(NamedEntity::new(
                self.symbol("DIGITS"),
                NamedEntityKind::Object(Object {
                    class: ObjectClass::Constant,
                    mode: Some(Mode::In),
                    subtype: Subtype::new(natural),
                    has_default: false,
                }),
                real.decl_pos(),
            )));

            let ent = Arc::new(NamedEntity::implicit(
                real.clone().into(),
                self.symbol("TO_STRING"),
                NamedEntityKind::Subprogram(Signature::new(params, Some(string))),
                real.decl_pos(),
            ));

            if let Some(implicit) = real.kind().implicits() {
                // This is safe because the standard package is analyzed in a single thread
                unsafe { implicit.push(&ent) };
            }
            res.push(ent);
        }

        // function TO_STRING (VALUE: REAL; FORMAT: STRING) return STRING;
        {
            let real = self.real();
            let string = self.string();

            let mut params = FormalRegion::new_params();
            params.add(Arc::new(NamedEntity::new(
                self.symbol("VALUE"),
                NamedEntityKind::Object(Object {
                    class: ObjectClass::Constant,
                    mode: Some(Mode::In),
                    subtype: Subtype::new(real.to_owned()),
                    has_default: false,
                }),
                real.decl_pos(),
            )));

            params.add(Arc::new(NamedEntity::new(
                self.symbol("FORMAT"),
                NamedEntityKind::Object(Object {
                    class: ObjectClass::Constant,
                    mode: Some(Mode::In),
                    subtype: Subtype::new(string.to_owned()),
                    has_default: false,
                }),
                real.decl_pos(),
            )));

            let ent = Arc::new(NamedEntity::implicit(
                real.clone().into(),
                self.symbol("TO_STRING"),
                NamedEntityKind::Subprogram(Signature::new(params, Some(string))),
                real.decl_pos(),
            ));

            if let Some(implicit) = real.kind().implicits() {
                // This is safe because the standard package is analyzed in a single thread
                unsafe { implicit.push(&ent) };
            }
            res.push(ent);
        }

        // function TO_STRING (VALUE: TIME; UNIT: TIME) return STRING
        {
            let time = self.time();
            let string = self.string();
            let mut params = FormalRegion::new_params();
            params.add(Arc::new(NamedEntity::new(
                self.symbol("VALUE"),
                NamedEntityKind::Object(Object {
                    class: ObjectClass::Constant,
                    mode: Some(Mode::In),
                    subtype: Subtype::new(time.to_owned()),
                    has_default: false,
                }),
                time.decl_pos(),
            )));

            params.add(Arc::new(NamedEntity::new(
                self.symbol("UNIT"),
                NamedEntityKind::Object(Object {
                    class: ObjectClass::Constant,
                    mode: Some(Mode::In),
                    subtype: Subtype::new(time.to_owned()),
                    has_default: false,
                }),
                time.decl_pos(),
            )));

            let ent = Arc::new(NamedEntity::implicit(
                time.clone().into(),
                self.symbol("TO_STRING"),
                NamedEntityKind::Subprogram(Signature::new(params, Some(string))),
                time.decl_pos(),
            ));

            if let Some(implicit) = time.kind().implicits() {
                // This is safe because the standard package is analyzed in a single thread
                unsafe { implicit.push(&ent) };
            }
            res.push(ent);
        }
        res
    }
}
