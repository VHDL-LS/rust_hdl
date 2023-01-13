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
use crate::SrcPos;

use super::formal_region::FormalRegion;
use super::implicits::ImplicitVec;
use super::named_entity::EntityId;
use super::named_entity::Type;
use super::named_entity::UniversalType;
use super::region::AnyEntKind;
use super::region::NamedEntities;
use super::region::Object;
use super::region::Region;
use super::region::Subtype;
use super::region::TypeEnt;
use super::AnyEnt;
use super::DesignRoot;

#[derive(Clone)]
pub struct UniversalTypes {
    pub integer: TypeEnt,
    pub real: TypeEnt,
}

impl UniversalTypes {
    pub fn new(pos: &SrcPos, symbols: &Symbols) -> Self {
        let integer = Arc::new(AnyEnt::new_with_id(
            EntityId::universal_integer(),
            Designator::Identifier(symbols.symtab().insert_utf8("universal_integer")),
            AnyEntKind::Type(Type::Universal(
                UniversalType::Integer,
                ImplicitVec::default(),
            )),
            Some(pos.clone()),
        ));

        let real = Arc::new(AnyEnt::new_with_id(
            EntityId::universal_real(),
            Designator::Identifier(symbols.symtab().insert_utf8("universal_real")),
            AnyEntKind::Type(Type::Universal(UniversalType::Real, ImplicitVec::default())),
            Some(pos.clone()),
        ));
        Self {
            real: TypeEnt::from_any(real).unwrap(),
            integer: TypeEnt::from_any(integer).unwrap(),
        }
    }
}

pub(super) struct StandardRegion<'a> {
    // Only for symbol table
    symbols: &'a Symbols,
    region: &'a Region,
}

impl<'a> StandardRegion<'a> {
    pub(super) fn new(root: &'a DesignRoot, region: &'a Region) -> Self {
        Self {
            symbols: &root.symbols,
            region,
        }
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
    ) -> Vec<Arc<AnyEnt>> {
        let mut implicit = Vec::new();

        let string = self.string();
        let boolean = self.boolean();
        let file_open_kind = self.file_open_kind();
        let file_open_status = self.file_open_status();

        // procedure FILE_OPEN (file F: FT; External_Name: in STRING; Open_Kind: in FILE_OPEN_KIND := READ_MODE);
        {
            let mut formals = FormalRegion::new_params();
            formals.add(Arc::new(AnyEnt::new(
                self.symbol("F"),
                AnyEntKind::InterfaceFile(file_type.to_owned()),
                file_type.decl_pos(),
            )));
            formals.add(Arc::new(AnyEnt::new(
                self.symbol("External_Name"),
                AnyEntKind::Object(Object {
                    class: ObjectClass::Constant,
                    mode: Some(Mode::In),
                    subtype: Subtype::new(string.clone()),
                    has_default: false,
                }),
                file_type.decl_pos(),
            )));

            formals.add(Arc::new(AnyEnt::new(
                self.symbol("Open_Kind"),
                AnyEntKind::Object(Object {
                    class: ObjectClass::Constant,
                    mode: Some(Mode::In),
                    subtype: Subtype::new(file_open_kind.clone()),
                    has_default: true,
                }),
                file_type.decl_pos(),
            )));
            implicit.push(Arc::new(AnyEnt::implicit(
                file_type.clone().into(),
                self.symbol("FILE_OPEN"),
                AnyEntKind::new_procedure_decl(formals),
                file_type.decl_pos(),
            )));
        }

        // procedure FILE_OPEN (Status: out FILE_OPEN_STATUS; file F: FT; External_Name: in STRING; Open_Kind: in FILE_OPEN_KIND := READ_MODE);
        {
            let mut formals = FormalRegion::new_params();
            formals.add(Arc::new(AnyEnt::new(
                self.symbol("Status"),
                AnyEntKind::Object(Object {
                    class: ObjectClass::Variable,
                    mode: Some(Mode::Out),
                    subtype: Subtype::new(file_open_status),
                    has_default: false,
                }),
                file_type.decl_pos(),
            )));
            formals.add(Arc::new(AnyEnt::new(
                self.symbol("F"),
                AnyEntKind::InterfaceFile(file_type.to_owned()),
                file_type.decl_pos(),
            )));
            formals.add(Arc::new(AnyEnt::new(
                self.symbol("External_Name"),
                AnyEntKind::Object(Object {
                    class: ObjectClass::Constant,
                    mode: Some(Mode::In),
                    subtype: Subtype::new(string),
                    has_default: false,
                }),
                file_type.decl_pos(),
            )));

            formals.add(Arc::new(AnyEnt::new(
                self.symbol("Open_Kind"),
                AnyEntKind::Object(Object {
                    class: ObjectClass::Constant,
                    mode: Some(Mode::In),
                    subtype: Subtype::new(file_open_kind),
                    has_default: true,
                }),
                file_type.decl_pos(),
            )));
            implicit.push(Arc::new(AnyEnt::implicit(
                file_type.clone().into(),
                self.symbol("FILE_OPEN"),
                AnyEntKind::new_procedure_decl(formals),
                file_type.decl_pos(),
            )));
        }

        // procedure FILE_CLOSE (file F: FT);
        {
            let mut formals = FormalRegion::new_params();
            formals.add(Arc::new(AnyEnt::new(
                self.symbol("F"),
                AnyEntKind::InterfaceFile(file_type.to_owned()),
                file_type.decl_pos(),
            )));

            implicit.push(Arc::new(AnyEnt::implicit(
                file_type.clone().into(),
                self.symbol("FILE_CLOSE"),
                AnyEntKind::new_procedure_decl(formals),
                file_type.decl_pos(),
            )));
        }

        // procedure READ (file F: FT; VALUE: out TM);
        {
            let mut formals = FormalRegion::new_params();
            formals.add(Arc::new(AnyEnt::new(
                self.symbol("F"),
                AnyEntKind::InterfaceFile(file_type.to_owned()),
                file_type.decl_pos(),
            )));

            formals.add(Arc::new(AnyEnt::new(
                self.symbol("VALUE"),
                AnyEntKind::Object(Object {
                    class: ObjectClass::Variable,
                    mode: Some(Mode::Out),
                    subtype: Subtype::new(type_mark.clone()),
                    has_default: false,
                }),
                file_type.decl_pos(),
            )));

            implicit.push(Arc::new(AnyEnt::implicit(
                file_type.clone().into(),
                self.symbol("READ"),
                AnyEntKind::new_procedure_decl(formals),
                file_type.decl_pos(),
            )));
        }

        // procedure WRITE (file F: FT; VALUE: in TM);
        {
            let mut formals = FormalRegion::new_params();
            formals.add(Arc::new(AnyEnt::new(
                self.symbol("F"),
                AnyEntKind::InterfaceFile(file_type.to_owned()),
                file_type.decl_pos(),
            )));

            formals.add(Arc::new(AnyEnt::new(
                self.symbol("VALUE"),
                AnyEntKind::Object(Object {
                    class: ObjectClass::Constant,
                    mode: Some(Mode::In),
                    subtype: Subtype::new(type_mark.clone()),
                    has_default: false,
                }),
                file_type.decl_pos(),
            )));

            implicit.push(Arc::new(AnyEnt::implicit(
                file_type.clone().into(),
                self.symbol("WRITE"),
                AnyEntKind::new_procedure_decl(formals),
                file_type.decl_pos(),
            )));
        }

        // procedure FLUSH (file F: FT);
        {
            let mut formals = FormalRegion::new_params();
            formals.add(Arc::new(AnyEnt::new(
                self.symbol("F"),
                AnyEntKind::InterfaceFile(file_type.to_owned()),
                file_type.decl_pos(),
            )));

            implicit.push(Arc::new(AnyEnt::implicit(
                file_type.clone().into(),
                self.symbol("FLUSH"),
                AnyEntKind::new_procedure_decl(formals),
                file_type.decl_pos(),
            )));
        }

        // function ENDFILE (file F: FT) return BOOLEAN;
        {
            let mut formals = FormalRegion::new_params();
            formals.add(Arc::new(AnyEnt::new(
                self.symbol("F"),
                AnyEntKind::InterfaceFile(file_type.to_owned()),
                file_type.decl_pos(),
            )));

            implicit.push(Arc::new(AnyEnt::implicit(
                file_type.clone().into(),
                self.symbol("ENDFILE"),
                AnyEntKind::new_function_decl(formals, boolean),
                file_type.decl_pos(),
            )));
        }

        implicit
    }

    /// Create implicit TO_STRING
    /// function TO_STRING (VALUE: T) return STRING;
    pub fn create_to_string(&self, type_ent: TypeEnt) -> Arc<AnyEnt> {
        let mut formals = FormalRegion::new_params();
        formals.add(Arc::new(AnyEnt::new(
            self.symbol("VALUE"),
            AnyEntKind::Object(Object {
                class: ObjectClass::Constant,
                mode: Some(Mode::In),
                subtype: Subtype::new(type_ent.to_owned()),
                has_default: false,
            }),
            type_ent.decl_pos(),
        )));

        Arc::new(AnyEnt::implicit(
            type_ent.clone().into(),
            self.symbol("TO_STRING"),
            AnyEntKind::new_function_decl(formals, self.string()),
            type_ent.decl_pos(),
        ))
    }

    /// Create implicit MAXIMUM/MINIMUM
    // function MINIMUM (L, R: T) return T;
    // function MAXIMUM (L, R: T) return T;
    fn create_min_or_maximum(&self, name: &str, type_ent: TypeEnt) -> Arc<AnyEnt> {
        let mut formals = FormalRegion::new_params();
        formals.add(Arc::new(AnyEnt::new(
            self.symbol("L"),
            AnyEntKind::Object(Object {
                class: ObjectClass::Constant,
                mode: Some(Mode::In),
                subtype: Subtype::new(type_ent.to_owned()),
                has_default: false,
            }),
            type_ent.decl_pos(),
        )));

        formals.add(Arc::new(AnyEnt::new(
            self.symbol("R"),
            AnyEntKind::Object(Object {
                class: ObjectClass::Constant,
                mode: Some(Mode::In),
                subtype: Subtype::new(type_ent.to_owned()),
                has_default: false,
            }),
            type_ent.decl_pos(),
        )));

        Arc::new(AnyEnt::implicit(
            type_ent.clone().into(),
            self.symbol(name),
            AnyEntKind::new_function_decl(formals, type_ent.clone()),
            type_ent.decl_pos(),
        ))
    }

    fn unary(&self, op: Operator, typ: TypeEnt, return_type: TypeEnt) -> Arc<AnyEnt> {
        let mut formals = FormalRegion::new_params();
        formals.add(Arc::new(AnyEnt::new(
            // @TODO anonymous
            self.symbol("V"),
            AnyEntKind::Object(Object {
                class: ObjectClass::Constant,
                mode: Some(Mode::In),
                subtype: Subtype::new(typ.to_owned()),
                has_default: false,
            }),
            typ.decl_pos(),
        )));

        Arc::new(AnyEnt::implicit(
            typ.clone().into(),
            Designator::OperatorSymbol(op),
            AnyEntKind::new_function_decl(formals, return_type),
            typ.decl_pos(),
        ))
    }

    fn symmetric_unary(&self, op: Operator, typ: TypeEnt) -> Arc<AnyEnt> {
        self.unary(op, typ.clone(), typ)
    }

    fn binary(
        &self,
        op: Operator,
        implicit_of: TypeEnt,
        left: TypeEnt,
        right: TypeEnt,
        return_type: TypeEnt,
    ) -> Arc<AnyEnt> {
        let mut formals = FormalRegion::new_params();
        formals.add(Arc::new(AnyEnt::new(
            // @TODO anonymous
            self.symbol("L"),
            AnyEntKind::Object(Object {
                class: ObjectClass::Constant,
                mode: Some(Mode::In),
                subtype: Subtype::new(left),
                has_default: false,
            }),
            implicit_of.decl_pos(),
        )));

        formals.add(Arc::new(AnyEnt::new(
            // @TODO anonymous
            self.symbol("R"),
            AnyEntKind::Object(Object {
                class: ObjectClass::Constant,
                mode: Some(Mode::In),
                subtype: Subtype::new(right),
                has_default: false,
            }),
            implicit_of.decl_pos(),
        )));

        Arc::new(AnyEnt::implicit(
            implicit_of.clone().into(),
            Designator::OperatorSymbol(op),
            AnyEntKind::new_function_decl(formals, return_type),
            implicit_of.decl_pos(),
        ))
    }

    fn symmetric_binary(&self, op: Operator, typ: TypeEnt) -> Arc<AnyEnt> {
        self.binary(op, typ.clone(), typ.clone(), typ.clone(), typ)
    }

    fn comparison(&self, op: Operator, typ: TypeEnt) -> Arc<AnyEnt> {
        self.binary(op, typ.clone(), typ.clone(), typ, self.boolean())
    }

    pub fn minimum(&self, type_ent: TypeEnt) -> Arc<AnyEnt> {
        self.create_min_or_maximum("MINIMUM", type_ent)
    }

    pub fn maximum(&self, type_ent: TypeEnt) -> Arc<AnyEnt> {
        self.create_min_or_maximum("MAXIMUM", type_ent)
    }

    /// Create implicit DEALLOCATE
    /// procedure DEALLOCATE (P: inout AT);
    pub fn deallocate(&self, type_ent: TypeEnt) -> Arc<AnyEnt> {
        let mut formals = FormalRegion::new_params();
        formals.add(Arc::new(AnyEnt::new(
            self.symbol("P"),
            AnyEntKind::Object(Object {
                class: ObjectClass::Variable,
                mode: Some(Mode::InOut),
                subtype: Subtype::new(type_ent.to_owned()),
                has_default: false,
            }),
            type_ent.decl_pos(),
        )));

        Arc::new(AnyEnt::implicit(
            type_ent.clone().into(),
            self.symbol("DEALLOCATE"),
            AnyEntKind::new_procedure_decl(formals),
            type_ent.decl_pos(),
        ))
    }

    pub fn comparators(&self, typ: TypeEnt) -> impl Iterator<Item = Arc<AnyEnt>> {
        [
            self.comparison(Operator::EQ, typ.clone()),
            self.comparison(Operator::NE, typ.clone()),
            self.comparison(Operator::LT, typ.clone()),
            self.comparison(Operator::LTE, typ.clone()),
            self.comparison(Operator::GT, typ.clone()),
            self.comparison(Operator::GTE, typ),
        ]
        .into_iter()
    }

    pub fn numeric_implicits(&self, typ: TypeEnt) -> impl Iterator<Item = Arc<AnyEnt>> {
        [
            self.minimum(typ.clone()),
            self.maximum(typ.clone()),
            self.create_to_string(typ.clone()),
            self.symmetric_unary(Operator::Minus, typ.clone()),
            self.symmetric_unary(Operator::Plus, typ.clone()),
            self.symmetric_unary(Operator::Abs, typ.clone()),
            self.symmetric_binary(Operator::Plus, typ.clone()),
            self.symmetric_binary(Operator::Minus, typ.clone()),
        ]
        .into_iter()
        .chain(self.comparators(typ).into_iter())
    }

    pub fn physical_implicits(&self, typ: TypeEnt) -> impl Iterator<Item = Arc<AnyEnt>> {
        [
            self.minimum(typ.clone()),
            self.maximum(typ.clone()),
            self.symmetric_unary(Operator::Minus, typ.clone()),
            self.symmetric_unary(Operator::Plus, typ.clone()),
            self.symmetric_unary(Operator::Abs, typ.clone()),
            self.symmetric_binary(Operator::Plus, typ.clone()),
            self.symmetric_binary(Operator::Minus, typ.clone()),
        ]
        .into_iter()
        .chain(self.comparators(typ).into_iter())
    }

    pub fn enum_implicits(&self, typ: TypeEnt) -> impl Iterator<Item = Arc<AnyEnt>> {
        [
            self.create_to_string(typ.clone()),
            self.minimum(typ.clone()),
            self.maximum(typ.clone()),
        ]
        .into_iter()
        .chain(self.comparators(typ).into_iter())
    }

    pub fn record_implicits(&self, typ: TypeEnt) -> impl Iterator<Item = Arc<AnyEnt>> {
        [
            self.comparison(Operator::EQ, typ.clone()),
            self.comparison(Operator::NE, typ),
        ]
        .into_iter()
    }

    pub fn array_implicits(&self, typ: TypeEnt) -> impl Iterator<Item = Arc<AnyEnt>> {
        [
            self.create_to_string(typ.clone()),
            self.comparison(Operator::EQ, typ.clone()),
            self.comparison(Operator::NE, typ),
        ]
        .into_iter()
    }

    pub fn access_implicits(&self, typ: TypeEnt) -> impl Iterator<Item = Arc<AnyEnt>> {
        [
            self.deallocate(typ.clone()),
            self.comparison(Operator::EQ, typ.clone()),
            self.comparison(Operator::NE, typ),
        ]
        .into_iter()
    }

    pub fn type_implicits(&self, typ: TypeEnt) -> Vec<Arc<AnyEnt>> {
        match typ.kind() {
            Type::Access(..) => self.access_implicits(typ).collect(),
            Type::Enum(..) => self.enum_implicits(typ).collect(),
            Type::Integer(..) => self.numeric_implicits(typ).collect(),
            Type::Real(..) => self.numeric_implicits(typ).collect(),
            Type::Record(..) => self.record_implicits(typ).collect(),
            Type::Physical(..) => self.physical_implicits(typ).collect(),
            Type::Array { .. } => self.array_implicits(typ).collect(),
            Type::Universal(..) => Vec::new(), // Defined before the standard package
            Type::Interface { .. }
            | Type::Alias(..)
            | Type::Protected(..)
            | Type::Incomplete(..)
            | Type::File(..)
            | Type::Subtype(..) => Vec::new(),
        }
    }

    // Return the

    // Return the implicit things defined at the end of the standard packge
    pub fn end_of_package_implicits(&self) -> Vec<Arc<AnyEnt>> {
        let mut res = Vec::new();

        for ent in self.region.immediates() {
            if let NamedEntities::Single(ent) = ent {
                if let Some(typ) = TypeEnt::from_any_ref(ent) {
                    for ent in self.type_implicits(typ.clone()) {
                        if let Some(implicit) = typ.kind().implicits() {
                            // This is safe because the standard package is analyzed in a single thread
                            unsafe { implicit.push(&ent) };
                        }
                        res.push(ent);
                    }
                }
            }
        }

        {
            let time = self.time();
            let to_string = self.create_to_string(time.clone());

            if let Some(implicit) = time.kind().implicits() {
                // This is safe because the standard package is analyzed in a single thread
                unsafe { implicit.push(&to_string) };
            }
            res.push(to_string);
        }

        for name in ["BOOLEAN", "BIT"] {
            let typ = self.lookup_type(name);
            let implicits = [
                self.symmetric_binary(Operator::And, typ.clone()),
                self.symmetric_binary(Operator::Or, typ.clone()),
                self.symmetric_binary(Operator::Nand, typ.clone()),
                self.symmetric_binary(Operator::Nor, typ.clone()),
                self.symmetric_binary(Operator::Xor, typ.clone()),
                self.symmetric_binary(Operator::Xnor, typ.clone()),
                self.symmetric_unary(Operator::Not, typ.clone()),
            ]
            .into_iter();

            for ent in implicits {
                if let Some(implicit) = typ.kind().implicits() {
                    // This is safe because the standard package is analyzed in a single thread
                    unsafe { implicit.push(&ent) };
                }
                res.push(ent);
            }
        }

        for name in ["BOOLEAN_VECTOR", "BIT_VECTOR"] {
            let atyp = self.lookup_type(name);
            let styp = self.lookup_type(name.strip_suffix("_VECTOR").unwrap());
            let ops = [
                Operator::And,
                Operator::Or,
                Operator::Nand,
                Operator::Nor,
                Operator::Xor,
                Operator::Xnor,
            ];

            let implicits = ops.iter().flat_map(|op| {
                let op = *op;
                [
                    // A op A -> A
                    self.symmetric_binary(op, atyp.clone()),
                    if op == Operator::Not {
                        // op A -> A
                        self.unary(op, atyp.clone(), atyp.clone())
                    } else {
                        // op A -> S
                        self.unary(op, atyp.clone(), styp.clone())
                    },
                    // A op S -> A
                    self.binary(op, atyp.clone(), atyp.clone(), styp.clone(), atyp.clone()),
                    // S op A -> A
                    self.binary(op, atyp.clone(), atyp.clone(), atyp.clone(), styp.clone()),
                ]
                .into_iter()
            });

            for ent in implicits {
                if let Some(implicit) = atyp.kind().implicits() {
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

            let mut formals = FormalRegion::new_params();
            formals.add(Arc::new(AnyEnt::new(
                self.symbol("VALUE"),
                AnyEntKind::Object(Object {
                    class: ObjectClass::Constant,
                    mode: Some(Mode::In),
                    subtype: Subtype::new(real.to_owned()),
                    has_default: false,
                }),
                real.decl_pos(),
            )));

            formals.add(Arc::new(AnyEnt::new(
                self.symbol("DIGITS"),
                AnyEntKind::Object(Object {
                    class: ObjectClass::Constant,
                    mode: Some(Mode::In),
                    subtype: Subtype::new(natural),
                    has_default: false,
                }),
                real.decl_pos(),
            )));

            let ent = Arc::new(AnyEnt::implicit(
                real.clone().into(),
                self.symbol("TO_STRING"),
                AnyEntKind::new_function_decl(formals, string),
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

            let mut formals = FormalRegion::new_params();
            formals.add(Arc::new(AnyEnt::new(
                self.symbol("VALUE"),
                AnyEntKind::Object(Object {
                    class: ObjectClass::Constant,
                    mode: Some(Mode::In),
                    subtype: Subtype::new(real.to_owned()),
                    has_default: false,
                }),
                real.decl_pos(),
            )));

            formals.add(Arc::new(AnyEnt::new(
                self.symbol("FORMAT"),
                AnyEntKind::Object(Object {
                    class: ObjectClass::Constant,
                    mode: Some(Mode::In),
                    subtype: Subtype::new(string.to_owned()),
                    has_default: false,
                }),
                real.decl_pos(),
            )));

            let ent = Arc::new(AnyEnt::implicit(
                real.clone().into(),
                self.symbol("TO_STRING"),
                AnyEntKind::new_function_decl(formals, string),
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
            let mut formals = FormalRegion::new_params();
            formals.add(Arc::new(AnyEnt::new(
                self.symbol("VALUE"),
                AnyEntKind::Object(Object {
                    class: ObjectClass::Constant,
                    mode: Some(Mode::In),
                    subtype: Subtype::new(time.to_owned()),
                    has_default: false,
                }),
                time.decl_pos(),
            )));

            formals.add(Arc::new(AnyEnt::new(
                self.symbol("UNIT"),
                AnyEntKind::Object(Object {
                    class: ObjectClass::Constant,
                    mode: Some(Mode::In),
                    subtype: Subtype::new(time.to_owned()),
                    has_default: false,
                }),
                time.decl_pos(),
            )));

            let ent = Arc::new(AnyEnt::implicit(
                time.clone().into(),
                self.symbol("TO_STRING"),
                AnyEntKind::new_function_decl(formals, string),
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
