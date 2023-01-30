// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2022, Olof Kraigher olof.kraigher@gmail.com

use crate::ast::Designator;
use crate::ast::Mode;
use crate::ast::ObjectClass;
use crate::ast::Operator;
use crate::data::Symbol;
use crate::syntax::Symbols;
use crate::SrcPos;

use super::formal_region::FormalRegion;
use super::named_entity::*;
use super::region::*;
use super::DesignRoot;
use super::EntityId;

#[derive(Clone)]
pub struct UniversalTypes {
    pub integer: EntityId,
    pub real: EntityId,
}

impl UniversalTypes {
    pub fn new(arena: &Arena, pos: &SrcPos, symbols: &Symbols) -> Self {
        let integer = arena.explicit(
            Designator::Identifier(symbols.symtab().insert_utf8("universal_integer")),
            AnyEntKind::Type(Type::Universal(UniversalType::Integer)),
            Some(pos),
        );

        let real = arena.explicit(
            Designator::Identifier(symbols.symtab().insert_utf8("universal_real")),
            AnyEntKind::Type(Type::Universal(UniversalType::Real)),
            Some(pos),
        );

        Self {
            real: real.id(),
            integer: integer.id(),
        }
    }
}

pub(super) struct StandardRegion<'a, 'r> {
    // Only for symbol table
    root: &'a DesignRoot,
    arena: &'a Arena,
    builder: ImplicitBuilder<'a>,
    region: &'r Region<'a>,
}

impl<'a, 'r> StandardRegion<'a, 'r> {
    pub(super) fn new(root: &'a DesignRoot, arena: &'a Arena, region: &'r Region<'a>) -> Self {
        Self {
            root,
            arena,
            region,
            builder: ImplicitBuilder {
                symbols: &root.symbols,
                arena,
            },
        }
    }

    fn symbol(&self, name: &str) -> Symbol {
        self.root.symbol_utf8(name)
    }

    fn lookup_type(&self, name: &str) -> TypeEnt<'a> {
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

    fn string(&self) -> TypeEnt<'a> {
        self.lookup_type("STRING")
    }

    fn boolean(&self) -> TypeEnt<'a> {
        self.lookup_type("BOOLEAN")
    }

    fn natural(&self) -> TypeEnt<'a> {
        self.lookup_type("NATURAL")
    }

    pub fn universal_integer(&self) -> TypeEnt<'a> {
        TypeEnt::from_any(
            self.arena
                .get(self.root.universal.as_ref().unwrap().integer),
        )
        .unwrap()
    }

    fn integer(&self) -> TypeEnt<'a> {
        self.lookup_type("INTEGER")
    }

    fn real(&self) -> TypeEnt<'a> {
        self.lookup_type("REAL")
    }

    pub fn time(&self) -> TypeEnt<'a> {
        self.lookup_type("TIME")
    }

    fn file_open_kind(&self) -> TypeEnt<'a> {
        self.lookup_type("FILE_OPEN_KIND")
    }
    fn file_open_status(&self) -> TypeEnt<'a> {
        self.lookup_type("FILE_OPEN_STATUS")
    }

    pub fn create_implicit_file_type_subprograms(
        &self,
        file_type: TypeEnt<'a>,
        type_mark: TypeEnt<'a>,
    ) -> Vec<EntRef<'a>> {
        let mut implicit = Vec::new();

        let string = self.string();
        let boolean = self.boolean();
        let file_open_kind = self.file_open_kind();
        let file_open_status = self.file_open_status();

        // procedure FILE_OPEN (file F: FT; External_Name: in STRING; Open_Kind: in FILE_OPEN_KIND := READ_MODE);
        {
            let mut formals = FormalRegion::new_params();
            formals.add(self.arena.explicit(
                self.symbol("F"),
                AnyEntKind::InterfaceFile(file_type),
                file_type.decl_pos(),
            ));
            formals.add(self.arena.explicit(
                self.symbol("External_Name"),
                AnyEntKind::Object(Object {
                    class: ObjectClass::Constant,
                    mode: Some(Mode::In),
                    subtype: Subtype::new(string),
                    has_default: false,
                }),
                file_type.decl_pos(),
            ));

            formals.add(self.arena.explicit(
                self.symbol("Open_Kind"),
                AnyEntKind::Object(Object {
                    class: ObjectClass::Constant,
                    mode: Some(Mode::In),
                    subtype: Subtype::new(file_open_kind),
                    has_default: true,
                }),
                file_type.decl_pos(),
            ));
            implicit.push(self.arena.implicit(
                file_type.into(),
                self.symbol("FILE_OPEN"),
                AnyEntKind::new_procedure_decl(formals),
                file_type.decl_pos(),
            ));
        }

        // procedure FILE_OPEN (Status: out FILE_OPEN_STATUS; file F: FT; External_Name: in STRING; Open_Kind: in FILE_OPEN_KIND := READ_MODE);
        {
            let mut formals = FormalRegion::new_params();
            formals.add(self.arena.explicit(
                self.symbol("Status"),
                AnyEntKind::Object(Object {
                    class: ObjectClass::Variable,
                    mode: Some(Mode::Out),
                    subtype: Subtype::new(file_open_status),
                    has_default: false,
                }),
                file_type.decl_pos(),
            ));
            formals.add(self.arena.explicit(
                self.symbol("F"),
                AnyEntKind::InterfaceFile(file_type.to_owned()),
                file_type.decl_pos(),
            ));
            formals.add(self.arena.explicit(
                self.symbol("External_Name"),
                AnyEntKind::Object(Object {
                    class: ObjectClass::Constant,
                    mode: Some(Mode::In),
                    subtype: Subtype::new(string),
                    has_default: false,
                }),
                file_type.decl_pos(),
            ));

            formals.add(self.arena.explicit(
                self.symbol("Open_Kind"),
                AnyEntKind::Object(Object {
                    class: ObjectClass::Constant,
                    mode: Some(Mode::In),
                    subtype: Subtype::new(file_open_kind),
                    has_default: true,
                }),
                file_type.decl_pos(),
            ));
            implicit.push(self.arena.implicit(
                file_type.into(),
                self.symbol("FILE_OPEN"),
                AnyEntKind::new_procedure_decl(formals),
                file_type.decl_pos(),
            ));
        }

        // procedure FILE_CLOSE (file F: FT);
        {
            let mut formals = FormalRegion::new_params();
            formals.add(self.arena.explicit(
                self.symbol("F"),
                AnyEntKind::InterfaceFile(file_type.to_owned()),
                file_type.decl_pos(),
            ));

            implicit.push(self.arena.implicit(
                file_type.into(),
                self.symbol("FILE_CLOSE"),
                AnyEntKind::new_procedure_decl(formals),
                file_type.decl_pos(),
            ));
        }

        // procedure READ (file F: FT; VALUE: out TM);
        {
            let mut formals = FormalRegion::new_params();
            formals.add(self.arena.explicit(
                self.symbol("F"),
                AnyEntKind::InterfaceFile(file_type.to_owned()),
                file_type.decl_pos(),
            ));

            formals.add(self.arena.explicit(
                self.symbol("VALUE"),
                AnyEntKind::Object(Object {
                    class: ObjectClass::Variable,
                    mode: Some(Mode::Out),
                    subtype: Subtype::new(type_mark),
                    has_default: false,
                }),
                file_type.decl_pos(),
            ));

            implicit.push(self.arena.implicit(
                file_type.into(),
                self.symbol("READ"),
                AnyEntKind::new_procedure_decl(formals),
                file_type.decl_pos(),
            ));
        }

        // procedure WRITE (file F: FT; VALUE: in TM);
        {
            let mut formals = FormalRegion::new_params();
            formals.add(self.arena.explicit(
                self.symbol("F"),
                AnyEntKind::InterfaceFile(file_type.to_owned()),
                file_type.decl_pos(),
            ));

            formals.add(self.arena.explicit(
                self.symbol("VALUE"),
                AnyEntKind::Object(Object {
                    class: ObjectClass::Constant,
                    mode: Some(Mode::In),
                    subtype: Subtype::new(type_mark),
                    has_default: false,
                }),
                file_type.decl_pos(),
            ));

            implicit.push(self.arena.implicit(
                file_type.into(),
                self.symbol("WRITE"),
                AnyEntKind::new_procedure_decl(formals),
                file_type.decl_pos(),
            ));
        }

        // procedure FLUSH (file F: FT);
        {
            let mut formals = FormalRegion::new_params();
            formals.add(self.arena.explicit(
                self.symbol("F"),
                AnyEntKind::InterfaceFile(file_type.to_owned()),
                file_type.decl_pos(),
            ));

            implicit.push(self.arena.implicit(
                file_type.into(),
                self.symbol("FLUSH"),
                AnyEntKind::new_procedure_decl(formals),
                file_type.decl_pos(),
            ));
        }

        // function ENDFILE (file F: FT) return BOOLEAN;
        {
            let mut formals = FormalRegion::new_params();
            formals.add(self.arena.explicit(
                self.symbol("F"),
                AnyEntKind::InterfaceFile(file_type.to_owned()),
                file_type.decl_pos(),
            ));

            implicit.push(self.arena.implicit(
                file_type.into(),
                self.symbol("ENDFILE"),
                AnyEntKind::new_function_decl(formals, boolean),
                file_type.decl_pos(),
            ));
        }

        implicit
    }

    /// Create implicit TO_STRING
    /// function TO_STRING (VALUE: T) return STRING;
    pub fn create_to_string(&self, type_ent: TypeEnt<'a>) -> EntRef<'a> {
        let mut formals = FormalRegion::new_params();
        formals.add(self.arena.explicit(
            self.symbol("VALUE"),
            AnyEntKind::Object(Object {
                class: ObjectClass::Constant,
                mode: Some(Mode::In),
                subtype: Subtype::new(type_ent),
                has_default: false,
            }),
            type_ent.decl_pos(),
        ));

        self.arena.implicit(
            type_ent.into(),
            self.symbol("TO_STRING"),
            AnyEntKind::new_function_decl(formals, self.string()),
            type_ent.decl_pos(),
        )
    }

    /// Create implicit DEALLOCATE
    /// procedure DEALLOCATE (P: inout AT);
    pub fn deallocate(&self, type_ent: TypeEnt<'a>) -> EntRef<'a> {
        let mut formals = FormalRegion::new_params();
        formals.add(self.arena.explicit(
            self.symbol("P"),
            AnyEntKind::Object(Object {
                class: ObjectClass::Variable,
                mode: Some(Mode::InOut),
                subtype: Subtype::new(type_ent.to_owned()),
                has_default: false,
            }),
            type_ent.decl_pos(),
        ));

        self.arena.implicit(
            type_ent.into(),
            self.symbol("DEALLOCATE"),
            AnyEntKind::new_procedure_decl(formals),
            type_ent.decl_pos(),
        )
    }

    fn comparison(&self, op: Operator, typ: TypeEnt<'a>) -> EntRef<'a> {
        self.builder.binary(op, typ, typ, typ, self.boolean())
    }

    pub fn comparators(&self, typ: TypeEnt<'a>) -> impl Iterator<Item = EntRef<'a>> {
        [
            self.comparison(Operator::EQ, typ),
            self.comparison(Operator::NE, typ),
            self.comparison(Operator::LT, typ),
            self.comparison(Operator::LTE, typ),
            self.comparison(Operator::GT, typ),
            self.comparison(Operator::GTE, typ),
        ]
        .into_iter()
    }

    pub fn numeric_implicits(
        &self,
        kind: UniversalType,
        typ: TypeEnt<'a>,
    ) -> impl Iterator<Item = EntRef<'a>> {
        let integer = self.integer();

        [
            self.builder.minimum(typ),
            self.builder.maximum(typ),
            self.create_to_string(typ),
            self.builder.symmetric_unary(Operator::Minus, typ),
            self.builder.symmetric_unary(Operator::Plus, typ),
            self.builder.symmetric_binary(Operator::Plus, typ),
            self.builder.symmetric_binary(Operator::Minus, typ),
            // 9.2.7 Multiplying operators
            self.builder.symmetric_binary(Operator::Times, typ),
            self.builder.symmetric_binary(Operator::Div, typ),
            // 9.2.8 Miscellaneous operators
            self.builder.symmetric_unary(Operator::Abs, typ),
            self.builder.binary(Operator::Pow, typ, typ, integer, typ),
        ]
        .into_iter()
        .chain(
            if kind == UniversalType::Integer {
                Some(
                    [
                        self.builder.symmetric_binary(Operator::Mod, typ),
                        self.builder.symmetric_binary(Operator::Rem, typ),
                    ]
                    .into_iter(),
                )
            } else {
                None
            }
            .into_iter()
            .flatten(),
        )
        .chain(self.comparators(typ).into_iter())
    }

    pub fn physical_implicits(&self, typ: TypeEnt<'a>) -> impl Iterator<Item = EntRef<'a>> {
        let integer = self.integer();
        let real = self.real();

        [
            self.builder.minimum(typ),
            self.builder.maximum(typ),
            self.builder.symmetric_unary(Operator::Minus, typ),
            self.builder.symmetric_unary(Operator::Plus, typ),
            self.builder.symmetric_unary(Operator::Abs, typ),
            self.builder.symmetric_binary(Operator::Plus, typ),
            self.builder.symmetric_binary(Operator::Minus, typ),
            // 9.2.7 Multiplying operators
            self.builder.binary(Operator::Times, typ, typ, integer, typ),
            self.builder.binary(Operator::Times, typ, typ, real, typ),
            self.builder.binary(Operator::Times, typ, integer, typ, typ),
            self.builder.binary(Operator::Times, typ, real, typ, typ),
            self.builder.binary(Operator::Div, typ, typ, integer, typ),
            self.builder.binary(Operator::Div, typ, typ, real, typ),
            self.builder
                .binary(Operator::Div, typ, typ, typ, self.universal_integer()),
            self.builder.symmetric_binary(Operator::Mod, typ),
            self.builder.symmetric_binary(Operator::Rem, typ),
        ]
        .into_iter()
        .chain(self.comparators(typ).into_iter())
    }

    pub fn enum_implicits(&self, typ: TypeEnt<'a>) -> impl Iterator<Item = EntRef<'a>> {
        [
            self.create_to_string(typ),
            self.builder.minimum(typ),
            self.builder.maximum(typ),
        ]
        .into_iter()
        .chain(self.comparators(typ).into_iter())
    }

    pub fn record_implicits(&self, typ: TypeEnt<'a>) -> impl Iterator<Item = EntRef<'a>> {
        [
            self.comparison(Operator::EQ, typ),
            self.comparison(Operator::NE, typ),
        ]
        .into_iter()
    }

    fn concatenations(
        &self,
        array_type: TypeEnt<'a>,
        elem_type: TypeEnt<'a>,
    ) -> impl Iterator<Item = EntRef<'a>> {
        [
            self.builder.binary(
                Operator::Concat,
                array_type,
                array_type,
                elem_type,
                array_type,
            ),
            self.builder.binary(
                Operator::Concat,
                array_type,
                elem_type,
                array_type,
                array_type,
            ),
            self.builder.symmetric_binary(Operator::Concat, array_type),
            self.builder.binary(
                Operator::Concat,
                array_type,
                elem_type,
                elem_type,
                array_type,
            ),
        ]
        .into_iter()
    }

    pub fn array_implicits(&self, typ: TypeEnt<'a>) -> impl Iterator<Item = EntRef<'a>> {
        let Type::Array{indexes, elem_type, ..}  = typ.kind() else {
            unreachable!("Must be array type")
        };

        let is_one_dimensional = indexes.len() == 1;
        [
            self.create_to_string(typ),
            self.comparison(Operator::EQ, typ),
            self.comparison(Operator::NE, typ),
        ]
        .into_iter()
        .chain(
            (if is_one_dimensional {
                Some(self.concatenations(typ, *elem_type))
            } else {
                None
            })
            .into_iter()
            .flatten(),
        )
    }

    pub fn access_implicits(&self, typ: TypeEnt<'a>) -> impl Iterator<Item = EntRef<'a>> {
        [
            self.deallocate(typ),
            self.comparison(Operator::EQ, typ),
            self.comparison(Operator::NE, typ),
        ]
        .into_iter()
    }

    pub fn type_implicits(&self, typ: TypeEnt<'a>) -> Vec<EntRef<'a>> {
        match typ.kind() {
            Type::Access(..) => self.access_implicits(typ).collect(),
            Type::Enum(..) => self.enum_implicits(typ).collect(),
            Type::Integer => self
                .numeric_implicits(UniversalType::Integer, typ)
                .collect(),
            Type::Real => self.numeric_implicits(UniversalType::Real, typ).collect(),
            Type::Record(..) => self.record_implicits(typ).collect(),
            Type::Physical => self.physical_implicits(typ).collect(),
            Type::Array { .. } => self.array_implicits(typ).collect(),
            Type::Universal(..) => Vec::new(), // Defined before the standard package
            Type::Interface { .. }
            | Type::Alias(..)
            | Type::Protected(..)
            | Type::Incomplete
            | Type::File
            | Type::Subtype(..) => Vec::new(),
        }
    }

    // Return the implicit things defined at the end of the standard packge
    pub fn end_of_package_implicits(self, arena: &'a Arena) -> Vec<EntRef<'a>> {
        let mut res = Vec::new();

        for ent in self.region.immediates() {
            if let NamedEntities::Single(ent) = ent {
                if let Some(typ) = TypeEnt::from_any(ent) {
                    for ent in self.type_implicits(typ) {
                        // This is safe because the standard package is analyzed in a single thread
                        unsafe {
                            arena.add_implicit(typ.id(), ent);
                        };
                        res.push(ent);
                    }
                }
            }
        }

        {
            let time = self.time();
            let to_string = self.create_to_string(time);

            unsafe {
                arena.add_implicit(time.id(), to_string);
            };
            res.push(to_string);
        }

        for name in ["BOOLEAN", "BIT"] {
            let typ = self.lookup_type(name);
            let implicits = [
                self.builder.symmetric_binary(Operator::And, typ),
                self.builder.symmetric_binary(Operator::Or, typ),
                self.builder.symmetric_binary(Operator::Nand, typ),
                self.builder.symmetric_binary(Operator::Nor, typ),
                self.builder.symmetric_binary(Operator::Xor, typ),
                self.builder.symmetric_binary(Operator::Xnor, typ),
                self.builder.symmetric_unary(Operator::Not, typ),
            ]
            .into_iter();

            for ent in implicits {
                unsafe {
                    arena.add_implicit(typ.id(), ent);
                };
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
                    self.builder.symmetric_binary(op, atyp),
                    if op == Operator::Not {
                        // op A -> A
                        self.builder.unary(op, atyp, atyp)
                    } else {
                        // op A -> S
                        self.builder.unary(op, atyp, styp)
                    },
                    // A op S -> A
                    self.builder.binary(op, atyp, atyp, styp, atyp),
                    // S op A -> A
                    self.builder.binary(op, atyp, atyp, atyp, styp),
                ]
                .into_iter()
            });

            for ent in implicits {
                // This is safe because the standard package is analyzed in a single thread
                unsafe {
                    arena.add_implicit(atyp.id(), ent);
                };
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
            formals.add(self.arena.explicit(
                self.symbol("VALUE"),
                AnyEntKind::Object(Object {
                    class: ObjectClass::Constant,
                    mode: Some(Mode::In),
                    subtype: Subtype::new(real.to_owned()),
                    has_default: false,
                }),
                real.decl_pos(),
            ));

            formals.add(self.arena.explicit(
                self.symbol("DIGITS"),
                AnyEntKind::Object(Object {
                    class: ObjectClass::Constant,
                    mode: Some(Mode::In),
                    subtype: Subtype::new(natural),
                    has_default: false,
                }),
                real.decl_pos(),
            ));

            let ent = arena.implicit(
                real.into(),
                self.symbol("TO_STRING"),
                AnyEntKind::new_function_decl(formals, string),
                real.decl_pos(),
            );

            // This is safe because the standard package is analyzed in a single thread
            unsafe {
                arena.add_implicit(real.id(), ent);
            };
            res.push(ent);
        }

        // function TO_STRING (VALUE: REAL; FORMAT: STRING) return STRING;
        {
            let real = self.real();
            let string = self.string();

            let mut formals = FormalRegion::new_params();
            formals.add(self.arena.explicit(
                self.symbol("VALUE"),
                AnyEntKind::Object(Object {
                    class: ObjectClass::Constant,
                    mode: Some(Mode::In),
                    subtype: Subtype::new(real.to_owned()),
                    has_default: false,
                }),
                real.decl_pos(),
            ));

            formals.add(self.arena.explicit(
                self.symbol("FORMAT"),
                AnyEntKind::Object(Object {
                    class: ObjectClass::Constant,
                    mode: Some(Mode::In),
                    subtype: Subtype::new(string.to_owned()),
                    has_default: false,
                }),
                real.decl_pos(),
            ));

            let ent = arena.implicit(
                real.into(),
                self.symbol("TO_STRING"),
                AnyEntKind::new_function_decl(formals, string),
                real.decl_pos(),
            );

            // This is safe because the standard package is analyzed in a single thread
            unsafe {
                arena.add_implicit(real.id(), ent);
            };
            res.push(ent);
        }

        // function TO_STRING (VALUE: TIME; UNIT: TIME) return STRING
        {
            let time = self.time();
            let string = self.string();
            let mut formals = FormalRegion::new_params();
            formals.add(self.arena.explicit(
                self.symbol("VALUE"),
                AnyEntKind::Object(Object {
                    class: ObjectClass::Constant,
                    mode: Some(Mode::In),
                    subtype: Subtype::new(time.to_owned()),
                    has_default: false,
                }),
                time.decl_pos(),
            ));

            formals.add(self.arena.explicit(
                self.symbol("UNIT"),
                AnyEntKind::Object(Object {
                    class: ObjectClass::Constant,
                    mode: Some(Mode::In),
                    subtype: Subtype::new(time.to_owned()),
                    has_default: false,
                }),
                time.decl_pos(),
            ));

            let ent = arena.implicit(
                time.into(),
                self.symbol("TO_STRING"),
                AnyEntKind::new_function_decl(formals, string),
                time.decl_pos(),
            );

            // This is safe because the standard package is analyzed in a single thread
            unsafe {
                arena.add_implicit(time.id(), ent);
            };
            res.push(ent);
        }
        res
    }
}

pub(crate) struct ImplicitBuilder<'a> {
    pub symbols: &'a Symbols,
    pub arena: &'a Arena,
}

impl<'a> ImplicitBuilder<'a> {
    fn symbol(&self, name: &str) -> Symbol {
        self.symbols.symtab().insert_utf8(name)
    }

    /// Create implicit MAXIMUM/MINIMUM
    // function MINIMUM (L, R: T) return T;
    // function MAXIMUM (L, R: T) return T;
    fn create_min_or_maximum(&self, name: &str, type_ent: TypeEnt<'a>) -> EntRef<'a> {
        let mut formals = FormalRegion::new_params();
        formals.add(self.arena.explicit(
            self.symbol("L"),
            AnyEntKind::Object(Object {
                class: ObjectClass::Constant,
                mode: Some(Mode::In),
                subtype: Subtype::new(type_ent),
                has_default: false,
            }),
            type_ent.decl_pos(),
        ));

        formals.add(self.arena.explicit(
            self.symbol("R"),
            AnyEntKind::Object(Object {
                class: ObjectClass::Constant,
                mode: Some(Mode::In),
                subtype: Subtype::new(type_ent),
                has_default: false,
            }),
            type_ent.decl_pos(),
        ));

        self.arena.implicit(
            type_ent.into(),
            self.symbol(name),
            AnyEntKind::new_function_decl(formals, type_ent),
            type_ent.decl_pos(),
        )
    }

    fn unary(&self, op: Operator, typ: TypeEnt<'a>, return_type: TypeEnt<'a>) -> EntRef<'a> {
        let mut formals = FormalRegion::new_params();
        formals.add(self.arena.explicit(
            // @TODO anonymous
            self.symbol("V"),
            AnyEntKind::Object(Object {
                class: ObjectClass::Constant,
                mode: Some(Mode::In),
                subtype: Subtype::new(typ.to_owned()),
                has_default: false,
            }),
            typ.decl_pos(),
        ));

        self.arena.implicit(
            typ.into(),
            Designator::OperatorSymbol(op),
            AnyEntKind::new_function_decl(formals, return_type),
            typ.decl_pos(),
        )
    }

    pub(crate) fn symmetric_unary(&self, op: Operator, typ: TypeEnt<'a>) -> EntRef<'a> {
        self.unary(op, typ, typ)
    }

    fn binary(
        &self,

        op: Operator,
        implicit_of: TypeEnt<'a>,
        left: TypeEnt<'a>,
        right: TypeEnt<'a>,
        return_type: TypeEnt<'a>,
    ) -> EntRef<'a> {
        let mut formals = FormalRegion::new_params();
        formals.add(self.arena.explicit(
            // @TODO anonymous
            self.symbol("L"),
            AnyEntKind::Object(Object {
                class: ObjectClass::Constant,
                mode: Some(Mode::In),
                subtype: Subtype::new(left),
                has_default: false,
            }),
            implicit_of.decl_pos(),
        ));

        formals.add(self.arena.explicit(
            // @TODO anonymous
            self.symbol("R"),
            AnyEntKind::Object(Object {
                class: ObjectClass::Constant,
                mode: Some(Mode::In),
                subtype: Subtype::new(right),
                has_default: false,
            }),
            implicit_of.decl_pos(),
        ));

        self.arena.implicit(
            implicit_of.into(),
            Designator::OperatorSymbol(op),
            AnyEntKind::new_function_decl(formals, return_type),
            implicit_of.decl_pos(),
        )
    }

    fn symmetric_binary(&self, op: Operator, typ: TypeEnt<'a>) -> EntRef<'a> {
        self.binary(op, typ, typ, typ, typ)
    }

    pub fn minimum(&self, type_ent: TypeEnt<'a>) -> EntRef<'a> {
        self.create_min_or_maximum("MINIMUM", type_ent)
    }

    pub fn maximum(&self, type_ent: TypeEnt<'a>) -> EntRef<'a> {
        self.create_min_or_maximum("MAXIMUM", type_ent)
    }
}
