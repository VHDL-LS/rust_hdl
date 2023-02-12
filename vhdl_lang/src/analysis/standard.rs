// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2022, Olof Kraigher olof.kraigher@gmail.com

use crate::ast::Declaration;
use crate::ast::Designator;
use crate::ast::Mode;
use crate::ast::ObjectClass;
use crate::ast::Operator;
use crate::data::DiagnosticHandler;
use crate::data::Symbol;
use crate::syntax::Symbols;
use crate::SrcPos;

use super::analyze::AnalyzeContext;
use super::formal_region::FormalRegion;
use super::named_entity::*;
use super::region::*;
use super::EntityId;

#[derive(Clone, Copy)]
pub(crate) struct UniversalTypes {
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

pub(crate) struct StandardTypes {
    pub boolean: EntityId,
    pub boolean_vector: EntityId,
    pub bit: EntityId,
    pub bit_vector: EntityId,
    pub character: EntityId,
    pub string: EntityId,
    pub integer: EntityId,
    pub natural: EntityId,
    pub real: EntityId,
    pub time: EntityId,
    pub file_open_kind: EntityId,
    pub file_open_status: EntityId,
    pub severity_level: EntityId,
}

impl StandardTypes {
    pub fn new(arena: &Arena, decls: &mut [Declaration]) -> Self {
        let mut boolean = None;
        let mut boolean_vector = None;
        let mut bit = None;
        let mut bit_vector = None;
        let mut character = None;
        let mut string = None;
        let mut integer = None;
        let mut natural = None;
        let mut real = None;
        let mut time = None;
        let mut file_open_status = None;
        let mut file_open_kind = None;
        let mut severity_level = None;

        // Reserve space in the arena for the standard types
        for decl in decls.iter_mut() {
            if let Declaration::Type(ref mut type_decl) = decl {
                let id = arena
                    .alloc(
                        Designator::Identifier(type_decl.ident.tree.item.clone()),
                        Related::None,
                        AnyEntKind::Type(Type::Incomplete),
                        Some(type_decl.ident.tree.pos.clone()),
                    )
                    .id();
                let name = type_decl.ident.tree.item.name();
                match name.bytes.as_slice() {
                    b"BOOLEAN" => {
                        boolean = Some(id);
                    }
                    b"BOOLEAN_VECTOR" => {
                        boolean_vector = Some(id);
                    }
                    b"BIT" => {
                        bit = Some(id);
                    }
                    b"BIT_VECTOR" => {
                        bit_vector = Some(id);
                    }
                    b"CHARACTER" => {
                        character = Some(id);
                    }
                    b"STRING" => {
                        string = Some(id);
                    }
                    b"INTEGER" => {
                        integer = Some(id);
                    }
                    b"NATURAL" => {
                        natural = Some(id);
                    }
                    b"REAL" => {
                        real = Some(id);
                    }
                    b"TIME" => {
                        time = Some(id);
                    }
                    b"FILE_OPEN_KIND" => {
                        file_open_kind = Some(id);
                    }
                    b"FILE_OPEN_STATUS" => {
                        file_open_status = Some(id);
                    }
                    b"SEVERITY_LEVEL" => {
                        severity_level = Some(id);
                    }
                    _ => {
                        continue;
                    }
                }
                type_decl.ident.decl = Some(id);
            }
        }

        Self {
            boolean: boolean.unwrap(),
            boolean_vector: boolean_vector.unwrap(),
            bit: bit.unwrap(),
            bit_vector: bit_vector.unwrap(),
            character: character.unwrap(),
            string: string.unwrap(),
            integer: integer.unwrap(),
            natural: natural.unwrap(),
            real: real.unwrap(),
            time: time.unwrap(),
            file_open_kind: file_open_kind.unwrap(),
            file_open_status: file_open_status.unwrap(),
            severity_level: severity_level.unwrap(),
        }
    }
}

impl<'a> AnalyzeContext<'a> {
    fn symbol(&self, name: &str) -> Symbol {
        self.root.symbol_utf8(name)
    }

    fn standard_types(&self) -> &StandardTypes {
        self.root.standard_types.as_ref().unwrap()
    }

    pub(crate) fn string(&self) -> TypeEnt<'a> {
        self.arena.get_type(self.standard_types().string)
    }

    pub(crate) fn boolean(&self) -> TypeEnt<'a> {
        self.arena.get_type(self.standard_types().boolean)
    }

    pub(crate) fn boolean_vector(&self) -> TypeEnt<'a> {
        self.arena.get_type(self.standard_types().boolean_vector)
    }

    pub(crate) fn bit(&self) -> TypeEnt<'a> {
        self.arena.get_type(self.standard_types().bit)
    }

    pub(crate) fn bit_vector(&self) -> TypeEnt<'a> {
        self.arena.get_type(self.standard_types().bit_vector)
    }

    pub(crate) fn natural(&self) -> TypeEnt<'a> {
        self.arena.get_type(self.standard_types().natural)
    }

    #[allow(dead_code)]
    pub(crate) fn character(&self) -> TypeEnt<'a> {
        self.arena.get_type(self.standard_types().character)
    }

    pub(crate) fn universal_integer(&self) -> BaseType<'a> {
        self.arena
            .get_type(self.root.universal.as_ref().unwrap().integer)
            .base()
    }

    pub(crate) fn universal_real(&self) -> BaseType<'a> {
        self.arena
            .get_type(self.root.universal.as_ref().unwrap().real)
            .base()
    }

    pub(crate) fn integer(&self) -> TypeEnt<'a> {
        self.arena.get_type(self.standard_types().integer)
    }

    pub(crate) fn real(&self) -> TypeEnt<'a> {
        self.arena.get_type(self.standard_types().real)
    }

    pub(crate) fn time(&self) -> TypeEnt<'a> {
        self.arena.get_type(self.standard_types().time)
    }

    fn file_open_kind(&self) -> TypeEnt<'a> {
        self.arena.get_type(self.standard_types().file_open_kind)
    }

    fn file_open_status(&self) -> TypeEnt<'a> {
        self.arena.get_type(self.standard_types().file_open_status)
    }

    pub(crate) fn severity_level(&self) -> TypeEnt<'a> {
        self.arena.get_type(self.standard_types().severity_level)
    }

    /// Create implicit MAXIMUM/MINIMUM
    // function MINIMUM (L, R: T) return T;
    // function MAXIMUM (L, R: T) return T;
    fn min_or_maximum(&self, name: &str, type_ent: TypeEnt<'a>) -> EntRef<'a> {
        let mut formals = FormalRegion::new_params();
        formals.add(self.arena.explicit(
            self.symbol("L"),
            AnyEntKind::Object(Object::if_constant(Subtype::new(type_ent))),
            type_ent.decl_pos(),
        ));

        formals.add(self.arena.explicit(
            self.symbol("R"),
            AnyEntKind::Object(Object::if_constant(Subtype::new(type_ent))),
            type_ent.decl_pos(),
        ));

        self.arena.implicit(
            type_ent.into(),
            self.symbol(name),
            AnyEntKind::new_function_decl(formals, type_ent),
            type_ent.decl_pos(),
        )
    }

    fn elementwise_min_or_maximum(
        &self,
        name: &str,
        arr_typ: TypeEnt<'a>,
        elem_typ: TypeEnt<'a>,
    ) -> EntRef<'a> {
        let mut formals = FormalRegion::new_params();
        formals.add(self.arena.explicit(
            self.symbol("L"),
            AnyEntKind::Object(Object::if_constant(Subtype::new(arr_typ))),
            arr_typ.decl_pos(),
        ));

        self.arena.implicit(
            arr_typ.into(),
            self.symbol(name),
            AnyEntKind::new_function_decl(formals, elem_typ),
            arr_typ.decl_pos(),
        )
    }

    fn unary(&self, op: Operator, typ: TypeEnt<'a>, return_type: TypeEnt<'a>) -> EntRef<'a> {
        let mut formals = FormalRegion::new_params();
        formals.add(self.arena.explicit(
            // @TODO anonymous
            self.symbol("V"),
            AnyEntKind::Object(Object::if_constant(Subtype::new(typ))),
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
            AnyEntKind::Object(Object::if_constant(Subtype::new(left))),
            implicit_of.decl_pos(),
        ));

        formals.add(self.arena.explicit(
            // @TODO anonymous
            self.symbol("R"),
            AnyEntKind::Object(Object::if_constant(Subtype::new(right))),
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
        self.min_or_maximum("MINIMUM", type_ent)
    }

    pub fn maximum(&self, type_ent: TypeEnt<'a>) -> EntRef<'a> {
        self.min_or_maximum("MAXIMUM", type_ent)
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
                AnyEntKind::Object(Object::if_constant(Subtype::new(string))),
                file_type.decl_pos(),
            ));

            formals.add(self.arena.explicit(
                self.symbol("Open_Kind"),
                AnyEntKind::Object(
                    Object::if_constant(Subtype::new(file_open_kind)).with_default(),
                ),
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
                AnyEntKind::Object(Object::if_constant(Subtype::new(file_open_status))),
                file_type.decl_pos(),
            ));
            formals.add(self.arena.explicit(
                self.symbol("F"),
                AnyEntKind::InterfaceFile(file_type.to_owned()),
                file_type.decl_pos(),
            ));
            formals.add(self.arena.explicit(
                self.symbol("External_Name"),
                AnyEntKind::Object(Object::if_constant(Subtype::new(string))),
                file_type.decl_pos(),
            ));

            formals.add(self.arena.explicit(
                self.symbol("Open_Kind"),
                AnyEntKind::Object(
                    Object::if_constant(Subtype::new(file_open_kind)).with_default(),
                ),
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
                    is_port: false,
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
                AnyEntKind::Object(Object::if_constant(Subtype::new(type_mark))),
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
            AnyEntKind::Object(Object::if_constant(Subtype::new(type_ent))),
            type_ent.decl_pos(),
        ));

        self.arena.implicit(
            type_ent.into(),
            self.symbol("TO_STRING"),
            AnyEntKind::new_function_decl(formals, self.string()),
            type_ent.decl_pos(),
        )
    }

    /// Create implicit TO_STRING
    /// function TO_STRING (VALUE: T) return STRING;
    pub fn to_x_string(&self, name: &str, type_ent: TypeEnt<'a>) -> EntRef<'a> {
        let mut formals = FormalRegion::new_params();
        formals.add(self.arena.explicit(
            self.symbol("VALUE"),
            AnyEntKind::Object(Object::if_constant(Subtype::new(type_ent))),
            type_ent.decl_pos(),
        ));

        self.arena.implicit(
            type_ent.into(),
            self.symbol(name),
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
                is_port: false,
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

    pub fn comparison(&self, op: Operator, typ: TypeEnt<'a>) -> EntRef<'a> {
        self.binary(op, typ, typ, typ, self.boolean())
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
            self.minimum(typ),
            self.maximum(typ),
            self.create_to_string(typ),
            self.symmetric_unary(Operator::Minus, typ),
            self.symmetric_unary(Operator::Plus, typ),
            self.symmetric_binary(Operator::Plus, typ),
            self.symmetric_binary(Operator::Minus, typ),
            // 9.2.7 Multiplying operators
            self.symmetric_binary(Operator::Times, typ),
            self.symmetric_binary(Operator::Div, typ),
            // 9.2.8 Miscellaneous operators
            self.symmetric_unary(Operator::Abs, typ),
            self.binary(Operator::Pow, typ, typ, integer, typ),
        ]
        .into_iter()
        .chain(
            if kind == UniversalType::Integer {
                Some(
                    [
                        self.symmetric_binary(Operator::Mod, typ),
                        self.symmetric_binary(Operator::Rem, typ),
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

    pub fn universal_implicits(
        &self,
        kind: UniversalType,
        typ: TypeEnt<'a>,
    ) -> impl Iterator<Item = EntRef<'a>> {
        [
            self.minimum(typ),
            self.maximum(typ),
            self.create_to_string(typ),
            self.symmetric_unary(Operator::Minus, typ),
            self.symmetric_unary(Operator::Plus, typ),
            self.symmetric_binary(Operator::Plus, typ),
            self.symmetric_binary(Operator::Minus, typ),
            // 9.2.7 Multiplying operators
            self.symmetric_binary(Operator::Times, typ),
            self.symmetric_binary(Operator::Div, typ),
            // 9.2.8 Miscellaneous operators
            self.symmetric_unary(Operator::Abs, typ),
            self.binary(
                Operator::Pow,
                typ,
                typ,
                self.universal_integer().into(),
                typ,
            ),
        ]
        .into_iter()
        .chain(
            if kind == UniversalType::Integer {
                Some(
                    [
                        self.symmetric_binary(Operator::Mod, typ),
                        self.symmetric_binary(Operator::Rem, typ),
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
            self.minimum(typ),
            self.maximum(typ),
            self.symmetric_unary(Operator::Minus, typ),
            self.symmetric_unary(Operator::Plus, typ),
            self.symmetric_unary(Operator::Abs, typ),
            self.symmetric_binary(Operator::Plus, typ),
            self.symmetric_binary(Operator::Minus, typ),
            // 9.2.7 Multiplying operators
            self.binary(Operator::Times, typ, typ, integer, typ),
            self.binary(Operator::Times, typ, typ, real, typ),
            self.binary(Operator::Times, typ, integer, typ, typ),
            self.binary(Operator::Times, typ, real, typ, typ),
            self.binary(Operator::Div, typ, typ, integer, typ),
            self.binary(Operator::Div, typ, typ, real, typ),
            self.binary(
                Operator::Div,
                typ,
                typ,
                typ,
                self.universal_integer().into(),
            ),
            self.symmetric_binary(Operator::Mod, typ),
            self.symmetric_binary(Operator::Rem, typ),
        ]
        .into_iter()
        .chain(self.comparators(typ).into_iter())
    }

    pub fn enum_implicits(
        &self,
        typ: TypeEnt<'a>,
        matching_op: bool,
    ) -> impl Iterator<Item = EntRef<'a>> {
        [
            self.create_to_string(typ),
            self.minimum(typ),
            self.maximum(typ),
        ]
        .into_iter()
        .chain(self.comparators(typ).into_iter())
        .chain(
            if matching_op {
                Some(
                    [
                        self.symmetric_binary(Operator::QueEQ, typ),
                        self.symmetric_binary(Operator::QueNE, typ),
                        self.symmetric_binary(Operator::QueGT, typ),
                        self.symmetric_binary(Operator::QueGTE, typ),
                        self.symmetric_binary(Operator::QueLT, typ),
                        self.symmetric_binary(Operator::QueLTE, typ),
                    ]
                    .into_iter(),
                )
            } else {
                None
            }
            .into_iter()
            .flatten(),
        )
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
            self.binary(
                Operator::Concat,
                array_type,
                array_type,
                elem_type,
                array_type,
            ),
            self.binary(
                Operator::Concat,
                array_type,
                elem_type,
                array_type,
                array_type,
            ),
            self.symmetric_binary(Operator::Concat, array_type),
            self.binary(
                Operator::Concat,
                array_type,
                elem_type,
                elem_type,
                array_type,
            ),
        ]
        .into_iter()
    }

    pub fn array_implicits(
        &self,
        typ: TypeEnt<'a>,
        matching_op: bool,
    ) -> impl Iterator<Item = EntRef<'a>> {
        let Type::Array{indexes, elem_type, ..}  = typ.kind() else {
            unreachable!("Must be array type")
        };

        let is_scalar = matches!(
            elem_type.base().kind(),
            Type::Integer | Type::Real | Type::Physical | Type::Enum(_)
        );

        let is_one_dimensional = indexes.len() == 1;
        let is_character_elem = matches!(elem_type.base().kind(), Type::Enum(designators) if designators.iter().all(|des| matches!(des, Designator::Character(_))));

        [
            self.comparison(Operator::EQ, typ),
            self.comparison(Operator::NE, typ),
        ]
        .into_iter()
        .chain(if is_one_dimensional && is_character_elem {
            // To string is only defined for 1d array types with character elements
            Some(self.create_to_string(typ)).into_iter()
        } else {
            None.into_iter()
        })
        .chain(
            (if is_one_dimensional {
                Some(self.concatenations(typ, *elem_type))
            } else {
                None
            })
            .into_iter()
            .flatten(),
        )
        .chain(
            (if is_scalar {
                Some(
                    [
                        self.comparison(Operator::GT, typ),
                        self.comparison(Operator::GTE, typ),
                        self.comparison(Operator::LT, typ),
                        self.comparison(Operator::LTE, typ),
                        self.elementwise_min_or_maximum("MINIMUM", typ, *elem_type),
                        self.elementwise_min_or_maximum("MAXIMUM", typ, *elem_type),
                    ]
                    .into_iter(),
                )
            } else {
                None
            })
            .into_iter()
            .flatten(),
        )
        .chain(
            if matching_op {
                Some(
                    [
                        self.binary(Operator::QueEQ, typ, typ, typ, *elem_type),
                        self.binary(Operator::QueNE, typ, typ, typ, *elem_type),
                        self.binary(Operator::QueGT, typ, typ, typ, *elem_type),
                        self.binary(Operator::QueGTE, typ, typ, typ, *elem_type),
                        self.binary(Operator::QueLT, typ, typ, typ, *elem_type),
                        self.binary(Operator::QueLTE, typ, typ, typ, *elem_type),
                    ]
                    .into_iter(),
                )
            } else {
                None
            }
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

    // Return the implicit things defined at the end of the standard packge
    pub fn end_of_package_implicits(
        &self,
        region: &mut Region<'a>,
        diagnostics: &mut dyn DiagnosticHandler,
    ) {
        {
            let time = self.time();
            let to_string = self.create_to_string(time);

            unsafe {
                self.arena.add_implicit(time.id(), to_string);
            };
            region.add(to_string, diagnostics);
        }

        for typ in [self.bit(), self.boolean()] {
            let implicits = [
                self.symmetric_binary(Operator::And, typ),
                self.symmetric_binary(Operator::Or, typ),
                self.symmetric_binary(Operator::Nand, typ),
                self.symmetric_binary(Operator::Nor, typ),
                self.symmetric_binary(Operator::Xor, typ),
                self.symmetric_binary(Operator::Xnor, typ),
                self.symmetric_unary(Operator::Not, typ),
            ]
            .into_iter();

            for ent in implicits {
                unsafe {
                    self.arena.add_implicit(typ.id(), ent);
                };
                region.add(ent, diagnostics);
            }
        }

        for (styp, atyp) in [
            (self.boolean(), self.boolean_vector()),
            (self.bit(), self.bit_vector()),
        ] {
            let ops = [
                Operator::And,
                Operator::Or,
                Operator::Nand,
                Operator::Nor,
                Operator::Xor,
                Operator::Xnor,
                Operator::Not,
            ];

            let implicits = ops.iter().flat_map(|op| {
                let op = *op;
                [
                    // A op A -> A
                    self.symmetric_binary(op, atyp),
                    if op == Operator::Not {
                        // op A -> A
                        self.unary(op, atyp, atyp)
                    } else {
                        // op A -> S
                        self.unary(op, atyp, styp)
                    },
                    // A op S -> A
                    self.binary(op, atyp, atyp, styp, atyp),
                    // S op A -> A
                    self.binary(op, atyp, styp, atyp, atyp),
                ]
                .into_iter()
            });

            for ent in implicits {
                // This is safe because the standard package is analyzed in a single thread
                unsafe {
                    self.arena.add_implicit(atyp.id(), ent);
                };
                region.add(ent, diagnostics);
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
                    is_port: false,
                    mode: Some(Mode::In),
                    subtype: Subtype::new(real.to_owned()),
                    has_default: false,
                }),
                real.decl_pos(),
            ));

            formals.add(self.arena.explicit(
                self.symbol("DIGITS"),
                AnyEntKind::Object(Object::if_constant(Subtype::new(natural))),
                real.decl_pos(),
            ));

            let ent = self.arena.implicit(
                real.into(),
                self.symbol("TO_STRING"),
                AnyEntKind::new_function_decl(formals, string),
                real.decl_pos(),
            );

            // This is safe because the standard package is analyzed in a single thread
            unsafe {
                self.arena.add_implicit(real.id(), ent);
            };
            region.add(ent, diagnostics);
        }

        // function TO_STRING (VALUE: REAL; FORMAT: STRING) return STRING;
        {
            let real = self.real();
            let string = self.string();

            let mut formals = FormalRegion::new_params();
            formals.add(self.arena.explicit(
                self.symbol("VALUE"),
                AnyEntKind::Object(Object::if_constant(Subtype::new(real))),
                real.decl_pos(),
            ));

            formals.add(self.arena.explicit(
                self.symbol("FORMAT"),
                AnyEntKind::Object(Object::if_constant(Subtype::new(string))),
                real.decl_pos(),
            ));

            let ent = self.arena.implicit(
                real.into(),
                self.symbol("TO_STRING"),
                AnyEntKind::new_function_decl(formals, string),
                real.decl_pos(),
            );

            // This is safe because the standard package is analyzed in a single thread
            unsafe {
                self.arena.add_implicit(real.id(), ent);
            };
            region.add(ent, diagnostics);
        }

        // function TO_STRING (VALUE: TIME; UNIT: TIME) return STRING
        {
            let time = self.time();
            let string = self.string();
            let mut formals = FormalRegion::new_params();
            formals.add(self.arena.explicit(
                self.symbol("VALUE"),
                AnyEntKind::Object(Object::if_constant(Subtype::new(time))),
                time.decl_pos(),
            ));

            formals.add(self.arena.explicit(
                self.symbol("UNIT"),
                AnyEntKind::Object(Object::if_constant(Subtype::new(time))),
                time.decl_pos(),
            ));

            let ent = self.arena.implicit(
                time.into(),
                self.symbol("TO_STRING"),
                AnyEntKind::new_function_decl(formals, string),
                time.decl_pos(),
            );

            // This is safe because the standard package is analyzed in a single thread
            unsafe {
                self.arena.add_implicit(time.id(), ent);
            };
            region.add(ent, diagnostics);
        }

        // Special TO_STRING variants for bit-vector
        {
            let typ = self.bit_vector();
            let to_string = typ.implicits.iter().find(|ent| matches!(ent.designator(), Designator::Identifier(ident) if ident.name_utf8() == "TO_STRING")).unwrap();

            let to_bstring = self.arena.alloc(
                Designator::Identifier(self.symbol("TO_BSTRING")),
                Related::ImplicitOf(typ.into()),
                AnyEntKind::Overloaded(Overloaded::Alias(
                    OverloadedEnt::from_any(to_string).unwrap(),
                )),
                to_string.decl_pos().cloned(),
            );

            let to_binary_string = self.arena.alloc(
                Designator::Identifier(self.symbol("TO_BINARY_STRING")),
                Related::ImplicitOf(typ.into()),
                AnyEntKind::Overloaded(Overloaded::Alias(
                    OverloadedEnt::from_any(to_string).unwrap(),
                )),
                to_string.decl_pos().cloned(),
            );

            let to_ostring = self.to_x_string("TO_OSTRING", typ);

            let to_octal_string = self.arena.alloc(
                Designator::Identifier(self.symbol("TO_OCTAL_STRING")),
                Related::ImplicitOf(typ.into()),
                AnyEntKind::Overloaded(Overloaded::Alias(
                    OverloadedEnt::from_any(to_ostring).unwrap(),
                )),
                to_string.decl_pos().cloned(),
            );

            let to_hstring = self.to_x_string("TO_HSTRING", typ);
            let to_hex_string = self.arena.alloc(
                Designator::Identifier(self.symbol("TO_HEX_STRING")),
                Related::ImplicitOf(typ.into()),
                AnyEntKind::Overloaded(Overloaded::Alias(
                    OverloadedEnt::from_any(to_hstring).unwrap(),
                )),
                to_string.decl_pos().cloned(),
            );

            let implicits = [
                to_bstring,
                to_binary_string,
                to_ostring,
                to_octal_string,
                to_hstring,
                to_hex_string,
            ];

            for ent in implicits {
                // This is safe because the standard package is analyzed in a single thread
                unsafe {
                    self.arena.add_implicit(typ.id(), ent);
                };
                region.add(ent, diagnostics);
            }
        }

        // ?? operator for bit
        {
            let typ = self.bit();
            let qq = self.unary(Operator::QueQue, typ, self.boolean());

            unsafe {
                self.arena.add_implicit(typ.id(), qq);
            };
            region.add(qq, diagnostics);
        }
    }
}
