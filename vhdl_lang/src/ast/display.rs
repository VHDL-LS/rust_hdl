// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2021, Olof Kraigher olof.kraigher@gmail.com

//! Implementation of Display

use super::*;
use std::fmt::{Display, Formatter, Result};

impl<T: Display> Display for WithPos<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", &self.item)
    }
}

impl Display for BaseSpecifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            BaseSpecifier::B => write!(f, "b"),
            BaseSpecifier::O => write!(f, "o"),
            BaseSpecifier::X => write!(f, "x"),
            BaseSpecifier::UB => write!(f, "ub"),
            BaseSpecifier::UO => write!(f, "uo"),
            BaseSpecifier::UX => write!(f, "ux"),
            BaseSpecifier::SB => write!(f, "sb"),
            BaseSpecifier::SO => write!(f, "so"),
            BaseSpecifier::SX => write!(f, "sx"),
            BaseSpecifier::D => write!(f, "d"),
        }
    }
}

impl Display for Unary {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Unary::And => write!(f, "and "),
            Unary::Or => write!(f, "or "),
            Unary::Nand => write!(f, "nand "),
            Unary::Nor => write!(f, "nor "),
            Unary::Xor => write!(f, "xor "),
            Unary::Xnor => write!(f, "xnor "),
            Unary::Abs => write!(f, "abs "),
            Unary::Not => write!(f, "not "),
            Unary::Minus => write!(f, "-"),
            Unary::Plus => write!(f, "+"),
            Unary::QueQue => write!(f, "?? "),
        }
    }
}

impl Display for Binary {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Binary::And => write!(f, " and "),
            Binary::Or => write!(f, " or "),
            Binary::Nand => write!(f, " nand "),
            Binary::Nor => write!(f, " nor "),
            Binary::Xor => write!(f, " xor "),
            Binary::Xnor => write!(f, " xnor "),
            Binary::EQ => write!(f, " = "),
            Binary::NE => write!(f, " /= "),
            Binary::LT => write!(f, " < "),
            Binary::LTE => write!(f, " <= "),
            Binary::GT => write!(f, " > "),
            Binary::GTE => write!(f, " >= "),
            Binary::QueEQ => write!(f, " ?= "),
            Binary::QueNE => write!(f, " ?/= "),
            Binary::QueLT => write!(f, " ?< "),
            Binary::QueLTE => write!(f, " ?<= "),
            Binary::QueGT => write!(f, " ?> "),
            Binary::QueGTE => write!(f, " ?>= "),
            Binary::SLL => write!(f, " sll "),
            Binary::SRL => write!(f, " srl "),
            Binary::SLA => write!(f, " sla "),
            Binary::SRA => write!(f, " sra "),
            Binary::ROL => write!(f, " rol "),
            Binary::ROR => write!(f, " ror "),
            Binary::Plus => write!(f, " + "),
            Binary::Minus => write!(f, " - "),
            Binary::Concat => write!(f, " & "),
            Binary::Times => write!(f, " * "),
            Binary::Div => write!(f, " / "),
            Binary::Mod => write!(f, " mod "),
            Binary::Rem => write!(f, " rem "),
            Binary::Pow => write!(f, " ** "),
        }
    }
}

impl Display for AttributeName {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", self.name)?;
        if let Some(ref signature) = self.signature {
            write!(f, "{}", signature)?;
        }
        write!(f, "'{}", self.attr)?;
        if let Some(ref expr) = self.expr {
            write!(f, "({})", expr)
        } else {
            Ok(())
        }
    }
}

impl Display for ExternalObjectClass {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            ExternalObjectClass::Constant => write!(f, "constant"),
            ExternalObjectClass::Signal => write!(f, "signal"),
            ExternalObjectClass::Variable => write!(f, "variable"),
        }
    }
}

impl Display for ExternalPath {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            ExternalPath::Package(ref path) => {
                write!(f, "@{}", path)
            }
            ExternalPath::Absolute(ref path) => {
                write!(f, ".{}", path)
            }
            ExternalPath::Relative(ref path, ref up_levels) => {
                write!(f, "{}{}", "^.".repeat(*up_levels), path)
            }
        }
    }
}

impl Display for ExternalName {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "<< {} {} : {} >>", self.class, self.path, self.subtype)
    }
}

impl Display for Name {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Name::Designator(designator) => write!(f, "{}", designator),
            Name::Selected(ref prefix, ref designator) => write!(f, "{}.{}", prefix, designator),
            Name::SelectedAll(ref prefix) => write!(f, "{}.all", prefix),
            Name::Indexed(ref prefix, ref exprs) => {
                write!(f, "{}(", prefix)?;
                let mut first = true;
                for expr in exprs {
                    if first {
                        write!(f, "{}", expr)?;
                    } else {
                        write!(f, ", {}", expr)?;
                    }
                    first = false;
                }
                write!(f, ")")
            }
            Name::Slice(ref prefix, ref drange) => write!(f, "{}({})", prefix, drange),
            Name::Attribute(ref attr) => write!(f, "{}", attr),
            Name::FunctionCall(ref fcall) => write!(f, "{}", fcall),
            Name::External(ref ename) => write!(f, "{}", ename),
        }
    }
}

impl Display for SelectedName {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            SelectedName::Selected(ref prefix, ref des) => write!(f, "{}.{}", prefix, &des),
            SelectedName::Designator(ref des) => write!(f, "{}", &des),
        }
    }
}

impl Display for FunctionCall {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", self.name)?;
        let mut first = true;
        for param in &self.parameters {
            if first {
                write!(f, "({}", param)?;
            } else {
                write!(f, ", {}", param)?;
            }
            first = false;
        }
        if !first {
            write!(f, ")")
        } else {
            Ok(())
        }
    }
}

impl Display for Choice {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Choice::Expression(ref expr) => write!(f, "{}", expr),
            Choice::DiscreteRange(ref drange) => write!(f, "{}", drange),
            Choice::Others => write!(f, "others"),
        }
    }
}

impl Display for ElementAssociation {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            ElementAssociation::Positional(ref expr) => {
                write!(f, "{}", expr)
            }
            ElementAssociation::Named(ref choices, ref expr) => {
                let mut first = true;
                for choice in choices {
                    if first {
                        write!(f, "{}", choice)?;
                    } else {
                        write!(f, " | {}", choice)?;
                    }
                    first = false;
                }
                write!(f, " => {}", expr)
            }
        }
    }
}

impl Display for ActualPart {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            ActualPart::Expression(ref expr) => write!(f, "{}", expr),
            ActualPart::Open => write!(f, "open"),
        }
    }
}

impl Display for AssociationElement {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        if let Some(ref formal) = self.formal {
            write!(f, "{} => ", formal)?;
        }
        write!(f, "{}", self.actual)
    }
}

impl Display for AbstractLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            AbstractLiteral::Integer(val) => write!(f, "{}", val),
            AbstractLiteral::Real(val) => write!(f, "{}", val),
        }
    }
}

impl Display for BitString {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        if let Some(length) = self.length {
            write!(f, "{}", length)?;
        }
        write!(f, "{}\"{}\"", self.base, self.value)
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Literal::String(ref val) => write!(f, "\"{}\"", val),
            Literal::BitString(ref val) => write!(f, "{}", val),
            Literal::Character(byte) => write!(f, "'{}'", *byte as char),
            Literal::AbstractLiteral(ref val) => write!(f, "{}", val),
            Literal::Physical(ref val, ref sym) => write!(f, "{} {}", val, sym),
            Literal::Null => write!(f, "null"),
        }
    }
}

impl Display for Allocator {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Allocator::Qualified(ref qexpr) => write!(f, "{}", qexpr),
            Allocator::Subtype(ref subtype) => write!(f, "{}", subtype),
        }
    }
}

impl Display for QualifiedExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self.expr.item {
            Expression::Aggregate(..) => write!(f, "{}'{}", self.name, self.expr),
            _ => write!(f, "{}'({})", self.name, self.expr),
        }
    }
}

fn binary_precedence(op: Binary) -> usize {
    match op {
        Binary::And => 2,
        Binary::Or => 2,
        Binary::Nand => 2,
        Binary::Nor => 2,
        Binary::Xor => 2,
        Binary::Xnor => 2,

        Binary::EQ => 3,
        Binary::NE => 3,
        Binary::LT => 3,
        Binary::LTE => 3,
        Binary::GT => 3,
        Binary::GTE => 3,
        Binary::QueEQ => 3,
        Binary::QueNE => 3,
        Binary::QueLT => 3,
        Binary::QueLTE => 3,
        Binary::QueGT => 3,
        Binary::QueGTE => 3,

        Binary::SLL => 4,
        Binary::SRL => 4,
        Binary::SLA => 4,
        Binary::SRA => 4,
        Binary::ROL => 4,
        Binary::ROR => 4,

        Binary::Plus => 5,
        Binary::Minus => 5,
        Binary::Concat => 5,

        Binary::Times => 7,
        Binary::Div => 7,
        Binary::Mod => 7,
        Binary::Rem => 7,

        Binary::Pow => 8,
    }
}

fn unary_precedence(op: Unary) -> usize {
    match op {
        Unary::And => 8,
        Unary::Or => 8,
        Unary::Nand => 8,
        Unary::Nor => 8,
        Unary::Xor => 8,
        Unary::Xnor => 8,

        Unary::Abs => 8,
        Unary::Not => 8,
        Unary::Minus => 6,
        Unary::Plus => 6,
        Unary::QueQue => 1,
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Expression::Binary(ref op, ref lhs, ref rhs) => {
                // Add parentheses as necessary to satisfy order of precedence.
                let precedence = binary_precedence(*op);
                match lhs.item {
                    Expression::Binary(op, ..) => {
                        if precedence <= binary_precedence(op) {
                            write!(f, "{}", lhs)?;
                        } else {
                            write!(f, "({})", lhs)?;
                        }
                    }
                    Expression::Unary(op, ..) => {
                        if precedence <= unary_precedence(op) {
                            write!(f, "{}", lhs)?;
                        } else {
                            write!(f, "({})", lhs)?;
                        }
                    }
                    _ => write!(f, "{}", lhs)?,
                }
                write!(f, "{}", op)?;
                match rhs.item {
                    Expression::Binary(op, ..) => {
                        if precedence < binary_precedence(op) {
                            write!(f, "{}", rhs)
                        } else {
                            write!(f, "({})", rhs)
                        }
                    }
                    _ => write!(f, "{}", rhs),
                }
            }
            Expression::Unary(ref op, ref expr) => {
                // Add parentheses as necessary to satisfy order of precedence.
                let precedence = unary_precedence(*op);
                write!(f, "{}", op)?;
                match expr.item {
                    // Binary operators having precedence over unary ones is
                    // confusing, so always add parentheses.
                    Expression::Binary(..) => {
                        write!(f, "({})", expr)
                    }
                    // Chained unary operators are always left to right, but
                    // chained operators with the same precedence are
                    // parenthesized for clarity.
                    Expression::Unary(op, ..) => {
                        if precedence != unary_precedence(op) {
                            write!(f, "{}", expr)
                        } else {
                            write!(f, "({})", expr)
                        }
                    }
                    _ => write!(f, "{}", expr),
                }
            }
            Expression::Aggregate(ref assocs) => {
                let mut first = true;
                for assoc in assocs {
                    if first {
                        write!(f, "({}", assoc)?;
                    } else {
                        write!(f, ", {}", assoc)?;
                    }
                    first = false;
                }
                if !first {
                    write!(f, ")")
                } else {
                    Ok(())
                }
            }
            Expression::Qualified(ref qexpr) => write!(f, "{}", qexpr),
            Expression::Name(ref name) => write!(f, "{}", name),
            Expression::Literal(ref literal) => write!(f, "{}", literal),
            Expression::New(ref alloc) => write!(f, "new {}", alloc),
        }
    }
}

impl Display for Direction {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Direction::Ascending => write!(f, "to"),
            Direction::Descending => write!(f, "downto"),
        }
    }
}

impl Display for DiscreteRange {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            DiscreteRange::Discrete(ref name, ref range) => {
                write!(f, "{}", name)?;
                match range {
                    Some(ref range) => write!(f, " range {}", range),
                    None => Ok(()),
                }
            }
            DiscreteRange::Range(ref range) => {
                write!(f, "{}", range)
            }
        }
    }
}

impl Display for RangeConstraint {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "{} {} {}",
            self.left_expr, self.direction, self.right_expr,
        )
    }
}

impl Display for Range {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Range::Range(ref constraint) => write!(f, "{}", constraint),
            Range::Attribute(ref attr) => write!(f, "{}", attr),
        }
    }
}

impl Display for ElementConstraint {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}{}", self.ident, self.constraint)
    }
}

impl Display for SubtypeConstraint {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            SubtypeConstraint::Range(ref range) => {
                write!(f, " range {}", range)
            }
            SubtypeConstraint::Array(ref dranges, ref constraint) => {
                write!(f, "(")?;
                let mut first = true;
                for drange in dranges {
                    if first {
                        write!(f, "{}", drange)?;
                    } else {
                        write!(f, ", {}", drange)?;
                    }
                    first = false;
                }
                if first {
                    write!(f, "open")?;
                }
                match constraint {
                    Some(ref constraint) => write!(f, "){}", constraint),
                    None => write!(f, ")"),
                }
            }
            SubtypeConstraint::Record(constraints) => {
                write!(f, "(")?;
                let mut first = true;
                for constraint in constraints {
                    if first {
                        write!(f, "{}", constraint)?;
                    } else {
                        write!(f, ", {}", constraint)?;
                    }
                    first = false;
                }
                write!(f, ")")
            }
        }
    }
}

impl Display for RecordElementResolution {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{} {}", self.ident, self.resolution)
    }
}

impl Display for ResolutionIndication {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            ResolutionIndication::FunctionName(ref name) => {
                write!(f, "{}", name)
            }
            ResolutionIndication::ArrayElement(ref name) => {
                write!(f, "({})", name)
            }
            ResolutionIndication::Record(elem_resolutions) => {
                let mut first = true;
                for elem_resolution in elem_resolutions {
                    if first {
                        write!(f, "({}", elem_resolution)?;
                    } else {
                        write!(f, ", {}", elem_resolution)?;
                    }
                    first = false;
                }
                if !first {
                    write!(f, ")")
                } else {
                    Ok(())
                }
            }
            ResolutionIndication::Unresolved => Ok(()),
        }
    }
}

impl Display for SubtypeIndication {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self.resolution {
            ResolutionIndication::Unresolved => (),
            _ => write!(f, "{} ", self.resolution)?,
        }
        write!(f, "{}", self.type_mark)?;
        match self.constraint {
            Some(ref constraint) => write!(f, "{}", constraint),
            None => Ok(()),
        }
    }
}

impl Display for ArrayIndex {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            ArrayIndex::IndexSubtypeDefintion(ref type_mark) => {
                write!(f, "{} range <>", type_mark)
            }
            ArrayIndex::Discrete(ref range) => {
                write!(f, "{}", range)
            }
        }
    }
}

impl Display for ElementDeclaration {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{} : {}", self.ident, self.subtype)
    }
}

impl Display for Designator {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Designator::Identifier(ref sym) => write!(f, "{}", sym),
            Designator::OperatorSymbol(ref latin1) => write!(f, "\"{}\"", latin1),
            Designator::Character(byte) => write!(f, "'{}'", *byte as char),
        }
    }
}

impl<T: Display> Display for WithRef<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", &self.item)
    }
}

impl Display for AliasDeclaration {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "alias {}", self.designator)?;
        if let Some(ref subtype_indication) = self.subtype_indication {
            write!(f, " : {}", subtype_indication)?;
        }
        write!(f, " is {}", self.name)?;
        match self.signature {
            Some(ref signature) => write!(f, "{};", signature),
            None => write!(f, ";"),
        }
    }
}

impl Display for EnumerationLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            EnumerationLiteral::Identifier(ref sym) => write!(f, "{}", sym),
            EnumerationLiteral::Character(byte) => write!(f, "'{}'", *byte as char),
        }
    }
}

impl Display for TypeDefinition {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            TypeDefinition::Enumeration(ref enum_literals) => {
                write!(f, " is (")?;
                let mut first = true;
                for literal in enum_literals {
                    if first {
                        write!(f, "{}", literal)?;
                    } else {
                        write!(f, ", {}", literal)?;
                    }
                    first = false;
                }
                write!(f, ");")
            }
            TypeDefinition::Integer(ref constraint) => {
                write!(f, " is range {};", constraint)
            }
            TypeDefinition::Physical(ref physical) => {
                writeln!(
                    f,
                    " is range {} units\n  {};",
                    physical.range, physical.primary_unit
                )?;
                for (ident, literal) in &physical.secondary_units {
                    writeln!(f, "  {} = {};", ident, literal)?;
                }
                write!(f, "end units;")
            }
            TypeDefinition::Array(ref indexes, ref subtype_indication) => {
                write!(f, " is array (")?;
                let mut first = true;
                for index in indexes {
                    if first {
                        write!(f, "{}", index)?;
                    } else {
                        write!(f, ", {}", index)?;
                    }
                    first = false;
                }
                write!(f, ") of {};", subtype_indication)
            }
            TypeDefinition::Record(ref elements) => {
                writeln!(f, " is record")?;
                for element in elements {
                    writeln!(f, "  {};", element)?;
                }
                write!(f, "end record;")
            }
            TypeDefinition::Access(ref subtype_indication) => {
                write!(f, " is access {};", subtype_indication)
            }
            TypeDefinition::Incomplete(..) => {
                write!(f, ";")
            }
            TypeDefinition::File(ref type_mark) => {
                write!(f, " is file of {};", type_mark)
            }
            TypeDefinition::Protected(..) => {
                // Not used: items
                write!(f, " is protected")
            }
            TypeDefinition::ProtectedBody(..) => {
                // Not used: type_reference, decl
                write!(f, " is protected body")
            }
            TypeDefinition::Subtype(ref subtype_indication) => {
                write!(f, " is {};", subtype_indication)
            }
        }
    }
}

impl Display for TypeDeclaration {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self.def {
            TypeDefinition::Subtype(..) => write!(f, "subtype")?,
            _ => write!(f, "type")?,
        }
        write!(f, " {}{}", self.ident, self.def)
    }
}

impl Display for ObjectClass {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            ObjectClass::Signal => write!(f, "signal"),
            ObjectClass::Constant => write!(f, "constant"),
            ObjectClass::Variable => write!(f, "variable"),
            ObjectClass::SharedVariable => write!(f, "shared variable"),
        }
    }
}

impl Display for ObjectDeclaration {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "{} {} : {}",
            self.class, self.ident, self.subtype_indication,
        )?;
        match self.expression {
            Some(ref expr) => write!(f, " := {};", expr),
            None => write!(f, ";"),
        }
    }
}

impl Display for FileDeclaration {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "file {} : {}", self.ident, self.subtype_indication)?;
        if let Some(ref expr) = self.open_info {
            write!(f, " open {}", expr)?;
        }
        match self.file_name {
            Some(ref expr) => write!(f, " is {};", expr),
            None => write!(f, ";"),
        }
    }
}

impl Display for SubprogramDesignator {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            SubprogramDesignator::Identifier(ref sym) => write!(f, "{}", sym),
            SubprogramDesignator::OperatorSymbol(ref latin1) => write!(f, "\"{}\"", latin1),
        }
    }
}

impl Display for ProcedureSpecification {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "procedure {}", self.designator)?;
        let mut first = true;
        for param in &self.parameter_list {
            if first {
                write!(f, "(\n  {}", param)?;
            } else {
                write!(f, ";\n  {}", param)?;
            }
            first = false;
        }
        if !first {
            write!(f, "\n)")
        } else {
            Ok(())
        }
    }
}

impl Display for FunctionSpecification {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        if !self.pure {
            write!(f, "impure ")?;
        }
        write!(f, "function {}", self.designator)?;
        let mut first = true;
        for param in &self.parameter_list {
            if first {
                write!(f, "(\n  {}", param)?;
            } else {
                write!(f, ";\n  {}", param)?;
            }
            first = false;
        }
        if !first {
            write!(f, "\n)")?;
        }
        write!(f, " return {}", self.return_type)
    }
}

impl Display for Signature {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Signature::Function(ref args, ref ret) => {
                write!(f, "[")?;
                let mut first = true;
                for arg in args {
                    if first {
                        write!(f, "{}", arg)?;
                    } else {
                        write!(f, ", {}", arg)?;
                    }
                    first = false;
                }
                if first {
                    write!(f, "return {}]", ret)
                } else {
                    write!(f, " return {}]", ret)
                }
            }
            Signature::Procedure(ref args) => {
                write!(f, "[")?;
                let mut first = true;
                for arg in args {
                    if first {
                        write!(f, "{}", arg)?;
                    } else {
                        write!(f, ", {}", arg)?;
                    }
                    first = false;
                }
                write!(f, "]")
            }
        }
    }
}

impl Display for SubprogramDeclaration {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            SubprogramDeclaration::Procedure(ref procedure) => write!(f, "{}", procedure),
            SubprogramDeclaration::Function(ref function) => write!(f, "{}", function),
        }
    }
}

impl Display for InterfaceFileDeclaration {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "file {} : {}", self.ident, self.subtype_indication)
    }
}

impl Display for InterfaceObjectDeclaration {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self.list_type {
            InterfaceListType::Port => {
                write!(
                    f,
                    "{} : {} {}",
                    self.ident, self.mode, self.subtype_indication
                )?;
            }
            InterfaceListType::Generic => {
                write!(f, "{} : {}", self.ident, self.subtype_indication)?;
            }
            InterfaceListType::Parameter => {
                write!(
                    f,
                    "{} {} : {} {}",
                    self.class, self.ident, self.mode, self.subtype_indication,
                )?;
            }
        }
        match self.expression {
            Some(ref expr) => write!(f, " := {}", expr),
            None => Ok(()),
        }
    }
}

impl Display for SubprogramDefault {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            SubprogramDefault::Name(ref name) => write!(f, "{}", name),
            SubprogramDefault::Box => write!(f, "<>"),
        }
    }
}

impl Display for InterfacePackageDeclaration {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "package {} is new {}\n  generic map (",
            self.ident, self.package_name
        )?;
        match &self.generic_map {
            InterfacePackageGenericMapAspect::Map(assoc_list) => {
                let mut first = true;
                for assoc in assoc_list {
                    if first {
                        write!(f, "\n    {}", assoc)?;
                    } else {
                        write!(f, ",\n    {}", assoc)?;
                    }
                    first = false;
                }
                write!(f, "\n  )")
            }
            InterfacePackageGenericMapAspect::Box => write!(f, "<>)"),
            InterfacePackageGenericMapAspect::Default => write!(f, "default)"),
        }
    }
}

impl Display for InterfaceDeclaration {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            InterfaceDeclaration::Object(ref decl) => write!(f, "{}", decl),
            InterfaceDeclaration::File(ref decl) => write!(f, "{}", decl),
            InterfaceDeclaration::Type(ref ident) => write!(f, "type {}", ident),
            InterfaceDeclaration::Subprogram(ref decl, ref default) => {
                write!(f, "{}", decl)?;
                match default {
                    Some(ref default) => write!(f, " is {}", default),
                    None => Ok(()),
                }
            }
            InterfaceDeclaration::Package(ref decl) => write!(f, "{}", decl),
        }
    }
}

impl Display for Mode {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Mode::In => write!(f, "in"),
            Mode::Out => write!(f, "out"),
            Mode::InOut => write!(f, "inout"),
            Mode::Buffer => write!(f, "buffer"),
            Mode::Linkage => write!(f, "linkage"),
        }
    }
}

impl Display for ComponentDeclaration {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "component {}", self.ident)?;

        let mut first = true;
        for generic in &self.generic_list {
            if first {
                write!(f, "\n  generic (\n    {}", generic)?;
            } else {
                write!(f, ";\n    {}", generic)?;
            }
            first = false;
        }
        if !first {
            write!(f, "\n  );")?;
        }

        let mut first = true;
        for port in &self.port_list {
            if first {
                write!(f, "\n  port (\n    {}", port)?;
            } else {
                write!(f, ";\n    {}", port)?;
            }
            first = false;
        }
        if !first {
            write!(f, "\n  );")?;
        }

        write!(f, "\nend component;")
    }
}

impl Display for ForGenerateStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        // Not used: body
        write!(
            f,
            "for {} in {} generate",
            self.index_name, self.discrete_range,
        )
    }
}

impl Display for ContextDeclaration {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        // Not used: items
        write!(f, "context {}", self.ident)
    }
}

impl Display for PackageInstantiation {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        // Not used: context_clause
        write!(f, "package {} is new {}", self.ident, self.package_name)?;
        if let Some(assoc_list) = &self.generic_map {
            let mut first = true;
            for assoc in assoc_list {
                if first {
                    write!(f, "\n  generic map (\n    {}", assoc)?;
                } else {
                    write!(f, ",\n    {}", assoc)?;
                }
                first = false;
            }
            if !first {
                write!(f, "\n  )")?;
            }
        }
        write!(f, ";")
    }
}

impl Display for ConfigurationDeclaration {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        // Not used: context_clause, decl, block_config, vunit_bind_inds
        write!(f, "configuration {} of {}", self.ident, self.entity_name)
    }
}

impl Display for EntityDeclaration {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        // Not used: context_clause, decl, statements
        write!(f, "entity {} is", self.ident)?;

        if let Some(generic_clause) = &self.generic_clause {
            let mut first = true;
            for generic in &generic_clause.item {
                if first {
                    write!(f, "\n  generic (\n    {}", generic)?;
                } else {
                    write!(f, ";\n    {}", generic)?;
                }
                first = false;
            }
            if !first {
                write!(f, "\n  );")?;
            }
        }

        if let Some(port_clause) = &self.port_clause {
            let mut first = true;
            for port in &port_clause.item {
                if first {
                    write!(f, "\n  port (\n    {}", port)?;
                } else {
                    write!(f, ";\n    {}", port)?;
                }
                first = false;
            }
            if !first {
                write!(f, "\n  );")?;
            }
        }

        write!(f, "\nend entity;")
    }
}

impl Display for PackageDeclaration {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        // Not used: context_clause, decl
        if let Some(generic_clause) = &self.generic_clause {
            write!(f, "package {} is", self.ident)?;

            let mut first = true;
            for generic in &generic_clause.item {
                if first {
                    write!(f, "\n  generic (\n    {}", generic)?;
                } else {
                    write!(f, ";\n    {}", generic)?;
                }
                first = false;
            }
            if !first {
                write!(f, "\n  );")
            } else {
                Ok(())
            }
        } else {
            write!(f, "package {}", self.ident)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::test::Code;
    use assert_matches::assert_matches;

    pub fn assert_format_eq<F, R: Display>(code: &str, res: &str, code_fun: F)
    where
        F: FnOnce(&Code) -> R,
    {
        assert_eq!(format!("{}", code_fun(&Code::new(code))), res);
    }

    pub fn assert_format<F, R: Display>(code: &str, code_fun: F)
    where
        F: FnOnce(&Code) -> R,
    {
        assert_format_eq(code, code, code_fun);
    }

    #[test]
    fn test_selected_name_single() {
        assert_format("foo", Code::selected_name);
    }

    #[test]
    fn test_selected_name_multiple() {
        assert_format("foo.bar.baz", Code::selected_name);
    }

    #[test]
    fn test_name_operator_symbol() {
        assert_format("\"+\"", Code::name);
    }

    #[test]
    fn test_name_character() {
        assert_format("'a'", Code::name);
    }

    #[test]
    fn test_name_selected() {
        assert_format("foo.bar.baz", Code::name);
    }

    #[test]
    fn test_name_selected_all() {
        assert_format("foo.all", Code::name);
    }

    #[test]
    fn test_name_indexed_single() {
        assert_format("foo(0)", Code::name);
    }

    #[test]
    fn test_name_indexed_multi() {
        assert_format("foo(0, 1)", Code::name);
    }

    #[test]
    fn test_name_slice() {
        assert_format("foo(0 to 1)", Code::name);
    }

    #[test]
    fn test_name_attribute() {
        assert_format("prefix'foo", Code::name);
    }

    #[test]
    fn test_name_attribute_expression() {
        assert_format("prefix'foo(expr + 1)", Code::name);
    }

    #[test]
    fn test_name_attribute_signature() {
        assert_format("prefix[return natural]'foo", Code::name);
    }

    #[test]
    fn test_name_attribute_signature_expression() {
        assert_format("prefix[return natural]'foo(expr + 1)", Code::name);
    }

    #[test]
    fn test_name_function_call_no_args() {
        assert_format("foo", |code| {
            Name::FunctionCall(Box::new(code.function_call()))
        });
    }

    #[test]
    fn test_name_function_call_no_formal() {
        assert_format("foo(0)", Code::name);
    }

    #[test]
    fn test_name_function_call_chained() {
        assert_format("prefix(0, 1)(3)", Code::name);
    }

    #[test]
    fn test_name_function_call_formal() {
        assert_format("foo(arg => 0)", Code::name);
    }

    #[test]
    fn test_name_function_call_actual_part_open() {
        assert_format("foo(open, arg => open)", Code::name);
    }

    #[test]
    fn test_name_external_implicit_relative() {
        assert_format("<< signal dut.foo : std_logic >>", Code::name);
    }

    #[test]
    fn test_name_external_explicit_relative() {
        assert_format("<< signal ^.dut.gen(0) : std_logic >>", Code::name);
    }

    #[test]
    fn test_name_external_explicit_relative_multiple_levels() {
        assert_format("<< signal ^.^.^.dut.gen(0) : std_logic >>", Code::name);
    }

    #[test]
    fn test_name_external_absolute() {
        assert_format("<< signal .dut.gen(0) : std_logic >>", Code::name);
    }

    #[test]
    fn test_name_external_package() {
        assert_format("<< signal @lib.pkg : std_logic >>", Code::name);
    }

    #[test]
    fn test_name_external_object_classes() {
        assert_format("<< constant dut.foo : std_logic >>", Code::name);
        assert_format("<< signal dut.foo : std_logic >>", Code::name);
        assert_format("<< variable dut.foo : std_logic >>", Code::name);
    }

    #[test]
    fn test_expression_binary() {
        assert_format("1 ** 2", Code::expr);
        assert_format("1 * 2", Code::expr);
        assert_format("1 / 2", Code::expr);
        assert_format("1 mod 2", Code::expr);
        assert_format("1 rem 2", Code::expr);
        assert_format("1 + 2", Code::expr);
        assert_format("1 - 2", Code::expr);
        assert_format("1 & 2", Code::expr);
        assert_format("1 sll 2", Code::expr);
        assert_format("1 srl 2", Code::expr);
        assert_format("1 sla 2", Code::expr);
        assert_format("1 sra 2", Code::expr);
        assert_format("1 rol 2", Code::expr);
        assert_format("1 ror 2", Code::expr);
        assert_format("1 = 2", Code::expr);
        assert_format("1 /= 2", Code::expr);
        assert_format("1 < 2", Code::expr);
        assert_format("1 <= 2", Code::expr);
        assert_format("1 > 2", Code::expr);
        assert_format("1 >= 2", Code::expr);
        assert_format("1 ?= 2", Code::expr);
        assert_format("1 ?/= 2", Code::expr);
        assert_format("1 ?< 2", Code::expr);
        assert_format("1 ?<= 2", Code::expr);
        assert_format("1 ?> 2", Code::expr);
        assert_format("1 ?>= 2", Code::expr);
        assert_format("1 and 2", Code::expr);
        assert_format("1 or 2", Code::expr);
        assert_format("1 nand 2", Code::expr);
        assert_format("1 nor 2", Code::expr);
        assert_format("1 xor 2", Code::expr);
        assert_format("1 xnor 2", Code::expr);
    }

    #[test]
    fn test_expression_unary() {
        assert_format("?? 1", Code::expr);
        assert_format("+1", Code::expr);
        assert_format("-1", Code::expr);
        assert_format("not 1", Code::expr);
        assert_format("abs 1", Code::expr);
        assert_format("and 1", Code::expr);
        assert_format("or 1", Code::expr);
        assert_format("nand 1", Code::expr);
        assert_format("nor 1", Code::expr);
        assert_format("xor 1", Code::expr);
        assert_format("xnor 1", Code::expr);
    }

    #[test]
    fn test_expression_precedence() {
        assert_format("1 * 2 + 3 * 4", Code::expr);
        assert_format("(1 + 2) * (3 + 4)", Code::expr);
        assert_format("1 + 2 + (3 + 4)", Code::expr);
        assert_format("-1 + -2", Code::expr);
        assert_format("-(1 + 2)", Code::expr);
        // Multiplication has precedence over negation
        assert_format("(-1) * -2", Code::expr);
        assert_format("-(1 * -2)", Code::expr);
        assert_format("-(-1)", Code::expr);
        assert_format("-(+1)", Code::expr);
        assert_format("-not 1", Code::expr);
        assert_format("not -1", Code::expr);
        assert_format("not (not 1)", Code::expr);
        assert_format("1 - -1", Code::expr);
    }

    #[test]
    fn test_expression_aggregate_positional() {
        assert_format("(1, 2)", Code::expr);
    }

    #[test]
    fn test_expression_aggregate_named_expression() {
        assert_format("(1 => 2)", Code::expr);
    }

    #[test]
    fn test_expression_aggregate_named_many_choices() {
        assert_format("(1 | 2 => 3)", Code::expr);
    }

    #[test]
    fn test_expression_aggregate_many_named_others() {
        assert_format("(1 | 2 => 3, others => 4)", Code::expr);
    }

    #[test]
    fn test_expression_aggregate_named_range() {
        assert_format("(0 to 1 => 2)", Code::expr);
        assert_format("(1 downto 0 => 2)", Code::expr);
    }

    #[test]
    fn test_expression_qualified() {
        assert_format("foo'(1 + 2)", Code::expr);
    }

    #[test]
    fn test_expression_name() {
        assert_format("foo.bar.baz", Code::expr);
    }

    #[test]
    fn test_expression_literal_string() {
        assert_format("\"string\"", Code::expr);
    }

    #[test]
    fn test_expression_literal_bit_string() {
        assert_format("b\"0110\"", Code::expr);
        assert_format("o\"1377\"", Code::expr);
        assert_format("x\"Aa5F\"", Code::expr);
        assert_format("ub\"0110\"", Code::expr);
        assert_format("uo\"1377\"", Code::expr);
        assert_format("ux\"Aa5F\"", Code::expr);
        assert_format("sb\"0110\"", Code::expr);
        assert_format("so\"1377\"", Code::expr);
        assert_format("sx\"Aa5F\"", Code::expr);
        assert_format("d\"1234\"", Code::expr);
    }

    #[test]
    fn test_expression_literal_bit_string_with_length() {
        assert_format("3b\"0110\"", Code::expr);
        assert_format("10o\"1377\"", Code::expr);
        assert_format("15x\"5FaA\"", Code::expr);
        assert_format("3ub\"0110\"", Code::expr);
        assert_format("10uo\"1377\"", Code::expr);
        assert_format("15ux\"5FaA\"", Code::expr);
        assert_format("3sb\"0110\"", Code::expr);
        assert_format("10so\"1377\"", Code::expr);
        assert_format("15sx\"5FaA\"", Code::expr);
        assert_format("12d\"1234\"", Code::expr);
    }

    #[test]
    fn test_expression_literal_character() {
        assert_format("'a'", Code::expr);
    }

    #[test]
    fn test_expression_literal_integer() {
        assert_format("123", Code::expr);
    }

    #[test]
    fn test_expression_literal_real() {
        assert_format("12.3", Code::expr);
    }

    #[test]
    fn test_expression_literal_physical_integer() {
        assert_format("1 ns", Code::expr);
    }

    #[test]
    fn test_expression_literal_physical_real() {
        assert_format("1.1 ns", Code::expr);
    }

    #[test]
    fn parses_null_literal() {
        assert_format("null", Code::expr);
    }

    #[test]
    fn test_expression_new_allocator_qualified() {
        assert_format("new integer_vector'(0, 1)", Code::expr);
    }

    #[test]
    fn test_expression_new_allocator_subtype() {
        assert_format("new integer_vector", Code::expr);
    }

    #[test]
    fn test_expression_new_allocator_subtype_constraint() {
        assert_format("new integer_vector(0 to 1)", Code::expr);
    }

    #[test]
    fn test_expression_new_allocator_subtype_constraint_range_attribute() {
        assert_format("new integer_vector(foo'range)", Code::expr);
    }

    #[test]
    fn test_discrete_range() {
        assert_format("foo.bar", Code::discrete_range);
    }

    #[test]
    fn test_discrete_range_range() {
        assert_format("foo.bar range 1 to 4", Code::discrete_range);
    }

    #[test]
    fn test_discrete_range_range_attribute() {
        assert_format("foo.bar'range", Code::discrete_range);
    }

    #[test]
    fn test_subtype_indication_without_constraint() {
        assert_format("std_logic", Code::subtype_indication);
    }

    #[test]
    fn test_subtype_indication_with_resolution_function() {
        assert_format("resolve std_logic", Code::subtype_indication);
    }

    #[test]
    fn test_subtype_indication_with_array_element_resolution_function() {
        assert_format("(resolve) integer_vector", Code::subtype_indication);
    }

    #[test]
    fn test_subtype_indication_with_record_element_resolution_function() {
        assert_format("(elem resolve) rec_t", Code::subtype_indication);
    }

    #[test]
    fn test_subtype_indication_with_record_element_resolution_function_many() {
        assert_format(
            "(elem1 (resolve1), elem2 resolve2, elem3 (sub_elem sub_resolve)) rec_t",
            Code::subtype_indication,
        );
    }

    #[test]
    fn test_subtype_indication_with_resolution_function_selected_name() {
        assert_format("lib.foo.resolve std_logic", Code::subtype_indication);
    }

    #[test]
    fn test_subtype_indication_with_range() {
        assert_format("integer range 0 to 2 - 1", Code::subtype_indication);
    }

    #[test]
    fn test_subtype_indication_with_range_attribute() {
        assert_format("integer range lib.foo.bar'range", Code::subtype_indication);
    }

    #[test]
    fn test_subtype_indication_with_array_constraint_range() {
        assert_format("integer_vector(2 - 1 downto 0)", Code::subtype_indication);
    }

    #[test]
    fn test_subtype_indication_with_array_constraint_discrete() {
        assert_format("integer_vector(lib.foo.bar)", Code::subtype_indication);
    }

    #[test]
    fn test_subtype_indication_with_array_constraint_attribute() {
        assert_format(
            "integer_vector(lib.pkg.bar'range)",
            Code::subtype_indication,
        );
    }

    #[test]
    fn test_subtype_indication_with_array_constraint_open() {
        assert_format("integer_vector(open)", Code::subtype_indication);
    }

    #[test]
    fn test_subtype_indication_with_multi_dim_array_constraints() {
        assert_format(
            "integer_vector(2 - 1 downto 0, 11 to 14)",
            Code::subtype_indication,
        );
    }

    #[test]
    fn test_subtype_indication_with_array_element_constraint() {
        assert_format(
            "integer_vector(2 - 1 downto 0, 11 to 14)(foo to bar)",
            Code::subtype_indication,
        );
    }

    #[test]
    fn test_subtype_indication_with_record_constraint() {
        assert_format(
            "axi_m2s_t(tdata(2 - 1 downto 0), tuser(3 to 5))",
            Code::subtype_indication,
        );
    }

    #[test]
    fn test_type_declaration_integer() {
        assert_format("type foo is range 0 to 1;", Code::type_decl);
    }

    #[test]
    fn test_type_declaration_enumeration() {
        assert_format("type foo is (alpha, beta);", Code::type_decl);
    }

    #[test]
    fn test_type_declaration_enumeration_character() {
        assert_format("type foo is ('a', 'b');", Code::type_decl);
    }

    #[test]
    fn test_type_declaration_enumeration_mixed() {
        assert_format("type foo is (ident, 'b');", Code::type_decl);
    }

    #[test]
    fn test_type_declaration_array_with_index_subtype() {
        assert_format(
            "type foo is array (natural range <>) of boolean;",
            Code::type_decl,
        );
    }

    #[test]
    fn test_type_declaration_array_with_discrete_subtype() {
        assert_format("type foo is array (natural) of boolean;", Code::type_decl);
    }

    #[test]
    fn test_type_declaration_array_with_selected_name() {
        assert_format(
            "type foo is array (lib.pkg.foo) of boolean;",
            Code::type_decl,
        );
    }

    #[test]
    fn test_type_declaration_array_with_range_attribute() {
        assert_format(
            "type foo is array (arr_t'range) of boolean;",
            Code::type_decl,
        );
    }

    #[test]
    fn test_type_declaration_array_with_constraint() {
        assert_format(
            "type foo is array (2 - 1 downto 0) of boolean;",
            Code::type_decl,
        );
    }

    #[test]
    fn test_type_declaration_array_mixed() {
        assert_format(
            "type foo is array (2 - 1 downto 0, integer range <>) of boolean;",
            Code::type_decl,
        );
    }

    #[test]
    fn test_type_declaration_record() {
        assert_format(
            "type foo is record
  element : boolean;
end record;",
            Code::type_decl,
        );
    }

    #[test]
    fn test_type_declaration_record_many() {
        assert_format(
            "type foo is record
  element : boolean;
  field : boolean;
  other_element : std_logic_vector(0 to 1);
end record;",
            Code::type_decl,
        );
    }

    #[test]
    fn test_type_declaration_subtype() {
        assert_format(
            "subtype vec_t is integer_vector(2 - 1 downto 0);",
            Code::type_decl,
        );
    }

    #[test]
    fn test_type_declaration_access() {
        assert_format(
            "type ptr_t is access integer_vector(2 - 1 downto 0);",
            Code::type_decl,
        );
    }

    #[test]
    fn test_type_declaration_incomplete() {
        assert_format("type incomplete;", Code::type_decl);
    }

    #[test]
    fn test_type_declaration_file() {
        assert_format("type foo is file of character;", Code::type_decl);
    }

    #[test]
    fn test_type_declaration_protected() {
        assert_format_eq(
            "type foo is protected
end protected;",
            "type foo is protected",
            Code::type_decl,
        );
    }

    #[test]
    fn test_type_declaration_protected_with_subprograms() {
        assert_format_eq(
            "type foo is protected
  procedure proc;
  function fun return ret;
end protected;",
            "type foo is protected",
            Code::type_decl,
        );
    }

    #[test]
    fn test_type_declaration_protected_body() {
        assert_format_eq(
            "type foo is protected body
  variable foo : natural;
  procedure proc is
  begin
  end;
end protected body;",
            "type foo is protected body",
            Code::type_decl,
        );
    }

    #[test]
    fn test_type_declaration_physical() {
        assert_format(
            "type phys is range 0 to 15 units
  primary_unit;
end units;",
            Code::type_decl,
        );
    }

    #[test]
    fn test_type_declaration_physical_secondary_units() {
        assert_format(
            "type phys is range 0 to 15 units
  primary_unit;
  secondary_unit = 5 primary_unit;
end units;",
            Code::type_decl,
        );
    }

    #[test]
    fn test_object_declaration_constant() {
        assert_format("constant foo : natural;", Code::object_decl);
    }

    #[test]
    fn test_object_declaration_signal() {
        assert_format("signal foo : natural;", Code::object_decl);
    }

    #[test]
    fn test_object_declaration_variable() {
        assert_format("variable foo : natural;", Code::object_decl);
    }

    #[test]
    fn test_object_declaration_shared_variable() {
        assert_format("shared variable foo : natural;", Code::object_decl);
    }

    #[test]
    fn test_object_declaration_optional_expression() {
        assert_format("constant foo : natural := 0;", Code::object_decl);
    }

    #[test]
    fn test_file_declaration() {
        assert_format("file foo : text;", Code::file_decl);
    }

    #[test]
    fn test_file_declaration_with_file_name() {
        assert_format("file foo : text is \"file_name\";", Code::file_decl);
    }

    #[test]
    fn test_file_declaration_with_open_information() {
        assert_format(
            "file foo : text open write_mode is \"file_name\";",
            Code::file_decl,
        );
    }

    #[test]
    fn test_alias_declaration() {
        assert_format("alias foo is name;", Code::alias_decl);
    }

    #[test]
    fn test_alias_declaration_with_subtype_indication() {
        assert_format("alias foo : vector(0 to 1) is name;", Code::alias_decl);
    }

    #[test]
    fn test_alias_declaration_with_signature() {
        assert_format("alias foo is name[return natural];", Code::alias_decl);
    }

    #[test]
    fn test_alias_declaration_with_operator_symbol() {
        assert_format("alias \"and\" is name;", Code::alias_decl);
    }

    #[test]
    fn test_alias_declaration_with_character() {
        assert_format("alias 'c' is 'b';", Code::alias_decl);
    }

    #[test]
    pub fn test_procedure_specification() {
        assert_format("procedure foo", Code::subprogram_decl);
    }

    #[test]
    pub fn test_function_specification() {
        assert_format("function foo return lib.foo.natural", Code::subprogram_decl);
    }

    #[test]
    pub fn test_function_specification_operator() {
        assert_format(
            "function \"+\" return lib.foo.natural",
            Code::subprogram_decl,
        );
    }

    #[test]
    pub fn test_function_specification_impure() {
        assert_format(
            "impure function foo return lib.foo.natural",
            Code::subprogram_decl,
        );
    }

    #[test]
    pub fn test_procedure_specification_with_parameters() {
        assert_format(
            "procedure foo(
  constant foo : in natural
)",
            Code::subprogram_decl,
        );
    }

    #[test]
    pub fn test_function_specification_with_parameters() {
        assert_format(
            "function foo(
  constant foo : in natural
) return lib.foo.natural",
            Code::subprogram_decl,
        );
    }

    #[test]
    pub fn test_interface_declaration_object() {
        assert_format("signal foo : in std_logic", Code::parameter);
        assert_format("signal foo : out std_logic", Code::parameter);
        assert_format("signal foo : inout std_logic", Code::parameter);
        assert_format("signal foo : buffer std_logic", Code::parameter);
        assert_format("signal foo : linkage std_logic", Code::parameter);

        assert_format("constant foo : in std_logic", Code::parameter);

        assert_format("variable foo : in std_logic", Code::parameter);
        assert_format("variable foo : out std_logic", Code::parameter);
        assert_format("variable foo : inout std_logic", Code::parameter);
        assert_format("variable foo : buffer std_logic", Code::parameter);
        assert_format("variable foo : linkage std_logic", Code::parameter);
    }

    #[test]
    fn test_interface_declaration_object_with_expression() {
        assert_format("constant foo : in natural := bar(0)", Code::parameter);
    }

    #[test]
    fn test_interface_declaration_object_generic() {
        assert_format("foo : natural := bar(0)", Code::generic);
    }

    #[test]
    fn test_interface_declaration_object_port() {
        assert_format("foo : in natural := bar(0)", Code::port);
    }

    #[test]
    fn test_interface_declaration_file() {
        assert_format("file foo : text", Code::parameter);
    }

    #[test]
    fn test_interface_declaration_type() {
        assert_format("type name", Code::parameter);
    }

    #[test]
    fn test_interface_declaration_subprogram() {
        assert_format("function foo return bar", Code::parameter);
        assert_format("procedure foo", Code::parameter);
        assert_format("impure function foo return bar", Code::parameter);
    }

    #[test]
    fn test_interface_declaration_subprogram_default() {
        assert_format("function foo return bar is lib.name", Code::parameter);
        assert_format("function foo return bar is <>", Code::parameter);
    }

    #[test]
    fn test_interface_declaration_package_map() {
        assert_format(
            "package foo is new lib.pkg
  generic map (
    foo => bar
  )",
            Code::parameter,
        );
    }

    #[test]
    fn test_interface_declaration_package_box() {
        assert_format(
            "package foo is new lib.pkg
  generic map (<>)",
            Code::parameter,
        );
    }

    #[test]
    fn test_interface_declaration_package_default() {
        assert_format(
            "package foo is new lib.pkg
  generic map (default)",
            Code::parameter,
        );
    }

    #[test]
    pub fn test_signature_function_only_return() {
        assert_format("[return bar.type_mark]", Code::signature);
    }

    #[test]
    pub fn test_signature_function_one_argument() {
        assert_format("[foo.type_mark return bar.type_mark]", Code::signature);
    }

    #[test]
    pub fn test_signature_function_many_arguments() {
        assert_format(
            "[foo.type_mark, foo2.type_mark return bar.type_mark]",
            Code::signature,
        );
    }

    #[test]
    pub fn test_signature_procedure() {
        assert_format("[foo.type_mark]", Code::signature);
    }

    #[test]
    fn test_component_declaration() {
        assert_format(
            "component foo
end component;",
            Code::component_decl,
        );
    }

    #[test]
    fn test_component_declaration_with_generic() {
        assert_format(
            "component foo
  generic (
    foo : natural
  );
end component;",
            Code::component_decl,
        );
    }

    #[test]
    fn test_component_declaration_with_port() {
        assert_format(
            "component foo
  port (
    foo : inout natural
  );
end component;",
            Code::component_decl,
        );
    }

    #[test]
    fn test_component_declaration_with_multiple() {
        assert_format(
            "component foo
  generic (
    foo : natural;
    bar : natural
  );
  port (
    baz : in natural;
    qux : out std_logic
  );
end component;",
            Code::component_decl,
        );
    }

    #[test]
    fn test_entity_declaration() {
        assert_format(
            "entity foo is
end entity;",
            Code::entity_decl,
        );
    }

    #[test]
    fn test_entity_declaration_with_generic() {
        assert_format(
            "entity foo is
  generic (
    foo : natural
  );
end entity;",
            Code::entity_decl,
        );
    }

    #[test]
    fn test_entity_declaration_with_port() {
        assert_format(
            "entity foo is
  port (
    foo : inout natural
  );
end entity;",
            Code::entity_decl,
        );
    }

    #[test]
    fn test_entity_declaration_with_multiple() {
        assert_format(
            "entity foo is
  generic (
    foo : natural;
    bar : natural
  );
  port (
    baz : in natural;
    qux : out std_logic
  );
end entity;",
            Code::entity_decl,
        );
    }

    #[test]
    fn test_for_generate_statement() {
        assert_format_eq(
            "for idx in 0 to 1 generate
end generate;",
            "for idx in 0 to 1 generate",
            |code| {
                assert_matches!(
                    code.concurrent_statement().statement,
                    ConcurrentStatement::ForGenerate(gen) => gen
                )
            },
        );
    }

    #[test]
    fn test_context_declaration() {
        assert_format_eq(
            "context ident is
end context;",
            "context ident",
            |code| {
                assert_matches!(
                    code.design_file().design_units.remove(0),
                    AnyDesignUnit::Primary(AnyPrimaryUnit::Context(context)) => context
                )
            },
        );
    }

    #[test]
    fn test_package_instantiation() {
        assert_format("package ident is new lib.foo.bar;", |code| {
            assert_matches!(
                code.design_file().design_units.remove(0),
                AnyDesignUnit::Primary(AnyPrimaryUnit::PackageInstance(instance)) => instance
            )
        });
    }

    #[test]
    fn test_package_instantiation_generic_map() {
        assert_format(
            "package ident is new lib.foo.bar
  generic map (
    foo => bar,
    baz => qux
  );",
            |code| {
                assert_matches!(
                    code.design_file().design_units.remove(0),
                    AnyDesignUnit::Primary(AnyPrimaryUnit::PackageInstance(instance)) => instance
                )
            },
        );
    }

    #[test]
    fn test_configuration_declaration() {
        assert_format_eq(
            "configuration cfg of entity_name is
  for rtl(0)
  end for;
end;",
            "configuration cfg of entity_name",
            |code| {
                assert_matches!(
                    code.design_file().design_units.remove(0),
                    AnyDesignUnit::Primary(AnyPrimaryUnit::Configuration(unit)) => unit
                )
            },
        );
    }

    #[test]
    fn test_package_declaration() {
        assert_format_eq(
            "package pkg_name is
end package;",
            "package pkg_name",
            |code| {
                assert_matches!(
                    code.design_file().design_units.remove(0),
                    AnyDesignUnit::Primary(AnyPrimaryUnit::Package(unit)) => unit
                )
            },
        );
    }

    #[test]
    fn test_package_declaration_with_generic() {
        assert_format_eq(
            "package pkg_name is
  generic (
    type foo;
    type bar
  );
end package;",
            "package pkg_name is
  generic (
    type foo;
    type bar
  );",
            |code| {
                assert_matches!(
                    code.design_file().design_units.remove(0),
                    AnyDesignUnit::Primary(AnyPrimaryUnit::Package(unit)) => unit
                )
            },
        );
    }
}
