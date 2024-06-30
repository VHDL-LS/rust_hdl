// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2024, Olof Kraigher olof.kraigher@gmail.com
use crate::ast::{AttributeDesignator, ObjectClass, RangeAttribute, TypeAttribute};
use crate::{named_entity, AnyEntKind, CompletionItem, EntRef, Object};

/// Produces completions for an attribute name, i.e.,
/// `foo'`
/// The provided ent is the entity directly before the tick, i.e.,
/// `foo` in the example above.
pub(crate) fn completions_for_attribute_name(ent: EntRef) -> Vec<CompletionItem> {
    let mut attributes: Vec<AttributeDesignator> = Vec::new();
    attributes.extend([
        AttributeDesignator::SimpleName,
        AttributeDesignator::InstanceName,
        AttributeDesignator::PathName,
    ]);

    match ent.kind() {
        AnyEntKind::Type(typ) => extend_attributes_of_type(typ, &mut attributes),
        AnyEntKind::Object(obj) => extend_attributes_of_objects(obj, &mut attributes),
        AnyEntKind::View(_) => attributes.push(AttributeDesignator::Converse),
        _ => {}
    }
    attributes
        .into_iter()
        .map(CompletionItem::Attribute)
        .chain(
            ent.attrs
                .values()
                .map(|(_, b)| b)
                .map(|ent| CompletionItem::Simple(ent.ent)),
        )
        .collect()
}

/// Extends applicable attributes when the attribute name is a type.
fn extend_attributes_of_type(typ: &named_entity::Type, attributes: &mut Vec<AttributeDesignator>) {
    use AttributeDesignator::*;
    if typ.is_scalar() {
        attributes.extend([Left, Right, Low, High, Ascending, Image, Value]);
    } else if typ.is_array() {
        attributes.extend([
            Left,
            Right,
            Low,
            High,
            Range(RangeAttribute::Range),
            Range(RangeAttribute::ReverseRange),
            Length,
            Ascending,
        ]);
    }
    if typ.is_discrete() {
        attributes.extend([Pos, Val, Succ, Pred, LeftOf, RightOf]);
    }
}

/// Extends applicable attributes when the attribute name is an object.
fn extend_attributes_of_objects(obj: &Object, attributes: &mut Vec<AttributeDesignator>) {
    extend_attributes_of_type(obj.subtype.type_mark().kind(), attributes);
    attributes.push(AttributeDesignator::Type(TypeAttribute::Subtype));
    if obj.class == ObjectClass::Signal {
        use crate::ast::SignalAttribute::*;
        attributes.extend(
            [
                Delayed,
                Stable,
                Quiet,
                Transaction,
                Event,
                Active,
                LastEvent,
                LastActive,
                LastValue,
                Driving,
                DrivingValue,
            ]
            .map(AttributeDesignator::Signal),
        );
    }
    if obj.subtype.type_mark().kind().is_array() {
        attributes.push(AttributeDesignator::Type(TypeAttribute::Element));
    }
}

#[cfg(test)]
mod tests {
    use crate::analysis::tests::LibraryBuilder;
    use crate::ast::RangeAttribute;
    use crate::list_completion_options;
    use crate::syntax::test::assert_eq_unordered;
    use crate::CompletionItem;

    #[test]
    pub fn completes_attributes() {
        use crate::ast::AttributeDesignator::*;
        use crate::ast::TypeAttribute::*;

        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libA",
            "\
package my_pkg is
    constant foo : BIT_VECTOR := \"001\";
    constant bar: NATURAL := foo'
end package;
",
        );

        let (root, _) = builder.get_analyzed_root();
        let cursor = code.s1("foo'").end();
        let options = list_completion_options(&root, code.source(), cursor);

        let expected_options = [
            Type(Element),
            Type(Subtype),
            Range(RangeAttribute::Range),
            Range(RangeAttribute::ReverseRange),
            Ascending,
            Left,
            Right,
            High,
            Low,
            Length,
            InstanceName,
            SimpleName,
            PathName,
        ]
        .map(CompletionItem::Attribute);

        assert_eq_unordered(&options, &expected_options);
    }

    #[test]
    pub fn completes_signal_attributes() {
        use crate::ast::AttributeDesignator::*;
        use crate::ast::SignalAttribute::*;
        use crate::ast::TypeAttribute::*;

        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libA",
            "\
package my_pkg is
    signal foo : BIT_VECTOR := \"001\";
    signal bar: NATURAL := foo'
end package;
",
        );

        let (root, _) = builder.get_analyzed_root();
        let cursor = code.s1("foo'").end();
        let options = list_completion_options(&root, code.source(), cursor);

        let expected_options = [
            Type(Element),
            Type(Subtype),
            Range(RangeAttribute::Range),
            Range(RangeAttribute::ReverseRange),
            Signal(Delayed),
            Signal(Stable),
            Signal(Quiet),
            Signal(Transaction),
            Signal(Event),
            Signal(Active),
            Signal(LastEvent),
            Signal(LastActive),
            Signal(LastValue),
            Signal(Driving),
            Signal(DrivingValue),
            Ascending,
            Left,
            Right,
            High,
            Low,
            Length,
            InstanceName,
            SimpleName,
            PathName,
        ]
        .map(CompletionItem::Attribute);

        assert_eq_unordered(&options, &expected_options);
    }
}
