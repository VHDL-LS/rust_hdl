// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2026, Lukas Scheller lukasscheller@icloud.com
use super::*;
use crate::syntax::checked::CheckedNode;
use crate::syntax::node::SyntaxToken;
use crate::syntax::*;
#[derive(Debug, Clone)]
pub struct CheckedAttributeSpecification(AttributeSpecificationSyntax);
impl CheckedNode for CheckedAttributeSpecification {
    type Syntax = AttributeSpecificationSyntax;
    fn cast_unchecked(syntax: AttributeSpecificationSyntax) -> Self {
        CheckedAttributeSpecification(syntax)
    }
    fn raw(&self) -> AttributeSpecificationSyntax {
        self.0.clone()
    }
}
impl CheckedAttributeSpecification {
    pub fn attribute_token(&self) -> SyntaxToken {
        self.0.attribute_token().unwrap()
    }
    pub fn attribute_designator_token_token(&self) -> SyntaxToken {
        self.0.attribute_designator_token_token().unwrap()
    }
    pub fn of_token(&self) -> SyntaxToken {
        self.0.of_token().unwrap()
    }
    pub fn entity_specification(&self) -> CheckedEntitySpecification {
        CheckedEntitySpecification::cast_unchecked(self.0.entity_specification().unwrap())
    }
    pub fn is_token(&self) -> SyntaxToken {
        self.0.is_token().unwrap()
    }
    pub fn expression(&self) -> CheckedExpression {
        CheckedExpression::cast_unchecked(self.0.expression().unwrap())
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.0.semi_colon_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedBindingIndication(BindingIndicationSyntax);
impl CheckedNode for CheckedBindingIndication {
    type Syntax = BindingIndicationSyntax;
    fn cast_unchecked(syntax: BindingIndicationSyntax) -> Self {
        CheckedBindingIndication(syntax)
    }
    fn raw(&self) -> BindingIndicationSyntax {
        self.0.clone()
    }
}
impl CheckedBindingIndication {
    pub fn use_token(&self) -> Option<SyntaxToken> {
        self.0.use_token()
    }
    pub fn entity_aspect(&self) -> Option<CheckedEntityAspect> {
        self.0
            .entity_aspect()
            .map(CheckedEntityAspect::cast_unchecked)
    }
    pub fn generic_map_aspect(&self) -> Option<CheckedGenericMapAspect> {
        self.0
            .generic_map_aspect()
            .map(CheckedGenericMapAspect::cast_unchecked)
    }
    pub fn port_map_aspect(&self) -> Option<CheckedPortMapAspect> {
        self.0
            .port_map_aspect()
            .map(CheckedPortMapAspect::cast_unchecked)
    }
}
#[derive(Debug, Clone)]
pub struct CheckedComponentSpecification(ComponentSpecificationSyntax);
impl CheckedNode for CheckedComponentSpecification {
    type Syntax = ComponentSpecificationSyntax;
    fn cast_unchecked(syntax: ComponentSpecificationSyntax) -> Self {
        CheckedComponentSpecification(syntax)
    }
    fn raw(&self) -> ComponentSpecificationSyntax {
        self.0.clone()
    }
}
impl CheckedComponentSpecification {
    pub fn instantiation_list(&self) -> CheckedInstantiationList {
        CheckedInstantiationList::cast_unchecked(self.0.instantiation_list().unwrap())
    }
    pub fn colon_token(&self) -> SyntaxToken {
        self.0.colon_token().unwrap()
    }
    pub fn name(&self) -> CheckedName {
        CheckedName::cast_unchecked(self.0.name().unwrap())
    }
}
#[derive(Debug, Clone)]
pub struct CheckedCompoundConfigurationSpecification(CompoundConfigurationSpecificationSyntax);
impl CheckedNode for CheckedCompoundConfigurationSpecification {
    type Syntax = CompoundConfigurationSpecificationSyntax;
    fn cast_unchecked(syntax: CompoundConfigurationSpecificationSyntax) -> Self {
        CheckedCompoundConfigurationSpecification(syntax)
    }
    fn raw(&self) -> CompoundConfigurationSpecificationSyntax {
        self.0.clone()
    }
}
impl CheckedCompoundConfigurationSpecification {
    pub fn component_configuration_preamble(&self) -> CheckedComponentConfigurationPreamble {
        CheckedComponentConfigurationPreamble::cast_unchecked(
            self.0.component_configuration_preamble().unwrap(),
        )
    }
    pub fn compound_configuration_specification_items(
        &self,
    ) -> CheckedCompoundConfigurationSpecificationItems {
        CheckedCompoundConfigurationSpecificationItems::cast_unchecked(
            self.0.compound_configuration_specification_items().unwrap(),
        )
    }
    pub fn component_configuration_epilogue(&self) -> CheckedComponentConfigurationEpilogue {
        CheckedComponentConfigurationEpilogue::cast_unchecked(
            self.0.component_configuration_epilogue().unwrap(),
        )
    }
}
#[derive(Debug, Clone)]
pub struct CheckedCompoundConfigurationSpecificationItems(
    CompoundConfigurationSpecificationItemsSyntax,
);
impl CheckedNode for CheckedCompoundConfigurationSpecificationItems {
    type Syntax = CompoundConfigurationSpecificationItemsSyntax;
    fn cast_unchecked(syntax: CompoundConfigurationSpecificationItemsSyntax) -> Self {
        CheckedCompoundConfigurationSpecificationItems(syntax)
    }
    fn raw(&self) -> CompoundConfigurationSpecificationItemsSyntax {
        self.0.clone()
    }
}
impl CheckedCompoundConfigurationSpecificationItems {
    pub fn semi_colon_terminated_binding_indication(
        &self,
    ) -> CheckedSemiColonTerminatedBindingIndication {
        CheckedSemiColonTerminatedBindingIndication::cast_unchecked(
            self.0.semi_colon_terminated_binding_indication().unwrap(),
        )
    }
    pub fn verification_unit_binding_indications(
        &self,
    ) -> impl Iterator<Item = CheckedVerificationUnitBindingIndication> + use<'_> {
        self.0
            .verification_unit_binding_indications()
            .map(CheckedVerificationUnitBindingIndication::cast_unchecked)
    }
}
#[derive(Debug, Clone)]
pub enum CheckedConfigurationSpecification {
    SimpleConfigurationSpecification(CheckedSimpleConfigurationSpecification),
    CompoundConfigurationSpecification(CheckedCompoundConfigurationSpecification),
}
impl CheckedNode for CheckedConfigurationSpecification {
    type Syntax = ConfigurationSpecificationSyntax;
    fn cast_unchecked(syntax: ConfigurationSpecificationSyntax) -> Self {
        match syntax {
            ConfigurationSpecificationSyntax::SimpleConfigurationSpecification(inner) => {
                CheckedConfigurationSpecification::SimpleConfigurationSpecification(
                    CheckedSimpleConfigurationSpecification::cast_unchecked(inner),
                )
            }
            ConfigurationSpecificationSyntax::CompoundConfigurationSpecification(inner) => {
                CheckedConfigurationSpecification::CompoundConfigurationSpecification(
                    CheckedCompoundConfigurationSpecification::cast_unchecked(inner),
                )
            }
        }
    }
    fn raw(&self) -> Self::Syntax {
        match self {
            CheckedConfigurationSpecification::SimpleConfigurationSpecification(inner) => {
                ConfigurationSpecificationSyntax::SimpleConfigurationSpecification(inner.raw())
            }
            CheckedConfigurationSpecification::CompoundConfigurationSpecification(inner) => {
                ConfigurationSpecificationSyntax::CompoundConfigurationSpecification(inner.raw())
            }
        }
    }
}
#[derive(Debug, Clone)]
pub struct CheckedDisconnectionSpecification(DisconnectionSpecificationSyntax);
impl CheckedNode for CheckedDisconnectionSpecification {
    type Syntax = DisconnectionSpecificationSyntax;
    fn cast_unchecked(syntax: DisconnectionSpecificationSyntax) -> Self {
        CheckedDisconnectionSpecification(syntax)
    }
    fn raw(&self) -> DisconnectionSpecificationSyntax {
        self.0.clone()
    }
}
impl CheckedDisconnectionSpecification {
    pub fn disconnect_token(&self) -> SyntaxToken {
        self.0.disconnect_token().unwrap()
    }
    pub fn guarded_signal_specification(&self) -> CheckedGuardedSignalSpecification {
        CheckedGuardedSignalSpecification::cast_unchecked(
            self.0.guarded_signal_specification().unwrap(),
        )
    }
    pub fn after_token(&self) -> SyntaxToken {
        self.0.after_token().unwrap()
    }
    pub fn expression(&self) -> CheckedExpression {
        CheckedExpression::cast_unchecked(self.0.expression().unwrap())
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.0.semi_colon_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedEntityEntityAspect(EntityEntityAspectSyntax);
impl CheckedNode for CheckedEntityEntityAspect {
    type Syntax = EntityEntityAspectSyntax;
    fn cast_unchecked(syntax: EntityEntityAspectSyntax) -> Self {
        CheckedEntityEntityAspect(syntax)
    }
    fn raw(&self) -> EntityEntityAspectSyntax {
        self.0.clone()
    }
}
impl CheckedEntityEntityAspect {
    pub fn entity_token(&self) -> SyntaxToken {
        self.0.entity_token().unwrap()
    }
    pub fn name(&self) -> CheckedName {
        CheckedName::cast_unchecked(self.0.name().unwrap())
    }
}
#[derive(Debug, Clone)]
pub struct CheckedEntityConfigurationAspect(EntityConfigurationAspectSyntax);
impl CheckedNode for CheckedEntityConfigurationAspect {
    type Syntax = EntityConfigurationAspectSyntax;
    fn cast_unchecked(syntax: EntityConfigurationAspectSyntax) -> Self {
        CheckedEntityConfigurationAspect(syntax)
    }
    fn raw(&self) -> EntityConfigurationAspectSyntax {
        self.0.clone()
    }
}
impl CheckedEntityConfigurationAspect {
    pub fn configuration_token(&self) -> SyntaxToken {
        self.0.configuration_token().unwrap()
    }
    pub fn name(&self) -> CheckedName {
        CheckedName::cast_unchecked(self.0.name().unwrap())
    }
}
#[derive(Debug, Clone)]
pub struct CheckedEntityOpenAspect(EntityOpenAspectSyntax);
impl CheckedNode for CheckedEntityOpenAspect {
    type Syntax = EntityOpenAspectSyntax;
    fn cast_unchecked(syntax: EntityOpenAspectSyntax) -> Self {
        CheckedEntityOpenAspect(syntax)
    }
    fn raw(&self) -> EntityOpenAspectSyntax {
        self.0.clone()
    }
}
impl CheckedEntityOpenAspect {
    pub fn open_token(&self) -> SyntaxToken {
        self.0.open_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub enum CheckedEntityAspect {
    EntityEntityAspect(CheckedEntityEntityAspect),
    EntityConfigurationAspect(CheckedEntityConfigurationAspect),
    EntityOpenAspect(CheckedEntityOpenAspect),
}
impl CheckedNode for CheckedEntityAspect {
    type Syntax = EntityAspectSyntax;
    fn cast_unchecked(syntax: EntityAspectSyntax) -> Self {
        match syntax {
            EntityAspectSyntax::EntityEntityAspect(inner) => {
                CheckedEntityAspect::EntityEntityAspect(CheckedEntityEntityAspect::cast_unchecked(
                    inner,
                ))
            }
            EntityAspectSyntax::EntityConfigurationAspect(inner) => {
                CheckedEntityAspect::EntityConfigurationAspect(
                    CheckedEntityConfigurationAspect::cast_unchecked(inner),
                )
            }
            EntityAspectSyntax::EntityOpenAspect(inner) => CheckedEntityAspect::EntityOpenAspect(
                CheckedEntityOpenAspect::cast_unchecked(inner),
            ),
        }
    }
    fn raw(&self) -> Self::Syntax {
        match self {
            CheckedEntityAspect::EntityEntityAspect(inner) => {
                EntityAspectSyntax::EntityEntityAspect(inner.raw())
            }
            CheckedEntityAspect::EntityConfigurationAspect(inner) => {
                EntityAspectSyntax::EntityConfigurationAspect(inner.raw())
            }
            CheckedEntityAspect::EntityOpenAspect(inner) => {
                EntityAspectSyntax::EntityOpenAspect(inner.raw())
            }
        }
    }
}
#[derive(Debug, Clone)]
pub struct CheckedEntityDesignator(EntityDesignatorSyntax);
impl CheckedNode for CheckedEntityDesignator {
    type Syntax = EntityDesignatorSyntax;
    fn cast_unchecked(syntax: EntityDesignatorSyntax) -> Self {
        CheckedEntityDesignator(syntax)
    }
    fn raw(&self) -> EntityDesignatorSyntax {
        self.0.clone()
    }
}
impl CheckedEntityDesignator {
    pub fn entity_tag(&self) -> EntityTagSyntax {
        self.0.entity_tag().unwrap()
    }
    pub fn signature(&self) -> CheckedSignature {
        CheckedSignature::cast_unchecked(self.0.signature().unwrap())
    }
}
#[derive(Debug, Clone)]
pub struct CheckedEntityDesignatorList(EntityDesignatorListSyntax);
impl CheckedNode for CheckedEntityDesignatorList {
    type Syntax = EntityDesignatorListSyntax;
    fn cast_unchecked(syntax: EntityDesignatorListSyntax) -> Self {
        CheckedEntityDesignatorList(syntax)
    }
    fn raw(&self) -> EntityDesignatorListSyntax {
        self.0.clone()
    }
}
impl CheckedEntityDesignatorList {
    pub fn entity_designators(&self) -> impl Iterator<Item = CheckedEntityDesignator> + use<'_> {
        self.0
            .entity_designators()
            .map(CheckedEntityDesignator::cast_unchecked)
    }
    pub fn comma_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0.comma_token()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedEntityNameListAll(EntityNameListAllSyntax);
impl CheckedNode for CheckedEntityNameListAll {
    type Syntax = EntityNameListAllSyntax;
    fn cast_unchecked(syntax: EntityNameListAllSyntax) -> Self {
        CheckedEntityNameListAll(syntax)
    }
    fn raw(&self) -> EntityNameListAllSyntax {
        self.0.clone()
    }
}
impl CheckedEntityNameListAll {
    pub fn all_token(&self) -> SyntaxToken {
        self.0.all_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedEntityNameListOthers(EntityNameListOthersSyntax);
impl CheckedNode for CheckedEntityNameListOthers {
    type Syntax = EntityNameListOthersSyntax;
    fn cast_unchecked(syntax: EntityNameListOthersSyntax) -> Self {
        CheckedEntityNameListOthers(syntax)
    }
    fn raw(&self) -> EntityNameListOthersSyntax {
        self.0.clone()
    }
}
impl CheckedEntityNameListOthers {
    pub fn others_token(&self) -> SyntaxToken {
        self.0.others_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub enum CheckedEntityNameList {
    EntityDesignatorList(CheckedEntityDesignatorList),
    EntityNameListAll(CheckedEntityNameListAll),
    EntityNameListOthers(CheckedEntityNameListOthers),
}
impl CheckedNode for CheckedEntityNameList {
    type Syntax = EntityNameListSyntax;
    fn cast_unchecked(syntax: EntityNameListSyntax) -> Self {
        match syntax {
            EntityNameListSyntax::EntityDesignatorList(inner) => {
                CheckedEntityNameList::EntityDesignatorList(
                    CheckedEntityDesignatorList::cast_unchecked(inner),
                )
            }
            EntityNameListSyntax::EntityNameListAll(inner) => {
                CheckedEntityNameList::EntityNameListAll(CheckedEntityNameListAll::cast_unchecked(
                    inner,
                ))
            }
            EntityNameListSyntax::EntityNameListOthers(inner) => {
                CheckedEntityNameList::EntityNameListOthers(
                    CheckedEntityNameListOthers::cast_unchecked(inner),
                )
            }
        }
    }
    fn raw(&self) -> Self::Syntax {
        match self {
            CheckedEntityNameList::EntityDesignatorList(inner) => {
                EntityNameListSyntax::EntityDesignatorList(inner.raw())
            }
            CheckedEntityNameList::EntityNameListAll(inner) => {
                EntityNameListSyntax::EntityNameListAll(inner.raw())
            }
            CheckedEntityNameList::EntityNameListOthers(inner) => {
                EntityNameListSyntax::EntityNameListOthers(inner.raw())
            }
        }
    }
}
#[derive(Debug, Clone)]
pub struct CheckedEntitySpecification(EntitySpecificationSyntax);
impl CheckedNode for CheckedEntitySpecification {
    type Syntax = EntitySpecificationSyntax;
    fn cast_unchecked(syntax: EntitySpecificationSyntax) -> Self {
        CheckedEntitySpecification(syntax)
    }
    fn raw(&self) -> EntitySpecificationSyntax {
        self.0.clone()
    }
}
impl CheckedEntitySpecification {
    pub fn entity_name_list(&self) -> CheckedEntityNameList {
        CheckedEntityNameList::cast_unchecked(self.0.entity_name_list().unwrap())
    }
    pub fn colon_token(&self) -> SyntaxToken {
        self.0.colon_token().unwrap()
    }
    pub fn entity_class(&self) -> EntityClassSyntax {
        self.0.entity_class().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedGuardedSignalSpecification(GuardedSignalSpecificationSyntax);
impl CheckedNode for CheckedGuardedSignalSpecification {
    type Syntax = GuardedSignalSpecificationSyntax;
    fn cast_unchecked(syntax: GuardedSignalSpecificationSyntax) -> Self {
        CheckedGuardedSignalSpecification(syntax)
    }
    fn raw(&self) -> GuardedSignalSpecificationSyntax {
        self.0.clone()
    }
}
impl CheckedGuardedSignalSpecification {
    pub fn signal_list(&self) -> CheckedSignalList {
        CheckedSignalList::cast_unchecked(self.0.signal_list().unwrap())
    }
    pub fn colon_token(&self) -> SyntaxToken {
        self.0.colon_token().unwrap()
    }
    pub fn name(&self) -> CheckedName {
        CheckedName::cast_unchecked(self.0.name().unwrap())
    }
}
#[derive(Debug, Clone)]
pub struct CheckedInstantiationListList(InstantiationListListSyntax);
impl CheckedNode for CheckedInstantiationListList {
    type Syntax = InstantiationListListSyntax;
    fn cast_unchecked(syntax: InstantiationListListSyntax) -> Self {
        CheckedInstantiationListList(syntax)
    }
    fn raw(&self) -> InstantiationListListSyntax {
        self.0.clone()
    }
}
impl CheckedInstantiationListList {
    pub fn identifier_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0.identifier_token()
    }
    pub fn comma_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0.comma_token()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedInstantiationListAll(InstantiationListAllSyntax);
impl CheckedNode for CheckedInstantiationListAll {
    type Syntax = InstantiationListAllSyntax;
    fn cast_unchecked(syntax: InstantiationListAllSyntax) -> Self {
        CheckedInstantiationListAll(syntax)
    }
    fn raw(&self) -> InstantiationListAllSyntax {
        self.0.clone()
    }
}
impl CheckedInstantiationListAll {
    pub fn all_token(&self) -> SyntaxToken {
        self.0.all_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedInstantiationListOthers(InstantiationListOthersSyntax);
impl CheckedNode for CheckedInstantiationListOthers {
    type Syntax = InstantiationListOthersSyntax;
    fn cast_unchecked(syntax: InstantiationListOthersSyntax) -> Self {
        CheckedInstantiationListOthers(syntax)
    }
    fn raw(&self) -> InstantiationListOthersSyntax {
        self.0.clone()
    }
}
impl CheckedInstantiationListOthers {
    pub fn others_token(&self) -> SyntaxToken {
        self.0.others_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub enum CheckedInstantiationList {
    InstantiationListList(CheckedInstantiationListList),
    InstantiationListAll(CheckedInstantiationListAll),
    InstantiationListOthers(CheckedInstantiationListOthers),
}
impl CheckedNode for CheckedInstantiationList {
    type Syntax = InstantiationListSyntax;
    fn cast_unchecked(syntax: InstantiationListSyntax) -> Self {
        match syntax {
            InstantiationListSyntax::InstantiationListList(inner) => {
                CheckedInstantiationList::InstantiationListList(
                    CheckedInstantiationListList::cast_unchecked(inner),
                )
            }
            InstantiationListSyntax::InstantiationListAll(inner) => {
                CheckedInstantiationList::InstantiationListAll(
                    CheckedInstantiationListAll::cast_unchecked(inner),
                )
            }
            InstantiationListSyntax::InstantiationListOthers(inner) => {
                CheckedInstantiationList::InstantiationListOthers(
                    CheckedInstantiationListOthers::cast_unchecked(inner),
                )
            }
        }
    }
    fn raw(&self) -> Self::Syntax {
        match self {
            CheckedInstantiationList::InstantiationListList(inner) => {
                InstantiationListSyntax::InstantiationListList(inner.raw())
            }
            CheckedInstantiationList::InstantiationListAll(inner) => {
                InstantiationListSyntax::InstantiationListAll(inner.raw())
            }
            CheckedInstantiationList::InstantiationListOthers(inner) => {
                InstantiationListSyntax::InstantiationListOthers(inner.raw())
            }
        }
    }
}
#[derive(Debug, Clone)]
pub struct CheckedSignalListList(SignalListListSyntax);
impl CheckedNode for CheckedSignalListList {
    type Syntax = SignalListListSyntax;
    fn cast_unchecked(syntax: SignalListListSyntax) -> Self {
        CheckedSignalListList(syntax)
    }
    fn raw(&self) -> SignalListListSyntax {
        self.0.clone()
    }
}
impl CheckedSignalListList {
    pub fn names(&self) -> impl Iterator<Item = CheckedName> + use<'_> {
        self.0.names().map(CheckedName::cast_unchecked)
    }
    pub fn comma_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0.comma_token()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedSignalListAll(SignalListAllSyntax);
impl CheckedNode for CheckedSignalListAll {
    type Syntax = SignalListAllSyntax;
    fn cast_unchecked(syntax: SignalListAllSyntax) -> Self {
        CheckedSignalListAll(syntax)
    }
    fn raw(&self) -> SignalListAllSyntax {
        self.0.clone()
    }
}
impl CheckedSignalListAll {
    pub fn all_token(&self) -> SyntaxToken {
        self.0.all_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedSignalListOthers(SignalListOthersSyntax);
impl CheckedNode for CheckedSignalListOthers {
    type Syntax = SignalListOthersSyntax;
    fn cast_unchecked(syntax: SignalListOthersSyntax) -> Self {
        CheckedSignalListOthers(syntax)
    }
    fn raw(&self) -> SignalListOthersSyntax {
        self.0.clone()
    }
}
impl CheckedSignalListOthers {
    pub fn others_token(&self) -> SyntaxToken {
        self.0.others_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub enum CheckedSignalList {
    SignalListList(CheckedSignalListList),
    SignalListAll(CheckedSignalListAll),
    SignalListOthers(CheckedSignalListOthers),
}
impl CheckedNode for CheckedSignalList {
    type Syntax = SignalListSyntax;
    fn cast_unchecked(syntax: SignalListSyntax) -> Self {
        match syntax {
            SignalListSyntax::SignalListList(inner) => {
                CheckedSignalList::SignalListList(CheckedSignalListList::cast_unchecked(inner))
            }
            SignalListSyntax::SignalListAll(inner) => {
                CheckedSignalList::SignalListAll(CheckedSignalListAll::cast_unchecked(inner))
            }
            SignalListSyntax::SignalListOthers(inner) => {
                CheckedSignalList::SignalListOthers(CheckedSignalListOthers::cast_unchecked(inner))
            }
        }
    }
    fn raw(&self) -> Self::Syntax {
        match self {
            CheckedSignalList::SignalListList(inner) => {
                SignalListSyntax::SignalListList(inner.raw())
            }
            CheckedSignalList::SignalListAll(inner) => SignalListSyntax::SignalListAll(inner.raw()),
            CheckedSignalList::SignalListOthers(inner) => {
                SignalListSyntax::SignalListOthers(inner.raw())
            }
        }
    }
}
#[derive(Debug, Clone)]
pub struct CheckedSimpleConfigurationSpecification(SimpleConfigurationSpecificationSyntax);
impl CheckedNode for CheckedSimpleConfigurationSpecification {
    type Syntax = SimpleConfigurationSpecificationSyntax;
    fn cast_unchecked(syntax: SimpleConfigurationSpecificationSyntax) -> Self {
        CheckedSimpleConfigurationSpecification(syntax)
    }
    fn raw(&self) -> SimpleConfigurationSpecificationSyntax {
        self.0.clone()
    }
}
impl CheckedSimpleConfigurationSpecification {
    pub fn component_configuration_preamble(&self) -> CheckedComponentConfigurationPreamble {
        CheckedComponentConfigurationPreamble::cast_unchecked(
            self.0.component_configuration_preamble().unwrap(),
        )
    }
    pub fn semi_colon_terminated_binding_indication(
        &self,
    ) -> CheckedSemiColonTerminatedBindingIndication {
        CheckedSemiColonTerminatedBindingIndication::cast_unchecked(
            self.0.semi_colon_terminated_binding_indication().unwrap(),
        )
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.0.semi_colon_token().unwrap()
    }
    pub fn component_configuration_epilogue(
        &self,
    ) -> Option<CheckedComponentConfigurationEpilogue> {
        self.0
            .component_configuration_epilogue()
            .map(CheckedComponentConfigurationEpilogue::cast_unchecked)
    }
}
#[derive(Debug, Clone)]
pub struct CheckedVerificationUnitBindingIndication(VerificationUnitBindingIndicationSyntax);
impl CheckedNode for CheckedVerificationUnitBindingIndication {
    type Syntax = VerificationUnitBindingIndicationSyntax;
    fn cast_unchecked(syntax: VerificationUnitBindingIndicationSyntax) -> Self {
        CheckedVerificationUnitBindingIndication(syntax)
    }
    fn raw(&self) -> VerificationUnitBindingIndicationSyntax {
        self.0.clone()
    }
}
impl CheckedVerificationUnitBindingIndication {
    pub fn use_token(&self) -> SyntaxToken {
        self.0.use_token().unwrap()
    }
    pub fn vunit_token(&self) -> SyntaxToken {
        self.0.vunit_token().unwrap()
    }
    pub fn verification_unit_list(&self) -> Option<CheckedVerificationUnitList> {
        self.0
            .verification_unit_list()
            .map(CheckedVerificationUnitList::cast_unchecked)
    }
}
#[derive(Debug, Clone)]
pub struct CheckedVerificationUnitList(VerificationUnitListSyntax);
impl CheckedNode for CheckedVerificationUnitList {
    type Syntax = VerificationUnitListSyntax;
    fn cast_unchecked(syntax: VerificationUnitListSyntax) -> Self {
        CheckedVerificationUnitList(syntax)
    }
    fn raw(&self) -> VerificationUnitListSyntax {
        self.0.clone()
    }
}
impl CheckedVerificationUnitList {
    pub fn names(&self) -> impl Iterator<Item = CheckedName> + use<'_> {
        self.0.names().map(CheckedName::cast_unchecked)
    }
    pub fn comma_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0.comma_token()
    }
}
