// This Source Code Form is subject to the terms of the Mozilla Public
// Lic// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// This Source Code Form is subject to the terms of the Mozilla Public
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2023, Olof Kraigher olof.kraigher@gmail.com

use crate::ast::visitor::VisitorResult::{Continue, Skip, Stop};
use crate::ast::*;
use crate::syntax::TokenAccess;
use itertools::Itertools;
use std::ops::Deref;

#[derive(PartialEq)]
pub enum VisitorResult {
    Continue,
    Stop,
    Skip,
}

pub trait Visitor {
    fn visit_attribute_name(
        &mut self,
        _node: &AttributeName,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_attribute_designator(
        &mut self,
        _node: &AttributeDesignator,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_external_path(
        &mut self,
        _node: &ExternalPath,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_external_name(
        &mut self,
        _node: &ExternalName,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_name(&mut self, _node: &Name, _ctx: &dyn TokenAccess) -> VisitorResult {
        Continue
    }
    fn visit_selected_name(
        &mut self,
        _node: &SelectedName,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_call_or_indexed(
        &mut self,
        _node: &CallOrIndexed,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_choice(&mut self, _node: &Choice, _ctx: &dyn TokenAccess) -> VisitorResult {
        Continue
    }
    fn visit_element_association(
        &mut self,
        _node: &ElementAssociation,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_actual_part(&mut self, _node: &ActualPart, _ctx: &dyn TokenAccess) -> VisitorResult {
        Continue
    }
    fn visit_association_element(
        &mut self,
        _node: &AssociationElement,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_abstract_literal(
        &mut self,
        _node: &AbstractLiteral,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_bit_string(&mut self, _node: &BitString, _ctx: &dyn TokenAccess) -> VisitorResult {
        Continue
    }
    fn visit_physical_literal(
        &mut self,
        _node: &PhysicalLiteral,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_literal(&mut self, _node: &Literal, _ctx: &dyn TokenAccess) -> VisitorResult {
        Continue
    }
    fn visit_allocator(&mut self, _node: &Allocator, _ctx: &dyn TokenAccess) -> VisitorResult {
        Continue
    }
    fn visit_qualified_expression(
        &mut self,
        _node: &QualifiedExpression,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_expression(&mut self, _node: &Expression, _ctx: &dyn TokenAccess) -> VisitorResult {
        Continue
    }
    fn visit_ident(&mut self, _node: &Ident, _ctx: &dyn TokenAccess) -> VisitorResult {
        Continue
    }
    fn visit_discrete_range(
        &mut self,
        _node: &DiscreteRange,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_range_constraint(
        &mut self,
        _node: &RangeConstraint,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_range(&mut self, _node: &Range, _ctx: &dyn TokenAccess) -> VisitorResult {
        Continue
    }
    fn visit_element_constraint(
        &mut self,
        _node: &ElementConstraint,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_subtype_constraint(
        &mut self,
        _node: &SubtypeConstraint,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_record_element_resolution(
        &mut self,
        _node: &RecordElementResolution,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_resolution_indication(
        &mut self,
        _node: &ResolutionIndication,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_type_mark(&mut self, _node: &TypeMark, _ctx: &dyn TokenAccess) -> VisitorResult {
        Continue
    }
    fn visit_subtype_indication(
        &mut self,
        _node: &SubtypeIndication,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_array_index(&mut self, _node: &ArrayIndex, _ctx: &dyn TokenAccess) -> VisitorResult {
        Continue
    }
    fn visit_element_declaration(
        &mut self,
        _node: &ElementDeclaration,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_protected_type_declarative_item(
        &mut self,
        _node: &ProtectedTypeDeclarativeItem,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_designator(&mut self, _node: &Designator, _ctx: &dyn TokenAccess) -> VisitorResult {
        Continue
    }
    fn visit_alias_declaration(
        &mut self,
        _node: &AliasDeclaration,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_attribute_declaration(
        &mut self,
        _node: &AttributeDeclaration,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_entity_tag(&mut self, _node: &EntityTag, _ctx: &dyn TokenAccess) -> VisitorResult {
        Continue
    }
    fn visit_entity_name(&mut self, _node: &EntityName, _ctx: &dyn TokenAccess) -> VisitorResult {
        Continue
    }
    fn visit_attribute_specification(
        &mut self,
        _node: &AttributeSpecification,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_attribute(&mut self, _node: &Attribute, _ctx: &dyn TokenAccess) -> VisitorResult {
        Continue
    }
    fn visit_protected_type_declaration(
        &mut self,
        _node: &ProtectedTypeDeclaration,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_protected_type_body(
        &mut self,
        _node: &ProtectedTypeBody,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_physical_type_declaration(
        &mut self,
        _node: &PhysicalTypeDeclaration,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_enumeration_literal(
        &mut self,
        _node: &EnumerationLiteral,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_type_definition(
        &mut self,
        _node: &TypeDefinition,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_type_declaration(
        &mut self,
        _node: &TypeDeclaration,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_object_declaration(
        &mut self,
        _node: &ObjectDeclaration,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_file_declaration(
        &mut self,
        _node: &FileDeclaration,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_subprogram_designator(
        &mut self,
        _node: &SubprogramDesignator,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_procedure_specification(
        &mut self,
        _node: &ProcedureSpecification,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_function_specification(
        &mut self,
        _node: &FunctionSpecification,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_subprogram_body(
        &mut self,
        _node: &SubprogramBody,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }

    fn visit_subprogram_header(
        &mut self,
        _node: &SubprogramHeader,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_signature(&mut self, _node: &Signature, _ctx: &dyn TokenAccess) -> VisitorResult {
        Continue
    }
    fn visit_subprogram_declaration(
        &mut self,
        _node: &SubprogramDeclaration,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_interface_file_declaration(
        &mut self,
        _node: &InterfaceFileDeclaration,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_interface_object_declaration(
        &mut self,
        _node: &InterfaceObjectDeclaration,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_subprogram_default(
        &mut self,
        _node: &SubprogramDefault,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_interface_package_generic_map_aspect(
        &mut self,
        _node: &InterfacePackageGenericMapAspect,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_interface_package_declaration(
        &mut self,
        _node: &InterfacePackageDeclaration,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_interface_declaration(
        &mut self,
        _node: &InterfaceDeclaration,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_port_clause(&mut self, _node: &PortClause, _ctx: &dyn TokenAccess) -> VisitorResult {
        Continue
    }
    fn visit_component_declaration(
        &mut self,
        _node: &ComponentDeclaration,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_declaration(&mut self, _node: &Declaration, _ctx: &dyn TokenAccess) -> VisitorResult {
        Continue
    }
    fn visit_wait_statement(
        &mut self,
        _node: &WaitStatement,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_assert_statement(
        &mut self,
        _node: &AssertStatement,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_report_statement(
        &mut self,
        _node: &ReportStatement,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_target(&mut self, _node: &Target, _ctx: &dyn TokenAccess) -> VisitorResult {
        Continue
    }
    fn visit_waveform_element(
        &mut self,
        _node: &WaveformElement,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_waveform(&mut self, _node: &Waveform, _ctx: &dyn TokenAccess) -> VisitorResult {
        Continue
    }
    fn visit_delay_mechanism(
        &mut self,
        _node: &DelayMechanism,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_signal_assignment(
        &mut self,
        _node: &SignalAssignment,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_signal_force_assignment(
        &mut self,
        _node: &SignalForceAssignment,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_signal_release_assignment(
        &mut self,
        _node: &SignalReleaseAssignment,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_variable_assignment(
        &mut self,
        _node: &VariableAssignment,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_if_statement(&mut self, _node: &IfStatement, _ctx: &dyn TokenAccess) -> VisitorResult {
        Continue
    }
    fn visit_case_statement(
        &mut self,
        _node: &CaseStatement,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_iteration_scheme(
        &mut self,
        _node: &IterationScheme,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_loop_statement(
        &mut self,
        _node: &LoopStatement,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_next_statement(
        &mut self,
        _node: &NextStatement,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_exit_statement(
        &mut self,
        _node: &ExitStatement,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_return_statement(
        &mut self,
        _node: &ReturnStatement,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_sequential_statement(
        &mut self,
        _node: &SequentialStatement,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_labeled_sequential_statement(
        &mut self,
        _node: &LabeledSequentialStatement,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_block_statement(
        &mut self,
        _node: &BlockStatement,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_block_header(&mut self, _node: &BlockHeader, _ctx: &dyn TokenAccess) -> VisitorResult {
        Continue
    }
    fn visit_sensitivity_list(
        &mut self,
        _node: &SensitivityList,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_process_statement(
        &mut self,
        _node: &ProcessStatement,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_concurrent_procedure_call(
        &mut self,
        _node: &ConcurrentProcedureCall,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_concurrent_assert_statement(
        &mut self,
        _node: &ConcurrentAssertStatement,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_concurrent_signal_assignment(
        &mut self,
        _node: &ConcurrentSignalAssignment,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_instantiated_unit(
        &mut self,
        _node: &InstantiatedUnit,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_instantiation_statement(
        &mut self,
        _node: &InstantiationStatement,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_generate_body(
        &mut self,
        _node: &GenerateBody,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_for_generate_statement(
        &mut self,
        _node: &ForGenerateStatement,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_if_generate_statement(
        &mut self,
        _node: &IfGenerateStatement,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_case_generate_statement(
        &mut self,
        _node: &CaseGenerateStatement,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_concurrent_statement(
        &mut self,
        _node: &ConcurrentStatement,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_labeled_concurrent_statement(
        &mut self,
        _node: &LabeledConcurrentStatement,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_library_clause(
        &mut self,
        _node: &LibraryClause,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_use_clause(&mut self, _node: &UseClause, _ctx: &dyn TokenAccess) -> VisitorResult {
        Continue
    }
    fn visit_context_reference(
        &mut self,
        _node: &ContextReference,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_context_item(&mut self, _node: &ContextItem, _ctx: &dyn TokenAccess) -> VisitorResult {
        Continue
    }
    fn visit_context_declaration(
        &mut self,
        _node: &ContextDeclaration,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_package_instantiation(
        &mut self,
        _node: &PackageInstantiation,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_instantiation_list(
        &mut self,
        _node: &InstantiationList,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_entity_aspect(
        &mut self,
        _node: &EntityAspect,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_binding_indication(
        &mut self,
        _node: &BindingIndication,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_component_specification(
        &mut self,
        _node: &ComponentSpecification,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_v_unit_binding_indication(
        &mut self,
        _node: &VUnitBindingIndication,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_configuration_specification(
        &mut self,
        _node: &ConfigurationSpecification,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_configuration_declarative_item(
        &mut self,
        _node: &ConfigurationDeclarativeItem,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_component_configuration(
        &mut self,
        _node: &ComponentConfiguration,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_configuration_item(
        &mut self,
        _node: &ConfigurationItem,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_block_configuration(
        &mut self,
        _node: &BlockConfiguration,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_configuration_declaration(
        &mut self,
        _node: &ConfigurationDeclaration,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_entity_declaration(
        &mut self,
        _node: &EntityDeclaration,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_architecture_body(
        &mut self,
        _node: &ArchitectureBody,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_package_declaration(
        &mut self,
        _node: &PackageDeclaration,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_package_body(&mut self, _node: &PackageBody, _ctx: &dyn TokenAccess) -> VisitorResult {
        Continue
    }
    fn visit_any_primary_unit(
        &mut self,
        _node: &AnyPrimaryUnit,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_any_secondary_unit(
        &mut self,
        _node: &AnySecondaryUnit,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_any_design_unit(
        &mut self,
        _node: &AnyDesignUnit,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_design_file(&mut self, _node: &DesignFile) -> VisitorResult {
        Continue
    }
    fn visit_reference(&mut self, _node: &Reference, _ctx: &dyn TokenAccess) -> VisitorResult {
        Continue
    }
    fn visit_item_with_pos(
        &mut self,
        _pos: &SrcPos,
        _node: &dyn ASTNode,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_item_with_decl(
        &mut self,
        _decl: &Option<EntityId>,
        _node: &dyn ASTNode,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_item_with_reference(
        &mut self,
        _ref: &Reference,
        _node: &dyn ASTNode,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        Continue
    }
    fn visit_name_list(&mut self, _node: &NameList, _ctx: &dyn TokenAccess) -> VisitorResult {
        Continue
    }

    fn visit_map_aspect(&mut self, _node: &MapAspect, _ctx: &dyn TokenAccess) -> VisitorResult {
        Continue
    }
}

/// An AST Node has two methods it needs to declare:
/// - A `visit(Visitor, Context)` method.
/// - A `children()` method.
pub trait ASTNode {
    /// Called when traversing the AST.
    /// Each node must call the respective method in the `Visitor` class.
    /// If the node doesn't have a representation
    /// (for example, for utility types such as `Box` or `Vec`),
    /// simply return `Continue`.
    fn visit(&self, visitor: &mut dyn Visitor, _ctx: &dyn TokenAccess) -> VisitorResult;

    /// Returns the Children of this Node.
    /// Must return all children that are considered AST elements.
    /// Doesn't return auxiliary information such as the Tokens.
    fn children(&self) -> Vec<&dyn ASTNode>;
}

pub fn walk(node: &dyn ASTNode, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) {
    let mut stack: Vec<&dyn ASTNode> = vec![node];
    while let Some(node) = stack.pop() {
        match node.visit(visitor, ctx) {
            Stop => return,
            Skip => continue,
            _ => {}
        }
        for child in node.children().into_iter().rev() {
            stack.push(child);
        }
    }
}

pub fn walk_design_file(node: &DesignFile, visitor: &mut dyn Visitor) {
    visitor.visit_design_file(node);
    for (tokens, unit) in &node.design_units {
        walk(unit, visitor, tokens);
    }
}

impl<T: ASTNode> ASTNode for Box<T> {
    fn visit(&self, _visitor: &mut dyn Visitor, _ctx: &dyn TokenAccess) -> VisitorResult {
        Continue
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![self.deref()]
    }
}

impl<T: ASTNode> ASTNode for Option<T> {
    fn visit(&self, _visitor: &mut dyn Visitor, _ctx: &dyn TokenAccess) -> VisitorResult {
        Continue
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match &self {
            None => vec![],
            Some(el) => vec![el],
        }
    }
}

impl<T: ASTNode, U: ASTNode> ASTNode for (T, U) {
    fn visit(&self, _visitor: &mut dyn Visitor, _ctx: &dyn TokenAccess) -> VisitorResult {
        Continue
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.0, &self.1]
    }
}

impl<T: ASTNode> ASTNode for Vec<T> {
    fn visit(&self, _visitor: &mut dyn Visitor, _ctx: &dyn TokenAccess) -> VisitorResult {
        Continue
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        self.iter().map(|f| f as &dyn ASTNode).collect()
    }
}

impl<T: ASTNode> ASTNode for WithPos<T> {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_item_with_pos(&self.pos, &self.item, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.item]
    }
}

impl<T: ASTNode> ASTNode for WithDecl<T> {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_item_with_decl(&self.decl, &self.tree, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.tree]
    }
}

impl<T: ASTNode> ASTNode for WithRef<T> {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_item_with_reference(&self.reference, &self.item, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.reference, &self.item]
    }
}

impl<T: ASTNode> ASTNode for Conditional<T> {
    fn visit(&self, _visitor: &mut dyn Visitor, _ctx: &dyn TokenAccess) -> VisitorResult {
        Continue
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.condition, &self.item]
    }
}

impl<T: ASTNode> ASTNode for Conditionals<T> {
    fn visit(&self, _visitor: &mut dyn Visitor, _ctx: &dyn TokenAccess) -> VisitorResult {
        Continue
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.conditionals, &self.else_item]
    }
}

impl<T: ASTNode> ASTNode for Selection<T> {
    fn visit(&self, _visitor: &mut dyn Visitor, _ctx: &dyn TokenAccess) -> VisitorResult {
        Continue
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.alternatives, &self.expression]
    }
}

impl<T: ASTNode> ASTNode for AssignmentRightHand<T> {
    fn visit(&self, _visitor: &mut dyn Visitor, _ctx: &dyn TokenAccess) -> VisitorResult {
        Continue
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match &self {
            AssignmentRightHand::Simple(expr) => vec![expr],
            AssignmentRightHand::Conditional(conds) => vec![conds],
            AssignmentRightHand::Selected(sel) => vec![sel],
        }
    }
}

impl<T: ASTNode> ASTNode for Alternative<T> {
    fn visit(&self, _visitor: &mut dyn Visitor, _ctx: &dyn TokenAccess) -> VisitorResult {
        Continue
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.choices, &self.item]
    }
}

impl ASTNode for DesignFile {
    fn visit(&self, visitor: &mut dyn Visitor, _ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_design_file(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        self.design_units
            .iter()
            .map(|it| &it.1 as &dyn ASTNode)
            .collect_vec()
    }
}

impl ASTNode for AnyDesignUnit {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_any_design_unit(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match self {
            AnyDesignUnit::Primary(unit) => vec![unit],
            AnyDesignUnit::Secondary(unit) => vec![unit],
        }
    }
}

impl ASTNode for AnyPrimaryUnit {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_any_primary_unit(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match self {
            AnyPrimaryUnit::Entity(decl) => vec![decl],
            AnyPrimaryUnit::Configuration(decl) => vec![decl],
            AnyPrimaryUnit::Package(decl) => vec![decl],
            AnyPrimaryUnit::PackageInstance(decl) => vec![decl],
            AnyPrimaryUnit::Context(decl) => vec![decl],
        }
    }
}

impl ASTNode for ContextDeclaration {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_context_declaration(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.ident, &self.items]
    }
}

impl ASTNode for ContextItem {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_context_item(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match self {
            ContextItem::Use(clause) => vec![clause],
            ContextItem::Library(clause) => vec![clause],
            ContextItem::Context(clause) => vec![clause],
        }
    }
}

impl ASTNode for ContextReference {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_context_reference(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.name_list]
    }
}

impl<T: ASTNode> ASTNode for SeparatedList<T> {
    fn visit(&self, _visitor: &mut dyn Visitor, _ctx: &dyn TokenAccess) -> VisitorResult {
        Continue
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        self.items.iter().map(|it| it as &dyn ASTNode).collect_vec()
    }
}

impl ASTNode for LibraryClause {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_library_clause(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.name_list]
    }
}

impl ASTNode for UseClause {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_use_clause(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.name_list]
    }
}

impl ASTNode for Ident {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_ident(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![]
    }
}

impl ASTNode for PackageInstantiation {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_package_instantiation(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![
            &self.context_clause,
            &self.ident,
            &self.package_name,
            &self.generic_map,
        ]
    }
}

impl ASTNode for AssociationElement {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_association_element(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.formal, &self.actual]
    }
}

impl ASTNode for ActualPart {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_actual_part(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match self {
            ActualPart::Expression(expr) => vec![expr],
            ActualPart::Open => vec![],
        }
    }
}

impl ASTNode for SelectedName {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_selected_name(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match self {
            SelectedName::Designator(desi) => vec![desi],
            SelectedName::Selected(name, desi) => vec![name, desi],
        }
    }
}

impl ASTNode for Designator {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_designator(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![]
    }
}

impl ASTNode for PackageDeclaration {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_package_declaration(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![
            &self.context_clause,
            &self.ident,
            &self.generic_clause,
            &self.decl,
        ]
    }
}

impl ASTNode for Declaration {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_declaration(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match self {
            Declaration::Object(decl) => vec![decl],
            Declaration::File(decl) => vec![decl],
            Declaration::Type(decl) => vec![decl],
            Declaration::Component(decl) => vec![decl],
            Declaration::Attribute(decl) => vec![decl],
            Declaration::Alias(decl) => vec![decl],
            Declaration::SubprogramDeclaration(decl) => vec![decl],
            Declaration::SubprogramBody(decl) => vec![decl],
            Declaration::Use(decl) => vec![decl],
            Declaration::Package(decl) => vec![decl],
            Declaration::Configuration(decl) => vec![decl],
        }
    }
}

impl ASTNode for ConfigurationSpecification {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_configuration_specification(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.spec, &self.bind_ind, &self.vunit_bind_inds]
    }
}

impl ASTNode for VUnitBindingIndication {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_v_unit_binding_indication(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.vunit_list]
    }
}

impl ASTNode for BindingIndication {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_binding_indication(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.entity_aspect, &self.generic_map, &self.port_map]
    }
}

impl ASTNode for EntityAspect {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_entity_aspect(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match self {
            EntityAspect::Entity(name, id) => vec![name, id],
            EntityAspect::Configuration(config) => vec![config],
            EntityAspect::Open => vec![],
        }
    }
}

impl ASTNode for ComponentSpecification {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_component_specification(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.instantiation_list, &self.component_name]
    }
}

impl ASTNode for InstantiationList {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_instantiation_list(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match self {
            InstantiationList::Labels(idents) => vec![idents],
            InstantiationList::Others => vec![],
            InstantiationList::All => vec![],
        }
    }
}

impl ASTNode for SubprogramBody {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_subprogram_body(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.specification, &self.declarations, &self.statements]
    }
}

impl ASTNode for SubprogramHeader {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_subprogram_header(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.map_aspect, &self.generic_list]
    }
}

impl ASTNode for LabeledSequentialStatement {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_labeled_sequential_statement(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.label, &self.statement]
    }
}

impl ASTNode for SubprogramDeclaration {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_subprogram_declaration(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match self {
            SubprogramDeclaration::Procedure(proc) => vec![proc],
            SubprogramDeclaration::Function(func) => vec![func],
        }
    }
}

impl ASTNode for SequentialStatement {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_sequential_statement(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match self {
            SequentialStatement::Wait(stmt) => vec![stmt],
            SequentialStatement::Assert(stmt) => vec![stmt],
            SequentialStatement::Report(stmt) => vec![stmt],
            SequentialStatement::VariableAssignment(stmt) => vec![stmt],
            SequentialStatement::SignalAssignment(stmt) => vec![stmt],
            SequentialStatement::SignalForceAssignment(stmt) => vec![stmt],
            SequentialStatement::SignalReleaseAssignment(stmt) => vec![stmt],
            SequentialStatement::ProcedureCall(stmt) => vec![stmt],
            SequentialStatement::If(stmt) => vec![stmt],
            SequentialStatement::Case(stmt) => vec![stmt],
            SequentialStatement::Loop(stmt) => vec![stmt],
            SequentialStatement::Next(stmt) => vec![stmt],
            SequentialStatement::Exit(stmt) => vec![stmt],
            SequentialStatement::Return(stmt) => vec![stmt],
            SequentialStatement::Null => vec![],
        }
    }
}

impl ASTNode for CaseStatement {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_case_statement(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.expression, &self.alternatives]
    }
}

impl ASTNode for ReturnStatement {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_return_statement(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match &self.expression {
            Some(expr) => vec![expr],
            None => vec![],
        }
    }
}

impl ASTNode for ExitStatement {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_exit_statement(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.loop_label, &self.condition]
    }
}

impl ASTNode for NextStatement {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_next_statement(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.loop_label, &self.condition]
    }
}

impl ASTNode for LoopStatement {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_loop_statement(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.iteration_scheme, &self.statements]
    }
}

impl ASTNode for IterationScheme {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_iteration_scheme(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match self {
            IterationScheme::While(scheme) => vec![scheme],
            IterationScheme::For(ident, range) => vec![ident, range],
        }
    }
}

impl ASTNode for DiscreteRange {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_discrete_range(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match self {
            DiscreteRange::Discrete(type_mark, range) => vec![type_mark, range],
            DiscreteRange::Range(range) => vec![range],
        }
    }
}

impl ASTNode for TypeMark {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_type_mark(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.name]
    }
}

impl ASTNode for Range {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_range(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match self {
            Range::Range(constraint) => vec![constraint],
            Range::Attribute(attr) => vec![attr.deref()],
        }
    }
}

impl ASTNode for RangeConstraint {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_range_constraint(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.left_expr, &self.right_expr]
    }
}

impl ASTNode for IfStatement {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_if_statement(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.conds]
    }
}

impl ASTNode for CallOrIndexed {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_call_or_indexed(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.name, &self.parameters]
    }
}

impl ASTNode for SignalReleaseAssignment {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_signal_release_assignment(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.target]
    }
}

impl ASTNode for Target {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_target(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match self {
            Target::Name(name) => vec![name],
            Target::Aggregate(aggr) => vec![aggr],
        }
    }
}

impl ASTNode for ElementAssociation {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_element_association(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match self {
            ElementAssociation::Positional(expr) => vec![expr],
            ElementAssociation::Named(choices, expr) => vec![choices, expr],
        }
    }
}

impl ASTNode for SignalForceAssignment {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_signal_force_assignment(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.rhs]
    }
}

impl ASTNode for SignalAssignment {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_signal_assignment(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.target, &self.delay_mechanism, &self.rhs]
    }
}

impl ASTNode for DelayMechanism {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_delay_mechanism(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match self {
            DelayMechanism::Transport => vec![],
            DelayMechanism::Inertial { reject } => vec![reject],
        }
    }
}

impl ASTNode for Waveform {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_waveform(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match self {
            Waveform::Elements(elements) => vec![elements],
            Waveform::Unaffected => vec![],
        }
    }
}

impl ASTNode for WaveformElement {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_waveform_element(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.value, &self.after]
    }
}

impl ASTNode for VariableAssignment {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_variable_assignment(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.target, &self.rhs]
    }
}

impl ASTNode for ReportStatement {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_report_statement(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.severity, &self.report]
    }
}

impl ASTNode for AssertStatement {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_assert_statement(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.condition, &self.report, &self.severity]
    }
}

impl ASTNode for Choice {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_choice(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match self {
            Choice::Expression(expr) => vec![expr],
            Choice::DiscreteRange(range) => vec![range],
            Choice::Others => vec![],
        }
    }
}

impl ASTNode for WaitStatement {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_wait_statement(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![
            &self.sensitivity_clause,
            &self.condition_clause,
            &self.timeout_clause,
        ]
    }
}

impl ASTNode for FunctionSpecification {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_function_specification(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.parameter_list, &self.header, &self.return_type]
    }
}

impl ASTNode for ProcedureSpecification {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_procedure_specification(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.designator, &self.header, &self.parameter_list]
    }
}

impl ASTNode for SubprogramDesignator {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_subprogram_designator(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![]
    }
}

impl ASTNode for AliasDeclaration {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_alias_declaration(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![
            &self.designator,
            &self.subtype_indication,
            &self.name,
            &self.signature,
        ]
    }
}

impl ASTNode for Attribute {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_attribute(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match self {
            Attribute::Specification(spec) => vec![spec],
            Attribute::Declaration(decl) => vec![decl],
        }
    }
}

impl ASTNode for SubtypeIndication {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_subtype_indication(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.resolution, &self.type_mark, &self.constraint]
    }
}

impl ASTNode for AttributeSpecification {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_attribute_specification(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.ident, &self.entity_name, &self.expr]
    }
}

impl ASTNode for EntityName {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_entity_name(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match self {
            EntityName::Name(name) => vec![name],
            EntityName::All | EntityName::Others => vec![],
        }
    }
}

impl ASTNode for ResolutionIndication {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_resolution_indication(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match self {
            ResolutionIndication::FunctionName(name) => vec![name],
            ResolutionIndication::ArrayElement(name) => vec![name],
            ResolutionIndication::Record(record) => vec![record],
            ResolutionIndication::Unresolved => vec![],
        }
    }
}

impl ASTNode for RecordElementResolution {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_record_element_resolution(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.ident, &self.resolution]
    }
}

impl ASTNode for SubtypeConstraint {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_subtype_constraint(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match self {
            SubtypeConstraint::Range(range) => vec![range],
            SubtypeConstraint::Array(ranges, constraint) => vec![ranges, constraint],
            SubtypeConstraint::Record(constraints) => vec![constraints],
        }
    }
}

impl ASTNode for ElementConstraint {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_element_constraint(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.ident, &self.constraint]
    }
}

impl ASTNode for AttributeDeclaration {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_attribute_declaration(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.ident, &self.type_mark]
    }
}

impl ASTNode for ComponentDeclaration {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_component_declaration(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.ident, &self.generic_list, &self.port_list]
    }
}

impl ASTNode for TypeDeclaration {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_type_declaration(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.ident, &self.def]
    }
}

impl ASTNode for TypeDefinition {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_type_definition(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match self {
            TypeDefinition::Enumeration(literals) => vec![literals],
            TypeDefinition::Numeric(range) => vec![range],
            TypeDefinition::Physical(decl) => vec![decl],
            TypeDefinition::Array(indices, indication) => vec![indices, indication],
            TypeDefinition::Record(record) => vec![record],
            TypeDefinition::Access(subtype) => vec![subtype],
            TypeDefinition::Incomplete(reference) => vec![reference],
            TypeDefinition::File(type_mark) => vec![type_mark],
            TypeDefinition::Protected(decl) => vec![decl],
            TypeDefinition::ProtectedBody(body) => vec![body],
            TypeDefinition::Subtype(subtype) => vec![subtype],
        }
    }
}

impl ASTNode for Reference {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_reference(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![]
    }
}

impl ASTNode for ProtectedTypeBody {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_protected_type_body(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.decl]
    }
}

impl ASTNode for ProtectedTypeDeclaration {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_protected_type_declaration(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.items]
    }
}

impl ASTNode for ProtectedTypeDeclarativeItem {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_protected_type_declarative_item(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match self {
            ProtectedTypeDeclarativeItem::Subprogram(decl) => vec![decl],
        }
    }
}

impl ASTNode for ElementDeclaration {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_element_declaration(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.ident, &self.subtype]
    }
}

impl ASTNode for ArrayIndex {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_array_index(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match self {
            ArrayIndex::IndexSubtypeDefintion(type_mark) => vec![type_mark],
            ArrayIndex::Discrete(range) => vec![range],
        }
    }
}

impl ASTNode for PhysicalTypeDeclaration {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_physical_type_declaration(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.range, &self.primary_unit, &self.secondary_units]
    }
}

impl ASTNode for PhysicalLiteral {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_physical_literal(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.value, &self.unit]
    }
}

impl ASTNode for EnumerationLiteral {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_enumeration_literal(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![]
    }
}

impl ASTNode for FileDeclaration {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_file_declaration(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![
            &self.ident,
            &self.subtype_indication,
            &self.file_name,
            &self.open_info,
        ]
    }
}

impl ASTNode for ObjectDeclaration {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_object_declaration(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.ident, &self.subtype_indication, &self.expression]
    }
}

impl ASTNode for InterfaceDeclaration {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_interface_declaration(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match self {
            InterfaceDeclaration::Object(obj) => vec![obj],
            InterfaceDeclaration::File(obj) => vec![obj],
            InterfaceDeclaration::Type(obj) => vec![obj],
            InterfaceDeclaration::Subprogram(decl, default) => vec![decl, default],
            InterfaceDeclaration::Package(pkg) => vec![pkg],
        }
    }
}

impl ASTNode for InterfaceObjectDeclaration {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_interface_object_declaration(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.ident, &self.subtype_indication, &self.expression]
    }
}

impl ASTNode for InterfaceFileDeclaration {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_interface_file_declaration(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.ident, &self.subtype_indication]
    }
}

impl ASTNode for SubprogramDefault {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_subprogram_default(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match self {
            SubprogramDefault::Name(name) => vec![name],
            SubprogramDefault::Box => vec![],
        }
    }
}

impl ASTNode for InterfacePackageDeclaration {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_interface_package_declaration(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.ident, &self.package_name, &self.generic_map]
    }
}

impl ASTNode for InterfacePackageGenericMapAspect {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_interface_package_generic_map_aspect(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match self {
            InterfacePackageGenericMapAspect::Map(map) => vec![map],
            InterfacePackageGenericMapAspect::Box => vec![],
            InterfacePackageGenericMapAspect::Default => vec![],
        }
    }
}

impl ASTNode for ConfigurationDeclaration {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_configuration_declaration(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![
            &self.context_clause,
            &self.ident,
            &self.entity_name,
            &self.decl,
            &self.vunit_bind_inds,
            &self.block_config,
        ]
    }
}

impl ASTNode for ConfigurationDeclarativeItem {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_configuration_declarative_item(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match self {
            ConfigurationDeclarativeItem::Use(clause) => vec![clause],
        }
    }
}

impl ASTNode for BlockConfiguration {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_block_configuration(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.block_spec, &self.use_clauses, &self.items]
    }
}

impl ASTNode for ConfigurationItem {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_configuration_item(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match self {
            ConfigurationItem::Block(block) => vec![block],
            ConfigurationItem::Component(component) => vec![component],
        }
    }
}

impl ASTNode for ComponentConfiguration {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_component_configuration(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![
            &self.spec,
            &self.bind_ind,
            &self.vunit_bind_inds,
            &self.block_config,
        ]
    }
}

impl ASTNode for EntityDeclaration {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_entity_declaration(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![
            &self.context_clause,
            &self.ident,
            &self.generic_clause,
            &self.port_clause,
            &self.decl,
            &self.statements,
        ]
    }
}

impl ASTNode for AnySecondaryUnit {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_any_secondary_unit(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match self {
            AnySecondaryUnit::Architecture(arch) => vec![arch],
            AnySecondaryUnit::PackageBody(package) => vec![package],
        }
    }
}

impl ASTNode for LabeledConcurrentStatement {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_labeled_concurrent_statement(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.label, &self.statement]
    }
}

impl ASTNode for ConcurrentStatement {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_concurrent_statement(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match &self {
            ConcurrentStatement::ProcedureCall(stmt) => vec![stmt],
            ConcurrentStatement::Block(stmt) => vec![stmt],
            ConcurrentStatement::Process(stmt) => vec![stmt],
            ConcurrentStatement::Assert(stmt) => vec![stmt],
            ConcurrentStatement::Assignment(stmt) => vec![stmt],
            ConcurrentStatement::Instance(stmt) => vec![stmt],
            ConcurrentStatement::ForGenerate(stmt) => vec![stmt],
            ConcurrentStatement::IfGenerate(stmt) => vec![stmt],
            ConcurrentStatement::CaseGenerate(stmt) => vec![stmt],
        }
    }
}

impl ASTNode for CaseGenerateStatement {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_case_generate_statement(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.sels]
    }
}

impl ASTNode for IfGenerateStatement {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_if_generate_statement(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.conds]
    }
}

impl ASTNode for ForGenerateStatement {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_for_generate_statement(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.index_name, &self.discrete_range, &self.body]
    }
}

impl ASTNode for InstantiationStatement {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_instantiation_statement(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.unit, &self.generic_map, &self.port_map]
    }
}

impl ASTNode for GenerateBody {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_generate_body(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.alternative_label, &self.decl, &self.statements]
    }
}

impl ASTNode for InstantiatedUnit {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_instantiated_unit(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match self {
            InstantiatedUnit::Component(component) => vec![component],
            InstantiatedUnit::Entity(name, ident) => vec![name, ident],
            InstantiatedUnit::Configuration(config) => vec![config],
        }
    }
}

impl ASTNode for ConcurrentSignalAssignment {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_concurrent_signal_assignment(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.target, &self.delay_mechanism, &self.rhs]
    }
}

impl ASTNode for ConcurrentAssertStatement {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_concurrent_assert_statement(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.statement]
    }
}

impl ASTNode for ProcessStatement {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_process_statement(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.sensitivity_list, &self.decl, &self.statements]
    }
}

impl ASTNode for BlockStatement {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_block_statement(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![
            &self.guard_condition,
            &self.header,
            &self.decl,
            &self.statements,
        ]
    }
}

impl ASTNode for BlockHeader {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_block_header(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![
            &self.generic_map,
            &self.generic_map,
            &self.port_map,
            &self.port_map,
        ]
    }
}

impl ASTNode for ConcurrentProcedureCall {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_concurrent_procedure_call(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.call]
    }
}

impl ASTNode for PackageBody {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_package_body(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.context_clause, &self.ident, &self.decl]
    }
}

impl ASTNode for SensitivityList {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_sensitivity_list(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match self {
            SensitivityList::Names(names) => vec![names],
            SensitivityList::All => vec![],
        }
    }
}

impl ASTNode for ArchitectureBody {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_architecture_body(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![
            &self.context_clause,
            &self.ident,
            &self.entity_name,
            &self.decl,
            &self.statements,
        ]
    }
}

impl ASTNode for Expression {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_expression(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match self {
            Expression::Binary(_, lhs, rhs) => vec![lhs, rhs],
            Expression::Unary(_, expr) => vec![expr],
            Expression::Aggregate(elements) => vec![elements],
            Expression::Qualified(qual) => vec![qual],
            Expression::Name(name) => vec![name],
            Expression::Literal(lit) => vec![lit],
            Expression::New(allocator) => vec![allocator],
        }
    }
}

impl ASTNode for QualifiedExpression {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_qualified_expression(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.type_mark, &self.expr]
    }
}

impl ASTNode for Allocator {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_allocator(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match self {
            Allocator::Qualified(qual) => vec![qual],
            Allocator::Subtype(subtype) => vec![subtype],
        }
    }
}

impl ASTNode for AttributeDesignator {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_attribute_designator(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![]
    }
}

impl ASTNode for Signature {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_signature(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match self {
            Signature::Function(t1, t2) => vec![t1, t2],
            Signature::Procedure(proc) => vec![proc],
        }
    }
}

impl ASTNode for Name {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_name(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match self {
            Name::Designator(desi) => vec![desi],
            Name::Selected(name, desi) => vec![name, desi],
            Name::SelectedAll(name) => vec![name],
            Name::Slice(name, range) => vec![name, range],
            Name::Attribute(attr) => vec![attr],
            Name::CallOrIndexed(coi) => vec![coi],
            Name::External(external) => vec![external],
        }
    }
}

impl ASTNode for ExternalName {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_external_name(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.path, &self.subtype]
    }
}

impl ASTNode for ExternalPath {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_external_path(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match self {
            ExternalPath::Package(name) => vec![name],
            ExternalPath::Absolute(name) => vec![name],
            ExternalPath::Relative(name, _) => vec![name],
        }
    }
}

impl ASTNode for AttributeName {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_attribute_name(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.name, &self.signature, &self.attr, &self.expr]
    }
}

impl ASTNode for AbstractLiteral {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_abstract_literal(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![]
    }
}

impl ASTNode for Literal {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_literal(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match self {
            Literal::String(_) | Literal::BitString(_) | Literal::Character(_) | Literal::Null => {
                vec![]
            }
            Literal::AbstractLiteral(lit) => vec![lit],
            Literal::Physical(phy) => vec![phy],
        }
    }
}

impl ASTNode for EntityTag {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_entity_tag(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.designator, &self.signature]
    }
}

impl ASTNode for MapAspect {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_map_aspect(self, ctx)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.list]
    }
}
