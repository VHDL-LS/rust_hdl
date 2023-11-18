// This Source Code Form is subject to the terms of the Mozilla Public
// Lic// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// This Source Code Form is subject to the terms of the Mozilla Public
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2023, Olof Kraigher olof.kraigher@gmail.com

use crate::ast::visitor::VisitorResult::{Continue, Skip, Stop};
use crate::ast::*;
use crate::syntax::TokenAccess;
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

    fn visit_subprogram_instantiation(
        &mut self,
        _node: &SubprogramInstantiation,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
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

    /// Populates the stack.
    /// This method pushes a reference to all its children onto the stack.
    /// Doesn't return auxiliary information such as the Tokens.
    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>);
}

pub fn walk(node: &dyn ASTNode, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) {
    let mut stack: Vec<&dyn ASTNode> = vec![node];
    while let Some(node) = stack.pop() {
        match node.visit(visitor, ctx) {
            Stop => return,
            Skip => continue,
            _ => {}
        }
        node.push_stack(&mut stack);
    }
}

pub fn walk_design_file(node: &DesignFile, visitor: &mut dyn Visitor) {
    visitor.visit_design_file(node);
    for (tokens, unit) in &node.design_units {
        walk(unit, visitor, tokens);
    }
}

macro_rules! push {
    ($stack:ident, $el:expr) => {
        $stack.push($el)
    };
    ($stack:ident, $el:expr, $($es:expr),+) => {{
        $stack.push($el);
        push!($stack, $($es),+)
    }};
}

impl<T: ASTNode> ASTNode for Box<T> {
    fn visit(&self, _visitor: &mut dyn Visitor, _ctx: &dyn TokenAccess) -> VisitorResult {
        Continue
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(stack, self.deref());
    }
}

impl<T: ASTNode> ASTNode for Option<T> {
    fn visit(&self, _visitor: &mut dyn Visitor, _ctx: &dyn TokenAccess) -> VisitorResult {
        Continue
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        match &self {
            None => {}
            Some(el) => push!(stack, el),
        }
    }
}

impl<T: ASTNode, U: ASTNode> ASTNode for (T, U) {
    fn visit(&self, _visitor: &mut dyn Visitor, _ctx: &dyn TokenAccess) -> VisitorResult {
        Continue
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(stack, &self.0, &self.1)
    }
}

impl<T: ASTNode> ASTNode for Vec<T> {
    fn visit(&self, _visitor: &mut dyn Visitor, _ctx: &dyn TokenAccess) -> VisitorResult {
        Continue
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        stack.extend(self.iter().rev().map(|f| f as &dyn ASTNode))
    }
}

impl<T: ASTNode> ASTNode for WithPos<T> {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_item_with_pos(&self.pos, &self.item, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(stack, &self.item)
    }
}

impl<T: ASTNode> ASTNode for WithDecl<T> {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_item_with_decl(&self.decl, &self.tree, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(stack, &self.tree)
    }
}

impl<T: ASTNode> ASTNode for WithRef<T> {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_item_with_reference(&self.reference, &self.item, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(stack, &self.reference, &self.item)
    }
}

impl<T: ASTNode> ASTNode for Conditional<T> {
    fn visit(&self, _visitor: &mut dyn Visitor, _ctx: &dyn TokenAccess) -> VisitorResult {
        Continue
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(stack, &self.condition, &self.item)
    }
}

impl<T: ASTNode> ASTNode for Conditionals<T> {
    fn visit(&self, _visitor: &mut dyn Visitor, _ctx: &dyn TokenAccess) -> VisitorResult {
        Continue
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(stack, &self.conditionals, &self.else_item)
    }
}

impl<T: ASTNode> ASTNode for Selection<T> {
    fn visit(&self, _visitor: &mut dyn Visitor, _ctx: &dyn TokenAccess) -> VisitorResult {
        Continue
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(stack, &self.alternatives, &self.expression)
    }
}

impl<T: ASTNode> ASTNode for AssignmentRightHand<T> {
    fn visit(&self, _visitor: &mut dyn Visitor, _ctx: &dyn TokenAccess) -> VisitorResult {
        Continue
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        match &self {
            AssignmentRightHand::Simple(expr) => push!(stack, expr),
            AssignmentRightHand::Conditional(conds) => push!(stack, conds),
            AssignmentRightHand::Selected(sel) => push!(stack, sel),
        }
    }
}

impl<T: ASTNode> ASTNode for Alternative<T> {
    fn visit(&self, _visitor: &mut dyn Visitor, _ctx: &dyn TokenAccess) -> VisitorResult {
        Continue
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(stack, &self.choices, &self.item)
    }
}

impl ASTNode for DesignFile {
    fn visit(&self, visitor: &mut dyn Visitor, _ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_design_file(self)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        stack.extend(
            self.design_units
                .iter()
                .rev()
                .map(|it| &it.1 as &dyn ASTNode),
        )
    }
}

impl ASTNode for AnyDesignUnit {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_any_design_unit(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        match self {
            AnyDesignUnit::Primary(unit) => push!(stack, unit),
            AnyDesignUnit::Secondary(unit) => push!(stack, unit),
        }
    }
}

impl ASTNode for AnyPrimaryUnit {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_any_primary_unit(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        match self {
            AnyPrimaryUnit::Entity(decl) => push!(stack, decl),
            AnyPrimaryUnit::Configuration(decl) => push!(stack, decl),
            AnyPrimaryUnit::Package(decl) => push!(stack, decl),
            AnyPrimaryUnit::PackageInstance(decl) => push!(stack, decl),
            AnyPrimaryUnit::Context(decl) => push!(stack, decl),
        }
    }
}

impl ASTNode for ContextDeclaration {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_context_declaration(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(stack, &self.ident, &self.items)
    }
}

impl ASTNode for ContextItem {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_context_item(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        match self {
            ContextItem::Use(clause) => push!(stack, clause),
            ContextItem::Library(clause) => push!(stack, clause),
            ContextItem::Context(clause) => push!(stack, clause),
        }
    }
}

impl ASTNode for ContextReference {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_context_reference(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(stack, &self.name_list)
    }
}

impl<T: ASTNode> ASTNode for SeparatedList<T> {
    fn visit(&self, _visitor: &mut dyn Visitor, _ctx: &dyn TokenAccess) -> VisitorResult {
        Continue
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        stack.extend(self.items.iter().rev().map(|it| it as &dyn ASTNode))
    }
}

impl ASTNode for LibraryClause {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_library_clause(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(stack, &self.name_list)
    }
}

impl ASTNode for UseClause {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_use_clause(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(stack, &self.name_list)
    }
}

impl ASTNode for Ident {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_ident(self, ctx)
    }

    fn push_stack(&self, _stack: &mut Vec<&dyn ASTNode>) {}
}

impl ASTNode for PackageInstantiation {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_package_instantiation(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(
            stack,
            &self.context_clause,
            &self.ident,
            &self.package_name,
            &self.generic_map
        )
    }
}

impl ASTNode for AssociationElement {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_association_element(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(stack, &self.formal, &self.actual)
    }
}

impl ASTNode for ActualPart {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_actual_part(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        match self {
            ActualPart::Expression(expr) => push!(stack, expr),
            ActualPart::Open => {}
        }
    }
}

impl ASTNode for SelectedName {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_selected_name(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        match self {
            SelectedName::Designator(desi) => push!(stack, desi),
            SelectedName::Selected(name, desi) => push!(stack, name, desi),
        }
    }
}

impl ASTNode for Designator {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_designator(self, ctx)
    }

    fn push_stack<'a>(&'a self, _stack: &mut Vec<&'a dyn ASTNode>) {}
}

impl ASTNode for PackageDeclaration {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_package_declaration(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(
            stack,
            &self.context_clause,
            &self.ident,
            &self.generic_clause,
            &self.decl
        )
    }
}

impl ASTNode for Declaration {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_declaration(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        match self {
            Declaration::Object(decl) => push!(stack, decl),
            Declaration::File(decl) => push!(stack, decl),
            Declaration::Type(decl) => push!(stack, decl),
            Declaration::Component(decl) => push!(stack, decl),
            Declaration::Attribute(decl) => push!(stack, decl),
            Declaration::Alias(decl) => push!(stack, decl),
            Declaration::SubprogramDeclaration(decl) => push!(stack, decl),
            Declaration::SubprogramBody(decl) => push!(stack, decl),
            Declaration::Use(decl) => push!(stack, decl),
            Declaration::Package(decl) => push!(stack, decl),
            Declaration::Configuration(decl) => push!(stack, decl),
            Declaration::SubprogramInstantiation(decl) => push!(stack, decl),
        }
    }
}

impl ASTNode for SubprogramInstantiation {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_subprogram_instantiation(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(
            stack,
            &self.ident,
            &self.subprogram_name,
            &self.signature,
            &self.generic_map
        )
    }
}

impl ASTNode for ConfigurationSpecification {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_configuration_specification(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(stack, &self.spec, &self.bind_ind, &self.vunit_bind_inds)
    }
}

impl ASTNode for VUnitBindingIndication {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_v_unit_binding_indication(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(stack, &self.vunit_list)
    }
}

impl ASTNode for BindingIndication {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_binding_indication(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(
            stack,
            &self.entity_aspect,
            &self.generic_map,
            &self.port_map
        )
    }
}

impl ASTNode for EntityAspect {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_entity_aspect(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        match self {
            EntityAspect::Entity(name, id) => push!(stack, name, id),
            EntityAspect::Configuration(config) => push!(stack, config),
            EntityAspect::Open => {}
        }
    }
}

impl ASTNode for ComponentSpecification {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_component_specification(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(stack, &self.instantiation_list, &self.component_name)
    }
}

impl ASTNode for InstantiationList {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_instantiation_list(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        match self {
            InstantiationList::Labels(idents) => push!(stack, idents),
            InstantiationList::Others => {}
            InstantiationList::All => {}
        }
    }
}

impl ASTNode for SubprogramBody {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_subprogram_body(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(
            stack,
            &self.specification,
            &self.declarations,
            &self.statements
        )
    }
}

impl ASTNode for SubprogramHeader {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_subprogram_header(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(stack, &self.map_aspect, &self.generic_list)
    }
}

impl ASTNode for LabeledSequentialStatement {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_labeled_sequential_statement(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(stack, &self.label, &self.statement)
    }
}

impl ASTNode for SubprogramSpecification {
    fn visit(&self, _visitor: &mut dyn Visitor, _ctx: &dyn TokenAccess) -> VisitorResult {
        Continue
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        match self {
            SubprogramSpecification::Procedure(proc) => push!(stack, proc),
            SubprogramSpecification::Function(func) => push!(stack, func),
        }
    }
}

impl ASTNode for SubprogramDeclaration {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_subprogram_declaration(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(stack, &self.specification)
    }
}

impl ASTNode for SequentialStatement {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_sequential_statement(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        match self {
            SequentialStatement::Wait(stmt) => push!(stack, stmt),
            SequentialStatement::Assert(stmt) => push!(stack, stmt),
            SequentialStatement::Report(stmt) => push!(stack, stmt),
            SequentialStatement::VariableAssignment(stmt) => push!(stack, stmt),
            SequentialStatement::SignalAssignment(stmt) => push!(stack, stmt),
            SequentialStatement::SignalForceAssignment(stmt) => push!(stack, stmt),
            SequentialStatement::SignalReleaseAssignment(stmt) => push!(stack, stmt),
            SequentialStatement::ProcedureCall(stmt) => push!(stack, stmt),
            SequentialStatement::If(stmt) => push!(stack, stmt),
            SequentialStatement::Case(stmt) => push!(stack, stmt),
            SequentialStatement::Loop(stmt) => push!(stack, stmt),
            SequentialStatement::Next(stmt) => push!(stack, stmt),
            SequentialStatement::Exit(stmt) => push!(stack, stmt),
            SequentialStatement::Return(stmt) => push!(stack, stmt),
            SequentialStatement::Null => {}
        }
    }
}

impl ASTNode for CaseStatement {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_case_statement(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(stack, &self.expression, &self.alternatives)
    }
}

impl ASTNode for ReturnStatement {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_return_statement(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        match &self.expression {
            Some(expr) => push!(stack, expr),
            None => {}
        }
    }
}

impl ASTNode for ExitStatement {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_exit_statement(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(stack, &self.loop_label, &self.condition)
    }
}

impl ASTNode for NextStatement {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_next_statement(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(stack, &self.loop_label, &self.condition)
    }
}

impl ASTNode for LoopStatement {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_loop_statement(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(stack, &self.iteration_scheme, &self.statements)
    }
}

impl ASTNode for IterationScheme {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_iteration_scheme(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        match self {
            IterationScheme::While(scheme) => push!(stack, scheme),
            IterationScheme::For(ident, range) => push!(stack, ident, range),
        }
    }
}

impl ASTNode for DiscreteRange {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_discrete_range(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        match self {
            DiscreteRange::Discrete(type_mark, range) => push!(stack, type_mark, range),
            DiscreteRange::Range(range) => push!(stack, range),
        }
    }
}

impl ASTNode for TypeMark {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_type_mark(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(stack, &self.name)
    }
}

impl ASTNode for Range {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_range(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        match self {
            Range::Range(constraint) => push!(stack, constraint),
            Range::Attribute(attr) => push!(stack, attr.deref()),
        }
    }
}

impl ASTNode for RangeConstraint {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_range_constraint(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(stack, &self.left_expr, &self.right_expr)
    }
}

impl ASTNode for IfStatement {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_if_statement(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(stack, &self.conds)
    }
}

impl ASTNode for CallOrIndexed {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_call_or_indexed(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(stack, &self.name, &self.parameters)
    }
}

impl ASTNode for SignalReleaseAssignment {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_signal_release_assignment(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(stack, &self.target)
    }
}

impl ASTNode for Target {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_target(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        match self {
            Target::Name(name) => push!(stack, name),
            Target::Aggregate(aggr) => push!(stack, aggr),
        }
    }
}

impl ASTNode for ElementAssociation {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_element_association(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        match self {
            ElementAssociation::Positional(expr) => push!(stack, expr),
            ElementAssociation::Named(choices, expr) => push!(stack, choices, expr),
        }
    }
}

impl ASTNode for SignalForceAssignment {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_signal_force_assignment(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(stack, &self.rhs)
    }
}

impl ASTNode for SignalAssignment {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_signal_assignment(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(stack, &self.target, &self.delay_mechanism, &self.rhs)
    }
}

impl ASTNode for DelayMechanism {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_delay_mechanism(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        match self {
            DelayMechanism::Transport => {}
            DelayMechanism::Inertial { reject } => push!(stack, reject),
        }
    }
}

impl ASTNode for Waveform {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_waveform(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        match self {
            Waveform::Elements(elements) => push!(stack, elements),
            Waveform::Unaffected => {}
        }
    }
}

impl ASTNode for WaveformElement {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_waveform_element(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(stack, &self.value, &self.after)
    }
}

impl ASTNode for VariableAssignment {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_variable_assignment(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(stack, &self.target, &self.rhs)
    }
}

impl ASTNode for ReportStatement {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_report_statement(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(stack, &self.severity, &self.report)
    }
}

impl ASTNode for AssertStatement {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_assert_statement(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(stack, &self.condition, &self.report, &self.severity)
    }
}

impl ASTNode for Choice {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_choice(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        match self {
            Choice::Expression(expr) => push!(stack, expr),
            Choice::DiscreteRange(range) => push!(stack, range),
            Choice::Others => {}
        }
    }
}

impl ASTNode for WaitStatement {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_wait_statement(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(
            stack,
            &self.sensitivity_clause,
            &self.condition_clause,
            &self.timeout_clause
        )
    }
}

impl ASTNode for FunctionSpecification {
    fn visit(&self, _visitor: &mut dyn Visitor, _ctx: &dyn TokenAccess) -> VisitorResult {
        Continue
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(
            stack,
            &self.designator,
            &self.parameter_list,
            &self.header,
            &self.return_type
        )
    }
}

impl ASTNode for ProcedureSpecification {
    fn visit(&self, _visitor: &mut dyn Visitor, _ctx: &dyn TokenAccess) -> VisitorResult {
        Continue
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(stack, &self.designator, &self.header, &self.parameter_list)
    }
}

impl ASTNode for SubprogramDesignator {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_subprogram_designator(self, ctx)
    }

    fn push_stack<'a>(&'a self, _stack: &mut Vec<&'a dyn ASTNode>) {}
}

impl ASTNode for AliasDeclaration {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_alias_declaration(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(
            stack,
            &self.designator,
            &self.subtype_indication,
            &self.name,
            &self.signature
        )
    }
}

impl ASTNode for Attribute {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_attribute(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        match self {
            Attribute::Specification(spec) => push!(stack, spec),
            Attribute::Declaration(decl) => push!(stack, decl),
        }
    }
}

impl ASTNode for SubtypeIndication {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_subtype_indication(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(stack, &self.resolution, &self.type_mark, &self.constraint)
    }
}

impl ASTNode for AttributeSpecification {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_attribute_specification(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(stack, &self.ident, &self.entity_name, &self.expr)
    }
}

impl ASTNode for EntityName {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_entity_name(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        match self {
            EntityName::Name(name) => push!(stack, name),
            EntityName::All | EntityName::Others => {}
        }
    }
}

impl ASTNode for ResolutionIndication {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_resolution_indication(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        match self {
            ResolutionIndication::FunctionName(name) => push!(stack, name),
            ResolutionIndication::ArrayElement(name) => push!(stack, name),
            ResolutionIndication::Record(record) => push!(stack, record),
            ResolutionIndication::Unresolved => {}
        }
    }
}

impl ASTNode for RecordElementResolution {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_record_element_resolution(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(stack, &self.ident, &self.resolution)
    }
}

impl ASTNode for SubtypeConstraint {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_subtype_constraint(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        match self {
            SubtypeConstraint::Range(range) => push!(stack, range),
            SubtypeConstraint::Array(ranges, constraint) => push!(stack, ranges, constraint),
            SubtypeConstraint::Record(constraints) => push!(stack, constraints),
        }
    }
}

impl ASTNode for ElementConstraint {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_element_constraint(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(stack, &self.ident, &self.constraint)
    }
}

impl ASTNode for AttributeDeclaration {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_attribute_declaration(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(stack, &self.ident, &self.type_mark)
    }
}

impl ASTNode for ComponentDeclaration {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_component_declaration(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(stack, &self.ident, &self.generic_list, &self.port_list)
    }
}

impl ASTNode for TypeDeclaration {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_type_declaration(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(stack, &self.ident, &self.def)
    }
}

impl ASTNode for TypeDefinition {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_type_definition(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        match self {
            TypeDefinition::Enumeration(literals) => push!(stack, literals),
            TypeDefinition::Numeric(range) => push!(stack, range),
            TypeDefinition::Physical(decl) => push!(stack, decl),
            TypeDefinition::Array(indices, indication) => push!(stack, indices, indication),
            TypeDefinition::Record(record) => push!(stack, record),
            TypeDefinition::Access(subtype) => push!(stack, subtype),
            TypeDefinition::Incomplete(reference) => push!(stack, reference),
            TypeDefinition::File(type_mark) => push!(stack, type_mark),
            TypeDefinition::Protected(decl) => push!(stack, decl),
            TypeDefinition::ProtectedBody(body) => push!(stack, body),
            TypeDefinition::Subtype(subtype) => push!(stack, subtype),
        }
    }
}

impl ASTNode for Reference {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_reference(self, ctx)
    }

    fn push_stack<'a>(&'a self, _stack: &mut Vec<&'a dyn ASTNode>) {}
}

impl ASTNode for ProtectedTypeBody {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_protected_type_body(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(stack, &self.decl)
    }
}

impl ASTNode for ProtectedTypeDeclaration {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_protected_type_declaration(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(stack, &self.items)
    }
}

impl ASTNode for ProtectedTypeDeclarativeItem {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_protected_type_declarative_item(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        match self {
            ProtectedTypeDeclarativeItem::Subprogram(decl) => push!(stack, decl),
        }
    }
}

impl ASTNode for ElementDeclaration {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_element_declaration(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(stack, &self.ident, &self.subtype)
    }
}

impl ASTNode for ArrayIndex {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_array_index(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        match self {
            ArrayIndex::IndexSubtypeDefintion(type_mark) => push!(stack, type_mark),
            ArrayIndex::Discrete(range) => push!(stack, range),
        }
    }
}

impl ASTNode for PhysicalTypeDeclaration {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_physical_type_declaration(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(
            stack,
            &self.range,
            &self.primary_unit,
            &self.secondary_units
        )
    }
}

impl ASTNode for PhysicalLiteral {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_physical_literal(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(stack, &self.value, &self.unit)
    }
}

impl ASTNode for EnumerationLiteral {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_enumeration_literal(self, ctx)
    }

    fn push_stack<'a>(&'a self, _stack: &mut Vec<&'a dyn ASTNode>) {}
}

impl ASTNode for FileDeclaration {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_file_declaration(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(
            stack,
            &self.ident,
            &self.subtype_indication,
            &self.file_name,
            &self.open_info
        )
    }
}

impl ASTNode for ObjectDeclaration {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_object_declaration(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(
            stack,
            &self.ident,
            &self.subtype_indication,
            &self.expression
        )
    }
}

impl ASTNode for InterfaceDeclaration {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_interface_declaration(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        match self {
            InterfaceDeclaration::Object(obj) => push!(stack, obj),
            InterfaceDeclaration::File(obj) => push!(stack, obj),
            InterfaceDeclaration::Type(obj) => push!(stack, obj),
            InterfaceDeclaration::Subprogram(decl, default) => push!(stack, decl, default),
            InterfaceDeclaration::Package(pkg) => push!(stack, pkg),
        }
    }
}

impl ASTNode for InterfaceObjectDeclaration {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_interface_object_declaration(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(
            stack,
            &self.ident,
            &self.subtype_indication,
            &self.expression
        )
    }
}

impl ASTNode for InterfaceFileDeclaration {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_interface_file_declaration(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(stack, &self.ident, &self.subtype_indication)
    }
}

impl ASTNode for SubprogramDefault {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_subprogram_default(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        match self {
            SubprogramDefault::Name(name) => push!(stack, name),
            SubprogramDefault::Box => {}
        }
    }
}

impl ASTNode for InterfacePackageDeclaration {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_interface_package_declaration(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(stack, &self.ident, &self.package_name, &self.generic_map)
    }
}

impl ASTNode for InterfacePackageGenericMapAspect {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_interface_package_generic_map_aspect(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        match self {
            InterfacePackageGenericMapAspect::Map(map) => push!(stack, map),
            InterfacePackageGenericMapAspect::Box => {}
            InterfacePackageGenericMapAspect::Default => {}
        }
    }
}

impl ASTNode for ConfigurationDeclaration {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_configuration_declaration(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(
            stack,
            &self.context_clause,
            &self.ident,
            &self.entity_name,
            &self.decl,
            &self.vunit_bind_inds,
            &self.block_config
        )
    }
}

impl ASTNode for ConfigurationDeclarativeItem {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_configuration_declarative_item(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        match self {
            ConfigurationDeclarativeItem::Use(clause) => push!(stack, clause),
        }
    }
}

impl ASTNode for BlockConfiguration {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_block_configuration(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(stack, &self.block_spec, &self.use_clauses, &self.items)
    }
}

impl ASTNode for ConfigurationItem {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_configuration_item(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        match self {
            ConfigurationItem::Block(block) => push!(stack, block),
            ConfigurationItem::Component(component) => push!(stack, component),
        }
    }
}

impl ASTNode for ComponentConfiguration {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_component_configuration(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(
            stack,
            &self.spec,
            &self.bind_ind,
            &self.vunit_bind_inds,
            &self.block_config
        )
    }
}

impl ASTNode for EntityDeclaration {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_entity_declaration(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(
            stack,
            &self.context_clause,
            &self.ident,
            &self.generic_clause,
            &self.port_clause,
            &self.decl,
            &self.statements
        )
    }
}

impl ASTNode for AnySecondaryUnit {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_any_secondary_unit(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        match self {
            AnySecondaryUnit::Architecture(arch) => push!(stack, arch),
            AnySecondaryUnit::PackageBody(package) => push!(stack, package),
        }
    }
}

impl ASTNode for LabeledConcurrentStatement {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_labeled_concurrent_statement(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(stack, &self.label, &self.statement)
    }
}

impl ASTNode for ConcurrentStatement {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_concurrent_statement(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        match &self {
            ConcurrentStatement::ProcedureCall(stmt) => push!(stack, stmt),
            ConcurrentStatement::Block(stmt) => push!(stack, stmt),
            ConcurrentStatement::Process(stmt) => push!(stack, stmt),
            ConcurrentStatement::Assert(stmt) => push!(stack, stmt),
            ConcurrentStatement::Assignment(stmt) => push!(stack, stmt),
            ConcurrentStatement::Instance(stmt) => push!(stack, stmt),
            ConcurrentStatement::ForGenerate(stmt) => push!(stack, stmt),
            ConcurrentStatement::IfGenerate(stmt) => push!(stack, stmt),
            ConcurrentStatement::CaseGenerate(stmt) => push!(stack, stmt),
        }
    }
}

impl ASTNode for CaseGenerateStatement {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_case_generate_statement(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(stack, &self.sels)
    }
}

impl ASTNode for IfGenerateStatement {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_if_generate_statement(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(stack, &self.conds)
    }
}

impl ASTNode for ForGenerateStatement {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_for_generate_statement(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(stack, &self.index_name, &self.discrete_range, &self.body)
    }
}

impl ASTNode for InstantiationStatement {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_instantiation_statement(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(stack, &self.unit, &self.generic_map, &self.port_map)
    }
}

impl ASTNode for GenerateBody {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_generate_body(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(stack, &self.alternative_label, &self.decl, &self.statements)
    }
}

impl ASTNode for InstantiatedUnit {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_instantiated_unit(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        match self {
            InstantiatedUnit::Component(component) => push!(stack, component),
            InstantiatedUnit::Entity(name, ident) => push!(stack, name, ident),
            InstantiatedUnit::Configuration(config) => push!(stack, config),
        }
    }
}

impl ASTNode for ConcurrentSignalAssignment {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_concurrent_signal_assignment(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(stack, &self.target, &self.delay_mechanism, &self.rhs)
    }
}

impl ASTNode for ConcurrentAssertStatement {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_concurrent_assert_statement(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(stack, &self.statement)
    }
}

impl ASTNode for ProcessStatement {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_process_statement(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(stack, &self.sensitivity_list, &self.decl, &self.statements)
    }
}

impl ASTNode for BlockStatement {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_block_statement(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(
            stack,
            &self.guard_condition,
            &self.header,
            &self.decl,
            &self.statements
        )
    }
}

impl ASTNode for BlockHeader {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_block_header(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(
            stack,
            &self.generic_map,
            &self.generic_map,
            &self.port_map,
            &self.port_map
        )
    }
}

impl ASTNode for ConcurrentProcedureCall {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_concurrent_procedure_call(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(stack, &self.call)
    }
}

impl ASTNode for PackageBody {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_package_body(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(stack, &self.context_clause, &self.ident, &self.decl)
    }
}

impl ASTNode for SensitivityList {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_sensitivity_list(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        match self {
            SensitivityList::Names(names) => push!(stack, names),
            SensitivityList::All => {}
        }
    }
}

impl ASTNode for ArchitectureBody {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_architecture_body(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(
            stack,
            &self.context_clause,
            &self.ident,
            &self.entity_name,
            &self.decl,
            &self.statements
        )
    }
}

impl ASTNode for Expression {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_expression(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        match self {
            Expression::Binary(_, lhs, rhs) => push!(stack, lhs, rhs),
            Expression::Unary(_, expr) => push!(stack, expr),
            Expression::Aggregate(elements) => push!(stack, elements),
            Expression::Qualified(qual) => push!(stack, qual),
            Expression::Name(name) => push!(stack, name),
            Expression::Literal(lit) => push!(stack, lit),
            Expression::New(allocator) => push!(stack, allocator),
        }
    }
}

impl ASTNode for QualifiedExpression {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_qualified_expression(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(stack, &self.type_mark, &self.expr)
    }
}

impl ASTNode for Allocator {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_allocator(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        match self {
            Allocator::Qualified(qual) => push!(stack, qual),
            Allocator::Subtype(subtype) => push!(stack, subtype),
        }
    }
}

impl ASTNode for AttributeDesignator {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_attribute_designator(self, ctx)
    }

    fn push_stack<'a>(&'a self, _stack: &mut Vec<&'a dyn ASTNode>) {}
}

impl ASTNode for Signature {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_signature(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        match self {
            Signature::Function(t1, t2) => push!(stack, t1, t2),
            Signature::Procedure(proc) => push!(stack, proc),
        }
    }
}

impl ASTNode for Name {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_name(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        match self {
            Name::Designator(desi) => push!(stack, desi),
            Name::Selected(name, desi) => push!(stack, name, desi),
            Name::SelectedAll(name) => push!(stack, name),
            Name::Slice(name, range) => push!(stack, name, range),
            Name::Attribute(attr) => push!(stack, attr),
            Name::CallOrIndexed(coi) => push!(stack, coi),
            Name::External(external) => push!(stack, external),
        }
    }
}

impl ASTNode for ExternalName {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_external_name(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(stack, &self.path, &self.subtype)
    }
}

impl ASTNode for ExternalPath {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_external_path(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        match self {
            ExternalPath::Package(name) => push!(stack, name),
            ExternalPath::Absolute(name) => push!(stack, name),
            ExternalPath::Relative(name, _) => push!(stack, name),
        }
    }
}

impl ASTNode for AttributeName {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_attribute_name(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(stack, &self.name, &self.signature, &self.attr, &self.expr)
    }
}

impl ASTNode for AbstractLiteral {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_abstract_literal(self, ctx)
    }

    fn push_stack<'a>(&'a self, _stack: &mut Vec<&'a dyn ASTNode>) {}
}

impl ASTNode for Literal {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_literal(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        match self {
            Literal::String(_) | Literal::BitString(_) | Literal::Character(_) | Literal::Null => {}
            Literal::AbstractLiteral(lit) => push!(stack, lit),
            Literal::Physical(phy) => push!(stack, phy),
        }
    }
}

impl ASTNode for EntityTag {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_entity_tag(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(stack, &self.designator, &self.signature)
    }
}

impl ASTNode for MapAspect {
    fn visit(&self, visitor: &mut dyn Visitor, ctx: &dyn TokenAccess) -> VisitorResult {
        visitor.visit_map_aspect(self, ctx)
    }

    fn push_stack<'a>(&'a self, stack: &mut Vec<&'a dyn ASTNode>) {
        push!(stack, &self.list)
    }
}
