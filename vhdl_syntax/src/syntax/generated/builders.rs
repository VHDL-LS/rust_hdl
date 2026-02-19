// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2026, Lukas Scheller lukasscheller@icloud.com
use super::*;
use crate::parser::builder::NodeBuilder;
use crate::syntax::node::SyntaxNode;
use crate::syntax::node_kind::NodeKind;
use crate::syntax::AstNode;
use crate::tokens::{Keyword as Kw, Token, TokenKind, Trivia, TriviaPiece};
pub struct AbsolutePathnameBuilder {
    dot_token: Token,
    partial_pathname: PartialPathnameSyntax,
}
impl Default for AbsolutePathnameBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl AbsolutePathnameBuilder {
    pub fn new() -> Self {
        Self {
            dot_token: Token::new(
                TokenKind::Dot,
                TokenKind::Dot.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            partial_pathname: PartialPathnameBuilder::default().build(),
        }
    }
    pub fn with_dot_token(mut self, t: Token) -> Self {
        self.dot_token = t;
        self
    }
    pub fn with_partial_pathname(mut self, n: PartialPathnameSyntax) -> Self {
        self.partial_pathname = n;
        self
    }
    pub fn build(self) -> AbsolutePathnameSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::AbsolutePathname);
        builder.push(self.dot_token);
        builder.push_node(self.partial_pathname.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        AbsolutePathnameSyntax::cast(node).unwrap()
    }
}
pub struct AccessTypeDefinitionBuilder {
    access_token: Token,
    subtype_indication: SubtypeIndicationSyntax,
}
impl AccessTypeDefinitionBuilder {
    pub fn new(subtype_indication: SubtypeIndicationSyntax) -> Self {
        Self {
            access_token: Token::new(
                TokenKind::Keyword(Kw::Access),
                Kw::Access.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            subtype_indication,
        }
    }
    pub fn with_access_token(mut self, t: Token) -> Self {
        self.access_token = t;
        self
    }
    pub fn with_subtype_indication(mut self, n: SubtypeIndicationSyntax) -> Self {
        self.subtype_indication = n;
        self
    }
    pub fn build(self) -> AccessTypeDefinitionSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::AccessTypeDefinition);
        builder.push(self.access_token);
        builder.push_node(self.subtype_indication.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        AccessTypeDefinitionSyntax::cast(node).unwrap()
    }
}
pub struct ActualPartBuilder {}
impl Default for ActualPartBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl ActualPartBuilder {
    pub fn new() -> Self {
        Self {}
    }
    pub fn build(self) -> ActualPartSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ActualPart);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ActualPartSyntax::cast(node).unwrap()
    }
}
pub struct AggregateBuilder {
    left_par_token: Token,
    element_associations: Vec<ElementAssociationSyntax>,
    comma_token: Vec<Token>,
    right_par_token: Token,
}
impl Default for AggregateBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl AggregateBuilder {
    pub fn new() -> Self {
        Self {
            left_par_token: Token::new(
                TokenKind::LeftPar,
                TokenKind::LeftPar.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            element_associations: Vec::new(),
            comma_token: Vec::new(),
            right_par_token: Token::new(
                TokenKind::RightPar,
                TokenKind::RightPar.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_left_par_token(mut self, t: Token) -> Self {
        self.left_par_token = t;
        self
    }
    pub fn add_element_associations(mut self, n: ElementAssociationSyntax) -> Self {
        self.element_associations.push(n);
        self
    }
    pub fn add_comma_token(mut self, t: Token) -> Self {
        self.comma_token.push(t);
        self
    }
    pub fn with_right_par_token(mut self, t: Token) -> Self {
        self.right_par_token = t;
        self
    }
    pub fn build(self) -> AggregateSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::Aggregate);
        builder.push(self.left_par_token);
        for n in self.element_associations {
            builder.push_node(n.raw().green().clone());
        }
        for t in self.comma_token {
            builder.push(t);
        }
        builder.push(self.right_par_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        AggregateSyntax::cast(node).unwrap()
    }
}
pub struct AggregateTargetBuilder {
    aggregate: AggregateSyntax,
}
impl Default for AggregateTargetBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl AggregateTargetBuilder {
    pub fn new() -> Self {
        Self {
            aggregate: AggregateBuilder::default().build(),
        }
    }
    pub fn with_aggregate(mut self, n: AggregateSyntax) -> Self {
        self.aggregate = n;
        self
    }
    pub fn build(self) -> AggregateTargetSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::AggregateTarget);
        builder.push_node(self.aggregate.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        AggregateTargetSyntax::cast(node).unwrap()
    }
}
pub struct AliasDeclarationBuilder {
    alias_token: Token,
    alias_designator: AliasDesignatorSyntax,
    colon_token: Option<Token>,
    subtype_indication: Option<SubtypeIndicationSyntax>,
    is_token: Token,
    name: NameSyntax,
    signature: Option<SignatureSyntax>,
    semi_colon_token: Token,
}
impl AliasDeclarationBuilder {
    pub fn new(alias_designator: AliasDesignatorSyntax, name: NameSyntax) -> Self {
        Self {
            alias_token: Token::new(
                TokenKind::Keyword(Kw::Alias),
                Kw::Alias.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            alias_designator,
            colon_token: None,
            subtype_indication: None,
            is_token: Token::new(
                TokenKind::Keyword(Kw::Is),
                Kw::Is.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            name,
            signature: None,
            semi_colon_token: Token::new(
                TokenKind::SemiColon,
                TokenKind::SemiColon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_alias_token(mut self, t: Token) -> Self {
        self.alias_token = t;
        self
    }
    pub fn with_alias_designator(mut self, n: AliasDesignatorSyntax) -> Self {
        self.alias_designator = n;
        self
    }
    pub fn with_colon_token(mut self, t: Token) -> Self {
        self.colon_token = Some(t);
        self
    }
    pub fn with_subtype_indication(mut self, n: SubtypeIndicationSyntax) -> Self {
        self.subtype_indication = Some(n);
        self
    }
    pub fn with_is_token(mut self, t: Token) -> Self {
        self.is_token = t;
        self
    }
    pub fn with_name(mut self, n: NameSyntax) -> Self {
        self.name = n;
        self
    }
    pub fn with_signature(mut self, n: SignatureSyntax) -> Self {
        self.signature = Some(n);
        self
    }
    pub fn with_semi_colon_token(mut self, t: Token) -> Self {
        self.semi_colon_token = t;
        self
    }
    pub fn build(self) -> AliasDeclarationSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::AliasDeclaration);
        builder.push(self.alias_token);
        builder.push(self.alias_designator.raw().token().clone());
        if let Some(t) = self.colon_token {
            builder.push(t);
        }
        if let Some(n) = self.subtype_indication {
            builder.push_node(n.raw().green().clone());
        }
        builder.push(self.is_token);
        builder.push_node(self.name.raw().green().clone());
        if let Some(n) = self.signature {
            builder.push_node(n.raw().green().clone());
        }
        builder.push(self.semi_colon_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        AliasDeclarationSyntax::cast(node).unwrap()
    }
}
pub struct AllSensitivityListBuilder {
    all_token: Token,
}
impl Default for AllSensitivityListBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl AllSensitivityListBuilder {
    pub fn new() -> Self {
        Self {
            all_token: Token::new(
                TokenKind::Keyword(Kw::All),
                Kw::All.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_all_token(mut self, t: Token) -> Self {
        self.all_token = t;
        self
    }
    pub fn build(self) -> AllSensitivityListSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::AllSensitivityList);
        builder.push(self.all_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        AllSensitivityListSyntax::cast(node).unwrap()
    }
}
pub struct ArchitectureBodyBuilder {
    architecture_preamble: ArchitecturePreambleSyntax,
    declarations: DeclarationsSyntax,
    declaration_statement_separator: DeclarationStatementSeparatorSyntax,
    concurrent_statements: ConcurrentStatementsSyntax,
    architecture_epilogue: ArchitectureEpilogueSyntax,
}
impl ArchitectureBodyBuilder {
    pub fn new(architecture_preamble: ArchitecturePreambleSyntax) -> Self {
        Self {
            architecture_preamble,
            declarations: DeclarationsBuilder::default().build(),
            declaration_statement_separator: DeclarationStatementSeparatorBuilder::default()
                .build(),
            concurrent_statements: ConcurrentStatementsBuilder::default().build(),
            architecture_epilogue: ArchitectureEpilogueBuilder::default().build(),
        }
    }
    pub fn with_architecture_preamble(mut self, n: ArchitecturePreambleSyntax) -> Self {
        self.architecture_preamble = n;
        self
    }
    pub fn with_declarations(mut self, n: DeclarationsSyntax) -> Self {
        self.declarations = n;
        self
    }
    pub fn with_declaration_statement_separator(
        mut self,
        n: DeclarationStatementSeparatorSyntax,
    ) -> Self {
        self.declaration_statement_separator = n;
        self
    }
    pub fn with_concurrent_statements(mut self, n: ConcurrentStatementsSyntax) -> Self {
        self.concurrent_statements = n;
        self
    }
    pub fn with_architecture_epilogue(mut self, n: ArchitectureEpilogueSyntax) -> Self {
        self.architecture_epilogue = n;
        self
    }
    pub fn build(self) -> ArchitectureBodySyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ArchitectureBody);
        builder.push_node(self.architecture_preamble.raw().green().clone());
        builder.push_node(self.declarations.raw().green().clone());
        builder.push_node(self.declaration_statement_separator.raw().green().clone());
        builder.push_node(self.concurrent_statements.raw().green().clone());
        builder.push_node(self.architecture_epilogue.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ArchitectureBodySyntax::cast(node).unwrap()
    }
}
pub struct ArchitectureEpilogueBuilder {
    end_token: Token,
    architecture_token: Option<Token>,
    identifier_token: Option<Token>,
    semi_colon_token: Token,
}
impl Default for ArchitectureEpilogueBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl ArchitectureEpilogueBuilder {
    pub fn new() -> Self {
        Self {
            end_token: Token::new(
                TokenKind::Keyword(Kw::End),
                Kw::End.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            architecture_token: None,
            identifier_token: None,
            semi_colon_token: Token::new(
                TokenKind::SemiColon,
                TokenKind::SemiColon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_end_token(mut self, t: Token) -> Self {
        self.end_token = t;
        self
    }
    pub fn with_architecture_token(mut self, t: Token) -> Self {
        self.architecture_token = Some(t);
        self
    }
    pub fn with_identifier_token(mut self, t: Token) -> Self {
        self.identifier_token = Some(t);
        self
    }
    pub fn with_semi_colon_token(mut self, t: Token) -> Self {
        self.semi_colon_token = t;
        self
    }
    pub fn build(self) -> ArchitectureEpilogueSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ArchitectureEpilogue);
        builder.push(self.end_token);
        if let Some(t) = self.architecture_token {
            builder.push(t);
        }
        if let Some(t) = self.identifier_token {
            builder.push(t);
        }
        builder.push(self.semi_colon_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ArchitectureEpilogueSyntax::cast(node).unwrap()
    }
}
pub struct ArchitecturePreambleBuilder {
    architecture_token: Token,
    name_token: Token,
    of_token: Token,
    entity_name: NameSyntax,
    is_token: Token,
}
impl ArchitecturePreambleBuilder {
    pub fn new(name_token: Token, entity_name: NameSyntax) -> Self {
        Self {
            architecture_token: Token::new(
                TokenKind::Keyword(Kw::Architecture),
                Kw::Architecture.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            name_token,
            of_token: Token::new(
                TokenKind::Keyword(Kw::Of),
                Kw::Of.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            entity_name,
            is_token: Token::new(
                TokenKind::Keyword(Kw::Is),
                Kw::Is.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_architecture_token(mut self, t: Token) -> Self {
        self.architecture_token = t;
        self
    }
    pub fn with_name_token(mut self, t: Token) -> Self {
        self.name_token = t;
        self
    }
    pub fn with_of_token(mut self, t: Token) -> Self {
        self.of_token = t;
        self
    }
    pub fn with_entity_name(mut self, n: NameSyntax) -> Self {
        self.entity_name = n;
        self
    }
    pub fn with_is_token(mut self, t: Token) -> Self {
        self.is_token = t;
        self
    }
    pub fn build(self) -> ArchitecturePreambleSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ArchitecturePreamble);
        builder.push(self.architecture_token);
        builder.push(self.name_token);
        builder.push(self.of_token);
        builder.push_node(self.entity_name.raw().green().clone());
        builder.push(self.is_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ArchitecturePreambleSyntax::cast(node).unwrap()
    }
}
pub struct ArrayConstraintBuilder {
    index_constraint: IndexConstraintSyntax,
    constraint: ConstraintSyntax,
}
impl ArrayConstraintBuilder {
    pub fn new(constraint: ConstraintSyntax) -> Self {
        Self {
            index_constraint: IndexConstraintBuilder::default().build(),
            constraint,
        }
    }
    pub fn with_index_constraint(mut self, n: IndexConstraintSyntax) -> Self {
        self.index_constraint = n;
        self
    }
    pub fn with_constraint(mut self, n: ConstraintSyntax) -> Self {
        self.constraint = n;
        self
    }
    pub fn build(self) -> ArrayConstraintSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ArrayConstraint);
        builder.push_node(self.index_constraint.raw().green().clone());
        builder.push_node(self.constraint.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ArrayConstraintSyntax::cast(node).unwrap()
    }
}
pub struct AssertionBuilder {
    assert_token: Token,
    condition: ExpressionSyntax,
    report_token: Option<Token>,
    report: Option<ExpressionSyntax>,
    severity_token: Option<Token>,
    severity: Option<ExpressionSyntax>,
}
impl AssertionBuilder {
    pub fn new(condition: ExpressionSyntax) -> Self {
        Self {
            assert_token: Token::new(
                TokenKind::Keyword(Kw::Assert),
                Kw::Assert.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            condition,
            report_token: None,
            report: None,
            severity_token: None,
            severity: None,
        }
    }
    pub fn with_assert_token(mut self, t: Token) -> Self {
        self.assert_token = t;
        self
    }
    pub fn with_condition(mut self, n: ExpressionSyntax) -> Self {
        self.condition = n;
        self
    }
    pub fn with_report_token(mut self, t: Token) -> Self {
        self.report_token = Some(t);
        self
    }
    pub fn with_report(mut self, n: ExpressionSyntax) -> Self {
        self.report = Some(n);
        self
    }
    pub fn with_severity_token(mut self, t: Token) -> Self {
        self.severity_token = Some(t);
        self
    }
    pub fn with_severity(mut self, n: ExpressionSyntax) -> Self {
        self.severity = Some(n);
        self
    }
    pub fn build(self) -> AssertionSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::Assertion);
        builder.push(self.assert_token);
        builder.push_node(self.condition.raw().green().clone());
        if let Some(t) = self.report_token {
            builder.push(t);
        }
        if let Some(n) = self.report {
            builder.push_node(n.raw().green().clone());
        }
        if let Some(t) = self.severity_token {
            builder.push(t);
        }
        if let Some(n) = self.severity {
            builder.push_node(n.raw().green().clone());
        }
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        AssertionSyntax::cast(node).unwrap()
    }
}
pub struct AssertionStatementBuilder {
    label_token: Option<Token>,
    colon_token: Option<Token>,
    assertion: AssertionSyntax,
    semi_colon_token: Token,
}
impl AssertionStatementBuilder {
    pub fn new(assertion: AssertionSyntax) -> Self {
        Self {
            label_token: None,
            colon_token: None,
            assertion,
            semi_colon_token: Token::new(
                TokenKind::SemiColon,
                TokenKind::SemiColon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_label_token(mut self, t: Token) -> Self {
        self.label_token = Some(t);
        self
    }
    pub fn with_colon_token(mut self, t: Token) -> Self {
        self.colon_token = Some(t);
        self
    }
    pub fn with_assertion(mut self, n: AssertionSyntax) -> Self {
        self.assertion = n;
        self
    }
    pub fn with_semi_colon_token(mut self, t: Token) -> Self {
        self.semi_colon_token = t;
        self
    }
    pub fn build(self) -> AssertionStatementSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::AssertionStatement);
        if let Some(t) = self.label_token {
            builder.push(t);
        }
        if let Some(t) = self.colon_token {
            builder.push(t);
        }
        builder.push_node(self.assertion.raw().green().clone());
        builder.push(self.semi_colon_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        AssertionStatementSyntax::cast(node).unwrap()
    }
}
pub struct AssociationElementBuilder {
    formal_part: Option<FormalPartSyntax>,
    right_arrow_token: Option<Token>,
    actual_part: ActualPartSyntax,
}
impl Default for AssociationElementBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl AssociationElementBuilder {
    pub fn new() -> Self {
        Self {
            formal_part: None,
            right_arrow_token: None,
            actual_part: ActualPartBuilder::default().build(),
        }
    }
    pub fn with_formal_part(mut self, n: FormalPartSyntax) -> Self {
        self.formal_part = Some(n);
        self
    }
    pub fn with_right_arrow_token(mut self, t: Token) -> Self {
        self.right_arrow_token = Some(t);
        self
    }
    pub fn with_actual_part(mut self, n: ActualPartSyntax) -> Self {
        self.actual_part = n;
        self
    }
    pub fn build(self) -> AssociationElementSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::AssociationElement);
        if let Some(n) = self.formal_part {
            builder.push_node(n.raw().green().clone());
        }
        if let Some(t) = self.right_arrow_token {
            builder.push(t);
        }
        builder.push_node(self.actual_part.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        AssociationElementSyntax::cast(node).unwrap()
    }
}
pub struct AssociationListBuilder {
    association_elements: Vec<AssociationElementSyntax>,
    comma_token: Vec<Token>,
}
impl Default for AssociationListBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl AssociationListBuilder {
    pub fn new() -> Self {
        Self {
            association_elements: Vec::new(),
            comma_token: Vec::new(),
        }
    }
    pub fn add_association_elements(mut self, n: AssociationElementSyntax) -> Self {
        self.association_elements.push(n);
        self
    }
    pub fn add_comma_token(mut self, t: Token) -> Self {
        self.comma_token.push(t);
        self
    }
    pub fn build(self) -> AssociationListSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::AssociationList);
        for n in self.association_elements {
            builder.push_node(n.raw().green().clone());
        }
        for t in self.comma_token {
            builder.push(t);
        }
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        AssociationListSyntax::cast(node).unwrap()
    }
}
pub struct AttributeDeclarationBuilder {
    attribute_token: Token,
    identifier_token: Token,
    colon_token: Token,
    name: NameSyntax,
    semi_colon_token: Token,
}
impl AttributeDeclarationBuilder {
    pub fn new(identifier_token: Token, name: NameSyntax) -> Self {
        Self {
            attribute_token: Token::new(
                TokenKind::Keyword(Kw::Attribute),
                Kw::Attribute.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            identifier_token,
            colon_token: Token::new(
                TokenKind::Colon,
                TokenKind::Colon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            name,
            semi_colon_token: Token::new(
                TokenKind::SemiColon,
                TokenKind::SemiColon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_attribute_token(mut self, t: Token) -> Self {
        self.attribute_token = t;
        self
    }
    pub fn with_identifier_token(mut self, t: Token) -> Self {
        self.identifier_token = t;
        self
    }
    pub fn with_colon_token(mut self, t: Token) -> Self {
        self.colon_token = t;
        self
    }
    pub fn with_name(mut self, n: NameSyntax) -> Self {
        self.name = n;
        self
    }
    pub fn with_semi_colon_token(mut self, t: Token) -> Self {
        self.semi_colon_token = t;
        self
    }
    pub fn build(self) -> AttributeDeclarationSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::AttributeDeclaration);
        builder.push(self.attribute_token);
        builder.push(self.identifier_token);
        builder.push(self.colon_token);
        builder.push_node(self.name.raw().green().clone());
        builder.push(self.semi_colon_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        AttributeDeclarationSyntax::cast(node).unwrap()
    }
}
pub struct AttributeNameBuilder {
    left_square_token: Token,
    signature: SignatureSyntax,
    right_square_token: Token,
    tick_token: Token,
    attribute_designator_token_token: Token,
    left_par_token: Token,
    expression: ExpressionSyntax,
    right_par_token: Token,
}
impl AttributeNameBuilder {
    pub fn new(
        attribute_designator_token_token: Token,
        signature: SignatureSyntax,
        expression: ExpressionSyntax,
    ) -> Self {
        Self {
            left_square_token: Token::new(
                TokenKind::LeftSquare,
                TokenKind::LeftSquare.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            signature,
            right_square_token: Token::new(
                TokenKind::RightSquare,
                TokenKind::RightSquare.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            tick_token: Token::new(
                TokenKind::Tick,
                TokenKind::Tick.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            attribute_designator_token_token,
            left_par_token: Token::new(
                TokenKind::LeftPar,
                TokenKind::LeftPar.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            expression,
            right_par_token: Token::new(
                TokenKind::RightPar,
                TokenKind::RightPar.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_left_square_token(mut self, t: Token) -> Self {
        self.left_square_token = t;
        self
    }
    pub fn with_signature(mut self, n: SignatureSyntax) -> Self {
        self.signature = n;
        self
    }
    pub fn with_right_square_token(mut self, t: Token) -> Self {
        self.right_square_token = t;
        self
    }
    pub fn with_tick_token(mut self, t: Token) -> Self {
        self.tick_token = t;
        self
    }
    pub fn with_attribute_designator_token_token(mut self, t: Token) -> Self {
        self.attribute_designator_token_token = t;
        self
    }
    pub fn with_left_par_token(mut self, t: Token) -> Self {
        self.left_par_token = t;
        self
    }
    pub fn with_expression(mut self, n: ExpressionSyntax) -> Self {
        self.expression = n;
        self
    }
    pub fn with_right_par_token(mut self, t: Token) -> Self {
        self.right_par_token = t;
        self
    }
    pub fn build(self) -> AttributeNameSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::AttributeName);
        builder.push(self.left_square_token);
        builder.push_node(self.signature.raw().green().clone());
        builder.push(self.right_square_token);
        builder.push(self.tick_token);
        builder.push(self.attribute_designator_token_token);
        builder.push(self.left_par_token);
        builder.push_node(self.expression.raw().green().clone());
        builder.push(self.right_par_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        AttributeNameSyntax::cast(node).unwrap()
    }
}
pub struct AttributeRangeBuilder {
    name: NameSyntax,
}
impl AttributeRangeBuilder {
    pub fn new(name: NameSyntax) -> Self {
        Self { name }
    }
    pub fn with_name(mut self, n: NameSyntax) -> Self {
        self.name = n;
        self
    }
    pub fn build(self) -> AttributeRangeSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::AttributeRange);
        builder.push_node(self.name.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        AttributeRangeSyntax::cast(node).unwrap()
    }
}
pub struct AttributeSpecificationBuilder {
    attribute_token: Token,
    attribute_designator_token_token: Token,
    of_token: Token,
    entity_specification: EntitySpecificationSyntax,
    is_token: Token,
    expression: ExpressionSyntax,
    semi_colon_token: Token,
}
impl AttributeSpecificationBuilder {
    pub fn new(
        attribute_designator_token_token: Token,
        entity_specification: EntitySpecificationSyntax,
        expression: ExpressionSyntax,
    ) -> Self {
        Self {
            attribute_token: Token::new(
                TokenKind::Keyword(Kw::Attribute),
                Kw::Attribute.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            attribute_designator_token_token,
            of_token: Token::new(
                TokenKind::Keyword(Kw::Of),
                Kw::Of.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            entity_specification,
            is_token: Token::new(
                TokenKind::Keyword(Kw::Is),
                Kw::Is.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            expression,
            semi_colon_token: Token::new(
                TokenKind::SemiColon,
                TokenKind::SemiColon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_attribute_token(mut self, t: Token) -> Self {
        self.attribute_token = t;
        self
    }
    pub fn with_attribute_designator_token_token(mut self, t: Token) -> Self {
        self.attribute_designator_token_token = t;
        self
    }
    pub fn with_of_token(mut self, t: Token) -> Self {
        self.of_token = t;
        self
    }
    pub fn with_entity_specification(mut self, n: EntitySpecificationSyntax) -> Self {
        self.entity_specification = n;
        self
    }
    pub fn with_is_token(mut self, t: Token) -> Self {
        self.is_token = t;
        self
    }
    pub fn with_expression(mut self, n: ExpressionSyntax) -> Self {
        self.expression = n;
        self
    }
    pub fn with_semi_colon_token(mut self, t: Token) -> Self {
        self.semi_colon_token = t;
        self
    }
    pub fn build(self) -> AttributeSpecificationSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::AttributeSpecification);
        builder.push(self.attribute_token);
        builder.push(self.attribute_designator_token_token);
        builder.push(self.of_token);
        builder.push_node(self.entity_specification.raw().green().clone());
        builder.push(self.is_token);
        builder.push_node(self.expression.raw().green().clone());
        builder.push(self.semi_colon_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        AttributeSpecificationSyntax::cast(node).unwrap()
    }
}
pub struct BinaryExpressionBuilder {
    lhs: ExpressionSyntax,
    op: BinaryOperatorSyntax,
    rhs: ExpressionSyntax,
}
impl BinaryExpressionBuilder {
    pub fn new(lhs: ExpressionSyntax, op: BinaryOperatorSyntax, rhs: ExpressionSyntax) -> Self {
        Self { lhs, op, rhs }
    }
    pub fn with_lhs(mut self, n: ExpressionSyntax) -> Self {
        self.lhs = n;
        self
    }
    pub fn with_op(mut self, n: BinaryOperatorSyntax) -> Self {
        self.op = n;
        self
    }
    pub fn with_rhs(mut self, n: ExpressionSyntax) -> Self {
        self.rhs = n;
        self
    }
    pub fn build(self) -> BinaryExpressionSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::BinaryExpression);
        builder.push_node(self.lhs.raw().green().clone());
        builder.push(self.op.raw().token().clone());
        builder.push_node(self.rhs.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        BinaryExpressionSyntax::cast(node).unwrap()
    }
}
pub struct BindingIndicationBuilder {
    use_token: Option<Token>,
    entity_aspect: Option<EntityAspectSyntax>,
    generic_map_aspect: Option<GenericMapAspectSyntax>,
    port_map_aspect: Option<PortMapAspectSyntax>,
}
impl Default for BindingIndicationBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl BindingIndicationBuilder {
    pub fn new() -> Self {
        Self {
            use_token: None,
            entity_aspect: None,
            generic_map_aspect: None,
            port_map_aspect: None,
        }
    }
    pub fn with_use_token(mut self, t: Token) -> Self {
        self.use_token = Some(t);
        self
    }
    pub fn with_entity_aspect(mut self, n: EntityAspectSyntax) -> Self {
        self.entity_aspect = Some(n);
        self
    }
    pub fn with_generic_map_aspect(mut self, n: GenericMapAspectSyntax) -> Self {
        self.generic_map_aspect = Some(n);
        self
    }
    pub fn with_port_map_aspect(mut self, n: PortMapAspectSyntax) -> Self {
        self.port_map_aspect = Some(n);
        self
    }
    pub fn build(self) -> BindingIndicationSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::BindingIndication);
        if let Some(t) = self.use_token {
            builder.push(t);
        }
        if let Some(n) = self.entity_aspect {
            builder.push_node(n.raw().green().clone());
        }
        if let Some(n) = self.generic_map_aspect {
            builder.push_node(n.raw().green().clone());
        }
        if let Some(n) = self.port_map_aspect {
            builder.push_node(n.raw().green().clone());
        }
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        BindingIndicationSyntax::cast(node).unwrap()
    }
}
pub struct BlockConfigurationBuilder {
    block_configuration_preamble: BlockConfigurationPreambleSyntax,
    block_configuration_items: BlockConfigurationItemsSyntax,
    block_configuration_epilogue: BlockConfigurationEpilogueSyntax,
}
impl BlockConfigurationBuilder {
    pub fn new(block_configuration_preamble: BlockConfigurationPreambleSyntax) -> Self {
        Self {
            block_configuration_preamble,
            block_configuration_items: BlockConfigurationItemsBuilder::default().build(),
            block_configuration_epilogue: BlockConfigurationEpilogueBuilder::default().build(),
        }
    }
    pub fn with_block_configuration_preamble(
        mut self,
        n: BlockConfigurationPreambleSyntax,
    ) -> Self {
        self.block_configuration_preamble = n;
        self
    }
    pub fn with_block_configuration_items(mut self, n: BlockConfigurationItemsSyntax) -> Self {
        self.block_configuration_items = n;
        self
    }
    pub fn with_block_configuration_epilogue(
        mut self,
        n: BlockConfigurationEpilogueSyntax,
    ) -> Self {
        self.block_configuration_epilogue = n;
        self
    }
    pub fn build(self) -> BlockConfigurationSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::BlockConfiguration);
        builder.push_node(self.block_configuration_preamble.raw().green().clone());
        builder.push_node(self.block_configuration_items.raw().green().clone());
        builder.push_node(self.block_configuration_epilogue.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        BlockConfigurationSyntax::cast(node).unwrap()
    }
}
pub struct BlockConfigurationEpilogueBuilder {
    end_token: Token,
    for_token: Token,
    semi_colon_token: Token,
}
impl Default for BlockConfigurationEpilogueBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl BlockConfigurationEpilogueBuilder {
    pub fn new() -> Self {
        Self {
            end_token: Token::new(
                TokenKind::Keyword(Kw::End),
                Kw::End.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            for_token: Token::new(
                TokenKind::Keyword(Kw::For),
                Kw::For.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            semi_colon_token: Token::new(
                TokenKind::SemiColon,
                TokenKind::SemiColon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_end_token(mut self, t: Token) -> Self {
        self.end_token = t;
        self
    }
    pub fn with_for_token(mut self, t: Token) -> Self {
        self.for_token = t;
        self
    }
    pub fn with_semi_colon_token(mut self, t: Token) -> Self {
        self.semi_colon_token = t;
        self
    }
    pub fn build(self) -> BlockConfigurationEpilogueSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::BlockConfigurationEpilogue);
        builder.push(self.end_token);
        builder.push(self.for_token);
        builder.push(self.semi_colon_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        BlockConfigurationEpilogueSyntax::cast(node).unwrap()
    }
}
pub struct BlockConfigurationItemBuilder {
    block_configuration: BlockConfigurationSyntax,
}
impl BlockConfigurationItemBuilder {
    pub fn new(block_configuration: BlockConfigurationSyntax) -> Self {
        Self {
            block_configuration,
        }
    }
    pub fn with_block_configuration(mut self, n: BlockConfigurationSyntax) -> Self {
        self.block_configuration = n;
        self
    }
    pub fn build(self) -> BlockConfigurationItemSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::BlockConfigurationItem);
        builder.push_node(self.block_configuration.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        BlockConfigurationItemSyntax::cast(node).unwrap()
    }
}
pub struct BlockConfigurationItemsBuilder {
    use_clauses: Vec<UseClauseSyntax>,
    configuration_items: Vec<ConfigurationItemSyntax>,
}
impl Default for BlockConfigurationItemsBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl BlockConfigurationItemsBuilder {
    pub fn new() -> Self {
        Self {
            use_clauses: Vec::new(),
            configuration_items: Vec::new(),
        }
    }
    pub fn add_use_clauses(mut self, n: UseClauseSyntax) -> Self {
        self.use_clauses.push(n);
        self
    }
    pub fn add_configuration_items(mut self, n: ConfigurationItemSyntax) -> Self {
        self.configuration_items.push(n);
        self
    }
    pub fn build(self) -> BlockConfigurationItemsSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::BlockConfigurationItems);
        for n in self.use_clauses {
            builder.push_node(n.raw().green().clone());
        }
        for n in self.configuration_items {
            builder.push_node(n.raw().green().clone());
        }
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        BlockConfigurationItemsSyntax::cast(node).unwrap()
    }
}
pub struct BlockConfigurationPreambleBuilder {
    for_token: Token,
    name: NameSyntax,
}
impl BlockConfigurationPreambleBuilder {
    pub fn new(name: NameSyntax) -> Self {
        Self {
            for_token: Token::new(
                TokenKind::Keyword(Kw::For),
                Kw::For.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            name,
        }
    }
    pub fn with_for_token(mut self, t: Token) -> Self {
        self.for_token = t;
        self
    }
    pub fn with_name(mut self, n: NameSyntax) -> Self {
        self.name = n;
        self
    }
    pub fn build(self) -> BlockConfigurationPreambleSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::BlockConfigurationPreamble);
        builder.push(self.for_token);
        builder.push_node(self.name.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        BlockConfigurationPreambleSyntax::cast(node).unwrap()
    }
}
pub struct BlockEpilogueBuilder {
    end_token: Token,
    block_token: Token,
    identifier_token: Option<Token>,
    semi_colon_token: Token,
}
impl Default for BlockEpilogueBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl BlockEpilogueBuilder {
    pub fn new() -> Self {
        Self {
            end_token: Token::new(
                TokenKind::Keyword(Kw::End),
                Kw::End.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            block_token: Token::new(
                TokenKind::Keyword(Kw::Block),
                Kw::Block.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            identifier_token: None,
            semi_colon_token: Token::new(
                TokenKind::SemiColon,
                TokenKind::SemiColon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_end_token(mut self, t: Token) -> Self {
        self.end_token = t;
        self
    }
    pub fn with_block_token(mut self, t: Token) -> Self {
        self.block_token = t;
        self
    }
    pub fn with_identifier_token(mut self, t: Token) -> Self {
        self.identifier_token = Some(t);
        self
    }
    pub fn with_semi_colon_token(mut self, t: Token) -> Self {
        self.semi_colon_token = t;
        self
    }
    pub fn build(self) -> BlockEpilogueSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::BlockEpilogue);
        builder.push(self.end_token);
        builder.push(self.block_token);
        if let Some(t) = self.identifier_token {
            builder.push(t);
        }
        builder.push(self.semi_colon_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        BlockEpilogueSyntax::cast(node).unwrap()
    }
}
pub struct BlockHeaderBuilder {
    generic_clause: Option<GenericClauseSyntax>,
    semi_colon_terminated_generic_map_aspect: Option<SemiColonTerminatedGenericMapAspectSyntax>,
    port_clause: Option<PortClauseSyntax>,
    semi_colon_terminated_port_map_aspect: Option<SemiColonTerminatedPortMapAspectSyntax>,
}
impl Default for BlockHeaderBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl BlockHeaderBuilder {
    pub fn new() -> Self {
        Self {
            generic_clause: None,
            semi_colon_terminated_generic_map_aspect: None,
            port_clause: None,
            semi_colon_terminated_port_map_aspect: None,
        }
    }
    pub fn with_generic_clause(mut self, n: GenericClauseSyntax) -> Self {
        self.generic_clause = Some(n);
        self
    }
    pub fn with_semi_colon_terminated_generic_map_aspect(
        mut self,
        n: SemiColonTerminatedGenericMapAspectSyntax,
    ) -> Self {
        self.semi_colon_terminated_generic_map_aspect = Some(n);
        self
    }
    pub fn with_port_clause(mut self, n: PortClauseSyntax) -> Self {
        self.port_clause = Some(n);
        self
    }
    pub fn with_semi_colon_terminated_port_map_aspect(
        mut self,
        n: SemiColonTerminatedPortMapAspectSyntax,
    ) -> Self {
        self.semi_colon_terminated_port_map_aspect = Some(n);
        self
    }
    pub fn build(self) -> BlockHeaderSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::BlockHeader);
        if let Some(n) = self.generic_clause {
            builder.push_node(n.raw().green().clone());
        }
        if let Some(n) = self.semi_colon_terminated_generic_map_aspect {
            builder.push_node(n.raw().green().clone());
        }
        if let Some(n) = self.port_clause {
            builder.push_node(n.raw().green().clone());
        }
        if let Some(n) = self.semi_colon_terminated_port_map_aspect {
            builder.push_node(n.raw().green().clone());
        }
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        BlockHeaderSyntax::cast(node).unwrap()
    }
}
pub struct BlockPreambleBuilder {
    label: LabelSyntax,
    block_token: Token,
    condition: Option<ParenthesizedExpressionSyntax>,
    is_token: Option<Token>,
}
impl BlockPreambleBuilder {
    pub fn new(label: LabelSyntax) -> Self {
        Self {
            label,
            block_token: Token::new(
                TokenKind::Keyword(Kw::Block),
                Kw::Block.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            condition: None,
            is_token: None,
        }
    }
    pub fn with_label(mut self, n: LabelSyntax) -> Self {
        self.label = n;
        self
    }
    pub fn with_block_token(mut self, t: Token) -> Self {
        self.block_token = t;
        self
    }
    pub fn with_condition(mut self, n: ParenthesizedExpressionSyntax) -> Self {
        self.condition = Some(n);
        self
    }
    pub fn with_is_token(mut self, t: Token) -> Self {
        self.is_token = Some(t);
        self
    }
    pub fn build(self) -> BlockPreambleSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::BlockPreamble);
        builder.push_node(self.label.raw().green().clone());
        builder.push(self.block_token);
        if let Some(n) = self.condition {
            builder.push_node(n.raw().green().clone());
        }
        if let Some(t) = self.is_token {
            builder.push(t);
        }
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        BlockPreambleSyntax::cast(node).unwrap()
    }
}
pub struct BlockStatementBuilder {
    block_preamble: BlockPreambleSyntax,
    block_header: BlockHeaderSyntax,
    declarations: DeclarationsSyntax,
    declaration_statement_separator: DeclarationStatementSeparatorSyntax,
    concurrent_statements: ConcurrentStatementsSyntax,
    block_epilogue: BlockEpilogueSyntax,
}
impl BlockStatementBuilder {
    pub fn new(block_preamble: BlockPreambleSyntax) -> Self {
        Self {
            block_preamble,
            block_header: BlockHeaderBuilder::default().build(),
            declarations: DeclarationsBuilder::default().build(),
            declaration_statement_separator: DeclarationStatementSeparatorBuilder::default()
                .build(),
            concurrent_statements: ConcurrentStatementsBuilder::default().build(),
            block_epilogue: BlockEpilogueBuilder::default().build(),
        }
    }
    pub fn with_block_preamble(mut self, n: BlockPreambleSyntax) -> Self {
        self.block_preamble = n;
        self
    }
    pub fn with_block_header(mut self, n: BlockHeaderSyntax) -> Self {
        self.block_header = n;
        self
    }
    pub fn with_declarations(mut self, n: DeclarationsSyntax) -> Self {
        self.declarations = n;
        self
    }
    pub fn with_declaration_statement_separator(
        mut self,
        n: DeclarationStatementSeparatorSyntax,
    ) -> Self {
        self.declaration_statement_separator = n;
        self
    }
    pub fn with_concurrent_statements(mut self, n: ConcurrentStatementsSyntax) -> Self {
        self.concurrent_statements = n;
        self
    }
    pub fn with_block_epilogue(mut self, n: BlockEpilogueSyntax) -> Self {
        self.block_epilogue = n;
        self
    }
    pub fn build(self) -> BlockStatementSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::BlockStatement);
        builder.push_node(self.block_preamble.raw().green().clone());
        builder.push_node(self.block_header.raw().green().clone());
        builder.push_node(self.declarations.raw().green().clone());
        builder.push_node(self.declaration_statement_separator.raw().green().clone());
        builder.push_node(self.concurrent_statements.raw().green().clone());
        builder.push_node(self.block_epilogue.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        BlockStatementSyntax::cast(node).unwrap()
    }
}
pub struct CaseGenerateAlternativeBuilder {
    when_token: Token,
    label: Option<LabelSyntax>,
    choices: ChoicesSyntax,
    right_arrow_token: Token,
    generate_statement_body: GenerateStatementBodySyntax,
}
impl Default for CaseGenerateAlternativeBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl CaseGenerateAlternativeBuilder {
    pub fn new() -> Self {
        Self {
            when_token: Token::new(
                TokenKind::Keyword(Kw::When),
                Kw::When.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            label: None,
            choices: ChoicesBuilder::default().build(),
            right_arrow_token: Token::new(
                TokenKind::RightArrow,
                TokenKind::RightArrow.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            generate_statement_body: GenerateStatementBodyBuilder::default().build(),
        }
    }
    pub fn with_when_token(mut self, t: Token) -> Self {
        self.when_token = t;
        self
    }
    pub fn with_label(mut self, n: LabelSyntax) -> Self {
        self.label = Some(n);
        self
    }
    pub fn with_choices(mut self, n: ChoicesSyntax) -> Self {
        self.choices = n;
        self
    }
    pub fn with_right_arrow_token(mut self, t: Token) -> Self {
        self.right_arrow_token = t;
        self
    }
    pub fn with_generate_statement_body(mut self, n: GenerateStatementBodySyntax) -> Self {
        self.generate_statement_body = n;
        self
    }
    pub fn build(self) -> CaseGenerateAlternativeSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::CaseGenerateAlternative);
        builder.push(self.when_token);
        if let Some(n) = self.label {
            builder.push_node(n.raw().green().clone());
        }
        builder.push_node(self.choices.raw().green().clone());
        builder.push(self.right_arrow_token);
        builder.push_node(self.generate_statement_body.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        CaseGenerateAlternativeSyntax::cast(node).unwrap()
    }
}
pub struct CaseGenerateStatementBuilder {
    case_generate_statement_preamble: CaseGenerateStatementPreambleSyntax,
    case_generate_alternatives: Vec<CaseGenerateAlternativeSyntax>,
    case_generate_statement_epilogue: CaseGenerateStatementEpilogueSyntax,
}
impl CaseGenerateStatementBuilder {
    pub fn new(case_generate_statement_preamble: CaseGenerateStatementPreambleSyntax) -> Self {
        Self {
            case_generate_statement_preamble,
            case_generate_alternatives: Vec::new(),
            case_generate_statement_epilogue: CaseGenerateStatementEpilogueBuilder::default()
                .build(),
        }
    }
    pub fn with_case_generate_statement_preamble(
        mut self,
        n: CaseGenerateStatementPreambleSyntax,
    ) -> Self {
        self.case_generate_statement_preamble = n;
        self
    }
    pub fn add_case_generate_alternatives(mut self, n: CaseGenerateAlternativeSyntax) -> Self {
        self.case_generate_alternatives.push(n);
        self
    }
    pub fn with_case_generate_statement_epilogue(
        mut self,
        n: CaseGenerateStatementEpilogueSyntax,
    ) -> Self {
        self.case_generate_statement_epilogue = n;
        self
    }
    pub fn build(self) -> CaseGenerateStatementSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::CaseGenerateStatement);
        builder.push_node(self.case_generate_statement_preamble.raw().green().clone());
        for n in self.case_generate_alternatives {
            builder.push_node(n.raw().green().clone());
        }
        builder.push_node(self.case_generate_statement_epilogue.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        CaseGenerateStatementSyntax::cast(node).unwrap()
    }
}
pub struct CaseGenerateStatementEpilogueBuilder {
    end_token: Token,
    generate_token: Token,
    identifier_token: Option<Token>,
    semi_colon_token: Token,
}
impl Default for CaseGenerateStatementEpilogueBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl CaseGenerateStatementEpilogueBuilder {
    pub fn new() -> Self {
        Self {
            end_token: Token::new(
                TokenKind::Keyword(Kw::End),
                Kw::End.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            generate_token: Token::new(
                TokenKind::Keyword(Kw::Generate),
                Kw::Generate.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            identifier_token: None,
            semi_colon_token: Token::new(
                TokenKind::SemiColon,
                TokenKind::SemiColon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_end_token(mut self, t: Token) -> Self {
        self.end_token = t;
        self
    }
    pub fn with_generate_token(mut self, t: Token) -> Self {
        self.generate_token = t;
        self
    }
    pub fn with_identifier_token(mut self, t: Token) -> Self {
        self.identifier_token = Some(t);
        self
    }
    pub fn with_semi_colon_token(mut self, t: Token) -> Self {
        self.semi_colon_token = t;
        self
    }
    pub fn build(self) -> CaseGenerateStatementEpilogueSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::CaseGenerateStatementEpilogue);
        builder.push(self.end_token);
        builder.push(self.generate_token);
        if let Some(t) = self.identifier_token {
            builder.push(t);
        }
        builder.push(self.semi_colon_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        CaseGenerateStatementEpilogueSyntax::cast(node).unwrap()
    }
}
pub struct CaseGenerateStatementPreambleBuilder {
    label: LabelSyntax,
    case_token: Token,
    expression: ExpressionSyntax,
    generate_token: Token,
}
impl CaseGenerateStatementPreambleBuilder {
    pub fn new(label: LabelSyntax, expression: ExpressionSyntax) -> Self {
        Self {
            label,
            case_token: Token::new(
                TokenKind::Keyword(Kw::Case),
                Kw::Case.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            expression,
            generate_token: Token::new(
                TokenKind::Keyword(Kw::Generate),
                Kw::Generate.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_label(mut self, n: LabelSyntax) -> Self {
        self.label = n;
        self
    }
    pub fn with_case_token(mut self, t: Token) -> Self {
        self.case_token = t;
        self
    }
    pub fn with_expression(mut self, n: ExpressionSyntax) -> Self {
        self.expression = n;
        self
    }
    pub fn with_generate_token(mut self, t: Token) -> Self {
        self.generate_token = t;
        self
    }
    pub fn build(self) -> CaseGenerateStatementPreambleSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::CaseGenerateStatementPreamble);
        builder.push_node(self.label.raw().green().clone());
        builder.push(self.case_token);
        builder.push_node(self.expression.raw().green().clone());
        builder.push(self.generate_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        CaseGenerateStatementPreambleSyntax::cast(node).unwrap()
    }
}
pub struct CaseStatementBuilder {
    case_statement_preamble: CaseStatementPreambleSyntax,
    case_statement_alternatives: Vec<CaseStatementAlternativeSyntax>,
    case_statement_epilogue: CaseStatementEpilogueSyntax,
}
impl CaseStatementBuilder {
    pub fn new(case_statement_preamble: CaseStatementPreambleSyntax) -> Self {
        Self {
            case_statement_preamble,
            case_statement_alternatives: Vec::new(),
            case_statement_epilogue: CaseStatementEpilogueBuilder::default().build(),
        }
    }
    pub fn with_case_statement_preamble(mut self, n: CaseStatementPreambleSyntax) -> Self {
        self.case_statement_preamble = n;
        self
    }
    pub fn add_case_statement_alternatives(mut self, n: CaseStatementAlternativeSyntax) -> Self {
        self.case_statement_alternatives.push(n);
        self
    }
    pub fn with_case_statement_epilogue(mut self, n: CaseStatementEpilogueSyntax) -> Self {
        self.case_statement_epilogue = n;
        self
    }
    pub fn build(self) -> CaseStatementSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::CaseStatement);
        builder.push_node(self.case_statement_preamble.raw().green().clone());
        for n in self.case_statement_alternatives {
            builder.push_node(n.raw().green().clone());
        }
        builder.push_node(self.case_statement_epilogue.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        CaseStatementSyntax::cast(node).unwrap()
    }
}
pub struct CaseStatementAlternativeBuilder {
    case_statement_alternative_preamble: CaseStatementAlternativePreambleSyntax,
    sequential_statements: SequentialStatementsSyntax,
}
impl Default for CaseStatementAlternativeBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl CaseStatementAlternativeBuilder {
    pub fn new() -> Self {
        Self {
            case_statement_alternative_preamble: CaseStatementAlternativePreambleBuilder::default()
                .build(),
            sequential_statements: SequentialStatementsBuilder::default().build(),
        }
    }
    pub fn with_case_statement_alternative_preamble(
        mut self,
        n: CaseStatementAlternativePreambleSyntax,
    ) -> Self {
        self.case_statement_alternative_preamble = n;
        self
    }
    pub fn with_sequential_statements(mut self, n: SequentialStatementsSyntax) -> Self {
        self.sequential_statements = n;
        self
    }
    pub fn build(self) -> CaseStatementAlternativeSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::CaseStatementAlternative);
        builder.push_node(
            self.case_statement_alternative_preamble
                .raw()
                .green()
                .clone(),
        );
        builder.push_node(self.sequential_statements.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        CaseStatementAlternativeSyntax::cast(node).unwrap()
    }
}
pub struct CaseStatementAlternativePreambleBuilder {
    when_token: Token,
    choices: ChoicesSyntax,
    right_arrow_token: Token,
}
impl Default for CaseStatementAlternativePreambleBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl CaseStatementAlternativePreambleBuilder {
    pub fn new() -> Self {
        Self {
            when_token: Token::new(
                TokenKind::Keyword(Kw::When),
                Kw::When.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            choices: ChoicesBuilder::default().build(),
            right_arrow_token: Token::new(
                TokenKind::RightArrow,
                TokenKind::RightArrow.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_when_token(mut self, t: Token) -> Self {
        self.when_token = t;
        self
    }
    pub fn with_choices(mut self, n: ChoicesSyntax) -> Self {
        self.choices = n;
        self
    }
    pub fn with_right_arrow_token(mut self, t: Token) -> Self {
        self.right_arrow_token = t;
        self
    }
    pub fn build(self) -> CaseStatementAlternativePreambleSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::CaseStatementAlternativePreamble);
        builder.push(self.when_token);
        builder.push_node(self.choices.raw().green().clone());
        builder.push(self.right_arrow_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        CaseStatementAlternativePreambleSyntax::cast(node).unwrap()
    }
}
pub struct CaseStatementEpilogueBuilder {
    end_token: Token,
    case_token: Token,
    que_token: Option<Token>,
    identifier_token: Option<Token>,
    semi_colon_token: Token,
}
impl Default for CaseStatementEpilogueBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl CaseStatementEpilogueBuilder {
    pub fn new() -> Self {
        Self {
            end_token: Token::new(
                TokenKind::Keyword(Kw::End),
                Kw::End.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            case_token: Token::new(
                TokenKind::Keyword(Kw::Case),
                Kw::Case.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            que_token: None,
            identifier_token: None,
            semi_colon_token: Token::new(
                TokenKind::SemiColon,
                TokenKind::SemiColon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_end_token(mut self, t: Token) -> Self {
        self.end_token = t;
        self
    }
    pub fn with_case_token(mut self, t: Token) -> Self {
        self.case_token = t;
        self
    }
    pub fn with_que_token(mut self, t: Token) -> Self {
        self.que_token = Some(t);
        self
    }
    pub fn with_identifier_token(mut self, t: Token) -> Self {
        self.identifier_token = Some(t);
        self
    }
    pub fn with_semi_colon_token(mut self, t: Token) -> Self {
        self.semi_colon_token = t;
        self
    }
    pub fn build(self) -> CaseStatementEpilogueSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::CaseStatementEpilogue);
        builder.push(self.end_token);
        builder.push(self.case_token);
        if let Some(t) = self.que_token {
            builder.push(t);
        }
        if let Some(t) = self.identifier_token {
            builder.push(t);
        }
        builder.push(self.semi_colon_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        CaseStatementEpilogueSyntax::cast(node).unwrap()
    }
}
pub struct CaseStatementPreambleBuilder {
    label: LabelSyntax,
    case_token: Token,
    que_token: Option<Token>,
    expression: Option<ExpressionSyntax>,
    is_token: Token,
}
impl CaseStatementPreambleBuilder {
    pub fn new(label: LabelSyntax) -> Self {
        Self {
            label,
            case_token: Token::new(
                TokenKind::Keyword(Kw::Case),
                Kw::Case.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            que_token: None,
            expression: None,
            is_token: Token::new(
                TokenKind::Keyword(Kw::Is),
                Kw::Is.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_label(mut self, n: LabelSyntax) -> Self {
        self.label = n;
        self
    }
    pub fn with_case_token(mut self, t: Token) -> Self {
        self.case_token = t;
        self
    }
    pub fn with_que_token(mut self, t: Token) -> Self {
        self.que_token = Some(t);
        self
    }
    pub fn with_expression(mut self, n: ExpressionSyntax) -> Self {
        self.expression = Some(n);
        self
    }
    pub fn with_is_token(mut self, t: Token) -> Self {
        self.is_token = t;
        self
    }
    pub fn build(self) -> CaseStatementPreambleSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::CaseStatementPreamble);
        builder.push_node(self.label.raw().green().clone());
        builder.push(self.case_token);
        if let Some(t) = self.que_token {
            builder.push(t);
        }
        if let Some(n) = self.expression {
            builder.push_node(n.raw().green().clone());
        }
        builder.push(self.is_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        CaseStatementPreambleSyntax::cast(node).unwrap()
    }
}
pub struct ChoicesBuilder {
    choices: Vec<ChoiceSyntax>,
    bar_token: Vec<Token>,
}
impl Default for ChoicesBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl ChoicesBuilder {
    pub fn new() -> Self {
        Self {
            choices: Vec::new(),
            bar_token: Vec::new(),
        }
    }
    pub fn add_choices(mut self, n: ChoiceSyntax) -> Self {
        self.choices.push(n);
        self
    }
    pub fn add_bar_token(mut self, t: Token) -> Self {
        self.bar_token.push(t);
        self
    }
    pub fn build(self) -> ChoicesSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::Choices);
        for n in self.choices {
            builder.push_node(n.raw().green().clone());
        }
        for t in self.bar_token {
            builder.push(t);
        }
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ChoicesSyntax::cast(node).unwrap()
    }
}
pub struct ComponentConfigurationBuilder {
    component_configuration_preamble: ComponentConfigurationPreambleSyntax,
    component_configuration_items: ComponentConfigurationItemsSyntax,
    component_configuration_epilogue: ComponentConfigurationEpilogueSyntax,
}
impl ComponentConfigurationBuilder {
    pub fn new(component_configuration_preamble: ComponentConfigurationPreambleSyntax) -> Self {
        Self {
            component_configuration_preamble,
            component_configuration_items: ComponentConfigurationItemsBuilder::default().build(),
            component_configuration_epilogue: ComponentConfigurationEpilogueBuilder::default()
                .build(),
        }
    }
    pub fn with_component_configuration_preamble(
        mut self,
        n: ComponentConfigurationPreambleSyntax,
    ) -> Self {
        self.component_configuration_preamble = n;
        self
    }
    pub fn with_component_configuration_items(
        mut self,
        n: ComponentConfigurationItemsSyntax,
    ) -> Self {
        self.component_configuration_items = n;
        self
    }
    pub fn with_component_configuration_epilogue(
        mut self,
        n: ComponentConfigurationEpilogueSyntax,
    ) -> Self {
        self.component_configuration_epilogue = n;
        self
    }
    pub fn build(self) -> ComponentConfigurationSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ComponentConfiguration);
        builder.push_node(self.component_configuration_preamble.raw().green().clone());
        builder.push_node(self.component_configuration_items.raw().green().clone());
        builder.push_node(self.component_configuration_epilogue.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ComponentConfigurationSyntax::cast(node).unwrap()
    }
}
pub struct ComponentConfigurationEpilogueBuilder {
    end_token: Token,
    for_token: Token,
    semi_colon_token: Token,
}
impl Default for ComponentConfigurationEpilogueBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl ComponentConfigurationEpilogueBuilder {
    pub fn new() -> Self {
        Self {
            end_token: Token::new(
                TokenKind::Keyword(Kw::End),
                Kw::End.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            for_token: Token::new(
                TokenKind::Keyword(Kw::For),
                Kw::For.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            semi_colon_token: Token::new(
                TokenKind::SemiColon,
                TokenKind::SemiColon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_end_token(mut self, t: Token) -> Self {
        self.end_token = t;
        self
    }
    pub fn with_for_token(mut self, t: Token) -> Self {
        self.for_token = t;
        self
    }
    pub fn with_semi_colon_token(mut self, t: Token) -> Self {
        self.semi_colon_token = t;
        self
    }
    pub fn build(self) -> ComponentConfigurationEpilogueSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ComponentConfigurationEpilogue);
        builder.push(self.end_token);
        builder.push(self.for_token);
        builder.push(self.semi_colon_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ComponentConfigurationEpilogueSyntax::cast(node).unwrap()
    }
}
pub struct ComponentConfigurationItemsBuilder {
    semi_colon_terminated_binding_indication: Option<SemiColonTerminatedBindingIndicationSyntax>,
    semi_colon_terminated_verification_unit_binding_indications:
        Vec<SemiColonTerminatedVerificationUnitBindingIndicationSyntax>,
    block_configuration: Option<BlockConfigurationSyntax>,
}
impl Default for ComponentConfigurationItemsBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl ComponentConfigurationItemsBuilder {
    pub fn new() -> Self {
        Self {
            semi_colon_terminated_binding_indication: None,
            semi_colon_terminated_verification_unit_binding_indications: Vec::new(),
            block_configuration: None,
        }
    }
    pub fn with_semi_colon_terminated_binding_indication(
        mut self,
        n: SemiColonTerminatedBindingIndicationSyntax,
    ) -> Self {
        self.semi_colon_terminated_binding_indication = Some(n);
        self
    }
    pub fn add_semi_colon_terminated_verification_unit_binding_indications(
        mut self,
        n: SemiColonTerminatedVerificationUnitBindingIndicationSyntax,
    ) -> Self {
        self.semi_colon_terminated_verification_unit_binding_indications
            .push(n);
        self
    }
    pub fn with_block_configuration(mut self, n: BlockConfigurationSyntax) -> Self {
        self.block_configuration = Some(n);
        self
    }
    pub fn build(self) -> ComponentConfigurationItemsSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ComponentConfigurationItems);
        if let Some(n) = self.semi_colon_terminated_binding_indication {
            builder.push_node(n.raw().green().clone());
        }
        for n in self.semi_colon_terminated_verification_unit_binding_indications {
            builder.push_node(n.raw().green().clone());
        }
        if let Some(n) = self.block_configuration {
            builder.push_node(n.raw().green().clone());
        }
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ComponentConfigurationItemsSyntax::cast(node).unwrap()
    }
}
pub struct ComponentConfigurationPreambleBuilder {
    for_token: Token,
    component_specification: ComponentSpecificationSyntax,
}
impl ComponentConfigurationPreambleBuilder {
    pub fn new(component_specification: ComponentSpecificationSyntax) -> Self {
        Self {
            for_token: Token::new(
                TokenKind::Keyword(Kw::For),
                Kw::For.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            component_specification,
        }
    }
    pub fn with_for_token(mut self, t: Token) -> Self {
        self.for_token = t;
        self
    }
    pub fn with_component_specification(mut self, n: ComponentSpecificationSyntax) -> Self {
        self.component_specification = n;
        self
    }
    pub fn build(self) -> ComponentConfigurationPreambleSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ComponentConfigurationPreamble);
        builder.push(self.for_token);
        builder.push_node(self.component_specification.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ComponentConfigurationPreambleSyntax::cast(node).unwrap()
    }
}
pub struct ComponentDeclarationBuilder {
    component_declaration_preamble: ComponentDeclarationPreambleSyntax,
    component_declaration_items: ComponentDeclarationItemsSyntax,
    component_declaration_epilogue: ComponentDeclarationEpilogueSyntax,
}
impl ComponentDeclarationBuilder {
    pub fn new(component_declaration_preamble: ComponentDeclarationPreambleSyntax) -> Self {
        Self {
            component_declaration_preamble,
            component_declaration_items: ComponentDeclarationItemsBuilder::default().build(),
            component_declaration_epilogue: ComponentDeclarationEpilogueBuilder::default().build(),
        }
    }
    pub fn with_component_declaration_preamble(
        mut self,
        n: ComponentDeclarationPreambleSyntax,
    ) -> Self {
        self.component_declaration_preamble = n;
        self
    }
    pub fn with_component_declaration_items(mut self, n: ComponentDeclarationItemsSyntax) -> Self {
        self.component_declaration_items = n;
        self
    }
    pub fn with_component_declaration_epilogue(
        mut self,
        n: ComponentDeclarationEpilogueSyntax,
    ) -> Self {
        self.component_declaration_epilogue = n;
        self
    }
    pub fn build(self) -> ComponentDeclarationSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ComponentDeclaration);
        builder.push_node(self.component_declaration_preamble.raw().green().clone());
        builder.push_node(self.component_declaration_items.raw().green().clone());
        builder.push_node(self.component_declaration_epilogue.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ComponentDeclarationSyntax::cast(node).unwrap()
    }
}
pub struct ComponentDeclarationEpilogueBuilder {
    end_token: Token,
    component_token: Token,
    identifier_token: Option<Token>,
    semi_colon_token: Token,
}
impl Default for ComponentDeclarationEpilogueBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl ComponentDeclarationEpilogueBuilder {
    pub fn new() -> Self {
        Self {
            end_token: Token::new(
                TokenKind::Keyword(Kw::End),
                Kw::End.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            component_token: Token::new(
                TokenKind::Keyword(Kw::Component),
                Kw::Component.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            identifier_token: None,
            semi_colon_token: Token::new(
                TokenKind::SemiColon,
                TokenKind::SemiColon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_end_token(mut self, t: Token) -> Self {
        self.end_token = t;
        self
    }
    pub fn with_component_token(mut self, t: Token) -> Self {
        self.component_token = t;
        self
    }
    pub fn with_identifier_token(mut self, t: Token) -> Self {
        self.identifier_token = Some(t);
        self
    }
    pub fn with_semi_colon_token(mut self, t: Token) -> Self {
        self.semi_colon_token = t;
        self
    }
    pub fn build(self) -> ComponentDeclarationEpilogueSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ComponentDeclarationEpilogue);
        builder.push(self.end_token);
        builder.push(self.component_token);
        if let Some(t) = self.identifier_token {
            builder.push(t);
        }
        builder.push(self.semi_colon_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ComponentDeclarationEpilogueSyntax::cast(node).unwrap()
    }
}
pub struct ComponentDeclarationItemsBuilder {
    generic_clause: Option<GenericClauseSyntax>,
    port_clause: Option<PortClauseSyntax>,
}
impl Default for ComponentDeclarationItemsBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl ComponentDeclarationItemsBuilder {
    pub fn new() -> Self {
        Self {
            generic_clause: None,
            port_clause: None,
        }
    }
    pub fn with_generic_clause(mut self, n: GenericClauseSyntax) -> Self {
        self.generic_clause = Some(n);
        self
    }
    pub fn with_port_clause(mut self, n: PortClauseSyntax) -> Self {
        self.port_clause = Some(n);
        self
    }
    pub fn build(self) -> ComponentDeclarationItemsSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ComponentDeclarationItems);
        if let Some(n) = self.generic_clause {
            builder.push_node(n.raw().green().clone());
        }
        if let Some(n) = self.port_clause {
            builder.push_node(n.raw().green().clone());
        }
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ComponentDeclarationItemsSyntax::cast(node).unwrap()
    }
}
pub struct ComponentDeclarationPreambleBuilder {
    component_token: Token,
    name_token: Token,
    is_token: Option<Token>,
}
impl ComponentDeclarationPreambleBuilder {
    pub fn new(name_token: Token) -> Self {
        Self {
            component_token: Token::new(
                TokenKind::Keyword(Kw::Component),
                Kw::Component.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            name_token,
            is_token: None,
        }
    }
    pub fn with_component_token(mut self, t: Token) -> Self {
        self.component_token = t;
        self
    }
    pub fn with_name_token(mut self, t: Token) -> Self {
        self.name_token = t;
        self
    }
    pub fn with_is_token(mut self, t: Token) -> Self {
        self.is_token = Some(t);
        self
    }
    pub fn build(self) -> ComponentDeclarationPreambleSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ComponentDeclarationPreamble);
        builder.push(self.component_token);
        builder.push(self.name_token);
        if let Some(t) = self.is_token {
            builder.push(t);
        }
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ComponentDeclarationPreambleSyntax::cast(node).unwrap()
    }
}
pub struct ComponentInstantiatedUnitBuilder {
    component_token: Option<Token>,
    name: NameSyntax,
}
impl ComponentInstantiatedUnitBuilder {
    pub fn new(name: NameSyntax) -> Self {
        Self {
            component_token: None,
            name,
        }
    }
    pub fn with_component_token(mut self, t: Token) -> Self {
        self.component_token = Some(t);
        self
    }
    pub fn with_name(mut self, n: NameSyntax) -> Self {
        self.name = n;
        self
    }
    pub fn build(self) -> ComponentInstantiatedUnitSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ComponentInstantiatedUnit);
        if let Some(t) = self.component_token {
            builder.push(t);
        }
        builder.push_node(self.name.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ComponentInstantiatedUnitSyntax::cast(node).unwrap()
    }
}
pub struct ComponentInstantiationItemsBuilder {
    generic_map_aspect: Option<GenericMapAspectSyntax>,
    port_map_aspect: Option<PortMapAspectSyntax>,
}
impl Default for ComponentInstantiationItemsBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl ComponentInstantiationItemsBuilder {
    pub fn new() -> Self {
        Self {
            generic_map_aspect: None,
            port_map_aspect: None,
        }
    }
    pub fn with_generic_map_aspect(mut self, n: GenericMapAspectSyntax) -> Self {
        self.generic_map_aspect = Some(n);
        self
    }
    pub fn with_port_map_aspect(mut self, n: PortMapAspectSyntax) -> Self {
        self.port_map_aspect = Some(n);
        self
    }
    pub fn build(self) -> ComponentInstantiationItemsSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ComponentInstantiationItems);
        if let Some(n) = self.generic_map_aspect {
            builder.push_node(n.raw().green().clone());
        }
        if let Some(n) = self.port_map_aspect {
            builder.push_node(n.raw().green().clone());
        }
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ComponentInstantiationItemsSyntax::cast(node).unwrap()
    }
}
pub struct ComponentInstantiationStatementBuilder {
    label: LabelSyntax,
    instantiated_unit: InstantiatedUnitSyntax,
    component_instantiation_items: ComponentInstantiationItemsSyntax,
    semi_colon_token: Token,
}
impl ComponentInstantiationStatementBuilder {
    pub fn new(label: LabelSyntax, instantiated_unit: InstantiatedUnitSyntax) -> Self {
        Self {
            label,
            instantiated_unit,
            component_instantiation_items: ComponentInstantiationItemsBuilder::default().build(),
            semi_colon_token: Token::new(
                TokenKind::SemiColon,
                TokenKind::SemiColon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_label(mut self, n: LabelSyntax) -> Self {
        self.label = n;
        self
    }
    pub fn with_instantiated_unit(mut self, n: InstantiatedUnitSyntax) -> Self {
        self.instantiated_unit = n;
        self
    }
    pub fn with_component_instantiation_items(
        mut self,
        n: ComponentInstantiationItemsSyntax,
    ) -> Self {
        self.component_instantiation_items = n;
        self
    }
    pub fn with_semi_colon_token(mut self, t: Token) -> Self {
        self.semi_colon_token = t;
        self
    }
    pub fn build(self) -> ComponentInstantiationStatementSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ComponentInstantiationStatement);
        builder.push_node(self.label.raw().green().clone());
        builder.push_node(self.instantiated_unit.raw().green().clone());
        builder.push_node(self.component_instantiation_items.raw().green().clone());
        builder.push(self.semi_colon_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ComponentInstantiationStatementSyntax::cast(node).unwrap()
    }
}
pub struct ComponentSpecificationBuilder {
    instantiation_list: InstantiationListSyntax,
    colon_token: Token,
    name: NameSyntax,
}
impl ComponentSpecificationBuilder {
    pub fn new(instantiation_list: InstantiationListSyntax, name: NameSyntax) -> Self {
        Self {
            instantiation_list,
            colon_token: Token::new(
                TokenKind::Colon,
                TokenKind::Colon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            name,
        }
    }
    pub fn with_instantiation_list(mut self, n: InstantiationListSyntax) -> Self {
        self.instantiation_list = n;
        self
    }
    pub fn with_colon_token(mut self, t: Token) -> Self {
        self.colon_token = t;
        self
    }
    pub fn with_name(mut self, n: NameSyntax) -> Self {
        self.name = n;
        self
    }
    pub fn build(self) -> ComponentSpecificationSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ComponentSpecification);
        builder.push_node(self.instantiation_list.raw().green().clone());
        builder.push(self.colon_token);
        builder.push_node(self.name.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ComponentSpecificationSyntax::cast(node).unwrap()
    }
}
pub struct CompoundConfigurationSpecificationBuilder {
    component_configuration_preamble: ComponentConfigurationPreambleSyntax,
    compound_configuration_specification_items: CompoundConfigurationSpecificationItemsSyntax,
    component_configuration_epilogue: ComponentConfigurationEpilogueSyntax,
}
impl CompoundConfigurationSpecificationBuilder {
    pub fn new(component_configuration_preamble: ComponentConfigurationPreambleSyntax) -> Self {
        Self {
            component_configuration_preamble,
            compound_configuration_specification_items:
                CompoundConfigurationSpecificationItemsBuilder::default().build(),
            component_configuration_epilogue: ComponentConfigurationEpilogueBuilder::default()
                .build(),
        }
    }
    pub fn with_component_configuration_preamble(
        mut self,
        n: ComponentConfigurationPreambleSyntax,
    ) -> Self {
        self.component_configuration_preamble = n;
        self
    }
    pub fn with_compound_configuration_specification_items(
        mut self,
        n: CompoundConfigurationSpecificationItemsSyntax,
    ) -> Self {
        self.compound_configuration_specification_items = n;
        self
    }
    pub fn with_component_configuration_epilogue(
        mut self,
        n: ComponentConfigurationEpilogueSyntax,
    ) -> Self {
        self.component_configuration_epilogue = n;
        self
    }
    pub fn build(self) -> CompoundConfigurationSpecificationSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::CompoundConfigurationSpecification);
        builder.push_node(self.component_configuration_preamble.raw().green().clone());
        builder.push_node(
            self.compound_configuration_specification_items
                .raw()
                .green()
                .clone(),
        );
        builder.push_node(self.component_configuration_epilogue.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        CompoundConfigurationSpecificationSyntax::cast(node).unwrap()
    }
}
pub struct CompoundConfigurationSpecificationItemsBuilder {
    semi_colon_terminated_binding_indication: SemiColonTerminatedBindingIndicationSyntax,
    verification_unit_binding_indications: Vec<VerificationUnitBindingIndicationSyntax>,
}
impl Default for CompoundConfigurationSpecificationItemsBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl CompoundConfigurationSpecificationItemsBuilder {
    pub fn new() -> Self {
        Self {
            semi_colon_terminated_binding_indication:
                SemiColonTerminatedBindingIndicationBuilder::default().build(),
            verification_unit_binding_indications: Vec::new(),
        }
    }
    pub fn with_semi_colon_terminated_binding_indication(
        mut self,
        n: SemiColonTerminatedBindingIndicationSyntax,
    ) -> Self {
        self.semi_colon_terminated_binding_indication = n;
        self
    }
    pub fn add_verification_unit_binding_indications(
        mut self,
        n: VerificationUnitBindingIndicationSyntax,
    ) -> Self {
        self.verification_unit_binding_indications.push(n);
        self
    }
    pub fn build(self) -> CompoundConfigurationSpecificationItemsSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::CompoundConfigurationSpecificationItems);
        builder.push_node(
            self.semi_colon_terminated_binding_indication
                .raw()
                .green()
                .clone(),
        );
        for n in self.verification_unit_binding_indications {
            builder.push_node(n.raw().green().clone());
        }
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        CompoundConfigurationSpecificationItemsSyntax::cast(node).unwrap()
    }
}
pub struct ConcurrentAssertionStatementBuilder {
    label: Option<LabelSyntax>,
    postponed_token: Option<Token>,
    assertion: AssertionSyntax,
    semi_colon_token: Token,
}
impl ConcurrentAssertionStatementBuilder {
    pub fn new(assertion: AssertionSyntax) -> Self {
        Self {
            label: None,
            postponed_token: None,
            assertion,
            semi_colon_token: Token::new(
                TokenKind::SemiColon,
                TokenKind::SemiColon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_label(mut self, n: LabelSyntax) -> Self {
        self.label = Some(n);
        self
    }
    pub fn with_postponed_token(mut self, t: Token) -> Self {
        self.postponed_token = Some(t);
        self
    }
    pub fn with_assertion(mut self, n: AssertionSyntax) -> Self {
        self.assertion = n;
        self
    }
    pub fn with_semi_colon_token(mut self, t: Token) -> Self {
        self.semi_colon_token = t;
        self
    }
    pub fn build(self) -> ConcurrentAssertionStatementSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ConcurrentAssertionStatement);
        if let Some(n) = self.label {
            builder.push_node(n.raw().green().clone());
        }
        if let Some(t) = self.postponed_token {
            builder.push(t);
        }
        builder.push_node(self.assertion.raw().green().clone());
        builder.push(self.semi_colon_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ConcurrentAssertionStatementSyntax::cast(node).unwrap()
    }
}
pub struct ConcurrentConditionalSignalAssignmentBuilder {
    label: Option<LabelSyntax>,
    postponed_token: Option<Token>,
    target: TargetSyntax,
    lte_token: Token,
    guarded_token: Option<Token>,
    delay_mechanism: Option<DelayMechanismSyntax>,
    conditional_waveforms: ConditionalWaveformsSyntax,
    semi_colon_token: Token,
}
impl ConcurrentConditionalSignalAssignmentBuilder {
    pub fn new(target: TargetSyntax, conditional_waveforms: ConditionalWaveformsSyntax) -> Self {
        Self {
            label: None,
            postponed_token: None,
            target,
            lte_token: Token::new(
                TokenKind::LTE,
                TokenKind::LTE.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            guarded_token: None,
            delay_mechanism: None,
            conditional_waveforms,
            semi_colon_token: Token::new(
                TokenKind::SemiColon,
                TokenKind::SemiColon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_label(mut self, n: LabelSyntax) -> Self {
        self.label = Some(n);
        self
    }
    pub fn with_postponed_token(mut self, t: Token) -> Self {
        self.postponed_token = Some(t);
        self
    }
    pub fn with_target(mut self, n: TargetSyntax) -> Self {
        self.target = n;
        self
    }
    pub fn with_lte_token(mut self, t: Token) -> Self {
        self.lte_token = t;
        self
    }
    pub fn with_guarded_token(mut self, t: Token) -> Self {
        self.guarded_token = Some(t);
        self
    }
    pub fn with_delay_mechanism(mut self, n: DelayMechanismSyntax) -> Self {
        self.delay_mechanism = Some(n);
        self
    }
    pub fn with_conditional_waveforms(mut self, n: ConditionalWaveformsSyntax) -> Self {
        self.conditional_waveforms = n;
        self
    }
    pub fn with_semi_colon_token(mut self, t: Token) -> Self {
        self.semi_colon_token = t;
        self
    }
    pub fn build(self) -> ConcurrentConditionalSignalAssignmentSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ConcurrentConditionalSignalAssignment);
        if let Some(n) = self.label {
            builder.push_node(n.raw().green().clone());
        }
        if let Some(t) = self.postponed_token {
            builder.push(t);
        }
        builder.push_node(self.target.raw().green().clone());
        builder.push(self.lte_token);
        if let Some(t) = self.guarded_token {
            builder.push(t);
        }
        if let Some(n) = self.delay_mechanism {
            builder.push_node(n.raw().green().clone());
        }
        builder.push_node(self.conditional_waveforms.raw().green().clone());
        builder.push(self.semi_colon_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ConcurrentConditionalSignalAssignmentSyntax::cast(node).unwrap()
    }
}
pub struct ConcurrentProcedureCallOrComponentInstantiationStatementBuilder {
    label: Option<LabelSyntax>,
    postponed_token: Option<Token>,
    name: NameSyntax,
    semi_colon_token: Token,
}
impl ConcurrentProcedureCallOrComponentInstantiationStatementBuilder {
    pub fn new(name: NameSyntax) -> Self {
        Self {
            label: None,
            postponed_token: None,
            name,
            semi_colon_token: Token::new(
                TokenKind::SemiColon,
                TokenKind::SemiColon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_label(mut self, n: LabelSyntax) -> Self {
        self.label = Some(n);
        self
    }
    pub fn with_postponed_token(mut self, t: Token) -> Self {
        self.postponed_token = Some(t);
        self
    }
    pub fn with_name(mut self, n: NameSyntax) -> Self {
        self.name = n;
        self
    }
    pub fn with_semi_colon_token(mut self, t: Token) -> Self {
        self.semi_colon_token = t;
        self
    }
    pub fn build(self) -> ConcurrentProcedureCallOrComponentInstantiationStatementSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ConcurrentProcedureCallOrComponentInstantiationStatement);
        if let Some(n) = self.label {
            builder.push_node(n.raw().green().clone());
        }
        if let Some(t) = self.postponed_token {
            builder.push(t);
        }
        builder.push_node(self.name.raw().green().clone());
        builder.push(self.semi_colon_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ConcurrentProcedureCallOrComponentInstantiationStatementSyntax::cast(node).unwrap()
    }
}
pub struct ConcurrentSelectedSignalAssignmentBuilder {
    concurrent_selected_signal_assignment_preamble:
        ConcurrentSelectedSignalAssignmentPreambleSyntax,
    target: TargetSyntax,
    lte_token: Token,
    guarded_token: Option<Token>,
    delay_mechanism: Option<DelayMechanismSyntax>,
    selected_waveforms: SelectedWaveformsSyntax,
    semi_colon_token: Token,
}
impl ConcurrentSelectedSignalAssignmentBuilder {
    pub fn new(
        concurrent_selected_signal_assignment_preamble : ConcurrentSelectedSignalAssignmentPreambleSyntax,
        target: TargetSyntax,
    ) -> Self {
        Self {
            concurrent_selected_signal_assignment_preamble,
            target,
            lte_token: Token::new(
                TokenKind::LTE,
                TokenKind::LTE.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            guarded_token: None,
            delay_mechanism: None,
            selected_waveforms: SelectedWaveformsBuilder::default().build(),
            semi_colon_token: Token::new(
                TokenKind::SemiColon,
                TokenKind::SemiColon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_concurrent_selected_signal_assignment_preamble(
        mut self,
        n: ConcurrentSelectedSignalAssignmentPreambleSyntax,
    ) -> Self {
        self.concurrent_selected_signal_assignment_preamble = n;
        self
    }
    pub fn with_target(mut self, n: TargetSyntax) -> Self {
        self.target = n;
        self
    }
    pub fn with_lte_token(mut self, t: Token) -> Self {
        self.lte_token = t;
        self
    }
    pub fn with_guarded_token(mut self, t: Token) -> Self {
        self.guarded_token = Some(t);
        self
    }
    pub fn with_delay_mechanism(mut self, n: DelayMechanismSyntax) -> Self {
        self.delay_mechanism = Some(n);
        self
    }
    pub fn with_selected_waveforms(mut self, n: SelectedWaveformsSyntax) -> Self {
        self.selected_waveforms = n;
        self
    }
    pub fn with_semi_colon_token(mut self, t: Token) -> Self {
        self.semi_colon_token = t;
        self
    }
    pub fn build(self) -> ConcurrentSelectedSignalAssignmentSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ConcurrentSelectedSignalAssignment);
        builder.push_node(
            self.concurrent_selected_signal_assignment_preamble
                .raw()
                .green()
                .clone(),
        );
        builder.push_node(self.target.raw().green().clone());
        builder.push(self.lte_token);
        if let Some(t) = self.guarded_token {
            builder.push(t);
        }
        if let Some(n) = self.delay_mechanism {
            builder.push_node(n.raw().green().clone());
        }
        builder.push_node(self.selected_waveforms.raw().green().clone());
        builder.push(self.semi_colon_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ConcurrentSelectedSignalAssignmentSyntax::cast(node).unwrap()
    }
}
pub struct ConcurrentSelectedSignalAssignmentPreambleBuilder {
    label: Option<LabelSyntax>,
    postponed_token: Option<Token>,
    with_token: Token,
    expression: ExpressionSyntax,
    select_token: Token,
    que_token: Option<Token>,
}
impl ConcurrentSelectedSignalAssignmentPreambleBuilder {
    pub fn new(expression: ExpressionSyntax) -> Self {
        Self {
            label: None,
            postponed_token: None,
            with_token: Token::new(
                TokenKind::Keyword(Kw::With),
                Kw::With.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            expression,
            select_token: Token::new(
                TokenKind::Keyword(Kw::Select),
                Kw::Select.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            que_token: None,
        }
    }
    pub fn with_label(mut self, n: LabelSyntax) -> Self {
        self.label = Some(n);
        self
    }
    pub fn with_postponed_token(mut self, t: Token) -> Self {
        self.postponed_token = Some(t);
        self
    }
    pub fn with_with_token(mut self, t: Token) -> Self {
        self.with_token = t;
        self
    }
    pub fn with_expression(mut self, n: ExpressionSyntax) -> Self {
        self.expression = n;
        self
    }
    pub fn with_select_token(mut self, t: Token) -> Self {
        self.select_token = t;
        self
    }
    pub fn with_que_token(mut self, t: Token) -> Self {
        self.que_token = Some(t);
        self
    }
    pub fn build(self) -> ConcurrentSelectedSignalAssignmentPreambleSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ConcurrentSelectedSignalAssignmentPreamble);
        if let Some(n) = self.label {
            builder.push_node(n.raw().green().clone());
        }
        if let Some(t) = self.postponed_token {
            builder.push(t);
        }
        builder.push(self.with_token);
        builder.push_node(self.expression.raw().green().clone());
        builder.push(self.select_token);
        if let Some(t) = self.que_token {
            builder.push(t);
        }
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ConcurrentSelectedSignalAssignmentPreambleSyntax::cast(node).unwrap()
    }
}
pub struct ConcurrentSimpleSignalAssignmentBuilder {
    label: Option<LabelSyntax>,
    postponed_token: Option<Token>,
    target: TargetSyntax,
    lte_token: Token,
    guarded_token: Option<Token>,
    delay_mechanism: Option<DelayMechanismSyntax>,
    waveform: WaveformSyntax,
    semi_colon_token: Token,
}
impl ConcurrentSimpleSignalAssignmentBuilder {
    pub fn new(target: TargetSyntax, waveform: WaveformSyntax) -> Self {
        Self {
            label: None,
            postponed_token: None,
            target,
            lte_token: Token::new(
                TokenKind::LTE,
                TokenKind::LTE.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            guarded_token: None,
            delay_mechanism: None,
            waveform,
            semi_colon_token: Token::new(
                TokenKind::SemiColon,
                TokenKind::SemiColon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_label(mut self, n: LabelSyntax) -> Self {
        self.label = Some(n);
        self
    }
    pub fn with_postponed_token(mut self, t: Token) -> Self {
        self.postponed_token = Some(t);
        self
    }
    pub fn with_target(mut self, n: TargetSyntax) -> Self {
        self.target = n;
        self
    }
    pub fn with_lte_token(mut self, t: Token) -> Self {
        self.lte_token = t;
        self
    }
    pub fn with_guarded_token(mut self, t: Token) -> Self {
        self.guarded_token = Some(t);
        self
    }
    pub fn with_delay_mechanism(mut self, n: DelayMechanismSyntax) -> Self {
        self.delay_mechanism = Some(n);
        self
    }
    pub fn with_waveform(mut self, n: WaveformSyntax) -> Self {
        self.waveform = n;
        self
    }
    pub fn with_semi_colon_token(mut self, t: Token) -> Self {
        self.semi_colon_token = t;
        self
    }
    pub fn build(self) -> ConcurrentSimpleSignalAssignmentSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ConcurrentSimpleSignalAssignment);
        if let Some(n) = self.label {
            builder.push_node(n.raw().green().clone());
        }
        if let Some(t) = self.postponed_token {
            builder.push(t);
        }
        builder.push_node(self.target.raw().green().clone());
        builder.push(self.lte_token);
        if let Some(t) = self.guarded_token {
            builder.push(t);
        }
        if let Some(n) = self.delay_mechanism {
            builder.push_node(n.raw().green().clone());
        }
        builder.push_node(self.waveform.raw().green().clone());
        builder.push(self.semi_colon_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ConcurrentSimpleSignalAssignmentSyntax::cast(node).unwrap()
    }
}
pub struct ConcurrentStatementsBuilder {
    concurrent_statements: Vec<ConcurrentStatementSyntax>,
}
impl Default for ConcurrentStatementsBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl ConcurrentStatementsBuilder {
    pub fn new() -> Self {
        Self {
            concurrent_statements: Vec::new(),
        }
    }
    pub fn add_concurrent_statements(mut self, n: ConcurrentStatementSyntax) -> Self {
        self.concurrent_statements.push(n);
        self
    }
    pub fn build(self) -> ConcurrentStatementsSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ConcurrentStatements);
        for n in self.concurrent_statements {
            builder.push_node(n.raw().green().clone());
        }
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ConcurrentStatementsSyntax::cast(node).unwrap()
    }
}
pub struct ConditionClauseBuilder {
    until_token: Token,
    expression: ExpressionSyntax,
}
impl ConditionClauseBuilder {
    pub fn new(expression: ExpressionSyntax) -> Self {
        Self {
            until_token: Token::new(
                TokenKind::Keyword(Kw::Until),
                Kw::Until.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            expression,
        }
    }
    pub fn with_until_token(mut self, t: Token) -> Self {
        self.until_token = t;
        self
    }
    pub fn with_expression(mut self, n: ExpressionSyntax) -> Self {
        self.expression = n;
        self
    }
    pub fn build(self) -> ConditionClauseSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ConditionClause);
        builder.push(self.until_token);
        builder.push_node(self.expression.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ConditionClauseSyntax::cast(node).unwrap()
    }
}
pub struct ConditionalElseItemBuilder {
    else_token: Token,
    expression: ExpressionSyntax,
}
impl ConditionalElseItemBuilder {
    pub fn new(expression: ExpressionSyntax) -> Self {
        Self {
            else_token: Token::new(
                TokenKind::Keyword(Kw::Else),
                Kw::Else.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            expression,
        }
    }
    pub fn with_else_token(mut self, t: Token) -> Self {
        self.else_token = t;
        self
    }
    pub fn with_expression(mut self, n: ExpressionSyntax) -> Self {
        self.expression = n;
        self
    }
    pub fn build(self) -> ConditionalElseItemSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ConditionalElseItem);
        builder.push(self.else_token);
        builder.push_node(self.expression.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ConditionalElseItemSyntax::cast(node).unwrap()
    }
}
pub struct ConditionalElseWhenExpressionBuilder {
    else_token: Token,
    expression: ExpressionSyntax,
    when_token: Token,
    condition: ExpressionSyntax,
}
impl ConditionalElseWhenExpressionBuilder {
    pub fn new(expression: ExpressionSyntax, condition: ExpressionSyntax) -> Self {
        Self {
            else_token: Token::new(
                TokenKind::Keyword(Kw::Else),
                Kw::Else.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            expression,
            when_token: Token::new(
                TokenKind::Keyword(Kw::When),
                Kw::When.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            condition,
        }
    }
    pub fn with_else_token(mut self, t: Token) -> Self {
        self.else_token = t;
        self
    }
    pub fn with_expression(mut self, n: ExpressionSyntax) -> Self {
        self.expression = n;
        self
    }
    pub fn with_when_token(mut self, t: Token) -> Self {
        self.when_token = t;
        self
    }
    pub fn with_condition(mut self, n: ExpressionSyntax) -> Self {
        self.condition = n;
        self
    }
    pub fn build(self) -> ConditionalElseWhenExpressionSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ConditionalElseWhenExpression);
        builder.push(self.else_token);
        builder.push_node(self.expression.raw().green().clone());
        builder.push(self.when_token);
        builder.push_node(self.condition.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ConditionalElseWhenExpressionSyntax::cast(node).unwrap()
    }
}
pub struct ConditionalExpressionBuilder {
    expression: ExpressionSyntax,
    when_token: Token,
    condition: ExpressionSyntax,
}
impl ConditionalExpressionBuilder {
    pub fn new(expression: ExpressionSyntax, condition: ExpressionSyntax) -> Self {
        Self {
            expression,
            when_token: Token::new(
                TokenKind::Keyword(Kw::When),
                Kw::When.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            condition,
        }
    }
    pub fn with_expression(mut self, n: ExpressionSyntax) -> Self {
        self.expression = n;
        self
    }
    pub fn with_when_token(mut self, t: Token) -> Self {
        self.when_token = t;
        self
    }
    pub fn with_condition(mut self, n: ExpressionSyntax) -> Self {
        self.condition = n;
        self
    }
    pub fn build(self) -> ConditionalExpressionSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ConditionalExpression);
        builder.push_node(self.expression.raw().green().clone());
        builder.push(self.when_token);
        builder.push_node(self.condition.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ConditionalExpressionSyntax::cast(node).unwrap()
    }
}
pub struct ConditionalExpressionsBuilder {
    conditional_expression: ConditionalExpressionSyntax,
    conditional_else_when_expressions: Vec<ConditionalElseWhenExpressionSyntax>,
    conditional_else_item: Option<ConditionalElseItemSyntax>,
}
impl ConditionalExpressionsBuilder {
    pub fn new(conditional_expression: ConditionalExpressionSyntax) -> Self {
        Self {
            conditional_expression,
            conditional_else_when_expressions: Vec::new(),
            conditional_else_item: None,
        }
    }
    pub fn with_conditional_expression(mut self, n: ConditionalExpressionSyntax) -> Self {
        self.conditional_expression = n;
        self
    }
    pub fn add_conditional_else_when_expressions(
        mut self,
        n: ConditionalElseWhenExpressionSyntax,
    ) -> Self {
        self.conditional_else_when_expressions.push(n);
        self
    }
    pub fn with_conditional_else_item(mut self, n: ConditionalElseItemSyntax) -> Self {
        self.conditional_else_item = Some(n);
        self
    }
    pub fn build(self) -> ConditionalExpressionsSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ConditionalExpressions);
        builder.push_node(self.conditional_expression.raw().green().clone());
        for n in self.conditional_else_when_expressions {
            builder.push_node(n.raw().green().clone());
        }
        if let Some(n) = self.conditional_else_item {
            builder.push_node(n.raw().green().clone());
        }
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ConditionalExpressionsSyntax::cast(node).unwrap()
    }
}
pub struct ConditionalForceAssignmentBuilder {
    target: TargetSyntax,
    lte_token: Token,
    force_token: Token,
    force_mode: Option<ForceModeSyntax>,
    conditional_expressions: ConditionalExpressionsSyntax,
    semi_colon_token: Token,
}
impl ConditionalForceAssignmentBuilder {
    pub fn new(
        target: TargetSyntax,
        conditional_expressions: ConditionalExpressionsSyntax,
    ) -> Self {
        Self {
            target,
            lte_token: Token::new(
                TokenKind::LTE,
                TokenKind::LTE.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            force_token: Token::new(
                TokenKind::Keyword(Kw::Force),
                Kw::Force.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            force_mode: None,
            conditional_expressions,
            semi_colon_token: Token::new(
                TokenKind::SemiColon,
                TokenKind::SemiColon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_target(mut self, n: TargetSyntax) -> Self {
        self.target = n;
        self
    }
    pub fn with_lte_token(mut self, t: Token) -> Self {
        self.lte_token = t;
        self
    }
    pub fn with_force_token(mut self, t: Token) -> Self {
        self.force_token = t;
        self
    }
    pub fn with_force_mode(mut self, n: ForceModeSyntax) -> Self {
        self.force_mode = Some(n);
        self
    }
    pub fn with_conditional_expressions(mut self, n: ConditionalExpressionsSyntax) -> Self {
        self.conditional_expressions = n;
        self
    }
    pub fn with_semi_colon_token(mut self, t: Token) -> Self {
        self.semi_colon_token = t;
        self
    }
    pub fn build(self) -> ConditionalForceAssignmentSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ConditionalForceAssignment);
        builder.push_node(self.target.raw().green().clone());
        builder.push(self.lte_token);
        builder.push(self.force_token);
        if let Some(n) = self.force_mode {
            builder.push(n.raw().token().clone());
        }
        builder.push_node(self.conditional_expressions.raw().green().clone());
        builder.push(self.semi_colon_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ConditionalForceAssignmentSyntax::cast(node).unwrap()
    }
}
pub struct ConditionalVariableAssignmentBuilder {
    target: TargetSyntax,
    colon_eq_token: Token,
    conditional_expressions: ConditionalExpressionsSyntax,
    semi_colon_token: Token,
}
impl ConditionalVariableAssignmentBuilder {
    pub fn new(
        target: TargetSyntax,
        conditional_expressions: ConditionalExpressionsSyntax,
    ) -> Self {
        Self {
            target,
            colon_eq_token: Token::new(
                TokenKind::ColonEq,
                TokenKind::ColonEq.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            conditional_expressions,
            semi_colon_token: Token::new(
                TokenKind::SemiColon,
                TokenKind::SemiColon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_target(mut self, n: TargetSyntax) -> Self {
        self.target = n;
        self
    }
    pub fn with_colon_eq_token(mut self, t: Token) -> Self {
        self.colon_eq_token = t;
        self
    }
    pub fn with_conditional_expressions(mut self, n: ConditionalExpressionsSyntax) -> Self {
        self.conditional_expressions = n;
        self
    }
    pub fn with_semi_colon_token(mut self, t: Token) -> Self {
        self.semi_colon_token = t;
        self
    }
    pub fn build(self) -> ConditionalVariableAssignmentSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ConditionalVariableAssignment);
        builder.push_node(self.target.raw().green().clone());
        builder.push(self.colon_eq_token);
        builder.push_node(self.conditional_expressions.raw().green().clone());
        builder.push(self.semi_colon_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ConditionalVariableAssignmentSyntax::cast(node).unwrap()
    }
}
pub struct ConditionalWaveformBuilder {
    waveform: WaveformSyntax,
    when_token: Token,
    expression: ExpressionSyntax,
}
impl ConditionalWaveformBuilder {
    pub fn new(waveform: WaveformSyntax, expression: ExpressionSyntax) -> Self {
        Self {
            waveform,
            when_token: Token::new(
                TokenKind::Keyword(Kw::When),
                Kw::When.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            expression,
        }
    }
    pub fn with_waveform(mut self, n: WaveformSyntax) -> Self {
        self.waveform = n;
        self
    }
    pub fn with_when_token(mut self, t: Token) -> Self {
        self.when_token = t;
        self
    }
    pub fn with_expression(mut self, n: ExpressionSyntax) -> Self {
        self.expression = n;
        self
    }
    pub fn build(self) -> ConditionalWaveformSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ConditionalWaveform);
        builder.push_node(self.waveform.raw().green().clone());
        builder.push(self.when_token);
        builder.push_node(self.expression.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ConditionalWaveformSyntax::cast(node).unwrap()
    }
}
pub struct ConditionalWaveformAssignmentBuilder {
    target: TargetSyntax,
    lte_token: Token,
    delay_mechanism: Option<DelayMechanismSyntax>,
    conditional_waveforms: ConditionalWaveformsSyntax,
    semi_colon_token: Token,
}
impl ConditionalWaveformAssignmentBuilder {
    pub fn new(target: TargetSyntax, conditional_waveforms: ConditionalWaveformsSyntax) -> Self {
        Self {
            target,
            lte_token: Token::new(
                TokenKind::LTE,
                TokenKind::LTE.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            delay_mechanism: None,
            conditional_waveforms,
            semi_colon_token: Token::new(
                TokenKind::SemiColon,
                TokenKind::SemiColon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_target(mut self, n: TargetSyntax) -> Self {
        self.target = n;
        self
    }
    pub fn with_lte_token(mut self, t: Token) -> Self {
        self.lte_token = t;
        self
    }
    pub fn with_delay_mechanism(mut self, n: DelayMechanismSyntax) -> Self {
        self.delay_mechanism = Some(n);
        self
    }
    pub fn with_conditional_waveforms(mut self, n: ConditionalWaveformsSyntax) -> Self {
        self.conditional_waveforms = n;
        self
    }
    pub fn with_semi_colon_token(mut self, t: Token) -> Self {
        self.semi_colon_token = t;
        self
    }
    pub fn build(self) -> ConditionalWaveformAssignmentSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ConditionalWaveformAssignment);
        builder.push_node(self.target.raw().green().clone());
        builder.push(self.lte_token);
        if let Some(n) = self.delay_mechanism {
            builder.push_node(n.raw().green().clone());
        }
        builder.push_node(self.conditional_waveforms.raw().green().clone());
        builder.push(self.semi_colon_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ConditionalWaveformAssignmentSyntax::cast(node).unwrap()
    }
}
pub struct ConditionalWaveformElseItemBuilder {
    else_token: Token,
    waveform: WaveformSyntax,
}
impl ConditionalWaveformElseItemBuilder {
    pub fn new(waveform: WaveformSyntax) -> Self {
        Self {
            else_token: Token::new(
                TokenKind::Keyword(Kw::Else),
                Kw::Else.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            waveform,
        }
    }
    pub fn with_else_token(mut self, t: Token) -> Self {
        self.else_token = t;
        self
    }
    pub fn with_waveform(mut self, n: WaveformSyntax) -> Self {
        self.waveform = n;
        self
    }
    pub fn build(self) -> ConditionalWaveformElseItemSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ConditionalWaveformElseItem);
        builder.push(self.else_token);
        builder.push_node(self.waveform.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ConditionalWaveformElseItemSyntax::cast(node).unwrap()
    }
}
pub struct ConditionalWaveformElseWhenExpressionBuilder {
    else_token: Token,
    waveform: WaveformSyntax,
    when_token: Token,
    condition: ExpressionSyntax,
}
impl ConditionalWaveformElseWhenExpressionBuilder {
    pub fn new(waveform: WaveformSyntax, condition: ExpressionSyntax) -> Self {
        Self {
            else_token: Token::new(
                TokenKind::Keyword(Kw::Else),
                Kw::Else.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            waveform,
            when_token: Token::new(
                TokenKind::Keyword(Kw::When),
                Kw::When.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            condition,
        }
    }
    pub fn with_else_token(mut self, t: Token) -> Self {
        self.else_token = t;
        self
    }
    pub fn with_waveform(mut self, n: WaveformSyntax) -> Self {
        self.waveform = n;
        self
    }
    pub fn with_when_token(mut self, t: Token) -> Self {
        self.when_token = t;
        self
    }
    pub fn with_condition(mut self, n: ExpressionSyntax) -> Self {
        self.condition = n;
        self
    }
    pub fn build(self) -> ConditionalWaveformElseWhenExpressionSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ConditionalWaveformElseWhenExpression);
        builder.push(self.else_token);
        builder.push_node(self.waveform.raw().green().clone());
        builder.push(self.when_token);
        builder.push_node(self.condition.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ConditionalWaveformElseWhenExpressionSyntax::cast(node).unwrap()
    }
}
pub struct ConditionalWaveformsBuilder {
    conditional_waveform: ConditionalWaveformSyntax,
    conditional_waveform_else_when_expressions: Vec<ConditionalWaveformElseWhenExpressionSyntax>,
    conditional_waveform_else_item: Option<ConditionalWaveformElseItemSyntax>,
}
impl ConditionalWaveformsBuilder {
    pub fn new(conditional_waveform: ConditionalWaveformSyntax) -> Self {
        Self {
            conditional_waveform,
            conditional_waveform_else_when_expressions: Vec::new(),
            conditional_waveform_else_item: None,
        }
    }
    pub fn with_conditional_waveform(mut self, n: ConditionalWaveformSyntax) -> Self {
        self.conditional_waveform = n;
        self
    }
    pub fn add_conditional_waveform_else_when_expressions(
        mut self,
        n: ConditionalWaveformElseWhenExpressionSyntax,
    ) -> Self {
        self.conditional_waveform_else_when_expressions.push(n);
        self
    }
    pub fn with_conditional_waveform_else_item(
        mut self,
        n: ConditionalWaveformElseItemSyntax,
    ) -> Self {
        self.conditional_waveform_else_item = Some(n);
        self
    }
    pub fn build(self) -> ConditionalWaveformsSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ConditionalWaveforms);
        builder.push_node(self.conditional_waveform.raw().green().clone());
        for n in self.conditional_waveform_else_when_expressions {
            builder.push_node(n.raw().green().clone());
        }
        if let Some(n) = self.conditional_waveform_else_item {
            builder.push_node(n.raw().green().clone());
        }
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ConditionalWaveformsSyntax::cast(node).unwrap()
    }
}
pub struct ConfigurationDeclarationBuilder {
    configuration_declaration_preamble: ConfigurationDeclarationPreambleSyntax,
    configuration_declaration_items: ConfigurationDeclarationItemsSyntax,
    configuration_declaration_epilogue: ConfigurationDeclarationEpilogueSyntax,
}
impl ConfigurationDeclarationBuilder {
    pub fn new(
        configuration_declaration_preamble: ConfigurationDeclarationPreambleSyntax,
        configuration_declaration_items: ConfigurationDeclarationItemsSyntax,
    ) -> Self {
        Self {
            configuration_declaration_preamble,
            configuration_declaration_items,
            configuration_declaration_epilogue: ConfigurationDeclarationEpilogueBuilder::default()
                .build(),
        }
    }
    pub fn with_configuration_declaration_preamble(
        mut self,
        n: ConfigurationDeclarationPreambleSyntax,
    ) -> Self {
        self.configuration_declaration_preamble = n;
        self
    }
    pub fn with_configuration_declaration_items(
        mut self,
        n: ConfigurationDeclarationItemsSyntax,
    ) -> Self {
        self.configuration_declaration_items = n;
        self
    }
    pub fn with_configuration_declaration_epilogue(
        mut self,
        n: ConfigurationDeclarationEpilogueSyntax,
    ) -> Self {
        self.configuration_declaration_epilogue = n;
        self
    }
    pub fn build(self) -> ConfigurationDeclarationSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ConfigurationDeclaration);
        builder.push_node(
            self.configuration_declaration_preamble
                .raw()
                .green()
                .clone(),
        );
        builder.push_node(self.configuration_declaration_items.raw().green().clone());
        builder.push_node(
            self.configuration_declaration_epilogue
                .raw()
                .green()
                .clone(),
        );
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ConfigurationDeclarationSyntax::cast(node).unwrap()
    }
}
pub struct ConfigurationDeclarationEpilogueBuilder {
    end_token: Token,
    configuration_token: Option<Token>,
    identifier_token: Option<Token>,
    semi_colon_token: Token,
}
impl Default for ConfigurationDeclarationEpilogueBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl ConfigurationDeclarationEpilogueBuilder {
    pub fn new() -> Self {
        Self {
            end_token: Token::new(
                TokenKind::Keyword(Kw::End),
                Kw::End.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            configuration_token: None,
            identifier_token: None,
            semi_colon_token: Token::new(
                TokenKind::SemiColon,
                TokenKind::SemiColon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_end_token(mut self, t: Token) -> Self {
        self.end_token = t;
        self
    }
    pub fn with_configuration_token(mut self, t: Token) -> Self {
        self.configuration_token = Some(t);
        self
    }
    pub fn with_identifier_token(mut self, t: Token) -> Self {
        self.identifier_token = Some(t);
        self
    }
    pub fn with_semi_colon_token(mut self, t: Token) -> Self {
        self.semi_colon_token = t;
        self
    }
    pub fn build(self) -> ConfigurationDeclarationEpilogueSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ConfigurationDeclarationEpilogue);
        builder.push(self.end_token);
        if let Some(t) = self.configuration_token {
            builder.push(t);
        }
        if let Some(t) = self.identifier_token {
            builder.push(t);
        }
        builder.push(self.semi_colon_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ConfigurationDeclarationEpilogueSyntax::cast(node).unwrap()
    }
}
pub struct ConfigurationDeclarationItemsBuilder {
    declarations: DeclarationsSyntax,
    semi_colon_terminated_verification_unit_binding_indications:
        Vec<SemiColonTerminatedVerificationUnitBindingIndicationSyntax>,
    block_configuration: BlockConfigurationSyntax,
}
impl ConfigurationDeclarationItemsBuilder {
    pub fn new(block_configuration: BlockConfigurationSyntax) -> Self {
        Self {
            declarations: DeclarationsBuilder::default().build(),
            semi_colon_terminated_verification_unit_binding_indications: Vec::new(),
            block_configuration,
        }
    }
    pub fn with_declarations(mut self, n: DeclarationsSyntax) -> Self {
        self.declarations = n;
        self
    }
    pub fn add_semi_colon_terminated_verification_unit_binding_indications(
        mut self,
        n: SemiColonTerminatedVerificationUnitBindingIndicationSyntax,
    ) -> Self {
        self.semi_colon_terminated_verification_unit_binding_indications
            .push(n);
        self
    }
    pub fn with_block_configuration(mut self, n: BlockConfigurationSyntax) -> Self {
        self.block_configuration = n;
        self
    }
    pub fn build(self) -> ConfigurationDeclarationItemsSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ConfigurationDeclarationItems);
        builder.push_node(self.declarations.raw().green().clone());
        for n in self.semi_colon_terminated_verification_unit_binding_indications {
            builder.push_node(n.raw().green().clone());
        }
        builder.push_node(self.block_configuration.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ConfigurationDeclarationItemsSyntax::cast(node).unwrap()
    }
}
pub struct ConfigurationDeclarationPreambleBuilder {
    configuration_token: Token,
    name_token: Token,
    of_token: Token,
    entity_name: NameSyntax,
    is_token: Token,
}
impl ConfigurationDeclarationPreambleBuilder {
    pub fn new(name_token: Token, entity_name: NameSyntax) -> Self {
        Self {
            configuration_token: Token::new(
                TokenKind::Keyword(Kw::Configuration),
                Kw::Configuration.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            name_token,
            of_token: Token::new(
                TokenKind::Keyword(Kw::Of),
                Kw::Of.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            entity_name,
            is_token: Token::new(
                TokenKind::Keyword(Kw::Is),
                Kw::Is.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_configuration_token(mut self, t: Token) -> Self {
        self.configuration_token = t;
        self
    }
    pub fn with_name_token(mut self, t: Token) -> Self {
        self.name_token = t;
        self
    }
    pub fn with_of_token(mut self, t: Token) -> Self {
        self.of_token = t;
        self
    }
    pub fn with_entity_name(mut self, n: NameSyntax) -> Self {
        self.entity_name = n;
        self
    }
    pub fn with_is_token(mut self, t: Token) -> Self {
        self.is_token = t;
        self
    }
    pub fn build(self) -> ConfigurationDeclarationPreambleSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ConfigurationDeclarationPreamble);
        builder.push(self.configuration_token);
        builder.push(self.name_token);
        builder.push(self.of_token);
        builder.push_node(self.entity_name.raw().green().clone());
        builder.push(self.is_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ConfigurationDeclarationPreambleSyntax::cast(node).unwrap()
    }
}
pub struct ConfigurationInstantiatedUnitBuilder {
    configuration_token: Token,
    name: NameSyntax,
}
impl ConfigurationInstantiatedUnitBuilder {
    pub fn new(name: NameSyntax) -> Self {
        Self {
            configuration_token: Token::new(
                TokenKind::Keyword(Kw::Configuration),
                Kw::Configuration.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            name,
        }
    }
    pub fn with_configuration_token(mut self, t: Token) -> Self {
        self.configuration_token = t;
        self
    }
    pub fn with_name(mut self, n: NameSyntax) -> Self {
        self.name = n;
        self
    }
    pub fn build(self) -> ConfigurationInstantiatedUnitSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ConfigurationInstantiatedUnit);
        builder.push(self.configuration_token);
        builder.push_node(self.name.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ConfigurationInstantiatedUnitSyntax::cast(node).unwrap()
    }
}
pub struct ConstantDeclarationBuilder {
    constant_token: Token,
    identifier_list: IdentifierListSyntax,
    colon_token: Token,
    subtype_indication: SubtypeIndicationSyntax,
    colon_eq_token: Option<Token>,
    expression: Option<ExpressionSyntax>,
    semi_colon_token: Token,
}
impl ConstantDeclarationBuilder {
    pub fn new(
        identifier_list: IdentifierListSyntax,
        subtype_indication: SubtypeIndicationSyntax,
    ) -> Self {
        Self {
            constant_token: Token::new(
                TokenKind::Keyword(Kw::Constant),
                Kw::Constant.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            identifier_list,
            colon_token: Token::new(
                TokenKind::Colon,
                TokenKind::Colon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            subtype_indication,
            colon_eq_token: None,
            expression: None,
            semi_colon_token: Token::new(
                TokenKind::SemiColon,
                TokenKind::SemiColon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_constant_token(mut self, t: Token) -> Self {
        self.constant_token = t;
        self
    }
    pub fn with_identifier_list(mut self, n: IdentifierListSyntax) -> Self {
        self.identifier_list = n;
        self
    }
    pub fn with_colon_token(mut self, t: Token) -> Self {
        self.colon_token = t;
        self
    }
    pub fn with_subtype_indication(mut self, n: SubtypeIndicationSyntax) -> Self {
        self.subtype_indication = n;
        self
    }
    pub fn with_colon_eq_token(mut self, t: Token) -> Self {
        self.colon_eq_token = Some(t);
        self
    }
    pub fn with_expression(mut self, n: ExpressionSyntax) -> Self {
        self.expression = Some(n);
        self
    }
    pub fn with_semi_colon_token(mut self, t: Token) -> Self {
        self.semi_colon_token = t;
        self
    }
    pub fn build(self) -> ConstantDeclarationSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ConstantDeclaration);
        builder.push(self.constant_token);
        builder.push_node(self.identifier_list.raw().green().clone());
        builder.push(self.colon_token);
        builder.push_node(self.subtype_indication.raw().green().clone());
        if let Some(t) = self.colon_eq_token {
            builder.push(t);
        }
        if let Some(n) = self.expression {
            builder.push_node(n.raw().green().clone());
        }
        builder.push(self.semi_colon_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ConstantDeclarationSyntax::cast(node).unwrap()
    }
}
pub struct ConstrainedArrayDefinitionBuilder {
    array_token: Token,
    index_constraint: IndexConstraintSyntax,
    of_token: Token,
    subtype_indication: SubtypeIndicationSyntax,
}
impl ConstrainedArrayDefinitionBuilder {
    pub fn new(subtype_indication: SubtypeIndicationSyntax) -> Self {
        Self {
            array_token: Token::new(
                TokenKind::Keyword(Kw::Array),
                Kw::Array.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            index_constraint: IndexConstraintBuilder::default().build(),
            of_token: Token::new(
                TokenKind::Keyword(Kw::Of),
                Kw::Of.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            subtype_indication,
        }
    }
    pub fn with_array_token(mut self, t: Token) -> Self {
        self.array_token = t;
        self
    }
    pub fn with_index_constraint(mut self, n: IndexConstraintSyntax) -> Self {
        self.index_constraint = n;
        self
    }
    pub fn with_of_token(mut self, t: Token) -> Self {
        self.of_token = t;
        self
    }
    pub fn with_subtype_indication(mut self, n: SubtypeIndicationSyntax) -> Self {
        self.subtype_indication = n;
        self
    }
    pub fn build(self) -> ConstrainedArrayDefinitionSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ConstrainedArrayDefinition);
        builder.push(self.array_token);
        builder.push_node(self.index_constraint.raw().green().clone());
        builder.push(self.of_token);
        builder.push_node(self.subtype_indication.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ConstrainedArrayDefinitionSyntax::cast(node).unwrap()
    }
}
pub struct ContextClauseBuilder {
    context_items: Vec<ContextItemSyntax>,
}
impl Default for ContextClauseBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl ContextClauseBuilder {
    pub fn new() -> Self {
        Self {
            context_items: Vec::new(),
        }
    }
    pub fn add_context_items(mut self, n: ContextItemSyntax) -> Self {
        self.context_items.push(n);
        self
    }
    pub fn build(self) -> ContextClauseSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ContextClause);
        for n in self.context_items {
            builder.push_node(n.raw().green().clone());
        }
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ContextClauseSyntax::cast(node).unwrap()
    }
}
pub struct ContextDeclarationBuilder {
    context_declaration_preamble: ContextDeclarationPreambleSyntax,
    context_clause: ContextClauseSyntax,
    context_declaration_epilogue: ContextDeclarationEpilogueSyntax,
}
impl ContextDeclarationBuilder {
    pub fn new(context_declaration_preamble: ContextDeclarationPreambleSyntax) -> Self {
        Self {
            context_declaration_preamble,
            context_clause: ContextClauseBuilder::default().build(),
            context_declaration_epilogue: ContextDeclarationEpilogueBuilder::default().build(),
        }
    }
    pub fn with_context_declaration_preamble(
        mut self,
        n: ContextDeclarationPreambleSyntax,
    ) -> Self {
        self.context_declaration_preamble = n;
        self
    }
    pub fn with_context_clause(mut self, n: ContextClauseSyntax) -> Self {
        self.context_clause = n;
        self
    }
    pub fn with_context_declaration_epilogue(
        mut self,
        n: ContextDeclarationEpilogueSyntax,
    ) -> Self {
        self.context_declaration_epilogue = n;
        self
    }
    pub fn build(self) -> ContextDeclarationSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ContextDeclaration);
        builder.push_node(self.context_declaration_preamble.raw().green().clone());
        builder.push_node(self.context_clause.raw().green().clone());
        builder.push_node(self.context_declaration_epilogue.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ContextDeclarationSyntax::cast(node).unwrap()
    }
}
pub struct ContextDeclarationEpilogueBuilder {
    end_token: Token,
    context_token: Option<Token>,
    identifier_token: Option<Token>,
    semi_colon_token: Token,
}
impl Default for ContextDeclarationEpilogueBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl ContextDeclarationEpilogueBuilder {
    pub fn new() -> Self {
        Self {
            end_token: Token::new(
                TokenKind::Keyword(Kw::End),
                Kw::End.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            context_token: None,
            identifier_token: None,
            semi_colon_token: Token::new(
                TokenKind::SemiColon,
                TokenKind::SemiColon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_end_token(mut self, t: Token) -> Self {
        self.end_token = t;
        self
    }
    pub fn with_context_token(mut self, t: Token) -> Self {
        self.context_token = Some(t);
        self
    }
    pub fn with_identifier_token(mut self, t: Token) -> Self {
        self.identifier_token = Some(t);
        self
    }
    pub fn with_semi_colon_token(mut self, t: Token) -> Self {
        self.semi_colon_token = t;
        self
    }
    pub fn build(self) -> ContextDeclarationEpilogueSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ContextDeclarationEpilogue);
        builder.push(self.end_token);
        if let Some(t) = self.context_token {
            builder.push(t);
        }
        if let Some(t) = self.identifier_token {
            builder.push(t);
        }
        builder.push(self.semi_colon_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ContextDeclarationEpilogueSyntax::cast(node).unwrap()
    }
}
pub struct ContextDeclarationPreambleBuilder {
    context_token: Token,
    name_token: Token,
    is_token: Token,
}
impl ContextDeclarationPreambleBuilder {
    pub fn new(name_token: Token) -> Self {
        Self {
            context_token: Token::new(
                TokenKind::Keyword(Kw::Context),
                Kw::Context.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            name_token,
            is_token: Token::new(
                TokenKind::Keyword(Kw::Is),
                Kw::Is.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_context_token(mut self, t: Token) -> Self {
        self.context_token = t;
        self
    }
    pub fn with_name_token(mut self, t: Token) -> Self {
        self.name_token = t;
        self
    }
    pub fn with_is_token(mut self, t: Token) -> Self {
        self.is_token = t;
        self
    }
    pub fn build(self) -> ContextDeclarationPreambleSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ContextDeclarationPreamble);
        builder.push(self.context_token);
        builder.push(self.name_token);
        builder.push(self.is_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ContextDeclarationPreambleSyntax::cast(node).unwrap()
    }
}
pub struct ContextReferenceBuilder {
    context_token: Token,
    name_list: NameListSyntax,
    semi_colon_token: Token,
}
impl Default for ContextReferenceBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl ContextReferenceBuilder {
    pub fn new() -> Self {
        Self {
            context_token: Token::new(
                TokenKind::Keyword(Kw::Context),
                Kw::Context.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            name_list: NameListBuilder::default().build(),
            semi_colon_token: Token::new(
                TokenKind::SemiColon,
                TokenKind::SemiColon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_context_token(mut self, t: Token) -> Self {
        self.context_token = t;
        self
    }
    pub fn with_name_list(mut self, n: NameListSyntax) -> Self {
        self.name_list = n;
        self
    }
    pub fn with_semi_colon_token(mut self, t: Token) -> Self {
        self.semi_colon_token = t;
        self
    }
    pub fn build(self) -> ContextReferenceSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ContextReference);
        builder.push(self.context_token);
        builder.push_node(self.name_list.raw().green().clone());
        builder.push(self.semi_colon_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ContextReferenceSyntax::cast(node).unwrap()
    }
}
pub struct DeclarationStatementSeparatorBuilder {
    begin_token: Token,
}
impl Default for DeclarationStatementSeparatorBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl DeclarationStatementSeparatorBuilder {
    pub fn new() -> Self {
        Self {
            begin_token: Token::new(
                TokenKind::Keyword(Kw::Begin),
                Kw::Begin.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_begin_token(mut self, t: Token) -> Self {
        self.begin_token = t;
        self
    }
    pub fn build(self) -> DeclarationStatementSeparatorSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::DeclarationStatementSeparator);
        builder.push(self.begin_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        DeclarationStatementSeparatorSyntax::cast(node).unwrap()
    }
}
pub struct DeclarationsBuilder {
    declarations: Vec<DeclarationSyntax>,
}
impl Default for DeclarationsBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl DeclarationsBuilder {
    pub fn new() -> Self {
        Self {
            declarations: Vec::new(),
        }
    }
    pub fn add_declarations(mut self, n: DeclarationSyntax) -> Self {
        self.declarations.push(n);
        self
    }
    pub fn build(self) -> DeclarationsSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::Declarations);
        for n in self.declarations {
            builder.push_node(n.raw().green().clone());
        }
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        DeclarationsSyntax::cast(node).unwrap()
    }
}
pub struct DesignFileBuilder {
    design_units: Vec<DesignUnitSyntax>,
    eof_token: Token,
}
impl DesignFileBuilder {
    pub fn new(eof_token: Token) -> Self {
        Self {
            design_units: Vec::new(),
            eof_token,
        }
    }
    pub fn add_design_units(mut self, n: DesignUnitSyntax) -> Self {
        self.design_units.push(n);
        self
    }
    pub fn with_eof_token(mut self, t: Token) -> Self {
        self.eof_token = t;
        self
    }
    pub fn build(self) -> DesignFileSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::DesignFile);
        for n in self.design_units {
            builder.push_node(n.raw().green().clone());
        }
        builder.push(self.eof_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        DesignFileSyntax::cast(node).unwrap()
    }
}
pub struct DesignUnitBuilder {
    context_clause: ContextClauseSyntax,
    library_unit: LibraryUnitSyntax,
}
impl DesignUnitBuilder {
    pub fn new(library_unit: LibraryUnitSyntax) -> Self {
        Self {
            context_clause: ContextClauseBuilder::default().build(),
            library_unit,
        }
    }
    pub fn with_context_clause(mut self, n: ContextClauseSyntax) -> Self {
        self.context_clause = n;
        self
    }
    pub fn with_library_unit(mut self, n: LibraryUnitSyntax) -> Self {
        self.library_unit = n;
        self
    }
    pub fn build(self) -> DesignUnitSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::DesignUnit);
        builder.push_node(self.context_clause.raw().green().clone());
        builder.push_node(self.library_unit.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        DesignUnitSyntax::cast(node).unwrap()
    }
}
pub struct DisconnectionSpecificationBuilder {
    disconnect_token: Token,
    guarded_signal_specification: GuardedSignalSpecificationSyntax,
    after_token: Token,
    expression: ExpressionSyntax,
    semi_colon_token: Token,
}
impl DisconnectionSpecificationBuilder {
    pub fn new(
        guarded_signal_specification: GuardedSignalSpecificationSyntax,
        expression: ExpressionSyntax,
    ) -> Self {
        Self {
            disconnect_token: Token::new(
                TokenKind::Keyword(Kw::Disconnect),
                Kw::Disconnect.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            guarded_signal_specification,
            after_token: Token::new(
                TokenKind::Keyword(Kw::After),
                Kw::After.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            expression,
            semi_colon_token: Token::new(
                TokenKind::SemiColon,
                TokenKind::SemiColon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_disconnect_token(mut self, t: Token) -> Self {
        self.disconnect_token = t;
        self
    }
    pub fn with_guarded_signal_specification(
        mut self,
        n: GuardedSignalSpecificationSyntax,
    ) -> Self {
        self.guarded_signal_specification = n;
        self
    }
    pub fn with_after_token(mut self, t: Token) -> Self {
        self.after_token = t;
        self
    }
    pub fn with_expression(mut self, n: ExpressionSyntax) -> Self {
        self.expression = n;
        self
    }
    pub fn with_semi_colon_token(mut self, t: Token) -> Self {
        self.semi_colon_token = t;
        self
    }
    pub fn build(self) -> DisconnectionSpecificationSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::DisconnectionSpecification);
        builder.push(self.disconnect_token);
        builder.push_node(self.guarded_signal_specification.raw().green().clone());
        builder.push(self.after_token);
        builder.push_node(self.expression.raw().green().clone());
        builder.push(self.semi_colon_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        DisconnectionSpecificationSyntax::cast(node).unwrap()
    }
}
pub struct DiscreteRangeChoiceBuilder {
    discrete_range: DiscreteRangeSyntax,
}
impl DiscreteRangeChoiceBuilder {
    pub fn new(discrete_range: DiscreteRangeSyntax) -> Self {
        Self { discrete_range }
    }
    pub fn with_discrete_range(mut self, n: DiscreteRangeSyntax) -> Self {
        self.discrete_range = n;
        self
    }
    pub fn build(self) -> DiscreteRangeChoiceSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::DiscreteRangeChoice);
        builder.push_node(self.discrete_range.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        DiscreteRangeChoiceSyntax::cast(node).unwrap()
    }
}
pub struct ElementAssociationBuilder {
    choice: ChoiceSyntax,
    right_arrow_token: Token,
    expression: ExpressionSyntax,
}
impl ElementAssociationBuilder {
    pub fn new(choice: ChoiceSyntax, expression: ExpressionSyntax) -> Self {
        Self {
            choice,
            right_arrow_token: Token::new(
                TokenKind::RightArrow,
                TokenKind::RightArrow.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            expression,
        }
    }
    pub fn with_choice(mut self, n: ChoiceSyntax) -> Self {
        self.choice = n;
        self
    }
    pub fn with_right_arrow_token(mut self, t: Token) -> Self {
        self.right_arrow_token = t;
        self
    }
    pub fn with_expression(mut self, n: ExpressionSyntax) -> Self {
        self.expression = n;
        self
    }
    pub fn build(self) -> ElementAssociationSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ElementAssociation);
        builder.push_node(self.choice.raw().green().clone());
        builder.push(self.right_arrow_token);
        builder.push_node(self.expression.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ElementAssociationSyntax::cast(node).unwrap()
    }
}
pub struct ElementDeclarationBuilder {
    identifier_list: IdentifierListSyntax,
    colon_token: Token,
    subtype_indication: SubtypeIndicationSyntax,
    semi_colon_token: Token,
}
impl ElementDeclarationBuilder {
    pub fn new(
        identifier_list: IdentifierListSyntax,
        subtype_indication: SubtypeIndicationSyntax,
    ) -> Self {
        Self {
            identifier_list,
            colon_token: Token::new(
                TokenKind::Colon,
                TokenKind::Colon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            subtype_indication,
            semi_colon_token: Token::new(
                TokenKind::SemiColon,
                TokenKind::SemiColon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_identifier_list(mut self, n: IdentifierListSyntax) -> Self {
        self.identifier_list = n;
        self
    }
    pub fn with_colon_token(mut self, t: Token) -> Self {
        self.colon_token = t;
        self
    }
    pub fn with_subtype_indication(mut self, n: SubtypeIndicationSyntax) -> Self {
        self.subtype_indication = n;
        self
    }
    pub fn with_semi_colon_token(mut self, t: Token) -> Self {
        self.semi_colon_token = t;
        self
    }
    pub fn build(self) -> ElementDeclarationSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ElementDeclaration);
        builder.push_node(self.identifier_list.raw().green().clone());
        builder.push(self.colon_token);
        builder.push_node(self.subtype_indication.raw().green().clone());
        builder.push(self.semi_colon_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ElementDeclarationSyntax::cast(node).unwrap()
    }
}
pub struct ElementResolutionResolutionIndicationBuilder {
    element_resolution: ElementResolutionSyntax,
}
impl ElementResolutionResolutionIndicationBuilder {
    pub fn new(element_resolution: ElementResolutionSyntax) -> Self {
        Self { element_resolution }
    }
    pub fn with_element_resolution(mut self, n: ElementResolutionSyntax) -> Self {
        self.element_resolution = n;
        self
    }
    pub fn build(self) -> ElementResolutionResolutionIndicationSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ElementResolutionResolutionIndication);
        builder.push_node(self.element_resolution.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ElementResolutionResolutionIndicationSyntax::cast(node).unwrap()
    }
}
pub struct EntityClassEntryBuilder {
    entity_class: EntityClassSyntax,
    box_token: Option<Token>,
}
impl EntityClassEntryBuilder {
    pub fn new(entity_class: EntityClassSyntax) -> Self {
        Self {
            entity_class,
            box_token: None,
        }
    }
    pub fn with_entity_class(mut self, n: EntityClassSyntax) -> Self {
        self.entity_class = n;
        self
    }
    pub fn with_box_token(mut self, t: Token) -> Self {
        self.box_token = Some(t);
        self
    }
    pub fn build(self) -> EntityClassEntrySyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::EntityClassEntry);
        builder.push(self.entity_class.raw().token().clone());
        if let Some(t) = self.box_token {
            builder.push(t);
        }
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        EntityClassEntrySyntax::cast(node).unwrap()
    }
}
pub struct EntityClassEntryListBuilder {
    entity_class_entrys: Vec<EntityClassEntrySyntax>,
    comma_token: Vec<Token>,
}
impl Default for EntityClassEntryListBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl EntityClassEntryListBuilder {
    pub fn new() -> Self {
        Self {
            entity_class_entrys: Vec::new(),
            comma_token: Vec::new(),
        }
    }
    pub fn add_entity_class_entrys(mut self, n: EntityClassEntrySyntax) -> Self {
        self.entity_class_entrys.push(n);
        self
    }
    pub fn add_comma_token(mut self, t: Token) -> Self {
        self.comma_token.push(t);
        self
    }
    pub fn build(self) -> EntityClassEntryListSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::EntityClassEntryList);
        for n in self.entity_class_entrys {
            builder.push_node(n.raw().green().clone());
        }
        for t in self.comma_token {
            builder.push(t);
        }
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        EntityClassEntryListSyntax::cast(node).unwrap()
    }
}
pub struct EntityConfigurationAspectBuilder {
    configuration_token: Token,
    name: NameSyntax,
}
impl EntityConfigurationAspectBuilder {
    pub fn new(name: NameSyntax) -> Self {
        Self {
            configuration_token: Token::new(
                TokenKind::Keyword(Kw::Configuration),
                Kw::Configuration.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            name,
        }
    }
    pub fn with_configuration_token(mut self, t: Token) -> Self {
        self.configuration_token = t;
        self
    }
    pub fn with_name(mut self, n: NameSyntax) -> Self {
        self.name = n;
        self
    }
    pub fn build(self) -> EntityConfigurationAspectSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::EntityConfigurationAspect);
        builder.push(self.configuration_token);
        builder.push_node(self.name.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        EntityConfigurationAspectSyntax::cast(node).unwrap()
    }
}
pub struct EntityDeclarationBuilder {
    entity_declaration_preamble: EntityDeclarationPreambleSyntax,
    entity_header: EntityHeaderSyntax,
    declarations: DeclarationsSyntax,
    declaration_statement_separator: DeclarationStatementSeparatorSyntax,
    concurrent_statements: ConcurrentStatementsSyntax,
    entity_declaration_epilogue: EntityDeclarationEpilogueSyntax,
}
impl EntityDeclarationBuilder {
    pub fn new(
        entity_declaration_preamble: EntityDeclarationPreambleSyntax,
        entity_declaration_epilogue: EntityDeclarationEpilogueSyntax,
    ) -> Self {
        Self {
            entity_declaration_preamble,
            entity_header: EntityHeaderBuilder::default().build(),
            declarations: DeclarationsBuilder::default().build(),
            declaration_statement_separator: DeclarationStatementSeparatorBuilder::default()
                .build(),
            concurrent_statements: ConcurrentStatementsBuilder::default().build(),
            entity_declaration_epilogue,
        }
    }
    pub fn with_entity_declaration_preamble(mut self, n: EntityDeclarationPreambleSyntax) -> Self {
        self.entity_declaration_preamble = n;
        self
    }
    pub fn with_entity_header(mut self, n: EntityHeaderSyntax) -> Self {
        self.entity_header = n;
        self
    }
    pub fn with_declarations(mut self, n: DeclarationsSyntax) -> Self {
        self.declarations = n;
        self
    }
    pub fn with_declaration_statement_separator(
        mut self,
        n: DeclarationStatementSeparatorSyntax,
    ) -> Self {
        self.declaration_statement_separator = n;
        self
    }
    pub fn with_concurrent_statements(mut self, n: ConcurrentStatementsSyntax) -> Self {
        self.concurrent_statements = n;
        self
    }
    pub fn with_entity_declaration_epilogue(mut self, n: EntityDeclarationEpilogueSyntax) -> Self {
        self.entity_declaration_epilogue = n;
        self
    }
    pub fn build(self) -> EntityDeclarationSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::EntityDeclaration);
        builder.push_node(self.entity_declaration_preamble.raw().green().clone());
        builder.push_node(self.entity_header.raw().green().clone());
        builder.push_node(self.declarations.raw().green().clone());
        builder.push_node(self.declaration_statement_separator.raw().green().clone());
        builder.push_node(self.concurrent_statements.raw().green().clone());
        builder.push_node(self.entity_declaration_epilogue.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        EntityDeclarationSyntax::cast(node).unwrap()
    }
}
pub struct EntityDeclarationEpilogueBuilder {
    end_token: Token,
    entity_token: Token,
    identifier_token: Token,
    semi_colon_token: Token,
}
impl EntityDeclarationEpilogueBuilder {
    pub fn new(identifier_token: Token) -> Self {
        Self {
            end_token: Token::new(
                TokenKind::Keyword(Kw::End),
                Kw::End.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            entity_token: Token::new(
                TokenKind::Keyword(Kw::Entity),
                Kw::Entity.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            identifier_token,
            semi_colon_token: Token::new(
                TokenKind::SemiColon,
                TokenKind::SemiColon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_end_token(mut self, t: Token) -> Self {
        self.end_token = t;
        self
    }
    pub fn with_entity_token(mut self, t: Token) -> Self {
        self.entity_token = t;
        self
    }
    pub fn with_identifier_token(mut self, t: Token) -> Self {
        self.identifier_token = t;
        self
    }
    pub fn with_semi_colon_token(mut self, t: Token) -> Self {
        self.semi_colon_token = t;
        self
    }
    pub fn build(self) -> EntityDeclarationEpilogueSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::EntityDeclarationEpilogue);
        builder.push(self.end_token);
        builder.push(self.entity_token);
        builder.push(self.identifier_token);
        builder.push(self.semi_colon_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        EntityDeclarationEpilogueSyntax::cast(node).unwrap()
    }
}
pub struct EntityDeclarationPreambleBuilder {
    entity_token: Token,
    name_token: Token,
    is_token: Token,
}
impl EntityDeclarationPreambleBuilder {
    pub fn new(name_token: Token) -> Self {
        Self {
            entity_token: Token::new(
                TokenKind::Keyword(Kw::Entity),
                Kw::Entity.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            name_token,
            is_token: Token::new(
                TokenKind::Keyword(Kw::Is),
                Kw::Is.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_entity_token(mut self, t: Token) -> Self {
        self.entity_token = t;
        self
    }
    pub fn with_name_token(mut self, t: Token) -> Self {
        self.name_token = t;
        self
    }
    pub fn with_is_token(mut self, t: Token) -> Self {
        self.is_token = t;
        self
    }
    pub fn build(self) -> EntityDeclarationPreambleSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::EntityDeclarationPreamble);
        builder.push(self.entity_token);
        builder.push(self.name_token);
        builder.push(self.is_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        EntityDeclarationPreambleSyntax::cast(node).unwrap()
    }
}
pub struct EntityDesignatorBuilder {
    entity_tag: EntityTagSyntax,
    signature: SignatureSyntax,
}
impl EntityDesignatorBuilder {
    pub fn new(entity_tag: EntityTagSyntax, signature: SignatureSyntax) -> Self {
        Self {
            entity_tag,
            signature,
        }
    }
    pub fn with_entity_tag(mut self, n: EntityTagSyntax) -> Self {
        self.entity_tag = n;
        self
    }
    pub fn with_signature(mut self, n: SignatureSyntax) -> Self {
        self.signature = n;
        self
    }
    pub fn build(self) -> EntityDesignatorSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::EntityDesignator);
        builder.push(self.entity_tag.raw().token().clone());
        builder.push_node(self.signature.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        EntityDesignatorSyntax::cast(node).unwrap()
    }
}
pub struct EntityDesignatorListBuilder {
    entity_designators: Vec<EntityDesignatorSyntax>,
    comma_token: Vec<Token>,
}
impl Default for EntityDesignatorListBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl EntityDesignatorListBuilder {
    pub fn new() -> Self {
        Self {
            entity_designators: Vec::new(),
            comma_token: Vec::new(),
        }
    }
    pub fn add_entity_designators(mut self, n: EntityDesignatorSyntax) -> Self {
        self.entity_designators.push(n);
        self
    }
    pub fn add_comma_token(mut self, t: Token) -> Self {
        self.comma_token.push(t);
        self
    }
    pub fn build(self) -> EntityDesignatorListSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::EntityDesignatorList);
        for n in self.entity_designators {
            builder.push_node(n.raw().green().clone());
        }
        for t in self.comma_token {
            builder.push(t);
        }
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        EntityDesignatorListSyntax::cast(node).unwrap()
    }
}
pub struct EntityEntityAspectBuilder {
    entity_token: Token,
    name: NameSyntax,
}
impl EntityEntityAspectBuilder {
    pub fn new(name: NameSyntax) -> Self {
        Self {
            entity_token: Token::new(
                TokenKind::Keyword(Kw::Entity),
                Kw::Entity.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            name,
        }
    }
    pub fn with_entity_token(mut self, t: Token) -> Self {
        self.entity_token = t;
        self
    }
    pub fn with_name(mut self, n: NameSyntax) -> Self {
        self.name = n;
        self
    }
    pub fn build(self) -> EntityEntityAspectSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::EntityEntityAspect);
        builder.push(self.entity_token);
        builder.push_node(self.name.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        EntityEntityAspectSyntax::cast(node).unwrap()
    }
}
pub struct EntityHeaderBuilder {
    generic_clause: GenericClauseSyntax,
    port_clause: PortClauseSyntax,
}
impl Default for EntityHeaderBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl EntityHeaderBuilder {
    pub fn new() -> Self {
        Self {
            generic_clause: GenericClauseBuilder::default().build(),
            port_clause: PortClauseBuilder::default().build(),
        }
    }
    pub fn with_generic_clause(mut self, n: GenericClauseSyntax) -> Self {
        self.generic_clause = n;
        self
    }
    pub fn with_port_clause(mut self, n: PortClauseSyntax) -> Self {
        self.port_clause = n;
        self
    }
    pub fn build(self) -> EntityHeaderSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::EntityHeader);
        builder.push_node(self.generic_clause.raw().green().clone());
        builder.push_node(self.port_clause.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        EntityHeaderSyntax::cast(node).unwrap()
    }
}
pub struct EntityInstantiatedUnitBuilder {
    entity_token: Token,
    name: NameSyntax,
    left_par_token: Option<Token>,
    identifier_token: Option<Token>,
    right_par_token: Option<Token>,
}
impl EntityInstantiatedUnitBuilder {
    pub fn new(name: NameSyntax) -> Self {
        Self {
            entity_token: Token::new(
                TokenKind::Keyword(Kw::Entity),
                Kw::Entity.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            name,
            left_par_token: None,
            identifier_token: None,
            right_par_token: None,
        }
    }
    pub fn with_entity_token(mut self, t: Token) -> Self {
        self.entity_token = t;
        self
    }
    pub fn with_name(mut self, n: NameSyntax) -> Self {
        self.name = n;
        self
    }
    pub fn with_left_par_token(mut self, t: Token) -> Self {
        self.left_par_token = Some(t);
        self
    }
    pub fn with_identifier_token(mut self, t: Token) -> Self {
        self.identifier_token = Some(t);
        self
    }
    pub fn with_right_par_token(mut self, t: Token) -> Self {
        self.right_par_token = Some(t);
        self
    }
    pub fn build(self) -> EntityInstantiatedUnitSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::EntityInstantiatedUnit);
        builder.push(self.entity_token);
        builder.push_node(self.name.raw().green().clone());
        if let Some(t) = self.left_par_token {
            builder.push(t);
        }
        if let Some(t) = self.identifier_token {
            builder.push(t);
        }
        if let Some(t) = self.right_par_token {
            builder.push(t);
        }
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        EntityInstantiatedUnitSyntax::cast(node).unwrap()
    }
}
pub struct EntityNameListAllBuilder {
    all_token: Token,
}
impl Default for EntityNameListAllBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl EntityNameListAllBuilder {
    pub fn new() -> Self {
        Self {
            all_token: Token::new(
                TokenKind::Keyword(Kw::All),
                Kw::All.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_all_token(mut self, t: Token) -> Self {
        self.all_token = t;
        self
    }
    pub fn build(self) -> EntityNameListAllSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::EntityNameListAll);
        builder.push(self.all_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        EntityNameListAllSyntax::cast(node).unwrap()
    }
}
pub struct EntityNameListOthersBuilder {
    others_token: Token,
}
impl Default for EntityNameListOthersBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl EntityNameListOthersBuilder {
    pub fn new() -> Self {
        Self {
            others_token: Token::new(
                TokenKind::Keyword(Kw::Others),
                Kw::Others.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_others_token(mut self, t: Token) -> Self {
        self.others_token = t;
        self
    }
    pub fn build(self) -> EntityNameListOthersSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::EntityNameListOthers);
        builder.push(self.others_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        EntityNameListOthersSyntax::cast(node).unwrap()
    }
}
pub struct EntityOpenAspectBuilder {
    open_token: Token,
}
impl Default for EntityOpenAspectBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl EntityOpenAspectBuilder {
    pub fn new() -> Self {
        Self {
            open_token: Token::new(
                TokenKind::Keyword(Kw::Open),
                Kw::Open.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_open_token(mut self, t: Token) -> Self {
        self.open_token = t;
        self
    }
    pub fn build(self) -> EntityOpenAspectSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::EntityOpenAspect);
        builder.push(self.open_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        EntityOpenAspectSyntax::cast(node).unwrap()
    }
}
pub struct EntitySpecificationBuilder {
    entity_name_list: EntityNameListSyntax,
    colon_token: Token,
    entity_class: EntityClassSyntax,
}
impl EntitySpecificationBuilder {
    pub fn new(entity_name_list: EntityNameListSyntax, entity_class: EntityClassSyntax) -> Self {
        Self {
            entity_name_list,
            colon_token: Token::new(
                TokenKind::Colon,
                TokenKind::Colon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            entity_class,
        }
    }
    pub fn with_entity_name_list(mut self, n: EntityNameListSyntax) -> Self {
        self.entity_name_list = n;
        self
    }
    pub fn with_colon_token(mut self, t: Token) -> Self {
        self.colon_token = t;
        self
    }
    pub fn with_entity_class(mut self, n: EntityClassSyntax) -> Self {
        self.entity_class = n;
        self
    }
    pub fn build(self) -> EntitySpecificationSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::EntitySpecification);
        builder.push_node(self.entity_name_list.raw().green().clone());
        builder.push(self.colon_token);
        builder.push(self.entity_class.raw().token().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        EntitySpecificationSyntax::cast(node).unwrap()
    }
}
pub struct EnumerationTypeDefinitionBuilder {
    left_par_token: Token,
    discrete_ranges: Vec<DiscreteRangeSyntax>,
    comma_token: Vec<Token>,
    right_par_token: Token,
}
impl Default for EnumerationTypeDefinitionBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl EnumerationTypeDefinitionBuilder {
    pub fn new() -> Self {
        Self {
            left_par_token: Token::new(
                TokenKind::LeftPar,
                TokenKind::LeftPar.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            discrete_ranges: Vec::new(),
            comma_token: Vec::new(),
            right_par_token: Token::new(
                TokenKind::RightPar,
                TokenKind::RightPar.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_left_par_token(mut self, t: Token) -> Self {
        self.left_par_token = t;
        self
    }
    pub fn add_discrete_ranges(mut self, n: DiscreteRangeSyntax) -> Self {
        self.discrete_ranges.push(n);
        self
    }
    pub fn add_comma_token(mut self, t: Token) -> Self {
        self.comma_token.push(t);
        self
    }
    pub fn with_right_par_token(mut self, t: Token) -> Self {
        self.right_par_token = t;
        self
    }
    pub fn build(self) -> EnumerationTypeDefinitionSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::EnumerationTypeDefinition);
        builder.push(self.left_par_token);
        for n in self.discrete_ranges {
            builder.push_node(n.raw().green().clone());
        }
        for t in self.comma_token {
            builder.push(t);
        }
        builder.push(self.right_par_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        EnumerationTypeDefinitionSyntax::cast(node).unwrap()
    }
}
pub struct ExitStatementBuilder {
    label_token: Option<Token>,
    colon_token: Option<Token>,
    exit_token: Token,
    loop_label_token: Option<Token>,
    when_token: Option<Token>,
    expression: Option<ExpressionSyntax>,
    semi_colon_token: Token,
}
impl Default for ExitStatementBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl ExitStatementBuilder {
    pub fn new() -> Self {
        Self {
            label_token: None,
            colon_token: None,
            exit_token: Token::new(
                TokenKind::Keyword(Kw::Exit),
                Kw::Exit.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            loop_label_token: None,
            when_token: None,
            expression: None,
            semi_colon_token: Token::new(
                TokenKind::SemiColon,
                TokenKind::SemiColon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_label_token(mut self, t: Token) -> Self {
        self.label_token = Some(t);
        self
    }
    pub fn with_colon_token(mut self, t: Token) -> Self {
        self.colon_token = Some(t);
        self
    }
    pub fn with_exit_token(mut self, t: Token) -> Self {
        self.exit_token = t;
        self
    }
    pub fn with_loop_label_token(mut self, t: Token) -> Self {
        self.loop_label_token = Some(t);
        self
    }
    pub fn with_when_token(mut self, t: Token) -> Self {
        self.when_token = Some(t);
        self
    }
    pub fn with_expression(mut self, n: ExpressionSyntax) -> Self {
        self.expression = Some(n);
        self
    }
    pub fn with_semi_colon_token(mut self, t: Token) -> Self {
        self.semi_colon_token = t;
        self
    }
    pub fn build(self) -> ExitStatementSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ExitStatement);
        if let Some(t) = self.label_token {
            builder.push(t);
        }
        if let Some(t) = self.colon_token {
            builder.push(t);
        }
        builder.push(self.exit_token);
        if let Some(t) = self.loop_label_token {
            builder.push(t);
        }
        if let Some(t) = self.when_token {
            builder.push(t);
        }
        if let Some(n) = self.expression {
            builder.push_node(n.raw().green().clone());
        }
        builder.push(self.semi_colon_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ExitStatementSyntax::cast(node).unwrap()
    }
}
pub struct ExpressionAllocatorBuilder {
    new_token: Token,
    expression: ExpressionSyntax,
}
impl ExpressionAllocatorBuilder {
    pub fn new(expression: ExpressionSyntax) -> Self {
        Self {
            new_token: Token::new(
                TokenKind::Keyword(Kw::New),
                Kw::New.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            expression,
        }
    }
    pub fn with_new_token(mut self, t: Token) -> Self {
        self.new_token = t;
        self
    }
    pub fn with_expression(mut self, n: ExpressionSyntax) -> Self {
        self.expression = n;
        self
    }
    pub fn build(self) -> ExpressionAllocatorSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ExpressionAllocator);
        builder.push(self.new_token);
        builder.push_node(self.expression.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ExpressionAllocatorSyntax::cast(node).unwrap()
    }
}
pub struct ExpressionChoiceBuilder {
    expression: ExpressionSyntax,
}
impl ExpressionChoiceBuilder {
    pub fn new(expression: ExpressionSyntax) -> Self {
        Self { expression }
    }
    pub fn with_expression(mut self, n: ExpressionSyntax) -> Self {
        self.expression = n;
        self
    }
    pub fn build(self) -> ExpressionChoiceSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ExpressionChoice);
        builder.push_node(self.expression.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ExpressionChoiceSyntax::cast(node).unwrap()
    }
}
pub struct ExternalConstantNameBuilder {
    lt_lt_token: Token,
    constant_token: Token,
    external_path_name: ExternalPathNameSyntax,
    colon_token: Token,
    subtype_indication: SubtypeIndicationSyntax,
    gt_gt_token: Token,
}
impl ExternalConstantNameBuilder {
    pub fn new(
        external_path_name: ExternalPathNameSyntax,
        subtype_indication: SubtypeIndicationSyntax,
    ) -> Self {
        Self {
            lt_lt_token: Token::new(
                TokenKind::LtLt,
                TokenKind::LtLt.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            constant_token: Token::new(
                TokenKind::Keyword(Kw::Constant),
                Kw::Constant.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            external_path_name,
            colon_token: Token::new(
                TokenKind::Colon,
                TokenKind::Colon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            subtype_indication,
            gt_gt_token: Token::new(
                TokenKind::GtGt,
                TokenKind::GtGt.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_lt_lt_token(mut self, t: Token) -> Self {
        self.lt_lt_token = t;
        self
    }
    pub fn with_constant_token(mut self, t: Token) -> Self {
        self.constant_token = t;
        self
    }
    pub fn with_external_path_name(mut self, n: ExternalPathNameSyntax) -> Self {
        self.external_path_name = n;
        self
    }
    pub fn with_colon_token(mut self, t: Token) -> Self {
        self.colon_token = t;
        self
    }
    pub fn with_subtype_indication(mut self, n: SubtypeIndicationSyntax) -> Self {
        self.subtype_indication = n;
        self
    }
    pub fn with_gt_gt_token(mut self, t: Token) -> Self {
        self.gt_gt_token = t;
        self
    }
    pub fn build(self) -> ExternalConstantNameSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ExternalConstantName);
        builder.push(self.lt_lt_token);
        builder.push(self.constant_token);
        builder.push_node(self.external_path_name.raw().green().clone());
        builder.push(self.colon_token);
        builder.push_node(self.subtype_indication.raw().green().clone());
        builder.push(self.gt_gt_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ExternalConstantNameSyntax::cast(node).unwrap()
    }
}
pub struct ExternalSignalNameBuilder {
    lt_lt_token: Token,
    signal_token: Token,
    external_path_name: ExternalPathNameSyntax,
    colon_token: Token,
    subtype_indication: SubtypeIndicationSyntax,
    gt_gt_token: Token,
}
impl ExternalSignalNameBuilder {
    pub fn new(
        external_path_name: ExternalPathNameSyntax,
        subtype_indication: SubtypeIndicationSyntax,
    ) -> Self {
        Self {
            lt_lt_token: Token::new(
                TokenKind::LtLt,
                TokenKind::LtLt.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            signal_token: Token::new(
                TokenKind::Keyword(Kw::Signal),
                Kw::Signal.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            external_path_name,
            colon_token: Token::new(
                TokenKind::Colon,
                TokenKind::Colon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            subtype_indication,
            gt_gt_token: Token::new(
                TokenKind::GtGt,
                TokenKind::GtGt.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_lt_lt_token(mut self, t: Token) -> Self {
        self.lt_lt_token = t;
        self
    }
    pub fn with_signal_token(mut self, t: Token) -> Self {
        self.signal_token = t;
        self
    }
    pub fn with_external_path_name(mut self, n: ExternalPathNameSyntax) -> Self {
        self.external_path_name = n;
        self
    }
    pub fn with_colon_token(mut self, t: Token) -> Self {
        self.colon_token = t;
        self
    }
    pub fn with_subtype_indication(mut self, n: SubtypeIndicationSyntax) -> Self {
        self.subtype_indication = n;
        self
    }
    pub fn with_gt_gt_token(mut self, t: Token) -> Self {
        self.gt_gt_token = t;
        self
    }
    pub fn build(self) -> ExternalSignalNameSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ExternalSignalName);
        builder.push(self.lt_lt_token);
        builder.push(self.signal_token);
        builder.push_node(self.external_path_name.raw().green().clone());
        builder.push(self.colon_token);
        builder.push_node(self.subtype_indication.raw().green().clone());
        builder.push(self.gt_gt_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ExternalSignalNameSyntax::cast(node).unwrap()
    }
}
pub struct ExternalVariableNameBuilder {
    lt_lt_token: Token,
    variable_token: Token,
    external_path_name: ExternalPathNameSyntax,
    colon_token: Token,
    subtype_indication: SubtypeIndicationSyntax,
    gt_gt_token: Token,
}
impl ExternalVariableNameBuilder {
    pub fn new(
        external_path_name: ExternalPathNameSyntax,
        subtype_indication: SubtypeIndicationSyntax,
    ) -> Self {
        Self {
            lt_lt_token: Token::new(
                TokenKind::LtLt,
                TokenKind::LtLt.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            variable_token: Token::new(
                TokenKind::Keyword(Kw::Variable),
                Kw::Variable.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            external_path_name,
            colon_token: Token::new(
                TokenKind::Colon,
                TokenKind::Colon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            subtype_indication,
            gt_gt_token: Token::new(
                TokenKind::GtGt,
                TokenKind::GtGt.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_lt_lt_token(mut self, t: Token) -> Self {
        self.lt_lt_token = t;
        self
    }
    pub fn with_variable_token(mut self, t: Token) -> Self {
        self.variable_token = t;
        self
    }
    pub fn with_external_path_name(mut self, n: ExternalPathNameSyntax) -> Self {
        self.external_path_name = n;
        self
    }
    pub fn with_colon_token(mut self, t: Token) -> Self {
        self.colon_token = t;
        self
    }
    pub fn with_subtype_indication(mut self, n: SubtypeIndicationSyntax) -> Self {
        self.subtype_indication = n;
        self
    }
    pub fn with_gt_gt_token(mut self, t: Token) -> Self {
        self.gt_gt_token = t;
        self
    }
    pub fn build(self) -> ExternalVariableNameSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ExternalVariableName);
        builder.push(self.lt_lt_token);
        builder.push(self.variable_token);
        builder.push_node(self.external_path_name.raw().green().clone());
        builder.push(self.colon_token);
        builder.push_node(self.subtype_indication.raw().green().clone());
        builder.push(self.gt_gt_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ExternalVariableNameSyntax::cast(node).unwrap()
    }
}
pub struct FileDeclarationBuilder {
    file_token: Token,
    identifier_list: IdentifierListSyntax,
    colon_token: Token,
    subtype_indication: SubtypeIndicationSyntax,
    file_open_information: FileOpenInformationSyntax,
    semi_colon_token: Token,
}
impl FileDeclarationBuilder {
    pub fn new(
        identifier_list: IdentifierListSyntax,
        subtype_indication: SubtypeIndicationSyntax,
        file_open_information: FileOpenInformationSyntax,
    ) -> Self {
        Self {
            file_token: Token::new(
                TokenKind::Keyword(Kw::File),
                Kw::File.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            identifier_list,
            colon_token: Token::new(
                TokenKind::Colon,
                TokenKind::Colon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            subtype_indication,
            file_open_information,
            semi_colon_token: Token::new(
                TokenKind::SemiColon,
                TokenKind::SemiColon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_file_token(mut self, t: Token) -> Self {
        self.file_token = t;
        self
    }
    pub fn with_identifier_list(mut self, n: IdentifierListSyntax) -> Self {
        self.identifier_list = n;
        self
    }
    pub fn with_colon_token(mut self, t: Token) -> Self {
        self.colon_token = t;
        self
    }
    pub fn with_subtype_indication(mut self, n: SubtypeIndicationSyntax) -> Self {
        self.subtype_indication = n;
        self
    }
    pub fn with_file_open_information(mut self, n: FileOpenInformationSyntax) -> Self {
        self.file_open_information = n;
        self
    }
    pub fn with_semi_colon_token(mut self, t: Token) -> Self {
        self.semi_colon_token = t;
        self
    }
    pub fn build(self) -> FileDeclarationSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::FileDeclaration);
        builder.push(self.file_token);
        builder.push_node(self.identifier_list.raw().green().clone());
        builder.push(self.colon_token);
        builder.push_node(self.subtype_indication.raw().green().clone());
        builder.push_node(self.file_open_information.raw().green().clone());
        builder.push(self.semi_colon_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        FileDeclarationSyntax::cast(node).unwrap()
    }
}
pub struct FileOpenInformationBuilder {
    open_token: Token,
    file_open_kind: ExpressionSyntax,
    is_token: Token,
    file_logical_name: ExpressionSyntax,
}
impl FileOpenInformationBuilder {
    pub fn new(file_open_kind: ExpressionSyntax, file_logical_name: ExpressionSyntax) -> Self {
        Self {
            open_token: Token::new(
                TokenKind::Keyword(Kw::Open),
                Kw::Open.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            file_open_kind,
            is_token: Token::new(
                TokenKind::Keyword(Kw::Is),
                Kw::Is.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            file_logical_name,
        }
    }
    pub fn with_open_token(mut self, t: Token) -> Self {
        self.open_token = t;
        self
    }
    pub fn with_file_open_kind(mut self, n: ExpressionSyntax) -> Self {
        self.file_open_kind = n;
        self
    }
    pub fn with_is_token(mut self, t: Token) -> Self {
        self.is_token = t;
        self
    }
    pub fn with_file_logical_name(mut self, n: ExpressionSyntax) -> Self {
        self.file_logical_name = n;
        self
    }
    pub fn build(self) -> FileOpenInformationSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::FileOpenInformation);
        builder.push(self.open_token);
        builder.push_node(self.file_open_kind.raw().green().clone());
        builder.push(self.is_token);
        builder.push_node(self.file_logical_name.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        FileOpenInformationSyntax::cast(node).unwrap()
    }
}
pub struct FileTypeDefinitionBuilder {
    file_token: Token,
    of_token: Token,
    name: NameSyntax,
}
impl FileTypeDefinitionBuilder {
    pub fn new(name: NameSyntax) -> Self {
        Self {
            file_token: Token::new(
                TokenKind::Keyword(Kw::File),
                Kw::File.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            of_token: Token::new(
                TokenKind::Keyword(Kw::Of),
                Kw::Of.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            name,
        }
    }
    pub fn with_file_token(mut self, t: Token) -> Self {
        self.file_token = t;
        self
    }
    pub fn with_of_token(mut self, t: Token) -> Self {
        self.of_token = t;
        self
    }
    pub fn with_name(mut self, n: NameSyntax) -> Self {
        self.name = n;
        self
    }
    pub fn build(self) -> FileTypeDefinitionSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::FileTypeDefinition);
        builder.push(self.file_token);
        builder.push(self.of_token);
        builder.push_node(self.name.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        FileTypeDefinitionSyntax::cast(node).unwrap()
    }
}
pub struct ForGenerateStatementBuilder {
    for_generate_statement_preamble: ForGenerateStatementPreambleSyntax,
    generate_statement_body: GenerateStatementBodySyntax,
    for_generate_statement_epilogue: ForGenerateStatementEpilogueSyntax,
}
impl ForGenerateStatementBuilder {
    pub fn new(for_generate_statement_preamble: ForGenerateStatementPreambleSyntax) -> Self {
        Self {
            for_generate_statement_preamble,
            generate_statement_body: GenerateStatementBodyBuilder::default().build(),
            for_generate_statement_epilogue: ForGenerateStatementEpilogueBuilder::default().build(),
        }
    }
    pub fn with_for_generate_statement_preamble(
        mut self,
        n: ForGenerateStatementPreambleSyntax,
    ) -> Self {
        self.for_generate_statement_preamble = n;
        self
    }
    pub fn with_generate_statement_body(mut self, n: GenerateStatementBodySyntax) -> Self {
        self.generate_statement_body = n;
        self
    }
    pub fn with_for_generate_statement_epilogue(
        mut self,
        n: ForGenerateStatementEpilogueSyntax,
    ) -> Self {
        self.for_generate_statement_epilogue = n;
        self
    }
    pub fn build(self) -> ForGenerateStatementSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ForGenerateStatement);
        builder.push_node(self.for_generate_statement_preamble.raw().green().clone());
        builder.push_node(self.generate_statement_body.raw().green().clone());
        builder.push_node(self.for_generate_statement_epilogue.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ForGenerateStatementSyntax::cast(node).unwrap()
    }
}
pub struct ForGenerateStatementEpilogueBuilder {
    end_token: Token,
    generate_token: Token,
    identifier_token: Option<Token>,
    semi_colon_token: Token,
}
impl Default for ForGenerateStatementEpilogueBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl ForGenerateStatementEpilogueBuilder {
    pub fn new() -> Self {
        Self {
            end_token: Token::new(
                TokenKind::Keyword(Kw::End),
                Kw::End.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            generate_token: Token::new(
                TokenKind::Keyword(Kw::Generate),
                Kw::Generate.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            identifier_token: None,
            semi_colon_token: Token::new(
                TokenKind::SemiColon,
                TokenKind::SemiColon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_end_token(mut self, t: Token) -> Self {
        self.end_token = t;
        self
    }
    pub fn with_generate_token(mut self, t: Token) -> Self {
        self.generate_token = t;
        self
    }
    pub fn with_identifier_token(mut self, t: Token) -> Self {
        self.identifier_token = Some(t);
        self
    }
    pub fn with_semi_colon_token(mut self, t: Token) -> Self {
        self.semi_colon_token = t;
        self
    }
    pub fn build(self) -> ForGenerateStatementEpilogueSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ForGenerateStatementEpilogue);
        builder.push(self.end_token);
        builder.push(self.generate_token);
        if let Some(t) = self.identifier_token {
            builder.push(t);
        }
        builder.push(self.semi_colon_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ForGenerateStatementEpilogueSyntax::cast(node).unwrap()
    }
}
pub struct ForGenerateStatementPreambleBuilder {
    label: Option<LabelSyntax>,
    for_token: Token,
    parameter_specification: ParameterSpecificationSyntax,
    generate_token: Token,
}
impl ForGenerateStatementPreambleBuilder {
    pub fn new(parameter_specification: ParameterSpecificationSyntax) -> Self {
        Self {
            label: None,
            for_token: Token::new(
                TokenKind::Keyword(Kw::For),
                Kw::For.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            parameter_specification,
            generate_token: Token::new(
                TokenKind::Keyword(Kw::Generate),
                Kw::Generate.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_label(mut self, n: LabelSyntax) -> Self {
        self.label = Some(n);
        self
    }
    pub fn with_for_token(mut self, t: Token) -> Self {
        self.for_token = t;
        self
    }
    pub fn with_parameter_specification(mut self, n: ParameterSpecificationSyntax) -> Self {
        self.parameter_specification = n;
        self
    }
    pub fn with_generate_token(mut self, t: Token) -> Self {
        self.generate_token = t;
        self
    }
    pub fn build(self) -> ForGenerateStatementPreambleSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ForGenerateStatementPreamble);
        if let Some(n) = self.label {
            builder.push_node(n.raw().green().clone());
        }
        builder.push(self.for_token);
        builder.push_node(self.parameter_specification.raw().green().clone());
        builder.push(self.generate_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ForGenerateStatementPreambleSyntax::cast(node).unwrap()
    }
}
pub struct ForIterationSchemeBuilder {
    for_token: Token,
    parameter_specification: ParameterSpecificationSyntax,
}
impl ForIterationSchemeBuilder {
    pub fn new(parameter_specification: ParameterSpecificationSyntax) -> Self {
        Self {
            for_token: Token::new(
                TokenKind::Keyword(Kw::For),
                Kw::For.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            parameter_specification,
        }
    }
    pub fn with_for_token(mut self, t: Token) -> Self {
        self.for_token = t;
        self
    }
    pub fn with_parameter_specification(mut self, n: ParameterSpecificationSyntax) -> Self {
        self.parameter_specification = n;
        self
    }
    pub fn build(self) -> ForIterationSchemeSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ForIterationScheme);
        builder.push(self.for_token);
        builder.push_node(self.parameter_specification.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ForIterationSchemeSyntax::cast(node).unwrap()
    }
}
pub struct FormalPartBuilder {
    name: NameSyntax,
    parenthesized_name: ParenthesizedNameSyntax,
}
impl FormalPartBuilder {
    pub fn new(name: NameSyntax, parenthesized_name: ParenthesizedNameSyntax) -> Self {
        Self {
            name,
            parenthesized_name,
        }
    }
    pub fn with_name(mut self, n: NameSyntax) -> Self {
        self.name = n;
        self
    }
    pub fn with_parenthesized_name(mut self, n: ParenthesizedNameSyntax) -> Self {
        self.parenthesized_name = n;
        self
    }
    pub fn build(self) -> FormalPartSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::FormalPart);
        builder.push_node(self.name.raw().green().clone());
        builder.push_node(self.parenthesized_name.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        FormalPartSyntax::cast(node).unwrap()
    }
}
pub struct FullTypeDeclarationBuilder {
    type_token: Token,
    identifier_token: Token,
    is_token: Token,
    type_definition: TypeDefinitionSyntax,
    semi_colon_token: Token,
}
impl FullTypeDeclarationBuilder {
    pub fn new(identifier_token: Token, type_definition: TypeDefinitionSyntax) -> Self {
        Self {
            type_token: Token::new(
                TokenKind::Keyword(Kw::Type),
                Kw::Type.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            identifier_token,
            is_token: Token::new(
                TokenKind::Keyword(Kw::Is),
                Kw::Is.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            type_definition,
            semi_colon_token: Token::new(
                TokenKind::SemiColon,
                TokenKind::SemiColon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_type_token(mut self, t: Token) -> Self {
        self.type_token = t;
        self
    }
    pub fn with_identifier_token(mut self, t: Token) -> Self {
        self.identifier_token = t;
        self
    }
    pub fn with_is_token(mut self, t: Token) -> Self {
        self.is_token = t;
        self
    }
    pub fn with_type_definition(mut self, n: TypeDefinitionSyntax) -> Self {
        self.type_definition = n;
        self
    }
    pub fn with_semi_colon_token(mut self, t: Token) -> Self {
        self.semi_colon_token = t;
        self
    }
    pub fn build(self) -> FullTypeDeclarationSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::FullTypeDeclaration);
        builder.push(self.type_token);
        builder.push(self.identifier_token);
        builder.push(self.is_token);
        builder.push_node(self.type_definition.raw().green().clone());
        builder.push(self.semi_colon_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        FullTypeDeclarationSyntax::cast(node).unwrap()
    }
}
pub struct FunctionSpecificationBuilder {
    function_purity: Option<FunctionPuritySyntax>,
    function_token: Token,
    designator: DesignatorSyntax,
    subprogram_header: SubprogramHeaderSyntax,
    parameter_list: ParameterListSyntax,
    return_token: Token,
    name: NameSyntax,
}
impl FunctionSpecificationBuilder {
    pub fn new(designator: DesignatorSyntax, name: NameSyntax) -> Self {
        Self {
            function_purity: None,
            function_token: Token::new(
                TokenKind::Keyword(Kw::Function),
                Kw::Function.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            designator,
            subprogram_header: SubprogramHeaderBuilder::default().build(),
            parameter_list: ParameterListBuilder::default().build(),
            return_token: Token::new(
                TokenKind::Keyword(Kw::Return),
                Kw::Return.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            name,
        }
    }
    pub fn with_function_purity(mut self, n: FunctionPuritySyntax) -> Self {
        self.function_purity = Some(n);
        self
    }
    pub fn with_function_token(mut self, t: Token) -> Self {
        self.function_token = t;
        self
    }
    pub fn with_designator(mut self, n: DesignatorSyntax) -> Self {
        self.designator = n;
        self
    }
    pub fn with_subprogram_header(mut self, n: SubprogramHeaderSyntax) -> Self {
        self.subprogram_header = n;
        self
    }
    pub fn with_parameter_list(mut self, n: ParameterListSyntax) -> Self {
        self.parameter_list = n;
        self
    }
    pub fn with_return_token(mut self, t: Token) -> Self {
        self.return_token = t;
        self
    }
    pub fn with_name(mut self, n: NameSyntax) -> Self {
        self.name = n;
        self
    }
    pub fn build(self) -> FunctionSpecificationSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::FunctionSpecification);
        if let Some(n) = self.function_purity {
            builder.push(n.raw().token().clone());
        }
        builder.push(self.function_token);
        builder.push(self.designator.raw().token().clone());
        builder.push_node(self.subprogram_header.raw().green().clone());
        builder.push_node(self.parameter_list.raw().green().clone());
        builder.push(self.return_token);
        builder.push_node(self.name.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        FunctionSpecificationSyntax::cast(node).unwrap()
    }
}
pub struct GenerateStatementBodyBuilder {
    declarations: DeclarationsSyntax,
    declaration_statement_separator: Option<DeclarationStatementSeparatorSyntax>,
    concurrent_statements: ConcurrentStatementsSyntax,
    generate_statement_body_epilogue: Option<GenerateStatementBodyEpilogueSyntax>,
}
impl Default for GenerateStatementBodyBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl GenerateStatementBodyBuilder {
    pub fn new() -> Self {
        Self {
            declarations: DeclarationsBuilder::default().build(),
            declaration_statement_separator: None,
            concurrent_statements: ConcurrentStatementsBuilder::default().build(),
            generate_statement_body_epilogue: None,
        }
    }
    pub fn with_declarations(mut self, n: DeclarationsSyntax) -> Self {
        self.declarations = n;
        self
    }
    pub fn with_declaration_statement_separator(
        mut self,
        n: DeclarationStatementSeparatorSyntax,
    ) -> Self {
        self.declaration_statement_separator = Some(n);
        self
    }
    pub fn with_concurrent_statements(mut self, n: ConcurrentStatementsSyntax) -> Self {
        self.concurrent_statements = n;
        self
    }
    pub fn with_generate_statement_body_epilogue(
        mut self,
        n: GenerateStatementBodyEpilogueSyntax,
    ) -> Self {
        self.generate_statement_body_epilogue = Some(n);
        self
    }
    pub fn build(self) -> GenerateStatementBodySyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::GenerateStatementBody);
        builder.push_node(self.declarations.raw().green().clone());
        if let Some(n) = self.declaration_statement_separator {
            builder.push_node(n.raw().green().clone());
        }
        builder.push_node(self.concurrent_statements.raw().green().clone());
        if let Some(n) = self.generate_statement_body_epilogue {
            builder.push_node(n.raw().green().clone());
        }
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        GenerateStatementBodySyntax::cast(node).unwrap()
    }
}
pub struct GenerateStatementBodyEpilogueBuilder {
    end_token: Token,
    identifier_token: Option<Token>,
    semi_colon_token: Token,
}
impl Default for GenerateStatementBodyEpilogueBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl GenerateStatementBodyEpilogueBuilder {
    pub fn new() -> Self {
        Self {
            end_token: Token::new(
                TokenKind::Keyword(Kw::End),
                Kw::End.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            identifier_token: None,
            semi_colon_token: Token::new(
                TokenKind::SemiColon,
                TokenKind::SemiColon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_end_token(mut self, t: Token) -> Self {
        self.end_token = t;
        self
    }
    pub fn with_identifier_token(mut self, t: Token) -> Self {
        self.identifier_token = Some(t);
        self
    }
    pub fn with_semi_colon_token(mut self, t: Token) -> Self {
        self.semi_colon_token = t;
        self
    }
    pub fn build(self) -> GenerateStatementBodyEpilogueSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::GenerateStatementBodyEpilogue);
        builder.push(self.end_token);
        if let Some(t) = self.identifier_token {
            builder.push(t);
        }
        builder.push(self.semi_colon_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        GenerateStatementBodyEpilogueSyntax::cast(node).unwrap()
    }
}
pub struct GenericClauseBuilder {
    generic_clause_preamble: GenericClausePreambleSyntax,
    interface_list: InterfaceListSyntax,
    generic_clause_epilogue: GenericClauseEpilogueSyntax,
}
impl Default for GenericClauseBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl GenericClauseBuilder {
    pub fn new() -> Self {
        Self {
            generic_clause_preamble: GenericClausePreambleBuilder::default().build(),
            interface_list: InterfaceListBuilder::default().build(),
            generic_clause_epilogue: GenericClauseEpilogueBuilder::default().build(),
        }
    }
    pub fn with_generic_clause_preamble(mut self, n: GenericClausePreambleSyntax) -> Self {
        self.generic_clause_preamble = n;
        self
    }
    pub fn with_interface_list(mut self, n: InterfaceListSyntax) -> Self {
        self.interface_list = n;
        self
    }
    pub fn with_generic_clause_epilogue(mut self, n: GenericClauseEpilogueSyntax) -> Self {
        self.generic_clause_epilogue = n;
        self
    }
    pub fn build(self) -> GenericClauseSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::GenericClause);
        builder.push_node(self.generic_clause_preamble.raw().green().clone());
        builder.push_node(self.interface_list.raw().green().clone());
        builder.push_node(self.generic_clause_epilogue.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        GenericClauseSyntax::cast(node).unwrap()
    }
}
pub struct GenericClauseEpilogueBuilder {
    right_par_token: Token,
    semi_colon_token: Token,
}
impl Default for GenericClauseEpilogueBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl GenericClauseEpilogueBuilder {
    pub fn new() -> Self {
        Self {
            right_par_token: Token::new(
                TokenKind::RightPar,
                TokenKind::RightPar.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            semi_colon_token: Token::new(
                TokenKind::SemiColon,
                TokenKind::SemiColon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_right_par_token(mut self, t: Token) -> Self {
        self.right_par_token = t;
        self
    }
    pub fn with_semi_colon_token(mut self, t: Token) -> Self {
        self.semi_colon_token = t;
        self
    }
    pub fn build(self) -> GenericClauseEpilogueSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::GenericClauseEpilogue);
        builder.push(self.right_par_token);
        builder.push(self.semi_colon_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        GenericClauseEpilogueSyntax::cast(node).unwrap()
    }
}
pub struct GenericClausePreambleBuilder {
    generic_token: Token,
    left_par_token: Token,
}
impl Default for GenericClausePreambleBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl GenericClausePreambleBuilder {
    pub fn new() -> Self {
        Self {
            generic_token: Token::new(
                TokenKind::Keyword(Kw::Generic),
                Kw::Generic.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            left_par_token: Token::new(
                TokenKind::LeftPar,
                TokenKind::LeftPar.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_generic_token(mut self, t: Token) -> Self {
        self.generic_token = t;
        self
    }
    pub fn with_left_par_token(mut self, t: Token) -> Self {
        self.left_par_token = t;
        self
    }
    pub fn build(self) -> GenericClausePreambleSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::GenericClausePreamble);
        builder.push(self.generic_token);
        builder.push(self.left_par_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        GenericClausePreambleSyntax::cast(node).unwrap()
    }
}
pub struct GenericMapAspectBuilder {
    generic_token: Token,
    map_token: Token,
    left_par_token: Token,
    association_list: AssociationListSyntax,
    right_par_token: Token,
}
impl Default for GenericMapAspectBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl GenericMapAspectBuilder {
    pub fn new() -> Self {
        Self {
            generic_token: Token::new(
                TokenKind::Keyword(Kw::Generic),
                Kw::Generic.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            map_token: Token::new(
                TokenKind::Keyword(Kw::Map),
                Kw::Map.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            left_par_token: Token::new(
                TokenKind::LeftPar,
                TokenKind::LeftPar.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            association_list: AssociationListBuilder::default().build(),
            right_par_token: Token::new(
                TokenKind::RightPar,
                TokenKind::RightPar.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_generic_token(mut self, t: Token) -> Self {
        self.generic_token = t;
        self
    }
    pub fn with_map_token(mut self, t: Token) -> Self {
        self.map_token = t;
        self
    }
    pub fn with_left_par_token(mut self, t: Token) -> Self {
        self.left_par_token = t;
        self
    }
    pub fn with_association_list(mut self, n: AssociationListSyntax) -> Self {
        self.association_list = n;
        self
    }
    pub fn with_right_par_token(mut self, t: Token) -> Self {
        self.right_par_token = t;
        self
    }
    pub fn build(self) -> GenericMapAspectSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::GenericMapAspect);
        builder.push(self.generic_token);
        builder.push(self.map_token);
        builder.push(self.left_par_token);
        builder.push_node(self.association_list.raw().green().clone());
        builder.push(self.right_par_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        GenericMapAspectSyntax::cast(node).unwrap()
    }
}
pub struct GroupConstituentListBuilder {
    names: Vec<NameSyntax>,
    comma_token: Vec<Token>,
}
impl Default for GroupConstituentListBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl GroupConstituentListBuilder {
    pub fn new() -> Self {
        Self {
            names: Vec::new(),
            comma_token: Vec::new(),
        }
    }
    pub fn add_names(mut self, n: NameSyntax) -> Self {
        self.names.push(n);
        self
    }
    pub fn add_comma_token(mut self, t: Token) -> Self {
        self.comma_token.push(t);
        self
    }
    pub fn build(self) -> GroupConstituentListSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::GroupConstituentList);
        for n in self.names {
            builder.push_node(n.raw().green().clone());
        }
        for t in self.comma_token {
            builder.push(t);
        }
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        GroupConstituentListSyntax::cast(node).unwrap()
    }
}
pub struct GroupDeclarationBuilder {
    group_token: Token,
    identifier_token: Token,
    colon_token: Token,
    name: NameSyntax,
    left_par_token: Token,
    group_constituent_list: GroupConstituentListSyntax,
    right_par_token: Token,
    semi_colon_token: Token,
}
impl GroupDeclarationBuilder {
    pub fn new(identifier_token: Token, name: NameSyntax) -> Self {
        Self {
            group_token: Token::new(
                TokenKind::Keyword(Kw::Group),
                Kw::Group.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            identifier_token,
            colon_token: Token::new(
                TokenKind::Colon,
                TokenKind::Colon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            name,
            left_par_token: Token::new(
                TokenKind::LeftPar,
                TokenKind::LeftPar.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            group_constituent_list: GroupConstituentListBuilder::default().build(),
            right_par_token: Token::new(
                TokenKind::RightPar,
                TokenKind::RightPar.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            semi_colon_token: Token::new(
                TokenKind::SemiColon,
                TokenKind::SemiColon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_group_token(mut self, t: Token) -> Self {
        self.group_token = t;
        self
    }
    pub fn with_identifier_token(mut self, t: Token) -> Self {
        self.identifier_token = t;
        self
    }
    pub fn with_colon_token(mut self, t: Token) -> Self {
        self.colon_token = t;
        self
    }
    pub fn with_name(mut self, n: NameSyntax) -> Self {
        self.name = n;
        self
    }
    pub fn with_left_par_token(mut self, t: Token) -> Self {
        self.left_par_token = t;
        self
    }
    pub fn with_group_constituent_list(mut self, n: GroupConstituentListSyntax) -> Self {
        self.group_constituent_list = n;
        self
    }
    pub fn with_right_par_token(mut self, t: Token) -> Self {
        self.right_par_token = t;
        self
    }
    pub fn with_semi_colon_token(mut self, t: Token) -> Self {
        self.semi_colon_token = t;
        self
    }
    pub fn build(self) -> GroupDeclarationSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::GroupDeclaration);
        builder.push(self.group_token);
        builder.push(self.identifier_token);
        builder.push(self.colon_token);
        builder.push_node(self.name.raw().green().clone());
        builder.push(self.left_par_token);
        builder.push_node(self.group_constituent_list.raw().green().clone());
        builder.push(self.right_par_token);
        builder.push(self.semi_colon_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        GroupDeclarationSyntax::cast(node).unwrap()
    }
}
pub struct GroupTemplateDeclarationBuilder {
    group_token: Token,
    identifier_token: Token,
    is_token: Token,
    left_par_token: Token,
    entity_class_entry_list: EntityClassEntryListSyntax,
    right_par_token: Token,
    semi_colon_token: Token,
}
impl GroupTemplateDeclarationBuilder {
    pub fn new(identifier_token: Token) -> Self {
        Self {
            group_token: Token::new(
                TokenKind::Keyword(Kw::Group),
                Kw::Group.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            identifier_token,
            is_token: Token::new(
                TokenKind::Keyword(Kw::Is),
                Kw::Is.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            left_par_token: Token::new(
                TokenKind::LeftPar,
                TokenKind::LeftPar.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            entity_class_entry_list: EntityClassEntryListBuilder::default().build(),
            right_par_token: Token::new(
                TokenKind::RightPar,
                TokenKind::RightPar.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            semi_colon_token: Token::new(
                TokenKind::SemiColon,
                TokenKind::SemiColon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_group_token(mut self, t: Token) -> Self {
        self.group_token = t;
        self
    }
    pub fn with_identifier_token(mut self, t: Token) -> Self {
        self.identifier_token = t;
        self
    }
    pub fn with_is_token(mut self, t: Token) -> Self {
        self.is_token = t;
        self
    }
    pub fn with_left_par_token(mut self, t: Token) -> Self {
        self.left_par_token = t;
        self
    }
    pub fn with_entity_class_entry_list(mut self, n: EntityClassEntryListSyntax) -> Self {
        self.entity_class_entry_list = n;
        self
    }
    pub fn with_right_par_token(mut self, t: Token) -> Self {
        self.right_par_token = t;
        self
    }
    pub fn with_semi_colon_token(mut self, t: Token) -> Self {
        self.semi_colon_token = t;
        self
    }
    pub fn build(self) -> GroupTemplateDeclarationSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::GroupTemplateDeclaration);
        builder.push(self.group_token);
        builder.push(self.identifier_token);
        builder.push(self.is_token);
        builder.push(self.left_par_token);
        builder.push_node(self.entity_class_entry_list.raw().green().clone());
        builder.push(self.right_par_token);
        builder.push(self.semi_colon_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        GroupTemplateDeclarationSyntax::cast(node).unwrap()
    }
}
pub struct GuardedSignalSpecificationBuilder {
    signal_list: SignalListSyntax,
    colon_token: Token,
    name: NameSyntax,
}
impl GuardedSignalSpecificationBuilder {
    pub fn new(signal_list: SignalListSyntax, name: NameSyntax) -> Self {
        Self {
            signal_list,
            colon_token: Token::new(
                TokenKind::Colon,
                TokenKind::Colon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            name,
        }
    }
    pub fn with_signal_list(mut self, n: SignalListSyntax) -> Self {
        self.signal_list = n;
        self
    }
    pub fn with_colon_token(mut self, t: Token) -> Self {
        self.colon_token = t;
        self
    }
    pub fn with_name(mut self, n: NameSyntax) -> Self {
        self.name = n;
        self
    }
    pub fn build(self) -> GuardedSignalSpecificationSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::GuardedSignalSpecification);
        builder.push_node(self.signal_list.raw().green().clone());
        builder.push(self.colon_token);
        builder.push_node(self.name.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        GuardedSignalSpecificationSyntax::cast(node).unwrap()
    }
}
pub struct IdentifierListBuilder {
    identifier_token: Token,
    comma_token: Token,
}
impl IdentifierListBuilder {
    pub fn new(identifier_token: Token) -> Self {
        Self {
            identifier_token,
            comma_token: Token::new(
                TokenKind::Comma,
                TokenKind::Comma.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_identifier_token(mut self, t: Token) -> Self {
        self.identifier_token = t;
        self
    }
    pub fn with_comma_token(mut self, t: Token) -> Self {
        self.comma_token = t;
        self
    }
    pub fn build(self) -> IdentifierListSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::IdentifierList);
        builder.push(self.identifier_token);
        builder.push(self.comma_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        IdentifierListSyntax::cast(node).unwrap()
    }
}
pub struct IfGenerateElseBuilder {
    else_token: Token,
    label: Option<LabelSyntax>,
    generate_token: Token,
    generate_statement_body: GenerateStatementBodySyntax,
}
impl Default for IfGenerateElseBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl IfGenerateElseBuilder {
    pub fn new() -> Self {
        Self {
            else_token: Token::new(
                TokenKind::Keyword(Kw::Else),
                Kw::Else.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            label: None,
            generate_token: Token::new(
                TokenKind::Keyword(Kw::Generate),
                Kw::Generate.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            generate_statement_body: GenerateStatementBodyBuilder::default().build(),
        }
    }
    pub fn with_else_token(mut self, t: Token) -> Self {
        self.else_token = t;
        self
    }
    pub fn with_label(mut self, n: LabelSyntax) -> Self {
        self.label = Some(n);
        self
    }
    pub fn with_generate_token(mut self, t: Token) -> Self {
        self.generate_token = t;
        self
    }
    pub fn with_generate_statement_body(mut self, n: GenerateStatementBodySyntax) -> Self {
        self.generate_statement_body = n;
        self
    }
    pub fn build(self) -> IfGenerateElseSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::IfGenerateElse);
        builder.push(self.else_token);
        if let Some(n) = self.label {
            builder.push_node(n.raw().green().clone());
        }
        builder.push(self.generate_token);
        builder.push_node(self.generate_statement_body.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        IfGenerateElseSyntax::cast(node).unwrap()
    }
}
pub struct IfGenerateElsifBuilder {
    elsif_token: Token,
    label: Option<LabelSyntax>,
    expression: ExpressionSyntax,
    generate_token: Token,
    generate_statement_body: GenerateStatementBodySyntax,
}
impl IfGenerateElsifBuilder {
    pub fn new(expression: ExpressionSyntax) -> Self {
        Self {
            elsif_token: Token::new(
                TokenKind::Keyword(Kw::Elsif),
                Kw::Elsif.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            label: None,
            expression,
            generate_token: Token::new(
                TokenKind::Keyword(Kw::Generate),
                Kw::Generate.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            generate_statement_body: GenerateStatementBodyBuilder::default().build(),
        }
    }
    pub fn with_elsif_token(mut self, t: Token) -> Self {
        self.elsif_token = t;
        self
    }
    pub fn with_label(mut self, n: LabelSyntax) -> Self {
        self.label = Some(n);
        self
    }
    pub fn with_expression(mut self, n: ExpressionSyntax) -> Self {
        self.expression = n;
        self
    }
    pub fn with_generate_token(mut self, t: Token) -> Self {
        self.generate_token = t;
        self
    }
    pub fn with_generate_statement_body(mut self, n: GenerateStatementBodySyntax) -> Self {
        self.generate_statement_body = n;
        self
    }
    pub fn build(self) -> IfGenerateElsifSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::IfGenerateElsif);
        builder.push(self.elsif_token);
        if let Some(n) = self.label {
            builder.push_node(n.raw().green().clone());
        }
        builder.push_node(self.expression.raw().green().clone());
        builder.push(self.generate_token);
        builder.push_node(self.generate_statement_body.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        IfGenerateElsifSyntax::cast(node).unwrap()
    }
}
pub struct IfGenerateStatementBuilder {
    if_generate_statement_preamble: IfGenerateStatementPreambleSyntax,
    generate_statement_body: GenerateStatementBodySyntax,
    if_generate_elsifs: Vec<IfGenerateElsifSyntax>,
    if_generate_else: Option<IfGenerateElseSyntax>,
    if_generate_statement_epilogue: IfGenerateStatementEpilogueSyntax,
}
impl IfGenerateStatementBuilder {
    pub fn new(if_generate_statement_preamble: IfGenerateStatementPreambleSyntax) -> Self {
        Self {
            if_generate_statement_preamble,
            generate_statement_body: GenerateStatementBodyBuilder::default().build(),
            if_generate_elsifs: Vec::new(),
            if_generate_else: None,
            if_generate_statement_epilogue: IfGenerateStatementEpilogueBuilder::default().build(),
        }
    }
    pub fn with_if_generate_statement_preamble(
        mut self,
        n: IfGenerateStatementPreambleSyntax,
    ) -> Self {
        self.if_generate_statement_preamble = n;
        self
    }
    pub fn with_generate_statement_body(mut self, n: GenerateStatementBodySyntax) -> Self {
        self.generate_statement_body = n;
        self
    }
    pub fn add_if_generate_elsifs(mut self, n: IfGenerateElsifSyntax) -> Self {
        self.if_generate_elsifs.push(n);
        self
    }
    pub fn with_if_generate_else(mut self, n: IfGenerateElseSyntax) -> Self {
        self.if_generate_else = Some(n);
        self
    }
    pub fn with_if_generate_statement_epilogue(
        mut self,
        n: IfGenerateStatementEpilogueSyntax,
    ) -> Self {
        self.if_generate_statement_epilogue = n;
        self
    }
    pub fn build(self) -> IfGenerateStatementSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::IfGenerateStatement);
        builder.push_node(self.if_generate_statement_preamble.raw().green().clone());
        builder.push_node(self.generate_statement_body.raw().green().clone());
        for n in self.if_generate_elsifs {
            builder.push_node(n.raw().green().clone());
        }
        if let Some(n) = self.if_generate_else {
            builder.push_node(n.raw().green().clone());
        }
        builder.push_node(self.if_generate_statement_epilogue.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        IfGenerateStatementSyntax::cast(node).unwrap()
    }
}
pub struct IfGenerateStatementEpilogueBuilder {
    end_token: Token,
    generate_token: Token,
    identifier_token: Option<Token>,
    semi_colon_token: Token,
}
impl Default for IfGenerateStatementEpilogueBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl IfGenerateStatementEpilogueBuilder {
    pub fn new() -> Self {
        Self {
            end_token: Token::new(
                TokenKind::Keyword(Kw::End),
                Kw::End.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            generate_token: Token::new(
                TokenKind::Keyword(Kw::Generate),
                Kw::Generate.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            identifier_token: None,
            semi_colon_token: Token::new(
                TokenKind::SemiColon,
                TokenKind::SemiColon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_end_token(mut self, t: Token) -> Self {
        self.end_token = t;
        self
    }
    pub fn with_generate_token(mut self, t: Token) -> Self {
        self.generate_token = t;
        self
    }
    pub fn with_identifier_token(mut self, t: Token) -> Self {
        self.identifier_token = Some(t);
        self
    }
    pub fn with_semi_colon_token(mut self, t: Token) -> Self {
        self.semi_colon_token = t;
        self
    }
    pub fn build(self) -> IfGenerateStatementEpilogueSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::IfGenerateStatementEpilogue);
        builder.push(self.end_token);
        builder.push(self.generate_token);
        if let Some(t) = self.identifier_token {
            builder.push(t);
        }
        builder.push(self.semi_colon_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        IfGenerateStatementEpilogueSyntax::cast(node).unwrap()
    }
}
pub struct IfGenerateStatementPreambleBuilder {
    label: Option<LabelSyntax>,
    if_token: Token,
    alternative_label: Option<LabelSyntax>,
    condition: ExpressionSyntax,
    generate_token: Token,
}
impl IfGenerateStatementPreambleBuilder {
    pub fn new(condition: ExpressionSyntax) -> Self {
        Self {
            label: None,
            if_token: Token::new(
                TokenKind::Keyword(Kw::If),
                Kw::If.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            alternative_label: None,
            condition,
            generate_token: Token::new(
                TokenKind::Keyword(Kw::Generate),
                Kw::Generate.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_label(mut self, n: LabelSyntax) -> Self {
        self.label = Some(n);
        self
    }
    pub fn with_if_token(mut self, t: Token) -> Self {
        self.if_token = t;
        self
    }
    pub fn with_alternative_label(mut self, n: LabelSyntax) -> Self {
        self.alternative_label = Some(n);
        self
    }
    pub fn with_condition(mut self, n: ExpressionSyntax) -> Self {
        self.condition = n;
        self
    }
    pub fn with_generate_token(mut self, t: Token) -> Self {
        self.generate_token = t;
        self
    }
    pub fn build(self) -> IfGenerateStatementPreambleSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::IfGenerateStatementPreamble);
        if let Some(n) = self.label {
            builder.push_node(n.raw().green().clone());
        }
        builder.push(self.if_token);
        if let Some(n) = self.alternative_label {
            builder.push_node(n.raw().green().clone());
        }
        builder.push_node(self.condition.raw().green().clone());
        builder.push(self.generate_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        IfGenerateStatementPreambleSyntax::cast(node).unwrap()
    }
}
pub struct IfStatementBuilder {
    if_statement_preamble: IfStatementPreambleSyntax,
    sequential_statements: SequentialStatementsSyntax,
    if_statement_elsifs: Vec<IfStatementElsifSyntax>,
    if_statement_else: Option<IfStatementElseSyntax>,
    if_statement_epilogue: IfStatementEpilogueSyntax,
}
impl IfStatementBuilder {
    pub fn new(if_statement_preamble: IfStatementPreambleSyntax) -> Self {
        Self {
            if_statement_preamble,
            sequential_statements: SequentialStatementsBuilder::default().build(),
            if_statement_elsifs: Vec::new(),
            if_statement_else: None,
            if_statement_epilogue: IfStatementEpilogueBuilder::default().build(),
        }
    }
    pub fn with_if_statement_preamble(mut self, n: IfStatementPreambleSyntax) -> Self {
        self.if_statement_preamble = n;
        self
    }
    pub fn with_sequential_statements(mut self, n: SequentialStatementsSyntax) -> Self {
        self.sequential_statements = n;
        self
    }
    pub fn add_if_statement_elsifs(mut self, n: IfStatementElsifSyntax) -> Self {
        self.if_statement_elsifs.push(n);
        self
    }
    pub fn with_if_statement_else(mut self, n: IfStatementElseSyntax) -> Self {
        self.if_statement_else = Some(n);
        self
    }
    pub fn with_if_statement_epilogue(mut self, n: IfStatementEpilogueSyntax) -> Self {
        self.if_statement_epilogue = n;
        self
    }
    pub fn build(self) -> IfStatementSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::IfStatement);
        builder.push_node(self.if_statement_preamble.raw().green().clone());
        builder.push_node(self.sequential_statements.raw().green().clone());
        for n in self.if_statement_elsifs {
            builder.push_node(n.raw().green().clone());
        }
        if let Some(n) = self.if_statement_else {
            builder.push_node(n.raw().green().clone());
        }
        builder.push_node(self.if_statement_epilogue.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        IfStatementSyntax::cast(node).unwrap()
    }
}
pub struct IfStatementElseBuilder {
    else_token: Token,
    sequential_statements: SequentialStatementsSyntax,
}
impl Default for IfStatementElseBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl IfStatementElseBuilder {
    pub fn new() -> Self {
        Self {
            else_token: Token::new(
                TokenKind::Keyword(Kw::Else),
                Kw::Else.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            sequential_statements: SequentialStatementsBuilder::default().build(),
        }
    }
    pub fn with_else_token(mut self, t: Token) -> Self {
        self.else_token = t;
        self
    }
    pub fn with_sequential_statements(mut self, n: SequentialStatementsSyntax) -> Self {
        self.sequential_statements = n;
        self
    }
    pub fn build(self) -> IfStatementElseSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::IfStatementElse);
        builder.push(self.else_token);
        builder.push_node(self.sequential_statements.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        IfStatementElseSyntax::cast(node).unwrap()
    }
}
pub struct IfStatementElsifBuilder {
    elsif_token: Token,
    expression: ExpressionSyntax,
    then_token: Token,
    sequential_statements: SequentialStatementsSyntax,
}
impl IfStatementElsifBuilder {
    pub fn new(expression: ExpressionSyntax) -> Self {
        Self {
            elsif_token: Token::new(
                TokenKind::Keyword(Kw::Elsif),
                Kw::Elsif.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            expression,
            then_token: Token::new(
                TokenKind::Keyword(Kw::Then),
                Kw::Then.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            sequential_statements: SequentialStatementsBuilder::default().build(),
        }
    }
    pub fn with_elsif_token(mut self, t: Token) -> Self {
        self.elsif_token = t;
        self
    }
    pub fn with_expression(mut self, n: ExpressionSyntax) -> Self {
        self.expression = n;
        self
    }
    pub fn with_then_token(mut self, t: Token) -> Self {
        self.then_token = t;
        self
    }
    pub fn with_sequential_statements(mut self, n: SequentialStatementsSyntax) -> Self {
        self.sequential_statements = n;
        self
    }
    pub fn build(self) -> IfStatementElsifSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::IfStatementElsif);
        builder.push(self.elsif_token);
        builder.push_node(self.expression.raw().green().clone());
        builder.push(self.then_token);
        builder.push_node(self.sequential_statements.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        IfStatementElsifSyntax::cast(node).unwrap()
    }
}
pub struct IfStatementEpilogueBuilder {
    end_token: Token,
    if_token: Token,
    identifier_token: Option<Token>,
    semi_colon_token: Token,
}
impl Default for IfStatementEpilogueBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl IfStatementEpilogueBuilder {
    pub fn new() -> Self {
        Self {
            end_token: Token::new(
                TokenKind::Keyword(Kw::End),
                Kw::End.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            if_token: Token::new(
                TokenKind::Keyword(Kw::If),
                Kw::If.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            identifier_token: None,
            semi_colon_token: Token::new(
                TokenKind::SemiColon,
                TokenKind::SemiColon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_end_token(mut self, t: Token) -> Self {
        self.end_token = t;
        self
    }
    pub fn with_if_token(mut self, t: Token) -> Self {
        self.if_token = t;
        self
    }
    pub fn with_identifier_token(mut self, t: Token) -> Self {
        self.identifier_token = Some(t);
        self
    }
    pub fn with_semi_colon_token(mut self, t: Token) -> Self {
        self.semi_colon_token = t;
        self
    }
    pub fn build(self) -> IfStatementEpilogueSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::IfStatementEpilogue);
        builder.push(self.end_token);
        builder.push(self.if_token);
        if let Some(t) = self.identifier_token {
            builder.push(t);
        }
        builder.push(self.semi_colon_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        IfStatementEpilogueSyntax::cast(node).unwrap()
    }
}
pub struct IfStatementPreambleBuilder {
    if_label_token: Option<Token>,
    colon_token: Option<Token>,
    if_token: Token,
    expression: ExpressionSyntax,
    then_token: Token,
}
impl IfStatementPreambleBuilder {
    pub fn new(expression: ExpressionSyntax) -> Self {
        Self {
            if_label_token: None,
            colon_token: None,
            if_token: Token::new(
                TokenKind::Keyword(Kw::If),
                Kw::If.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            expression,
            then_token: Token::new(
                TokenKind::Keyword(Kw::Then),
                Kw::Then.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_if_label_token(mut self, t: Token) -> Self {
        self.if_label_token = Some(t);
        self
    }
    pub fn with_colon_token(mut self, t: Token) -> Self {
        self.colon_token = Some(t);
        self
    }
    pub fn with_if_token(mut self, t: Token) -> Self {
        self.if_token = t;
        self
    }
    pub fn with_expression(mut self, n: ExpressionSyntax) -> Self {
        self.expression = n;
        self
    }
    pub fn with_then_token(mut self, t: Token) -> Self {
        self.then_token = t;
        self
    }
    pub fn build(self) -> IfStatementPreambleSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::IfStatementPreamble);
        if let Some(t) = self.if_label_token {
            builder.push(t);
        }
        if let Some(t) = self.colon_token {
            builder.push(t);
        }
        builder.push(self.if_token);
        builder.push_node(self.expression.raw().green().clone());
        builder.push(self.then_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        IfStatementPreambleSyntax::cast(node).unwrap()
    }
}
pub struct IncompleteTypeDeclarationBuilder {
    type_token: Token,
    identifier_token: Token,
    semi_colon_token: Token,
}
impl IncompleteTypeDeclarationBuilder {
    pub fn new(identifier_token: Token) -> Self {
        Self {
            type_token: Token::new(
                TokenKind::Keyword(Kw::Type),
                Kw::Type.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            identifier_token,
            semi_colon_token: Token::new(
                TokenKind::SemiColon,
                TokenKind::SemiColon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_type_token(mut self, t: Token) -> Self {
        self.type_token = t;
        self
    }
    pub fn with_identifier_token(mut self, t: Token) -> Self {
        self.identifier_token = t;
        self
    }
    pub fn with_semi_colon_token(mut self, t: Token) -> Self {
        self.semi_colon_token = t;
        self
    }
    pub fn build(self) -> IncompleteTypeDeclarationSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::IncompleteTypeDeclaration);
        builder.push(self.type_token);
        builder.push(self.identifier_token);
        builder.push(self.semi_colon_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        IncompleteTypeDeclarationSyntax::cast(node).unwrap()
    }
}
pub struct IndexConstraintBuilder {
    left_par_token: Token,
    discrete_ranges: Vec<DiscreteRangeSyntax>,
    comma_token: Vec<Token>,
    right_par_token: Token,
}
impl Default for IndexConstraintBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl IndexConstraintBuilder {
    pub fn new() -> Self {
        Self {
            left_par_token: Token::new(
                TokenKind::LeftPar,
                TokenKind::LeftPar.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            discrete_ranges: Vec::new(),
            comma_token: Vec::new(),
            right_par_token: Token::new(
                TokenKind::RightPar,
                TokenKind::RightPar.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_left_par_token(mut self, t: Token) -> Self {
        self.left_par_token = t;
        self
    }
    pub fn add_discrete_ranges(mut self, n: DiscreteRangeSyntax) -> Self {
        self.discrete_ranges.push(n);
        self
    }
    pub fn add_comma_token(mut self, t: Token) -> Self {
        self.comma_token.push(t);
        self
    }
    pub fn with_right_par_token(mut self, t: Token) -> Self {
        self.right_par_token = t;
        self
    }
    pub fn build(self) -> IndexConstraintSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::IndexConstraint);
        builder.push(self.left_par_token);
        for n in self.discrete_ranges {
            builder.push_node(n.raw().green().clone());
        }
        for t in self.comma_token {
            builder.push(t);
        }
        builder.push(self.right_par_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        IndexConstraintSyntax::cast(node).unwrap()
    }
}
pub struct IndexSubtypeDefinitionBuilder {
    name: NameSyntax,
    range_token: Token,
    box_token: Token,
}
impl IndexSubtypeDefinitionBuilder {
    pub fn new(name: NameSyntax) -> Self {
        Self {
            name,
            range_token: Token::new(
                TokenKind::Keyword(Kw::Range),
                Kw::Range.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            box_token: Token::new(
                TokenKind::BOX,
                TokenKind::BOX.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_name(mut self, n: NameSyntax) -> Self {
        self.name = n;
        self
    }
    pub fn with_range_token(mut self, t: Token) -> Self {
        self.range_token = t;
        self
    }
    pub fn with_box_token(mut self, t: Token) -> Self {
        self.box_token = t;
        self
    }
    pub fn build(self) -> IndexSubtypeDefinitionSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::IndexSubtypeDefinition);
        builder.push_node(self.name.raw().green().clone());
        builder.push(self.range_token);
        builder.push(self.box_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        IndexSubtypeDefinitionSyntax::cast(node).unwrap()
    }
}
pub struct IndexSubtypeDefinitionListBuilder {
    index_subtype_definitions: Vec<IndexSubtypeDefinitionSyntax>,
    comma_token: Vec<Token>,
}
impl Default for IndexSubtypeDefinitionListBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl IndexSubtypeDefinitionListBuilder {
    pub fn new() -> Self {
        Self {
            index_subtype_definitions: Vec::new(),
            comma_token: Vec::new(),
        }
    }
    pub fn add_index_subtype_definitions(mut self, n: IndexSubtypeDefinitionSyntax) -> Self {
        self.index_subtype_definitions.push(n);
        self
    }
    pub fn add_comma_token(mut self, t: Token) -> Self {
        self.comma_token.push(t);
        self
    }
    pub fn build(self) -> IndexSubtypeDefinitionListSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::IndexSubtypeDefinitionList);
        for n in self.index_subtype_definitions {
            builder.push_node(n.raw().green().clone());
        }
        for t in self.comma_token {
            builder.push(t);
        }
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        IndexSubtypeDefinitionListSyntax::cast(node).unwrap()
    }
}
pub struct InertialDelayMechanismBuilder {
    reject_token: Option<Token>,
    expression: Option<ExpressionSyntax>,
    inertial_token: Token,
}
impl Default for InertialDelayMechanismBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl InertialDelayMechanismBuilder {
    pub fn new() -> Self {
        Self {
            reject_token: None,
            expression: None,
            inertial_token: Token::new(
                TokenKind::Keyword(Kw::Inertial),
                Kw::Inertial.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_reject_token(mut self, t: Token) -> Self {
        self.reject_token = Some(t);
        self
    }
    pub fn with_expression(mut self, n: ExpressionSyntax) -> Self {
        self.expression = Some(n);
        self
    }
    pub fn with_inertial_token(mut self, t: Token) -> Self {
        self.inertial_token = t;
        self
    }
    pub fn build(self) -> InertialDelayMechanismSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::InertialDelayMechanism);
        if let Some(t) = self.reject_token {
            builder.push(t);
        }
        if let Some(n) = self.expression {
            builder.push_node(n.raw().green().clone());
        }
        builder.push(self.inertial_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        InertialDelayMechanismSyntax::cast(node).unwrap()
    }
}
pub struct InstantiationListAllBuilder {
    all_token: Token,
}
impl Default for InstantiationListAllBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl InstantiationListAllBuilder {
    pub fn new() -> Self {
        Self {
            all_token: Token::new(
                TokenKind::Keyword(Kw::All),
                Kw::All.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_all_token(mut self, t: Token) -> Self {
        self.all_token = t;
        self
    }
    pub fn build(self) -> InstantiationListAllSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::InstantiationListAll);
        builder.push(self.all_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        InstantiationListAllSyntax::cast(node).unwrap()
    }
}
pub struct InstantiationListListBuilder {
    identifier_token: Vec<Token>,
    comma_token: Vec<Token>,
}
impl Default for InstantiationListListBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl InstantiationListListBuilder {
    pub fn new() -> Self {
        Self {
            identifier_token: Vec::new(),
            comma_token: Vec::new(),
        }
    }
    pub fn add_identifier_token(mut self, t: Token) -> Self {
        self.identifier_token.push(t);
        self
    }
    pub fn add_comma_token(mut self, t: Token) -> Self {
        self.comma_token.push(t);
        self
    }
    pub fn build(self) -> InstantiationListListSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::InstantiationListList);
        for t in self.identifier_token {
            builder.push(t);
        }
        for t in self.comma_token {
            builder.push(t);
        }
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        InstantiationListListSyntax::cast(node).unwrap()
    }
}
pub struct InstantiationListOthersBuilder {
    others_token: Token,
}
impl Default for InstantiationListOthersBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl InstantiationListOthersBuilder {
    pub fn new() -> Self {
        Self {
            others_token: Token::new(
                TokenKind::Keyword(Kw::Others),
                Kw::Others.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_others_token(mut self, t: Token) -> Self {
        self.others_token = t;
        self
    }
    pub fn build(self) -> InstantiationListOthersSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::InstantiationListOthers);
        builder.push(self.others_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        InstantiationListOthersSyntax::cast(node).unwrap()
    }
}
pub struct InterfaceConstantDeclarationBuilder {
    constant_token: Token,
    identifier_list: IdentifierListSyntax,
    colon_token: Token,
    in_token: Token,
    subtype_indication: SubtypeIndicationSyntax,
    colon_eq_token: Token,
    expression: ExpressionSyntax,
}
impl InterfaceConstantDeclarationBuilder {
    pub fn new(
        identifier_list: IdentifierListSyntax,
        subtype_indication: SubtypeIndicationSyntax,
        expression: ExpressionSyntax,
    ) -> Self {
        Self {
            constant_token: Token::new(
                TokenKind::Keyword(Kw::Constant),
                Kw::Constant.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            identifier_list,
            colon_token: Token::new(
                TokenKind::Colon,
                TokenKind::Colon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            in_token: Token::new(
                TokenKind::Keyword(Kw::In),
                Kw::In.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            subtype_indication,
            colon_eq_token: Token::new(
                TokenKind::ColonEq,
                TokenKind::ColonEq.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            expression,
        }
    }
    pub fn with_constant_token(mut self, t: Token) -> Self {
        self.constant_token = t;
        self
    }
    pub fn with_identifier_list(mut self, n: IdentifierListSyntax) -> Self {
        self.identifier_list = n;
        self
    }
    pub fn with_colon_token(mut self, t: Token) -> Self {
        self.colon_token = t;
        self
    }
    pub fn with_in_token(mut self, t: Token) -> Self {
        self.in_token = t;
        self
    }
    pub fn with_subtype_indication(mut self, n: SubtypeIndicationSyntax) -> Self {
        self.subtype_indication = n;
        self
    }
    pub fn with_colon_eq_token(mut self, t: Token) -> Self {
        self.colon_eq_token = t;
        self
    }
    pub fn with_expression(mut self, n: ExpressionSyntax) -> Self {
        self.expression = n;
        self
    }
    pub fn build(self) -> InterfaceConstantDeclarationSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::InterfaceConstantDeclaration);
        builder.push(self.constant_token);
        builder.push_node(self.identifier_list.raw().green().clone());
        builder.push(self.colon_token);
        builder.push(self.in_token);
        builder.push_node(self.subtype_indication.raw().green().clone());
        builder.push(self.colon_eq_token);
        builder.push_node(self.expression.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        InterfaceConstantDeclarationSyntax::cast(node).unwrap()
    }
}
pub struct InterfaceFileDeclarationBuilder {
    file_token: Token,
    identifier_list: IdentifierListSyntax,
    colon_token: Token,
    subtype_indication: SubtypeIndicationSyntax,
}
impl InterfaceFileDeclarationBuilder {
    pub fn new(
        identifier_list: IdentifierListSyntax,
        subtype_indication: SubtypeIndicationSyntax,
    ) -> Self {
        Self {
            file_token: Token::new(
                TokenKind::Keyword(Kw::File),
                Kw::File.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            identifier_list,
            colon_token: Token::new(
                TokenKind::Colon,
                TokenKind::Colon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            subtype_indication,
        }
    }
    pub fn with_file_token(mut self, t: Token) -> Self {
        self.file_token = t;
        self
    }
    pub fn with_identifier_list(mut self, n: IdentifierListSyntax) -> Self {
        self.identifier_list = n;
        self
    }
    pub fn with_colon_token(mut self, t: Token) -> Self {
        self.colon_token = t;
        self
    }
    pub fn with_subtype_indication(mut self, n: SubtypeIndicationSyntax) -> Self {
        self.subtype_indication = n;
        self
    }
    pub fn build(self) -> InterfaceFileDeclarationSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::InterfaceFileDeclaration);
        builder.push(self.file_token);
        builder.push_node(self.identifier_list.raw().green().clone());
        builder.push(self.colon_token);
        builder.push_node(self.subtype_indication.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        InterfaceFileDeclarationSyntax::cast(node).unwrap()
    }
}
pub struct InterfaceFunctionSpecificationBuilder {
    function_purity: Option<FunctionPuritySyntax>,
    function_token: Token,
    designator: DesignatorSyntax,
    parameter_list: Option<ParameterListSyntax>,
    return_token: Token,
    name: NameSyntax,
}
impl InterfaceFunctionSpecificationBuilder {
    pub fn new(designator: DesignatorSyntax, name: NameSyntax) -> Self {
        Self {
            function_purity: None,
            function_token: Token::new(
                TokenKind::Keyword(Kw::Function),
                Kw::Function.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            designator,
            parameter_list: None,
            return_token: Token::new(
                TokenKind::Keyword(Kw::Return),
                Kw::Return.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            name,
        }
    }
    pub fn with_function_purity(mut self, n: FunctionPuritySyntax) -> Self {
        self.function_purity = Some(n);
        self
    }
    pub fn with_function_token(mut self, t: Token) -> Self {
        self.function_token = t;
        self
    }
    pub fn with_designator(mut self, n: DesignatorSyntax) -> Self {
        self.designator = n;
        self
    }
    pub fn with_parameter_list(mut self, n: ParameterListSyntax) -> Self {
        self.parameter_list = Some(n);
        self
    }
    pub fn with_return_token(mut self, t: Token) -> Self {
        self.return_token = t;
        self
    }
    pub fn with_name(mut self, n: NameSyntax) -> Self {
        self.name = n;
        self
    }
    pub fn build(self) -> InterfaceFunctionSpecificationSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::InterfaceFunctionSpecification);
        if let Some(n) = self.function_purity {
            builder.push(n.raw().token().clone());
        }
        builder.push(self.function_token);
        builder.push(self.designator.raw().token().clone());
        if let Some(n) = self.parameter_list {
            builder.push_node(n.raw().green().clone());
        }
        builder.push(self.return_token);
        builder.push_node(self.name.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        InterfaceFunctionSpecificationSyntax::cast(node).unwrap()
    }
}
pub struct InterfaceIncompleteTypeDeclarationBuilder {
    type_token: Token,
    identifier_token: Token,
    semi_colon_token: Token,
}
impl InterfaceIncompleteTypeDeclarationBuilder {
    pub fn new(identifier_token: Token) -> Self {
        Self {
            type_token: Token::new(
                TokenKind::Keyword(Kw::Type),
                Kw::Type.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            identifier_token,
            semi_colon_token: Token::new(
                TokenKind::SemiColon,
                TokenKind::SemiColon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_type_token(mut self, t: Token) -> Self {
        self.type_token = t;
        self
    }
    pub fn with_identifier_token(mut self, t: Token) -> Self {
        self.identifier_token = t;
        self
    }
    pub fn with_semi_colon_token(mut self, t: Token) -> Self {
        self.semi_colon_token = t;
        self
    }
    pub fn build(self) -> InterfaceIncompleteTypeDeclarationSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::InterfaceIncompleteTypeDeclaration);
        builder.push(self.type_token);
        builder.push(self.identifier_token);
        builder.push(self.semi_colon_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        InterfaceIncompleteTypeDeclarationSyntax::cast(node).unwrap()
    }
}
pub struct InterfaceListBuilder {
    interface_declarations: Vec<InterfaceDeclarationSyntax>,
    semi_colon_token: Vec<Token>,
}
impl Default for InterfaceListBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl InterfaceListBuilder {
    pub fn new() -> Self {
        Self {
            interface_declarations: Vec::new(),
            semi_colon_token: Vec::new(),
        }
    }
    pub fn add_interface_declarations(mut self, n: InterfaceDeclarationSyntax) -> Self {
        self.interface_declarations.push(n);
        self
    }
    pub fn add_semi_colon_token(mut self, t: Token) -> Self {
        self.semi_colon_token.push(t);
        self
    }
    pub fn build(self) -> InterfaceListSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::InterfaceList);
        for n in self.interface_declarations {
            builder.push_node(n.raw().green().clone());
        }
        for t in self.semi_colon_token {
            builder.push(t);
        }
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        InterfaceListSyntax::cast(node).unwrap()
    }
}
pub struct InterfacePackageDeclarationBuilder {
    interface_package_declaration_preamble: InterfacePackageDeclarationPreambleSyntax,
    new_token: Token,
    name: NameSyntax,
    interface_package_generic_map_aspect: InterfacePackageGenericMapAspectSyntax,
    semi_colon_token: Token,
}
impl InterfacePackageDeclarationBuilder {
    pub fn new(
        interface_package_declaration_preamble: InterfacePackageDeclarationPreambleSyntax,
        name: NameSyntax,
        interface_package_generic_map_aspect: InterfacePackageGenericMapAspectSyntax,
    ) -> Self {
        Self {
            interface_package_declaration_preamble,
            new_token: Token::new(
                TokenKind::Keyword(Kw::New),
                Kw::New.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            name,
            interface_package_generic_map_aspect,
            semi_colon_token: Token::new(
                TokenKind::SemiColon,
                TokenKind::SemiColon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_interface_package_declaration_preamble(
        mut self,
        n: InterfacePackageDeclarationPreambleSyntax,
    ) -> Self {
        self.interface_package_declaration_preamble = n;
        self
    }
    pub fn with_new_token(mut self, t: Token) -> Self {
        self.new_token = t;
        self
    }
    pub fn with_name(mut self, n: NameSyntax) -> Self {
        self.name = n;
        self
    }
    pub fn with_interface_package_generic_map_aspect(
        mut self,
        n: InterfacePackageGenericMapAspectSyntax,
    ) -> Self {
        self.interface_package_generic_map_aspect = n;
        self
    }
    pub fn with_semi_colon_token(mut self, t: Token) -> Self {
        self.semi_colon_token = t;
        self
    }
    pub fn build(self) -> InterfacePackageDeclarationSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::InterfacePackageDeclaration);
        builder.push_node(
            self.interface_package_declaration_preamble
                .raw()
                .green()
                .clone(),
        );
        builder.push(self.new_token);
        builder.push_node(self.name.raw().green().clone());
        builder.push_node(
            self.interface_package_generic_map_aspect
                .raw()
                .green()
                .clone(),
        );
        builder.push(self.semi_colon_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        InterfacePackageDeclarationSyntax::cast(node).unwrap()
    }
}
pub struct InterfacePackageDeclarationPreambleBuilder {
    package_token: Token,
    identifier_token: Token,
    is_token: Token,
}
impl InterfacePackageDeclarationPreambleBuilder {
    pub fn new(identifier_token: Token) -> Self {
        Self {
            package_token: Token::new(
                TokenKind::Keyword(Kw::Package),
                Kw::Package.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            identifier_token,
            is_token: Token::new(
                TokenKind::Keyword(Kw::Is),
                Kw::Is.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_package_token(mut self, t: Token) -> Self {
        self.package_token = t;
        self
    }
    pub fn with_identifier_token(mut self, t: Token) -> Self {
        self.identifier_token = t;
        self
    }
    pub fn with_is_token(mut self, t: Token) -> Self {
        self.is_token = t;
        self
    }
    pub fn build(self) -> InterfacePackageDeclarationPreambleSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::InterfacePackageDeclarationPreamble);
        builder.push(self.package_token);
        builder.push(self.identifier_token);
        builder.push(self.is_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        InterfacePackageDeclarationPreambleSyntax::cast(node).unwrap()
    }
}
pub struct InterfacePackageGenericMapAspectBuilder {
    generic_token: Token,
    map_token: Token,
    left_par_token: Token,
    interface_package_generic_map_aspect_inner: InterfacePackageGenericMapAspectInnerSyntax,
    right_par_token: Token,
}
impl InterfacePackageGenericMapAspectBuilder {
    pub fn new(
        interface_package_generic_map_aspect_inner: InterfacePackageGenericMapAspectInnerSyntax,
    ) -> Self {
        Self {
            generic_token: Token::new(
                TokenKind::Keyword(Kw::Generic),
                Kw::Generic.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            map_token: Token::new(
                TokenKind::Keyword(Kw::Map),
                Kw::Map.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            left_par_token: Token::new(
                TokenKind::LeftPar,
                TokenKind::LeftPar.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            interface_package_generic_map_aspect_inner,
            right_par_token: Token::new(
                TokenKind::RightPar,
                TokenKind::RightPar.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_generic_token(mut self, t: Token) -> Self {
        self.generic_token = t;
        self
    }
    pub fn with_map_token(mut self, t: Token) -> Self {
        self.map_token = t;
        self
    }
    pub fn with_left_par_token(mut self, t: Token) -> Self {
        self.left_par_token = t;
        self
    }
    pub fn with_interface_package_generic_map_aspect_inner(
        mut self,
        n: InterfacePackageGenericMapAspectInnerSyntax,
    ) -> Self {
        self.interface_package_generic_map_aspect_inner = n;
        self
    }
    pub fn with_right_par_token(mut self, t: Token) -> Self {
        self.right_par_token = t;
        self
    }
    pub fn build(self) -> InterfacePackageGenericMapAspectSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::InterfacePackageGenericMapAspect);
        builder.push(self.generic_token);
        builder.push(self.map_token);
        builder.push(self.left_par_token);
        builder.push_node(
            self.interface_package_generic_map_aspect_inner
                .raw()
                .green()
                .clone(),
        );
        builder.push(self.right_par_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        InterfacePackageGenericMapAspectSyntax::cast(node).unwrap()
    }
}
pub struct InterfacePackageGenericMapAspectAssociationsBuilder {
    association_list: AssociationListSyntax,
}
impl Default for InterfacePackageGenericMapAspectAssociationsBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl InterfacePackageGenericMapAspectAssociationsBuilder {
    pub fn new() -> Self {
        Self {
            association_list: AssociationListBuilder::default().build(),
        }
    }
    pub fn with_association_list(mut self, n: AssociationListSyntax) -> Self {
        self.association_list = n;
        self
    }
    pub fn build(self) -> InterfacePackageGenericMapAspectAssociationsSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::InterfacePackageGenericMapAspectAssociations);
        builder.push_node(self.association_list.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        InterfacePackageGenericMapAspectAssociationsSyntax::cast(node).unwrap()
    }
}
pub struct InterfacePackageGenericMapAspectBoxBuilder {
    box_token: Token,
}
impl Default for InterfacePackageGenericMapAspectBoxBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl InterfacePackageGenericMapAspectBoxBuilder {
    pub fn new() -> Self {
        Self {
            box_token: Token::new(
                TokenKind::BOX,
                TokenKind::BOX.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_box_token(mut self, t: Token) -> Self {
        self.box_token = t;
        self
    }
    pub fn build(self) -> InterfacePackageGenericMapAspectBoxSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::InterfacePackageGenericMapAspectBox);
        builder.push(self.box_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        InterfacePackageGenericMapAspectBoxSyntax::cast(node).unwrap()
    }
}
pub struct InterfacePackageGenericMapAspectDefaultBuilder {
    default_token: Token,
}
impl Default for InterfacePackageGenericMapAspectDefaultBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl InterfacePackageGenericMapAspectDefaultBuilder {
    pub fn new() -> Self {
        Self {
            default_token: Token::new(
                TokenKind::Keyword(Kw::Default),
                Kw::Default.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_default_token(mut self, t: Token) -> Self {
        self.default_token = t;
        self
    }
    pub fn build(self) -> InterfacePackageGenericMapAspectDefaultSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::InterfacePackageGenericMapAspectDefault);
        builder.push(self.default_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        InterfacePackageGenericMapAspectDefaultSyntax::cast(node).unwrap()
    }
}
pub struct InterfaceProcedureSpecificationBuilder {
    procedure_token: Token,
    designator: DesignatorSyntax,
    parameter_token: Token,
    left_par_token: Token,
    interface_list: InterfaceListSyntax,
    right_par_token: Token,
}
impl InterfaceProcedureSpecificationBuilder {
    pub fn new(designator: DesignatorSyntax) -> Self {
        Self {
            procedure_token: Token::new(
                TokenKind::Keyword(Kw::Procedure),
                Kw::Procedure.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            designator,
            parameter_token: Token::new(
                TokenKind::Keyword(Kw::Parameter),
                Kw::Parameter.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            left_par_token: Token::new(
                TokenKind::LeftPar,
                TokenKind::LeftPar.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            interface_list: InterfaceListBuilder::default().build(),
            right_par_token: Token::new(
                TokenKind::RightPar,
                TokenKind::RightPar.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_procedure_token(mut self, t: Token) -> Self {
        self.procedure_token = t;
        self
    }
    pub fn with_designator(mut self, n: DesignatorSyntax) -> Self {
        self.designator = n;
        self
    }
    pub fn with_parameter_token(mut self, t: Token) -> Self {
        self.parameter_token = t;
        self
    }
    pub fn with_left_par_token(mut self, t: Token) -> Self {
        self.left_par_token = t;
        self
    }
    pub fn with_interface_list(mut self, n: InterfaceListSyntax) -> Self {
        self.interface_list = n;
        self
    }
    pub fn with_right_par_token(mut self, t: Token) -> Self {
        self.right_par_token = t;
        self
    }
    pub fn build(self) -> InterfaceProcedureSpecificationSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::InterfaceProcedureSpecification);
        builder.push(self.procedure_token);
        builder.push(self.designator.raw().token().clone());
        builder.push(self.parameter_token);
        builder.push(self.left_par_token);
        builder.push_node(self.interface_list.raw().green().clone());
        builder.push(self.right_par_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        InterfaceProcedureSpecificationSyntax::cast(node).unwrap()
    }
}
pub struct InterfaceSignalDeclarationBuilder {
    signal_token: Option<Token>,
    identifier_list: IdentifierListSyntax,
    colon_token: Token,
    mode: Option<ModeSyntax>,
    subtype_indication: SubtypeIndicationSyntax,
    bus_token: Option<Token>,
    colon_eq_token: Option<Token>,
    expression: Option<ExpressionSyntax>,
    semi_colon_token: Token,
}
impl InterfaceSignalDeclarationBuilder {
    pub fn new(
        identifier_list: IdentifierListSyntax,
        subtype_indication: SubtypeIndicationSyntax,
    ) -> Self {
        Self {
            signal_token: None,
            identifier_list,
            colon_token: Token::new(
                TokenKind::Colon,
                TokenKind::Colon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            mode: None,
            subtype_indication,
            bus_token: None,
            colon_eq_token: None,
            expression: None,
            semi_colon_token: Token::new(
                TokenKind::SemiColon,
                TokenKind::SemiColon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_signal_token(mut self, t: Token) -> Self {
        self.signal_token = Some(t);
        self
    }
    pub fn with_identifier_list(mut self, n: IdentifierListSyntax) -> Self {
        self.identifier_list = n;
        self
    }
    pub fn with_colon_token(mut self, t: Token) -> Self {
        self.colon_token = t;
        self
    }
    pub fn with_mode(mut self, n: ModeSyntax) -> Self {
        self.mode = Some(n);
        self
    }
    pub fn with_subtype_indication(mut self, n: SubtypeIndicationSyntax) -> Self {
        self.subtype_indication = n;
        self
    }
    pub fn with_bus_token(mut self, t: Token) -> Self {
        self.bus_token = Some(t);
        self
    }
    pub fn with_colon_eq_token(mut self, t: Token) -> Self {
        self.colon_eq_token = Some(t);
        self
    }
    pub fn with_expression(mut self, n: ExpressionSyntax) -> Self {
        self.expression = Some(n);
        self
    }
    pub fn with_semi_colon_token(mut self, t: Token) -> Self {
        self.semi_colon_token = t;
        self
    }
    pub fn build(self) -> InterfaceSignalDeclarationSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::InterfaceSignalDeclaration);
        if let Some(t) = self.signal_token {
            builder.push(t);
        }
        builder.push_node(self.identifier_list.raw().green().clone());
        builder.push(self.colon_token);
        if let Some(n) = self.mode {
            builder.push(n.raw().token().clone());
        }
        builder.push_node(self.subtype_indication.raw().green().clone());
        if let Some(t) = self.bus_token {
            builder.push(t);
        }
        if let Some(t) = self.colon_eq_token {
            builder.push(t);
        }
        if let Some(n) = self.expression {
            builder.push_node(n.raw().green().clone());
        }
        builder.push(self.semi_colon_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        InterfaceSignalDeclarationSyntax::cast(node).unwrap()
    }
}
pub struct InterfaceSubprogramDeclarationBuilder {
    interface_subprogram_specification: InterfaceSubprogramSpecificationSyntax,
    is_token: Option<Token>,
    interface_subprogram_default: Option<InterfaceSubprogramDefaultSyntax>,
    semi_colon_token: Token,
}
impl InterfaceSubprogramDeclarationBuilder {
    pub fn new(interface_subprogram_specification: InterfaceSubprogramSpecificationSyntax) -> Self {
        Self {
            interface_subprogram_specification,
            is_token: None,
            interface_subprogram_default: None,
            semi_colon_token: Token::new(
                TokenKind::SemiColon,
                TokenKind::SemiColon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_interface_subprogram_specification(
        mut self,
        n: InterfaceSubprogramSpecificationSyntax,
    ) -> Self {
        self.interface_subprogram_specification = n;
        self
    }
    pub fn with_is_token(mut self, t: Token) -> Self {
        self.is_token = Some(t);
        self
    }
    pub fn with_interface_subprogram_default(
        mut self,
        n: InterfaceSubprogramDefaultSyntax,
    ) -> Self {
        self.interface_subprogram_default = Some(n);
        self
    }
    pub fn with_semi_colon_token(mut self, t: Token) -> Self {
        self.semi_colon_token = t;
        self
    }
    pub fn build(self) -> InterfaceSubprogramDeclarationSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::InterfaceSubprogramDeclaration);
        builder.push_node(
            self.interface_subprogram_specification
                .raw()
                .green()
                .clone(),
        );
        if let Some(t) = self.is_token {
            builder.push(t);
        }
        if let Some(n) = self.interface_subprogram_default {
            builder.push_node(n.raw().green().clone());
        }
        builder.push(self.semi_colon_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        InterfaceSubprogramDeclarationSyntax::cast(node).unwrap()
    }
}
pub struct InterfaceSubprogramDefaultBoxBuilder {
    box_token: Token,
}
impl Default for InterfaceSubprogramDefaultBoxBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl InterfaceSubprogramDefaultBoxBuilder {
    pub fn new() -> Self {
        Self {
            box_token: Token::new(
                TokenKind::BOX,
                TokenKind::BOX.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_box_token(mut self, t: Token) -> Self {
        self.box_token = t;
        self
    }
    pub fn build(self) -> InterfaceSubprogramDefaultBoxSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::InterfaceSubprogramDefaultBox);
        builder.push(self.box_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        InterfaceSubprogramDefaultBoxSyntax::cast(node).unwrap()
    }
}
pub struct InterfaceSubprogramDefaultNameBuilder {
    name: NameSyntax,
}
impl InterfaceSubprogramDefaultNameBuilder {
    pub fn new(name: NameSyntax) -> Self {
        Self { name }
    }
    pub fn with_name(mut self, n: NameSyntax) -> Self {
        self.name = n;
        self
    }
    pub fn build(self) -> InterfaceSubprogramDefaultNameSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::InterfaceSubprogramDefaultName);
        builder.push_node(self.name.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        InterfaceSubprogramDefaultNameSyntax::cast(node).unwrap()
    }
}
pub struct InterfaceVariableDeclarationBuilder {
    variable_token: Option<Token>,
    identifier_list: IdentifierListSyntax,
    colon_token: Token,
    mode: Option<ModeSyntax>,
    subtype_indication: SubtypeIndicationSyntax,
    colon_eq_token: Option<Token>,
    expression: Option<ExpressionSyntax>,
    semi_colon_token: Token,
}
impl InterfaceVariableDeclarationBuilder {
    pub fn new(
        identifier_list: IdentifierListSyntax,
        subtype_indication: SubtypeIndicationSyntax,
    ) -> Self {
        Self {
            variable_token: None,
            identifier_list,
            colon_token: Token::new(
                TokenKind::Colon,
                TokenKind::Colon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            mode: None,
            subtype_indication,
            colon_eq_token: None,
            expression: None,
            semi_colon_token: Token::new(
                TokenKind::SemiColon,
                TokenKind::SemiColon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_variable_token(mut self, t: Token) -> Self {
        self.variable_token = Some(t);
        self
    }
    pub fn with_identifier_list(mut self, n: IdentifierListSyntax) -> Self {
        self.identifier_list = n;
        self
    }
    pub fn with_colon_token(mut self, t: Token) -> Self {
        self.colon_token = t;
        self
    }
    pub fn with_mode(mut self, n: ModeSyntax) -> Self {
        self.mode = Some(n);
        self
    }
    pub fn with_subtype_indication(mut self, n: SubtypeIndicationSyntax) -> Self {
        self.subtype_indication = n;
        self
    }
    pub fn with_colon_eq_token(mut self, t: Token) -> Self {
        self.colon_eq_token = Some(t);
        self
    }
    pub fn with_expression(mut self, n: ExpressionSyntax) -> Self {
        self.expression = Some(n);
        self
    }
    pub fn with_semi_colon_token(mut self, t: Token) -> Self {
        self.semi_colon_token = t;
        self
    }
    pub fn build(self) -> InterfaceVariableDeclarationSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::InterfaceVariableDeclaration);
        if let Some(t) = self.variable_token {
            builder.push(t);
        }
        builder.push_node(self.identifier_list.raw().green().clone());
        builder.push(self.colon_token);
        if let Some(n) = self.mode {
            builder.push(n.raw().token().clone());
        }
        builder.push_node(self.subtype_indication.raw().green().clone());
        if let Some(t) = self.colon_eq_token {
            builder.push(t);
        }
        if let Some(n) = self.expression {
            builder.push_node(n.raw().green().clone());
        }
        builder.push(self.semi_colon_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        InterfaceVariableDeclarationSyntax::cast(node).unwrap()
    }
}
pub struct LabelBuilder {
    identifier_token: Token,
    colon_token: Token,
}
impl LabelBuilder {
    pub fn new(identifier_token: Token) -> Self {
        Self {
            identifier_token,
            colon_token: Token::new(
                TokenKind::Colon,
                TokenKind::Colon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_identifier_token(mut self, t: Token) -> Self {
        self.identifier_token = t;
        self
    }
    pub fn with_colon_token(mut self, t: Token) -> Self {
        self.colon_token = t;
        self
    }
    pub fn build(self) -> LabelSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::Label);
        builder.push(self.identifier_token);
        builder.push(self.colon_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        LabelSyntax::cast(node).unwrap()
    }
}
pub struct LibraryClauseBuilder {
    library_token: Token,
    identifier_list: IdentifierListSyntax,
    semi_colon_token: Token,
}
impl LibraryClauseBuilder {
    pub fn new(identifier_list: IdentifierListSyntax) -> Self {
        Self {
            library_token: Token::new(
                TokenKind::Keyword(Kw::Library),
                Kw::Library.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            identifier_list,
            semi_colon_token: Token::new(
                TokenKind::SemiColon,
                TokenKind::SemiColon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_library_token(mut self, t: Token) -> Self {
        self.library_token = t;
        self
    }
    pub fn with_identifier_list(mut self, n: IdentifierListSyntax) -> Self {
        self.identifier_list = n;
        self
    }
    pub fn with_semi_colon_token(mut self, t: Token) -> Self {
        self.semi_colon_token = t;
        self
    }
    pub fn build(self) -> LibraryClauseSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::LibraryClause);
        builder.push(self.library_token);
        builder.push_node(self.identifier_list.raw().green().clone());
        builder.push(self.semi_colon_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        LibraryClauseSyntax::cast(node).unwrap()
    }
}
pub struct LiteralExpressionBuilder {
    literal: LiteralSyntax,
}
impl LiteralExpressionBuilder {
    pub fn new(literal: LiteralSyntax) -> Self {
        Self { literal }
    }
    pub fn with_literal(mut self, n: LiteralSyntax) -> Self {
        self.literal = n;
        self
    }
    pub fn build(self) -> LiteralExpressionSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::LiteralExpression);
        builder.push(self.literal.raw().token().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        LiteralExpressionSyntax::cast(node).unwrap()
    }
}
pub struct LoopStatementBuilder {
    loop_statement_preamble: LoopStatementPreambleSyntax,
    sequential_statements: SequentialStatementsSyntax,
    loop_statement_epilogue: LoopStatementEpilogueSyntax,
}
impl LoopStatementBuilder {
    pub fn new(loop_statement_preamble: LoopStatementPreambleSyntax) -> Self {
        Self {
            loop_statement_preamble,
            sequential_statements: SequentialStatementsBuilder::default().build(),
            loop_statement_epilogue: LoopStatementEpilogueBuilder::default().build(),
        }
    }
    pub fn with_loop_statement_preamble(mut self, n: LoopStatementPreambleSyntax) -> Self {
        self.loop_statement_preamble = n;
        self
    }
    pub fn with_sequential_statements(mut self, n: SequentialStatementsSyntax) -> Self {
        self.sequential_statements = n;
        self
    }
    pub fn with_loop_statement_epilogue(mut self, n: LoopStatementEpilogueSyntax) -> Self {
        self.loop_statement_epilogue = n;
        self
    }
    pub fn build(self) -> LoopStatementSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::LoopStatement);
        builder.push_node(self.loop_statement_preamble.raw().green().clone());
        builder.push_node(self.sequential_statements.raw().green().clone());
        builder.push_node(self.loop_statement_epilogue.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        LoopStatementSyntax::cast(node).unwrap()
    }
}
pub struct LoopStatementEpilogueBuilder {
    end_token: Token,
    loop_token: Token,
    identifier_token: Option<Token>,
    semi_colon_token: Token,
}
impl Default for LoopStatementEpilogueBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl LoopStatementEpilogueBuilder {
    pub fn new() -> Self {
        Self {
            end_token: Token::new(
                TokenKind::Keyword(Kw::End),
                Kw::End.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            loop_token: Token::new(
                TokenKind::Keyword(Kw::Loop),
                Kw::Loop.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            identifier_token: None,
            semi_colon_token: Token::new(
                TokenKind::SemiColon,
                TokenKind::SemiColon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_end_token(mut self, t: Token) -> Self {
        self.end_token = t;
        self
    }
    pub fn with_loop_token(mut self, t: Token) -> Self {
        self.loop_token = t;
        self
    }
    pub fn with_identifier_token(mut self, t: Token) -> Self {
        self.identifier_token = Some(t);
        self
    }
    pub fn with_semi_colon_token(mut self, t: Token) -> Self {
        self.semi_colon_token = t;
        self
    }
    pub fn build(self) -> LoopStatementEpilogueSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::LoopStatementEpilogue);
        builder.push(self.end_token);
        builder.push(self.loop_token);
        if let Some(t) = self.identifier_token {
            builder.push(t);
        }
        builder.push(self.semi_colon_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        LoopStatementEpilogueSyntax::cast(node).unwrap()
    }
}
pub struct LoopStatementPreambleBuilder {
    label: LabelSyntax,
    iteration_scheme: Option<IterationSchemeSyntax>,
    loop_token: Token,
}
impl LoopStatementPreambleBuilder {
    pub fn new(label: LabelSyntax) -> Self {
        Self {
            label,
            iteration_scheme: None,
            loop_token: Token::new(
                TokenKind::Keyword(Kw::Loop),
                Kw::Loop.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_label(mut self, n: LabelSyntax) -> Self {
        self.label = n;
        self
    }
    pub fn with_iteration_scheme(mut self, n: IterationSchemeSyntax) -> Self {
        self.iteration_scheme = Some(n);
        self
    }
    pub fn with_loop_token(mut self, t: Token) -> Self {
        self.loop_token = t;
        self
    }
    pub fn build(self) -> LoopStatementPreambleSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::LoopStatementPreamble);
        builder.push_node(self.label.raw().green().clone());
        if let Some(n) = self.iteration_scheme {
            builder.push_node(n.raw().green().clone());
        }
        builder.push(self.loop_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        LoopStatementPreambleSyntax::cast(node).unwrap()
    }
}
pub struct NameBuilder {
    name_prefix: NamePrefixSyntax,
    name_tails: Vec<NameTailSyntax>,
}
impl NameBuilder {
    pub fn new(name_prefix: NamePrefixSyntax) -> Self {
        Self {
            name_prefix,
            name_tails: Vec::new(),
        }
    }
    pub fn with_name_prefix(mut self, n: NamePrefixSyntax) -> Self {
        self.name_prefix = n;
        self
    }
    pub fn add_name_tails(mut self, n: NameTailSyntax) -> Self {
        self.name_tails.push(n);
        self
    }
    pub fn build(self) -> NameSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::Name);
        builder.push_node(self.name_prefix.raw().green().clone());
        for n in self.name_tails {
            builder.push_node(n.raw().green().clone());
        }
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        NameSyntax::cast(node).unwrap()
    }
}
pub struct NameDesignatorPrefixBuilder {
    name_designator: NameDesignatorSyntax,
}
impl NameDesignatorPrefixBuilder {
    pub fn new(name_designator: NameDesignatorSyntax) -> Self {
        Self { name_designator }
    }
    pub fn with_name_designator(mut self, n: NameDesignatorSyntax) -> Self {
        self.name_designator = n;
        self
    }
    pub fn build(self) -> NameDesignatorPrefixSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::NameDesignatorPrefix);
        builder.push(self.name_designator.raw().token().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        NameDesignatorPrefixSyntax::cast(node).unwrap()
    }
}
pub struct NameExpressionBuilder {
    name: NameSyntax,
}
impl NameExpressionBuilder {
    pub fn new(name: NameSyntax) -> Self {
        Self { name }
    }
    pub fn with_name(mut self, n: NameSyntax) -> Self {
        self.name = n;
        self
    }
    pub fn build(self) -> NameExpressionSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::NameExpression);
        builder.push_node(self.name.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        NameExpressionSyntax::cast(node).unwrap()
    }
}
pub struct NameListBuilder {
    names: Vec<NameSyntax>,
    comma_token: Vec<Token>,
}
impl Default for NameListBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl NameListBuilder {
    pub fn new() -> Self {
        Self {
            names: Vec::new(),
            comma_token: Vec::new(),
        }
    }
    pub fn add_names(mut self, n: NameSyntax) -> Self {
        self.names.push(n);
        self
    }
    pub fn add_comma_token(mut self, t: Token) -> Self {
        self.comma_token.push(t);
        self
    }
    pub fn build(self) -> NameListSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::NameList);
        for n in self.names {
            builder.push_node(n.raw().green().clone());
        }
        for t in self.comma_token {
            builder.push(t);
        }
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        NameListSyntax::cast(node).unwrap()
    }
}
pub struct NameResolutionIndicationBuilder {
    name: NameSyntax,
}
impl NameResolutionIndicationBuilder {
    pub fn new(name: NameSyntax) -> Self {
        Self { name }
    }
    pub fn with_name(mut self, n: NameSyntax) -> Self {
        self.name = n;
        self
    }
    pub fn build(self) -> NameResolutionIndicationSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::NameResolutionIndication);
        builder.push_node(self.name.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        NameResolutionIndicationSyntax::cast(node).unwrap()
    }
}
pub struct NameTargetBuilder {
    name: NameSyntax,
}
impl NameTargetBuilder {
    pub fn new(name: NameSyntax) -> Self {
        Self { name }
    }
    pub fn with_name(mut self, n: NameSyntax) -> Self {
        self.name = n;
        self
    }
    pub fn build(self) -> NameTargetSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::NameTarget);
        builder.push_node(self.name.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        NameTargetSyntax::cast(node).unwrap()
    }
}
pub struct NextStatementBuilder {
    label: LabelSyntax,
    next_token: Token,
    loop_label_token: Option<Token>,
    when_token: Option<Token>,
    expression: Option<ExpressionSyntax>,
    semi_colon_token: Token,
}
impl NextStatementBuilder {
    pub fn new(label: LabelSyntax) -> Self {
        Self {
            label,
            next_token: Token::new(
                TokenKind::Keyword(Kw::Next),
                Kw::Next.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            loop_label_token: None,
            when_token: None,
            expression: None,
            semi_colon_token: Token::new(
                TokenKind::SemiColon,
                TokenKind::SemiColon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_label(mut self, n: LabelSyntax) -> Self {
        self.label = n;
        self
    }
    pub fn with_next_token(mut self, t: Token) -> Self {
        self.next_token = t;
        self
    }
    pub fn with_loop_label_token(mut self, t: Token) -> Self {
        self.loop_label_token = Some(t);
        self
    }
    pub fn with_when_token(mut self, t: Token) -> Self {
        self.when_token = Some(t);
        self
    }
    pub fn with_expression(mut self, n: ExpressionSyntax) -> Self {
        self.expression = Some(n);
        self
    }
    pub fn with_semi_colon_token(mut self, t: Token) -> Self {
        self.semi_colon_token = t;
        self
    }
    pub fn build(self) -> NextStatementSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::NextStatement);
        builder.push_node(self.label.raw().green().clone());
        builder.push(self.next_token);
        if let Some(t) = self.loop_label_token {
            builder.push(t);
        }
        if let Some(t) = self.when_token {
            builder.push(t);
        }
        if let Some(n) = self.expression {
            builder.push_node(n.raw().green().clone());
        }
        builder.push(self.semi_colon_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        NextStatementSyntax::cast(node).unwrap()
    }
}
pub struct NullStatementBuilder {
    label: LabelSyntax,
    null_token: Token,
    semi_colon_token: Token,
}
impl NullStatementBuilder {
    pub fn new(label: LabelSyntax) -> Self {
        Self {
            label,
            null_token: Token::new(
                TokenKind::Keyword(Kw::Null),
                Kw::Null.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            semi_colon_token: Token::new(
                TokenKind::SemiColon,
                TokenKind::SemiColon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_label(mut self, n: LabelSyntax) -> Self {
        self.label = n;
        self
    }
    pub fn with_null_token(mut self, t: Token) -> Self {
        self.null_token = t;
        self
    }
    pub fn with_semi_colon_token(mut self, t: Token) -> Self {
        self.semi_colon_token = t;
        self
    }
    pub fn build(self) -> NullStatementSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::NullStatement);
        builder.push_node(self.label.raw().green().clone());
        builder.push(self.null_token);
        builder.push(self.semi_colon_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        NullStatementSyntax::cast(node).unwrap()
    }
}
pub struct NumericTypeDefinitionBuilder {
    range_constraint: RangeConstraintSyntax,
}
impl NumericTypeDefinitionBuilder {
    pub fn new(range_constraint: RangeConstraintSyntax) -> Self {
        Self { range_constraint }
    }
    pub fn with_range_constraint(mut self, n: RangeConstraintSyntax) -> Self {
        self.range_constraint = n;
        self
    }
    pub fn build(self) -> NumericTypeDefinitionSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::NumericTypeDefinition);
        builder.push_node(self.range_constraint.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        NumericTypeDefinitionSyntax::cast(node).unwrap()
    }
}
pub struct OpenDiscreteRangeBuilder {
    open_token: Token,
}
impl Default for OpenDiscreteRangeBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl OpenDiscreteRangeBuilder {
    pub fn new() -> Self {
        Self {
            open_token: Token::new(
                TokenKind::Keyword(Kw::Open),
                Kw::Open.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_open_token(mut self, t: Token) -> Self {
        self.open_token = t;
        self
    }
    pub fn build(self) -> OpenDiscreteRangeSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::OpenDiscreteRange);
        builder.push(self.open_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        OpenDiscreteRangeSyntax::cast(node).unwrap()
    }
}
pub struct OthersChoiceBuilder {
    others_token: Token,
}
impl Default for OthersChoiceBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl OthersChoiceBuilder {
    pub fn new() -> Self {
        Self {
            others_token: Token::new(
                TokenKind::Keyword(Kw::Others),
                Kw::Others.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_others_token(mut self, t: Token) -> Self {
        self.others_token = t;
        self
    }
    pub fn build(self) -> OthersChoiceSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::OthersChoice);
        builder.push(self.others_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        OthersChoiceSyntax::cast(node).unwrap()
    }
}
pub struct PackageBuilder {
    package_preamble: PackagePreambleSyntax,
    package_header: PackageHeaderSyntax,
    declarations: DeclarationsSyntax,
    package_epilogue: PackageEpilogueSyntax,
}
impl PackageBuilder {
    pub fn new(package_preamble: PackagePreambleSyntax) -> Self {
        Self {
            package_preamble,
            package_header: PackageHeaderBuilder::default().build(),
            declarations: DeclarationsBuilder::default().build(),
            package_epilogue: PackageEpilogueBuilder::default().build(),
        }
    }
    pub fn with_package_preamble(mut self, n: PackagePreambleSyntax) -> Self {
        self.package_preamble = n;
        self
    }
    pub fn with_package_header(mut self, n: PackageHeaderSyntax) -> Self {
        self.package_header = n;
        self
    }
    pub fn with_declarations(mut self, n: DeclarationsSyntax) -> Self {
        self.declarations = n;
        self
    }
    pub fn with_package_epilogue(mut self, n: PackageEpilogueSyntax) -> Self {
        self.package_epilogue = n;
        self
    }
    pub fn build(self) -> PackageSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::Package);
        builder.push_node(self.package_preamble.raw().green().clone());
        builder.push_node(self.package_header.raw().green().clone());
        builder.push_node(self.declarations.raw().green().clone());
        builder.push_node(self.package_epilogue.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        PackageSyntax::cast(node).unwrap()
    }
}
pub struct PackageBodyBuilder {
    package_body_preamble: PackageBodyPreambleSyntax,
    declarations: DeclarationsSyntax,
    package_body_epilogue: PackageBodyEpilogueSyntax,
}
impl PackageBodyBuilder {
    pub fn new(package_body_preamble: PackageBodyPreambleSyntax) -> Self {
        Self {
            package_body_preamble,
            declarations: DeclarationsBuilder::default().build(),
            package_body_epilogue: PackageBodyEpilogueBuilder::default().build(),
        }
    }
    pub fn with_package_body_preamble(mut self, n: PackageBodyPreambleSyntax) -> Self {
        self.package_body_preamble = n;
        self
    }
    pub fn with_declarations(mut self, n: DeclarationsSyntax) -> Self {
        self.declarations = n;
        self
    }
    pub fn with_package_body_epilogue(mut self, n: PackageBodyEpilogueSyntax) -> Self {
        self.package_body_epilogue = n;
        self
    }
    pub fn build(self) -> PackageBodySyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::PackageBody);
        builder.push_node(self.package_body_preamble.raw().green().clone());
        builder.push_node(self.declarations.raw().green().clone());
        builder.push_node(self.package_body_epilogue.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        PackageBodySyntax::cast(node).unwrap()
    }
}
pub struct PackageBodyDeclarationBuilder {
    package_body: PackageBodySyntax,
}
impl PackageBodyDeclarationBuilder {
    pub fn new(package_body: PackageBodySyntax) -> Self {
        Self { package_body }
    }
    pub fn with_package_body(mut self, n: PackageBodySyntax) -> Self {
        self.package_body = n;
        self
    }
    pub fn build(self) -> PackageBodyDeclarationSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::PackageBodyDeclaration);
        builder.push_node(self.package_body.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        PackageBodyDeclarationSyntax::cast(node).unwrap()
    }
}
pub struct PackageBodyEpilogueBuilder {
    end_token: Token,
    package_token: Option<Token>,
    body_token: Option<Token>,
    identifier_token: Option<Token>,
    semi_colon_token: Token,
}
impl Default for PackageBodyEpilogueBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl PackageBodyEpilogueBuilder {
    pub fn new() -> Self {
        Self {
            end_token: Token::new(
                TokenKind::Keyword(Kw::End),
                Kw::End.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            package_token: None,
            body_token: None,
            identifier_token: None,
            semi_colon_token: Token::new(
                TokenKind::SemiColon,
                TokenKind::SemiColon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_end_token(mut self, t: Token) -> Self {
        self.end_token = t;
        self
    }
    pub fn with_package_token(mut self, t: Token) -> Self {
        self.package_token = Some(t);
        self
    }
    pub fn with_body_token(mut self, t: Token) -> Self {
        self.body_token = Some(t);
        self
    }
    pub fn with_identifier_token(mut self, t: Token) -> Self {
        self.identifier_token = Some(t);
        self
    }
    pub fn with_semi_colon_token(mut self, t: Token) -> Self {
        self.semi_colon_token = t;
        self
    }
    pub fn build(self) -> PackageBodyEpilogueSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::PackageBodyEpilogue);
        builder.push(self.end_token);
        if let Some(t) = self.package_token {
            builder.push(t);
        }
        if let Some(t) = self.body_token {
            builder.push(t);
        }
        if let Some(t) = self.identifier_token {
            builder.push(t);
        }
        builder.push(self.semi_colon_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        PackageBodyEpilogueSyntax::cast(node).unwrap()
    }
}
pub struct PackageBodyPreambleBuilder {
    package_token: Token,
    body_token: Token,
    name_token: Token,
    is_token: Token,
}
impl PackageBodyPreambleBuilder {
    pub fn new(name_token: Token) -> Self {
        Self {
            package_token: Token::new(
                TokenKind::Keyword(Kw::Package),
                Kw::Package.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            body_token: Token::new(
                TokenKind::Keyword(Kw::Body),
                Kw::Body.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            name_token,
            is_token: Token::new(
                TokenKind::Keyword(Kw::Is),
                Kw::Is.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_package_token(mut self, t: Token) -> Self {
        self.package_token = t;
        self
    }
    pub fn with_body_token(mut self, t: Token) -> Self {
        self.body_token = t;
        self
    }
    pub fn with_name_token(mut self, t: Token) -> Self {
        self.name_token = t;
        self
    }
    pub fn with_is_token(mut self, t: Token) -> Self {
        self.is_token = t;
        self
    }
    pub fn build(self) -> PackageBodyPreambleSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::PackageBodyPreamble);
        builder.push(self.package_token);
        builder.push(self.body_token);
        builder.push(self.name_token);
        builder.push(self.is_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        PackageBodyPreambleSyntax::cast(node).unwrap()
    }
}
pub struct PackageDeclarationBuilder {
    package: PackageSyntax,
}
impl PackageDeclarationBuilder {
    pub fn new(package: PackageSyntax) -> Self {
        Self { package }
    }
    pub fn with_package(mut self, n: PackageSyntax) -> Self {
        self.package = n;
        self
    }
    pub fn build(self) -> PackageDeclarationSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::PackageDeclaration);
        builder.push_node(self.package.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        PackageDeclarationSyntax::cast(node).unwrap()
    }
}
pub struct PackageEpilogueBuilder {
    end_token: Token,
    package_token: Option<Token>,
    identifier_token: Option<Token>,
    semi_colon_token: Token,
}
impl Default for PackageEpilogueBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl PackageEpilogueBuilder {
    pub fn new() -> Self {
        Self {
            end_token: Token::new(
                TokenKind::Keyword(Kw::End),
                Kw::End.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            package_token: None,
            identifier_token: None,
            semi_colon_token: Token::new(
                TokenKind::SemiColon,
                TokenKind::SemiColon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_end_token(mut self, t: Token) -> Self {
        self.end_token = t;
        self
    }
    pub fn with_package_token(mut self, t: Token) -> Self {
        self.package_token = Some(t);
        self
    }
    pub fn with_identifier_token(mut self, t: Token) -> Self {
        self.identifier_token = Some(t);
        self
    }
    pub fn with_semi_colon_token(mut self, t: Token) -> Self {
        self.semi_colon_token = t;
        self
    }
    pub fn build(self) -> PackageEpilogueSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::PackageEpilogue);
        builder.push(self.end_token);
        if let Some(t) = self.package_token {
            builder.push(t);
        }
        if let Some(t) = self.identifier_token {
            builder.push(t);
        }
        builder.push(self.semi_colon_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        PackageEpilogueSyntax::cast(node).unwrap()
    }
}
pub struct PackageHeaderBuilder {
    generic_clause: Option<GenericClauseSyntax>,
    generic_map_aspect: Option<GenericMapAspectSyntax>,
    semi_colon_token: Option<Token>,
}
impl Default for PackageHeaderBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl PackageHeaderBuilder {
    pub fn new() -> Self {
        Self {
            generic_clause: None,
            generic_map_aspect: None,
            semi_colon_token: None,
        }
    }
    pub fn with_generic_clause(mut self, n: GenericClauseSyntax) -> Self {
        self.generic_clause = Some(n);
        self
    }
    pub fn with_generic_map_aspect(mut self, n: GenericMapAspectSyntax) -> Self {
        self.generic_map_aspect = Some(n);
        self
    }
    pub fn with_semi_colon_token(mut self, t: Token) -> Self {
        self.semi_colon_token = Some(t);
        self
    }
    pub fn build(self) -> PackageHeaderSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::PackageHeader);
        if let Some(n) = self.generic_clause {
            builder.push_node(n.raw().green().clone());
        }
        if let Some(n) = self.generic_map_aspect {
            builder.push_node(n.raw().green().clone());
        }
        if let Some(t) = self.semi_colon_token {
            builder.push(t);
        }
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        PackageHeaderSyntax::cast(node).unwrap()
    }
}
pub struct PackageInstantiationBuilder {
    package_instantiation_preamble: PackageInstantiationPreambleSyntax,
    generic_map_aspect: Option<GenericMapAspectSyntax>,
    semi_colon_token: Token,
}
impl PackageInstantiationBuilder {
    pub fn new(package_instantiation_preamble: PackageInstantiationPreambleSyntax) -> Self {
        Self {
            package_instantiation_preamble,
            generic_map_aspect: None,
            semi_colon_token: Token::new(
                TokenKind::SemiColon,
                TokenKind::SemiColon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_package_instantiation_preamble(
        mut self,
        n: PackageInstantiationPreambleSyntax,
    ) -> Self {
        self.package_instantiation_preamble = n;
        self
    }
    pub fn with_generic_map_aspect(mut self, n: GenericMapAspectSyntax) -> Self {
        self.generic_map_aspect = Some(n);
        self
    }
    pub fn with_semi_colon_token(mut self, t: Token) -> Self {
        self.semi_colon_token = t;
        self
    }
    pub fn build(self) -> PackageInstantiationSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::PackageInstantiation);
        builder.push_node(self.package_instantiation_preamble.raw().green().clone());
        if let Some(n) = self.generic_map_aspect {
            builder.push_node(n.raw().green().clone());
        }
        builder.push(self.semi_colon_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        PackageInstantiationSyntax::cast(node).unwrap()
    }
}
pub struct PackageInstantiationDeclarationBuilder {
    package_instantiation: PackageInstantiationSyntax,
}
impl PackageInstantiationDeclarationBuilder {
    pub fn new(package_instantiation: PackageInstantiationSyntax) -> Self {
        Self {
            package_instantiation,
        }
    }
    pub fn with_package_instantiation(mut self, n: PackageInstantiationSyntax) -> Self {
        self.package_instantiation = n;
        self
    }
    pub fn build(self) -> PackageInstantiationDeclarationSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::PackageInstantiationDeclaration);
        builder.push_node(self.package_instantiation.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        PackageInstantiationDeclarationSyntax::cast(node).unwrap()
    }
}
pub struct PackageInstantiationDeclarationPrimaryUnitBuilder {
    package_instantiation: PackageInstantiationSyntax,
}
impl PackageInstantiationDeclarationPrimaryUnitBuilder {
    pub fn new(package_instantiation: PackageInstantiationSyntax) -> Self {
        Self {
            package_instantiation,
        }
    }
    pub fn with_package_instantiation(mut self, n: PackageInstantiationSyntax) -> Self {
        self.package_instantiation = n;
        self
    }
    pub fn build(self) -> PackageInstantiationDeclarationPrimaryUnitSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::PackageInstantiationDeclarationPrimaryUnit);
        builder.push_node(self.package_instantiation.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        PackageInstantiationDeclarationPrimaryUnitSyntax::cast(node).unwrap()
    }
}
pub struct PackageInstantiationPreambleBuilder {
    package_token: Token,
    name_token: Token,
    is_token: Token,
    new_token: Token,
    name: NameSyntax,
}
impl PackageInstantiationPreambleBuilder {
    pub fn new(name_token: Token, name: NameSyntax) -> Self {
        Self {
            package_token: Token::new(
                TokenKind::Keyword(Kw::Package),
                Kw::Package.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            name_token,
            is_token: Token::new(
                TokenKind::Keyword(Kw::Is),
                Kw::Is.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            new_token: Token::new(
                TokenKind::Keyword(Kw::New),
                Kw::New.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            name,
        }
    }
    pub fn with_package_token(mut self, t: Token) -> Self {
        self.package_token = t;
        self
    }
    pub fn with_name_token(mut self, t: Token) -> Self {
        self.name_token = t;
        self
    }
    pub fn with_is_token(mut self, t: Token) -> Self {
        self.is_token = t;
        self
    }
    pub fn with_new_token(mut self, t: Token) -> Self {
        self.new_token = t;
        self
    }
    pub fn with_name(mut self, n: NameSyntax) -> Self {
        self.name = n;
        self
    }
    pub fn build(self) -> PackageInstantiationPreambleSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::PackageInstantiationPreamble);
        builder.push(self.package_token);
        builder.push(self.name_token);
        builder.push(self.is_token);
        builder.push(self.new_token);
        builder.push_node(self.name.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        PackageInstantiationPreambleSyntax::cast(node).unwrap()
    }
}
pub struct PackagePathnameBuilder {
    comm_at_token: Token,
    dot_token: Vec<Token>,
    simple_name_token: Vec<Token>,
}
impl Default for PackagePathnameBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl PackagePathnameBuilder {
    pub fn new() -> Self {
        Self {
            comm_at_token: Token::new(
                TokenKind::CommAt,
                TokenKind::CommAt.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            dot_token: Vec::new(),
            simple_name_token: Vec::new(),
        }
    }
    pub fn with_comm_at_token(mut self, t: Token) -> Self {
        self.comm_at_token = t;
        self
    }
    pub fn add_dot_token(mut self, t: Token) -> Self {
        self.dot_token.push(t);
        self
    }
    pub fn add_simple_name_token(mut self, t: Token) -> Self {
        self.simple_name_token.push(t);
        self
    }
    pub fn build(self) -> PackagePathnameSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::PackagePathname);
        builder.push(self.comm_at_token);
        for t in self.dot_token {
            builder.push(t);
        }
        for t in self.simple_name_token {
            builder.push(t);
        }
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        PackagePathnameSyntax::cast(node).unwrap()
    }
}
pub struct PackagePreambleBuilder {
    package_token: Token,
    name_token: Token,
    is_token: Token,
}
impl PackagePreambleBuilder {
    pub fn new(name_token: Token) -> Self {
        Self {
            package_token: Token::new(
                TokenKind::Keyword(Kw::Package),
                Kw::Package.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            name_token,
            is_token: Token::new(
                TokenKind::Keyword(Kw::Is),
                Kw::Is.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_package_token(mut self, t: Token) -> Self {
        self.package_token = t;
        self
    }
    pub fn with_name_token(mut self, t: Token) -> Self {
        self.name_token = t;
        self
    }
    pub fn with_is_token(mut self, t: Token) -> Self {
        self.is_token = t;
        self
    }
    pub fn build(self) -> PackagePreambleSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::PackagePreamble);
        builder.push(self.package_token);
        builder.push(self.name_token);
        builder.push(self.is_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        PackagePreambleSyntax::cast(node).unwrap()
    }
}
pub struct ParameterListBuilder {
    parameter_token: Option<Token>,
    parenthesized_interface_list: ParenthesizedInterfaceListSyntax,
}
impl Default for ParameterListBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl ParameterListBuilder {
    pub fn new() -> Self {
        Self {
            parameter_token: None,
            parenthesized_interface_list: ParenthesizedInterfaceListBuilder::default().build(),
        }
    }
    pub fn with_parameter_token(mut self, t: Token) -> Self {
        self.parameter_token = Some(t);
        self
    }
    pub fn with_parenthesized_interface_list(
        mut self,
        n: ParenthesizedInterfaceListSyntax,
    ) -> Self {
        self.parenthesized_interface_list = n;
        self
    }
    pub fn build(self) -> ParameterListSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ParameterList);
        if let Some(t) = self.parameter_token {
            builder.push(t);
        }
        builder.push_node(self.parenthesized_interface_list.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ParameterListSyntax::cast(node).unwrap()
    }
}
pub struct ParameterSpecificationBuilder {
    identifier_token: Token,
    in_token: Token,
    discrete_range: DiscreteRangeSyntax,
}
impl ParameterSpecificationBuilder {
    pub fn new(identifier_token: Token, discrete_range: DiscreteRangeSyntax) -> Self {
        Self {
            identifier_token,
            in_token: Token::new(
                TokenKind::Keyword(Kw::In),
                Kw::In.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            discrete_range,
        }
    }
    pub fn with_identifier_token(mut self, t: Token) -> Self {
        self.identifier_token = t;
        self
    }
    pub fn with_in_token(mut self, t: Token) -> Self {
        self.in_token = t;
        self
    }
    pub fn with_discrete_range(mut self, n: DiscreteRangeSyntax) -> Self {
        self.discrete_range = n;
        self
    }
    pub fn build(self) -> ParameterSpecificationSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ParameterSpecification);
        builder.push(self.identifier_token);
        builder.push(self.in_token);
        builder.push_node(self.discrete_range.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ParameterSpecificationSyntax::cast(node).unwrap()
    }
}
pub struct ParenthesizedElementResolutionResolutionIndicationBuilder {
    left_par_token: Token,
    element_resolution_resolution_indication: ElementResolutionResolutionIndicationSyntax,
    right_par_token: Token,
}
impl ParenthesizedElementResolutionResolutionIndicationBuilder {
    pub fn new(
        element_resolution_resolution_indication: ElementResolutionResolutionIndicationSyntax,
    ) -> Self {
        Self {
            left_par_token: Token::new(
                TokenKind::LeftPar,
                TokenKind::LeftPar.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            element_resolution_resolution_indication,
            right_par_token: Token::new(
                TokenKind::RightPar,
                TokenKind::RightPar.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_left_par_token(mut self, t: Token) -> Self {
        self.left_par_token = t;
        self
    }
    pub fn with_element_resolution_resolution_indication(
        mut self,
        n: ElementResolutionResolutionIndicationSyntax,
    ) -> Self {
        self.element_resolution_resolution_indication = n;
        self
    }
    pub fn with_right_par_token(mut self, t: Token) -> Self {
        self.right_par_token = t;
        self
    }
    pub fn build(self) -> ParenthesizedElementResolutionResolutionIndicationSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ParenthesizedElementResolutionResolutionIndication);
        builder.push(self.left_par_token);
        builder.push_node(
            self.element_resolution_resolution_indication
                .raw()
                .green()
                .clone(),
        );
        builder.push(self.right_par_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ParenthesizedElementResolutionResolutionIndicationSyntax::cast(node).unwrap()
    }
}
pub struct ParenthesizedExpressionBuilder {
    left_par_token: Token,
    expression: ExpressionSyntax,
    right_par_token: Token,
}
impl ParenthesizedExpressionBuilder {
    pub fn new(expression: ExpressionSyntax) -> Self {
        Self {
            left_par_token: Token::new(
                TokenKind::LeftPar,
                TokenKind::LeftPar.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            expression,
            right_par_token: Token::new(
                TokenKind::RightPar,
                TokenKind::RightPar.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_left_par_token(mut self, t: Token) -> Self {
        self.left_par_token = t;
        self
    }
    pub fn with_expression(mut self, n: ExpressionSyntax) -> Self {
        self.expression = n;
        self
    }
    pub fn with_right_par_token(mut self, t: Token) -> Self {
        self.right_par_token = t;
        self
    }
    pub fn build(self) -> ParenthesizedExpressionSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ParenthesizedExpression);
        builder.push(self.left_par_token);
        builder.push_node(self.expression.raw().green().clone());
        builder.push(self.right_par_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ParenthesizedExpressionSyntax::cast(node).unwrap()
    }
}
pub struct ParenthesizedExpressionOrAggregateBuilder {
    left_par_token: Token,
    element_associations: Vec<ElementAssociationSyntax>,
    comma_token: Vec<Token>,
    right_par_token: Token,
}
impl Default for ParenthesizedExpressionOrAggregateBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl ParenthesizedExpressionOrAggregateBuilder {
    pub fn new() -> Self {
        Self {
            left_par_token: Token::new(
                TokenKind::LeftPar,
                TokenKind::LeftPar.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            element_associations: Vec::new(),
            comma_token: Vec::new(),
            right_par_token: Token::new(
                TokenKind::RightPar,
                TokenKind::RightPar.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_left_par_token(mut self, t: Token) -> Self {
        self.left_par_token = t;
        self
    }
    pub fn add_element_associations(mut self, n: ElementAssociationSyntax) -> Self {
        self.element_associations.push(n);
        self
    }
    pub fn add_comma_token(mut self, t: Token) -> Self {
        self.comma_token.push(t);
        self
    }
    pub fn with_right_par_token(mut self, t: Token) -> Self {
        self.right_par_token = t;
        self
    }
    pub fn build(self) -> ParenthesizedExpressionOrAggregateSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ParenthesizedExpressionOrAggregate);
        builder.push(self.left_par_token);
        for n in self.element_associations {
            builder.push_node(n.raw().green().clone());
        }
        for t in self.comma_token {
            builder.push(t);
        }
        builder.push(self.right_par_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ParenthesizedExpressionOrAggregateSyntax::cast(node).unwrap()
    }
}
pub struct ParenthesizedInterfaceListBuilder {
    left_par_token: Token,
    interface_list: InterfaceListSyntax,
    right_par_token: Token,
}
impl Default for ParenthesizedInterfaceListBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl ParenthesizedInterfaceListBuilder {
    pub fn new() -> Self {
        Self {
            left_par_token: Token::new(
                TokenKind::LeftPar,
                TokenKind::LeftPar.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            interface_list: InterfaceListBuilder::default().build(),
            right_par_token: Token::new(
                TokenKind::RightPar,
                TokenKind::RightPar.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_left_par_token(mut self, t: Token) -> Self {
        self.left_par_token = t;
        self
    }
    pub fn with_interface_list(mut self, n: InterfaceListSyntax) -> Self {
        self.interface_list = n;
        self
    }
    pub fn with_right_par_token(mut self, t: Token) -> Self {
        self.right_par_token = t;
        self
    }
    pub fn build(self) -> ParenthesizedInterfaceListSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ParenthesizedInterfaceList);
        builder.push(self.left_par_token);
        builder.push_node(self.interface_list.raw().green().clone());
        builder.push(self.right_par_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ParenthesizedInterfaceListSyntax::cast(node).unwrap()
    }
}
pub struct ParenthesizedNameBuilder {
    left_par_token: Token,
    name: NameSyntax,
    right_par_token: Token,
}
impl ParenthesizedNameBuilder {
    pub fn new(name: NameSyntax) -> Self {
        Self {
            left_par_token: Token::new(
                TokenKind::LeftPar,
                TokenKind::LeftPar.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            name,
            right_par_token: Token::new(
                TokenKind::RightPar,
                TokenKind::RightPar.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_left_par_token(mut self, t: Token) -> Self {
        self.left_par_token = t;
        self
    }
    pub fn with_name(mut self, n: NameSyntax) -> Self {
        self.name = n;
        self
    }
    pub fn with_right_par_token(mut self, t: Token) -> Self {
        self.right_par_token = t;
        self
    }
    pub fn build(self) -> ParenthesizedNameSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ParenthesizedName);
        builder.push(self.left_par_token);
        builder.push_node(self.name.raw().green().clone());
        builder.push(self.right_par_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ParenthesizedNameSyntax::cast(node).unwrap()
    }
}
pub struct ParenthesizedProcessSensitivityListBuilder {
    left_par_token: Token,
    process_sensitivity_list: ProcessSensitivityListSyntax,
    right_par_token: Token,
}
impl ParenthesizedProcessSensitivityListBuilder {
    pub fn new(process_sensitivity_list: ProcessSensitivityListSyntax) -> Self {
        Self {
            left_par_token: Token::new(
                TokenKind::LeftPar,
                TokenKind::LeftPar.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            process_sensitivity_list,
            right_par_token: Token::new(
                TokenKind::RightPar,
                TokenKind::RightPar.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_left_par_token(mut self, t: Token) -> Self {
        self.left_par_token = t;
        self
    }
    pub fn with_process_sensitivity_list(mut self, n: ProcessSensitivityListSyntax) -> Self {
        self.process_sensitivity_list = n;
        self
    }
    pub fn with_right_par_token(mut self, t: Token) -> Self {
        self.right_par_token = t;
        self
    }
    pub fn build(self) -> ParenthesizedProcessSensitivityListSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ParenthesizedProcessSensitivityList);
        builder.push(self.left_par_token);
        builder.push_node(self.process_sensitivity_list.raw().green().clone());
        builder.push(self.right_par_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ParenthesizedProcessSensitivityListSyntax::cast(node).unwrap()
    }
}
pub struct PartialPathnameBuilder {
    identifier_token: Vec<Token>,
    dot_token: Vec<Token>,
}
impl Default for PartialPathnameBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl PartialPathnameBuilder {
    pub fn new() -> Self {
        Self {
            identifier_token: Vec::new(),
            dot_token: Vec::new(),
        }
    }
    pub fn add_identifier_token(mut self, t: Token) -> Self {
        self.identifier_token.push(t);
        self
    }
    pub fn add_dot_token(mut self, t: Token) -> Self {
        self.dot_token.push(t);
        self
    }
    pub fn build(self) -> PartialPathnameSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::PartialPathname);
        for t in self.identifier_token {
            builder.push(t);
        }
        for t in self.dot_token {
            builder.push(t);
        }
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        PartialPathnameSyntax::cast(node).unwrap()
    }
}
pub struct PhysicalLiteralBuilder {
    abstract_literal_token: Token,
    name: NameSyntax,
}
impl PhysicalLiteralBuilder {
    pub fn new(abstract_literal_token: Token, name: NameSyntax) -> Self {
        Self {
            abstract_literal_token,
            name,
        }
    }
    pub fn with_abstract_literal_token(mut self, t: Token) -> Self {
        self.abstract_literal_token = t;
        self
    }
    pub fn with_name(mut self, n: NameSyntax) -> Self {
        self.name = n;
        self
    }
    pub fn build(self) -> PhysicalLiteralSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::PhysicalLiteral);
        builder.push(self.abstract_literal_token);
        builder.push_node(self.name.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        PhysicalLiteralSyntax::cast(node).unwrap()
    }
}
pub struct PhysicalLiteralExpressionBuilder {
    physical_literal: PhysicalLiteralSyntax,
}
impl PhysicalLiteralExpressionBuilder {
    pub fn new(physical_literal: PhysicalLiteralSyntax) -> Self {
        Self { physical_literal }
    }
    pub fn with_physical_literal(mut self, n: PhysicalLiteralSyntax) -> Self {
        self.physical_literal = n;
        self
    }
    pub fn build(self) -> PhysicalLiteralExpressionSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::PhysicalLiteralExpression);
        builder.push_node(self.physical_literal.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        PhysicalLiteralExpressionSyntax::cast(node).unwrap()
    }
}
pub struct PhysicalTypeDefinitionBuilder {
    range_constraint: RangeConstraintSyntax,
    unit_declarations: UnitDeclarationsSyntax,
    physical_type_definition_epilogue: PhysicalTypeDefinitionEpilogueSyntax,
}
impl PhysicalTypeDefinitionBuilder {
    pub fn new(
        range_constraint: RangeConstraintSyntax,
        unit_declarations: UnitDeclarationsSyntax,
        physical_type_definition_epilogue: PhysicalTypeDefinitionEpilogueSyntax,
    ) -> Self {
        Self {
            range_constraint,
            unit_declarations,
            physical_type_definition_epilogue,
        }
    }
    pub fn with_range_constraint(mut self, n: RangeConstraintSyntax) -> Self {
        self.range_constraint = n;
        self
    }
    pub fn with_unit_declarations(mut self, n: UnitDeclarationsSyntax) -> Self {
        self.unit_declarations = n;
        self
    }
    pub fn with_physical_type_definition_epilogue(
        mut self,
        n: PhysicalTypeDefinitionEpilogueSyntax,
    ) -> Self {
        self.physical_type_definition_epilogue = n;
        self
    }
    pub fn build(self) -> PhysicalTypeDefinitionSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::PhysicalTypeDefinition);
        builder.push_node(self.range_constraint.raw().green().clone());
        builder.push_node(self.unit_declarations.raw().green().clone());
        builder.push_node(self.physical_type_definition_epilogue.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        PhysicalTypeDefinitionSyntax::cast(node).unwrap()
    }
}
pub struct PhysicalTypeDefinitionEpilogueBuilder {
    end_token: Token,
    units_token: Token,
    name_token: Token,
}
impl PhysicalTypeDefinitionEpilogueBuilder {
    pub fn new(name_token: Token) -> Self {
        Self {
            end_token: Token::new(
                TokenKind::Keyword(Kw::End),
                Kw::End.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            units_token: Token::new(
                TokenKind::Keyword(Kw::Units),
                Kw::Units.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            name_token,
        }
    }
    pub fn with_end_token(mut self, t: Token) -> Self {
        self.end_token = t;
        self
    }
    pub fn with_units_token(mut self, t: Token) -> Self {
        self.units_token = t;
        self
    }
    pub fn with_name_token(mut self, t: Token) -> Self {
        self.name_token = t;
        self
    }
    pub fn build(self) -> PhysicalTypeDefinitionEpilogueSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::PhysicalTypeDefinitionEpilogue);
        builder.push(self.end_token);
        builder.push(self.units_token);
        builder.push(self.name_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        PhysicalTypeDefinitionEpilogueSyntax::cast(node).unwrap()
    }
}
pub struct PortClauseBuilder {
    port_clause_preamble: PortClausePreambleSyntax,
    interface_list: InterfaceListSyntax,
    port_clause_epilogue: PortClauseEpilogueSyntax,
}
impl Default for PortClauseBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl PortClauseBuilder {
    pub fn new() -> Self {
        Self {
            port_clause_preamble: PortClausePreambleBuilder::default().build(),
            interface_list: InterfaceListBuilder::default().build(),
            port_clause_epilogue: PortClauseEpilogueBuilder::default().build(),
        }
    }
    pub fn with_port_clause_preamble(mut self, n: PortClausePreambleSyntax) -> Self {
        self.port_clause_preamble = n;
        self
    }
    pub fn with_interface_list(mut self, n: InterfaceListSyntax) -> Self {
        self.interface_list = n;
        self
    }
    pub fn with_port_clause_epilogue(mut self, n: PortClauseEpilogueSyntax) -> Self {
        self.port_clause_epilogue = n;
        self
    }
    pub fn build(self) -> PortClauseSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::PortClause);
        builder.push_node(self.port_clause_preamble.raw().green().clone());
        builder.push_node(self.interface_list.raw().green().clone());
        builder.push_node(self.port_clause_epilogue.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        PortClauseSyntax::cast(node).unwrap()
    }
}
pub struct PortClauseEpilogueBuilder {
    right_par_token: Token,
    semi_colon_token: Token,
}
impl Default for PortClauseEpilogueBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl PortClauseEpilogueBuilder {
    pub fn new() -> Self {
        Self {
            right_par_token: Token::new(
                TokenKind::RightPar,
                TokenKind::RightPar.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            semi_colon_token: Token::new(
                TokenKind::SemiColon,
                TokenKind::SemiColon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_right_par_token(mut self, t: Token) -> Self {
        self.right_par_token = t;
        self
    }
    pub fn with_semi_colon_token(mut self, t: Token) -> Self {
        self.semi_colon_token = t;
        self
    }
    pub fn build(self) -> PortClauseEpilogueSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::PortClauseEpilogue);
        builder.push(self.right_par_token);
        builder.push(self.semi_colon_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        PortClauseEpilogueSyntax::cast(node).unwrap()
    }
}
pub struct PortClausePreambleBuilder {
    port_token: Token,
    left_par_token: Token,
}
impl Default for PortClausePreambleBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl PortClausePreambleBuilder {
    pub fn new() -> Self {
        Self {
            port_token: Token::new(
                TokenKind::Keyword(Kw::Port),
                Kw::Port.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            left_par_token: Token::new(
                TokenKind::LeftPar,
                TokenKind::LeftPar.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_port_token(mut self, t: Token) -> Self {
        self.port_token = t;
        self
    }
    pub fn with_left_par_token(mut self, t: Token) -> Self {
        self.left_par_token = t;
        self
    }
    pub fn build(self) -> PortClausePreambleSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::PortClausePreamble);
        builder.push(self.port_token);
        builder.push(self.left_par_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        PortClausePreambleSyntax::cast(node).unwrap()
    }
}
pub struct PortMapAspectBuilder {
    port_token: Token,
    map_token: Token,
    left_par_token: Token,
    association_list: AssociationListSyntax,
    right_par_token: Token,
}
impl Default for PortMapAspectBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl PortMapAspectBuilder {
    pub fn new() -> Self {
        Self {
            port_token: Token::new(
                TokenKind::Keyword(Kw::Port),
                Kw::Port.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            map_token: Token::new(
                TokenKind::Keyword(Kw::Map),
                Kw::Map.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            left_par_token: Token::new(
                TokenKind::LeftPar,
                TokenKind::LeftPar.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            association_list: AssociationListBuilder::default().build(),
            right_par_token: Token::new(
                TokenKind::RightPar,
                TokenKind::RightPar.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_port_token(mut self, t: Token) -> Self {
        self.port_token = t;
        self
    }
    pub fn with_map_token(mut self, t: Token) -> Self {
        self.map_token = t;
        self
    }
    pub fn with_left_par_token(mut self, t: Token) -> Self {
        self.left_par_token = t;
        self
    }
    pub fn with_association_list(mut self, n: AssociationListSyntax) -> Self {
        self.association_list = n;
        self
    }
    pub fn with_right_par_token(mut self, t: Token) -> Self {
        self.right_par_token = t;
        self
    }
    pub fn build(self) -> PortMapAspectSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::PortMapAspect);
        builder.push(self.port_token);
        builder.push(self.map_token);
        builder.push(self.left_par_token);
        builder.push_node(self.association_list.raw().green().clone());
        builder.push(self.right_par_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        PortMapAspectSyntax::cast(node).unwrap()
    }
}
pub struct PrimaryUnitDeclarationBuilder {
    identifier_token: Token,
    semi_colon_token: Token,
}
impl PrimaryUnitDeclarationBuilder {
    pub fn new(identifier_token: Token) -> Self {
        Self {
            identifier_token,
            semi_colon_token: Token::new(
                TokenKind::SemiColon,
                TokenKind::SemiColon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_identifier_token(mut self, t: Token) -> Self {
        self.identifier_token = t;
        self
    }
    pub fn with_semi_colon_token(mut self, t: Token) -> Self {
        self.semi_colon_token = t;
        self
    }
    pub fn build(self) -> PrimaryUnitDeclarationSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::PrimaryUnitDeclaration);
        builder.push(self.identifier_token);
        builder.push(self.semi_colon_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        PrimaryUnitDeclarationSyntax::cast(node).unwrap()
    }
}
pub struct PrimaryUnitPackageDeclarationBuilder {
    package: PackageSyntax,
}
impl PrimaryUnitPackageDeclarationBuilder {
    pub fn new(package: PackageSyntax) -> Self {
        Self { package }
    }
    pub fn with_package(mut self, n: PackageSyntax) -> Self {
        self.package = n;
        self
    }
    pub fn build(self) -> PrimaryUnitPackageDeclarationSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::PrimaryUnitPackageDeclaration);
        builder.push_node(self.package.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        PrimaryUnitPackageDeclarationSyntax::cast(node).unwrap()
    }
}
pub struct ProcedureCallStatementBuilder {
    label: LabelSyntax,
    name: NameSyntax,
    semi_colon_token: Token,
}
impl ProcedureCallStatementBuilder {
    pub fn new(label: LabelSyntax, name: NameSyntax) -> Self {
        Self {
            label,
            name,
            semi_colon_token: Token::new(
                TokenKind::SemiColon,
                TokenKind::SemiColon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_label(mut self, n: LabelSyntax) -> Self {
        self.label = n;
        self
    }
    pub fn with_name(mut self, n: NameSyntax) -> Self {
        self.name = n;
        self
    }
    pub fn with_semi_colon_token(mut self, t: Token) -> Self {
        self.semi_colon_token = t;
        self
    }
    pub fn build(self) -> ProcedureCallStatementSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ProcedureCallStatement);
        builder.push_node(self.label.raw().green().clone());
        builder.push_node(self.name.raw().green().clone());
        builder.push(self.semi_colon_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ProcedureCallStatementSyntax::cast(node).unwrap()
    }
}
pub struct ProcedureSpecificationBuilder {
    procedure_token: Token,
    designator: DesignatorSyntax,
    subprogram_header: SubprogramHeaderSyntax,
    parameter_list: Option<ParameterListSyntax>,
}
impl ProcedureSpecificationBuilder {
    pub fn new(designator: DesignatorSyntax) -> Self {
        Self {
            procedure_token: Token::new(
                TokenKind::Keyword(Kw::Procedure),
                Kw::Procedure.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            designator,
            subprogram_header: SubprogramHeaderBuilder::default().build(),
            parameter_list: None,
        }
    }
    pub fn with_procedure_token(mut self, t: Token) -> Self {
        self.procedure_token = t;
        self
    }
    pub fn with_designator(mut self, n: DesignatorSyntax) -> Self {
        self.designator = n;
        self
    }
    pub fn with_subprogram_header(mut self, n: SubprogramHeaderSyntax) -> Self {
        self.subprogram_header = n;
        self
    }
    pub fn with_parameter_list(mut self, n: ParameterListSyntax) -> Self {
        self.parameter_list = Some(n);
        self
    }
    pub fn build(self) -> ProcedureSpecificationSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ProcedureSpecification);
        builder.push(self.procedure_token);
        builder.push(self.designator.raw().token().clone());
        builder.push_node(self.subprogram_header.raw().green().clone());
        if let Some(n) = self.parameter_list {
            builder.push_node(n.raw().green().clone());
        }
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ProcedureSpecificationSyntax::cast(node).unwrap()
    }
}
pub struct ProcessStatementBuilder {
    process_statement_preamble: ProcessStatementPreambleSyntax,
    declarations: DeclarationsSyntax,
    declaration_statement_separator: DeclarationStatementSeparatorSyntax,
    concurrent_statements: ConcurrentStatementsSyntax,
    process_statement_epilogue: ProcessStatementEpilogueSyntax,
}
impl ProcessStatementBuilder {
    pub fn new(process_statement_preamble: ProcessStatementPreambleSyntax) -> Self {
        Self {
            process_statement_preamble,
            declarations: DeclarationsBuilder::default().build(),
            declaration_statement_separator: DeclarationStatementSeparatorBuilder::default()
                .build(),
            concurrent_statements: ConcurrentStatementsBuilder::default().build(),
            process_statement_epilogue: ProcessStatementEpilogueBuilder::default().build(),
        }
    }
    pub fn with_process_statement_preamble(mut self, n: ProcessStatementPreambleSyntax) -> Self {
        self.process_statement_preamble = n;
        self
    }
    pub fn with_declarations(mut self, n: DeclarationsSyntax) -> Self {
        self.declarations = n;
        self
    }
    pub fn with_declaration_statement_separator(
        mut self,
        n: DeclarationStatementSeparatorSyntax,
    ) -> Self {
        self.declaration_statement_separator = n;
        self
    }
    pub fn with_concurrent_statements(mut self, n: ConcurrentStatementsSyntax) -> Self {
        self.concurrent_statements = n;
        self
    }
    pub fn with_process_statement_epilogue(mut self, n: ProcessStatementEpilogueSyntax) -> Self {
        self.process_statement_epilogue = n;
        self
    }
    pub fn build(self) -> ProcessStatementSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ProcessStatement);
        builder.push_node(self.process_statement_preamble.raw().green().clone());
        builder.push_node(self.declarations.raw().green().clone());
        builder.push_node(self.declaration_statement_separator.raw().green().clone());
        builder.push_node(self.concurrent_statements.raw().green().clone());
        builder.push_node(self.process_statement_epilogue.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ProcessStatementSyntax::cast(node).unwrap()
    }
}
pub struct ProcessStatementEpilogueBuilder {
    end_token: Token,
    postponed_token: Option<Token>,
    process_token: Token,
    identifier_token: Option<Token>,
    semi_colon_token: Token,
}
impl Default for ProcessStatementEpilogueBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl ProcessStatementEpilogueBuilder {
    pub fn new() -> Self {
        Self {
            end_token: Token::new(
                TokenKind::Keyword(Kw::End),
                Kw::End.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            postponed_token: None,
            process_token: Token::new(
                TokenKind::Keyword(Kw::Process),
                Kw::Process.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            identifier_token: None,
            semi_colon_token: Token::new(
                TokenKind::SemiColon,
                TokenKind::SemiColon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_end_token(mut self, t: Token) -> Self {
        self.end_token = t;
        self
    }
    pub fn with_postponed_token(mut self, t: Token) -> Self {
        self.postponed_token = Some(t);
        self
    }
    pub fn with_process_token(mut self, t: Token) -> Self {
        self.process_token = t;
        self
    }
    pub fn with_identifier_token(mut self, t: Token) -> Self {
        self.identifier_token = Some(t);
        self
    }
    pub fn with_semi_colon_token(mut self, t: Token) -> Self {
        self.semi_colon_token = t;
        self
    }
    pub fn build(self) -> ProcessStatementEpilogueSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ProcessStatementEpilogue);
        builder.push(self.end_token);
        if let Some(t) = self.postponed_token {
            builder.push(t);
        }
        builder.push(self.process_token);
        if let Some(t) = self.identifier_token {
            builder.push(t);
        }
        builder.push(self.semi_colon_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ProcessStatementEpilogueSyntax::cast(node).unwrap()
    }
}
pub struct ProcessStatementPreambleBuilder {
    label: LabelSyntax,
    postponed_token: Option<Token>,
    process_token: Token,
    parenthesized_process_sensitivity_list: Option<ParenthesizedProcessSensitivityListSyntax>,
    is_token: Option<Token>,
}
impl ProcessStatementPreambleBuilder {
    pub fn new(label: LabelSyntax) -> Self {
        Self {
            label,
            postponed_token: None,
            process_token: Token::new(
                TokenKind::Keyword(Kw::Process),
                Kw::Process.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            parenthesized_process_sensitivity_list: None,
            is_token: None,
        }
    }
    pub fn with_label(mut self, n: LabelSyntax) -> Self {
        self.label = n;
        self
    }
    pub fn with_postponed_token(mut self, t: Token) -> Self {
        self.postponed_token = Some(t);
        self
    }
    pub fn with_process_token(mut self, t: Token) -> Self {
        self.process_token = t;
        self
    }
    pub fn with_parenthesized_process_sensitivity_list(
        mut self,
        n: ParenthesizedProcessSensitivityListSyntax,
    ) -> Self {
        self.parenthesized_process_sensitivity_list = Some(n);
        self
    }
    pub fn with_is_token(mut self, t: Token) -> Self {
        self.is_token = Some(t);
        self
    }
    pub fn build(self) -> ProcessStatementPreambleSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ProcessStatementPreamble);
        builder.push_node(self.label.raw().green().clone());
        if let Some(t) = self.postponed_token {
            builder.push(t);
        }
        builder.push(self.process_token);
        if let Some(n) = self.parenthesized_process_sensitivity_list {
            builder.push_node(n.raw().green().clone());
        }
        if let Some(t) = self.is_token {
            builder.push(t);
        }
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ProcessStatementPreambleSyntax::cast(node).unwrap()
    }
}
pub struct ProtectedTypeBodyBuilder {
    protected_type_body_preamble: ProtectedTypeBodyPreambleSyntax,
    declarations: DeclarationsSyntax,
    protected_type_body_epilogue: ProtectedTypeBodyEpilogueSyntax,
}
impl ProtectedTypeBodyBuilder {
    pub fn new(protected_type_body_epilogue: ProtectedTypeBodyEpilogueSyntax) -> Self {
        Self {
            protected_type_body_preamble: ProtectedTypeBodyPreambleBuilder::default().build(),
            declarations: DeclarationsBuilder::default().build(),
            protected_type_body_epilogue,
        }
    }
    pub fn with_protected_type_body_preamble(mut self, n: ProtectedTypeBodyPreambleSyntax) -> Self {
        self.protected_type_body_preamble = n;
        self
    }
    pub fn with_declarations(mut self, n: DeclarationsSyntax) -> Self {
        self.declarations = n;
        self
    }
    pub fn with_protected_type_body_epilogue(mut self, n: ProtectedTypeBodyEpilogueSyntax) -> Self {
        self.protected_type_body_epilogue = n;
        self
    }
    pub fn build(self) -> ProtectedTypeBodySyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ProtectedTypeBody);
        builder.push_node(self.protected_type_body_preamble.raw().green().clone());
        builder.push_node(self.declarations.raw().green().clone());
        builder.push_node(self.protected_type_body_epilogue.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ProtectedTypeBodySyntax::cast(node).unwrap()
    }
}
pub struct ProtectedTypeBodyEpilogueBuilder {
    end_token: Token,
    protected_token: Token,
    body_token: Token,
    name_token: Token,
}
impl ProtectedTypeBodyEpilogueBuilder {
    pub fn new(name_token: Token) -> Self {
        Self {
            end_token: Token::new(
                TokenKind::Keyword(Kw::End),
                Kw::End.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            protected_token: Token::new(
                TokenKind::Keyword(Kw::Protected),
                Kw::Protected.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            body_token: Token::new(
                TokenKind::Keyword(Kw::Body),
                Kw::Body.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            name_token,
        }
    }
    pub fn with_end_token(mut self, t: Token) -> Self {
        self.end_token = t;
        self
    }
    pub fn with_protected_token(mut self, t: Token) -> Self {
        self.protected_token = t;
        self
    }
    pub fn with_body_token(mut self, t: Token) -> Self {
        self.body_token = t;
        self
    }
    pub fn with_name_token(mut self, t: Token) -> Self {
        self.name_token = t;
        self
    }
    pub fn build(self) -> ProtectedTypeBodyEpilogueSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ProtectedTypeBodyEpilogue);
        builder.push(self.end_token);
        builder.push(self.protected_token);
        builder.push(self.body_token);
        builder.push(self.name_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ProtectedTypeBodyEpilogueSyntax::cast(node).unwrap()
    }
}
pub struct ProtectedTypeBodyPreambleBuilder {
    protected_token: Token,
    body_token: Token,
}
impl Default for ProtectedTypeBodyPreambleBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl ProtectedTypeBodyPreambleBuilder {
    pub fn new() -> Self {
        Self {
            protected_token: Token::new(
                TokenKind::Keyword(Kw::Protected),
                Kw::Protected.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            body_token: Token::new(
                TokenKind::Keyword(Kw::Body),
                Kw::Body.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_protected_token(mut self, t: Token) -> Self {
        self.protected_token = t;
        self
    }
    pub fn with_body_token(mut self, t: Token) -> Self {
        self.body_token = t;
        self
    }
    pub fn build(self) -> ProtectedTypeBodyPreambleSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ProtectedTypeBodyPreamble);
        builder.push(self.protected_token);
        builder.push(self.body_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ProtectedTypeBodyPreambleSyntax::cast(node).unwrap()
    }
}
pub struct ProtectedTypeDeclarationBuilder {
    protected_type_declaration_preamble: ProtectedTypeDeclarationPreambleSyntax,
    declarations: DeclarationsSyntax,
    protected_type_declaration_epilogue: ProtectedTypeDeclarationEpilogueSyntax,
}
impl ProtectedTypeDeclarationBuilder {
    pub fn new(
        protected_type_declaration_epilogue: ProtectedTypeDeclarationEpilogueSyntax,
    ) -> Self {
        Self {
            protected_type_declaration_preamble: ProtectedTypeDeclarationPreambleBuilder::default()
                .build(),
            declarations: DeclarationsBuilder::default().build(),
            protected_type_declaration_epilogue,
        }
    }
    pub fn with_protected_type_declaration_preamble(
        mut self,
        n: ProtectedTypeDeclarationPreambleSyntax,
    ) -> Self {
        self.protected_type_declaration_preamble = n;
        self
    }
    pub fn with_declarations(mut self, n: DeclarationsSyntax) -> Self {
        self.declarations = n;
        self
    }
    pub fn with_protected_type_declaration_epilogue(
        mut self,
        n: ProtectedTypeDeclarationEpilogueSyntax,
    ) -> Self {
        self.protected_type_declaration_epilogue = n;
        self
    }
    pub fn build(self) -> ProtectedTypeDeclarationSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ProtectedTypeDeclaration);
        builder.push_node(
            self.protected_type_declaration_preamble
                .raw()
                .green()
                .clone(),
        );
        builder.push_node(self.declarations.raw().green().clone());
        builder.push_node(
            self.protected_type_declaration_epilogue
                .raw()
                .green()
                .clone(),
        );
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ProtectedTypeDeclarationSyntax::cast(node).unwrap()
    }
}
pub struct ProtectedTypeDeclarationEpilogueBuilder {
    end_token: Token,
    protected_token: Token,
    name_token: Token,
}
impl ProtectedTypeDeclarationEpilogueBuilder {
    pub fn new(name_token: Token) -> Self {
        Self {
            end_token: Token::new(
                TokenKind::Keyword(Kw::End),
                Kw::End.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            protected_token: Token::new(
                TokenKind::Keyword(Kw::Protected),
                Kw::Protected.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            name_token,
        }
    }
    pub fn with_end_token(mut self, t: Token) -> Self {
        self.end_token = t;
        self
    }
    pub fn with_protected_token(mut self, t: Token) -> Self {
        self.protected_token = t;
        self
    }
    pub fn with_name_token(mut self, t: Token) -> Self {
        self.name_token = t;
        self
    }
    pub fn build(self) -> ProtectedTypeDeclarationEpilogueSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ProtectedTypeDeclarationEpilogue);
        builder.push(self.end_token);
        builder.push(self.protected_token);
        builder.push(self.name_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ProtectedTypeDeclarationEpilogueSyntax::cast(node).unwrap()
    }
}
pub struct ProtectedTypeDeclarationPreambleBuilder {
    protected_token: Token,
}
impl Default for ProtectedTypeDeclarationPreambleBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl ProtectedTypeDeclarationPreambleBuilder {
    pub fn new() -> Self {
        Self {
            protected_token: Token::new(
                TokenKind::Keyword(Kw::Protected),
                Kw::Protected.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_protected_token(mut self, t: Token) -> Self {
        self.protected_token = t;
        self
    }
    pub fn build(self) -> ProtectedTypeDeclarationPreambleSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ProtectedTypeDeclarationPreamble);
        builder.push(self.protected_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ProtectedTypeDeclarationPreambleSyntax::cast(node).unwrap()
    }
}
pub struct PslClockDeclarationBuilder {}
impl Default for PslClockDeclarationBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl PslClockDeclarationBuilder {
    pub fn new() -> Self {
        Self {}
    }
    pub fn build(self) -> PslClockDeclarationSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::PslClockDeclaration);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        PslClockDeclarationSyntax::cast(node).unwrap()
    }
}
pub struct PslDirectiveBuilder {}
impl Default for PslDirectiveBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl PslDirectiveBuilder {
    pub fn new() -> Self {
        Self {}
    }
    pub fn build(self) -> PslDirectiveSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::PslDirective);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        PslDirectiveSyntax::cast(node).unwrap()
    }
}
pub struct PslPropertyDeclarationBuilder {}
impl Default for PslPropertyDeclarationBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl PslPropertyDeclarationBuilder {
    pub fn new() -> Self {
        Self {}
    }
    pub fn build(self) -> PslPropertyDeclarationSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::PslPropertyDeclaration);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        PslPropertyDeclarationSyntax::cast(node).unwrap()
    }
}
pub struct PslSequenceDeclarationBuilder {}
impl Default for PslSequenceDeclarationBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl PslSequenceDeclarationBuilder {
    pub fn new() -> Self {
        Self {}
    }
    pub fn build(self) -> PslSequenceDeclarationSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::PslSequenceDeclaration);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        PslSequenceDeclarationSyntax::cast(node).unwrap()
    }
}
pub struct PslVerificationUnitBuilder {}
impl Default for PslVerificationUnitBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl PslVerificationUnitBuilder {
    pub fn new() -> Self {
        Self {}
    }
    pub fn build(self) -> PslVerificationUnitSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::PslVerificationUnit);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        PslVerificationUnitSyntax::cast(node).unwrap()
    }
}
pub struct QualifiedExpressionBuilder {
    name: NameSyntax,
    tick_token: Token,
    expression: ExpressionSyntax,
}
impl QualifiedExpressionBuilder {
    pub fn new(name: NameSyntax, expression: ExpressionSyntax) -> Self {
        Self {
            name,
            tick_token: Token::new(
                TokenKind::Tick,
                TokenKind::Tick.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            expression,
        }
    }
    pub fn with_name(mut self, n: NameSyntax) -> Self {
        self.name = n;
        self
    }
    pub fn with_tick_token(mut self, t: Token) -> Self {
        self.tick_token = t;
        self
    }
    pub fn with_expression(mut self, n: ExpressionSyntax) -> Self {
        self.expression = n;
        self
    }
    pub fn build(self) -> QualifiedExpressionSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::QualifiedExpression);
        builder.push_node(self.name.raw().green().clone());
        builder.push(self.tick_token);
        builder.push_node(self.expression.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        QualifiedExpressionSyntax::cast(node).unwrap()
    }
}
pub struct RangeConstraintBuilder {
    range_token: Token,
    range: RangeSyntax,
}
impl RangeConstraintBuilder {
    pub fn new(range: RangeSyntax) -> Self {
        Self {
            range_token: Token::new(
                TokenKind::Keyword(Kw::Range),
                Kw::Range.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            range,
        }
    }
    pub fn with_range_token(mut self, t: Token) -> Self {
        self.range_token = t;
        self
    }
    pub fn with_range(mut self, n: RangeSyntax) -> Self {
        self.range = n;
        self
    }
    pub fn build(self) -> RangeConstraintSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::RangeConstraint);
        builder.push(self.range_token);
        builder.push_node(self.range.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        RangeConstraintSyntax::cast(node).unwrap()
    }
}
pub struct RangeConstraintConstraintBuilder {
    range_constraint: RangeConstraintSyntax,
}
impl RangeConstraintConstraintBuilder {
    pub fn new(range_constraint: RangeConstraintSyntax) -> Self {
        Self { range_constraint }
    }
    pub fn with_range_constraint(mut self, n: RangeConstraintSyntax) -> Self {
        self.range_constraint = n;
        self
    }
    pub fn build(self) -> RangeConstraintConstraintSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::RangeConstraintConstraint);
        builder.push_node(self.range_constraint.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        RangeConstraintConstraintSyntax::cast(node).unwrap()
    }
}
pub struct RangeExpressionBuilder {
    lhs: ExpressionSyntax,
    direction: DirectionSyntax,
    rhs: ExpressionSyntax,
}
impl RangeExpressionBuilder {
    pub fn new(lhs: ExpressionSyntax, direction: DirectionSyntax, rhs: ExpressionSyntax) -> Self {
        Self {
            lhs,
            direction,
            rhs,
        }
    }
    pub fn with_lhs(mut self, n: ExpressionSyntax) -> Self {
        self.lhs = n;
        self
    }
    pub fn with_direction(mut self, n: DirectionSyntax) -> Self {
        self.direction = n;
        self
    }
    pub fn with_rhs(mut self, n: ExpressionSyntax) -> Self {
        self.rhs = n;
        self
    }
    pub fn build(self) -> RangeExpressionSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::RangeExpression);
        builder.push_node(self.lhs.raw().green().clone());
        builder.push(self.direction.raw().token().clone());
        builder.push_node(self.rhs.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        RangeExpressionSyntax::cast(node).unwrap()
    }
}
pub struct RawTokensBuilder {}
impl Default for RawTokensBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl RawTokensBuilder {
    pub fn new() -> Self {
        Self {}
    }
    pub fn build(self) -> RawTokensSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::RawTokens);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        RawTokensSyntax::cast(node).unwrap()
    }
}
pub struct RecordConstraintBuilder {
    left_par_token: Token,
    record_element_constraints: Vec<RecordElementConstraintSyntax>,
    comma_token: Vec<Token>,
    right_par_token: Token,
}
impl Default for RecordConstraintBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl RecordConstraintBuilder {
    pub fn new() -> Self {
        Self {
            left_par_token: Token::new(
                TokenKind::LeftPar,
                TokenKind::LeftPar.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            record_element_constraints: Vec::new(),
            comma_token: Vec::new(),
            right_par_token: Token::new(
                TokenKind::RightPar,
                TokenKind::RightPar.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_left_par_token(mut self, t: Token) -> Self {
        self.left_par_token = t;
        self
    }
    pub fn add_record_element_constraints(mut self, n: RecordElementConstraintSyntax) -> Self {
        self.record_element_constraints.push(n);
        self
    }
    pub fn add_comma_token(mut self, t: Token) -> Self {
        self.comma_token.push(t);
        self
    }
    pub fn with_right_par_token(mut self, t: Token) -> Self {
        self.right_par_token = t;
        self
    }
    pub fn build(self) -> RecordConstraintSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::RecordConstraint);
        builder.push(self.left_par_token);
        for n in self.record_element_constraints {
            builder.push_node(n.raw().green().clone());
        }
        for t in self.comma_token {
            builder.push(t);
        }
        builder.push(self.right_par_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        RecordConstraintSyntax::cast(node).unwrap()
    }
}
pub struct RecordElementConstraintBuilder {
    name: NameSyntax,
    constraint: ConstraintSyntax,
}
impl RecordElementConstraintBuilder {
    pub fn new(name: NameSyntax, constraint: ConstraintSyntax) -> Self {
        Self { name, constraint }
    }
    pub fn with_name(mut self, n: NameSyntax) -> Self {
        self.name = n;
        self
    }
    pub fn with_constraint(mut self, n: ConstraintSyntax) -> Self {
        self.constraint = n;
        self
    }
    pub fn build(self) -> RecordElementConstraintSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::RecordElementConstraint);
        builder.push_node(self.name.raw().green().clone());
        builder.push_node(self.constraint.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        RecordElementConstraintSyntax::cast(node).unwrap()
    }
}
pub struct RecordElementDeclarationsBuilder {
    element_declarations: Vec<ElementDeclarationSyntax>,
}
impl Default for RecordElementDeclarationsBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl RecordElementDeclarationsBuilder {
    pub fn new() -> Self {
        Self {
            element_declarations: Vec::new(),
        }
    }
    pub fn add_element_declarations(mut self, n: ElementDeclarationSyntax) -> Self {
        self.element_declarations.push(n);
        self
    }
    pub fn build(self) -> RecordElementDeclarationsSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::RecordElementDeclarations);
        for n in self.element_declarations {
            builder.push_node(n.raw().green().clone());
        }
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        RecordElementDeclarationsSyntax::cast(node).unwrap()
    }
}
pub struct RecordElementResolutionBuilder {
    name_token: Token,
    resolution_indication: ResolutionIndicationSyntax,
}
impl RecordElementResolutionBuilder {
    pub fn new(name_token: Token, resolution_indication: ResolutionIndicationSyntax) -> Self {
        Self {
            name_token,
            resolution_indication,
        }
    }
    pub fn with_name_token(mut self, t: Token) -> Self {
        self.name_token = t;
        self
    }
    pub fn with_resolution_indication(mut self, n: ResolutionIndicationSyntax) -> Self {
        self.resolution_indication = n;
        self
    }
    pub fn build(self) -> RecordElementResolutionSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::RecordElementResolution);
        builder.push(self.name_token);
        builder.push_node(self.resolution_indication.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        RecordElementResolutionSyntax::cast(node).unwrap()
    }
}
pub struct RecordResolutionBuilder {
    record_element_resolutions: Vec<RecordElementResolutionSyntax>,
    comma_token: Vec<Token>,
}
impl Default for RecordResolutionBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl RecordResolutionBuilder {
    pub fn new() -> Self {
        Self {
            record_element_resolutions: Vec::new(),
            comma_token: Vec::new(),
        }
    }
    pub fn add_record_element_resolutions(mut self, n: RecordElementResolutionSyntax) -> Self {
        self.record_element_resolutions.push(n);
        self
    }
    pub fn add_comma_token(mut self, t: Token) -> Self {
        self.comma_token.push(t);
        self
    }
    pub fn build(self) -> RecordResolutionSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::RecordResolution);
        for n in self.record_element_resolutions {
            builder.push_node(n.raw().green().clone());
        }
        for t in self.comma_token {
            builder.push(t);
        }
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        RecordResolutionSyntax::cast(node).unwrap()
    }
}
pub struct RecordResolutionElementResolutionBuilder {
    record_resolution: RecordResolutionSyntax,
}
impl Default for RecordResolutionElementResolutionBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl RecordResolutionElementResolutionBuilder {
    pub fn new() -> Self {
        Self {
            record_resolution: RecordResolutionBuilder::default().build(),
        }
    }
    pub fn with_record_resolution(mut self, n: RecordResolutionSyntax) -> Self {
        self.record_resolution = n;
        self
    }
    pub fn build(self) -> RecordResolutionElementResolutionSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::RecordResolutionElementResolution);
        builder.push_node(self.record_resolution.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        RecordResolutionElementResolutionSyntax::cast(node).unwrap()
    }
}
pub struct RecordTypeDefinitionBuilder {
    record_type_definition_preamble: RecordTypeDefinitionPreambleSyntax,
    record_element_declarations: RecordElementDeclarationsSyntax,
    record_type_definition_epilogue: RecordTypeDefinitionEpilogueSyntax,
}
impl RecordTypeDefinitionBuilder {
    pub fn new(record_type_definition_epilogue: RecordTypeDefinitionEpilogueSyntax) -> Self {
        Self {
            record_type_definition_preamble: RecordTypeDefinitionPreambleBuilder::default().build(),
            record_element_declarations: RecordElementDeclarationsBuilder::default().build(),
            record_type_definition_epilogue,
        }
    }
    pub fn with_record_type_definition_preamble(
        mut self,
        n: RecordTypeDefinitionPreambleSyntax,
    ) -> Self {
        self.record_type_definition_preamble = n;
        self
    }
    pub fn with_record_element_declarations(mut self, n: RecordElementDeclarationsSyntax) -> Self {
        self.record_element_declarations = n;
        self
    }
    pub fn with_record_type_definition_epilogue(
        mut self,
        n: RecordTypeDefinitionEpilogueSyntax,
    ) -> Self {
        self.record_type_definition_epilogue = n;
        self
    }
    pub fn build(self) -> RecordTypeDefinitionSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::RecordTypeDefinition);
        builder.push_node(self.record_type_definition_preamble.raw().green().clone());
        builder.push_node(self.record_element_declarations.raw().green().clone());
        builder.push_node(self.record_type_definition_epilogue.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        RecordTypeDefinitionSyntax::cast(node).unwrap()
    }
}
pub struct RecordTypeDefinitionEpilogueBuilder {
    end_token: Token,
    record_token: Token,
    identifier_token: Token,
}
impl RecordTypeDefinitionEpilogueBuilder {
    pub fn new(identifier_token: Token) -> Self {
        Self {
            end_token: Token::new(
                TokenKind::Keyword(Kw::End),
                Kw::End.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            record_token: Token::new(
                TokenKind::Keyword(Kw::Record),
                Kw::Record.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            identifier_token,
        }
    }
    pub fn with_end_token(mut self, t: Token) -> Self {
        self.end_token = t;
        self
    }
    pub fn with_record_token(mut self, t: Token) -> Self {
        self.record_token = t;
        self
    }
    pub fn with_identifier_token(mut self, t: Token) -> Self {
        self.identifier_token = t;
        self
    }
    pub fn build(self) -> RecordTypeDefinitionEpilogueSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::RecordTypeDefinitionEpilogue);
        builder.push(self.end_token);
        builder.push(self.record_token);
        builder.push(self.identifier_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        RecordTypeDefinitionEpilogueSyntax::cast(node).unwrap()
    }
}
pub struct RecordTypeDefinitionPreambleBuilder {
    record_token: Token,
}
impl Default for RecordTypeDefinitionPreambleBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl RecordTypeDefinitionPreambleBuilder {
    pub fn new() -> Self {
        Self {
            record_token: Token::new(
                TokenKind::Keyword(Kw::Record),
                Kw::Record.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_record_token(mut self, t: Token) -> Self {
        self.record_token = t;
        self
    }
    pub fn build(self) -> RecordTypeDefinitionPreambleSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::RecordTypeDefinitionPreamble);
        builder.push(self.record_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        RecordTypeDefinitionPreambleSyntax::cast(node).unwrap()
    }
}
pub struct RelativePathnameBuilder {
    circ_token: Vec<Token>,
    dot_token: Vec<Token>,
    partial_pathname: PartialPathnameSyntax,
}
impl Default for RelativePathnameBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl RelativePathnameBuilder {
    pub fn new() -> Self {
        Self {
            circ_token: Vec::new(),
            dot_token: Vec::new(),
            partial_pathname: PartialPathnameBuilder::default().build(),
        }
    }
    pub fn add_circ_token(mut self, t: Token) -> Self {
        self.circ_token.push(t);
        self
    }
    pub fn add_dot_token(mut self, t: Token) -> Self {
        self.dot_token.push(t);
        self
    }
    pub fn with_partial_pathname(mut self, n: PartialPathnameSyntax) -> Self {
        self.partial_pathname = n;
        self
    }
    pub fn build(self) -> RelativePathnameSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::RelativePathname);
        for t in self.circ_token {
            builder.push(t);
        }
        for t in self.dot_token {
            builder.push(t);
        }
        builder.push_node(self.partial_pathname.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        RelativePathnameSyntax::cast(node).unwrap()
    }
}
pub struct ReportStatementBuilder {
    label: LabelSyntax,
    report_token: Token,
    report: ExpressionSyntax,
    severity_token: Option<Token>,
    severity: Option<ExpressionSyntax>,
}
impl ReportStatementBuilder {
    pub fn new(label: LabelSyntax, report: ExpressionSyntax) -> Self {
        Self {
            label,
            report_token: Token::new(
                TokenKind::Keyword(Kw::Report),
                Kw::Report.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            report,
            severity_token: None,
            severity: None,
        }
    }
    pub fn with_label(mut self, n: LabelSyntax) -> Self {
        self.label = n;
        self
    }
    pub fn with_report_token(mut self, t: Token) -> Self {
        self.report_token = t;
        self
    }
    pub fn with_report(mut self, n: ExpressionSyntax) -> Self {
        self.report = n;
        self
    }
    pub fn with_severity_token(mut self, t: Token) -> Self {
        self.severity_token = Some(t);
        self
    }
    pub fn with_severity(mut self, n: ExpressionSyntax) -> Self {
        self.severity = Some(n);
        self
    }
    pub fn build(self) -> ReportStatementSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ReportStatement);
        builder.push_node(self.label.raw().green().clone());
        builder.push(self.report_token);
        builder.push_node(self.report.raw().green().clone());
        if let Some(t) = self.severity_token {
            builder.push(t);
        }
        if let Some(n) = self.severity {
            builder.push_node(n.raw().green().clone());
        }
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ReportStatementSyntax::cast(node).unwrap()
    }
}
pub struct ResolutionIndicationElementResolutionBuilder {
    resolution_indication: ResolutionIndicationSyntax,
}
impl ResolutionIndicationElementResolutionBuilder {
    pub fn new(resolution_indication: ResolutionIndicationSyntax) -> Self {
        Self {
            resolution_indication,
        }
    }
    pub fn with_resolution_indication(mut self, n: ResolutionIndicationSyntax) -> Self {
        self.resolution_indication = n;
        self
    }
    pub fn build(self) -> ResolutionIndicationElementResolutionSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ResolutionIndicationElementResolution);
        builder.push_node(self.resolution_indication.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ResolutionIndicationElementResolutionSyntax::cast(node).unwrap()
    }
}
pub struct ReturnStatementBuilder {
    label: LabelSyntax,
    return_token: Token,
    expression: Option<ExpressionSyntax>,
}
impl ReturnStatementBuilder {
    pub fn new(label: LabelSyntax) -> Self {
        Self {
            label,
            return_token: Token::new(
                TokenKind::Keyword(Kw::Return),
                Kw::Return.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            expression: None,
        }
    }
    pub fn with_label(mut self, n: LabelSyntax) -> Self {
        self.label = n;
        self
    }
    pub fn with_return_token(mut self, t: Token) -> Self {
        self.return_token = t;
        self
    }
    pub fn with_expression(mut self, n: ExpressionSyntax) -> Self {
        self.expression = Some(n);
        self
    }
    pub fn build(self) -> ReturnStatementSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::ReturnStatement);
        builder.push_node(self.label.raw().green().clone());
        builder.push(self.return_token);
        if let Some(n) = self.expression {
            builder.push_node(n.raw().green().clone());
        }
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        ReturnStatementSyntax::cast(node).unwrap()
    }
}
pub struct SecondaryUnitDeclarationBuilder {
    identifier_token: Token,
    eq_token: Token,
    physical_literal: PhysicalLiteralSyntax,
    semi_colon_token: Token,
}
impl SecondaryUnitDeclarationBuilder {
    pub fn new(identifier_token: Token, physical_literal: PhysicalLiteralSyntax) -> Self {
        Self {
            identifier_token,
            eq_token: Token::new(
                TokenKind::EQ,
                TokenKind::EQ.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            physical_literal,
            semi_colon_token: Token::new(
                TokenKind::SemiColon,
                TokenKind::SemiColon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_identifier_token(mut self, t: Token) -> Self {
        self.identifier_token = t;
        self
    }
    pub fn with_eq_token(mut self, t: Token) -> Self {
        self.eq_token = t;
        self
    }
    pub fn with_physical_literal(mut self, n: PhysicalLiteralSyntax) -> Self {
        self.physical_literal = n;
        self
    }
    pub fn with_semi_colon_token(mut self, t: Token) -> Self {
        self.semi_colon_token = t;
        self
    }
    pub fn build(self) -> SecondaryUnitDeclarationSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::SecondaryUnitDeclaration);
        builder.push(self.identifier_token);
        builder.push(self.eq_token);
        builder.push_node(self.physical_literal.raw().green().clone());
        builder.push(self.semi_colon_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        SecondaryUnitDeclarationSyntax::cast(node).unwrap()
    }
}
pub struct SecondaryUnitPackageBodyBuilder {
    package_body: PackageBodySyntax,
}
impl SecondaryUnitPackageBodyBuilder {
    pub fn new(package_body: PackageBodySyntax) -> Self {
        Self { package_body }
    }
    pub fn with_package_body(mut self, n: PackageBodySyntax) -> Self {
        self.package_body = n;
        self
    }
    pub fn build(self) -> SecondaryUnitPackageBodySyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::SecondaryUnitPackageBody);
        builder.push_node(self.package_body.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        SecondaryUnitPackageBodySyntax::cast(node).unwrap()
    }
}
pub struct SelectedAssignmentPreambleBuilder {
    with_token: Token,
    expression: ExpressionSyntax,
    select_token: Token,
    que_token: Option<Token>,
}
impl SelectedAssignmentPreambleBuilder {
    pub fn new(expression: ExpressionSyntax) -> Self {
        Self {
            with_token: Token::new(
                TokenKind::Keyword(Kw::With),
                Kw::With.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            expression,
            select_token: Token::new(
                TokenKind::Keyword(Kw::Select),
                Kw::Select.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            que_token: None,
        }
    }
    pub fn with_with_token(mut self, t: Token) -> Self {
        self.with_token = t;
        self
    }
    pub fn with_expression(mut self, n: ExpressionSyntax) -> Self {
        self.expression = n;
        self
    }
    pub fn with_select_token(mut self, t: Token) -> Self {
        self.select_token = t;
        self
    }
    pub fn with_que_token(mut self, t: Token) -> Self {
        self.que_token = Some(t);
        self
    }
    pub fn build(self) -> SelectedAssignmentPreambleSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::SelectedAssignmentPreamble);
        builder.push(self.with_token);
        builder.push_node(self.expression.raw().green().clone());
        builder.push(self.select_token);
        if let Some(t) = self.que_token {
            builder.push(t);
        }
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        SelectedAssignmentPreambleSyntax::cast(node).unwrap()
    }
}
pub struct SelectedExpressionItemBuilder {
    expression: ExpressionSyntax,
    when_token: Token,
    choices: ChoicesSyntax,
}
impl SelectedExpressionItemBuilder {
    pub fn new(expression: ExpressionSyntax) -> Self {
        Self {
            expression,
            when_token: Token::new(
                TokenKind::Keyword(Kw::When),
                Kw::When.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            choices: ChoicesBuilder::default().build(),
        }
    }
    pub fn with_expression(mut self, n: ExpressionSyntax) -> Self {
        self.expression = n;
        self
    }
    pub fn with_when_token(mut self, t: Token) -> Self {
        self.when_token = t;
        self
    }
    pub fn with_choices(mut self, n: ChoicesSyntax) -> Self {
        self.choices = n;
        self
    }
    pub fn build(self) -> SelectedExpressionItemSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::SelectedExpressionItem);
        builder.push_node(self.expression.raw().green().clone());
        builder.push(self.when_token);
        builder.push_node(self.choices.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        SelectedExpressionItemSyntax::cast(node).unwrap()
    }
}
pub struct SelectedExpressionsBuilder {
    selected_expression_items: Vec<SelectedExpressionItemSyntax>,
    comma_token: Vec<Token>,
}
impl Default for SelectedExpressionsBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl SelectedExpressionsBuilder {
    pub fn new() -> Self {
        Self {
            selected_expression_items: Vec::new(),
            comma_token: Vec::new(),
        }
    }
    pub fn add_selected_expression_items(mut self, n: SelectedExpressionItemSyntax) -> Self {
        self.selected_expression_items.push(n);
        self
    }
    pub fn add_comma_token(mut self, t: Token) -> Self {
        self.comma_token.push(t);
        self
    }
    pub fn build(self) -> SelectedExpressionsSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::SelectedExpressions);
        for n in self.selected_expression_items {
            builder.push_node(n.raw().green().clone());
        }
        for t in self.comma_token {
            builder.push(t);
        }
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        SelectedExpressionsSyntax::cast(node).unwrap()
    }
}
pub struct SelectedForceAssignmentBuilder {
    selected_assignment_preamble: SelectedAssignmentPreambleSyntax,
    target: TargetSyntax,
    lte_token: Token,
    force_token: Token,
    force_mode: Option<ForceModeSyntax>,
    selected_expressions: SelectedExpressionsSyntax,
    semi_colon_token: Token,
}
impl SelectedForceAssignmentBuilder {
    pub fn new(
        selected_assignment_preamble: SelectedAssignmentPreambleSyntax,
        target: TargetSyntax,
    ) -> Self {
        Self {
            selected_assignment_preamble,
            target,
            lte_token: Token::new(
                TokenKind::LTE,
                TokenKind::LTE.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            force_token: Token::new(
                TokenKind::Keyword(Kw::Force),
                Kw::Force.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            force_mode: None,
            selected_expressions: SelectedExpressionsBuilder::default().build(),
            semi_colon_token: Token::new(
                TokenKind::SemiColon,
                TokenKind::SemiColon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_selected_assignment_preamble(
        mut self,
        n: SelectedAssignmentPreambleSyntax,
    ) -> Self {
        self.selected_assignment_preamble = n;
        self
    }
    pub fn with_target(mut self, n: TargetSyntax) -> Self {
        self.target = n;
        self
    }
    pub fn with_lte_token(mut self, t: Token) -> Self {
        self.lte_token = t;
        self
    }
    pub fn with_force_token(mut self, t: Token) -> Self {
        self.force_token = t;
        self
    }
    pub fn with_force_mode(mut self, n: ForceModeSyntax) -> Self {
        self.force_mode = Some(n);
        self
    }
    pub fn with_selected_expressions(mut self, n: SelectedExpressionsSyntax) -> Self {
        self.selected_expressions = n;
        self
    }
    pub fn with_semi_colon_token(mut self, t: Token) -> Self {
        self.semi_colon_token = t;
        self
    }
    pub fn build(self) -> SelectedForceAssignmentSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::SelectedForceAssignment);
        builder.push_node(self.selected_assignment_preamble.raw().green().clone());
        builder.push_node(self.target.raw().green().clone());
        builder.push(self.lte_token);
        builder.push(self.force_token);
        if let Some(n) = self.force_mode {
            builder.push(n.raw().token().clone());
        }
        builder.push_node(self.selected_expressions.raw().green().clone());
        builder.push(self.semi_colon_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        SelectedForceAssignmentSyntax::cast(node).unwrap()
    }
}
pub struct SelectedNameBuilder {
    dot_token: Token,
    suffix: SuffixSyntax,
}
impl SelectedNameBuilder {
    pub fn new(suffix: SuffixSyntax) -> Self {
        Self {
            dot_token: Token::new(
                TokenKind::Dot,
                TokenKind::Dot.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            suffix,
        }
    }
    pub fn with_dot_token(mut self, t: Token) -> Self {
        self.dot_token = t;
        self
    }
    pub fn with_suffix(mut self, n: SuffixSyntax) -> Self {
        self.suffix = n;
        self
    }
    pub fn build(self) -> SelectedNameSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::SelectedName);
        builder.push(self.dot_token);
        builder.push(self.suffix.raw().token().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        SelectedNameSyntax::cast(node).unwrap()
    }
}
pub struct SelectedVariableAssignmentBuilder {
    selected_assignment_preamble: SelectedAssignmentPreambleSyntax,
    target: TargetSyntax,
    colon_eq_token: Token,
    selected_expressions: SelectedExpressionsSyntax,
    semi_colon_token: Token,
}
impl SelectedVariableAssignmentBuilder {
    pub fn new(
        selected_assignment_preamble: SelectedAssignmentPreambleSyntax,
        target: TargetSyntax,
    ) -> Self {
        Self {
            selected_assignment_preamble,
            target,
            colon_eq_token: Token::new(
                TokenKind::ColonEq,
                TokenKind::ColonEq.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            selected_expressions: SelectedExpressionsBuilder::default().build(),
            semi_colon_token: Token::new(
                TokenKind::SemiColon,
                TokenKind::SemiColon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_selected_assignment_preamble(
        mut self,
        n: SelectedAssignmentPreambleSyntax,
    ) -> Self {
        self.selected_assignment_preamble = n;
        self
    }
    pub fn with_target(mut self, n: TargetSyntax) -> Self {
        self.target = n;
        self
    }
    pub fn with_colon_eq_token(mut self, t: Token) -> Self {
        self.colon_eq_token = t;
        self
    }
    pub fn with_selected_expressions(mut self, n: SelectedExpressionsSyntax) -> Self {
        self.selected_expressions = n;
        self
    }
    pub fn with_semi_colon_token(mut self, t: Token) -> Self {
        self.semi_colon_token = t;
        self
    }
    pub fn build(self) -> SelectedVariableAssignmentSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::SelectedVariableAssignment);
        builder.push_node(self.selected_assignment_preamble.raw().green().clone());
        builder.push_node(self.target.raw().green().clone());
        builder.push(self.colon_eq_token);
        builder.push_node(self.selected_expressions.raw().green().clone());
        builder.push(self.semi_colon_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        SelectedVariableAssignmentSyntax::cast(node).unwrap()
    }
}
pub struct SelectedWaveformAssignmentBuilder {
    selected_assignment_preamble: SelectedAssignmentPreambleSyntax,
    target: TargetSyntax,
    lte_token: Token,
    delay_mechanism: Option<DelayMechanismSyntax>,
    selected_waveforms: SelectedWaveformsSyntax,
    semi_colon_token: Token,
}
impl SelectedWaveformAssignmentBuilder {
    pub fn new(
        selected_assignment_preamble: SelectedAssignmentPreambleSyntax,
        target: TargetSyntax,
    ) -> Self {
        Self {
            selected_assignment_preamble,
            target,
            lte_token: Token::new(
                TokenKind::LTE,
                TokenKind::LTE.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            delay_mechanism: None,
            selected_waveforms: SelectedWaveformsBuilder::default().build(),
            semi_colon_token: Token::new(
                TokenKind::SemiColon,
                TokenKind::SemiColon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_selected_assignment_preamble(
        mut self,
        n: SelectedAssignmentPreambleSyntax,
    ) -> Self {
        self.selected_assignment_preamble = n;
        self
    }
    pub fn with_target(mut self, n: TargetSyntax) -> Self {
        self.target = n;
        self
    }
    pub fn with_lte_token(mut self, t: Token) -> Self {
        self.lte_token = t;
        self
    }
    pub fn with_delay_mechanism(mut self, n: DelayMechanismSyntax) -> Self {
        self.delay_mechanism = Some(n);
        self
    }
    pub fn with_selected_waveforms(mut self, n: SelectedWaveformsSyntax) -> Self {
        self.selected_waveforms = n;
        self
    }
    pub fn with_semi_colon_token(mut self, t: Token) -> Self {
        self.semi_colon_token = t;
        self
    }
    pub fn build(self) -> SelectedWaveformAssignmentSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::SelectedWaveformAssignment);
        builder.push_node(self.selected_assignment_preamble.raw().green().clone());
        builder.push_node(self.target.raw().green().clone());
        builder.push(self.lte_token);
        if let Some(n) = self.delay_mechanism {
            builder.push_node(n.raw().green().clone());
        }
        builder.push_node(self.selected_waveforms.raw().green().clone());
        builder.push(self.semi_colon_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        SelectedWaveformAssignmentSyntax::cast(node).unwrap()
    }
}
pub struct SelectedWaveformItemBuilder {
    waveform: WaveformSyntax,
    when_token: Token,
    choices: ChoicesSyntax,
}
impl SelectedWaveformItemBuilder {
    pub fn new(waveform: WaveformSyntax) -> Self {
        Self {
            waveform,
            when_token: Token::new(
                TokenKind::Keyword(Kw::When),
                Kw::When.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            choices: ChoicesBuilder::default().build(),
        }
    }
    pub fn with_waveform(mut self, n: WaveformSyntax) -> Self {
        self.waveform = n;
        self
    }
    pub fn with_when_token(mut self, t: Token) -> Self {
        self.when_token = t;
        self
    }
    pub fn with_choices(mut self, n: ChoicesSyntax) -> Self {
        self.choices = n;
        self
    }
    pub fn build(self) -> SelectedWaveformItemSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::SelectedWaveformItem);
        builder.push_node(self.waveform.raw().green().clone());
        builder.push(self.when_token);
        builder.push_node(self.choices.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        SelectedWaveformItemSyntax::cast(node).unwrap()
    }
}
pub struct SelectedWaveformsBuilder {
    selected_waveform_items: Vec<SelectedWaveformItemSyntax>,
    comma_token: Vec<Token>,
}
impl Default for SelectedWaveformsBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl SelectedWaveformsBuilder {
    pub fn new() -> Self {
        Self {
            selected_waveform_items: Vec::new(),
            comma_token: Vec::new(),
        }
    }
    pub fn add_selected_waveform_items(mut self, n: SelectedWaveformItemSyntax) -> Self {
        self.selected_waveform_items.push(n);
        self
    }
    pub fn add_comma_token(mut self, t: Token) -> Self {
        self.comma_token.push(t);
        self
    }
    pub fn build(self) -> SelectedWaveformsSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::SelectedWaveforms);
        for n in self.selected_waveform_items {
            builder.push_node(n.raw().green().clone());
        }
        for t in self.comma_token {
            builder.push(t);
        }
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        SelectedWaveformsSyntax::cast(node).unwrap()
    }
}
pub struct SemiColonTerminatedBindingIndicationBuilder {
    binding_indication: BindingIndicationSyntax,
    semi_colon_token: Token,
}
impl Default for SemiColonTerminatedBindingIndicationBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl SemiColonTerminatedBindingIndicationBuilder {
    pub fn new() -> Self {
        Self {
            binding_indication: BindingIndicationBuilder::default().build(),
            semi_colon_token: Token::new(
                TokenKind::SemiColon,
                TokenKind::SemiColon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_binding_indication(mut self, n: BindingIndicationSyntax) -> Self {
        self.binding_indication = n;
        self
    }
    pub fn with_semi_colon_token(mut self, t: Token) -> Self {
        self.semi_colon_token = t;
        self
    }
    pub fn build(self) -> SemiColonTerminatedBindingIndicationSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::SemiColonTerminatedBindingIndication);
        builder.push_node(self.binding_indication.raw().green().clone());
        builder.push(self.semi_colon_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        SemiColonTerminatedBindingIndicationSyntax::cast(node).unwrap()
    }
}
pub struct SemiColonTerminatedGenericMapAspectBuilder {
    generic_map_aspect: GenericMapAspectSyntax,
    semi_colon_token: Token,
}
impl Default for SemiColonTerminatedGenericMapAspectBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl SemiColonTerminatedGenericMapAspectBuilder {
    pub fn new() -> Self {
        Self {
            generic_map_aspect: GenericMapAspectBuilder::default().build(),
            semi_colon_token: Token::new(
                TokenKind::SemiColon,
                TokenKind::SemiColon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_generic_map_aspect(mut self, n: GenericMapAspectSyntax) -> Self {
        self.generic_map_aspect = n;
        self
    }
    pub fn with_semi_colon_token(mut self, t: Token) -> Self {
        self.semi_colon_token = t;
        self
    }
    pub fn build(self) -> SemiColonTerminatedGenericMapAspectSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::SemiColonTerminatedGenericMapAspect);
        builder.push_node(self.generic_map_aspect.raw().green().clone());
        builder.push(self.semi_colon_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        SemiColonTerminatedGenericMapAspectSyntax::cast(node).unwrap()
    }
}
pub struct SemiColonTerminatedPortMapAspectBuilder {
    port_map_aspect: PortMapAspectSyntax,
    semi_colon_token: Token,
}
impl Default for SemiColonTerminatedPortMapAspectBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl SemiColonTerminatedPortMapAspectBuilder {
    pub fn new() -> Self {
        Self {
            port_map_aspect: PortMapAspectBuilder::default().build(),
            semi_colon_token: Token::new(
                TokenKind::SemiColon,
                TokenKind::SemiColon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_port_map_aspect(mut self, n: PortMapAspectSyntax) -> Self {
        self.port_map_aspect = n;
        self
    }
    pub fn with_semi_colon_token(mut self, t: Token) -> Self {
        self.semi_colon_token = t;
        self
    }
    pub fn build(self) -> SemiColonTerminatedPortMapAspectSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::SemiColonTerminatedPortMapAspect);
        builder.push_node(self.port_map_aspect.raw().green().clone());
        builder.push(self.semi_colon_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        SemiColonTerminatedPortMapAspectSyntax::cast(node).unwrap()
    }
}
pub struct SemiColonTerminatedVerificationUnitBindingIndicationBuilder {
    verification_unit_binding_indication: VerificationUnitBindingIndicationSyntax,
    semi_colon_token: Token,
}
impl Default for SemiColonTerminatedVerificationUnitBindingIndicationBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl SemiColonTerminatedVerificationUnitBindingIndicationBuilder {
    pub fn new() -> Self {
        Self {
            verification_unit_binding_indication:
                VerificationUnitBindingIndicationBuilder::default().build(),
            semi_colon_token: Token::new(
                TokenKind::SemiColon,
                TokenKind::SemiColon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_verification_unit_binding_indication(
        mut self,
        n: VerificationUnitBindingIndicationSyntax,
    ) -> Self {
        self.verification_unit_binding_indication = n;
        self
    }
    pub fn with_semi_colon_token(mut self, t: Token) -> Self {
        self.semi_colon_token = t;
        self
    }
    pub fn build(self) -> SemiColonTerminatedVerificationUnitBindingIndicationSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::SemiColonTerminatedVerificationUnitBindingIndication);
        builder.push_node(
            self.verification_unit_binding_indication
                .raw()
                .green()
                .clone(),
        );
        builder.push(self.semi_colon_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        SemiColonTerminatedVerificationUnitBindingIndicationSyntax::cast(node).unwrap()
    }
}
pub struct SensitivityClauseBuilder {
    on_token: Token,
    name_list: NameListSyntax,
}
impl Default for SensitivityClauseBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl SensitivityClauseBuilder {
    pub fn new() -> Self {
        Self {
            on_token: Token::new(
                TokenKind::Keyword(Kw::On),
                Kw::On.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            name_list: NameListBuilder::default().build(),
        }
    }
    pub fn with_on_token(mut self, t: Token) -> Self {
        self.on_token = t;
        self
    }
    pub fn with_name_list(mut self, n: NameListSyntax) -> Self {
        self.name_list = n;
        self
    }
    pub fn build(self) -> SensitivityClauseSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::SensitivityClause);
        builder.push(self.on_token);
        builder.push_node(self.name_list.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        SensitivityClauseSyntax::cast(node).unwrap()
    }
}
pub struct SensitivityListBuilder {
    names: Vec<NameSyntax>,
    comma_token: Vec<Token>,
}
impl Default for SensitivityListBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl SensitivityListBuilder {
    pub fn new() -> Self {
        Self {
            names: Vec::new(),
            comma_token: Vec::new(),
        }
    }
    pub fn add_names(mut self, n: NameSyntax) -> Self {
        self.names.push(n);
        self
    }
    pub fn add_comma_token(mut self, t: Token) -> Self {
        self.comma_token.push(t);
        self
    }
    pub fn build(self) -> SensitivityListSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::SensitivityList);
        for n in self.names {
            builder.push_node(n.raw().green().clone());
        }
        for t in self.comma_token {
            builder.push(t);
        }
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        SensitivityListSyntax::cast(node).unwrap()
    }
}
pub struct SequentialStatementsBuilder {
    sequential_statements: Vec<SequentialStatementSyntax>,
}
impl Default for SequentialStatementsBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl SequentialStatementsBuilder {
    pub fn new() -> Self {
        Self {
            sequential_statements: Vec::new(),
        }
    }
    pub fn add_sequential_statements(mut self, n: SequentialStatementSyntax) -> Self {
        self.sequential_statements.push(n);
        self
    }
    pub fn build(self) -> SequentialStatementsSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::SequentialStatements);
        for n in self.sequential_statements {
            builder.push_node(n.raw().green().clone());
        }
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        SequentialStatementsSyntax::cast(node).unwrap()
    }
}
pub struct SharedVariableDeclarationBuilder {
    shared_token: Token,
    variable_token: Token,
    identifier_list: IdentifierListSyntax,
    colon_token: Token,
    subtype_indication: SubtypeIndicationSyntax,
    colon_eq_token: Option<Token>,
    expression: Option<ExpressionSyntax>,
    semi_colon_token: Token,
}
impl SharedVariableDeclarationBuilder {
    pub fn new(
        identifier_list: IdentifierListSyntax,
        subtype_indication: SubtypeIndicationSyntax,
    ) -> Self {
        Self {
            shared_token: Token::new(
                TokenKind::Keyword(Kw::Shared),
                Kw::Shared.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            variable_token: Token::new(
                TokenKind::Keyword(Kw::Variable),
                Kw::Variable.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            identifier_list,
            colon_token: Token::new(
                TokenKind::Colon,
                TokenKind::Colon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            subtype_indication,
            colon_eq_token: None,
            expression: None,
            semi_colon_token: Token::new(
                TokenKind::SemiColon,
                TokenKind::SemiColon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_shared_token(mut self, t: Token) -> Self {
        self.shared_token = t;
        self
    }
    pub fn with_variable_token(mut self, t: Token) -> Self {
        self.variable_token = t;
        self
    }
    pub fn with_identifier_list(mut self, n: IdentifierListSyntax) -> Self {
        self.identifier_list = n;
        self
    }
    pub fn with_colon_token(mut self, t: Token) -> Self {
        self.colon_token = t;
        self
    }
    pub fn with_subtype_indication(mut self, n: SubtypeIndicationSyntax) -> Self {
        self.subtype_indication = n;
        self
    }
    pub fn with_colon_eq_token(mut self, t: Token) -> Self {
        self.colon_eq_token = Some(t);
        self
    }
    pub fn with_expression(mut self, n: ExpressionSyntax) -> Self {
        self.expression = Some(n);
        self
    }
    pub fn with_semi_colon_token(mut self, t: Token) -> Self {
        self.semi_colon_token = t;
        self
    }
    pub fn build(self) -> SharedVariableDeclarationSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::SharedVariableDeclaration);
        builder.push(self.shared_token);
        builder.push(self.variable_token);
        builder.push_node(self.identifier_list.raw().green().clone());
        builder.push(self.colon_token);
        builder.push_node(self.subtype_indication.raw().green().clone());
        if let Some(t) = self.colon_eq_token {
            builder.push(t);
        }
        if let Some(n) = self.expression {
            builder.push_node(n.raw().green().clone());
        }
        builder.push(self.semi_colon_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        SharedVariableDeclarationSyntax::cast(node).unwrap()
    }
}
pub struct SignalDeclarationBuilder {
    signal_token: Token,
    identifier_list: IdentifierListSyntax,
    colon_token: Token,
    subtype_indication: SubtypeIndicationSyntax,
    signal_kind: Option<SignalKindSyntax>,
    colon_eq_token: Option<Token>,
    expression: Option<ExpressionSyntax>,
    semi_colon_token: Token,
}
impl SignalDeclarationBuilder {
    pub fn new(
        identifier_list: IdentifierListSyntax,
        subtype_indication: SubtypeIndicationSyntax,
    ) -> Self {
        Self {
            signal_token: Token::new(
                TokenKind::Keyword(Kw::Signal),
                Kw::Signal.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            identifier_list,
            colon_token: Token::new(
                TokenKind::Colon,
                TokenKind::Colon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            subtype_indication,
            signal_kind: None,
            colon_eq_token: None,
            expression: None,
            semi_colon_token: Token::new(
                TokenKind::SemiColon,
                TokenKind::SemiColon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_signal_token(mut self, t: Token) -> Self {
        self.signal_token = t;
        self
    }
    pub fn with_identifier_list(mut self, n: IdentifierListSyntax) -> Self {
        self.identifier_list = n;
        self
    }
    pub fn with_colon_token(mut self, t: Token) -> Self {
        self.colon_token = t;
        self
    }
    pub fn with_subtype_indication(mut self, n: SubtypeIndicationSyntax) -> Self {
        self.subtype_indication = n;
        self
    }
    pub fn with_signal_kind(mut self, n: SignalKindSyntax) -> Self {
        self.signal_kind = Some(n);
        self
    }
    pub fn with_colon_eq_token(mut self, t: Token) -> Self {
        self.colon_eq_token = Some(t);
        self
    }
    pub fn with_expression(mut self, n: ExpressionSyntax) -> Self {
        self.expression = Some(n);
        self
    }
    pub fn with_semi_colon_token(mut self, t: Token) -> Self {
        self.semi_colon_token = t;
        self
    }
    pub fn build(self) -> SignalDeclarationSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::SignalDeclaration);
        builder.push(self.signal_token);
        builder.push_node(self.identifier_list.raw().green().clone());
        builder.push(self.colon_token);
        builder.push_node(self.subtype_indication.raw().green().clone());
        if let Some(n) = self.signal_kind {
            builder.push(n.raw().token().clone());
        }
        if let Some(t) = self.colon_eq_token {
            builder.push(t);
        }
        if let Some(n) = self.expression {
            builder.push_node(n.raw().green().clone());
        }
        builder.push(self.semi_colon_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        SignalDeclarationSyntax::cast(node).unwrap()
    }
}
pub struct SignalListAllBuilder {
    all_token: Token,
}
impl Default for SignalListAllBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl SignalListAllBuilder {
    pub fn new() -> Self {
        Self {
            all_token: Token::new(
                TokenKind::Keyword(Kw::All),
                Kw::All.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_all_token(mut self, t: Token) -> Self {
        self.all_token = t;
        self
    }
    pub fn build(self) -> SignalListAllSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::SignalListAll);
        builder.push(self.all_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        SignalListAllSyntax::cast(node).unwrap()
    }
}
pub struct SignalListListBuilder {
    names: Vec<NameSyntax>,
    comma_token: Vec<Token>,
}
impl Default for SignalListListBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl SignalListListBuilder {
    pub fn new() -> Self {
        Self {
            names: Vec::new(),
            comma_token: Vec::new(),
        }
    }
    pub fn add_names(mut self, n: NameSyntax) -> Self {
        self.names.push(n);
        self
    }
    pub fn add_comma_token(mut self, t: Token) -> Self {
        self.comma_token.push(t);
        self
    }
    pub fn build(self) -> SignalListListSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::SignalListList);
        for n in self.names {
            builder.push_node(n.raw().green().clone());
        }
        for t in self.comma_token {
            builder.push(t);
        }
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        SignalListListSyntax::cast(node).unwrap()
    }
}
pub struct SignalListOthersBuilder {
    others_token: Token,
}
impl Default for SignalListOthersBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl SignalListOthersBuilder {
    pub fn new() -> Self {
        Self {
            others_token: Token::new(
                TokenKind::Keyword(Kw::Others),
                Kw::Others.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_others_token(mut self, t: Token) -> Self {
        self.others_token = t;
        self
    }
    pub fn build(self) -> SignalListOthersSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::SignalListOthers);
        builder.push(self.others_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        SignalListOthersSyntax::cast(node).unwrap()
    }
}
pub struct SignatureBuilder {
    left_square_token: Token,
    names: Vec<NameSyntax>,
    return_token: Token,
    return_type: NameSyntax,
    right_square_token: Token,
}
impl SignatureBuilder {
    pub fn new(return_type: NameSyntax) -> Self {
        Self {
            left_square_token: Token::new(
                TokenKind::LeftSquare,
                TokenKind::LeftSquare.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            names: Vec::new(),
            return_token: Token::new(
                TokenKind::Keyword(Kw::Return),
                Kw::Return.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            return_type,
            right_square_token: Token::new(
                TokenKind::RightSquare,
                TokenKind::RightSquare.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_left_square_token(mut self, t: Token) -> Self {
        self.left_square_token = t;
        self
    }
    pub fn add_names(mut self, n: NameSyntax) -> Self {
        self.names.push(n);
        self
    }
    pub fn with_return_token(mut self, t: Token) -> Self {
        self.return_token = t;
        self
    }
    pub fn with_return_type(mut self, n: NameSyntax) -> Self {
        self.return_type = n;
        self
    }
    pub fn with_right_square_token(mut self, t: Token) -> Self {
        self.right_square_token = t;
        self
    }
    pub fn build(self) -> SignatureSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::Signature);
        builder.push(self.left_square_token);
        for n in self.names {
            builder.push_node(n.raw().green().clone());
        }
        builder.push(self.return_token);
        builder.push_node(self.return_type.raw().green().clone());
        builder.push(self.right_square_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        SignatureSyntax::cast(node).unwrap()
    }
}
pub struct SimpleConfigurationSpecificationBuilder {
    component_configuration_preamble: ComponentConfigurationPreambleSyntax,
    semi_colon_terminated_binding_indication: SemiColonTerminatedBindingIndicationSyntax,
    semi_colon_token: Token,
    component_configuration_epilogue: Option<ComponentConfigurationEpilogueSyntax>,
}
impl SimpleConfigurationSpecificationBuilder {
    pub fn new(component_configuration_preamble: ComponentConfigurationPreambleSyntax) -> Self {
        Self {
            component_configuration_preamble,
            semi_colon_terminated_binding_indication:
                SemiColonTerminatedBindingIndicationBuilder::default().build(),
            semi_colon_token: Token::new(
                TokenKind::SemiColon,
                TokenKind::SemiColon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            component_configuration_epilogue: None,
        }
    }
    pub fn with_component_configuration_preamble(
        mut self,
        n: ComponentConfigurationPreambleSyntax,
    ) -> Self {
        self.component_configuration_preamble = n;
        self
    }
    pub fn with_semi_colon_terminated_binding_indication(
        mut self,
        n: SemiColonTerminatedBindingIndicationSyntax,
    ) -> Self {
        self.semi_colon_terminated_binding_indication = n;
        self
    }
    pub fn with_semi_colon_token(mut self, t: Token) -> Self {
        self.semi_colon_token = t;
        self
    }
    pub fn with_component_configuration_epilogue(
        mut self,
        n: ComponentConfigurationEpilogueSyntax,
    ) -> Self {
        self.component_configuration_epilogue = Some(n);
        self
    }
    pub fn build(self) -> SimpleConfigurationSpecificationSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::SimpleConfigurationSpecification);
        builder.push_node(self.component_configuration_preamble.raw().green().clone());
        builder.push_node(
            self.semi_colon_terminated_binding_indication
                .raw()
                .green()
                .clone(),
        );
        builder.push(self.semi_colon_token);
        if let Some(n) = self.component_configuration_epilogue {
            builder.push_node(n.raw().green().clone());
        }
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        SimpleConfigurationSpecificationSyntax::cast(node).unwrap()
    }
}
pub struct SimpleForceAssignmentBuilder {
    label: Option<LabelSyntax>,
    target: TargetSyntax,
    lte_token: Token,
    force_token: Token,
    force_mode: Option<ForceModeSyntax>,
    expression: ExpressionSyntax,
    semi_colon_token: Token,
}
impl SimpleForceAssignmentBuilder {
    pub fn new(target: TargetSyntax, expression: ExpressionSyntax) -> Self {
        Self {
            label: None,
            target,
            lte_token: Token::new(
                TokenKind::LTE,
                TokenKind::LTE.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            force_token: Token::new(
                TokenKind::Keyword(Kw::Force),
                Kw::Force.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            force_mode: None,
            expression,
            semi_colon_token: Token::new(
                TokenKind::SemiColon,
                TokenKind::SemiColon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_label(mut self, n: LabelSyntax) -> Self {
        self.label = Some(n);
        self
    }
    pub fn with_target(mut self, n: TargetSyntax) -> Self {
        self.target = n;
        self
    }
    pub fn with_lte_token(mut self, t: Token) -> Self {
        self.lte_token = t;
        self
    }
    pub fn with_force_token(mut self, t: Token) -> Self {
        self.force_token = t;
        self
    }
    pub fn with_force_mode(mut self, n: ForceModeSyntax) -> Self {
        self.force_mode = Some(n);
        self
    }
    pub fn with_expression(mut self, n: ExpressionSyntax) -> Self {
        self.expression = n;
        self
    }
    pub fn with_semi_colon_token(mut self, t: Token) -> Self {
        self.semi_colon_token = t;
        self
    }
    pub fn build(self) -> SimpleForceAssignmentSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::SimpleForceAssignment);
        if let Some(n) = self.label {
            builder.push_node(n.raw().green().clone());
        }
        builder.push_node(self.target.raw().green().clone());
        builder.push(self.lte_token);
        builder.push(self.force_token);
        if let Some(n) = self.force_mode {
            builder.push(n.raw().token().clone());
        }
        builder.push_node(self.expression.raw().green().clone());
        builder.push(self.semi_colon_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        SimpleForceAssignmentSyntax::cast(node).unwrap()
    }
}
pub struct SimpleReleaseAssignmentBuilder {
    label: Option<LabelSyntax>,
    target: TargetSyntax,
    lte_token: Token,
    release_token: Token,
    force_mode: Option<ForceModeSyntax>,
    semi_colon_token: Token,
}
impl SimpleReleaseAssignmentBuilder {
    pub fn new(target: TargetSyntax) -> Self {
        Self {
            label: None,
            target,
            lte_token: Token::new(
                TokenKind::LTE,
                TokenKind::LTE.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            release_token: Token::new(
                TokenKind::Keyword(Kw::Release),
                Kw::Release.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            force_mode: None,
            semi_colon_token: Token::new(
                TokenKind::SemiColon,
                TokenKind::SemiColon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_label(mut self, n: LabelSyntax) -> Self {
        self.label = Some(n);
        self
    }
    pub fn with_target(mut self, n: TargetSyntax) -> Self {
        self.target = n;
        self
    }
    pub fn with_lte_token(mut self, t: Token) -> Self {
        self.lte_token = t;
        self
    }
    pub fn with_release_token(mut self, t: Token) -> Self {
        self.release_token = t;
        self
    }
    pub fn with_force_mode(mut self, n: ForceModeSyntax) -> Self {
        self.force_mode = Some(n);
        self
    }
    pub fn with_semi_colon_token(mut self, t: Token) -> Self {
        self.semi_colon_token = t;
        self
    }
    pub fn build(self) -> SimpleReleaseAssignmentSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::SimpleReleaseAssignment);
        if let Some(n) = self.label {
            builder.push_node(n.raw().green().clone());
        }
        builder.push_node(self.target.raw().green().clone());
        builder.push(self.lte_token);
        builder.push(self.release_token);
        if let Some(n) = self.force_mode {
            builder.push(n.raw().token().clone());
        }
        builder.push(self.semi_colon_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        SimpleReleaseAssignmentSyntax::cast(node).unwrap()
    }
}
pub struct SimpleVariableAssignmentBuilder {
    target: TargetSyntax,
    colon_eq_token: Token,
    expression: ExpressionSyntax,
    semi_colon_token: Token,
}
impl SimpleVariableAssignmentBuilder {
    pub fn new(target: TargetSyntax, expression: ExpressionSyntax) -> Self {
        Self {
            target,
            colon_eq_token: Token::new(
                TokenKind::ColonEq,
                TokenKind::ColonEq.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            expression,
            semi_colon_token: Token::new(
                TokenKind::SemiColon,
                TokenKind::SemiColon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_target(mut self, n: TargetSyntax) -> Self {
        self.target = n;
        self
    }
    pub fn with_colon_eq_token(mut self, t: Token) -> Self {
        self.colon_eq_token = t;
        self
    }
    pub fn with_expression(mut self, n: ExpressionSyntax) -> Self {
        self.expression = n;
        self
    }
    pub fn with_semi_colon_token(mut self, t: Token) -> Self {
        self.semi_colon_token = t;
        self
    }
    pub fn build(self) -> SimpleVariableAssignmentSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::SimpleVariableAssignment);
        builder.push_node(self.target.raw().green().clone());
        builder.push(self.colon_eq_token);
        builder.push_node(self.expression.raw().green().clone());
        builder.push(self.semi_colon_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        SimpleVariableAssignmentSyntax::cast(node).unwrap()
    }
}
pub struct SimpleWaveformAssignmentBuilder {
    label: Option<LabelSyntax>,
    target: TargetSyntax,
    lte_token: Token,
    delay_mechanism: Option<DelayMechanismSyntax>,
    waveform: WaveformSyntax,
    semi_colon_token: Token,
}
impl SimpleWaveformAssignmentBuilder {
    pub fn new(target: TargetSyntax, waveform: WaveformSyntax) -> Self {
        Self {
            label: None,
            target,
            lte_token: Token::new(
                TokenKind::LTE,
                TokenKind::LTE.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            delay_mechanism: None,
            waveform,
            semi_colon_token: Token::new(
                TokenKind::SemiColon,
                TokenKind::SemiColon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_label(mut self, n: LabelSyntax) -> Self {
        self.label = Some(n);
        self
    }
    pub fn with_target(mut self, n: TargetSyntax) -> Self {
        self.target = n;
        self
    }
    pub fn with_lte_token(mut self, t: Token) -> Self {
        self.lte_token = t;
        self
    }
    pub fn with_delay_mechanism(mut self, n: DelayMechanismSyntax) -> Self {
        self.delay_mechanism = Some(n);
        self
    }
    pub fn with_waveform(mut self, n: WaveformSyntax) -> Self {
        self.waveform = n;
        self
    }
    pub fn with_semi_colon_token(mut self, t: Token) -> Self {
        self.semi_colon_token = t;
        self
    }
    pub fn build(self) -> SimpleWaveformAssignmentSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::SimpleWaveformAssignment);
        if let Some(n) = self.label {
            builder.push_node(n.raw().green().clone());
        }
        builder.push_node(self.target.raw().green().clone());
        builder.push(self.lte_token);
        if let Some(n) = self.delay_mechanism {
            builder.push_node(n.raw().green().clone());
        }
        builder.push_node(self.waveform.raw().green().clone());
        builder.push(self.semi_colon_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        SimpleWaveformAssignmentSyntax::cast(node).unwrap()
    }
}
pub struct SubprogramBodyBuilder {
    subprogram_body_preamble: SubprogramBodyPreambleSyntax,
    declarations: DeclarationsSyntax,
    declaration_statement_separator: DeclarationStatementSeparatorSyntax,
    concurrent_statements: ConcurrentStatementsSyntax,
    subprogram_body_epilogue: SubprogramBodyEpilogueSyntax,
}
impl SubprogramBodyBuilder {
    pub fn new(subprogram_body_preamble: SubprogramBodyPreambleSyntax) -> Self {
        Self {
            subprogram_body_preamble,
            declarations: DeclarationsBuilder::default().build(),
            declaration_statement_separator: DeclarationStatementSeparatorBuilder::default()
                .build(),
            concurrent_statements: ConcurrentStatementsBuilder::default().build(),
            subprogram_body_epilogue: SubprogramBodyEpilogueBuilder::default().build(),
        }
    }
    pub fn with_subprogram_body_preamble(mut self, n: SubprogramBodyPreambleSyntax) -> Self {
        self.subprogram_body_preamble = n;
        self
    }
    pub fn with_declarations(mut self, n: DeclarationsSyntax) -> Self {
        self.declarations = n;
        self
    }
    pub fn with_declaration_statement_separator(
        mut self,
        n: DeclarationStatementSeparatorSyntax,
    ) -> Self {
        self.declaration_statement_separator = n;
        self
    }
    pub fn with_concurrent_statements(mut self, n: ConcurrentStatementsSyntax) -> Self {
        self.concurrent_statements = n;
        self
    }
    pub fn with_subprogram_body_epilogue(mut self, n: SubprogramBodyEpilogueSyntax) -> Self {
        self.subprogram_body_epilogue = n;
        self
    }
    pub fn build(self) -> SubprogramBodySyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::SubprogramBody);
        builder.push_node(self.subprogram_body_preamble.raw().green().clone());
        builder.push_node(self.declarations.raw().green().clone());
        builder.push_node(self.declaration_statement_separator.raw().green().clone());
        builder.push_node(self.concurrent_statements.raw().green().clone());
        builder.push_node(self.subprogram_body_epilogue.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        SubprogramBodySyntax::cast(node).unwrap()
    }
}
pub struct SubprogramBodyEpilogueBuilder {
    end_token: Token,
    subprogram_kind: Option<SubprogramKindSyntax>,
    designator: Option<DesignatorSyntax>,
    semi_colon_token: Token,
}
impl Default for SubprogramBodyEpilogueBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl SubprogramBodyEpilogueBuilder {
    pub fn new() -> Self {
        Self {
            end_token: Token::new(
                TokenKind::Keyword(Kw::End),
                Kw::End.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            subprogram_kind: None,
            designator: None,
            semi_colon_token: Token::new(
                TokenKind::SemiColon,
                TokenKind::SemiColon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_end_token(mut self, t: Token) -> Self {
        self.end_token = t;
        self
    }
    pub fn with_subprogram_kind(mut self, n: SubprogramKindSyntax) -> Self {
        self.subprogram_kind = Some(n);
        self
    }
    pub fn with_designator(mut self, n: DesignatorSyntax) -> Self {
        self.designator = Some(n);
        self
    }
    pub fn with_semi_colon_token(mut self, t: Token) -> Self {
        self.semi_colon_token = t;
        self
    }
    pub fn build(self) -> SubprogramBodyEpilogueSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::SubprogramBodyEpilogue);
        builder.push(self.end_token);
        if let Some(n) = self.subprogram_kind {
            builder.push(n.raw().token().clone());
        }
        if let Some(n) = self.designator {
            builder.push(n.raw().token().clone());
        }
        builder.push(self.semi_colon_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        SubprogramBodyEpilogueSyntax::cast(node).unwrap()
    }
}
pub struct SubprogramBodyPreambleBuilder {
    subprogram_specification: SubprogramSpecificationSyntax,
    is_token: Token,
}
impl SubprogramBodyPreambleBuilder {
    pub fn new(subprogram_specification: SubprogramSpecificationSyntax) -> Self {
        Self {
            subprogram_specification,
            is_token: Token::new(
                TokenKind::Keyword(Kw::Is),
                Kw::Is.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_subprogram_specification(mut self, n: SubprogramSpecificationSyntax) -> Self {
        self.subprogram_specification = n;
        self
    }
    pub fn with_is_token(mut self, t: Token) -> Self {
        self.is_token = t;
        self
    }
    pub fn build(self) -> SubprogramBodyPreambleSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::SubprogramBodyPreamble);
        builder.push_node(self.subprogram_specification.raw().green().clone());
        builder.push(self.is_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        SubprogramBodyPreambleSyntax::cast(node).unwrap()
    }
}
pub struct SubprogramDeclarationBuilder {
    subprogram_specification: SubprogramSpecificationSyntax,
    semi_colon_token: Token,
}
impl SubprogramDeclarationBuilder {
    pub fn new(subprogram_specification: SubprogramSpecificationSyntax) -> Self {
        Self {
            subprogram_specification,
            semi_colon_token: Token::new(
                TokenKind::SemiColon,
                TokenKind::SemiColon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_subprogram_specification(mut self, n: SubprogramSpecificationSyntax) -> Self {
        self.subprogram_specification = n;
        self
    }
    pub fn with_semi_colon_token(mut self, t: Token) -> Self {
        self.semi_colon_token = t;
        self
    }
    pub fn build(self) -> SubprogramDeclarationSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::SubprogramDeclaration);
        builder.push_node(self.subprogram_specification.raw().green().clone());
        builder.push(self.semi_colon_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        SubprogramDeclarationSyntax::cast(node).unwrap()
    }
}
pub struct SubprogramHeaderBuilder {
    subprogram_header_generic_clause: Option<SubprogramHeaderGenericClauseSyntax>,
    generic_map_aspect: Option<GenericMapAspectSyntax>,
}
impl Default for SubprogramHeaderBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl SubprogramHeaderBuilder {
    pub fn new() -> Self {
        Self {
            subprogram_header_generic_clause: None,
            generic_map_aspect: None,
        }
    }
    pub fn with_subprogram_header_generic_clause(
        mut self,
        n: SubprogramHeaderGenericClauseSyntax,
    ) -> Self {
        self.subprogram_header_generic_clause = Some(n);
        self
    }
    pub fn with_generic_map_aspect(mut self, n: GenericMapAspectSyntax) -> Self {
        self.generic_map_aspect = Some(n);
        self
    }
    pub fn build(self) -> SubprogramHeaderSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::SubprogramHeader);
        if let Some(n) = self.subprogram_header_generic_clause {
            builder.push_node(n.raw().green().clone());
        }
        if let Some(n) = self.generic_map_aspect {
            builder.push_node(n.raw().green().clone());
        }
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        SubprogramHeaderSyntax::cast(node).unwrap()
    }
}
pub struct SubprogramHeaderGenericClauseBuilder {
    generic_token: Token,
    left_par_token: Token,
    interface_list: InterfaceListSyntax,
    right_par_token: Token,
}
impl Default for SubprogramHeaderGenericClauseBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl SubprogramHeaderGenericClauseBuilder {
    pub fn new() -> Self {
        Self {
            generic_token: Token::new(
                TokenKind::Keyword(Kw::Generic),
                Kw::Generic.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            left_par_token: Token::new(
                TokenKind::LeftPar,
                TokenKind::LeftPar.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            interface_list: InterfaceListBuilder::default().build(),
            right_par_token: Token::new(
                TokenKind::RightPar,
                TokenKind::RightPar.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_generic_token(mut self, t: Token) -> Self {
        self.generic_token = t;
        self
    }
    pub fn with_left_par_token(mut self, t: Token) -> Self {
        self.left_par_token = t;
        self
    }
    pub fn with_interface_list(mut self, n: InterfaceListSyntax) -> Self {
        self.interface_list = n;
        self
    }
    pub fn with_right_par_token(mut self, t: Token) -> Self {
        self.right_par_token = t;
        self
    }
    pub fn build(self) -> SubprogramHeaderGenericClauseSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::SubprogramHeaderGenericClause);
        builder.push(self.generic_token);
        builder.push(self.left_par_token);
        builder.push_node(self.interface_list.raw().green().clone());
        builder.push(self.right_par_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        SubprogramHeaderGenericClauseSyntax::cast(node).unwrap()
    }
}
pub struct SubprogramInstantiationDeclarationBuilder {
    subprogram_instantiation_declaration_preamble: SubprogramInstantiationDeclarationPreambleSyntax,
    generic_map_aspect: Option<GenericMapAspectSyntax>,
    semi_colon_token: Token,
}
impl SubprogramInstantiationDeclarationBuilder {
    pub fn new(
        subprogram_instantiation_declaration_preamble : SubprogramInstantiationDeclarationPreambleSyntax,
    ) -> Self {
        Self {
            subprogram_instantiation_declaration_preamble,
            generic_map_aspect: None,
            semi_colon_token: Token::new(
                TokenKind::SemiColon,
                TokenKind::SemiColon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_subprogram_instantiation_declaration_preamble(
        mut self,
        n: SubprogramInstantiationDeclarationPreambleSyntax,
    ) -> Self {
        self.subprogram_instantiation_declaration_preamble = n;
        self
    }
    pub fn with_generic_map_aspect(mut self, n: GenericMapAspectSyntax) -> Self {
        self.generic_map_aspect = Some(n);
        self
    }
    pub fn with_semi_colon_token(mut self, t: Token) -> Self {
        self.semi_colon_token = t;
        self
    }
    pub fn build(self) -> SubprogramInstantiationDeclarationSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::SubprogramInstantiationDeclaration);
        builder.push_node(
            self.subprogram_instantiation_declaration_preamble
                .raw()
                .green()
                .clone(),
        );
        if let Some(n) = self.generic_map_aspect {
            builder.push_node(n.raw().green().clone());
        }
        builder.push(self.semi_colon_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        SubprogramInstantiationDeclarationSyntax::cast(node).unwrap()
    }
}
pub struct SubprogramInstantiationDeclarationPreambleBuilder {
    subprogram_kind: SubprogramKindSyntax,
    name_token: Token,
    is_token: Token,
    new_token: Token,
    name: NameSyntax,
    signature: Option<SignatureSyntax>,
}
impl SubprogramInstantiationDeclarationPreambleBuilder {
    pub fn new(name_token: Token, subprogram_kind: SubprogramKindSyntax, name: NameSyntax) -> Self {
        Self {
            subprogram_kind,
            name_token,
            is_token: Token::new(
                TokenKind::Keyword(Kw::Is),
                Kw::Is.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            new_token: Token::new(
                TokenKind::Keyword(Kw::New),
                Kw::New.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            name,
            signature: None,
        }
    }
    pub fn with_subprogram_kind(mut self, n: SubprogramKindSyntax) -> Self {
        self.subprogram_kind = n;
        self
    }
    pub fn with_name_token(mut self, t: Token) -> Self {
        self.name_token = t;
        self
    }
    pub fn with_is_token(mut self, t: Token) -> Self {
        self.is_token = t;
        self
    }
    pub fn with_new_token(mut self, t: Token) -> Self {
        self.new_token = t;
        self
    }
    pub fn with_name(mut self, n: NameSyntax) -> Self {
        self.name = n;
        self
    }
    pub fn with_signature(mut self, n: SignatureSyntax) -> Self {
        self.signature = Some(n);
        self
    }
    pub fn build(self) -> SubprogramInstantiationDeclarationPreambleSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::SubprogramInstantiationDeclarationPreamble);
        builder.push(self.subprogram_kind.raw().token().clone());
        builder.push(self.name_token);
        builder.push(self.is_token);
        builder.push(self.new_token);
        builder.push_node(self.name.raw().green().clone());
        if let Some(n) = self.signature {
            builder.push_node(n.raw().green().clone());
        }
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        SubprogramInstantiationDeclarationPreambleSyntax::cast(node).unwrap()
    }
}
pub struct SubtypeDeclarationBuilder {
    subtype_token: Token,
    identifier_token: Token,
    is_token: Token,
    subtype_indication: SubtypeIndicationSyntax,
    semi_colon_token: Token,
}
impl SubtypeDeclarationBuilder {
    pub fn new(identifier_token: Token, subtype_indication: SubtypeIndicationSyntax) -> Self {
        Self {
            subtype_token: Token::new(
                TokenKind::Keyword(Kw::Subtype),
                Kw::Subtype.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            identifier_token,
            is_token: Token::new(
                TokenKind::Keyword(Kw::Is),
                Kw::Is.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            subtype_indication,
            semi_colon_token: Token::new(
                TokenKind::SemiColon,
                TokenKind::SemiColon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_subtype_token(mut self, t: Token) -> Self {
        self.subtype_token = t;
        self
    }
    pub fn with_identifier_token(mut self, t: Token) -> Self {
        self.identifier_token = t;
        self
    }
    pub fn with_is_token(mut self, t: Token) -> Self {
        self.is_token = t;
        self
    }
    pub fn with_subtype_indication(mut self, n: SubtypeIndicationSyntax) -> Self {
        self.subtype_indication = n;
        self
    }
    pub fn with_semi_colon_token(mut self, t: Token) -> Self {
        self.semi_colon_token = t;
        self
    }
    pub fn build(self) -> SubtypeDeclarationSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::SubtypeDeclaration);
        builder.push(self.subtype_token);
        builder.push(self.identifier_token);
        builder.push(self.is_token);
        builder.push_node(self.subtype_indication.raw().green().clone());
        builder.push(self.semi_colon_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        SubtypeDeclarationSyntax::cast(node).unwrap()
    }
}
pub struct SubtypeIndicationBuilder {
    resolution_indication: ResolutionIndicationSyntax,
    name: NameSyntax,
}
impl SubtypeIndicationBuilder {
    pub fn new(resolution_indication: ResolutionIndicationSyntax, name: NameSyntax) -> Self {
        Self {
            resolution_indication,
            name,
        }
    }
    pub fn with_resolution_indication(mut self, n: ResolutionIndicationSyntax) -> Self {
        self.resolution_indication = n;
        self
    }
    pub fn with_name(mut self, n: NameSyntax) -> Self {
        self.name = n;
        self
    }
    pub fn build(self) -> SubtypeIndicationSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::SubtypeIndication);
        builder.push_node(self.resolution_indication.raw().green().clone());
        builder.push_node(self.name.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        SubtypeIndicationSyntax::cast(node).unwrap()
    }
}
pub struct SubtypeIndicationAllocatorBuilder {
    new_token: Token,
    subtype_indication: SubtypeIndicationSyntax,
}
impl SubtypeIndicationAllocatorBuilder {
    pub fn new(subtype_indication: SubtypeIndicationSyntax) -> Self {
        Self {
            new_token: Token::new(
                TokenKind::Keyword(Kw::New),
                Kw::New.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            subtype_indication,
        }
    }
    pub fn with_new_token(mut self, t: Token) -> Self {
        self.new_token = t;
        self
    }
    pub fn with_subtype_indication(mut self, n: SubtypeIndicationSyntax) -> Self {
        self.subtype_indication = n;
        self
    }
    pub fn build(self) -> SubtypeIndicationAllocatorSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::SubtypeIndicationAllocator);
        builder.push(self.new_token);
        builder.push_node(self.subtype_indication.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        SubtypeIndicationAllocatorSyntax::cast(node).unwrap()
    }
}
pub struct SubtypeIndicationDiscreteDiscreteRangeBuilder {
    subtype_indication: SubtypeIndicationSyntax,
}
impl SubtypeIndicationDiscreteDiscreteRangeBuilder {
    pub fn new(subtype_indication: SubtypeIndicationSyntax) -> Self {
        Self { subtype_indication }
    }
    pub fn with_subtype_indication(mut self, n: SubtypeIndicationSyntax) -> Self {
        self.subtype_indication = n;
        self
    }
    pub fn build(self) -> SubtypeIndicationDiscreteDiscreteRangeSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::SubtypeIndicationDiscreteDiscreteRange);
        builder.push_node(self.subtype_indication.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        SubtypeIndicationDiscreteDiscreteRangeSyntax::cast(node).unwrap()
    }
}
pub struct SubtypeIndicationDiscreteRangeBuilder {
    range: RangeSyntax,
}
impl SubtypeIndicationDiscreteRangeBuilder {
    pub fn new(range: RangeSyntax) -> Self {
        Self { range }
    }
    pub fn with_range(mut self, n: RangeSyntax) -> Self {
        self.range = n;
        self
    }
    pub fn build(self) -> SubtypeIndicationDiscreteRangeSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::SubtypeIndicationDiscreteRange);
        builder.push_node(self.range.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        SubtypeIndicationDiscreteRangeSyntax::cast(node).unwrap()
    }
}
pub struct TimeoutClauseBuilder {
    for_token: Token,
    expression: ExpressionSyntax,
}
impl TimeoutClauseBuilder {
    pub fn new(expression: ExpressionSyntax) -> Self {
        Self {
            for_token: Token::new(
                TokenKind::Keyword(Kw::For),
                Kw::For.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            expression,
        }
    }
    pub fn with_for_token(mut self, t: Token) -> Self {
        self.for_token = t;
        self
    }
    pub fn with_expression(mut self, n: ExpressionSyntax) -> Self {
        self.expression = n;
        self
    }
    pub fn build(self) -> TimeoutClauseSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::TimeoutClause);
        builder.push(self.for_token);
        builder.push_node(self.expression.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        TimeoutClauseSyntax::cast(node).unwrap()
    }
}
pub struct TransportDelayMechanismBuilder {
    transport_token: Token,
}
impl Default for TransportDelayMechanismBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl TransportDelayMechanismBuilder {
    pub fn new() -> Self {
        Self {
            transport_token: Token::new(
                TokenKind::Keyword(Kw::Transport),
                Kw::Transport.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_transport_token(mut self, t: Token) -> Self {
        self.transport_token = t;
        self
    }
    pub fn build(self) -> TransportDelayMechanismSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::TransportDelayMechanism);
        builder.push(self.transport_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        TransportDelayMechanismSyntax::cast(node).unwrap()
    }
}
pub struct TypeConversionBuilder {
    name: NameSyntax,
    left_par_token: Token,
    expression: ExpressionSyntax,
    right_par_token: Token,
}
impl TypeConversionBuilder {
    pub fn new(name: NameSyntax, expression: ExpressionSyntax) -> Self {
        Self {
            name,
            left_par_token: Token::new(
                TokenKind::LeftPar,
                TokenKind::LeftPar.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            expression,
            right_par_token: Token::new(
                TokenKind::RightPar,
                TokenKind::RightPar.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_name(mut self, n: NameSyntax) -> Self {
        self.name = n;
        self
    }
    pub fn with_left_par_token(mut self, t: Token) -> Self {
        self.left_par_token = t;
        self
    }
    pub fn with_expression(mut self, n: ExpressionSyntax) -> Self {
        self.expression = n;
        self
    }
    pub fn with_right_par_token(mut self, t: Token) -> Self {
        self.right_par_token = t;
        self
    }
    pub fn build(self) -> TypeConversionSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::TypeConversion);
        builder.push_node(self.name.raw().green().clone());
        builder.push(self.left_par_token);
        builder.push_node(self.expression.raw().green().clone());
        builder.push(self.right_par_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        TypeConversionSyntax::cast(node).unwrap()
    }
}
pub struct UnaffectedWaveformBuilder {
    unaffected_token: Token,
}
impl Default for UnaffectedWaveformBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl UnaffectedWaveformBuilder {
    pub fn new() -> Self {
        Self {
            unaffected_token: Token::new(
                TokenKind::Keyword(Kw::Unaffected),
                Kw::Unaffected.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_unaffected_token(mut self, t: Token) -> Self {
        self.unaffected_token = t;
        self
    }
    pub fn build(self) -> UnaffectedWaveformSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::UnaffectedWaveform);
        builder.push(self.unaffected_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        UnaffectedWaveformSyntax::cast(node).unwrap()
    }
}
pub struct UnaryExpressionBuilder {
    op: UnaryOperatorSyntax,
    expression: ExpressionSyntax,
}
impl UnaryExpressionBuilder {
    pub fn new(op: UnaryOperatorSyntax, expression: ExpressionSyntax) -> Self {
        Self { op, expression }
    }
    pub fn with_op(mut self, n: UnaryOperatorSyntax) -> Self {
        self.op = n;
        self
    }
    pub fn with_expression(mut self, n: ExpressionSyntax) -> Self {
        self.expression = n;
        self
    }
    pub fn build(self) -> UnaryExpressionSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::UnaryExpression);
        builder.push(self.op.raw().token().clone());
        builder.push_node(self.expression.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        UnaryExpressionSyntax::cast(node).unwrap()
    }
}
pub struct UnboundedArrayDefinitionBuilder {
    array_token: Token,
    left_par_token: Token,
    index_subtype_definition_list: IndexSubtypeDefinitionListSyntax,
    right_par_token: Token,
    of_token: Token,
    subtype_indication: SubtypeIndicationSyntax,
}
impl UnboundedArrayDefinitionBuilder {
    pub fn new(subtype_indication: SubtypeIndicationSyntax) -> Self {
        Self {
            array_token: Token::new(
                TokenKind::Keyword(Kw::Array),
                Kw::Array.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            left_par_token: Token::new(
                TokenKind::LeftPar,
                TokenKind::LeftPar.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            index_subtype_definition_list: IndexSubtypeDefinitionListBuilder::default().build(),
            right_par_token: Token::new(
                TokenKind::RightPar,
                TokenKind::RightPar.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            of_token: Token::new(
                TokenKind::Keyword(Kw::Of),
                Kw::Of.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            subtype_indication,
        }
    }
    pub fn with_array_token(mut self, t: Token) -> Self {
        self.array_token = t;
        self
    }
    pub fn with_left_par_token(mut self, t: Token) -> Self {
        self.left_par_token = t;
        self
    }
    pub fn with_index_subtype_definition_list(
        mut self,
        n: IndexSubtypeDefinitionListSyntax,
    ) -> Self {
        self.index_subtype_definition_list = n;
        self
    }
    pub fn with_right_par_token(mut self, t: Token) -> Self {
        self.right_par_token = t;
        self
    }
    pub fn with_of_token(mut self, t: Token) -> Self {
        self.of_token = t;
        self
    }
    pub fn with_subtype_indication(mut self, n: SubtypeIndicationSyntax) -> Self {
        self.subtype_indication = n;
        self
    }
    pub fn build(self) -> UnboundedArrayDefinitionSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::UnboundedArrayDefinition);
        builder.push(self.array_token);
        builder.push(self.left_par_token);
        builder.push_node(self.index_subtype_definition_list.raw().green().clone());
        builder.push(self.right_par_token);
        builder.push(self.of_token);
        builder.push_node(self.subtype_indication.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        UnboundedArrayDefinitionSyntax::cast(node).unwrap()
    }
}
pub struct UnitDeclarationsBuilder {
    units_token: Token,
    primary_unit_declaration: PrimaryUnitDeclarationSyntax,
    secondary_unit_declarations: Vec<SecondaryUnitDeclarationSyntax>,
}
impl UnitDeclarationsBuilder {
    pub fn new(primary_unit_declaration: PrimaryUnitDeclarationSyntax) -> Self {
        Self {
            units_token: Token::new(
                TokenKind::Keyword(Kw::Units),
                Kw::Units.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            primary_unit_declaration,
            secondary_unit_declarations: Vec::new(),
        }
    }
    pub fn with_units_token(mut self, t: Token) -> Self {
        self.units_token = t;
        self
    }
    pub fn with_primary_unit_declaration(mut self, n: PrimaryUnitDeclarationSyntax) -> Self {
        self.primary_unit_declaration = n;
        self
    }
    pub fn add_secondary_unit_declarations(mut self, n: SecondaryUnitDeclarationSyntax) -> Self {
        self.secondary_unit_declarations.push(n);
        self
    }
    pub fn build(self) -> UnitDeclarationsSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::UnitDeclarations);
        builder.push(self.units_token);
        builder.push_node(self.primary_unit_declaration.raw().green().clone());
        for n in self.secondary_unit_declarations {
            builder.push_node(n.raw().green().clone());
        }
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        UnitDeclarationsSyntax::cast(node).unwrap()
    }
}
pub struct UseClauseBuilder {
    use_token: Token,
    name_list: NameListSyntax,
    semi_colon_token: Token,
}
impl Default for UseClauseBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl UseClauseBuilder {
    pub fn new() -> Self {
        Self {
            use_token: Token::new(
                TokenKind::Keyword(Kw::Use),
                Kw::Use.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            name_list: NameListBuilder::default().build(),
            semi_colon_token: Token::new(
                TokenKind::SemiColon,
                TokenKind::SemiColon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_use_token(mut self, t: Token) -> Self {
        self.use_token = t;
        self
    }
    pub fn with_name_list(mut self, n: NameListSyntax) -> Self {
        self.name_list = n;
        self
    }
    pub fn with_semi_colon_token(mut self, t: Token) -> Self {
        self.semi_colon_token = t;
        self
    }
    pub fn build(self) -> UseClauseSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::UseClause);
        builder.push(self.use_token);
        builder.push_node(self.name_list.raw().green().clone());
        builder.push(self.semi_colon_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        UseClauseSyntax::cast(node).unwrap()
    }
}
pub struct UseClauseContextItemBuilder {
    use_clause: UseClauseSyntax,
}
impl Default for UseClauseContextItemBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl UseClauseContextItemBuilder {
    pub fn new() -> Self {
        Self {
            use_clause: UseClauseBuilder::default().build(),
        }
    }
    pub fn with_use_clause(mut self, n: UseClauseSyntax) -> Self {
        self.use_clause = n;
        self
    }
    pub fn build(self) -> UseClauseContextItemSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::UseClauseContextItem);
        builder.push_node(self.use_clause.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        UseClauseContextItemSyntax::cast(node).unwrap()
    }
}
pub struct UseClauseDeclarationBuilder {
    use_clause: UseClauseSyntax,
}
impl Default for UseClauseDeclarationBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl UseClauseDeclarationBuilder {
    pub fn new() -> Self {
        Self {
            use_clause: UseClauseBuilder::default().build(),
        }
    }
    pub fn with_use_clause(mut self, n: UseClauseSyntax) -> Self {
        self.use_clause = n;
        self
    }
    pub fn build(self) -> UseClauseDeclarationSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::UseClauseDeclaration);
        builder.push_node(self.use_clause.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        UseClauseDeclarationSyntax::cast(node).unwrap()
    }
}
pub struct VariableDeclarationBuilder {
    variable_token: Token,
    identifier_list: IdentifierListSyntax,
    colon_token: Token,
    subtype_indication: SubtypeIndicationSyntax,
    colon_eq_token: Option<Token>,
    expression: Option<ExpressionSyntax>,
    semi_colon_token: Token,
}
impl VariableDeclarationBuilder {
    pub fn new(
        identifier_list: IdentifierListSyntax,
        subtype_indication: SubtypeIndicationSyntax,
    ) -> Self {
        Self {
            variable_token: Token::new(
                TokenKind::Keyword(Kw::Variable),
                Kw::Variable.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            identifier_list,
            colon_token: Token::new(
                TokenKind::Colon,
                TokenKind::Colon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            subtype_indication,
            colon_eq_token: None,
            expression: None,
            semi_colon_token: Token::new(
                TokenKind::SemiColon,
                TokenKind::SemiColon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_variable_token(mut self, t: Token) -> Self {
        self.variable_token = t;
        self
    }
    pub fn with_identifier_list(mut self, n: IdentifierListSyntax) -> Self {
        self.identifier_list = n;
        self
    }
    pub fn with_colon_token(mut self, t: Token) -> Self {
        self.colon_token = t;
        self
    }
    pub fn with_subtype_indication(mut self, n: SubtypeIndicationSyntax) -> Self {
        self.subtype_indication = n;
        self
    }
    pub fn with_colon_eq_token(mut self, t: Token) -> Self {
        self.colon_eq_token = Some(t);
        self
    }
    pub fn with_expression(mut self, n: ExpressionSyntax) -> Self {
        self.expression = Some(n);
        self
    }
    pub fn with_semi_colon_token(mut self, t: Token) -> Self {
        self.semi_colon_token = t;
        self
    }
    pub fn build(self) -> VariableDeclarationSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::VariableDeclaration);
        builder.push(self.variable_token);
        builder.push_node(self.identifier_list.raw().green().clone());
        builder.push(self.colon_token);
        builder.push_node(self.subtype_indication.raw().green().clone());
        if let Some(t) = self.colon_eq_token {
            builder.push(t);
        }
        if let Some(n) = self.expression {
            builder.push_node(n.raw().green().clone());
        }
        builder.push(self.semi_colon_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        VariableDeclarationSyntax::cast(node).unwrap()
    }
}
pub struct VerificationUnitBindingIndicationBuilder {
    use_token: Token,
    vunit_token: Token,
    verification_unit_list: VerificationUnitListSyntax,
}
impl Default for VerificationUnitBindingIndicationBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl VerificationUnitBindingIndicationBuilder {
    pub fn new() -> Self {
        Self {
            use_token: Token::new(
                TokenKind::Keyword(Kw::Use),
                Kw::Use.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            vunit_token: Token::new(
                TokenKind::Keyword(Kw::Vunit),
                Kw::Vunit.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            verification_unit_list: VerificationUnitListBuilder::default().build(),
        }
    }
    pub fn with_use_token(mut self, t: Token) -> Self {
        self.use_token = t;
        self
    }
    pub fn with_vunit_token(mut self, t: Token) -> Self {
        self.vunit_token = t;
        self
    }
    pub fn with_verification_unit_list(mut self, n: VerificationUnitListSyntax) -> Self {
        self.verification_unit_list = n;
        self
    }
    pub fn build(self) -> VerificationUnitBindingIndicationSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::VerificationUnitBindingIndication);
        builder.push(self.use_token);
        builder.push(self.vunit_token);
        builder.push_node(self.verification_unit_list.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        VerificationUnitBindingIndicationSyntax::cast(node).unwrap()
    }
}
pub struct VerificationUnitListBuilder {
    names: Vec<NameSyntax>,
    comma_token: Vec<Token>,
}
impl Default for VerificationUnitListBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl VerificationUnitListBuilder {
    pub fn new() -> Self {
        Self {
            names: Vec::new(),
            comma_token: Vec::new(),
        }
    }
    pub fn add_names(mut self, n: NameSyntax) -> Self {
        self.names.push(n);
        self
    }
    pub fn add_comma_token(mut self, t: Token) -> Self {
        self.comma_token.push(t);
        self
    }
    pub fn build(self) -> VerificationUnitListSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::VerificationUnitList);
        for n in self.names {
            builder.push_node(n.raw().green().clone());
        }
        for t in self.comma_token {
            builder.push(t);
        }
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        VerificationUnitListSyntax::cast(node).unwrap()
    }
}
pub struct WaitStatementBuilder {
    label: LabelSyntax,
    wait_token: Token,
    sensitivity_clause: Option<SensitivityClauseSyntax>,
    condition_clause: Option<ConditionClauseSyntax>,
    timeout_clause: Option<TimeoutClauseSyntax>,
    semi_colon_token: Token,
}
impl WaitStatementBuilder {
    pub fn new(label: LabelSyntax) -> Self {
        Self {
            label,
            wait_token: Token::new(
                TokenKind::Keyword(Kw::Wait),
                Kw::Wait.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            sensitivity_clause: None,
            condition_clause: None,
            timeout_clause: None,
            semi_colon_token: Token::new(
                TokenKind::SemiColon,
                TokenKind::SemiColon.canonical_text().unwrap(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
        }
    }
    pub fn with_label(mut self, n: LabelSyntax) -> Self {
        self.label = n;
        self
    }
    pub fn with_wait_token(mut self, t: Token) -> Self {
        self.wait_token = t;
        self
    }
    pub fn with_sensitivity_clause(mut self, n: SensitivityClauseSyntax) -> Self {
        self.sensitivity_clause = Some(n);
        self
    }
    pub fn with_condition_clause(mut self, n: ConditionClauseSyntax) -> Self {
        self.condition_clause = Some(n);
        self
    }
    pub fn with_timeout_clause(mut self, n: TimeoutClauseSyntax) -> Self {
        self.timeout_clause = Some(n);
        self
    }
    pub fn with_semi_colon_token(mut self, t: Token) -> Self {
        self.semi_colon_token = t;
        self
    }
    pub fn build(self) -> WaitStatementSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::WaitStatement);
        builder.push_node(self.label.raw().green().clone());
        builder.push(self.wait_token);
        if let Some(n) = self.sensitivity_clause {
            builder.push_node(n.raw().green().clone());
        }
        if let Some(n) = self.condition_clause {
            builder.push_node(n.raw().green().clone());
        }
        if let Some(n) = self.timeout_clause {
            builder.push_node(n.raw().green().clone());
        }
        builder.push(self.semi_colon_token);
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        WaitStatementSyntax::cast(node).unwrap()
    }
}
pub struct WaveformElementBuilder {
    expression: ExpressionSyntax,
    after_token: Token,
    time_expression: ExpressionSyntax,
}
impl WaveformElementBuilder {
    pub fn new(expression: ExpressionSyntax, time_expression: ExpressionSyntax) -> Self {
        Self {
            expression,
            after_token: Token::new(
                TokenKind::Keyword(Kw::After),
                Kw::After.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            time_expression,
        }
    }
    pub fn with_expression(mut self, n: ExpressionSyntax) -> Self {
        self.expression = n;
        self
    }
    pub fn with_after_token(mut self, t: Token) -> Self {
        self.after_token = t;
        self
    }
    pub fn with_time_expression(mut self, n: ExpressionSyntax) -> Self {
        self.time_expression = n;
        self
    }
    pub fn build(self) -> WaveformElementSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::WaveformElement);
        builder.push_node(self.expression.raw().green().clone());
        builder.push(self.after_token);
        builder.push_node(self.time_expression.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        WaveformElementSyntax::cast(node).unwrap()
    }
}
pub struct WaveformElementsBuilder {
    waveform_elements: Vec<WaveformElementSyntax>,
    comma_token: Vec<Token>,
}
impl Default for WaveformElementsBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl WaveformElementsBuilder {
    pub fn new() -> Self {
        Self {
            waveform_elements: Vec::new(),
            comma_token: Vec::new(),
        }
    }
    pub fn add_waveform_elements(mut self, n: WaveformElementSyntax) -> Self {
        self.waveform_elements.push(n);
        self
    }
    pub fn add_comma_token(mut self, t: Token) -> Self {
        self.comma_token.push(t);
        self
    }
    pub fn build(self) -> WaveformElementsSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::WaveformElements);
        for n in self.waveform_elements {
            builder.push_node(n.raw().green().clone());
        }
        for t in self.comma_token {
            builder.push(t);
        }
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        WaveformElementsSyntax::cast(node).unwrap()
    }
}
pub struct WhileIterationSchemeBuilder {
    while_token: Token,
    expression: ExpressionSyntax,
}
impl WhileIterationSchemeBuilder {
    pub fn new(expression: ExpressionSyntax) -> Self {
        Self {
            while_token: Token::new(
                TokenKind::Keyword(Kw::While),
                Kw::While.canonical_text(),
                Trivia::from([TriviaPiece::Spaces(1)]),
            ),
            expression,
        }
    }
    pub fn with_while_token(mut self, t: Token) -> Self {
        self.while_token = t;
        self
    }
    pub fn with_expression(mut self, n: ExpressionSyntax) -> Self {
        self.expression = n;
        self
    }
    pub fn build(self) -> WhileIterationSchemeSyntax {
        let mut builder = NodeBuilder::new();
        builder.start_node(NodeKind::WhileIterationScheme);
        builder.push(self.while_token);
        builder.push_node(self.expression.raw().green().clone());
        builder.end_node();
        let green = builder.end();
        let node = SyntaxNode::new_root(green);
        WhileIterationSchemeSyntax::cast(node).unwrap()
    }
}
