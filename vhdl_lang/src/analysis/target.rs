use super::analyze::*;
use super::region::*;
use crate::ast::*;
use crate::data::*;
use std::sync::Arc;

/// Analysis of assignment targets
///
/// examples:
///   target <= 1;
///   target(0).elem := 1

impl<'a> AnalyzeContext<'a> {
    pub fn analyze_target(
        &self,
        parent: &Region<'_>,
        target: &mut WithPos<Target>,
        assignment_type: AssignmentType,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        match target.item {
            Target::Name(ref mut name) => {
                self.analyze_target_name(parent, name, &target.pos, assignment_type, diagnostics)?;
                Ok(())
            }
            Target::Aggregate(ref mut assocs) => {
                self.analyze_aggregate(parent, assocs, diagnostics)?;
                Ok(())
            }
        }
    }

    fn resolve_target_name(
        &self,
        region: &Region<'_>,
        name_pos: &SrcPos,
        name: &mut Name,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult<Option<ResolvedTargetName>> {
        match name {
            Name::Selected(prefix, suffix) => {
                suffix.clear_reference();

                match self.resolve_target_name(
                    region,
                    &prefix.pos,
                    &mut prefix.item,
                    diagnostics,
                )? {
                    Some(resolved) => {
                        match self.lookup_selected(&prefix.pos, resolved.leaf(), suffix) {
                            Ok(Some(NamedEntities::Single(named_entity))) => {
                                suffix.set_unique_reference(&named_entity);
                                Ok(Some(resolved.with_leaf(named_entity)))
                            }
                            Ok(Some(NamedEntities::Overloaded(..))) => {
                                diagnostics.push(invalid_assignment_target(name_pos));
                                Ok(None)
                            }
                            Ok(None) => Ok(None),
                            Err(err) => {
                                err.add_to(diagnostics)?;
                                Ok(None)
                            }
                        }
                    }
                    None => Ok(None),
                }
            }

            Name::SelectedAll(prefix) => {
                self.resolve_target_name(region, &prefix.pos, &mut prefix.item, diagnostics)?;
                Ok(None)
            }
            Name::Designator(designator) => {
                designator.clear_reference();

                match region.lookup_within(name_pos, designator.designator()) {
                    Ok(NamedEntities::Single(named_entity)) => {
                        designator.set_unique_reference(&named_entity);
                        Ok(Some(ResolvedTargetName::new(named_entity)))
                    }
                    Ok(NamedEntities::Overloaded(..)) => {
                        diagnostics.push(invalid_assignment_target(name_pos));
                        Ok(None)
                    }
                    Err(diag) => {
                        diagnostics.push(diag);
                        Ok(None)
                    }
                }
            }
            Name::Indexed(ref mut prefix, ref mut exprs) => {
                self.resolve_target_name(region, &prefix.pos, &mut prefix.item, diagnostics)?;
                for expr in exprs.iter_mut() {
                    self.analyze_expression(region, expr, diagnostics)?;
                }
                Ok(None)
            }

            Name::Slice(ref mut prefix, ref mut drange) => {
                self.resolve_target_name(region, &prefix.pos, &mut prefix.item, diagnostics)?;
                self.analyze_discrete_range(region, drange.as_mut(), diagnostics)?;
                Ok(None)
            }
            Name::Attribute(..) => {
                diagnostics.push(invalid_assignment_target(name_pos));
                Ok(None)
            }

            // Convert an association list to a list of indexed
            // During parsing function calls and indexed names are ambiguous
            // Thus we convert function calls to indexd names during the analysis stage
            Name::FunctionCall(ref mut fcall) => {
                let FunctionCall {
                    name: prefix,
                    parameters,
                } = fcall.as_mut();

                if let Some(indexes) = assoc_elems_to_indexes(parameters) {
                    *name = Name::Indexed(Box::new(prefix.clone()), indexes);
                    self.resolve_target_name(region, name_pos, name, diagnostics)
                } else {
                    diagnostics.push(invalid_assignment_target(name_pos));
                    Ok(None)
                }
            }
            Name::External(ref mut ename) => {
                let ExternalName { subtype, .. } = ename.as_mut();
                self.analyze_subtype_indication(region, subtype, diagnostics)?;
                Ok(None)
            }
        }
    }

    pub fn analyze_target_name(
        &self,
        region: &Region<'_>,
        target: &mut Name,
        target_pos: &SrcPos,
        assignment_type: AssignmentType,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        let resolved_name = self.resolve_target_name(region, target_pos, target, diagnostics)?;

        if let Some(resolved_name) = resolved_name {
            if let Some(object) = resolved_name.object() {
                if !is_valid_assignment_target(object) {
                    diagnostics.push(Diagnostic::error(
                        target_pos,
                        format!(
                            "{} may not be the target of an assignment",
                            object.describe()
                        ),
                    ));
                } else if !is_valid_assignment_type(object, assignment_type) {
                    diagnostics.push(Diagnostic::error(
                        target_pos,
                        format!(
                            "{} may not be the target of a {} assignment",
                            object.describe(),
                            assignment_type.to_str()
                        ),
                    ));
                }
            }
            // @TODO alias of record element is a false positive since we only store record element declaration
            // example:
            // alias foo is rec.field;
            // foo := 1;
        }

        Ok(())
    }
}

enum ResolvedTargetName {
    // The object which is the true target of the assignment
    // Example:

    // This shared_var shared variable record is the true assignment target
    // lib.pkg.shared_var.field := 1

    // lib, lib.pkg
    BeforeObject(Arc<NamedEntity>),

    // lib.pkg.shared_var
    Object(Arc<NamedEntity>),

    // lib.pkg.shared_var.field
    AfterObject(Arc<NamedEntity>, Arc<NamedEntity>),
}

impl ResolvedTargetName {
    fn new(ent: Arc<NamedEntity>) -> Self {
        if ent.actual_kind().is_object() {
            Self::Object(ent)
        } else {
            Self::BeforeObject(ent)
        }
    }

    fn with_leaf(self, ent: Arc<NamedEntity>) -> Self {
        if ent.actual_kind().is_object() {
            debug_assert!(matches!(self, Self::BeforeObject(_)));
            Self::Object(ent)
        } else {
            match self {
                Self::BeforeObject(_) => Self::BeforeObject(ent),
                Self::Object(object) => Self::AfterObject(object, ent),
                Self::AfterObject(object, _) => Self::AfterObject(object, ent),
            }
        }
    }

    fn leaf(&self) -> &NamedEntity {
        match self {
            Self::BeforeObject(ref ent) => ent,
            Self::Object(object) => object,
            Self::AfterObject(_, ent) => ent,
        }
    }

    fn object(&self) -> Option<&NamedEntity> {
        match self {
            Self::BeforeObject(_) => None,
            Self::Object(object) => Some(object),
            Self::AfterObject(object, _) => Some(object),
        }
    }
}

#[derive(Copy, Clone)]
pub enum AssignmentType {
    // Assignement with <=
    Signal,
    // Assignment with :=
    Variable,
}

impl AssignmentType {
    fn to_str(&self) -> &str {
        match self {
            AssignmentType::Signal => "signal",
            AssignmentType::Variable => "variable",
        }
    }
}

/// Check that the assignment target is a writable object and not constant or input only
fn is_valid_assignment_target(ent: &NamedEntity) -> bool {
    match ent.actual_kind() {
        NamedEntityKind::Object(object) => {
            object.class != ObjectClass::Constant && !matches!(object.mode, Some(Mode::In))
        }
        // @TODO allow record element declarations for now,
        //       should check their parent object in the future instead
        NamedEntityKind::ElementDeclaration(..) => true,
        NamedEntityKind::UnknownAlias => true,
        _ => false,
    }
}

// Check that a signal is not the target of a variable assignment and vice-versa
fn is_valid_assignment_type(ent: &NamedEntity, assignment_type: AssignmentType) -> bool {
    let class = match ent.actual_kind() {
        NamedEntityKind::Object(object) => object.class,
        _ => {
            // Other entity kinds are not relevant for this check
            return true;
        }
    };

    match assignment_type {
        AssignmentType::Signal => matches!(class, ObjectClass::Signal),
        AssignmentType::Variable => {
            matches!(class, ObjectClass::Variable | ObjectClass::SharedVariable)
        }
    }
}

fn assoc_elem_to_index(assoc_elem: AssociationElement) -> Option<WithPos<Expression>> {
    if assoc_elem.formal.is_some() {
        return None;
    }

    match assoc_elem.actual.item {
        ActualPart::Open => None,
        ActualPart::Expression(expr) => Some(WithPos::new(expr, assoc_elem.actual.pos)),
    }
}

fn assoc_elems_to_indexes(
    assoc_elems: &mut Vec<AssociationElement>,
) -> Option<Vec<WithPos<Expression>>> {
    let mut result: Vec<WithPos<Expression>> = Vec::with_capacity(assoc_elems.len());

    for elem in assoc_elems.drain(..) {
        if let Some(expr) = assoc_elem_to_index(elem) {
            result.push(expr);
        } else {
            return None;
        }
    }

    Some(result)
}

fn invalid_assignment_target(pos: &SrcPos) -> Diagnostic {
    Diagnostic::error(pos, "Invalid assignment target")
}
