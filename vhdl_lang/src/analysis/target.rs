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

    pub fn analyze_target_name(
        &self,
        region: &Region<'_>,
        target: &mut Name,
        target_pos: &SrcPos,
        assignment_type: AssignmentType,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult<Option<Arc<NamedEntity>>> {
        let resolved_name = self.resolve_name(region, target_pos, target, diagnostics)?;

        if let Some(resolved_name) = resolved_name {
            match resolved_name {
                NamedEntities::Overloaded(..) => {
                    // Only non-overloaded names may be the target of an assignment
                    diagnostics.push(Diagnostic::error(
                        target_pos,
                        "not a valid assignment target",
                    ));
                    Ok(None)
                }
                NamedEntities::Single(ent) => {
                    if !is_valid_assignment_target(&ent) {
                        diagnostics.push(Diagnostic::error(
                            target_pos,
                            format!("{} may not be the target of an assignment", ent.describe()),
                        ));
                        Ok(None)
                    } else if !is_valid_assignment_type(&ent, assignment_type) {
                        diagnostics.push(Diagnostic::error(
                            target_pos,
                            format!(
                                "{} may not be the target of a {} assignment",
                                ent.describe(),
                                assignment_type.to_str()
                            ),
                        ));
                        Ok(None)
                    } else {
                        Ok(Some(ent))
                    }
                }
            }
        } else {
            Ok(None)
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
    match ent.as_actual().kind() {
        NamedEntityKind::Object(object) => {
            object.class != ObjectClass::Constant && !matches!(object.mode, Some(Mode::In))
        }
        // @TODO allow record element declarations for now,
        //       should check their parent object in the future instead
        NamedEntityKind::ElementDeclaration(..) => true,
        NamedEntityKind::OtherAlias => true,
        _ => false,
    }
}

// Check that a signal is not the target of a variable assignment and vice-versa
fn is_valid_assignment_type(ent: &NamedEntity, assignment_type: AssignmentType) -> bool {
    let class = match ent.as_actual().kind() {
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
