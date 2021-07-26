use super::analyze::*;
use super::names::*;
use super::region::*;
use crate::ast::*;
use crate::data::*;

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
    ) -> FatalNullResult {
        match self.resolve_object_prefix(
            region,
            target_pos,
            target,
            "Invalid assignment target",
            diagnostics,
        ) {
            Ok(resolved_name) => {
                if let ResolvedName::ObjectSelection {
                    ref base_object, ..
                } = resolved_name
                {
                    if !is_valid_assignment_target(base_object) {
                        diagnostics.push(Diagnostic::error(
                            target_pos,
                            format!(
                                "{} may not be the target of an assignment",
                                base_object.describe_class()
                            ),
                        ));
                    } else if !is_valid_assignment_type(base_object, assignment_type) {
                        diagnostics.push(Diagnostic::error(
                            target_pos,
                            format!(
                                "{} may not be the target of a {} assignment",
                                base_object.describe_class(),
                                assignment_type.to_str()
                            ),
                        ));
                    }
                } else {
                    diagnostics.push(Diagnostic::error(target_pos, "Invalid assignment target"));
                }
            }
            Err(err) => {
                err.add_to(diagnostics)?;
            }
        }

        Ok(())
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
    fn to_str(self) -> String {
        match self {
            AssignmentType::Signal => String::from("signal"),
            AssignmentType::Variable => String::from("variable"),
        }
    }
}

/// Check that the assignment target is a writable object and not constant or input only
fn is_valid_assignment_target(ent: &ObjectEnt) -> bool {
    let object = ent.object();
    object.class != ObjectClass::Constant && !matches!(object.mode, Some(Mode::In))
}

// Check that a signal is not the target of a variable assignment and vice-versa
fn is_valid_assignment_type(ent: &ObjectEnt, assignment_type: AssignmentType) -> bool {
    let class = ent.class();
    match assignment_type {
        AssignmentType::Signal => matches!(class, ObjectClass::Signal),
        AssignmentType::Variable => {
            matches!(class, ObjectClass::Variable | ObjectClass::SharedVariable)
        }
    }
}
