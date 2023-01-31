use super::analyze::*;
use super::named_entity::*;
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
    pub fn resolve_target(
        &self,
        scope: &Scope<'a>,
        target: &mut WithPos<Target>,
        assignment_type: AssignmentType,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> EvalResult<TypeEnt<'a>> {
        match target.item {
            Target::Name(ref mut name) => {
                self.resolve_target_name(scope, name, &target.pos, assignment_type, diagnostics)
            }
            Target::Aggregate(ref mut assocs) => {
                self.analyze_aggregate(scope, assocs, diagnostics)?;
                Err(EvalError::Unknown)
            }
        }
    }

    pub fn resolve_target_name(
        &self,
        scope: &Scope<'a>,
        target: &mut Name,
        target_pos: &SrcPos,
        assignment_type: AssignmentType,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> EvalResult<TypeEnt<'a>> {
        let object_name = self.resolve_object_name(
            scope,
            target_pos,
            target,
            "may not be the target of an assignment",
            diagnostics,
        )?;
        if !is_valid_assignment_target(&object_name.base) {
            diagnostics.push(Diagnostic::error(
                target_pos,
                format!(
                    "{} may not be the target of an assignment",
                    object_name.base.describe_class()
                ),
            ));
        } else if !is_valid_assignment_type(&object_name.base, assignment_type) {
            diagnostics.push(Diagnostic::error(
                target_pos,
                format!(
                    "{} may not be the target of a {} assignment",
                    object_name.base.describe_class(),
                    assignment_type.to_str()
                ),
            ));
        }
        Ok(object_name.type_mark())
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
    fn to_str(self) -> &'static str {
        match self {
            AssignmentType::Signal => "signal",
            AssignmentType::Variable => "variable",
        }
    }
}

/// Check that the assignment target is a writable object and not constant or input only
fn is_valid_assignment_target(base: &ObjectBase) -> bool {
    base.class() != ObjectClass::Constant && !matches!(base.mode(), Some(Mode::In))
}

// Check that a signal is not the target of a variable assignment and vice-versa
fn is_valid_assignment_type(base: &ObjectBase, assignment_type: AssignmentType) -> bool {
    let class = base.class();
    match assignment_type {
        AssignmentType::Signal => matches!(class, ObjectClass::Signal),
        AssignmentType::Variable => {
            matches!(class, ObjectClass::Variable | ObjectClass::SharedVariable)
        }
    }
}
