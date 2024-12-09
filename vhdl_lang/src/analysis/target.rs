use super::analyze::*;
use super::scope::*;
use crate::ast::token_range::WithTokenSpan;
use crate::ast::*;
use crate::data::error_codes::ErrorCode;
use crate::data::*;
use crate::named_entity::*;
use vhdl_lang::TokenSpan;

/// Analysis of assignment targets
///
/// examples:
///   target <= 1;
///   target(0).elem := 1
impl<'a> AnalyzeContext<'a, '_> {
    pub fn resolve_target(
        &self,
        scope: &Scope<'a>,
        target: &mut WithTokenSpan<Target>,
        assignment_type: AssignmentType,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> EvalResult<TypeEnt<'a>> {
        match target.item {
            Target::Name(ref mut name) => {
                self.resolve_target_name(scope, name, target.span, assignment_type, diagnostics)
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
        target_pos: TokenSpan,
        assignment_type: AssignmentType,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> EvalResult<TypeEnt<'a>> {
        let object_name = self.resolve_object_name(
            scope,
            target_pos,
            target,
            "may not be the target of an assignment",
            ErrorCode::MismatchedKinds,
            diagnostics,
        )?;
        if !object_name.base.can_be_assigned_to() {
            diagnostics.add(
                target_pos.pos(self.ctx),
                format!(
                    "{} may not be the target of an assignment",
                    object_name.base.describe_class()
                ),
                ErrorCode::MismatchedKinds,
            );
        } else if !object_name.base.is_valid_assignment_type(assignment_type) {
            diagnostics.add(
                target_pos.pos(self.ctx),
                format!(
                    "{} may not be the target of a {} assignment",
                    object_name.base.describe_class(),
                    assignment_type.to_str()
                ),
                ErrorCode::MismatchedKinds,
            );
        }
        Ok(object_name.type_mark())
    }
}

#[derive(Copy, Clone)]
pub enum AssignmentType {
    // Assignment with <=
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
