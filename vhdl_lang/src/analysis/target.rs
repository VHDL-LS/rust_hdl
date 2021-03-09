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

    pub fn resolve_target_name(
        &self,
        region: &Region<'_>,
        name_pos: &SrcPos,
        name: &mut Name,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult<Option<NamedEntities>> {
        match name {
            Name::Selected(prefix, suffix) => {
                suffix.clear_reference();

                match self.resolve_target_name(
                    region,
                    &prefix.pos,
                    &mut prefix.item,
                    diagnostics,
                )? {
                    Some(NamedEntities::Single(ref named_entity)) => {
                        match self.lookup_selected(&prefix.pos, named_entity, suffix) {
                            Ok(Some(visible)) => {
                                suffix.set_reference(&visible);
                                Ok(Some(visible))
                            }
                            Ok(None) => Ok(None),
                            Err(err) => {
                                err.add_to(diagnostics)?;
                                Ok(None)
                            }
                        }
                    }
                    Some(NamedEntities::Overloaded(..)) => Ok(None),
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
                    Ok(visible) => {
                        designator.set_reference(&visible);
                        Ok(Some(visible))
                    }
                    Err(diagnostic) => {
                        diagnostics.push(diagnostic);
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
                diagnostics.push(Diagnostic::error(name_pos, "Invalid assignment target"));
                Ok(None)
            }
            Name::FunctionCall(ref mut fcall) => {
                self.analyze_function_call(region, fcall, diagnostics)?;
                Ok(None)
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
    ) -> FatalResult<Option<Arc<NamedEntity>>> {
        let resolved_name = self.resolve_target_name(region, target_pos, target, diagnostics)?;

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
        NamedEntityKind::UnknownAlias => true,
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
