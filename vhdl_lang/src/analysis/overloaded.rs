// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2023, Olof Kraigher olof.kraigher@gmail.com

use fnv::FnvHashSet;

use super::analyze::*;
use super::formal_region::InterfaceEnt;
use super::named_entity::*;
use super::region::*;
use crate::ast::search::clear_references;
use crate::ast::*;
use crate::data::*;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Disambiguated<'a> {
    Unambiguous(OverloadedEnt<'a>),
    Ambiguous(Vec<OverloadedEnt<'a>>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DisambiguatedType<'a> {
    Unambiguous(TypeEnt<'a>),
    Ambiguous(FnvHashSet<BaseType<'a>>),
}

// The reason a subprogram was rejected as a candidate of a call
#[derive(Debug)]
enum Rejection<'a> {
    // The return type of the subprogram is not correct
    ReturnType,

    // The subprogram is a procedure but we expected a function
    Procedure,

    // The amount of actuals or named actuals do not match formals
    MissingFormals(Vec<InterfaceEnt<'a>>),
}

struct Candidate<'a> {
    ent: OverloadedEnt<'a>,
    rejection: Option<Rejection<'a>>,
}

struct Candidates<'a>(Vec<Candidate<'a>>);

impl<'a> Candidates<'a> {
    fn new(candidates: &OverloadedName<'a>) -> Self {
        Self(
            candidates
                .entities()
                .map(|ent| Candidate {
                    ent,
                    rejection: None,
                })
                .collect(),
        )
    }

    fn remaining(&mut self) -> impl Iterator<Item = &mut Candidate<'a>> {
        self.0.iter_mut().filter(|cand| cand.rejection.is_none())
    }

    fn split(self) -> (Vec<OverloadedEnt<'a>>, Vec<Candidate<'a>>) {
        let (remaining, rejected): (Vec<_>, Vec<_>) = self
            .0
            .into_iter()
            .partition(|cand| cand.rejection.is_none());
        let remaining: Vec<_> = remaining.into_iter().map(|cand| cand.ent).collect();
        (remaining, rejected)
    }

    fn finish(
        self,
        name: &WithPos<Designator>,
        ttyp: Option<TypeEnt<'a>>,
    ) -> Result<Disambiguated<'a>, Diagnostic> {
        let (remaining, mut rejected) = self.split();

        if remaining.len() == 1 {
            Ok(Disambiguated::Unambiguous(remaining[0]))
        } else if !remaining.is_empty() {
            Ok(Disambiguated::Ambiguous(remaining))
        } else {
            if let [ref single] = rejected.as_slice() {
                if let Some(ttyp) = ttyp {
                    if matches!(single.rejection, Some(Rejection::ReturnType))
                        && matches!(single.ent.kind(), Overloaded::EnumLiteral(_))
                    {
                        // Special case to get better error for single rejected enumeration literal
                        // For example when assigning true to an integer.
                        return Err(Diagnostic::error(
                            name,
                            format!("'{}' does not match {}", name, ttyp.describe()),
                        ));
                    }
                }
            }

            let err_prefix = if rejected.len() == 1 {
                // Provide better error for unique function name
                "Invalid call to"
            } else {
                "Could not resolve"
            };

            let mut diag = Diagnostic::error(name, format!("{err_prefix} '{name}'"));

            rejected.sort_by(|x, y| x.ent.decl_pos().cmp(&y.ent.decl_pos()));

            for cand in rejected {
                if let Some(decl_pos) = cand.ent.decl_pos() {
                    let rejection = cand.rejection.unwrap();

                    match rejection {
                        Rejection::ReturnType => diag.add_related(
                            decl_pos,
                            format!("Does not match return type of {}", cand.ent.describe()),
                        ),
                        Rejection::Procedure => diag.add_related(
                            decl_pos,
                            format!(
                                "Procedure {} cannot be used as a function",
                                cand.ent.describe()
                            ),
                        ),
                        Rejection::MissingFormals(missing) => {
                            for formal in missing.iter() {
                                diag.add_related(
                                    decl_pos,
                                    format!("Missing association of {}", formal.describe()),
                                )
                            }
                        }
                    };
                }
            }
            Err(diag)
        }
    }
}

impl<'a> Disambiguated<'a> {
    pub fn into_type(self) -> DisambiguatedType<'a> {
        match self {
            Disambiguated::Unambiguous(ent) => {
                DisambiguatedType::Unambiguous(ent.return_type().unwrap())
            }
            Disambiguated::Ambiguous(overloaded) => DisambiguatedType::Ambiguous(
                overloaded
                    .into_iter()
                    .map(|e| e.return_type().unwrap().base())
                    .collect(),
            ),
        }
    }
}

impl<'a> AnalyzeContext<'a> {
    /// Typecheck one overloaded call where the exact subprogram is known
    pub fn check_call(
        &self,
        scope: &Scope<'a>,
        error_pos: &SrcPos,
        ent: OverloadedEnt<'a>,
        assocs: &mut [AssociationElement],
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        self.analyze_assoc_elems_with_formal_region(
            error_pos,
            ent.formals(),
            scope,
            assocs,
            diagnostics,
        )?;
        Ok(())
    }

    #[allow(clippy::too_many_arguments)]
    pub fn disambiguate(
        &self,
        scope: &Scope<'a>,
        call_pos: &SrcPos,               // The position of the entire call
        call_name: &WithPos<Designator>, // The position and name of the subprogram name in the call
        parameters: &mut [AssociationElement],
        ttyp: Option<TypeEnt<'a>>,
        all_overloaded: Vec<OverloadedEnt<'a>>,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> EvalResult<Disambiguated<'a>> {
        // Apply target type constraint if it exists
        let overloaded = if let Some(ttyp) = ttyp {
            let mut overloaded = all_overloaded.clone();
            let tbase = ttyp.base();
            overloaded.retain(|ent| {
                if let Some(return_type) = ent.return_type() {
                    self.can_be_target_type(return_type, tbase)
                } else {
                    false
                }
            });

            overloaded
        } else {
            all_overloaded.clone()
        };

        // Does not need disambiguation
        if overloaded.len() == 1 {
            let ent = overloaded[0];
            self.check_call(scope, call_pos, ent, parameters, diagnostics)?;
            return Ok(Disambiguated::Unambiguous(ent));
        } else if overloaded.is_empty() {
            // No candidate
            let mut diag = Diagnostic::error(
                &call_name.pos,
                format!("Could not resolve call to '{}'", call_name.designator()),
            );
            diag.add_subprogram_candidates("Does not match", all_overloaded);
            diagnostics.push(diag);
            return Err(EvalError::Unknown);
        }

        let mut candidates = Vec::with_capacity(overloaded.len());
        for ent in overloaded.iter() {
            if let Some(resolved) = as_fatal(self.resolve_association_formals(
                call_pos,
                ent.formals(),
                scope,
                parameters,
                &mut NullDiagnostics,
            ))? {
                candidates.push((ent, resolved));
            }

            for elem in parameters.iter_mut() {
                clear_references(elem);
            }
        }

        // Only one candidate matched actual/formal profile
        if candidates.len() == 1 {
            let ent = *candidates[0].0;
            self.check_call(scope, call_pos, ent, parameters, diagnostics)?;
            return Ok(Disambiguated::Unambiguous(ent));
        }

        // No candidate matched actual/formal profile
        if candidates.is_empty() {
            return if overloaded.len() == 1 {
                let ent = overloaded[0];
                self.check_call(scope, call_pos, ent, parameters, diagnostics)?;
                return Ok(Disambiguated::Unambiguous(ent));
            } else {
                let mut diag = Diagnostic::error(
                    &call_name.pos,
                    format!("Could not resolve call to '{}'", call_name.designator()),
                );
                diag.add_subprogram_candidates("Does not match", overloaded);
                diagnostics.push(diag);
                Err(EvalError::Unknown)
            };
        }

        let mut actual_types = Vec::with_capacity(parameters.len());

        for assoc in parameters.iter_mut() {
            match &mut assoc.actual.item {
                ActualPart::Expression(expr) => {
                    let actual_type =
                        self.expr_pos_type(scope, &assoc.actual.pos, expr, diagnostics)?;
                    actual_types.push(Some(actual_type));
                }
                ActualPart::Open => {
                    actual_types.push(None);
                }
            }
        }

        let type_candidates: Vec<_> = candidates
            .iter()
            .cloned()
            .filter(|(_, formals)| {
                actual_types.iter().enumerate().all(|(idx, actual_type)| {
                    if let Some(actual_type) = actual_type {
                        self.is_possible(actual_type, formals[idx].type_mark().base())
                    } else {
                        true
                    }
                })
            })
            .map(|(ent, _)| *ent)
            .collect();

        // Only one candidate matches type profile, check it
        if type_candidates.len() == 1 {
            let ent = type_candidates[0];
            self.check_call(scope, call_pos, ent, parameters, diagnostics)?;
            return Ok(Disambiguated::Unambiguous(ent));
        } else if type_candidates.is_empty() {
            let mut diag = Diagnostic::error(
                &call_name.pos,
                format!("Could not resolve call to '{}'", call_name.designator()),
            );
            diag.add_subprogram_candidates("Does not match", overloaded);
            diagnostics.push(diag);
            Err(EvalError::Unknown)
        } else {
            Ok(Disambiguated::Ambiguous(type_candidates))
        }
    }

    pub fn disambiguate_no_actuals(
        &self,
        name: &WithPos<Designator>,
        ttyp: Option<TypeEnt<'a>>,
        overloaded: &OverloadedName<'a>,
    ) -> Result<Option<Disambiguated<'a>>, Diagnostic> {
        let mut candidates = Candidates::new(overloaded);

        let tbase = ttyp.map(|ttyp| ttyp.base());

        for cand in candidates.remaining() {
            if !cand.ent.signature().can_be_called_without_actuals() {
                cand.rejection = Some(Rejection::MissingFormals(
                    cand.ent.signature().formals_without_defaults().collect(),
                ));
            } else if let Some(return_type) = cand.ent.return_type() {
                if let Some(tbase) = tbase {
                    if !self.can_be_target_type(return_type, tbase) {
                        cand.rejection = Some(Rejection::ReturnType);
                    }
                }
            } else {
                cand.rejection = Some(Rejection::Procedure);
            }
        }

        Ok(Some(candidates.finish(name, ttyp)?))
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::analysis::tests::NoDiagnostics;
    use crate::analysis::tests::TestSetup;
    use crate::data::DiagnosticHandler;
    use crate::syntax::test::check_diagnostics;
    use crate::syntax::test::Code;

    impl<'a> TestSetup<'a> {
        fn disambiguate(
            &'a self,
            code: &Code,
            ttyp: Option<TypeEnt<'a>>,
            diagnostics: &mut dyn DiagnosticHandler,
        ) -> Option<Disambiguated<'a>> {
            let mut fcall = code.function_call();

            let des = if let Name::Designator(des) = &fcall.item.name.item {
                WithPos::new(des.item.clone(), fcall.item.name.pos.clone())
            } else {
                panic!("Expected designator")
            };

            let overloaded = if let NamedEntities::Overloaded(overloaded) =
                self.scope.lookup(&fcall.item.name.pos, &des.item).unwrap()
            {
                overloaded
            } else {
                panic!("Expected overloaded")
            };

            as_fatal(self.ctx().disambiguate(
                &self.scope,
                &fcall.pos,
                &des,
                &mut fcall.item.parameters,
                ttyp,
                overloaded.entities().collect(),
                diagnostics,
            ))
            .unwrap()
        }
    }

    #[test]
    fn single_fcall() {
        let test = TestSetup::new();
        let decl = test.declarative_part(
            "
function myfun return integer;
        ",
        );

        let func = test.lookup_overloaded(decl.s1("myfun"));

        assert_eq!(
            test.disambiguate(&test.snippet("myfun"), None, &mut NoDiagnostics),
            Some(Disambiguated::Unambiguous(func))
        );
    }

    #[test]
    fn single_fcall_bad_type() {
        let test = TestSetup::new();
        let decl = test.declarative_part(
            "
function myfun(arg : integer) return integer;
        ",
        );

        let mut diagnostics = Vec::new();
        let call = test.snippet("myfun('c')");
        assert_eq!(
            test.disambiguate(&call, None, &mut diagnostics),
            Some(Disambiguated::Unambiguous(
                test.lookup_overloaded(decl.s1("myfun"))
            )),
        );

        check_diagnostics(
            diagnostics,
            vec![Diagnostic::error(
                call.s1("'c'"),
                "character literal does not match integer type 'INTEGER'",
            )],
        );
    }

    #[test]
    fn disambiguate_fcall_based_on_named_association() {
        let test = TestSetup::new();
        let decl = test.declarative_part(
            "
function myfun(arg1 : integer) return integer;
function myfun(arg2 : integer) return character;
        ",
        );

        let func = test.lookup_overloaded(decl.s1("myfun(arg1").s1("myfun"));

        assert_eq!(
            test.disambiguate(&test.snippet("myfun(arg1 => 0)"), None, &mut NoDiagnostics),
            Some(Disambiguated::Unambiguous(func))
        );
    }

    #[test]
    fn disambiguate_fcall_formal_mismatch() {
        let test = TestSetup::new();
        let decl = test.declarative_part(
            "
function myfun(arg1 : integer) return integer;
        ",
        );

        let fcall = test.snippet("myfun(missing => 0)");
        let mut diagnostics = Vec::new();
        assert_eq!(
            test.disambiguate(&fcall, None, &mut diagnostics),
            Some(Disambiguated::Unambiguous(
                test.lookup_overloaded(decl.s1("myfun"))
            ))
        );
        check_diagnostics(
            diagnostics,
            vec![
                Diagnostic::error(fcall.s1("missing"), "No declaration of 'missing'"),
                Diagnostic::error(fcall, "No association of interface constant 'arg1'")
                    .related(decl.s1("arg1"), "Defined here"),
            ],
        );
    }

    #[test]
    fn disambiguate_fcall_no_candidates_matching_actuals() {
        let test = TestSetup::new();
        let decl = test.declarative_part(
            "
function myfun(arg1 : integer) return integer;
function myfun(arg2 : integer) return character;
        ",
        );

        let fcall = test.snippet("myfun");
        let mut diagnostics = Vec::new();
        assert_eq!(test.disambiguate(&fcall, None, &mut diagnostics), None);
        check_diagnostics(
            diagnostics,
            vec![
                Diagnostic::error(fcall.s1("myfun"), "Could not resolve call to 'myfun'")
                    .related(
                        decl.s1("myfun"),
                        "Does not match myfun[INTEGER return INTEGER]",
                    )
                    .related(
                        decl.s("myfun", 2),
                        "Does not match myfun[INTEGER return CHARACTER]",
                    ),
            ],
        );
    }

    #[test]
    fn disambiguate_fcall_based_on_type() {
        let test = TestSetup::new();
        let decl = test.declarative_part(
            "
function myfun(arg1 : character) return integer;
function myfun(arg1 : integer) return integer;
        ",
        );
        let fcall = test.snippet("myfun('c')");
        assert_eq!(
            test.disambiguate(&fcall, None, &mut NoDiagnostics),
            Some(Disambiguated::Unambiguous(
                test.lookup_overloaded(decl.s1("myfun"))
            ))
        );

        let fcall = test.snippet("myfun(0)");
        assert_eq!(
            test.disambiguate(&fcall, None, &mut NoDiagnostics),
            Some(Disambiguated::Unambiguous(
                test.lookup_overloaded(decl.s("myfun", 2))
            ))
        );
    }

    #[test]
    fn ambiguous_call() {
        let test = TestSetup::new();
        let decl = test.declarative_part(
            "
function myfun(arg1 : integer) return character;
function myfun(arg1 : integer) return integer;
        ",
        );
        let fcall = test.snippet("myfun(0)");
        assert_eq!(
            test.disambiguate(&fcall, None, &mut NoDiagnostics),
            Some(Disambiguated::Ambiguous(vec![
                test.lookup_overloaded(decl.s1("myfun")),
                test.lookup_overloaded(decl.s("myfun", 2))
            ]))
        );
    }

    #[test]
    fn ambiguous_call_without_match() {
        let test = TestSetup::new();
        let decl = test.declarative_part(
            "
function myfun(arg1 : character) return integer;
function myfun(arg1 : character) return character;
        ",
        );
        let fcall = test.snippet("myfun(0)");
        let mut diagnostics = Vec::new();
        assert_eq!(test.disambiguate(&fcall, None, &mut diagnostics), None);
        check_diagnostics(
            diagnostics,
            vec![
                Diagnostic::error(fcall.s1("myfun"), "Could not resolve call to 'myfun'")
                    .related(
                        decl.s1("myfun"),
                        "Does not match myfun[CHARACTER return INTEGER]",
                    )
                    .related(
                        decl.s("myfun", 2),
                        "Does not match myfun[CHARACTER return CHARACTER]",
                    ),
            ],
        );
    }

    #[test]
    fn disambiguates_target_type() {
        let test = TestSetup::new();
        let decl = test.declarative_part(
            "
function myfun(arg1 : integer) return integer;
function myfun(arg1 : integer) return character;
        ",
        );
        let fcall = test.snippet("myfun(0)");
        assert_eq!(
            test.disambiguate(
                &fcall,
                Some(test.lookup_type("integer")),
                &mut NoDiagnostics
            ),
            Some(Disambiguated::Unambiguous(
                test.lookup_overloaded(decl.s1("myfun"))
            ))
        );
    }

    #[test]
    fn target_type_no_match() {
        let test = TestSetup::new();
        let decl = test.declarative_part(
            "
function myfun(arg1 : integer) return integer;
function myfun(arg1 : integer) return character;
        ",
        );
        let fcall = test.snippet("myfun(0)");
        let mut diagnostics = Vec::new();

        assert_eq!(
            test.disambiguate(&fcall, Some(test.lookup_type("boolean")), &mut diagnostics),
            None,
        );

        check_diagnostics(
            diagnostics,
            vec![
                Diagnostic::error(fcall.s1("myfun"), "Could not resolve call to 'myfun'")
                    .related(
                        decl.s1("myfun"),
                        "Does not match myfun[INTEGER return INTEGER]",
                    )
                    .related(
                        decl.s("myfun", 2),
                        "Does not match myfun[INTEGER return CHARACTER]",
                    ),
            ],
        )
    }
}
