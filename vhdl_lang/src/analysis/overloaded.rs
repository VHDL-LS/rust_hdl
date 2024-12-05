// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2023, Olof Kraigher olof.kraigher@gmail.com

use fnv::FnvHashSet;
use vhdl_lang::TokenAccess;

use super::analyze::*;
use super::expression::ExpressionType;
use super::scope::*;
use crate::ast::search::clear_references;
use crate::ast::token_range::WithToken;
use crate::ast::*;
use crate::data::error_codes::ErrorCode;
use crate::data::*;
use crate::named_entity::*;

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
#[derive(Copy, Clone)]
pub enum SubprogramKind<'a> {
    Function(Option<TypeEnt<'a>>),
    Procedure,
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
        ctx: &dyn TokenAccess,
        name: &WithToken<Designator>,
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
                        return Err(Diagnostic::new(
                            name.pos(ctx),
                            format!("'{}' does not match {}", name, ttyp.describe()),
                            ErrorCode::TypeMismatch,
                        ));
                    }
                }
            }

            let (err_prefix, code) = if rejected.len() == 1 {
                // Provide better error for unique function name
                ("Invalid call to", ErrorCode::InvalidCall)
            } else {
                ("Could not resolve", ErrorCode::Unresolved)
            };

            let mut diag = Diagnostic::new(name.pos(ctx), format!("{err_prefix} '{name}'"), code);

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

#[derive(Clone)]
pub(super) struct ResolvedCall<'a> {
    pub subpgm: OverloadedEnt<'a>,
    pub formals: Vec<TypeEnt<'a>>,
}

impl<'a> AsRef<OverloadedEnt<'a>> for OverloadedEnt<'a> {
    fn as_ref(&self) -> &OverloadedEnt<'a> {
        self
    }
}

impl<'a> AsRef<OverloadedEnt<'a>> for ResolvedCall<'a> {
    fn as_ref(&self) -> &OverloadedEnt<'a> {
        &self.subpgm
    }
}

impl<'a> AnalyzeContext<'a, '_> {
    /// Typecheck one overloaded call where the exact subprogram is known
    pub fn check_call(
        &self,
        scope: &Scope<'a>,
        error_pos: &SrcPos,
        ent: OverloadedEnt<'a>,
        assocs: &mut [AssociationElement],
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        self.check_association(error_pos, ent.formals(), scope, assocs, diagnostics)?;
        Ok(())
    }

    fn disambiguate_by_kind(candidates: &mut Vec<OverloadedEnt<'a>>, kind: SubprogramKind<'a>) {
        match kind {
            SubprogramKind::Function(_) => candidates.retain(|cand| cand.is_function()),
            SubprogramKind::Procedure => candidates.retain(|cand| cand.is_procedure()),
        };
    }

    fn disambiguate_by_assoc_formals(
        &self,
        scope: &Scope<'a>,
        call_pos: &SrcPos,
        candidates: &[OverloadedEnt<'a>],
        assocs: &mut [AssociationElement],
    ) -> EvalResult<Vec<ResolvedCall<'a>>> {
        let mut result = Vec::with_capacity(candidates.len());
        for ent in candidates.iter() {
            if let Some(resolved) = as_fatal(self.resolve_association_formals(
                call_pos,
                ent.formals(),
                scope,
                assocs,
                &mut NullDiagnostics,
            ))? {
                result.push(ResolvedCall {
                    subpgm: *ent,
                    formals: resolved,
                });
            }

            for elem in assocs.iter_mut() {
                clear_references(elem, self.ctx);
            }
        }

        Ok(result)
    }

    fn actual_types(
        &self,
        scope: &Scope<'a>,
        assocs: &mut [AssociationElement],
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> EvalResult<Vec<Option<ExpressionType<'a>>>> {
        let mut actual_types = Vec::with_capacity(assocs.len());

        for assoc in assocs.iter_mut() {
            match &mut assoc.actual.item {
                ActualPart::Expression(expr) => {
                    let actual_type =
                        self.expr_pos_type(scope, assoc.actual.span, expr, diagnostics)?;
                    actual_types.push(Some(actual_type));
                }
                ActualPart::Open => {
                    actual_types.push(None);
                }
            }
        }

        Ok(actual_types)
    }

    #[allow(clippy::too_many_arguments)]
    pub fn disambiguate(
        &self,
        scope: &Scope<'a>,
        call_pos: &SrcPos,                 // The position of the entire call
        call_name: &WithToken<Designator>, // The position and name of the subprogram name in the call
        assocs: &mut [AssociationElement],
        kind: SubprogramKind<'a>,
        all_overloaded: Vec<OverloadedEnt<'a>>,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> EvalResult<Disambiguated<'a>> {
        if all_overloaded.len() == 1 {
            let ent = all_overloaded[0];
            self.check_call(scope, call_pos, ent, assocs, diagnostics)?;
            return Ok(Disambiguated::Unambiguous(ent));
        }

        let mut ok_kind = all_overloaded.clone();
        Self::disambiguate_by_kind(&mut ok_kind, kind);

        // Does not need disambiguation
        if ok_kind.len() == 1 {
            let ent = ok_kind[0];
            self.check_call(scope, call_pos, ent, assocs, diagnostics)?;
            return Ok(Disambiguated::Unambiguous(ent));
        } else if ok_kind.is_empty() {
            diagnostics.push(Diagnostic::ambiguous(self.ctx, call_name, all_overloaded));
            return Err(EvalError::Unknown);
        }

        // Disambiguate based on uninstantiated subprogram
        ok_kind.retain(|ent| !ent.is_uninst_subprogram());

        if ok_kind.len() == 1 {
            let ent = ok_kind[0];
            self.check_call(scope, call_pos, ent, assocs, diagnostics)?;
            return Ok(Disambiguated::Unambiguous(ent));
        } else if ok_kind.is_empty() {
            diagnostics.add(
                call_name.pos(self.ctx),
                format!("uninstantiated subprogram {} cannot be called", call_name),
                ErrorCode::InvalidCall,
            );
            return Err(EvalError::Unknown);
        }

        let ok_formals = self.disambiguate_by_assoc_formals(scope, call_pos, &ok_kind, assocs)?;

        // Only one candidate matched actual/formal profile
        if ok_formals.len() == 1 {
            let ent = ok_formals[0].subpgm;
            self.check_call(scope, call_pos, ent, assocs, diagnostics)?;
            return Ok(Disambiguated::Unambiguous(ent));
        } else if ok_formals.is_empty() {
            // No candidate matched actual/formal profile
            diagnostics.push(Diagnostic::ambiguous(self.ctx, call_name, ok_kind));
            return Err(EvalError::Unknown);
        }

        let actual_types = self.actual_types(scope, assocs, diagnostics)?;

        let mut ok_assoc_types = ok_formals.clone();
        self.implicit_matcher()
            .disambiguate_by_assoc_types(&actual_types, &mut ok_assoc_types);

        if ok_assoc_types.len() == 1 {
            let ent = ok_assoc_types[0].subpgm;
            self.check_call(scope, call_pos, ent, assocs, diagnostics)?;
            return Ok(Disambiguated::Unambiguous(ent));
        } else if ok_assoc_types.is_empty() {
            diagnostics.push(Diagnostic::ambiguous(
                self.ctx,
                call_name,
                ok_formals.into_iter().map(|resolved| resolved.subpgm),
            ));
            return Err(EvalError::Unknown);
        }

        let ok_return_type = if let SubprogramKind::Function(rtyp) = kind {
            let mut ok_return_type = ok_assoc_types.clone();
            self.any_matcher()
                .disambiguate_op_by_return_type(&mut ok_return_type, rtyp);

            // Only one candidate matches type profile, check it
            if ok_return_type.len() == 1 {
                let ent = ok_return_type[0].subpgm;
                self.check_call(scope, call_pos, ent, assocs, diagnostics)?;
                return Ok(Disambiguated::Unambiguous(ent));
            } else if ok_return_type.is_empty() {
                diagnostics.push(Diagnostic::ambiguous(
                    self.ctx,
                    call_name,
                    ok_assoc_types.into_iter().map(|resolved| resolved.subpgm),
                ));
                return Err(EvalError::Unknown);
            }
            ok_return_type
        } else {
            ok_assoc_types
        };

        let mut strict_ok_assoc_types = ok_return_type.clone();
        self.strict_matcher()
            .disambiguate_by_assoc_types(&actual_types, &mut strict_ok_assoc_types);

        if strict_ok_assoc_types.len() == 1 {
            let ent = strict_ok_assoc_types[0].subpgm;
            self.check_call(scope, call_pos, ent, assocs, diagnostics)?;
            return Ok(Disambiguated::Unambiguous(ent));
        } else if strict_ok_assoc_types.is_empty() {
            // Do not disambiguate away to emtpy result
            strict_ok_assoc_types.clone_from(&ok_return_type);
        }

        Ok(Disambiguated::Ambiguous(
            strict_ok_assoc_types
                .into_iter()
                .map(|resolved| resolved.subpgm)
                .collect(),
        ))
    }

    pub fn disambiguate_no_actuals(
        &self,
        name: &WithToken<Designator>,
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

        Ok(Some(candidates.finish(self.ctx, name, ttyp)?))
    }
}

impl Diagnostic {
    fn ambiguous<'a>(
        ctx: &dyn TokenAccess,
        name: &WithToken<Designator>,
        rejected: impl IntoIterator<Item = OverloadedEnt<'a>>,
    ) -> Self {
        let mut diag = Diagnostic::new(
            name.pos(ctx),
            format!("Could not resolve call to '{}'", name.item.designator()),
            ErrorCode::AmbiguousCall,
        );
        diag.add_subprogram_candidates("Does not match", rejected);
        diag
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::analysis::overloaded;
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
                WithToken::new(des.item.clone(), fcall.span.start_token)
            } else {
                panic!("Expected designator")
            };

            let overloaded = if let NamedEntities::Overloaded(overloaded) =
                self.scope.lookup(&des.item).unwrap()
            {
                overloaded
            } else {
                panic!("Expected overloaded")
            };

            as_fatal(self.ctx(&code.tokenize()).disambiguate(
                &self.scope,
                &fcall.pos(&code.tokenize()),
                &des,
                &mut fcall.item.parameters.items,
                overloaded::SubprogramKind::Function(ttyp),
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
            vec![Diagnostic::new(
                call.s1("'c'"),
                "character literal does not match integer type 'INTEGER'",
                ErrorCode::TypeMismatch,
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
                Diagnostic::new(
                    fcall.s1("missing"),
                    "No declaration of 'missing'",
                    ErrorCode::Unresolved,
                ),
                Diagnostic::new(
                    fcall,
                    "No association of parameter 'arg1'",
                    ErrorCode::Unassociated,
                )
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
            vec![Diagnostic::new(
                fcall.s1("myfun"),
                "Could not resolve call to 'myfun'",
                ErrorCode::AmbiguousCall,
            )
            .related(
                decl.s1("myfun"),
                "Does not match function myfun[INTEGER return INTEGER]",
            )
            .related(
                decl.s("myfun", 2),
                "Does not match function myfun[INTEGER return CHARACTER]",
            )],
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
                test.lookup_overloaded(decl.s("myfun", 2)),
                test.lookup_overloaded(decl.s1("myfun")),
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
            vec![Diagnostic::new(
                fcall.s1("myfun"),
                "Could not resolve call to 'myfun'",
                ErrorCode::AmbiguousCall,
            )
            .related(
                decl.s1("myfun"),
                "Does not match function myfun[CHARACTER return INTEGER]",
            )
            .related(
                decl.s("myfun", 2),
                "Does not match function myfun[CHARACTER return CHARACTER]",
            )],
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
            vec![Diagnostic::new(
                fcall.s1("myfun"),
                "Could not resolve call to 'myfun'",
                ErrorCode::AmbiguousCall,
            )
            .related(
                decl.s1("myfun"),
                "Does not match function myfun[INTEGER return INTEGER]",
            )
            .related(
                decl.s("myfun", 2),
                "Does not match function myfun[INTEGER return CHARACTER]",
            )],
        )
    }

    #[test]
    fn ambiguous_builtin_favors_non_implicit_conversion_unary() {
        let test = TestSetup::new();
        let code = test.snippet("to_string(0)");

        let uint_to_string = test
            .ctx(&code.tokenize())
            .universal_integer()
            .lookup_implicit_of("TO_STRING");

        assert_eq!(
            test.disambiguate(&code, None, &mut NoDiagnostics),
            Some(Disambiguated::Unambiguous(uint_to_string))
        );
    }

    #[test]
    fn ambiguous_builtin_favors_non_implicit_conversion_binary() {
        let test = TestSetup::new();
        let code = test.snippet("minimum(0, integer'(0))");

        let minimum = test
            .ctx(&code.tokenize())
            .integer()
            .lookup_implicit_of("MINIMUM");

        assert_eq!(
            test.disambiguate(&code, None, &mut NoDiagnostics),
            Some(Disambiguated::Unambiguous(minimum))
        );
    }
}
