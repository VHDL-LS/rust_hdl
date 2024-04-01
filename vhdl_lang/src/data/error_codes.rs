use crate::{Diagnostic, Severity, SrcPos};
use std::fmt::{Display, Formatter};

#[derive(PartialEq, Debug, Clone, Copy, Eq, Hash)]
pub enum ErrorCode {
    /// A syntax error happens during tokenization or parsing.
    ///
    /// # Example
    /// ```vhdl
    /// entity foo is
    ///     port (
    ///         clk: in bit;
    ///                    ^ Last interface element may not end with ';'
    ///     );
    /// end entity;
    /// ```
    SyntaxError = 0,
    // Analysis
    /// A circular dependency was found where one module depends on another module which
    /// (directly or indirectly) again depends on the first module.
    ///
    /// # Example
    /// ```vhdl
    /// use work.bar;
    ///
    /// package foo is
    /// end package;
    ///
    /// use work.foo;
    ///
    /// package bar is
    /// end package;
    /// ```
    CircularDependency = 1,
    /// A formal parameter is invalid / malformed in a certain context
    ///
    /// # Example
    /// ```vhdl
    /// constant x : bit := foo(b.all => '0');
    /// ```
    InvalidFormal,
    /// Converting from a formal parameter is invalid
    ///
    /// # Example
    /// ```vhdl
    /// entity foo is
    ///     port (
    ///         bar: out natural
    ///     );
    /// end entity;
    ///
    /// -- When instantiating foo
    /// function fun1(arg : natural) return natural;
    ///
    /// foo_inst: entity work.foo
    /// port map (
    ///     fun1(arg => bar) => sig
    /// );
    /// ```
    InvalidFormalConversion,
    /// Something of type A cannot be converted to type B
    ///
    /// # Example
    /// ```vhdl
    /// constant x : integer := integer('a');
    /// ```
    TypeMismatch,
    /// There are multiple functions that a call could address.
    /// All methods to disambiguate are exhausted.
    ///
    /// # Example
    /// ```vhdl
    /// function foo return integer;
    /// function foo return character;
    /// function bar(arg: integer) return integer;
    /// function bar(arg: character) return integer;
    /// constant baz: integer := bar(foo);
    /// ```
    AmbiguousCall,
    /// Named arguments appear before positional arguments when calling a function
    ///
    /// # Example
    /// ```vhdl
    /// function foo(a, b: integer) return bit;
    /// constant bar: bit := foo(a => 1, 2);
    /// ```
    NamedArgumentsBeforePositional,
    /// More arguments than are expected are passed to a function
    ///
    /// # Example
    /// ```vhdl
    /// function foo(a, b: integer) return bit;
    /// constant bar: bit := foo(1, 2, 3);
    /// ```
    TooManyArguments,
    /// A formal parameter wasn't associated in a function call
    ///
    /// # Example
    /// ```vhdl
    /// function foo(a, b: integer) return bit;
    /// constant bar: bit := foo(a => 1);
    /// ```
    Unassociated,
    /// A formal element has already been associated
    ///
    /// # Example
    /// ```vhdl
    /// function foo(a: integer) return bit;
    /// constant bar: bit := foo(a => 1, a => 2);
    /// ```
    AlreadyAssociated,
    /// The interface mode of a formal parameter (i.e., `signal`, `variable`, ...)
    /// of a function does not match the declared more
    ///
    /// # Example
    /// ```vhdl
    /// function foo(signal a: integer) return bit;
    /// constant bar: bit := foo(a => 1); -- a must be associated to a signal, not a constant
    /// ```
    InterfaceModeMismatch,
    /// An element is not allowed inside a sensitivity list
    ///
    /// # Example
    /// ```vhdl
    /// architecture foo of bar is
    /// shared variable x : bit;
    /// begin
    ///
    /// process (x)
    /// begin
    /// end process;
    ///
    /// end architecture;
    /// ```
    DisallowedInSensitivityList,
    /// A declaration is not allowed in a certain context.
    /// For example, variables cannot be declared in an architecture declarative part
    /// `signal`s, `constant`s or `shared variable`s could be declared, however.
    ///
    /// # Example
    /// ```vhdl
    /// architecture foo of bar is
    ///     variable baz : bit;
    /// begin
    /// end architecture;
    /// ```
    DeclarationNotAllowed,
    /// The class of an attribute does not match the declared
    ///
    /// # Example
    /// ```vhdl
    /// signal bad : boolean;
    /// attribute foo : boolean;
    /// attribute foo of bad : variable is true; -- should be signal, not variable
    /// ```
    MismatchedEntityClass,
    /// The attribute specification is not in the immediate declarative part
    ///
    /// # Example
    /// ```vhdl
    /// entity bar is
    ///     port (
    ///         a : in bit
    ///     );
    /// end entity;
    ///
    /// architecture foo of bar is
    ///     attribute baz : string;
    ///     attribute baz of a : signal is "foobar";
    /// begin
    /// end architecture;
    /// ```
    AttributeSpecNotInImmediateDeclarativePart,
    /// There is no overloaded function with the provided signature available
    NoOverloadedWithSignature,
    /// A prefix should only have a signature for subprograms and enum literals
    ShouldNotHaveSignature,
    /// A signature is required to disambiguate
    SignatureRequired,
    /// The value of an expression is ambiguous
    AmbiguousExpression,
    /// A declaration was already declared previously
    DuplicateDeclaration,
    /// A designator is hidden by a conflicting use clause
    ConflictingUseClause,
    /// A protected type that does not have a body
    MissingProtectedBodyType,
    /// A deferred constant is not allowed in the given context
    DeferredConstantNotAllowed,
    /// The signature between an uninstantiated subprogram and it's instantiated
    /// counterpart does not match
    SignatureMismatch,
    /// When instantiating an uninstantiated subprogram, no distinct subprogram is available
    AmbiguousInstantiation,
    /// Instantiating a function as procedure or vice-versa
    MismatchedInstantiationType,
    /// Function returns without a value
    VoidReturn,
    /// Procedure returns with value
    NonVoidReturn,
    /// Illegal return statement, for example in a process
    ///
    /// # Example
    ///
    /// ```vhdl
    /// process (clk)
    /// begin
    ///     if rising_edge(clk) then
    ///         return;
    ///     end if;
    /// end process;
    /// ```
    IllegalReturn,
    /// Exit statement called outside a loop
    ExitOutsideLoop,
    /// Next statement called outside a loop
    NextOutsideLoop,
    /// A loop label was found at a position where it shouldn't be
    ///
    /// # Example
    /// ```vhdl
    /// bad0: loop
    /// end loop;
    ///
    /// loop
    ///     exit bad0;
    /// end loop;
    /// ```
    InvalidLoopLabelPosition,
    /// A call to an uninstantiated subprogram was made
    UninstantiatedSubprogramCall,
    /// Got something (a named entity such as a type, procedure, e.t.c.)
    /// while expecting another thing. For example, got something that names a procedure while
    /// expecting a type name.
    ///
    /// This is different from a type error. When a type error occurs,
    /// the kinds already match.
    MismatchedKinds,
    /// An extra index constraint is present
    TooManyConstraints,
    /// There are not enough constraints
    TooFewConstraints,
    /// A constraint cannot be used for a given type
    IllegalConstraint,
    /// A string or symbol was used in a context where an operator was expected but there
    /// is no operator for that string.
    InvalidOperatorSymbol,
    /// An unresolved name was used
    ///
    /// # Example
    /// ```vhdl
    ///  -- There is nothing named 'bar' in this scope
    /// variable foo: integer = bar;
    /// ```
    Unresolved,
    /// An index that is out of range for an N-Dimensional array
    DimensionMismatch,
    /// A literal that cannot be assigned to its target type
    InvalidLiteral,
    /// A Design Unit (such as an architecture) was declared before another
    ///Design Unit (such as an entity) which is illegal.
    DeclaredBefore,
    /// A configuration was found that is not in the same library as the entity
    ConfigNotInSameLibrary,
    /// No implicit conversion using the `??` operator is possible
    NoImplicitConversion,
    /// Expected sub-aggregate
    ExpectedSubAggregate,
    /// An attribute was used on an element that it cannot be used on
    IllegalAttribute,
    /// Something cannot be prefixed
    CannotBePrefixed,
    /// A non-scalar is used in a range
    NonScalarInRange,
    /// A signature appeared that was not expected
    UnexpectedSignature,
    /// A deferred constant is missing its full constant declaration in the package body
    MissingDeferredDeclaration,
    /// A deferred type declaration is missing its full declaration
    MissingFullTypeDeclaration,
    /// Calling a name like a function or procedure where that is not applicable
    InvalidCall,
    // Linting
    /// A declaration that is unused
    UnusedDeclaration = 4096,
    /// The declaration
    /// ```vhdl
    /// library work;
    /// ```
    /// was made.
    UnnecessaryWorkLibrary,
    /// A context clause that is not associated to a design unit
    ///
    /// # Example
    /// ```vhdl
    /// library ieee;
    /// use ieee.std_logic_1164.all;
    ///
    /// -- End of file
    /// ```
    UnassociatedContext,
    /// An internal error that signifies that some precondition within vhdl_lang wasn't met.
    Internal = -1,
    /// A related error message. This error code is never generated directly and only used
    /// as 'drop-in' when related messages are drained from a bigger error message
    Related = -2,
}

/// Specialized diagnostics with pre-defined messages and error codes
impl Diagnostic {
    pub fn syntax_error(item: impl AsRef<SrcPos>, msg: impl Into<String>) -> Diagnostic {
        Self::new(item, msg, Severity::Error, ErrorCode::SyntaxError)
    }

    pub fn circular_dependency(item: impl AsRef<SrcPos>) -> Diagnostic {
        Self::new(
            item,
            "Found circular dependency",
            Severity::Error,
            ErrorCode::CircularDependency,
        )
    }

    pub fn internal(item: impl AsRef<SrcPos>, msg: impl Into<String>) -> Diagnostic {
        Self::new(item, msg, Severity::Error, ErrorCode::Internal)
    }
}

impl Display for ErrorCode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "E{:0>3}", *self as u32)
    }
}
