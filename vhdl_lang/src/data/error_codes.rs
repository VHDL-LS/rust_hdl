use crate::{Diagnostic, Severity, SrcPos};

#[derive(PartialEq, Debug, Clone, Eq, Hash)]
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
    /// No architecture was found for an entity while instantiating that entity.
    ///
    /// # Example
    /// ```vhdl
    /// entity foo is
    /// end foo;
    ///
    /// architecture bar of foo is
    /// begin
    /// end bar;
    ///
    /// -- In some other library
    /// foo_inst: entity work.foo(baz)
    ///                           ~~~ No architecture named 'baz' for entity 'foo'
    /// ```
    NoArchForEnt,
    /// No primary unit exists within a library
    ///
    /// # Example
    /// ```vhdl
    /// entity foo is
    /// end entity foo;
    ///
    /// architecture baz of bar is
    /// begin               ~~~ Does not exist within the current library
    /// end architecture;
    /// ```
    NoPrimaryUnit,
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
    InvalidTypeConversion,
    NoFunctionAccepting,
    /// There are multiple functions that a call could address.
    /// All methods to disambiguate are exhausted.
    ///
    /// # Example
    /// ```vhdl
    /// function foo return integer;
    /// function foo return character;
    /// function bar(arg: integer) return integer;
    /// function bar(arg: character) return integer;
    /// constant baz: integer := myfun(f1);
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
    /// The interface mode of a formal parameter of a function does not match the declared more
    ///
    /// # Example
    /// ```vhdl
    /// function foo(signal a: integer) return bit;
    /// constant bar: bit := foo(a => 1);
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
    ///
    /// # Example
    /// ```vhdl
    /// architecture foo of bar is
    ///     variable baz : bit;
    /// begin
    /// end architecture;
    /// ```
    DeclarationNotAllowed,
    /// An element of certain kind cannot be aliased
    ///
    /// # Example
    /// ```vhdl
    /// library ieee;
    ///
    /// alias ieee2 : bit is ieee;
    /// ```
    CannotBeAliased,
    /// An element is not an attribute when attaching an attribute to a certain kind // TODO
    ///
    /// # Example
    /// ```vhdl
    /// architecture foo of bar is
    ///     signal bar: bit;
    ///     attribute foo of bar : signal is '1';
    /// begin
    /// end architecture;
    /// ```
    NotAnAttribute,
    /// The class of an attribute does not match the declared
    ///
    /// # Example
    /// ```vhdl
    /// architecture foo of bar is
    ///     attribute bar : bit;
    ///     attribute bar of foo : signal is '1';
    /// begin
    /// end architecture;
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
    ///
    /// # Example
    /// ```vhdl
    ///
    /// ```
    NoOverloadedWithSignature,
    /// A prefix should only have a signature for subprograms and enum literals
    ShouldNotHaveSignature,
    /// A signature is required to disambiguate
    SignatureRequired,
    /// The value of an expression is ambiguous
    AmbiguousExpression,
    /// An object may not be the target of an assignment
    ///
    /// # Example
    /// ```vhdl
    /// architecture a of ent is
    ///   signal foo : boolean;
    /// begin
    ///   main : process
    ///   begin
    ///     foo'stable := 1;
    ///   end process;
    /// end architecture;
    /// ```
    IllegalTarget,
    /// No declaration of some designator was found
    NotDeclared,
    /// A declaration was already declared previously
    DuplicateDeclaration,
    /// A designator is hidden by a conflicting use clause
    ConflictingUseClause,
    /// A protected type that does not have a body
    MissingProtectedBodyType,
    /// A deferred constant is not allowed in the given context
    DeferredConstantNotAllowed,
    /// An invalid named entity was selected
    InvalidSelected,
    /// The signature between an uninstantiated subprogram and it's instantiated
    /// counterpart does not match
    SignatureMismatch,
    /// When instantiating an uninstantiated subprogram, no distinct subprogram is available
    AmbiguousInstantiation,
    /// Attempt to instantiate something that is not an uninstantiated subprogram
    NotInstantiable,
    /// Instantiating a function as procedure or vice-versa
    MismatchedInstantiationType,
    /// Function returns without a value
    VoidReturn,
    /// Procedure returns with value
    NonVoidReturn,
    /// Return statement in processes
    IllegalReturn,
    /// Exit call outside a loop
    ExitOutsideLoop,
    /// Next call outside a loop
    NextOutsideLoop,
    /// Got something other than a loop label when expecting a loop label
    MismatchedLoopLabel,
    /// A loop label was found where it shouldn't be
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
    /// Could not resolve a call to a function
    Unresolved,
    /// A prefix is invalid in a certain context
    InvalidPrefix,
    /// An index that is out of range for an N-Dimensional array
    IndexOutOfRange,
    /// A literal that cannot be assigned to its target type
    InvalidLiteral,
    /// An Architecture was declared before an entity
    ArchBeforeEntity,
    /// A configuration was found that is not in the same library as the entity
    ConfigNotInSameLibrary,
    /// Library not found
    NoSuchLibrary,
    /// Something should be a selected name but isn't
    NotASelectedName,
    /// No implicit conversion is possible
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
