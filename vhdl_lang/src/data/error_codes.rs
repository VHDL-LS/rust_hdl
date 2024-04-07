use crate::{Diagnostic, Severity, SrcPos};
use enum_map::{enum_map, Enum, EnumMap};
use std::fmt::{Display, Formatter};
use std::ops::{Index, IndexMut};
use strum::{EnumString, IntoStaticStr};

#[derive(PartialEq, Debug, Clone, Copy, Eq, Hash, EnumString, IntoStaticStr, Enum)]
#[strum(serialize_all = "snake_case")]
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
    SyntaxError,

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
    CircularDependency,

    /// A formal parameter is invalid / malformed in a certain context
    ///
    /// # Example
    /// ```vhdl
    /// constant x : bit := foo(b.all => '0');
    /// ```
    InvalidFormal,

    /// Invalid conversion from a formal parameter
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

    /// Issued when two types don't match in a context where they should.
    ///
    /// # Example
    /// ```vhdl
    /// constant x : integer := 'a';
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
    NamedBeforePositional,

    /// When calling a function, more arguments are passed to that function than it can accept.
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
    /// For example, in an architecture declarative part,
    /// `signal`s, `constant`s or `shared variable`s can be declared.
    /// However, variables may not be declared in that context.
    ///
    /// # Example
    /// ```vhdl
    /// architecture foo of bar is
    ///     variable baz : bit;
    /// begin
    /// end architecture;
    /// ```
    DeclarationNotAllowed,

    /// The entity class of an attribute does not match the declared
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
    MisplacedAttributeSpec,

    /// There is no overloaded function with an explicitly provided signature available
    ///
    /// # Example
    ///
    /// ```vhdl
    /// function foo return natural;
    /// attribute bar : natural;
    ///
    /// attribute bar of foo[return boolean] : function is 0;
    /// ```
    NoOverloadedWithSignature,

    /// An explicit signature is used in a context where no signature is expected.
    ///
    /// # Example
    /// ```vhdl
    /// type enum_t is (alpha, beta);
    /// alias alias_t is enum_t[return integer];
    /// ```
    IllegalSignature,

    /// A signature is required to disambiguate
    ///
    /// # Example
    /// ```vhdl
    /// procedure foo(arg: natural);
    /// alias bar is subpgm;
    /// ```
    SignatureRequired,

    /// The value of an expression is ambiguous
    AmbiguousExpression,

    /// A declaration was already declared previously
    ///
    /// # Example
    /// ```vhdl
    /// constant foo: bit := '0';
    /// constant foo: bit := '1';
    /// ```
    Duplicate,

    /// A designator is hidden by a conflicting use clause
    ConflictingUseClause,

    /// A protected type that does not have a body
    ///
    /// # Example
    ///
    /// ```vhdl
    /// type a1 is protected
    /// end protected;
    ///
    /// -- No `type a1 is protected body ... follows`
    /// ```
    MissingProtectedBodyType,

    /// A deferred constant is not allowed in the given context
    IllegalDeferredConstant,

    /// The signature between an uninstantiated subprogram and it's instantiated
    /// counterpart does not match
    ///
    /// # Example
    /// ```vhdl
    /// procedure foo
    ///     generic (type T)
    ///     parameter (x : bit)
    /// is begin
    /// end foo;
    ///
    /// procedure proc is new foo [bit, bit];
    /// ```
    SignatureMismatch,

    /// When instantiating an uninstantiated subprogram, no distinct subprogram is available
    AmbiguousInstantiation,

    /// Instantiating a function as procedure or vice-versa
    MismatchedSubprogramInstantiation,

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
    InvalidLoopLabel,

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
    Unused,

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

    // Misc
    /// An internal error that signifies that some precondition within vhdl_lang wasn't met.
    /// If an error with this error code occurs,
    /// please file an issue at https://github.com/VHDL-LS/rust_hdl/issues
    Internal,

    /// A related error message. This error code is never generated directly and only used
    /// as 'drop-in' when related messages are drained from a bigger error message
    Related,
}

/// The `SeverityMap` maps error codes to severities.
///
/// Implementations for `Index` and `IndexMut` are provided, so elements within the map can
/// be accessed using the `[]` operator.
/// The value returned by indexing into the severity map has the following meaning:
/// * If the value is `Some(Severity)`,
///   a diagnostic with the given error code should be displayed with that severity
/// * If the value is `None`, a diagnostic with that severity should not be displayed
#[derive(Clone, PartialEq, Eq, Debug, Copy)]
pub struct SeverityMap {
    // Using an `EnumMap` ensures that each error code is mapped to exactly one severity.
    // Additionally, this allows efficient implementation using an array internally.
    inner: EnumMap<ErrorCode, Option<Severity>>,
}

impl Default for SeverityMap {
    fn default() -> Self {
        use ErrorCode::*;
        use Severity::*;
        let map = enum_map! {
            SyntaxError
            | CircularDependency
            | InvalidFormal
            | InvalidFormalConversion
            | TypeMismatch
            | AmbiguousCall
            | NamedBeforePositional
            | TooManyArguments
            | Unassociated
            | AlreadyAssociated
            | InterfaceModeMismatch
            | DisallowedInSensitivityList
            | DeclarationNotAllowed
            | MismatchedEntityClass
            | MisplacedAttributeSpec
            | NoOverloadedWithSignature
            | IllegalSignature
            | SignatureRequired
            | AmbiguousExpression
            | Duplicate
            | ConflictingUseClause
            | MissingProtectedBodyType
            | IllegalDeferredConstant
            | SignatureMismatch
            | AmbiguousInstantiation
            | MismatchedSubprogramInstantiation
            | VoidReturn
            | NonVoidReturn
            | IllegalReturn
            | ExitOutsideLoop
            | NextOutsideLoop
            | InvalidLoopLabel
            | MismatchedKinds
            | TooManyConstraints
            | TooFewConstraints
            | IllegalConstraint
            | InvalidOperatorSymbol
            | Unresolved
            | DimensionMismatch
            | InvalidLiteral
            | DeclaredBefore
            | ConfigNotInSameLibrary
            | NoImplicitConversion
            | ExpectedSubAggregate
            | IllegalAttribute
            | CannotBePrefixed
            | NonScalarInRange
            | UnexpectedSignature
            | MissingDeferredDeclaration
            | MissingFullTypeDeclaration
            | InvalidCall => Some(Error),
            Unused
            | UnnecessaryWorkLibrary
            | UnassociatedContext => Some(Warning),
            Internal => Some(Error),
            Related => Some(Hint)
        };
        SeverityMap { inner: map }
    }
}

impl Index<ErrorCode> for SeverityMap {
    type Output = Option<Severity>;

    fn index(&self, key: ErrorCode) -> &Self::Output {
        self.inner.index(key)
    }
}

impl IndexMut<ErrorCode> for SeverityMap {
    fn index_mut(&mut self, key: ErrorCode) -> &mut Self::Output {
        self.inner.index_mut(key)
    }
}

impl ErrorCode {
    pub fn as_str(&self) -> &str {
        self.into()
    }
}

#[test]
fn serialize_from_string() {
    assert_eq!(
        ErrorCode::try_from("void_return"),
        Ok(ErrorCode::VoidReturn)
    );
    assert_eq!(
        ErrorCode::try_from("misplaced_attribute_spec"),
        Ok(ErrorCode::MisplacedAttributeSpec)
    );
    assert_eq!(
        ErrorCode::try_from("syntax_error"),
        Ok(ErrorCode::SyntaxError)
    );
    assert_eq!(
        ErrorCode::try_from("not_an_error_code"),
        Err(strum::ParseError::VariantNotFound)
    );
}

#[test]
fn serialize_to_string() {
    assert_eq!(ErrorCode::VoidReturn.as_str(), "void_return");
    assert_eq!(
        ErrorCode::MisplacedAttributeSpec.as_str(),
        "misplaced_attribute_spec"
    );
    assert_eq!(ErrorCode::SyntaxError.as_str(), "syntax_error");
}

/// Specialized diagnostics with pre-defined messages and error codes
impl Diagnostic {
    pub fn syntax_error(item: impl AsRef<SrcPos>, msg: impl Into<String>) -> Diagnostic {
        Self::new(item, msg, ErrorCode::SyntaxError)
    }

    pub fn circular_dependency(item: impl AsRef<SrcPos>) -> Diagnostic {
        Self::new(
            item,
            "Found circular dependency",
            ErrorCode::CircularDependency,
        )
    }

    pub fn internal(item: impl AsRef<SrcPos>, msg: impl Into<String>) -> Diagnostic {
        Self::new(item, msg, ErrorCode::Internal)
    }
}

impl Display for ErrorCode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}
