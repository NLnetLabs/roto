use std::{borrow, cmp, fmt, hash, ops, str};

use serde::{Serialize, Serializer};
use smallvec::SmallVec;

use routecore::asn::Asn;

use crate::compiler::error::CompileError;
use crate::first_into_compile_err;
use crate::parser::span::Spanned;
use crate::parser::{ParseError, Parser};
use crate::types::typevalue::TypeValue;

#[derive(Clone, Debug, Default)]
pub struct SyntaxTree {
    pub expressions: Vec<RootExpr>,
}

impl SyntaxTree {
    pub fn parse_str(input: &str) -> Result<Self, ParseError> {
        let tree = Parser::parse(input)?;
        if tree.expressions.is_empty() {
            return Err(ParseError::EmptyInput);
        }
        Ok(tree)
    }
}

#[derive(Debug, Clone)]
pub enum RootExpr {
    FilterMap(Box<FilterMap>),
    Rib(Rib),
    // PrefixList(PrefixListExpr),
    Table(Table),
    OutputStream(OutputStream),
    Ty(RecordTypeAssignment),
}

impl RootExpr {
    pub fn get_filter_map(&self) -> Result<&FilterMap, CompileError> {
        match self {
            Self::FilterMap(m) => Ok(m),
            _ => Err(CompileError::new("not a filter-map".into())),
        }
    }
}

/// A list of values of the same type or a list where all the values can be
/// converted to the same type
#[derive(Clone, Debug)]
pub struct ListValueExpr {
    pub values: Spanned<Vec<ValueExpr>>,
}

/// The value of a (anonymous) record
/// Defined and directly used, mainly as an argument to a method, where the
/// actual type can be inferred unambiguously.
#[derive(Clone, Debug)]
pub struct AnonymousRecordValueExpr {
    pub key_values: Spanned<Vec<(Spanned<Identifier>, ValueExpr)>>,
}

/// Used in the 'Define' section to create variables to hold a record.
#[derive(Clone, Debug)]
pub struct TypedRecordValueExpr {
    pub type_id: Spanned<TypeIdentifier>,
    pub key_values: Spanned<Vec<(Spanned<Identifier>, ValueExpr)>>,
}

/// The value of a typed record
#[derive(Clone, Debug)]
pub struct RecordTypeAssignment {
    pub ident: Spanned<TypeIdentifier>,
    pub record_type: RecordTypeIdentifier,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FilterType {
    FilterMap,
    Filter,
}

impl FilterType {
    pub fn is_filter(&self) -> bool {
        if let FilterType::Filter = self {
            return true;
        }
        false
    }
}

#[derive(Clone, Debug)]
pub struct FilterMap {
    pub ty: FilterType,
    pub ident: Spanned<Identifier>,
    pub for_ident: Option<TypeIdentField>,
    pub with_kv: Vec<TypeIdentField>,
    pub body: FilterMapBody,
}

#[derive(Clone, Debug)]
pub struct FilterMapBody {
    pub define: Define,
    pub expressions: Vec<FilterMapExpr>,
    pub apply: Option<ApplySection>,
}

/// These are the sections that can appear multiple times in a Filter(Map)
#[derive(Debug, Clone)]
pub enum FilterMapExpr {
    Term(TermSection),
    Action(ActionSection),
    // Empty, // Import(ImportBody),
}

//------------ Define -------------------------------------------------------
#[derive(Clone, Debug)]
pub struct Define {
    pub for_kv: Option<TypeIdentField>, // associated Rib record type
    pub with_kv: Vec<TypeIdentField>,   // arguments
    pub body: DefineBody,
}

#[derive(Clone, Debug)]
pub enum RxTxType {
    RxOnly(TypeIdentField),
    Split(TypeIdentField, TypeIdentField),
    PassThrough(TypeIdentField),
}

#[derive(Clone, Debug)]
pub struct DefineBody {
    pub rx_tx_type: RxTxType,
    pub use_ext_data: Vec<(Identifier, Identifier)>,
    pub assignments: Vec<(Spanned<Identifier>, ValueExpr)>,
}

#[derive(Clone, Debug)]
pub struct TermSection {
    pub ident: Identifier,
    pub for_kv: Option<TypeIdentField>,
    pub with_kv: Vec<TypeIdentField>,
    pub body: TermBody,
}

#[derive(Clone, Debug)]
pub struct TermBody {
    pub scopes: Vec<TermScope>,
}

/// Everything that can appear inside a named `term` block.
#[derive(Clone, Debug)]
pub struct TermScope {
    pub scope: Option<Identifier>,
    pub operator: MatchOperator,
    pub match_arms: Vec<(Option<TermPatternMatchArm>, Vec<LogicalExpr>)>,
}

/// A Match arm, with or without a data field. Used to capture a MatchExpr and
/// separate it from its logical expression(s), so that the TermScope (down
/// below) can capture it as an optional expression for that TermScope. so the
/// difference between a Match Expression that represents a match arm (of an
/// Enum) and a 'regular' collection of logical expressions, is just this
/// optional VariantMatchExpr.
#[derive(Clone, Debug)]
pub struct TermPatternMatchArm {
    pub variant_id: Spanned<Identifier>,
    pub data_field: Option<Spanned<Identifier>>,
}

//------------ Action -------------------------------------------------------

// 'action' Identifier '{' ActionBody '}')* ForStatement WithStatement
//  '{' ActionBody '}'

#[derive(Clone, Debug)]
pub struct ActionSection {
    pub ident: Identifier,
    // pub for_kv: Option<TypeIdentField>,
    pub with_kv: Vec<TypeIdentField>,
    pub body: ActionSectionBody,
}

#[derive(Clone, Debug)]
pub struct ActionSectionBody {
    pub expressions: Vec<ComputeExpr>,
}

/// An Optional Global Compute Expressions can be either an ordinary Compute
/// Expression, or a global method call, i.e. 'some-global-method(a, b)', so an
/// expression without any dots in it ending in a method call.
///
/// Action Expressions are always turned into regular Compute Expressions at
/// parse time (so: here), consequently there's no `eval()` for an Optional
/// GlobalComputeExpr.
#[derive(Clone, Debug)]
pub enum OptionalGlobalComputeExpr {
    GlobalMethodExpr(MethodComputeExpr),
    ComputeExpr(ComputeExpr),
}

#[derive(Clone, Debug)]
pub struct ApplySection {
    pub body: ApplyBody,
    pub for_kv: Option<TypeIdentField>,
    pub with_kv: Vec<TypeIdentField>,
}

#[derive(Clone, Debug)]
pub struct ApplyBody {
    pub scopes: Vec<ApplyScope>,
    pub accept_reject: Option<AcceptReject>,
}

#[derive(Clone, Debug)]
pub struct ApplyScope {
    pub scope: Option<Identifier>,
    pub match_action: MatchActionExpr,
}

/// the Apply section can host regular rules that bind a term to an action
/// under specified conditions. It can also host a match expression that
/// does the same for enums.
#[derive(Clone, Debug)]
pub enum MatchActionExpr {
    FilterMatchAction(FilterMatchActionExpr),
    PatternMatchAction(PatternMatchActionExpr),
}

/// A regular 'filter match` expression that binds a term to a (number of)
/// action(s).
#[derive(Clone, Debug)]
pub struct FilterMatchActionExpr {
    pub operator: MatchOperator,
    pub filter_ident: ValueExpr,
    pub negate: bool,
    pub actions: Vec<(Option<ValueExpr>, Option<AcceptReject>)>,
}

/// A complete pattern match on a variable where every match arm can have
/// multiple actions. Similar to TermMatchActionExpr, but a PatternMatchAction
/// can only take actions in its body, no Logic Expressions.
#[derive(Clone, Debug)]
pub struct PatternMatchActionExpr {
    /// The data field of the MatchOperator is the identifier of the variable
    /// to be matched on
    pub operator: MatchOperator,
    /// All the match arms appearing in the source code, with an optional
    /// guard, i.e. a condition on this variant.
    pub match_arms: Vec<PatternMatchActionArm>,
}

/// An invocation of an action, used in the Apply section only, it consists of
/// the name of the actions plus an optional arguments list of variable names,
/// whose values are to be passed in at runtime. The Action definition should
/// have all the variables defined in a `with` statement.
///
/// The fields of this struct are the same as `MethodComputeExpr`, but it gets
/// treated differently at eval time.
#[derive(Clone, Debug)]
pub struct ActionCallExpr {
    pub action_id: Spanned<Identifier>,
    pub args: Option<ArgExprList>,
}

/// The same as the ActionCallExpr and the MethodCallExpr, but for its
/// treatment by the evaluator.
#[derive(Clone, Debug)]
pub struct TermCallExpr {
    pub term_id: Spanned<Identifier>,
    pub args: Option<ArgExprList>,
}

#[derive(Clone, Debug)]
pub struct PatternMatchActionArm {
    pub variant_id: Spanned<Identifier>,
    pub data_field: Option<Spanned<Identifier>>,
    pub guard: Option<TermCallExpr>,
    pub actions: Vec<(Option<ActionCallExpr>, Option<AcceptReject>)>,
}

/// A TermMatchExpr describes a variant of an enum together with its data field
/// and one or more logical expressions, that will evaluate to a boolean, it
/// may reference the data field. Note that this MatchExpr will be split out in
/// (variant_id, data_field) and the logical expressions to be able to store it
/// in a TermScope as `VariantMatchExpr`s. Since it only store Logical
/// Expressions it is only fit for use in a Term section. In the Apply sections
/// the PatternMatchActionExpr is used.
#[derive(Clone, Debug)]
pub struct TermMatchExpr {
    pub variant_id: Identifier,
    pub data_field: Option<Identifier>,
    pub logical_expr: Vec<LogicalExpr>,
}

//------------ Rib -----------------------------------------------------------

// Rib ::= "rib" Identifier 'contains' TypeIdentifier '{' RibBody '}'

#[derive(Clone, Debug)]
pub struct Rib {
    pub ident: Identifier,
    pub contain_ty: Spanned<TypeIdentifier>,
    pub body: RibBody,
}

#[derive(Clone, Debug)]
pub struct RibBody {
    pub key_values: Spanned<Vec<RibField>>,
}

#[derive(Clone, Debug)]
pub enum RibField {
    PrimitiveField(TypeIdentField),
    RecordField(Box<(Spanned<Identifier>, RecordTypeIdentifier)>),
    ListField(Box<(Spanned<Identifier>, ListTypeIdentifier)>),
}

#[derive(Clone, Debug)]
pub struct Table {
    pub ident: Identifier,
    pub contain_ty: Spanned<TypeIdentifier>,
    pub body: RibBody,
}

#[derive(Clone, Debug)]
pub struct OutputStream {
    pub ident: Identifier,
    pub contain_ty: Spanned<TypeIdentifier>,
    pub body: RibBody,
}

/// An identifier is the name of variables or other things.
///
/// It is a word composed of a leading alphabetic Unicode character, followed
/// by alphanumeric Unicode characters or underscore or hyphen.
#[derive(Clone, Debug, Ord, PartialOrd)]
pub struct Identifier {
    /// The actual identifier.
    pub ident: ShortString,
}

impl AsRef<str> for Identifier {
    fn as_ref(&self) -> &str {
        self.ident.as_ref()
    }
}

impl borrow::Borrow<str> for Identifier {
    fn borrow(&self) -> &str {
        self.ident.as_ref()
    }
}

impl<T: AsRef<str>> PartialEq<T> for Identifier {
    fn eq(&self, other: &T) -> bool {
        self.ident == other.as_ref()
    }
}

impl Eq for Identifier {}

impl hash::Hash for Identifier {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.ident.hash(state)
    }
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.ident.fmt(f)
    }
}

/// An identifier is the unique name of all expressions that we allow to be
/// named.
///
/// It is a word composed of a leading alphabetic Unicode character or an
/// underscore, followed by alphanumeric Unicode characters or underscore or
/// period.
#[derive(Clone, Debug)]
pub struct TypeIdentifier {
    /// The actual identifier.
    pub ident: ShortString,
}

impl AsRef<str> for TypeIdentifier {
    fn as_ref(&self) -> &str {
        self.ident.as_ref()
    }
}

impl borrow::Borrow<str> for TypeIdentifier {
    fn borrow(&self) -> &str {
        self.ident.as_ref()
    }
}

impl<T: AsRef<str>> PartialEq<T> for TypeIdentifier {
    fn eq(&self, other: &T) -> bool {
        self.ident == other.as_ref()
    }
}

impl Eq for TypeIdentifier {}

impl hash::Hash for TypeIdentifier {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.ident.hash(state)
    }
}

impl fmt::Display for TypeIdentifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.ident.fmt(f)
    }
}

/// A `field_name: Type` pair.
#[derive(Clone, Debug)]
pub struct TypeIdentField {
    /// The name of the field.
    pub field_name: Spanned<Identifier>,
    /// The type of the field.
    pub ty: Spanned<TypeIdentifier>,
}

#[derive(Clone, Debug)]
pub struct ListTypeIdentifier {
    pub inner_type: TypeIdentifier,
}

/// Our take on a literal string is just a Identifier wrapped in two double
/// quotes. We don't do any escaping or anything like that.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct StringLiteral(pub(crate) String);

impl From<StringLiteral> for String {
    fn from(literal: StringLiteral) -> Self {
        literal.0
    }
}

/// The user-defined type of a record. It's very similar to a RibBody (in EBNF
/// it's the same), but it simplifies creating the SymbolTable, because they're
/// semantically different.
#[derive(Clone, Debug)]
pub struct RecordTypeIdentifier {
    pub key_values: Spanned<Vec<RibField>>,
}

//============= Literals ====================================================

/// An integer literal is a sequence of digits.
/// IntegerLiteral ::= [0-9]+
///
/// We parse it as a string and then convert it to an integer.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IntegerLiteral(pub i64);

impl From<&'_ IntegerLiteral> for ShortString {
    fn from(literal: &IntegerLiteral) -> Self {
        ShortString::from(literal.0.to_string().as_str())
    }
}

impl From<&'_ IntegerLiteral> for i64 {
    fn from(literal: &IntegerLiteral) -> Self {
        literal.0
    }
}

//------------ PrefixLengthLiteral -------------------------------------------

/// A prefix length literal is a sequence of digits preceded by a '/'.
/// PrefixLengthLiteral ::= /[0-9]+
///
/// We parse it as a string and then convert it to an integer.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PrefixLengthLiteral(pub u8);

impl From<&'_ PrefixLengthLiteral> for ShortString {
    fn from(literal: &PrefixLengthLiteral) -> Self {
        ShortString::from(literal.0.to_string().as_str())
    }
}

impl From<&'_ PrefixLengthLiteral> for u8 {
    fn from(literal: &PrefixLengthLiteral) -> Self {
        literal.0
    }
}

//------------ HexLiteral ---------------------------------------------------

/// A hex literal is a sequence of hex digits, prefixed by '0x'
/// HexLiteral ::= '0x' [0-9a-fA-F]+
#[derive(Clone, Debug)]

pub struct HexLiteral(pub u64);

impl From<&'_ HexLiteral> for ShortString {
    fn from(literal: &HexLiteral) -> Self {
        ShortString::from(literal.0.to_string().as_str())
    }
}

impl From<&'_ HexLiteral> for u64 {
    fn from(literal: &HexLiteral) -> Self {
        literal.0
    }
}

//------------ AsnLiteral ---------------------------------------------------

/// An ASN literal is a sequence of hex digits, prefixed by 'AS'
#[derive(Clone, Debug)]

pub struct AsnLiteral(pub u32);

impl From<&'_ AsnLiteral> for ShortString {
    fn from(literal: &AsnLiteral) -> Self {
        ShortString::from(literal.0.to_string().as_str())
    }
}

impl From<&'_ AsnLiteral> for Asn {
    fn from(literal: &AsnLiteral) -> Self {
        Asn::from_u32(literal.0)
    }
}

#[derive(Clone, Debug)]
pub struct StandardCommunityLiteral(pub routecore::bgp::communities::StandardCommunity);

#[derive(Clone, Debug)]
pub struct ExtendedCommunityLiteral(pub routecore::bgp::communities::ExtendedCommunity);

#[derive(Clone, Debug)]
pub struct LargeCommunityLiteral(pub routecore::bgp::communities::LargeCommunity);

//------------ FloatLiteral --------------------------------------------------

/// A float literal is a sequence of digits with a decimal point.
/// FloatLiteral ::= [0-9]+ '.' [0-9]+
///
#[derive(Clone, Debug)]
pub struct FloatLiteral(pub f64);

/// A boolean literal is either `true` or `false`.
#[derive(Clone, Debug)]

pub struct BooleanLiteral(pub bool);

impl From<&'_ BooleanLiteral> for ShortString {
    fn from(literal: &BooleanLiteral) -> Self {
        ShortString::from(literal.0.to_string().as_str())
    }
}

impl From<&'_ BooleanLiteral> for bool {
    fn from(literal: &BooleanLiteral) -> Self {
        literal.0
    }
}

/// A byte string literal is a sequence of bytes, preceded by '0x'
#[derive(Clone, Debug)]
pub struct ByteStringLiteral(pub SmallVec<[u8; 24]>);

/// Every filter needs to return either a 'accept' or 'reject' statement.
/// failing to set it properly ends in the whole thing being cancelled.
#[derive(
    Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Serialize,
)]
pub enum AcceptReject {
    Accept,
    Reject,
    NoReturn,
}

impl std::fmt::Display for AcceptReject {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AcceptReject::Accept => write!(f, "accept"),
            AcceptReject::NoReturn => write!(f, "no return"),
            AcceptReject::Reject => write!(f, "reject"),
        }
    }
}

#[derive(Clone, Debug)]
pub enum LiteralExpr {
    StringLiteral(StringLiteral),
    PrefixLiteral(Prefix),
    PrefixLengthLiteral(PrefixLengthLiteral),
    AsnLiteral(AsnLiteral),
    IpAddressLiteral(IpAddress),
    ExtendedCommunityLiteral(ExtendedCommunityLiteral),
    StandardCommunityLiteral(StandardCommunityLiteral),
    LargeCommunityLiteral(LargeCommunityLiteral),
    IntegerLiteral(IntegerLiteral),
    HexLiteral(HexLiteral),
    BooleanLiteral(BooleanLiteral),
}

impl TryFrom<&'_ LiteralExpr> for TypeValue {
    type Error = CompileError;

    fn try_from(value: &'_ LiteralExpr) -> Result<Self, Self::Error> {
        match value {
            LiteralExpr::StringLiteral(v) => Ok(v.clone().into()),
            LiteralExpr::PrefixLiteral(v) => Ok(v.try_into()?),
            LiteralExpr::PrefixLengthLiteral(v) => Ok(v.clone().into()),
            LiteralExpr::AsnLiteral(v) => Ok(v.clone().into()),
            LiteralExpr::IpAddressLiteral(v) => Ok(v.into()),
            LiteralExpr::ExtendedCommunityLiteral(v) => v.clone().try_into(),
            LiteralExpr::StandardCommunityLiteral(v) => v.clone().try_into(),
            LiteralExpr::LargeCommunityLiteral(v) => v.clone().try_into(),
            LiteralExpr::IntegerLiteral(v) => Ok(v.clone().into()),
            LiteralExpr::HexLiteral(v) => Ok(v.clone().into()),
            LiteralExpr::BooleanLiteral(v) => Ok(v.clone().into()),
        }
    }
}

#[derive(Clone, Debug)]
pub struct LiteralAccessExpr {
    pub literal: LiteralExpr,
    pub access_expr: Vec<AccessExpr>,
}

/// An expression that ultimately will resolve into a TypeValue, e.g. the
/// right-hand of an assignment, or an argument to a method, etc.
#[derive(Clone, Debug)]
pub enum ValueExpr {
    /// a literal, or a chain of field accesses and/or methods on a literal,
    /// e.g. `10.0.0.0/8.covers(..)`
    LiteralAccessExpr(LiteralAccessExpr),
    /// a JunOS style prefix match expression, e.g. `0.0.0.0/0
    /// prefix-length-range /12-/16`
    PrefixMatchExpr(PrefixMatchExpr),
    /// an access receiver (an expression named with a single identifier, e.g.
    /// `my_var`), or a chain of field accesses and/or methods on an access
    /// receiver.
    ComputeExpr(ComputeExpr),
    /// an expression of the form `word(argument)`, so nothing in front of
    /// `word`, this would be something like a builtin method call, or an
    /// action or term with an argument.
    RootMethodCallExpr(MethodComputeExpr),
    /// a record that doesn't have a type mentioned in the assignment of it,
    /// e.g `{ value_1: 100, value_2: "bla" }`. This can also be a sub-record
    /// of a record that does have an explicit type.
    AnonymousRecordExpr(AnonymousRecordValueExpr),
    /// an expression of a record that does have a type, e.g. `MyType {
    /// value_1: 100, value_2: "bla" }`, where MyType is a user-defined Record
    /// Type.
    TypedRecordExpr(Spanned<TypedRecordValueExpr>),
    /// An expression that yields a list of values, e.g. `[100, 200, 300]`
    ListExpr(ListValueExpr),
}

#[derive(Clone, Debug)]
pub struct ArgExprList {
    pub args: Spanned<Vec<ValueExpr>>,
}

impl ArgExprList {
    pub fn is_empty(&self) -> bool {
        self.args.is_empty()
    }
}

// ============ Compound Expressions ========================================

// Compound expressions consist of field access of data structures or method
// calls on data structures.

//------------- AccessExpr --------------------------------------------------

// Either a method call or a field access. Used as part of a ComputeExpr.

// AccessExpr ::= MethodComputeExpr | AccessReceiver

#[derive(Clone, Debug)]
pub enum AccessExpr {
    MethodComputeExpr(MethodComputeExpr),
    FieldAccessExpr(FieldAccessExpr),
}

impl AccessExpr {
    pub fn get_ident(&self) -> Result<&ShortString, CompileError> {
        match self {
            AccessExpr::MethodComputeExpr(expr) => Ok(&expr.ident.ident),
            AccessExpr::FieldAccessExpr(expr)
                if !expr.field_names.is_empty() =>
            {
                Ok(&first_into_compile_err!(expr.field_names)?.ident)
            }
            expr => Err(CompileError::Internal(format!(
                "Cannot find ident in AccessExpr: {:?}",
                expr
            ))),
        }
    }
}

/// A ComputeExpr is an expression that starts with an access receiver,
/// optionally followed by one or more method calls, and/or access receivers,
/// e.g. 'rib-rov.longest_match(route.prefix).prefix.len()`.
///
/// Note that an expression ending in a field access, i.e. not a method call,
/// is also parsed as a ComputeExpr, but with an empty method call list.
#[derive(Clone, Debug)]
pub struct ComputeExpr {
    pub receiver: AccessReceiver,
    pub access_expr: Vec<AccessExpr>,
}

impl ComputeExpr {
    pub fn get_receiver(&self) -> &AccessReceiver {
        &self.receiver
    }

    // An Access Receiver always exists, but it might not have an identifier,
    // currently that can only mean that it is a global method being called,
    // but that's up to the caller to figure out.
    pub fn get_receiver_ident(&self) -> Result<ShortString, CompileError> {
        self.get_receiver()
            .get_ident()
            .ok_or_else(|| {
                CompileError::from("Missing identifier in Expression.")
            })
            .map(|ar| ar.ident.clone())
    }
}

/// The AccessReceiver is the specifier of a data structure that is being called
/// (used as part of a ComputeExpr) or used to retrieve one of its fields. Can
/// also be a stand-alone specifier.
#[derive(Clone, Debug)]
pub enum AccessReceiver {
    /// The identifier of the data structure.
    Ident(Spanned<Identifier>),
    /// or it can only be in the Global Scope (for global methods), it doesn't
    /// have a string as identifier then.
    GlobalScope,
}

impl AccessReceiver {
    pub fn get_ident(&self) -> Option<&Spanned<Identifier>> {
        if let Self::Ident(ident) = &self {
            Some(ident)
        } else {
            None
        }
    }
}

impl std::fmt::Display for AccessReceiver {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Ident(ident) => write!(f, "{}", ident),
            Self::GlobalScope => write!(f, "GLOBAL SCOPE"),
        }
    }
}

//------------- FieldAccessExpr ---------------------------------------------

// The chain of fields that are being accessed. The last field is the name of
// the field that is accessed.

// FieldAccessExpr ::= ( '.'? Identifier )+

#[derive(Clone, Debug)]
pub struct FieldAccessExpr {
    // The chain of fields that are being accessed. The last field is the
    // name of the field that is accessed.
    pub field_names: Vec<Spanned<Identifier>>,
}

/// The method that is being called on the data structure (directly or on one
/// of its fields).
#[derive(Clone, Debug)]
pub struct MethodComputeExpr {
    /// The name of the method.
    pub ident: Spanned<Identifier>,
    /// The list with arguments
    pub args: ArgExprList,
}

//============ First-Order Logic ============================================

// "No, no, you're not thinking. You're just being logical." -- Niels Bohr

// A first-order logical formula is a sequence of (predicate) symbols, logical
// connectives (a.k.a. logical operators), comparison operators, and
// quantifiers. Logical formulas can be grouped (for operator precedence) and
// nested through the contained symbols. Nesting will raise the arity of the
// formula.
//
// We are only using a binary logical formula that consists of a tuple of
// (boolean expression, logical operator, boolean expression), where boolean
// expressions can be grouped with parentheses. Also, we are only the
// connectives listed in this enum, i.e. ∧ (and), ∨ (or), and ¬ (not).
//
// This hardly limits the logical expressiveness of our language, because the
// user can:
// - Create the missing connectives (material implication ('if-then'),
//   bi-directional implication ('if and only if') from the given connectives
//   (for an arity of 2 at least) or they can be constructed in code in the
//   `apply` section.
// - Use the logical connectives in a grouped fashion to reduce any number of
//   Boolean expressions down to a binary formula, e.g. (A ∧ B) ∨ (C ∧ D) is
//   equivalent to A ∧ B ∨ C ∧ D. This limits the number of boolean functions
//   we have to consider when evaluating the formula, i.e. a fully complete
//   set of boolean functions has 16 functions for an arity of 2.
//
// The first point above reduces the cognitive overhead by simplifying the
// flow of the program and using familiar constructs, like 'if..then' and
// early returns.
//
// The second point also reduces (perceived) ambiguity because we do not allow
// using implicit logic operator precedence (that probably no one knows
// anyway).

// https://en.wikipedia.org/wiki/First-order_logic#Formulas

// The form in which roto users can express logical formulas, etc. is:

// term {
//      <logical formula>;
//      <logical formula>;
//      ...
// }
//
// where <logical formula> is one of:
//
// <boolean expression> || <boolean expression>;
// <boolean expression> && <boolean expression>;
// !<boolean expression>;
// <boolean expression>;

// Ex.:
// ┌──────────┐┌──┐┌──────────┐ ┌──┐ ┌──────────┐
// │(BoolExpr)││&&││(BoolExpr)│ │||│ │(BoolExpr)│
// └──────────┘└──┘└──────────┘ └──┘ └──────────┘
// ▲──────────LF::And─────────▲      ▲──LF::BE──▲
//
// ▲──────────BoolExpr────────▲      ▲─BoolExpr─▲
//
// ▲────────────────────LF::Or──────────────────▲

/// The Logical expression evaluates to a logical formula, that is a tuple of
/// (optional boolean expression, logical operator, boolean expression). The
/// first boolean expression is optional, only in the case of a negation (not)
/// operator. The second boolean expression is always present.
#[derive(Clone, Debug)]
pub enum LogicalExpr {
    OrExpr(OrExpr),
    AndExpr(AndExpr),
    NotExpr(NotExpr),
    BooleanExpr(BooleanExpr),
}

#[derive(Clone, Debug)]
pub enum CompareArg {
    /// A "stand-alone" left|right-hand side argument of a comparison
    ValueExpr(ValueExpr),
    /// A nested logical formula, e.g. (A && B) || (C && D) used as a left|
    /// right-hand side argument of a comparison. Note that this can only
    /// have the opposite hand be a boolean expression.
    GroupedLogicalExpr(GroupedLogicalExpr),
}

/// A Boolean expression is an expression that *may* evaluate to one of:
/// - a Boolean-valued function, which is a fn : X → B, where X is an arbitrary
///   set and B is a boolean value. For example, an Integer expression can
///   never evaluate to a boolean value, but a method call expression may
///   evaluate to a method that returns a Boolean value.
/// - a Literal Boolean value, "true" or "false"
/// - a Boolean-typed variable, including boolean-typed record fields
/// - an Expression containing a boolean-valued operator, such as '==', '!=',
///   ">=", "<="
#[derive(Clone, Debug)]
pub enum BooleanExpr {
    /// A complete formula that is wrapped in parentheses is a Boolean-Valued
    /// Function, since it will always return a Boolean value.
    GroupedLogicalExpr(GroupedLogicalExpr),
    /// "true" | "false" literals
    BooleanLiteral(BooleanLiteral),
    /// A syntactically correct comparison always evaluates to a
    /// Boolean-Valued Function, since it will always return a Boolean value.
    CompareExpr(Box<CompareExpr>),
    /// A ComputeExpression *may* evaluate to a function that returns a boolean
    ComputeExpr(ComputeExpr),
    /// Just like a ComputeExpr, a Literal, or a Literal access, e.g.
    /// `10.0.0.0/16.covers()`, may return a boolean
    LiteralAccessExpr(LiteralAccessExpr),
    /// Set Compare expression, will *always* result in a boolean-valued
    /// function. Syntactic sugar for a truth-function that performs
    /// fn : a -> {a} ∩ B
    ListCompareExpr(Box<ListCompareExpr>),
    /// syntactic sugar for a method on a prefix function that returns a
    /// boolean.
    PrefixMatchExpr(PrefixMatchExpr),
}

#[derive(Clone, Debug)]
pub struct CompareExpr {
    pub left: CompareArg,
    pub op: CompareOp,
    pub right: CompareArg,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub enum CompareOp {
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    Or,
    And,
    In,
    NotIn,
}

#[derive(Clone, Debug)]
pub struct AndExpr {
    pub left: BooleanExpr,
    pub right: BooleanExpr,
}

#[derive(Clone, Debug)]
pub struct OrExpr {
    pub left: BooleanExpr,
    pub right: BooleanExpr,
}

#[derive(Clone, Debug)]
pub struct NotExpr {
    pub expr: BooleanExpr,
}

#[derive(Clone, Debug)]
pub struct ListCompareExpr {
    pub left: ValueExpr,
    pub op: CompareOp,
    pub right: ValueExpr,
}

#[derive(Clone, Debug)]
pub struct GroupedLogicalExpr {
    pub expr: Box<LogicalExpr>,
}

#[derive(Clone, Debug)]
pub enum MatchOperator {
    // 'match' followed by a block containing truth expressions
    Match,
    // a `match some_value with` match pattern for enums, the block
    // enumerates the variants
    MatchValueWith(Spanned<Identifier>),
    // Query quantifiers, where the following block contains expressions that
    // may yield multiple instances of type values
    Some,
    ExactlyOne,
    All,
}

impl MatchOperator {
    pub(crate) fn get_ident(&self) -> Result<Spanned<Identifier>, CompileError> {
        if let MatchOperator::MatchValueWith(id) = self {
            Ok(id.clone())
        } else {
            Err(CompileError::from(format!(
                "Cannot find identifier for this match: {:?}",
                self
            )))
        }
    }
}

#[derive(Clone, Debug)]
pub enum PrefixMatchType {
    Exact,
    Longer,
    OrLonger,
    PrefixLengthRange(PrefixLengthRange),
    UpTo(PrefixLength),
    Through(PrefixLength),
    NetMask(IpAddress),
}

#[derive(Clone, Debug)]
pub struct PrefixMatchExpr {
    pub prefix: Prefix,
    pub ty: PrefixMatchType,
}

#[derive(Clone, Debug)]
pub enum IpAddress {
    Ipv4(Ipv4Addr),
    Ipv6(Ipv6Addr),
}

impl std::fmt::Display for IpAddress {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IpAddress::Ipv4(Ipv4Addr(v4)) => write!(f, "{v4}"),
            IpAddress::Ipv6(Ipv6Addr(v6)) => write!(f, "{v6}"),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Ipv4Addr(pub std::net::Ipv4Addr);

#[derive(Clone, Debug)]
pub struct Ipv6Addr(pub std::net::Ipv6Addr);

#[derive(Clone, Debug)]
pub struct Prefix {
    pub addr: IpAddress,
    pub len: PrefixLength,
}

#[derive(Clone, Debug)]
pub struct PrefixLength(pub u8);

#[derive(Clone, Debug)]
pub struct PrefixLengthRange {
    pub start: PrefixLength,
    pub end: PrefixLength,
}

//------------ ShortString ---------------------------------------------------

#[derive(Clone)]
pub struct ShortString {
    bytes: SmallVec<[u8; 24]>,
}

impl ShortString {
    pub fn as_bytes(&self) -> &[u8] {
        &self.bytes
    }

    pub fn as_str(&self) -> &str {
        unsafe { str::from_utf8_unchecked(&self.bytes) }
    }
}

impl<'a> From<&'a str> for ShortString {
    fn from(src: &'a str) -> ShortString {
        ShortString {
            bytes: SmallVec::from_slice(src.as_bytes()),
        }
    }
}

impl ops::Deref for ShortString {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.as_str()
    }
}

impl AsRef<str> for ShortString {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl AsRef<[u8]> for ShortString {
    fn as_ref(&self) -> &[u8] {
        self.as_bytes()
    }
}

impl borrow::Borrow<str> for ShortString {
    fn borrow(&self) -> &str {
        self.as_str()
    }
}

impl borrow::Borrow<[u8]> for ShortString {
    fn borrow(&self) -> &[u8] {
        self.as_bytes()
    }
}

impl<T: AsRef<str>> PartialEq<T> for ShortString {
    fn eq(&self, other: &T) -> bool {
        self.as_str().eq(other.as_ref())
    }
}

impl Eq for ShortString {}

impl<T: AsRef<str>> PartialOrd<T> for ShortString {
    fn partial_cmp(&self, other: &T) -> Option<cmp::Ordering> {
        self.as_str().partial_cmp(other.as_ref())
    }
}

impl Ord for ShortString {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        self.as_str().cmp(other.as_str())
    }
}

impl hash::Hash for ShortString {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.as_str().hash(state)
    }
}

impl fmt::Display for ShortString {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

impl fmt::Debug for ShortString {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(self.as_str(), f)
    }
}

impl Serialize for ShortString {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(self)
    }
}
