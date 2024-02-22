use std::{borrow, cmp, fmt, hash, ops, str};

use log::trace;
use nom::branch::{alt, permutation};
use nom::bytes::complete::{
    take, take_while, take_while1, take_while_m_n,
};
use nom::character::complete::{
    char, digit1, hex_digit0, multispace0, multispace1,
};
use nom::combinator::{all_consuming, cut, not, opt, recognize};
use nom::error::{
    context, ErrorKind, FromExternalError, ParseError, VerboseError,
};
use nom::multi::{
    fold_many0, many0, many1, separated_list0, separated_list1,
};
use nom::sequence::{
    delimited, pair, preceded, separated_pair, terminated, tuple,
};
use nom::{
    bytes::complete::{tag, take_until},
    character::complete::char as tag_char,
    combinator::map,
    IResult,
};
use nom::{AsChar, Finish};
use serde::{Serialize, Serializer};
use smallvec::SmallVec;

use routecore::bgp::communities::HumanReadableCommunity as Community;
use routecore::bgp::communities::{ExtendedCommunity, LargeCommunity, StandardCommunity};
use routecore::asn::Asn;

use crate::compiler::error::CompileError;
use crate::types::typevalue::TypeValue;
use crate::{first_into_compile_err, parse_string};

/// ======== Root ===========================================================

/// The Root of the file.
///
// Root ::= RootExpr+

#[derive(Clone, Debug, Default)]
pub struct SyntaxTree {
    pub expressions: Vec<RootExpr>,
}

impl SyntaxTree {
    pub fn parse_str(
        input: &str,
    ) -> Result<(&str, Self), VerboseError<&str>> {
        Self::parse_root(input).finish()
    }

    fn parse_root(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, expressions) = all_consuming(many1(preceded(
            skip_opt_ws,
            terminated(RootExpr::parse, skip_opt_ws),
        )))(input)?;
        Ok((input, Self { expressions }))
    }
}

//------------ RootExpr -----------------------------------------------------

// RootExpr ::= FilterMap |
//     "filter-map" Identifier ForStatement WithStatement '{' FilterMap '}' |
//     "filter" Identifier ForStatement WithStatement '{' Filter '}' |
//     "rib" Identifier 'contains' TypeIdentifier '{' RibBody '}' |
//     "table" Identifier '{' TableBody '}' |
//     "output-stream" Identifier '{' StreamBody '}' |
//     RecordTypeAssignment |
//     Comment

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
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, expressions) = context(
            "root",
            alt((
                map(Rib::parse, Self::Rib),
                map(Table::parse, Self::Table),
                map(OutputStream::parse, Self::OutputStream),
                map(FilterMap::parse, |m| Self::FilterMap(Box::new(m))),
                map(RecordTypeAssignment::parse, Self::Ty),
            )),
        )(input)?;
        Ok((input, expressions))
    }

    pub fn get_filter_map(&self) -> Result<&FilterMap, CompileError> {
        match self {
            Self::FilterMap(m) => Ok(m),
            _ => Err(CompileError::new("not a filter-map".into())),
        }
    }
}

//------------ ListValueExpr ------------------------------------------------

// ListValueExpr ::= '[' ValueExpr+ ']'

// A list of values of the same type or a list where all the values can be
// converted to the same type

#[derive(Clone, Debug)]
pub struct ListValueExpr {
    pub values: Vec<ValueExpr>,
}

impl ListValueExpr {
    fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, values) = context(
            "list value",
            delimited(
                opt_ws(char('[')),
                context(
                    "List Value",
                    separated_list1(char(','), opt_ws(ValueExpr::parse)),
                ),
                opt_ws(char(']')),
            ),
        )(input)?;

        Ok((input, ListValueExpr { values }))
    }
}

//------------ AnonymousRecordValueExpr -------------------------------------

// RecordValueExpr ::= '{' (Identifier ':' ValueExpr, )+ '}'

// The value of a (anonymous) record
// Defined and directly used, mainly as an argument to a method, where the
// actual type can be inferred unambiguously.

#[derive(Clone, Debug)]
pub struct AnonymousRecordValueExpr {
    pub key_values: Vec<(Identifier, ValueExpr)>,
}

impl AnonymousRecordValueExpr {
    fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, key_values) = context(
            "record value",
            delimited(
                opt_ws(char('{')),
                context(
                    "Record Value",
                    separated_list1(
                        char(','),
                        opt_ws(tuple((
                            terminated(
                                opt_ws(Identifier::parse),
                                opt_ws(char(':')),
                            ),
                            opt_ws(ValueExpr::parse),
                        ))),
                    ),
                ),
                opt_ws(char('}')),
            ),
        )(input)?;

        Ok((input, AnonymousRecordValueExpr { key_values }))
    }
}

//------------ TypedRecordValueExpr -----------------------------------------

// RecordValueExpr ::= Identifier '{' (Identifier ':' ValueExpr, )+ '}'

// Used in the 'Define' section to create variables to hold a record.

#[derive(Clone, Debug)]
pub struct TypedRecordValueExpr {
    pub type_id: TypeIdentifier,
    pub key_values: Vec<(Identifier, ValueExpr)>,
}

impl TypedRecordValueExpr {
    fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, (type_id, key_values)) = context(
            "typed record value",
            tuple((
                opt_ws(TypeIdentifier::parse),
                delimited(
                    opt_ws(char('{')),
                    context(
                        "Record Value",
                        separated_list1(
                            char(','),
                            opt_ws(tuple((
                                terminated(
                                    opt_ws(Identifier::parse),
                                    opt_ws(char(':')),
                                ),
                                opt_ws(ValueExpr::parse),
                            ))),
                        ),
                    ),
                    opt_ws(char('}')),
                ),
            )),
        )(input)?;

        Ok((
            input,
            TypedRecordValueExpr {
                type_id,
                key_values,
            },
        ))
    }
}

// The value of a typed record

//------------ RecordTypeAssignment -----------------------------------------

// RecordTypeAssignment ::= "type" Identifier '{' RecordTypeIdentifier '}'

#[derive(Clone, Debug)]
pub struct RecordTypeAssignment {
    pub ident: TypeIdentifier,
    pub record_type: RecordTypeIdentifier,
}

impl RecordTypeAssignment {
    fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, (ident, record_type)) = context(
            "record type definition",
            tuple((
                context(
                    "record type name",
                    preceded(
                        opt_ws(tag("type")),
                        opt_ws(TypeIdentifier::parse),
                    ),
                ),
                context(
                    "type definition",
                    delimited(
                        opt_ws(char('{')),
                        cut(RecordTypeIdentifier::parse),
                        opt_ws(char('}')),
                    ),
                ),
            )),
        )(input)?;

        Ok((input, RecordTypeAssignment { ident, record_type }))
    }
}

//------------ FilterMap -------------------------------------------------------

// FilterMap ::= "filter-map" Identifier "for" Identifier WithStatement
//              WithStatement '{' FilterMapBody '}' |
//               "filter" Identifier "for" Identifier WithStatement '{' FilterBody '}'
#[derive(Clone, Copy, Debug)]
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
    pub ident: Identifier,
    pub for_ident: Option<TypeIdentField>,
    pub with_kv: Vec<TypeIdentField>,
    pub body: FilterMapBody,
}

impl FilterMap {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, (ty, ident, for_ident, with_kv, body)) = context(
            "filter-map definition",
            tuple((
                alt((
                    context(
                        "filter-map type",
                        map(opt_ws(tag("filter-map")), |_| {
                            FilterType::FilterMap
                        }),
                    ),
                    context(
                        "filter-map type",
                        map(opt_ws(tag("filter")), |_| FilterType::Filter),
                    ),
                )),
                opt_ws(Identifier::parse),
                for_statement,
                with_statement,
                context(
                    "filter-map body",
                    delimited(
                        opt_ws(char('{')),
                        cut(FilterMapBody::parse),
                        opt_ws(char('}')),
                    ),
                ),
                // map(many0(char('\n')), |_| ()),
            )),
        )(input)?;

        Ok((
            input,
            FilterMap {
                ty,
                ident,
                body,
                for_ident,
                with_kv: with_kv.unwrap_or_default(),
            },
        ))
    }
}

//------------ FilterMapBody ---------------------------------------------------

// FilterMapBody ::= Define FilterMapExpr+ Apply

#[derive(Clone, Debug)]
pub struct FilterMapBody {
    pub define: Define,
    pub expressions: Vec<FilterMapExpr>,
    pub apply: Option<ApplySection>,
}

impl FilterMapBody {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, (define, expressions, apply)) = permutation((
            Define::parse,
            context("filter-map expressions", many0(FilterMapExpr::parse)),
            opt(ApplySection::parse),
        ))(input)?;

        Ok((
            input,
            Self {
                define,
                expressions,
                apply,
            },
        ))
    }
}

// These are the sections that can appear multiple times in a Filter(Map)
#[derive(Debug, Clone)]
pub enum FilterMapExpr {
    Term(TermSection),
    Action(ActionSection),
    // Empty, // Import(ImportBody),
}

//------------ FilterMapExpr ----------------------------------------------------
//
// FilterMapExpr ::= Define |
//   Term+ |
//   Action+ |
//   'import' ForStatement '{' ImportBody '}'

impl FilterMapExpr {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, expressions) = context(
            "filter-map expression",
            alt((
                map(TermSection::parse, Self::Term),
                map(ActionSection::parse, Self::Action),
                // map(multispace1, |_| Self::Empty),
                // map(ImportBody::parse, FilterMapBody::Import),
            )),
        )(input)?;
        Ok((input, expressions))
    }
}

//------------ Define -------------------------------------------------------
#[derive(Clone, Debug)]
pub struct Define {
    pub for_kv: Option<TypeIdentField>, // associated Rib record type
    pub with_kv: Vec<TypeIdentField>,   // arguments
    pub body: DefineBody,
}

impl Define {
    fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, (for_kv, with_kv, body)) = context(
            "define definition",
            preceded(
                opt_ws(tag("define")),
                tuple((
                    for_statement,
                    with_statement,
                    context(
                        "define block",
                        delimited(
                            opt_ws(char('{')),
                            DefineBody::parse,
                            opt_ws(char('}')),
                        ),
                    ),
                )),
            ),
        )(input)?;

        Ok((
            input,
            Self {
                for_kv,
                with_kv: with_kv.unwrap_or_default(),
                body,
            },
        ))
    }
}

#[derive(Clone, Debug)]
pub enum RxTxType {
    RxOnly(TypeIdentField),
    Split(TypeIdentField, TypeIdentField),
    PassThrough(TypeIdentField),
}

//------------ DefineBody ---------------------------------------------------

// DefineBody ::= (( 'use' Identifier ';' )? (('rx' Identifier ':'
//     TypeIdentifier ';') ('tx' Identifier ':' TypeIdentifier ';')) | (
//     'rx_tx' Identifier ':' TypeIdentifier ';' ))? ( Identifier '='
//     ComputeExpr ';' )+ )+

#[derive(Clone, Debug)]
pub struct DefineBody {
    pub rx_tx_type: RxTxType,
    pub use_ext_data: Vec<(Identifier, Identifier)>,
    pub assignments: Vec<(Identifier, ValueExpr)>,
}

impl DefineBody {
    fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, (rx_tx_type, use_ext_data, assignments)) = tuple((
            alt((
                map(
                    delimited(
                        opt_ws(tag("rx_tx")),
                        opt_ws(TypeIdentField::parse),
                        opt_ws(char(';')),
                    ),
                    RxTxType::PassThrough,
                ),
                map(
                    permutation((
                        delimited(
                            opt_ws(tag("rx")),
                            opt_ws(TypeIdentField::parse),
                            opt_ws(char(';')),
                        ),
                        delimited(
                            opt_ws(tag("tx")),
                            opt_ws(TypeIdentField::parse),
                            opt_ws(char(';')),
                        ),
                    )),
                    |t| RxTxType::Split(t.0, t.1),
                ),
                map(
                    delimited(
                        opt_ws(tag("rx")),
                        opt_ws(TypeIdentField::parse),
                        opt_ws(char(';')),
                    ),
                    RxTxType::RxOnly,
                ),
            )),
            many0(delimited(
                opt_ws(tag("use")),
                tuple((opt_ws(Identifier::parse), opt_ws(Identifier::parse))),
                opt_ws(char(';')),
            )),
            many0(context(
                "assignments",
                separated_pair(
                    opt_ws(Identifier::parse),
                    preceded(multispace0, char('=')),
                    terminated(opt_ws(ValueExpr::parse), opt_ws(char(';'))),
                ),
            )),
        ))(
            input
        )?;

        // let (use_ext_data, assignments) = statements.iter().cloned().unzip();

        Ok((
            input,
            Self {
                rx_tx_type,
                use_ext_data,
                assignments,
            },
        ))
    }
}

//------------ Term ---------------------------------------------------------

#[derive(Clone, Debug)]
pub struct TermSection {
    pub ident: Identifier,
    pub for_kv: Option<TypeIdentField>,
    pub with_kv: Vec<TypeIdentField>,
    pub body: TermBody,
}

impl TermSection {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, (ident, for_kv, with_kv, body)) = context(
            "term definition",
            tuple((
                preceded(
                    opt_ws(tag("term")),
                    cut(context(
                        "term name",
                        delimited(
                            multispace1,
                            Identifier::parse,
                            multispace1,
                        ),
                    )),
                ),
                for_statement,
                with_statement,
                context(
                    "term block",
                    cut(delimited(
                        opt_ws(char('{')),
                        TermBody::parse,
                        opt_ws(char('}')),
                    )),
                ),
            )),
        )(input)?;

        Ok((
            input,
            TermSection {
                ident,
                for_kv,
                with_kv: with_kv.unwrap_or_default(),
                body,
            },
        ))
    }
}

//------------ TermBody -----------------------------------------------------

// TermBody ::= TermScope+

#[derive(Clone, Debug)]
pub struct TermBody {
    pub scopes: Vec<TermScope>,
}

impl TermBody {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, scopes) =
            context("term body", many0(TermScope::parse))(input)?;
        Ok((input, Self { scopes }))
    }
}

//------------ TermScope ----------------------------------------------------

// Everything that can appear inside a named `term` block.

// TermScope ::=
//      ('use' Identifier ';')?
//     ( MatchOperator '{' ( ( LogicalExpr ';' ) | VariantMatchExpr ) )+ '}'

#[derive(Clone, Debug)]
pub struct TermScope {
    pub scope: Option<Identifier>,
    pub operator: MatchOperator,
    pub match_arms: Vec<(Option<TermPatternMatchArm>, Vec<LogicalExpr>)>,
}

impl TermScope {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, (scope, (operator, match_exprs))) = tuple((
            opt(opt_ws(context(
                "use scope",
                preceded(
                    opt_ws(tag("use")),
                    delimited(
                        multispace1,
                        Identifier::parse,
                        opt_ws(char(';')),
                    ),
                ),
            ))),
            context(
                "match expressions",
                tuple((
                    opt_ws(MatchOperator::parse),
                    delimited(
                        opt_ws(char('{')),
                        many0(context(
                            "match expression",
                            alt((
                                map(TermMatchExpr::parse, |m_e| {
                                    (
                                        Some(TermPatternMatchArm {
                                            variant_id: m_e.variant_id,
                                            data_field: m_e.data_field,
                                        }),
                                        m_e.logical_expr,
                                    )
                                }),
                                map(
                                    terminated(
                                        LogicalExpr::parse,
                                        opt_ws(char(';')),
                                    ),
                                    |l_e| (None, vec![l_e]),
                                ),
                            )),
                        )),
                        opt_ws(char('}')),
                    ),
                )),
            ),
        ))(input)?;
        Ok((
            input,
            Self {
                scope,
                operator,
                match_arms: match_exprs,
            },
        ))
    }
}

//------------ TermPatternMatchArm ------------------------------------------

// A Match arm, with or without a data field. Used to capture a MatchExpr and
// separate it from its logical expression(s), so that the TermScope (down
// below) can capture it as an optional expression for that TermScope. so the
// difference between a Match Expression that represents a match arm (of an
// Enum) and a 'regular' collection of logical expressions, is just this
// optional VariantMatchExpr.

#[derive(Clone, Debug)]
pub struct TermPatternMatchArm {
    pub variant_id: Identifier,
    pub data_field: Option<Identifier>,
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

impl ActionSection {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, (ident, with_kv, body)) = context(
            "action definition",
            tuple((
                preceded(
                    opt_ws(tag("action")),
                    cut(context(
                        "action name",
                        delimited(
                            multispace1,
                            Identifier::parse,
                            multispace1,
                        ),
                    )),
                ),
                with_statement,
                context(
                    "action block",
                    delimited(
                        opt_ws(char('{')),
                        ActionSectionBody::parse,
                        opt_ws(char('}')),
                    ),
                ),
            )),
        )(input)?;

        Ok((
            input,
            ActionSection {
                ident,
                // for_kv,
                with_kv: with_kv.unwrap_or_default(),
                body,
            },
        ))
    }
}

//------------ ActionBody -----------------------------------------------------

// ActionBody ::= (ActionExpr ';')+

#[derive(Clone, Debug)]
pub struct ActionSectionBody {
    pub expressions: Vec<ComputeExpr>,
}

impl ActionSectionBody {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, expressions) = context(
            "action body",
            many1(opt_ws(terminated(
                OptionalGlobalComputeExpr::parse,
                opt_ws(char(';')),
            ))),
        )(input)?;
        Ok((input, Self { expressions }))
    }
}

//------------ OptionalGlobalComputeExpr ------------------------------------

// ActionExpr ::= ComputeExpr | GlobalMethodExpr

// An Optional Global Compute Expressions can be either an ordinary Compute
// Expression, or a global method call, i.e. 'some-global-method(a, b)', so an
// expression without any dots in it ending in a method call.

// Action Expressions are always turned into regular Compute Expressions at
// parse time (so: here), consequently there's no `eval()` for an Optional
// GlobalComputeExpr.

#[derive(Clone, Debug)]
pub enum OptionalGlobalComputeExpr {
    GlobalMethodExpr(MethodComputeExpr),
    ComputeExpr(ComputeExpr),
}

impl OptionalGlobalComputeExpr {
    pub fn parse(
        input: &str,
    ) -> IResult<&str, ComputeExpr, VerboseError<&str>> {
        map(
            context(
                "action expression",
                alt((
                    map(
                        MethodComputeExpr::parse,
                        OptionalGlobalComputeExpr::GlobalMethodExpr,
                    ),
                    map(
                        ComputeExpr::parse,
                        OptionalGlobalComputeExpr::ComputeExpr,
                    ),
                )),
            ),
            |expr| expr.into_compute_expr(),
        )(input)
    }

    pub(crate) fn into_compute_expr(self) -> ComputeExpr {
        match self {
            Self::ComputeExpr(compute_expr) => compute_expr,
            Self::GlobalMethodExpr(ref access_expr) => ComputeExpr {
                access_expr: vec![AccessExpr::MethodComputeExpr(
                    access_expr.clone(),
                )],
                receiver: AccessReceiver::GlobalScope,
            },
        }
    }
}

//------------ ImportBody ---------------------------------------------------

// #[derive(Clone, Debug)]
// pub struct ImportBody {}

//------------ ApplySection -------------------------------------------------

// ApplySection ::= 'apply' ForStatement WithStatement '{' ApplyBody '}'

#[derive(Clone, Debug)]
pub struct ApplySection {
    pub body: ApplyBody,
    pub for_kv: Option<TypeIdentField>,
    pub with_kv: Vec<TypeIdentField>,
}

impl ApplySection {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, (for_kv, with_kv, body)) = context(
            "apply definition",
            preceded(
                opt_ws(tag("apply")),
                tuple((
                    for_statement,
                    with_statement,
                    context(
                        "apply block",
                        delimited(
                            opt_ws(char('{')),
                            ApplyBody::parse,
                            opt_ws(char('}')),
                        ),
                    ),
                )),
            ),
        )(input)?;

        Ok((
            input,
            Self {
                for_kv,
                with_kv: with_kv.unwrap_or_default(),
                body,
            },
        ))
    }
}

//------------ ApplyBody -----------------------------------------------------

// ApplyBody ::= ApplyScope+ (AcceptReject ';')?

#[derive(Clone, Debug)]
pub struct ApplyBody {
    pub scopes: Vec<ApplyScope>,
    pub accept_reject: Option<AcceptReject>,
}

impl ApplyBody {
    fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, (scopes, accept_reject)) = context(
            "apply body",
            tuple((
                many0(ApplyScope::parse),
                context("final accept reject", opt(opt_ws(accept_reject))),
            )),
        )(input)?;
        Ok((
            input,
            Self {
                scopes,
                accept_reject,
            },
        ))
    }
}

//------------ ApplyScope -----------------------------------------------------

// ApplyScope ::= ( 'use' Identifier ';' )? 'filter' MatchOperator (
//      ComputeExpr | Identifier ) 'not'? 'matching' '{' ( ( ComputeExpr |
//      Identifier ) ';' ( AcceptReject ';' )? )+ '}' ';'

#[derive(Clone, Debug)]
pub struct ApplyScope {
    pub scope: Option<Identifier>,
    pub match_action: MatchActionExpr,
}

impl ApplyScope {
    fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, (scope, apply_scope)) = tuple((
            opt_ws(opt(context(
                "use scope",
                preceded(
                    opt_ws(tag("use")),
                    delimited(
                        multispace1,
                        Identifier::parse,
                        opt_ws(char(';')),
                    ),
                ),
            ))),
            alt((
                map(
                    tuple((
                        preceded(
                            opt_ws(tag("filter")),
                            opt_ws(MatchOperator::parse),
                        ),
                        context(
                            "action expressions",
                            opt_ws(tuple((
                                ValueExpr::parse,
                                opt(terminated(
                                    opt(opt_ws(tag("not"))),
                                    opt_ws(tag("matching")),
                                )),
                                delimited(
                                    opt_ws(char('{')),
                                    alt((
                                        many1(context(
                                            "Call Expression",
                                            tuple((
                                                map(
                                                    opt_ws(terminated(
                                                        ValueExpr::parse,
                                                        opt_ws(char(';')),
                                                    )),
                                                    Some,
                                                ),
                                                opt(opt_ws(accept_reject)),
                                            )),
                                        )),
                                        map(opt_ws(accept_reject), |ar| {
                                            vec![(None, Some(ar))]
                                        }),
                                    )),
                                    terminated(
                                        opt_ws(char('}')),
                                        opt_ws(char(';')),
                                    ),
                                ),
                            ))),
                        ),
                    )),
                    |expr| {
                        (
                            input,
                            MatchActionExpr::FilterMatchAction(
                                FilterMatchActionExpr {
                                    operator: expr.0,
                                    negate: if let Some(negate) = expr.1 .1 {
                                        negate.is_none()
                                    } else {
                                        false
                                    },
                                    actions: expr.1 .2,
                                    filter_ident: expr.1 .0,
                                },
                            ),
                        )
                    },
                ),
                map(
                    context(
                        "pattern match expression",
                        opt_ws(PatternMatchActionExpr::parse),
                    ),
                    |expr| (input, MatchActionExpr::PatternMatchAction(expr)),
                ),
            )),
        ))(input)?;
        Ok((
            input,
            Self {
                scope,
                match_action: apply_scope.1,
            },
        ))
    }
}

// the Apply section can host regular rules that bind a term to an action
// under specified conditions. It can also host a match expression that
// does the same for enums.

//      MatchAction ::= FilterMatch | PatternMatch
#[derive(Clone, Debug)]
pub enum MatchActionExpr {
    FilterMatchAction(FilterMatchActionExpr),
    PatternMatchAction(PatternMatchActionExpr),
}

// A regular 'filter match` expression that binds a term to a (number of)
// action(s).

// FilterMatchAction ::= MatchOperator Value 'not'? 'matching' '{'
// ActionCallExpr+ ';' ( AcceptReject ';' )? '}' ';'?

#[derive(Clone, Debug)]

pub struct FilterMatchActionExpr {
    pub operator: MatchOperator,
    pub filter_ident: ValueExpr,
    pub negate: bool,
    pub actions: Vec<(Option<ValueExpr>, Option<AcceptReject>)>,
}

// A complete pattern match on a variable where every match arm can have
// multiple actions. Similar to TermMatchActionExpr, but a PatternMatchAction
// can only take actions in its body, no Logic Expressions.

// PatternMatchAction ::= MatchOperator '{' PatternMatchActionArm+ '},'

#[derive(Clone, Debug)]
pub struct PatternMatchActionExpr {
    // The data field of the MatchOperator is the identifier of the variable
    // to be matched on
    pub operator: MatchOperator,
    // All the match arms appearing in the source code, with an optional
    // guard, i.e. a condition on this variant.
    pub match_arms: Vec<PatternMatchActionArm>,
}

impl PatternMatchActionExpr {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, (variable_id, match_arms)) = context(
            "pattern match expression",
            context(
                "match expressions",
                tuple((
                    opt_ws(MatchOperator::parse),
                    delimited(
                        opt_ws(char('{')),
                        many1(context(
                            "match expression",
                            opt_ws(PatternMatchActionArm::parse),
                        )),
                        opt_ws(char('}')),
                    ),
                )),
            ),
        )(input)?;
        Ok((
            input,
            Self {
                operator: variable_id,
                match_arms,
            },
        ))
    }
}

//------------ ActionCallExpr -----------------------------------------------

// An invocation of an action, used in the Apply section only, it consists of
// the name of the actions plus an optional arguments list of variable names,
// whose values are to be passed in at runtime. The Action definition should
// have all the variables defined in a `with` statement.
//
// The fields of this struct are the same as `MethodComputeExpr`, but it gets
// treated differently at eval time.
#[derive(Clone, Debug)]
pub struct ActionCallExpr {
    pub action_id: Identifier,
    pub args: Option<ArgExprList>,
}

impl ActionCallExpr {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, (action_id, args)) = context(
            "action call expression",
            tuple((
                opt_ws(Identifier::parse),
                opt(delimited(char('('), ArgExprList::parse, char(')'))),
            )),
        )(input)?;

        Ok((input, Self { action_id, args }))
    }
}

//------------ TermActionExpr -----------------------------------------------

// The same as the ActionCallExpr and the MethodCallExpr, but for its
// treatment by the evaluator.

#[derive(Clone, Debug)]
pub struct TermCallExpr {
    pub term_id: Identifier,
    pub args: Option<ArgExprList>,
}

impl TermCallExpr {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, (term_id, args)) = context(
            "term call expression",
            tuple((
                opt_ws(Identifier::parse),
                opt(delimited(char('('), ArgExprList::parse, char(')'))),
            )),
        )(input)?;

        Ok((input, Self { term_id, args }))
    }
}

#[derive(Clone, Debug)]
pub struct PatternMatchActionArm {
    pub variant_id: Identifier,
    pub data_field: Option<Identifier>,
    pub guard: Option<TermCallExpr>,
    pub actions: Vec<(Option<ActionCallExpr>, Option<AcceptReject>)>,
}

impl PatternMatchActionArm {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        // let (input, (variant_id, data_field, guard, actions)) =
        let (input, (variant_id, data_field)) = context(
            "pattern match expression",
            tuple((
                opt_ws(Identifier::parse),
                opt(delimited(
                    opt_ws(char('(')),
                    Identifier::parse,
                    opt_ws(char(')')),
                )),
            )),
        )(input)?;

        let (input, guard) = opt(preceded(
            opt_ws(char('|')),
            opt_ws(TermCallExpr::parse),
        ))(input)?;

        let (input, actions) = preceded(
            opt_ws(tag("->")),
            // Either a ..
            alt((
                // bunch of action expressions, within curly
                // braces and semi-colon separated, and
                // terminated with an optional comma, or...
                delimited(
                    opt_ws(char('{')),
                    alt((
                        many1(context(
                            "Action Expression",
                            tuple((
                                map(
                                    opt_ws(terminated(
                                        ActionCallExpr::parse,
                                        opt_ws(char(';')),
                                    )),
                                    Some,
                                ),
                                opt(opt_ws(accept_reject)),
                            )),
                        )),
                        map(opt_ws(accept_reject), |ar| {
                            vec![(None, Some(ar))]
                        }),
                    )),
                    terminated(opt_ws(char('}')), opt_ws(char(','))),
                ),
                // a single action expression that must have a
                // comma at the end. Cannot end with a
                // accept_reject
                map(
                    terminated(
                        opt_ws(ActionCallExpr::parse),
                        opt_ws(char(',')),
                    ),
                    |l_e| vec![(Some(l_e), None)],
                ),
            )),
        )(input)?;

        Ok((
            input,
            Self {
                variant_id,
                guard,
                data_field,
                actions,
            },
        ))
    }
}

//------------ TermMatchExpr ------------------------------------------------

// A TermMatchExpr describes a variant of an enum together with its data field
// and one or more logical expressions, that will evaluate to a boolean, it
// may reference the data field. Note that this MatchExpr will be split out in
// (variant_id, data_field) and the logical expressions to be able to store it
// in a TermScope as `VariantMatchExpr`s. Since it only store Logical
// Expressions it is only fit for use in a Term section. In the Apply sections
// the PatternMatchActionExpr is used.

// MatchExpr := Identifier '(' Identifier ')' '->' (( LogicalExpr ';' ',' ) |
//  '{'  ( LogicalExpr ';' )+ '}' ','? )
#[derive(Clone, Debug)]
pub struct TermMatchExpr {
    pub variant_id: Identifier,
    pub data_field: Option<Identifier>,
    pub logical_expr: Vec<LogicalExpr>,
}

impl TermMatchExpr {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, (variant_id, data_field, logical_expr)) = context(
            "match expression",
            tuple((
                opt_ws(Identifier::parse),
                opt(delimited(
                    opt_ws(char('(')),
                    Identifier::parse,
                    opt_ws(char(')')),
                )),
                preceded(
                    opt_ws(tag("->")),
                    // Either a ..
                    alt((
                        // bunch of logical expressions, within curly
                        // braces and semi-colon separated, and
                        // terminated with an optional comma, or...
                        terminated(
                            delimited(
                                opt_ws(char('{')),
                                many1(terminated(
                                    LogicalExpr::parse,
                                    opt_ws(char(';')),
                                )),
                                opt_ws(char('}')),
                            ),
                            opt(opt_ws(char(','))),
                        ),
                        // a single logical expression that must
                        // have a comma at the end.
                        map(
                            terminated(LogicalExpr::parse, opt_ws(char(','))),
                            |l_e| vec![l_e],
                        ),
                    )),
                ),
            )),
        )(
            input
        )?;
        Ok((
            input,
            Self {
                variant_id,
                data_field,
                logical_expr,
            },
        ))
    }
}

//------------ Rib -----------------------------------------------------------

// Rib ::= "rib" Identifier 'contains' TypeIdentifier '{' RibBody '}'

#[derive(Clone, Debug)]
pub struct Rib {
    pub ident: Identifier,
    pub contain_ty: TypeIdentifier,
    pub body: RibBody,
}

impl Rib {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, (ident, contain_ty, body, _)) = context(
            "rib definition",
            tuple((
                preceded(
                    tag("rib"),
                    context(
                        "rib name",
                        delimited(
                            multispace1,
                            Identifier::parse,
                            multispace1,
                        ),
                    ),
                ),
                context(
                    "contains",
                    preceded(
                        opt_ws(tag("contains")),
                        delimited(
                            multispace1,
                            TypeIdentifier::parse,
                            multispace1,
                        ),
                    ),
                ),
                context(
                    "rib block",
                    cut(delimited(
                        opt_ws(char('{')),
                        RibBody::parse,
                        opt_ws(char('}')),
                    )),
                ),
                map(skip_opt_ws, |_| ()),
            )),
        )(input)?;

        Ok((
            input,
            Rib {
                ident,
                contain_ty,
                body,
            },
        ))
    }
}

#[derive(Clone, Debug)]
pub struct RibBody {
    pub key_values: Vec<RibField>,
}

#[derive(Clone, Debug)]
pub enum RibField {
    PrimitiveField(TypeIdentField),
    RecordField(Box<(Identifier, RecordTypeIdentifier)>),
    ListField(Box<(Identifier, ListTypeIdentifier)>),
}

//------------ RibBody -------------------------------------------------------

//
// The body of a Rib consists of an (optional) enumeration of (field_name,
// type) pairs.

// RibBody ::= ( Identifier ':' (
//                 TypeIdentifier |
//                 '{' RecordTypeIdentifier '}' |
//                 '[' ListTypeIdentifier ']' ) ','? )+

impl RibBody {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, key_values) = context(
            "items list",
            separated_list1(
                char(','),
                opt_ws(cut(alt((
                    map(TypeIdentField::parse, RibField::PrimitiveField),
                    map(
                        tuple((
                            terminated(
                                opt_ws(Identifier::parse),
                                opt_ws(char(':')),
                            ),
                            delimited(
                                opt_ws(char('{')),
                                RecordTypeIdentifier::parse,
                                opt_ws(char('}')),
                            ),
                        )),
                        |r| RibField::RecordField(Box::new(r)),
                    ),
                    map(
                        tuple((
                            terminated(
                                opt_ws(Identifier::parse),
                                opt_ws(char(':')),
                            ),
                            delimited(
                                opt_ws(char('[')),
                                ListTypeIdentifier::parse,
                                opt_ws(char(']')),
                            ),
                        )),
                        |l| RibField::ListField(Box::new(l)),
                    ),
                )))),
            ),
        )(input)?;
        Ok((input, RibBody { key_values }))
    }
}

//------------ Table -----------------------------------------------------

// Table ::= "table" Identifier 'contains' TypeIdentifier '{' TableBody '}'

#[derive(Clone, Debug)]
pub struct Table {
    pub ident: Identifier,
    pub contain_ty: TypeIdentifier,
    pub body: RibBody,
}

impl Table {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, (ident, contain_ty, body, _)) = context(
            "table definition",
            tuple((
                preceded(
                    opt_ws(tag("table")),
                    context(
                        "table name",
                        delimited(
                            multispace1,
                            Identifier::parse,
                            multispace1,
                        ),
                    ),
                ),
                context(
                    "contains",
                    preceded(
                        opt_ws(tag("contains")),
                        delimited(
                            multispace1,
                            TypeIdentifier::parse,
                            multispace1,
                        ),
                    ),
                ),
                context(
                    "table block",
                    cut(delimited(
                        opt_ws(char('{')),
                        RibBody::parse,
                        opt_ws(char('}')),
                    )),
                ),
                map(skip_opt_ws, |_| ()),
            )),
        )(input)?;

        Ok((
            input,
            Table {
                ident,
                contain_ty,
                body,
            },
        ))
    }
}

//------------ OutputStream -------------------------------------------------

// Table ::= "table" Identifier 'contains' TypeIdentifier '{' TableBody '}'

#[derive(Clone, Debug)]
pub struct OutputStream {
    pub ident: Identifier,
    pub contain_ty: TypeIdentifier,
    pub body: RibBody,
}

impl OutputStream {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, (ident, contain_ty, body, _)) = context(
            "output-stream definition",
            tuple((
                preceded(
                    opt_ws(tag("output-stream")),
                    context(
                        "output-stream name",
                        delimited(
                            multispace1,
                            Identifier::parse,
                            multispace1,
                        ),
                    ),
                ),
                context(
                    "contains",
                    preceded(
                        opt_ws(tag("contains")),
                        delimited(
                            multispace1,
                            TypeIdentifier::parse,
                            multispace1,
                        ),
                    ),
                ),
                context(
                    "output-stream block",
                    cut(delimited(
                        opt_ws(char('{')),
                        RibBody::parse,
                        opt_ws(char('}')),
                    )),
                ),
                map(skip_opt_ws, |_| ()),
            )),
        )(input)?;

        Ok((
            input,
            OutputStream {
                ident,
                contain_ty,
                body,
            },
        ))
    }
}

//============ Separators ====================================================

/// Parses something preceded by optional white space.
fn opt_ws<'a, O, F>(
    parse: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, VerboseError<&str>>
where
    F: FnMut(&'a str) -> IResult<&str, O, VerboseError<&str>>,
{
    preceded(skip_opt_ws, parse)
}

/// Optional white space.
///
/// White space is all actual white space characters plus comments.
fn skip_opt_ws(input: &str) -> IResult<&str, (), VerboseError<&str>> {
    fold_many0(alt((map(multispace1, |_| ()), comment)), || (), |_, _| ())(
        input,
    )
}

/// Comments start with a hash and run to the end of a line.
fn comment(input: &str) -> IResult<&str, (), VerboseError<&str>> {
    let (input, _) = tuple((tag("//"), take_until("\n")))(input)?;
    map(tag_char('\n'), |_| ())(input)
}

//------------ Identifier ----------------------------------------------------

/// An identifier is the name of variables or other things.
///
/// It is a word composed of a leading alphabetic Unicode character, followed
/// by alphanumeric Unicode characters or underscore or hyphen.
///
/// Identifier ::= ([a-z]) ([0-9a-z-_])*
///
#[derive(Clone, Debug, Ord, PartialOrd)]
pub struct Identifier {
    /// The actual identifier.
    pub ident: ShortString,
}

impl Identifier {
    fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, _) = context(
            "no keyword",
            not(delimited(
                multispace1,
                tuple((
                    tag("for"),
                    tag("type"),
                    tag("with"),
                    tag("in"),
                    tag("not"),
                    tag("define"),
                    tag("filter-map"),
                    tag("import"),
                    tag("term"),
                    tag("filter"),
                    tag("match"),
                    tag("route"),
                    tag("matches"),
                    tag("return"),
                    tag("prefix"),
                    tag("true"),
                    tag("false"),
                    tag("apply"),
                    tag("use"),
                    tag("type"),
                )),
                multispace1,
            )),
        )(input)?;
        let (input, ident) = context(
            "identifier",
            recognize(preceded(
                take_while1(|ch: char| {
                    ch.is_alphabetic() || ch == '_' || ch == '-'
                }),
                take_while(|ch: char| {
                    ch.is_alphanumeric() || ch == '_' || ch == '-'
                }),
            )),
        )(input)?;

        Ok((
            input,
            Identifier {
                ident: ident.into(),
            },
        ))
    }
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

//------------ TypeIdentifier -----------------------------------------------

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

/// A TypeIdentifier is the unique name of a type.

/// TypeIdentifier ::= [A-Z] ([0-9a-zA-Z])*

impl TypeIdentifier {
    fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, ident) = recognize(pair(
            take_while1(|ch: char| ch.is_alphabetic() && ch.is_uppercase()),
            take_while(|ch: char| ch.is_alphanumeric()),
        ))(input)?;
        Ok((
            input,
            TypeIdentifier {
                ident: ident.into(),
            },
        ))
    }
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

//------------ TypeIdentField ------------------------------------------------

/// A `field_name: Type` pair.

#[derive(Clone, Debug)]
pub struct TypeIdentField {
    /// The name of the field.
    pub field_name: Identifier,
    /// The type of the field.
    pub ty: TypeIdentifier,
}

impl TypeIdentField {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, (field_name, ty)) = context(
            "key-value pair",
            separated_pair(
                preceded(multispace0, Identifier::parse),
                preceded(multispace0, char(':')),
                preceded(multispace0, TypeIdentifier::parse),
            ),
        )(input)?;

        Ok((input, Self { field_name, ty }))
    }
}

//------------ ListTypeIdentifier -------------------------------------------

// ListTypeIdentifier ::= '[' TypeIdentifier  ','? ']'+

#[derive(Clone, Debug)]
pub struct ListTypeIdentifier {
    pub inner_type: TypeIdentifier,
}

impl ListTypeIdentifier {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, inner_type) =
            context("List Value", TypeIdentifier::parse)(input)?;
        Ok((input, ListTypeIdentifier { inner_type }))
    }
}

//------------ StringLiteral -----------------------------------------------

// Our take on a literal string is just a Identifier wrapped in two double
// quotes. We don't do any escaping or anything like that.

// StringLiteral ::= '"' Identifier '"'
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct StringLiteral(pub(crate) String);

impl StringLiteral {
    fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, string) = parse_string::parse_string(input)?;
        Ok((input, Self(string)))
    }
}

impl From<StringLiteral> for String {
    fn from(literal: StringLiteral) -> Self {
        literal.0
    }
}

//------------ RecordTypeIdentifier -----------------------------------------

// The user-defined type of a record. It's very similar to a RibBody (in EBNF
// it's the same), but it simplifies creating the SymbolTable, because they're
// semantically different.

// RecordTypeIdentifier ::= '{' ( Identifier ':'
//                          RecordTypeIdentifier | '{' RecordBody '}'
//                      ','? )+ '}'

#[derive(Clone, Debug)]
pub struct RecordTypeIdentifier {
    pub key_values: Vec<RibField>,
}

impl RecordTypeIdentifier {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, key_values) = context(
            "Record Value",
            separated_list1(
                char(','),
                opt_ws(cut(alt((
                    map(TypeIdentField::parse, RibField::PrimitiveField),
                    map(
                        tuple((
                            terminated(
                                opt_ws(Identifier::parse),
                                opt_ws(char(':')),
                            ),
                            delimited(
                                opt_ws(char('[')),
                                ListTypeIdentifier::parse,
                                opt_ws(char(']')),
                            ),
                        )),
                        |r| RibField::ListField(Box::new(r)),
                    ),
                    map(
                        tuple((
                            terminated(
                                opt_ws(Identifier::parse),
                                opt_ws(char(':')),
                            ),
                            delimited(
                                opt_ws(char('{')),
                                RecordTypeIdentifier::parse,
                                opt_ws(char('}')),
                            ),
                        )),
                        |r| RibField::RecordField(Box::new(r)),
                    ),
                )))),
            ),
        )(input)?;
        Ok((input, RecordTypeIdentifier { key_values }))
    }
}

//============= Literals ====================================================

//------------ IntegerLiteral -----------------------------------------------

/// An integer literal is a sequence of digits.
/// IntegerLiteral ::= [0-9]+
///
/// We parse it as a string and then convert it to an integer.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IntegerLiteral(pub i64);

impl IntegerLiteral {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, digits) = context(
            "integer literal",
            recognize(pair(
                opt(char('-')),
                take_while1(|ch: char| ch.is_ascii_digit()),
            )),
        )(input)?;

        let value = digits.parse::<i64>().map_err(|e| {
            nom::Err::Failure(VerboseError::from_external_error(
                input,
                nom::error::ErrorKind::Char,
                e,
            ))
        })?;
        Ok((input, Self(value)))
    }
}

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

impl PrefixLengthLiteral {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, digits) = context(
            "prefix length literal",
            preceded(char('/'), take_while1(|ch: char| ch.is_ascii_digit())),
        )(input)?;

        let value = digits.parse::<u8>().map_err(|e| {
            nom::Err::Failure(VerboseError::from_external_error(
                input,
                nom::error::ErrorKind::Char,
                e,
            ))
        })?;
        Ok((input, Self(value)))
    }
}

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

impl HexLiteral {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, digits) = context(
            "hex literal",
            recognize(pair(
                tag("0x"),
                take_while1(|ch: char| ch.is_ascii_hexdigit()),
            )),
        )(input)?;

        let value = u64::from_str_radix(&digits[2..], 16).map_err(|e| {
            nom::Err::Failure(VerboseError::from_external_error(
                input,
                nom::error::ErrorKind::HexDigit,
                e,
            ))
        })?;
        Ok((input, Self(value)))
    }
}

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
///
///
#[derive(Clone, Debug)]

pub struct AsnLiteral(pub u32);

// To avoid a parse collision with an extended community literal i.e
// 'AS{}:{}:{}', we are checking that the literal is not followed by a ':'.
impl AsnLiteral {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, digits) = context(
            "ASN literal",
            tuple((
                recognize(pair(
                    tag("AS"),
                    take_while1(|ch: char| ch.is_ascii_hexdigit()),
                )),
                not(char(':')),
            )),
        )(input)?;

        let value = digits.0[2..].parse::<u32>().map_err(|e| {
            nom::Err::Failure(VerboseError::from_external_error(
                input,
                nom::error::ErrorKind::AlphaNumeric,
                e,
            ))
        })?;
        Ok((input, Self(value)))
    }
}

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

//------------ StandardCommunityLiteral --------------------------------------

#[derive(Clone, Debug)]
pub struct StandardCommunityLiteral(pub String);

impl StandardCommunityLiteral {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, std_comm) = context(
            "Standard Community Literal",
            tuple((
                opt(tag("0x")),
                take_while_m_n(1, 5, |ch: char| ch.is_hex_digit()),
                char(':'),
                opt(tag("0x")),
                take_while_m_n(1, 5, |ch: char| ch.is_hex_digit()),
                not(char(':')),
            )),
        )(input)?;

        // Let routecore do all the heavy lifting here: Not erroring out here
        // on an invalid tag, or too big a AS or AN, because the eval phase of
        // the compilation (that uses routecore's `from_str` will error out
        // with a way more useful error. Also syntactically it might not be
        // wrong to have unknown tags, or too big a AS or AN here.

        // See tests in bgp_filters.rs for the kind of errors the eval() phase
        // returns.

        // What we are doing here is catering for both decimal and hexadecimal
        // literals provided. Hexadecimals will be converted into decimal, so
        // that the from_str method from routecore can process them. Again, we
        // are not trying to parse here as tightly fitted as possible (that
        // would be a u16 here), we're parsing the largest possible type, so
        // that the eval phase can optionally error out.
        let value_1 = if let Some(_hex_tag) = std_comm.0 {
            u64::from_str_radix(std_comm.1, 16).map_err(|e| {
                nom::Err::Failure(VerboseError::from_external_error(
                    input,
                    nom::error::ErrorKind::HexDigit,
                    e,
                ))
            })?
        } else {
            std_comm.1.parse::<u64>().map_err(|e| {
                nom::Err::Failure(VerboseError::from_external_error(
                    input,
                    nom::error::ErrorKind::Digit,
                    e,
                ))
            })?
        };

        let value_2 = if let Some(_hex_tag) = std_comm.3 {
            u64::from_str_radix(std_comm.4, 16).map_err(|e| {
                nom::Err::Failure(VerboseError::from_external_error(
                    input,
                    nom::error::ErrorKind::HexDigit,
                    e,
                ))
            })?
        } else {
            std_comm.4.parse::<u64>().map_err(|e| {
                nom::Err::Failure(VerboseError::from_external_error(
                    input,
                    nom::error::ErrorKind::Digit,
                    e,
                ))
            })?
        };

        // See tests in bgp_filters.rs for the kind of errors the eval() phase
        // returns.
        Ok((input, Self(format!("{}:{}", value_1, value_2))))
    }
}

impl From<&'_ StandardCommunityLiteral> for ShortString {
    fn from(literal: &StandardCommunityLiteral) -> Self {
        ShortString::from(literal.0.to_string().as_str())
    }
}

impl TryFrom<&'_ StandardCommunityLiteral> for Community {
    type Error = CompileError;

    // The aforementioned heavy lifting is here.
    fn try_from(
        literal: &StandardCommunityLiteral,
    ) -> Result<Self, Self::Error> {
        let comm = <StandardCommunity as str::FromStr>::from_str(
                &literal.0
            ).map_err(
                |e| CompileError::from(format!(
                    "Cannot convert literal '{}' into Extended Community: {e}", literal.0,
                )))?;

        Ok(
            comm.into(),
        )
    }
}

//------------ ExtendedCommunityLiteral --------------------------------------

#[derive(Clone, Debug)]
pub struct ExtendedCommunityLiteral(pub String);

// To avoid a collision with a IPv6 address, which can never be three hex
// blocks, we make sure that the ext. community here is never followed by a
// ':'
impl ExtendedCommunityLiteral {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, ext_comm) = context(
            "Extended Community Literal",
            tuple((
                take_while1(|ch: char| ch.is_alpha()),
                tag(":"),
                opt(tag("0x")),
                take_while1(|ch: char| ch.is_hex_digit()),
                tag(":"),
                opt(tag("0x")),
                take_while_m_n(1, 5, |ch: char| ch.is_hex_digit()),
                not(char(':')),
            )),
        )(input)?;

        // Let routecore do all the heavy lifting here: Not erroring out here
        // on an invalid tag, or too big a AS or AN, because the eval phase of
        // the compilation (that uses routecore's `from_str` will error out
        // with a way more useful error. Also syntactically it might not be
        // wrong to have unknown tags, or too big a AS or AN here.

        // See tests in bgp_filters.rs for the kind of errors the eval() phase
        // returns.

        // What we are doing here is catering for both decimal and hexadecimal
        // literals provided. Hexadecimals will be converted into decimal, so
        // that the from_str method from routecore can process them. Again, we
        // are not trying to parse here as tightly fitted as possible (that
        // would be a u16 here), we're parsing the largest possible type, so
        // that the eval phase can optionally error out.
        let value_1 = if let Some(_hex_tag) = ext_comm.2 {
            u64::from_str_radix(ext_comm.3, 16).map_err(|e| {
                nom::Err::Failure(VerboseError::from_external_error(
                    input,
                    nom::error::ErrorKind::HexDigit,
                    e,
                ))
            })?
        } else {
            ext_comm.3.parse::<u64>().map_err(|e| {
                nom::Err::Failure(VerboseError::from_external_error(
                    input,
                    nom::error::ErrorKind::Digit,
                    e,
                ))
            })?
        };

        let value_2 = if let Some(_hex_tag) = ext_comm.5 {
            u32::from_str_radix(ext_comm.6, 16).map_err(|e| {
                nom::Err::Failure(VerboseError::from_external_error(
                    input,
                    nom::error::ErrorKind::HexDigit,
                    e,
                ))
            })?
        } else {
            ext_comm.6.parse::<u32>().map_err(|e| {
                nom::Err::Failure(VerboseError::from_external_error(
                    input,
                    nom::error::ErrorKind::Digit,
                    e,
                ))
            })?
        };

        Ok((input, Self(format!("{}:{value_1}:{value_2}", ext_comm.0))))
    }
}

impl From<&'_ ExtendedCommunityLiteral> for ShortString {
    fn from(literal: &ExtendedCommunityLiteral) -> Self {
        ShortString::from(literal.0.to_string().as_str())
    }
}

impl<'a> TryFrom<&'a ExtendedCommunityLiteral> for Community {
    type Error = CompileError;

    // The aforementioned heavy lifting is here.
    fn try_from(
        literal: &ExtendedCommunityLiteral,
    ) -> Result<Self, Self::Error> {
        let comm = <ExtendedCommunity as str::FromStr>::from_str(
                &literal.0
            ).map_err(
                |e| CompileError::from(format!(
                    "Cannot convert literal '{}' into Extended Community: {e}", literal.0,
                )))?;

        Ok(
            comm.into()
        )
    }
}

//------------ LargeCommunityLiteral -----------------------------------------

#[derive(Clone, Debug)]
pub struct LargeCommunityLiteral(pub String);

impl LargeCommunityLiteral {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, l_comm) = context(
            "Large Community Literal",
            tuple((
                opt(alt((tag("AS"), tag("0x")))),
                take_while1(|ch: char| ch.is_hex_digit()),
                char(':'),
                opt(tag("0x")),
                take_while1(|ch: char| ch.is_hex_digit()),
                char(':'),
                opt(tag("0x")),
                take_while1(|ch: char| ch.is_hex_digit()),
                not(char(':'))
            )),
        )(input)?;

        // Let routecore do all the heavy lifting here: Not erroring out here
        // on an invalid tag, or too big a AS or AN, because the eval phase of
        // the compilation (that uses routecore's `from_str` will error out
        // with a way more useful error. Also syntactically it might not be
        // wrong to have unknown tags, or too big a AS or AN here.

        // See tests in bgp_filters.rs for the kind of errors the eval() phase
        // returns.

        // What we are doing here is catering for both decimal and hexadecimal
        // literals provided. Hexadecimals will be converted into decimal, so
        // that the from_str method from routecore can process them. Again, we
        // are not trying to parse here as tightly fitted as possible (that
        // would be a u16 here), we're parsing the largest possible type, so
        // that the eval phase can optionally error out.
        let value_1 = match l_comm.0 {
            Some("0x") => u64::from_str_radix(l_comm.1, 16).map_err(|e| {
                nom::Err::Failure(VerboseError::from_external_error(
                    input,
                    nom::error::ErrorKind::HexDigit,
                    e,
                ))
            })?,
            _ => l_comm.1.parse::<u64>().map_err(|e| {
                nom::Err::Failure(VerboseError::from_external_error(
                    input,
                    nom::error::ErrorKind::Digit,
                    e,
                ))
            })?,
        };

        let value_2 = if let Some(_hex_tag) = l_comm.3 {
            u32::from_str_radix(l_comm.4, 16).map_err(|e| {
                nom::Err::Failure(VerboseError::from_external_error(
                    input,
                    nom::error::ErrorKind::HexDigit,
                    e,
                ))
            })?
        } else {
            l_comm.4.parse::<u32>().map_err(|e| {
                nom::Err::Failure(VerboseError::from_external_error(
                    input,
                    nom::error::ErrorKind::Digit,
                    e,
                ))
            })?
        };

        let value_3 = if let Some(_hex_tag) = l_comm.6 {
            u32::from_str_radix(l_comm.7, 16).map_err(|e| {
                nom::Err::Failure(VerboseError::from_external_error(
                    input,
                    nom::error::ErrorKind::HexDigit,
                    e,
                ))
            })?
        } else {
            l_comm.7.parse::<u32>().map_err(|e| {
                nom::Err::Failure(VerboseError::from_external_error(
                    input,
                    nom::error::ErrorKind::Digit,
                    e,
                ))
            })?
        };

        Ok((
            input,
            Self(format!(
                "{}{value_1}:{value_2}:{value_3}",
                if let Some("AS") = l_comm.0 { "AS" } else { "" }
            )),
        ))
    }
}

impl From<&'_ LargeCommunityLiteral> for ShortString {
    fn from(literal: &LargeCommunityLiteral) -> Self {
        ShortString::from(literal.0.to_string().as_str())
    }
}

impl TryFrom<&'_ LargeCommunityLiteral> for Community {
    type Error = CompileError;

    // The aforementioned heavy lifting is here.
    fn try_from(
        literal: &LargeCommunityLiteral,
    ) -> Result<Self, Self::Error> {
        let comm = <LargeCommunity as str::FromStr>::from_str(
                &literal.0
            ).map_err(
                |e| CompileError::from(format!(
                    "Cannot convert literal '{}' into Large Community: {e}", literal.0,
                )))?;

        Ok(comm.into())
    }
}

//------------ FloatLiteral --------------------------------------------------

/// A float literal is a sequence of digits with a decimal point.
/// FloatLiteral ::= [0-9]+ '.' [0-9]+
///
#[derive(Clone, Debug)]
pub struct FloatLiteral(pub f64);

impl FloatLiteral {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, digits) = context(
            "float literal",
            recognize(tuple((
                opt(char('-')),
                take_while1(|ch: char| ch.is_ascii_digit()),
                char('.'),
                take_while1(|ch: char| ch.is_ascii_digit()),
            ))),
        )(input)?;

        let value = digits.parse::<f64>().map_err(|e| {
            nom::Err::Failure(VerboseError::from_external_error(
                input,
                nom::error::ErrorKind::AlphaNumeric,
                e,
            ))
        })?;
        Ok((input, Self(value)))
    }
}

//------------ BooleanLiteral -----------------------------------------------
/// A boolean literal is either `true` or `false`.
/// BooleanLiteral ::= 'true' | 'false'
#[derive(Clone, Debug)]

pub struct BooleanLiteral(pub bool);

impl BooleanLiteral {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, value) = alt((
            map(tag("true"), |_| Self(true)),
            map(tag("false"), |_| Self(false)),
        ))(input)?;

        Ok((input, value))
    }
}

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

//------------ ByteStringLiteral --------------------------------------------
/// A byte string literal is a sequence of bytes, preceded by '0x'
/// ByteStringLiteral ::= 0x[0-9a-fA-F]+

#[derive(Clone, Debug)]
pub struct ByteStringLiteral(pub SmallVec<[u8; 24]>);

impl<'a> ByteStringLiteral {
    pub fn parse(input: &'a str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, value) = (preceded(
            tag("0x"),
            many1(terminated(take(2_usize), opt(char('_')))),
        ))(input)?;

        let mut result_value: SmallVec<[u8; 24]> = SmallVec::new();

        for v in &value {
            let v = u8::from_str_radix(v, 16);

            if let Ok(v) = v {
                result_value.push(v);
            } else {
                return Err(nom::Err::Error(VerboseError::from_error_kind(
                    input,
                    ErrorKind::Digit,
                )));
            }
        }

        Ok((input, Self(result_value)))
    }
}

//------------ AcceptReject -------------------------------------------------

// Every filter needs to return either a 'accept' or 'reject' statement.
// failing to set it properly ends in the whole thing being cancelled.

// AcceptReject ::= 'return'? ( 'accept' | 'reject' ) ';'

#[derive(
    Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Serialize,
)]
pub enum AcceptReject {
    Accept,
    Reject,
    NoReturn,
}

fn accept_reject(
    input: &str,
) -> IResult<&str, AcceptReject, VerboseError<&str>> {
    context(
        "accept or reject",
        preceded(
            opt(opt_ws(tag("return"))),
            alt((
                map(
                    terminated(opt_ws(tag("accept")), opt_ws(char(';'))),
                    |_| AcceptReject::Accept,
                ),
                map(
                    terminated(opt_ws(tag("reject")), opt_ws(char(';'))),
                    |_| AcceptReject::Reject,
                ),
            )),
        ),
    )(input)
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

//------------ LiteralExpr ---------------------------------------------------

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
    BooleanLiteral(BooleanLiteral)
}

impl LiteralExpr {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        alt((
            map(StringLiteral::parse, LiteralExpr::StringLiteral),
            map(
                ExtendedCommunityLiteral::parse,
                LiteralExpr::ExtendedCommunityLiteral,
            ),
            map(
                LargeCommunityLiteral::parse,
                LiteralExpr::LargeCommunityLiteral,
            ),
            map(
                StandardCommunityLiteral::parse,
                LiteralExpr::StandardCommunityLiteral,
            ),
            map(Prefix::parse, LiteralExpr::PrefixLiteral),
            map(IpAddress::parse, LiteralExpr::IpAddressLiteral),
            map(AsnLiteral::parse, LiteralExpr::AsnLiteral),
            map(HexLiteral::parse, LiteralExpr::HexLiteral),
            map(IntegerLiteral::parse, LiteralExpr::IntegerLiteral),
            map(PrefixLengthLiteral::parse, LiteralExpr::PrefixLengthLiteral),
            map(tag("true"), |_| LiteralExpr::BooleanLiteral(BooleanLiteral(true))),
            map(tag("false"), |_| {
                LiteralExpr::BooleanLiteral(BooleanLiteral(false))
            })
        ))(input)
    }
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

//------------ LiteralAccessExpr ---------------------------------------------

#[derive(Clone, Debug)]
pub struct LiteralAccessExpr {
    pub literal: LiteralExpr,
    pub access_expr: Vec<AccessExpr>
}

impl LiteralAccessExpr {
    fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, (literal, access_expr)) = tuple((
            LiteralExpr::parse,
            many0(preceded(char('.'), AccessExpr::parse)),
        ))(input)?;

        Ok((input, Self { literal, access_expr }))
    }
}

//------------ ValueExpr -----------------------------------------------------

// ValueExpr ::= LiteralAccessExpr | PrefixMatchExpr | ComputeExpr |
// RootMethodCallExpr | TypedRecordExpr | ListExpr

// An expression that ultimately will resolve into a TypeValue, e.g. the
// right-hand of an assignment, or an argument to a method, etc.

#[derive(Clone, Debug)]
pub enum ValueExpr {
    // a literal, or a chain of field accesses and/or methods on a literal,
    // e.g. `10.0.0.0/8.covers(..)`
    LiteralAccessExpr(LiteralAccessExpr),
    // a JunOS style prefix match expression, e.g. `0.0.0.0/0
    // prefix-length-range /12-/16`
    PrefixMatchExpr(PrefixMatchExpr),
    // an access receiver (an expression named with a single identifier, e.g.
    // `my_var`), or a chain of field accesses and/or methods on an access
    // receiver.
    ComputeExpr(ComputeExpr),
    // an expression of the form `word(argument)`, so nothing in front of
    // `word`, this would be something like a builtin method call, or an
    // action or term with an argument.
    RootMethodCallExpr(MethodComputeExpr),
    // a record that doesn't have a type mentioned in the assignment of it,
    // e.g `{ value_1: 100, value_2: "bla" }`. This can also be a sub-record
    // of a record that does have an explicit type.
    AnonymousRecordExpr(AnonymousRecordValueExpr),
    // an expression of a record that does have a type, e.g. `MyType {
    // value_1: 100, value_2: "bla" }`, where MyType is a user-defined Record
    // Type.
    TypedRecordExpr(TypedRecordValueExpr),
    // An expression that yields a list of values, e.g. `[100, 200, 300]`
    ListExpr(ListValueExpr),
}

impl ValueExpr {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, value_expr) = alt((
            map(ListValueExpr::parse, ValueExpr::ListExpr),
            map(
                AnonymousRecordValueExpr::parse,
                ValueExpr::AnonymousRecordExpr,
            ),
            map(TypedRecordValueExpr::parse, ValueExpr::TypedRecordExpr),
            map(PrefixMatchExpr::parse, ValueExpr::PrefixMatchExpr),
            map(LiteralAccessExpr::parse, ValueExpr::LiteralAccessExpr),
            map(MethodComputeExpr::parse, ValueExpr::RootMethodCallExpr),
            map(ComputeExpr::parse, ValueExpr::ComputeExpr),
        ))(input)?;

        Ok((input, value_expr))
    }
}

//------------ ArgExprList -------------------------------------------------

// ArgExprList ::= ( ValueExpr ','? )*

#[derive(Clone, Debug)]
pub struct ArgExprList {
    pub args: Vec<ValueExpr>,
}

impl ArgExprList {
    fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, args) = context(
            "argument expressions",
            separated_list0(
                preceded(multispace0, char(',')),
                opt_ws(ValueExpr::parse),
            ),
        )(input)?;

        Ok((input, Self { args }))
    }

    pub fn is_empty(&self) -> bool {
        self.args.is_empty()
    }
}

//------------ for & with statements ----------------------------------------

// ForStatement ::= ('for' Identifier':' TypeIdentifier)?

fn for_statement(
    input: &str,
) -> IResult<&str, Option<TypeIdentField>, VerboseError<&str>> {
    context(
        "for",
        opt(preceded(opt_ws(tag("for")), TypeIdentField::parse)),
    )(input)
}

// WithStatement ::= ('with' (Identifier':' TypeIdentifier)+)?

fn with_statement(
    input: &str,
) -> IResult<&str, Option<Vec<TypeIdentField>>, VerboseError<&str>> {
    context(
        "with",
        opt(preceded(
            opt_ws(tag("with")),
            separated_list1(char(','), TypeIdentField::parse),
        )),
    )(input)
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
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        alt((
            map(MethodComputeExpr::parse, AccessExpr::MethodComputeExpr),
            map(FieldAccessExpr::parse, AccessExpr::FieldAccessExpr),
        ))(input)
    }

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

//------------- ComputeExpr ----------------------------------------------------

// It's complete EBNF would be:
// CompoundExpr ::= AccessReceiver(.MethodComputeExpr)? | AccessReceiver

// A ComputeExpr is an expression that starts with an access receiver,
// optionally followed by one or more method calls, and/or access receivers,
// e.g. 'rib-rov.longest_match(route.prefix).prefix.len()`.
//
// Note that an expression ending in a field access, i.e. not a method call,
// is also parsed as a ComputeExpr, but with an empty method call list.

#[derive(Clone, Debug)]
pub struct ComputeExpr {
    pub receiver: AccessReceiver,
    pub access_expr: Vec<AccessExpr>,
}

impl ComputeExpr {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, (receiver, access_expr)) = tuple((
            AccessReceiver::parse,
            many0(preceded(char('.'), AccessExpr::parse)),
        ))(input)?;
        Ok((
            input,
            Self {
                receiver,
                access_expr,
            },
        ))
    }

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

//------------- AccessReceiver ------------------------------------------------

// The AccessReceiver is the specifier of a data structure that is being called
// (used as part of a ComputeExpr) or used to retrieve one of its fields. Can
// also be a stand-alone specifier.

// AccessReceiver ::= Identifier | GlobalScope

#[derive(Clone, Debug)]
pub enum AccessReceiver {
    // The identifier of the data structure.
    Ident(Identifier),
    // or it can only be in the Global Scope (for global methods), it doesn't
    // have a string as identifier then.
    GlobalScope,
}

impl AccessReceiver {
    fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, receiver) =
            context("access receiver", Identifier::parse)(input)?;

        Ok((input, Self::Ident(receiver)))
    }
}

impl AccessReceiver {
    pub fn get_ident(&self) -> Option<&Identifier> {
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
    pub field_names: Vec<Identifier>,
}

impl FieldAccessExpr {
    fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, field_names) = context(
            "field expression",
            separated_list1(
                char('.'),
                terminated(Identifier::parse, not(char('('))),
            ),
        )(input)?;
        Ok((input, Self { field_names }))
    }
}

//------------- MethodComputeExpr  ------------------------------------------

// The method that is being called on the data structure (directly or on one
// of its fields).

// MethodComputeExpr ::= Identifier '(' ArgExprList ')'

#[derive(Clone, Debug)]
pub struct MethodComputeExpr {
    // The name of the method.
    pub ident: Identifier,
    // The list with arguments
    pub args: ArgExprList,
}

impl MethodComputeExpr {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, (ident, args)) = context(
            "method call expression",
            tuple((
                Identifier::parse,
                delimited(
                    opt_ws(char('(')),
                    ArgExprList::parse,
                    opt_ws(char(')')),
                ),
            )),
        )(input)?;
        Ok((input, Self { ident, args }))
    }
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

//------------ LogicalExpr --------------------------------------------------

// The Logical expression evaluates to a logical formula, that is a tuple of
// (optional boolean expression, logical operator, boolean expression). The
// first boolean expression is optional, only in the case of a negation (not)
// operator. The second boolean expression is always present.

// LogicalExpr ::= OrExpr | AndExpr | NotExpr | BooleanExpr

#[derive(Clone, Debug)]
pub enum LogicalExpr {
    OrExpr(OrExpr),
    AndExpr(AndExpr),
    NotExpr(NotExpr),
    BooleanExpr(BooleanExpr),
}

impl LogicalExpr {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        opt_ws(alt((
            map(AndExpr::parse, LogicalExpr::AndExpr),
            map(OrExpr::parse, LogicalExpr::OrExpr),
            map(NotExpr::parse, LogicalExpr::NotExpr),
            map(BooleanExpr::parse, LogicalExpr::BooleanExpr),
        )))(input)
    }
}

#[derive(Clone, Debug)]
pub enum CompareArg {
    // A "stand-alone" left|right-hand side argument of a comparison
    ValueExpr(ValueExpr),
    // A nested logical formula, e.g. (A && B) || (C && D) used as a left|
    // right-hand side argument of a comparison. Note that this can only
    // have the opposite hand be a boolean expression.
    GroupedLogicalExpr(GroupedLogicalExpr),
}

impl CompareArg {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        alt((
            map(GroupedLogicalExpr::parse, CompareArg::GroupedLogicalExpr),
            map(ValueExpr::parse, CompareArg::ValueExpr),
        ))(input)
    }
}

//------------ BooleanExpr --------------------------------------------------

// A Boolean expression is an expression that *may* evaluate to one of:
// - a Boolean-valued function, which is a fn : X → B, where X is an arbitrary
//   set and B is a boolean value. For example, an Integer expression can
//   never evaluate to a boolean value, but a method call expression may
//   evaluate to a method that returns a Boolean value.
// - a Literal Boolean value, "true" or "false"
// - a Boolean-typed variable, including boolean-typed record fields
// - an Expression containing a boolean-valued operator, such as '==', '!=',
//   ">=", "<="

// BooleanExpr ::= BooleanLiteral | ComputeExpr | CompareExpr
//          | ListCompareExpr | AccessReceiver | PrefixMatchExpr | Identifier

#[derive(Clone, Debug)]
pub enum BooleanExpr {
    // A complete formula that is wrapped in parentheses is a Boolean-Valued
    // Function, since it will always return a Boolean value.
    GroupedLogicalExpr(GroupedLogicalExpr),
    // "true" | "false" literals
    BooleanLiteral(BooleanLiteral),
    // A syntactically correct comparison always evaluates to a
    // Boolean-Valued Function, since it will always return a Boolean value.
    CompareExpr(Box<CompareExpr>),
    // A ComputeExpression *may* evaluate to a function that returns a boolean
    ComputeExpr(ComputeExpr),
    // Just like a ComputeExpr, a Literal, or a Literal access, e.g.
    // `10.0.0.0/16.covers()`, may return a boolean
    LiteralAccessExpr(LiteralAccessExpr),
    // Set Compare expression, will *always* result in a boolean-valued
    // function. Syntactic sugar for a truth-function that performs
    // fn : a -> {a} ∩ B
    ListCompareExpr(Box<ListCompareExpr>),
    // syntactic sugar for a method on a prefix function that returns a
    // boolean.
    PrefixMatchExpr(PrefixMatchExpr),
}

impl BooleanExpr {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        alt((
            map(GroupedLogicalExpr::parse, BooleanExpr::GroupedLogicalExpr),
            map(ListCompareExpr::parse, |e| {
                BooleanExpr::ListCompareExpr(Box::new(e))
            }),
            map(CompareExpr::parse, |e| {
                BooleanExpr::CompareExpr(Box::new(e))
            }),
            map(OptionalGlobalComputeExpr::parse, BooleanExpr::ComputeExpr),
            map(PrefixMatchExpr::parse, BooleanExpr::PrefixMatchExpr),
            map(LiteralAccessExpr::parse, BooleanExpr::LiteralAccessExpr),
            map(BooleanLiteral::parse, BooleanExpr::BooleanLiteral),
        ))(input)
    }
}

//------------ CompareExpr --------------------------------------------------

// CompareExpr ::= CompareArg CompareOp CompareArg

#[derive(Clone, Debug)]
pub struct CompareExpr {
    pub left: CompareArg,
    pub op: CompareOp,
    pub right: CompareArg,
}

impl CompareExpr {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, (left, op, right)) = context(
            "Compare Expression",
            tuple((
                opt_ws(CompareArg::parse),
                opt_ws(alt((
                    map(tag("=="), |_| CompareOp::Eq),
                    map(tag("!="), |_| CompareOp::Ne),
                    map(tag("<="), |_| CompareOp::Le),
                    map(tag("<"), |_| CompareOp::Lt),
                    map(tag(">="), |_| CompareOp::Ge),
                    map(tag(">"), |_| CompareOp::Gt),
                ))),
                opt_ws(CompareArg::parse),
            )),
        )(input)?;

        Ok((input, Self { left, op, right }))
    }
}

//------------ CompareOp ----------------------------------------------------

// CompareOp ::= '==' | '!=' | '<' | '<=' | '>' | '>='

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

//------------ AndExpr ------------------------------------------------------

// AndExpr ::= MatchExpr '&&' MatchExpr

#[derive(Clone, Debug)]
pub struct AndExpr {
    pub left: BooleanExpr,
    pub right: BooleanExpr,
}

impl AndExpr {
    fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, (left, right)) = tuple((
            opt_ws(BooleanExpr::parse),
            preceded(opt_ws(tag("&&")), opt_ws(BooleanExpr::parse)),
        ))(input)?;

        Ok((input, Self { left, right }))
    }
}

//------------ OrExpr -------------------------------------------------------

// OrExpr ::= MatchExpr '||' MatchExpr

#[derive(Clone, Debug)]
pub struct OrExpr {
    pub left: BooleanExpr,
    pub right: BooleanExpr,
}

impl OrExpr {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, (left, right)) = context(
            "or expr",
            tuple((
                opt_ws(BooleanExpr::parse),
                preceded(opt_ws(tag("||")), opt_ws(BooleanExpr::parse)),
            )),
        )(input)?;

        Ok((input, Self { left, right }))
    }
}

#[derive(Clone, Debug)]
pub struct NotExpr {
    pub expr: BooleanExpr,
}

impl NotExpr {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, expr) =
            preceded(opt_ws(tag("!")), opt_ws(BooleanExpr::parse))(input)?;
        Ok((input, Self { expr }))
    }
}

//------------ ListCompareExpr -----------------------------------------------

// ListCompareExpr ::= ( ValueExpr ( 'in' | 'not in' ) ValueExpr )+

#[derive(Clone, Debug)]
pub struct ListCompareExpr {
    pub left: ValueExpr,
    pub op: CompareOp,
    pub right: ValueExpr,
}

impl ListCompareExpr {
    fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, (left, op, right)) = tuple((
            opt_ws(ValueExpr::parse),
            alt((
                map(opt_ws(tag("in")), |_| CompareOp::In),
                map(opt_ws(tag("not in")), |_| CompareOp::NotIn),
            )),
            opt_ws(ValueExpr::parse),
        ))(input)?;

        trace!("ListCompareExpr {:?} {:?} {:?}", left, op, right);
        Ok((input, Self { left, op, right }))
    }
}

//------------- GroupedFormulaExpr ------------------------------------------

// GroupedFormulaExpr ::= '(' LogicalExpr ')'

#[derive(Clone, Debug)]
pub struct GroupedLogicalExpr {
    pub expr: Box<LogicalExpr>,
}

impl GroupedLogicalExpr {
    fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, expr) = delimited(
            opt_ws(char('(')),
            LogicalExpr::parse,
            opt_ws(char(')')),
        )(input)?;
        Ok((
            input,
            Self {
                expr: Box::new(expr),
            },
        ))
    }
}

#[derive(Clone, Debug)]
pub enum MatchOperator {
    // 'match' followed by a block containing truth expressions
    Match,
    // a `match some_value with` match pattern for enums, the block
    // enumerates the variants
    MatchValueWith(Identifier),
    // Query quantifiers, where the following block contains expressions that
    // may yield multiple instances of type values
    Some,
    ExactlyOne,
    All,
}

impl MatchOperator {
    fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        alt((
            map(
                tuple((
                    tag("match"),
                    opt_ws(Identifier::parse),
                    opt_ws(tag("with")),
                )),
                |t| MatchOperator::MatchValueWith(t.1),
            ),
            map(tag("match"), |_| MatchOperator::Match),
            map(tag("some"), |_| MatchOperator::Some),
            map(tag("exactly-one"), |_| MatchOperator::ExactlyOne),
            map(tag("all"), |_| MatchOperator::All),
        ))(input)
    }

    pub(crate) fn get_ident(&self) -> Result<Identifier, CompileError> {
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

//------------ PrefixMatchType ----------------------------------------------

// PrefixMatchType ::= ( 'exact' | 'longer' | 'orlonger' |
//      'prefix-length-range' | 'upto' | 'through' | 'netmask' )
//      ( PrefixLength | PrefixLengthRange | IpAddress )

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

impl PrefixMatchType {
    fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, res) = alt((
            map(tag("exact"), |_| PrefixMatchType::Exact),
            map(tag("longer"), |_| PrefixMatchType::Longer),
            map(tag("orlonger"), |_| PrefixMatchType::OrLonger),
            map(
                preceded(
                    tag("prefix-length-range"),
                    opt_ws(PrefixLengthRange::parse),
                ),
                PrefixMatchType::PrefixLengthRange,
            ),
            map(
                preceded(tag("upto"), opt_ws(PrefixLength::parse)),
                PrefixMatchType::UpTo,
            ),
            map(
                preceded(tag("through"), opt_ws(PrefixLength::parse)),
                PrefixMatchType::Through,
            ),
            map(
                preceded(tag("netmask"), opt_ws(IpAddress::parse)),
                PrefixMatchType::NetMask,
            ),
        ))(input)?;

        Ok((input, res))
    }
}

//------------ PrefixMatchExpr ----------------------------------------------

// PrefixMatchExpr ::= Prefix PrefixMatchType

#[derive(Clone, Debug)]
pub struct PrefixMatchExpr {
    pub prefix: Prefix,
    pub ty: PrefixMatchType,
}

impl PrefixMatchExpr {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, (prefix, ty)) = tuple((
            opt_ws(Prefix::parse),
            opt_ws(PrefixMatchType::parse),
        ))(input)?;

        Ok((input, Self { prefix, ty }))
    }
}

//------------ IpAddress ----------------------------------------------------

// IpAddress ::= IpV4Address | IpV6Address

#[derive(Clone, Debug)]
pub enum IpAddress {
    Ipv4(Ipv4Addr),
    Ipv6(Ipv6Addr),
}

impl IpAddress {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, res) = alt((
            map(Ipv4Addr::parse, IpAddress::Ipv4),
            map(Ipv6Addr::parse, IpAddress::Ipv6),
        ))(input)?;
        Ok((input, res))
    }
}

impl std::fmt::Display for IpAddress {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IpAddress::Ipv4(Ipv4Addr(v4)) => write!(f, "{v4}"),
            IpAddress::Ipv6(Ipv6Addr(v6)) => write!(f, "{v6}"),
        }
    }
}

//------------ Ipv4Addr -----------------------------------------------------

// Ipv4Addr ::= <ipv4 address>
#[derive(Clone, Debug)]
pub struct Ipv4Addr(pub std::net::Ipv4Addr);

impl Ipv4Addr {
    fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, ip_addr_str) = context(
            "IpV4 Literal",
            recognize(tuple((
                terminated(digit1, char('.')),
                terminated(digit1, char('.')),
                terminated(digit1, char('.')),
                take_while1(|ch: char| ch.is_dec_digit()),
                not(char(':')),
            ))),
        )(input)?;

        let addr =
            ip_addr_str.parse::<std::net::Ipv4Addr>().map_err(|_| {
                nom::Err::Error(VerboseError::from_error_kind(
                    input,
                    ErrorKind::Digit,
                ))
            })?;

        Ok((input, Self(addr)))
    }
}

//------------ Ipv6Addr -----------------------------------------------------

// Ipv6Addr ::= <ipv6 address>

#[derive(Clone, Debug)]
pub struct Ipv6Addr(pub std::net::Ipv6Addr);

impl Ipv6Addr {
    fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, ip_addr_str) = context(
            "IpV6 Literal",
            recognize(tuple((
                terminated(hex_digit0, char(':')),
                opt(terminated(hex_digit0, char(':'))),
                opt(terminated(hex_digit0, char(':'))),
                opt(terminated(hex_digit0, char(':'))),
                opt(terminated(hex_digit0, char(':'))),
                opt(terminated(hex_digit0, char(':'))),
                opt(terminated(hex_digit0, char(':'))),
                hex_digit0,
                not(char('.')),
            ))),
        )(input)?;

        let addr =
            ip_addr_str.parse::<std::net::Ipv6Addr>().map_err(|_| {
                nom::Err::Error(VerboseError::from_error_kind(
                    input,
                    ErrorKind::Digit,
                ))
            })?;

        Ok((input, Self(addr)))
    }
}

//------------ Prefix -------------------------------------------------------

// Prefix ::= IpAddress '/' PrefixLength

#[derive(Clone, Debug)]
pub struct Prefix {
    pub addr: IpAddress,
    pub len: PrefixLength,
}

impl Prefix {
    fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, (addr, len)) = tuple((
            opt_ws(IpAddress::parse),
            opt_ws(PrefixLength::parse),
        ))(input)?;

        Ok((input, Prefix { addr, len }))
    }
}

//------------ PrefixLength -------------------------------------------------

// PrefixLength ::= '/' <u8>

#[derive(Clone, Debug)]
pub struct PrefixLength(pub u8);

impl PrefixLength {
    fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, length) = preceded(char('/'), opt_ws(digit1))(input)?;

        let length = length.parse::<u8>().map_err(|_| {
            nom::Err::Error(VerboseError::from_error_kind(
                input,
                ErrorKind::Digit,
            ))
        })?;

        Ok((input, Self(length)))
    }
}

//------------ PrefixLengthRange --------------------------------------------

// PrefixLengthRange ::= PrefixLength '-' PrefixLength

#[derive(Clone, Debug)]
pub struct PrefixLengthRange {
    pub start: PrefixLength,
    pub end: PrefixLength,
}

impl PrefixLengthRange {
    fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, (start, end)) = tuple((
            opt_ws(PrefixLength::parse),
            preceded(char('-'), opt_ws(PrefixLength::parse)),
        ))(input)?;

        Ok((input, Self { start, end }))
    }
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
