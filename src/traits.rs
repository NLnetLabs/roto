// =========== RotoFilter trait ============================================

use serde::Serialize;

use crate::{
    ast::ShortString,
    compiler::compile::CompileError,
    types::{
        collections::Record,
        datasources::DataSourceMethodValue,
        enum_types::GlobalEnumTypeDef,
        typedef::{MethodProps, TypeDef},
        typevalue::TypeValue,
    },
    vm::{StackValue, VmError, FieldIndex},
};

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, Serialize)]
pub enum Token {
    // A value represented by the index of the entry in the `local_variables`
    // VariableRefTable of the CompilerState.
    Variable(usize),
    Method(usize),
    Variant(usize),
    Argument(usize),
    // Action Sections can have arguments passed in, a symbol labelled with
    // this token references that argument. The first usize represents the
    // index of the TermSection for which it is the `with` argument. The
    // second usize is the index of the `with` argument per TermSection. For
    // now this is always zero (only one argument per TermSection allowed).
    ActionArgument(usize, usize),
    // Idem but for Terms
    TermArgument(usize, usize),
    // There can only ever be one RxType
    RxType(TypeDef),
    // There can only ever be one TxType too
    TxType,
    // There can only ever be one RouteContext type too
    RouteContext(TypeDef),
    // External Data Sources
    Table(usize),
    Rib(usize),
    // A generic stream that can be used by Roto to send messages to and that
    // can be configured through a Roto script, e.g. a Kafka or a MQTT stream.
    OutputStream(usize),
    // A mapping to the (recursive) field of a record, the first u8 is a
    // numbered field of the record, the second points into the first
    // sub-field etc.
    FieldAccess(Vec<u8>),
    // A numbered term section that was found in the evaluated symbol map.
    TermSection(usize),
    // A term that is used only once (in a match expression) and will be
    // compiled at the spot.
    AnonymousTerm,
    ActionSection(usize),
    NoAction,
    MatchAction(usize),
    // None as data indicates a constant that wasn't stored (yet) in the
    // symbol table.
    Constant(Option<usize>),
    // An anonymous record, the fields live in the `args` vec of the
    // symbol.
    AnonymousRecord,
    TypedRecord,
    List,
    BuiltinType(u8),
    // A named, hard-coded global Enum
    Enum(GlobalEnumTypeDef),
    // Anonymous Enum
    AnonymousEnum,
    ConstEnumVariant,
    // Some structural symbols that are non-terminal, meaning they have
    // children, may not have to need any Token.
    NonTerminal,
}

impl Token {
    pub fn push(&mut self, value: u8) -> Result<(), CompileError> {
        match self {
            Token::FieldAccess(v) => v.push(value),
            _ => {
                return Err(CompileError::Internal(format!(
                    "Cannot add value with type '{}' to FieldAccess",
                    value
                )));
            }
        };

        Ok(())
    }

    pub fn is_term(&self) -> bool {
        matches!(self, Token::TermSection(_))
    }

    pub fn is_action(&self) -> bool {
        matches!(self, Token::ActionSection(_))
    }

    pub fn is_variable(&self) -> bool {
        matches!(self, Token::Variable(_))
    }

    pub fn is_argument(&self) -> bool {
        matches!(self, Token::Argument(_))
    }

    pub fn is_data_source(&self) -> bool {
        matches!(self, Token::Rib(_) | Token::Table(_))
    }
}

impl TryFrom<Token> for usize {
    type Error = CompileError;

    fn try_from(token: Token) -> Result<Self, CompileError> {        
        match token {
            Token::Table(v) | Token::Rib(v) => Ok(v),
            Token::Method(v) => Ok(v),
            Token::Variable(v) => Ok(v),
            Token::RxType(_) => Ok(0),
            Token::TxType => Ok(1),
            Token::Variant(v) => Ok(v),
            _ => Err(CompileError::Internal(
                    format!("Cannot convert {:?} to usize",
                    token
                )))
        }
    }
}

// impl From for Field Index.
impl TryFrom<Token> for FieldIndex {
    type Error = CompileError;

    fn try_from(value: Token) -> Result<Self, Self::Error> {
        if let Token::FieldAccess(fa) = value {
            Ok(fa.iter().map(|fi| *fi as usize).collect::<Vec<_>>().into())
        } else {
            Err(CompileError::from(format!(
                "Cannot convert token {:?} into FieldIndex",
                value
            )))
        }
    }
}

pub trait RotoType: Into<TypeValue>
where
    Self: std::fmt::Debug + Sized,
{
    fn take_value(self) -> Self {
        self
    }

    fn get_props_for_method(
        ty: TypeDef,
        method_name: &super::ast::Identifier,
    ) -> Result<MethodProps, CompileError>
    where
        Self: std::marker::Sized;

    fn into_type(
        self,
        type_value: &TypeDef,
    ) -> Result<TypeValue, CompileError>
    where
        Self: std::marker::Sized;

    fn exec_value_method<'a>(
        &'a self,
        method_token: usize,
        args: &'a [StackValue],
        res_type: TypeDef,
    ) -> Result<TypeValue, VmError>;

    fn exec_consume_value_method(
        self,
        method_token: usize,
        args: Vec<TypeValue>,
        res_type: TypeDef,
    ) -> Result<TypeValue, VmError>;

    fn exec_type_method(
        method_token: usize,
        args: &[StackValue],
        res_type: TypeDef,
    ) -> Result<TypeValue, VmError>;
}

pub trait RotoRib: Send + Sync {
    fn exec_value_method<'a>(
        &'a self,
        method_token: usize,
        args: &'a [StackValue],
        res_type: TypeDef,
    ) -> Result<TypeValue, VmError>;

    fn exec_ref_value_method<'a>(
        &self,
        method_token: usize,
        args: &[StackValue],
        res_type: TypeDef,
    ) -> Result<DataSourceMethodValue, VmError>;

    fn get_by_key<'a>(&'a self, key: &str) -> Option<&'a Record>;

    fn len(&self) -> usize;

    fn is_empty(&self) -> bool;

    fn get_name(&self) -> ShortString;

    fn get_type(&self) -> TypeDef;
}
