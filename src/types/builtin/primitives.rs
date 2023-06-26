use std::fmt::{Display, Formatter};

use log::trace;
use routecore::asn::LongSegmentError;
use serde::Serialize;

use crate::attr_change_set::VectorValue;
use crate::compile::CompileError;
use crate::traits::RotoType;
use crate::types::collections::{ElementTypeValue, List};
use crate::types::typedef::MethodProps;
use crate::vm::{StackValue, VmError};

use super::super::typedef::TypeDef;
use super::super::typevalue::TypeValue;
use super::builtin_type_value::BuiltinTypeValue;

// ----------- A simple u32 type --------------------------------------------

#[derive(Debug, Eq, Copy, Clone, Serialize)]
pub struct U32(pub(crate) u32);

impl U32 {
    pub fn new(val: u32) -> Self {
        U32(val)
    }
}

impl From<U32> for TypeValue {
    fn from(val: U32) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::U32(val))
    }
}

impl RotoType for U32 {
    fn get_props_for_method(
        _ty: TypeDef,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError>
    where
        Self: std::marker::Sized,
    {
        match method_name.ident.as_str() {
            "set" => Ok(MethodProps::new(
                TypeDef::Unknown,
                U32Token::Set.into(),
                vec![TypeDef::IntegerLiteral],
            )
            .consume_value()),
            _ => Err(format!(
                "Unknown method: '{}' for type U32",
                method_name.ident
            )
            .into()),
        }
    }

    fn exec_value_method<'a>(
        &'a self,
        method_token: usize,
        args: &[StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        Err(VmError::InvalidMethodCall)
    }

    fn exec_consume_value_method(
        self,
        method_token: usize,
        mut args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        match method_token.into() {
            U32Token::Set => {
                if let Ok(TypeValue::Builtin(BuiltinTypeValue::U32(int_u32))) =
                    args.remove(0).into_type(&TypeDef::U32)
                {
                    Ok(TypeValue::Builtin(BuiltinTypeValue::U32(int_u32)))
                } else {
                    Err(VmError::InvalidValueType)
                }
            }
        }
    }

    fn exec_type_method<'a>(    
        _method_token: usize,
        _args: &[StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        Err(VmError::InvalidMethodCall)
    }

    fn into_type(
        self,
        type_def: &TypeDef,
    ) -> Result<TypeValue, CompileError> {
        match type_def {
            TypeDef::U32 => {
                Ok(TypeValue::Builtin(BuiltinTypeValue::U32(self)))
            }
            TypeDef::Asn => Ok(TypeValue::Builtin(BuiltinTypeValue::Asn(
                Asn(routecore::asn::Asn::from(self.0)),
            ))),
            TypeDef::PrefixLength => {
                match self.0 {
                    0..=128 => Ok(TypeValue::Builtin(BuiltinTypeValue::PrefixLength(PrefixLength(self.0 as u8)))),
                    _ =>
                    Err(format!("Cannot convert an instance of type U32 with a value greater than 128 into type {:?}", type_def)
                    .into())
                }
            },
            TypeDef::U8 => {
                match self.0 {
                    0..=255 => Ok(TypeValue::Builtin(BuiltinTypeValue::U8(U8(self.0 as u8)))),
                    _ => Err(format!("Cannot convert an instance of type U32 with a value greater than 128 into type {:?}", type_def)
                    .into())
                }
            }
            _ => {
                Err(format!("Cannot convert type U32 into type {:?}", type_def)
                    .into())
            }
        }
    }
}

impl PartialEq for U32 {
    fn eq(&self, other: &Self) -> bool {
        if let Ok(TypeValue::Builtin(BuiltinTypeValue::U32(U32(o)))) =
            other.into_type(&TypeDef::U32)
        {
            o == self.0
        } else {
            false
        }
    }
}

impl std::hash::Hash for U32 {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl Display for U32 {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug)]
pub enum U32Token {
    Set,
}



impl From<usize> for U32Token {
    fn from(val: usize) -> Self {
        match val {
            0 => U32Token::Set,
            _ => panic!("Unknown token value: {}", val),
        }
    }
}

impl From<U32Token> for usize {
    fn from(val: U32Token) -> Self {
        match val {
            U32Token::Set => 0,
        }
    }
}

// ----------- A simple u8 type ---------------------------------------------

#[derive(Debug, Eq, Copy, Clone, Serialize)]
pub struct U8(pub(crate) u8);

impl U8 {
    pub fn new(val: u8) -> Self {
        U8(val)
    }
}

impl RotoType for U8 {
    fn get_props_for_method(
        _ty: TypeDef,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError>
    where
        Self: std::marker::Sized,
    {
        match method_name.ident.as_str() {
            "set" => Ok(MethodProps::new(
                TypeDef::Unknown,
                U8Token::Set.into(),
                vec![TypeDef::IntegerLiteral],
            )
            .consume_value()),
            _ => Err(format!(
                "Unknown method: '{}' for type U8",
                method_name.ident
            )
            .into()),
        }
    }

    fn exec_value_method<'a>(
        &'a self,
        _method_token: usize,
        _args: &[StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        Err(VmError::InvalidMethodCall)
    }

    fn exec_consume_value_method(
        self,
        method_token: usize,
        mut args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        match method_token.into() {
            U32Token::Set => {
                if let Ok(TypeValue::Builtin(BuiltinTypeValue::U8(int_u8))) =
                    args.remove(0).into_type(&TypeDef::U8)
                {
                    Ok(TypeValue::Builtin(BuiltinTypeValue::U8(int_u8)))
                } else {
                    Err(VmError::InvalidValueType)
                }
            }
        }
    }

    fn exec_type_method<'a>(
        _method_token: usize,
        _args: &[StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        Err(VmError::InvalidMethodCall)
    }

    fn into_type(
        self,
        type_def: &TypeDef,
    ) -> Result<TypeValue, CompileError> {
        match type_def {
            // Self
            TypeDef::U8 => Ok(TypeValue::Builtin(BuiltinTypeValue::U8(self))),
            TypeDef::U32 => {
                let value = self.0;
                Ok(TypeValue::Builtin(BuiltinTypeValue::U32(U32(
                    value as u32
                ))))
            }
            TypeDef::PrefixLength => match self.0 {
                0..=128 => Ok(TypeValue::Builtin(
                    BuiltinTypeValue::PrefixLength(PrefixLength(self.0)),
                )),
                _ => Err(format!(
                    "Prefix length must be between 0 and 128, not {}",
                    self.0
                )
                .into()),
            },
            _ => {
                Err(format!("Cannot convert type U8 to type {:?}", type_def)
                    .into())
            }
        }
    }
}

impl PartialEq for U8 {
    fn eq(&self, other: &Self) -> bool {
        if let Ok(TypeValue::Builtin(BuiltinTypeValue::U8(U8(o)))) =
            other.into_type(&TypeDef::U8)
        {
            o == self.0
        } else {
            false
        }
    }
}

impl std::hash::Hash for U8 {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl Display for U8 {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<U8> for TypeValue {
    fn from(val: U8) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::U8(val))
    }
}

#[derive(Debug)]
pub enum U8Token {
    Set,
}

impl From<usize> for U8Token {
    fn from(val: usize) -> Self {
        match val {
            0 => U8Token::Set,
            _ => panic!("Unknown token value: {}", val),
        }
    }
}

impl From<U8Token> for usize {
    fn from(val: U8Token) -> Self {
        match val {
            U8Token::Set => 0,
        }
    }
}

// ----------- Boolean type -------------------------------------------------

#[derive(Debug, Eq, PartialEq, Copy, Clone, Hash, Serialize)]
pub struct Boolean(pub(crate) bool);
impl Boolean {
    pub fn new(val: bool) -> Self {
        Boolean(val)
    }

    pub fn is_false(&self) -> bool {
        !self.0
    }
}

impl RotoType for Boolean {
    fn get_props_for_method(
        _ty: TypeDef,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError>
    where
        Self: std::marker::Sized,
    {
        match method_name.ident.as_str() {
            "set" => Ok(MethodProps::new(
                TypeDef::Unknown,
                BooleanToken::Set.into(),
                vec![TypeDef::Boolean],
            )
            .consume_value()),
            _ => Err(format!(
                "Unknown method: '{}' for type Boolean",
                method_name.ident
            )
            .into()),
        }
    }

    fn exec_value_method<'a>(
        &'a self,
        _method_token: usize,
        _args: &[StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        Err(VmError::InvalidMethodCall)
    }

    fn exec_consume_value_method(
        self,
        method_token: usize,
        mut args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        match method_token.into() {
            BooleanToken::Set => {
                if let Ok(TypeValue::Builtin(BuiltinTypeValue::Boolean(b))) =
                    args.remove(0).into_type(&TypeDef::Boolean)
                {
                    Ok(TypeValue::Builtin(BuiltinTypeValue::Boolean(b)))
                } else {
                    Err(VmError::InvalidValueType)
                }
            }
        }
    }

    fn exec_type_method<'a>(
        _method_token: usize,
        _args: &[StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        Err(VmError::InvalidMethodCall)
    }

    fn into_type(
        self,
        type_def: &TypeDef,
    ) -> Result<TypeValue, CompileError> {
        match type_def {
            TypeDef::Boolean => {
                Ok(TypeValue::Builtin(BuiltinTypeValue::Boolean(self)))
            }
            _ => Err(format!(
                "Cannot convert type Boolean to type {:?}",
                type_def
            )
            .into()),
        }
    }
}

impl From<Boolean> for TypeValue {
    fn from(val: Boolean) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::Boolean(val))
    }
}

impl From<Boolean> for BuiltinTypeValue {
    fn from(val: Boolean) -> Self {
        BuiltinTypeValue::Boolean(val)
    }
}

impl From<bool> for BuiltinTypeValue {
    fn from(value: bool) -> Self {
        BuiltinTypeValue::Boolean(Boolean(value))
    }
}

impl Display for Boolean {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug)]
pub enum BooleanToken {
    Set,
}

impl From<usize> for BooleanToken {
    fn from(val: usize) -> Self {
        match val {
            0 => BooleanToken::Set,
            _ => panic!("Unknown token value: {}", val),
        }
    }
}

impl From<BooleanToken> for usize {
    fn from(val: BooleanToken) -> Self {
        val as usize
    }
}

//------------ StringLiteral type -------------------------------------------

#[derive(Debug, Eq, PartialEq, Clone, Hash, Serialize)]
pub struct StringLiteral(pub(crate) String);
impl StringLiteral {
    pub fn new(val: String) -> Self {
        StringLiteral(val)
    }
}

impl RotoType for StringLiteral {
    fn get_props_for_method(
        _ty: TypeDef,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError> {
        match method_name.ident.as_str() {
            "cmp" => Ok(MethodProps::new(
                TypeDef::IntegerLiteral,
                StringLiteralToken::Cmp.into(),
                vec![TypeDef::StringLiteral, TypeDef::StringLiteral],
            )),
            "format" => Ok(MethodProps::new(
                TypeDef::StringLiteral,
                StringLiteralToken::Format.into(),
                vec![TypeDef::StringLiteral, TypeDef::StringLiteral],
            )),
            _ => Err(format!(
                "Unknown method: '{}' for type StringLiteral",
                method_name.ident
            )
            .into()),
        }
    }

    fn exec_value_method<'a>(
        &'a self,
        _method_token: usize,
        _args: &[StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }

    fn exec_consume_value_method(
        self,
        method_token: usize,
        mut args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        match method_token.into() {
            StringLiteralToken::Set => {
                if let Ok(TypeValue::Builtin(BuiltinTypeValue::StringLiteral(str))) =
                    args.remove(0).into_type(&TypeDef::StringLiteral)
                {
                    Ok(TypeValue::Builtin(BuiltinTypeValue::StringLiteral(str)))
                } else {
                    Err(VmError::InvalidValueType)
                }
            }
            StringLiteralToken::Cmp => Err(VmError::InvalidMethodCall),
            StringLiteralToken::Format => Err(VmError::InvalidMethodCall),
        }
    }

    fn exec_type_method<'a>(
        method_token: usize,
        args: &[StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        match method_token.into() {
            StringLiteralToken::Format => {
                trace!("string arguments {:?}", args);

                let format_str = if let TypeValue::Builtin(
                    BuiltinTypeValue::StringLiteral(StringLiteral(str)),
                ) = args[0].as_ref()
                {
                    str
                } else {
                    return Err(VmError::AnonymousArgumentNotFound);
                };

                let mut sub_str = format_str.splitn(2, "{}");

                let new_string = String::from_iter([
                    sub_str.next().unwrap(),
                    &args[1].as_ref().to_string(),
                    if let Some(s) = sub_str.next() { s } else { "" },
                ]);

                Ok(
                    TypeValue::Builtin(BuiltinTypeValue::StringLiteral(
                        StringLiteral(new_string),
                    ))
                )
            }
            StringLiteralToken::Cmp => unimplemented!(),
            StringLiteralToken::Set => Err(VmError::InvalidMethodCall),
        }
    }

    fn into_type(
        self,
        type_def: &TypeDef,
    ) -> Result<TypeValue, CompileError> {
        match type_def {
            TypeDef::StringLiteral => {
                Ok(TypeValue::Builtin(BuiltinTypeValue::StringLiteral(self)))
            }
            _ => Err(format!(
                "Cannot convert type StringLiteral to type {:?}",
                type_def
            )
            .into()),
        }
    }
}

impl From<StringLiteral> for TypeValue {
    fn from(val: StringLiteral) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::StringLiteral(val))
    }
}

impl Display for StringLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug)]
pub enum StringLiteralToken {
    Cmp = 0,
    Format = 1,
    Set = 2
}

impl From<usize> for StringLiteralToken {
    fn from(val: usize) -> Self {
        match val {
            0 => StringLiteralToken::Cmp,
            1 => StringLiteralToken::Format,
            2 => StringLiteralToken::Set,
            _ => panic!("Unknown token value: {}", val),
        }
    }
}

impl From<StringLiteralToken> for usize {
    fn from(val: StringLiteralToken) -> Self {
        val as usize
    }
}

//------------ IntegerLiteral type ------------------------------------------

#[derive(Debug, Eq, Ord, PartialEq, PartialOrd, Copy, Clone, Serialize)]
pub struct IntegerLiteral(pub(crate) i64);
impl IntegerLiteral {
    pub fn new(val: i64) -> Self {
        IntegerLiteral(val)
    }
}

impl RotoType for IntegerLiteral {
    fn get_props_for_method(
        _ty: TypeDef,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError> {
        match method_name.ident.as_str() {
            "cmp" => Ok(MethodProps::new(
                TypeDef::IntegerLiteral,
                IntegerLiteralToken::Cmp.into(),
                vec![TypeDef::IntegerLiteral, TypeDef::IntegerLiteral],
            )),
            _ => Err(format!(
                "Unknown method: '{}' for type Prefix",
                method_name.ident
            )
            .into()),
        }
    }

    fn into_type(
        self,
        type_def: &TypeDef,
    ) -> Result<TypeValue, CompileError> {
        match type_def {
            TypeDef::IntegerLiteral => {
                Ok(TypeValue::Builtin(BuiltinTypeValue::IntegerLiteral(self)))
            }
            TypeDef::PrefixLength => match self.0 {
                0..=128 => {
                    Ok(TypeValue::Builtin(BuiltinTypeValue::PrefixLength(
                        PrefixLength(self.0 as u8),
                    )))
                }
                _ => Err(format!(
                    "Prefix length must be between 0 and 128, not {}",
                    self.0
                )
                .into()),
            },
            TypeDef::Asn => match self.0 {
                0..=4294967295 => {
                    Ok(TypeValue::Builtin(BuiltinTypeValue::Asn(Asn(routecore::asn::Asn::from(self.0 as u32)))))
                },
                _ => Err(CompileError::from("Cannot convert type IntegerLiteral > 4294967295 into Asn".to_string()))
            }
            TypeDef::U32 => u32::try_from(self.0)
                .map(|v| TypeValue::Builtin(BuiltinTypeValue::U32(U32(v))))
                .map_err(|_| {
                    CompileError::from(format!(
                        "Cannot convert type IntegerLiteral with {} into U32",
                        self.0
                    ))
                }),
            TypeDef::U8 => u8::try_from(self.0)
                .map(|v| TypeValue::Builtin(BuiltinTypeValue::U8(U8(v))))
                .map_err(|_| {
                    CompileError::from(format!(
                        "Cannot convert type IntegerLiteral with {} into U8",
                        self.0
                    ))
                }),
            _ => Err(format!(
                "Cannot convert type IntegerLiteral to type {:?}",
                type_def
            )
            .into()),
        }
    }

    fn exec_value_method<'a>(
        &'a self,
        method_token: usize,
        args: &[StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        match method_token.into() {
            IntegerLiteralToken::Cmp => {
                Ok(TypeValue::Builtin(BuiltinTypeValue::Boolean(Boolean(args[0] == args[1]))))
            }
        }
    }

    fn exec_consume_value_method(
        self,
        _method_token: usize,
        _args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        // There is no 'set' method for an IntegerLiteral, this type should
        // not be assigned to a Record or List (they should use the concrete
        // type, e.g. U8).
        Err(VmError::InvalidMethodCall)
    }

    fn exec_type_method<'a>(
        _method_token: usize,
        _args: &[StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        Err(VmError::InvalidMethodCall)
    }
}

// impl PartialEq for IntegerLiteral {
//     fn eq(&self, other: &Self) -> bool {
//         trace!("EQ IntegerLiteral");
//         if let Ok(TypeValue::Builtin(BuiltinTypeValue::IntegerLiteral(
//             IntegerLiteral(o),
//         ))) = other.into_type(&TypeDef::IntegerLiteral)
//         {
//             o == self.0
//         } else {
//             false
//         }
//     }
// }

impl std::hash::Hash for IntegerLiteral {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl From<IntegerLiteral> for TypeValue {
    fn from(val: IntegerLiteral) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::IntegerLiteral(val))
    }
}

impl Display for IntegerLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug)]
pub(crate) enum IntegerLiteralToken {
    Cmp,
}

impl From<usize> for IntegerLiteralToken {
    fn from(val: usize) -> Self {
        match val {
            0 => IntegerLiteralToken::Cmp,
            _ => panic!("Unknown token value: {}", val),
        }
    }
}

impl From<IntegerLiteralToken> for usize {
    fn from(val: IntegerLiteralToken) -> Self {
        val as usize
    }
}

//------------ HexLiteral type ----------------------------------------------

#[derive(Debug, Eq, PartialEq, Copy, Clone, Hash, Serialize)]
pub struct HexLiteral(pub(crate) u64);
impl HexLiteral {
    pub fn new(val: u64) -> Self {
        HexLiteral(val)
    }
}

impl RotoType for HexLiteral {
    fn get_props_for_method(
        _ty: TypeDef,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError> {
        match method_name.ident.as_str() {
            "cmp" => Ok(MethodProps::new(
                TypeDef::IntegerLiteral,
                HexLiteralToken::Cmp.into(),
                vec![TypeDef::HexLiteral, TypeDef::HexLiteral],
            )),
            _ => Err(format!(
                "Unknown method: '{}' for type Prefix",
                method_name.ident
            )
            .into()),
        }
    }

    fn into_type(
        self,
        type_def: &TypeDef,
    ) -> Result<TypeValue, CompileError> {
        match type_def {
            TypeDef::HexLiteral => {
                Ok(TypeValue::Builtin(BuiltinTypeValue::HexLiteral(self)))
            }
            TypeDef::Community => {
                // still bogus, but at least it should convert from the hexliteral type.
                let c = routecore::bgp::communities::Community::from(
                    self.0.to_be_bytes(),
                );
                Ok(TypeValue::Builtin(BuiltinTypeValue::Community(
                    Community(c),
                )))
            }
            TypeDef::U8 => u8::try_from(self.0)
                .map(|v| TypeValue::Builtin(BuiltinTypeValue::U8(U8(v))))
                .map_err(|_| {
                    CompileError::from(format!(
                        "Cannot convert type IntegerLiteral with {} into U8",
                        self.0
                    ))
                }),
            TypeDef::U32 => u32::try_from(self.0)
                .map(|v| TypeValue::Builtin(BuiltinTypeValue::U32(U32(v))))
                .map_err(|_| {
                    CompileError::from(format!(
                        "Cannot convert type IntegerLiteral with {} into U8",
                        self.0
                    ))
                }),
            _ => Err(format!(
                "Cannot convert type HexLiteral to type {:?}",
                type_def
            )
            .into()),
        }
    }

    fn exec_value_method<'a>(
        &'a self,
        _method_token: usize,
        _args: &[StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }

    fn exec_consume_value_method(
        self,
        _method_token: usize,
        _args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }

    fn exec_type_method<'a>(
        _method_token: usize,
        _args: &[StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }
}

impl From<HexLiteral> for TypeValue {
    fn from(val: HexLiteral) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::HexLiteral(val))
    }
}

impl Display for HexLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug)]
pub(crate) enum HexLiteralToken {
    Cmp,
}

impl From<usize> for HexLiteralToken {
    fn from(val: usize) -> Self {
        match val {
            0 => HexLiteralToken::Cmp,
            _ => panic!("Unknown token value: {}", val),
        }
    }
}

impl From<HexLiteralToken> for usize {
    fn from(val: HexLiteralToken) -> Self {
        val as usize
    }
}

// ----------- Prefix type --------------------------------------------------

#[derive(Debug, Eq, PartialEq, Copy, Clone, Hash, Serialize)]
pub struct Prefix(pub(crate) routecore::addr::Prefix);

impl Prefix {
    pub fn new(prefix: routecore::addr::Prefix) -> Self {
        Self(prefix)
    }

    pub fn exec_method(
        &self,
        _method: usize,
        _args: Vec<&TypeValue>,
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce(TypeValue) -> TypeValue + '_>, CompileError>
    {
        todo!()
    }
}

impl RotoType for Prefix {
    fn get_props_for_method(
        _ty: TypeDef,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError>
    where
        Self: std::marker::Sized,
    {
        match method_name.ident.as_str() {
            "from" => Ok(MethodProps::new(
                TypeDef::Prefix,
                PrefixToken::From.into(),
                vec![TypeDef::IpAddress, TypeDef::PrefixLength],
            )),
            "address" => Ok(MethodProps::new(
                TypeDef::IpAddress,
                PrefixToken::Address.into(),
                vec![],
            )),
            "len" => Ok(MethodProps::new(
                TypeDef::PrefixLength,
                PrefixToken::Len.into(),
                vec![],
            )),
            "matches" => Ok(MethodProps::new(
                TypeDef::Boolean,
                PrefixToken::Matches.into(),
                vec![TypeDef::Prefix],
            )),
            "exists" => Ok(MethodProps::new(
                TypeDef::Boolean,
                PrefixToken::Exists.into(),
                vec![],
            )),
            _ => Err(format!(
                "Unknown method: '{}' for type Prefix",
                method_name.ident
            )
            .into()),
        }
    }

    fn into_type(
        self,
        type_def: &TypeDef,
    ) -> Result<TypeValue, CompileError> {
        match type_def {
            TypeDef::Prefix => {
                Ok(TypeValue::Builtin(BuiltinTypeValue::Prefix(self)))
            }
            _ => Err(format!(
                "Cannot convert type Prefix to type {:?}",
                type_def
            )
            .into()),
        }
    }

    fn exec_value_method<'a>(
        &'a self,
        method_token: usize,
        _args: &[StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        match method_token.into() {
            PrefixToken::Address => {
                let prefix = self.0;
                Ok(
                    TypeValue::Builtin(BuiltinTypeValue::IpAddress(
                        IpAddress(prefix.addr()),
                    ))
                )
            }
            PrefixToken::Len => {
                let Prefix(pfx) = self;
                Ok(
                    TypeValue::Builtin(BuiltinTypeValue::PrefixLength(
                        PrefixLength(pfx.len()),
                    ))
                )
            }
            PrefixToken::From => unimplemented!(),
            PrefixToken::Exists => Ok(true.into()),
            PrefixToken::Matches => todo!(),
        }
    }

    fn exec_consume_value_method(
        self,
        _method_token: usize,
        _args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }

    fn exec_type_method<'a>(
        method_token: usize,
        args: &[StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        match method_token.into() {
            PrefixToken::From => {
                if let TypeValue::Builtin(BuiltinTypeValue::IpAddress(ip)) =
                    args[0].as_ref()
                {
                    let len: PrefixLength = args[1]
                        .as_ref()
                        .try_into()
                        .map_err(|_e| VmError::InvalidConversion)?;
                    let ip = ip.0;
                    Ok(
                        routecore::addr::Prefix::new(ip, len.into())
                            .map_or_else(|_| TypeValue::Unknown, |p| p.into())
                    )
                } else {
                    Err(VmError::AnonymousArgumentNotFound)
                }
            }
            PrefixToken::Exists => unimplemented!(),
            PrefixToken::Address => unimplemented!(),
            PrefixToken::Len => unimplemented!(),
            PrefixToken::Matches => unimplemented!(),
        }
    }
}

impl From<Prefix> for TypeValue {
    fn from(val: Prefix) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::Prefix(val))
    }
}

impl From<routecore::addr::Prefix> for Prefix {
    fn from(val: routecore::addr::Prefix) -> Self {
        Prefix(val)
    }
}

impl Display for Prefix {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<Prefix> for routecore::addr::Prefix {
    fn from(val: Prefix) -> Self {
        val.0
    }
}

#[derive(Debug)]
#[repr(u8)]
pub(crate) enum PrefixToken {
    From = 0,
    Exists = 1,
    Address = 2,
    Len = 3,
    Matches = 4,
}

impl From<usize> for PrefixToken {
    fn from(val: usize) -> Self {
        match val {
            0 => PrefixToken::From,
            1 => PrefixToken::Exists,
            2 => PrefixToken::Address,
            3 => PrefixToken::Len,
            4 => PrefixToken::Matches,
            _ => panic!("Unknown token value: {}", val),
        }
    }
}

impl From<PrefixToken> for usize {
    fn from(val: PrefixToken) -> Self {
        val as usize
    }
}

//------------ PrefixLengthLiteral type -------------------------------------

#[derive(Debug, Eq, PartialEq, Copy, Clone, Hash, Serialize)]
pub struct PrefixLength(pub(crate) u8);

impl PrefixLength {
    pub fn new(val: u8) -> Self {
        PrefixLength(val)
    }
}

impl RotoType for PrefixLength {
    fn get_props_for_method(
        _ty: TypeDef,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError>
    where
        Self: std::marker::Sized,
    {
        match method_name.ident.as_str() {
            "from" => Ok(MethodProps::new(
                TypeDef::PrefixLength,
                PrefixLengthToken::From.into(),
                vec![TypeDef::U8],
            )),
            _ => Err(format!(
                "Unknown method: '{}' for type PrefixLength",
                method_name.ident
            )
            .into()),
        }
    }

    fn into_type(
        self,
        type_def: &TypeDef,
    ) -> Result<TypeValue, CompileError> {
        match type_def {
            TypeDef::PrefixLength => {
                Ok(TypeValue::Builtin(BuiltinTypeValue::PrefixLength(self)))
            }
            _ => Err(format!(
                "Cannot convert type PrefixLength to type {:?}",
                type_def
            )
            .into()),
        }
    }

    fn exec_value_method<'a>(
        &'a self,
        _method_token: usize,
        _args: &[StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }

    fn exec_consume_value_method(
        self,
        method_token: usize,
        args: Vec<TypeValue>,
        type_def: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }

    fn exec_type_method<'a>(
        _method_token: usize,
        _args: &[StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }
}

impl From<PrefixLength> for TypeValue {
    fn from(val: PrefixLength) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::PrefixLength(val))
    }
}

impl Display for PrefixLength {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "/{}", self.0)
    }
}

#[derive(Debug)]
pub(crate) enum PrefixLengthToken {
    From,
}

impl From<usize> for PrefixLengthToken {
    fn from(val: usize) -> Self {
        match val {
            0 => PrefixLengthToken::From,
            _ => panic!("Unknown token value: {}", val),
        }
    }
}

impl From<PrefixLengthToken> for usize {
    fn from(val: PrefixLengthToken) -> Self {
        val as usize
    }
}

impl TryFrom<&TypeValue> for PrefixLength {
    type Error = CompileError;

    fn try_from(value: &TypeValue) -> Result<Self, Self::Error> {
        match value {
            TypeValue::Builtin(BuiltinTypeValue::PrefixLength(pl)) => Ok(*pl),
            TypeValue::Builtin(BuiltinTypeValue::IntegerLiteral(int_lit)) => {
                if let TypeValue::Builtin(BuiltinTypeValue::PrefixLength(
                    PrefixLength(pl),
                )) = int_lit.into_type(&TypeDef::PrefixLength)?
                {
                    Ok(PrefixLength(pl))
                } else {
                    Err(format!(
                        "Cannot convert type {:?} to type PrefixLength",
                        value
                    )
                    .into())
                }
            }
            _ => Err(format!(
                "Cannot convert type {:?} to type PrefixLength",
                value
            )
            .into()),
        }
    }
}

impl From<PrefixLength> for u8 {
    fn from(val: PrefixLength) -> Self {
        val.0
    }
}

// ----------- Community ----------------------------------------------------

#[derive(Debug, Eq, PartialEq, Copy, Clone, Hash, Serialize)]
pub struct Community(pub(crate) routecore::bgp::communities::Community);

impl Community {
    pub fn new(com: routecore::bgp::communities::Community) -> Self {
        Self(com)
    }
}

impl RotoType for Community {
    fn get_props_for_method(
        _ty: TypeDef,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError>
    where
        Self: std::marker::Sized,
    {
        match method_name.ident.as_str() {
            "from" => Ok(MethodProps::new(
                TypeDef::Community,
                CommunityToken::From.into(),
                vec![TypeDef::U32],
            )),
            "standard" => Ok(MethodProps::new(
                TypeDef::Community,
                CommunityToken::Standard.into(),
                vec![TypeDef::U32],
            )),
            "extended" => Ok(MethodProps::new(
                TypeDef::Community,
                CommunityToken::Extended.into(),
                vec![TypeDef::U32],
            )),
            "large" => Ok(MethodProps::new(
                TypeDef::Community,
                CommunityToken::Large.into(),
                vec![TypeDef::U32],
            )),
            "as" => Ok(MethodProps::new(
                TypeDef::U32,
                CommunityToken::As.into(),
                vec![],
            )),
            "value" => Ok(MethodProps::new(
                TypeDef::U32,
                CommunityToken::Value.into(),
                vec![],
            )),
            "exists" => Ok(MethodProps::new(
                TypeDef::Boolean,
                CommunityToken::Exists.into(),
                vec![],
            )),
            _ => Err(format!(
                "Unknown method: '{}' for type Community",
                method_name.ident
            )
            .into()),
        }
    }

    fn into_type(
        self,
        type_def: &TypeDef,
    ) -> Result<TypeValue, CompileError> {
        match type_def {
            TypeDef::Community => {
                Ok(TypeValue::Builtin(BuiltinTypeValue::Community(self)))
            }
            _ => Err(format!(
                "Cannot convert type Community to type {:?}",
                type_def
            )
            .into()),
        }
    }

    fn exec_value_method<'a>(
        &'a self,
        _method_token: usize,
        _args: &[StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }

    fn exec_consume_value_method(
        self,
        _method_token: usize,
        _args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }

    fn exec_type_method<'a>(
        _method_token: usize,
        _args: &[StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }
}

impl From<Community> for TypeValue {
    fn from(val: Community) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::Community(val))
    }
}

impl From<routecore::bgp::communities::Community> for TypeValue {
    fn from(val: routecore::bgp::communities::Community) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::Community(Community(val)))
    }
}

impl From<routecore::bgp::communities::Community> for BuiltinTypeValue {
    fn from(value: routecore::bgp::communities::Community) -> Self {
        BuiltinTypeValue::Community(Community(value))
    }
}

impl From<Vec<routecore::bgp::communities::Community>> for TypeValue {
    fn from(value: Vec<routecore::bgp::communities::Community>) -> Self {
        let list: Vec<ElementTypeValue> = value
            .iter()
            .map(|c| ElementTypeValue::Primitive(TypeValue::from(*c)))
            .collect::<Vec<_>>();
        TypeValue::List(crate::types::collections::List(list))
    }
}

impl From<Vec<routecore::bgp::communities::Community>> for BuiltinTypeValue {
    fn from(value: Vec<routecore::bgp::communities::Community>) -> Self {
        let list: Vec<ElementTypeValue> = value
            .iter()
            .map(|c| ElementTypeValue::Primitive(TypeValue::from(*c)))
            .collect::<Vec<_>>();
        BuiltinTypeValue::Communities(List(list))
    }
}

impl Display for Community {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

#[derive(Debug)]
pub enum CommunityToken {
    From,
    Standard,
    Extended,
    Large,
    As,
    Value,
    Exists,
}

impl From<usize> for CommunityToken {
    fn from(val: usize) -> Self {
        match val {
            0 => CommunityToken::From,
            1 => CommunityToken::Standard,
            2 => CommunityToken::Extended,
            3 => CommunityToken::Large,
            4 => CommunityToken::As,
            5 => CommunityToken::Value,
            6 => CommunityToken::Exists,
            _ => panic!("Unknown token value: {}", val),
        }
    }
}

impl From<CommunityToken> for usize {
    fn from(val: CommunityToken) -> Self {
        val as usize
    }
}

//------------ MatchType ----------------------------------------------------

#[derive(Debug, Eq, PartialEq)]
pub enum MatchType {
    ExactMatch,
    LongestMatch,
    EmptyMatch,
}

// ----------- IpAddress type -----------------------------------------------

#[derive(Debug, Eq, PartialEq, Copy, Clone, Hash, Serialize)]
pub struct IpAddress(pub(crate) std::net::IpAddr);

impl IpAddress {
    pub fn new(addr: std::net::IpAddr) -> Self {
        IpAddress(addr)
    }
}

impl RotoType for IpAddress {
    fn get_props_for_method(
        _ty: TypeDef,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError>
    where
        Self: std::marker::Sized,
    {
        match method_name.ident.as_str() {
            "from" => Ok(MethodProps::new(
                TypeDef::IpAddress,
                IpAddressToken::From.into(),
                vec![TypeDef::StringLiteral],
            )),
            "matches" => Ok(MethodProps::new(
                TypeDef::Boolean,
                IpAddressToken::Matches.into(),
                vec![TypeDef::Prefix],
            )),
            _ => Err(format!(
                "Unknown method: '{}' for type IpAddress",
                method_name.ident
            )
            .into()),
        }
    }

    fn into_type(
        self,
        type_def: &TypeDef,
    ) -> Result<TypeValue, CompileError> {
        match type_def {
            TypeDef::IpAddress => {
                Ok(TypeValue::Builtin(BuiltinTypeValue::IpAddress(self)))
            }
            _ => Err(format!(
                "Cannot convert type IpAddress to type {:?}",
                type_def
            )
            .into()),
        }
    }

    fn exec_value_method<'a>(
        &'a self,
        _method_token: usize,
        _args: &[StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!();
    }

    fn exec_consume_value_method(
        self,
        _method_token: usize,
        _args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }

    fn exec_type_method<'a>(
        _method_token: usize,
        _args: &[StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }
}

impl From<IpAddress> for TypeValue {
    fn from(val: IpAddress) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::IpAddress(val))
    }
}

impl Display for IpAddress {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug)]
pub(crate) enum IpAddressToken {
    From,
    Matches,
}

impl From<usize> for IpAddressToken {
    fn from(val: usize) -> Self {
        match val {
            0 => IpAddressToken::From,
            1 => IpAddressToken::Matches,
            _ => panic!("Unknown token value: {}", val),
        }
    }
}

impl From<IpAddressToken> for usize {
    fn from(val: IpAddressToken) -> Self {
        val as usize
    }
}

// ----------- Asn type -----------------------------------------------------

#[derive(Debug, Eq, Copy, Clone, Hash, Serialize)]
pub struct Asn(pub(crate) routecore::asn::Asn);

impl Asn {
    pub fn new(asn: routecore::asn::Asn) -> Self {
        Asn(asn)
    }

    pub fn from_u32(asn: u32) -> Self {
        Asn(routecore::asn::Asn::from(asn))
    }

    pub fn get_asn(&self) -> routecore::asn::Asn {
        self.0
    }
}

impl RotoType for Asn {
    fn get_props_for_method(
        _ty: TypeDef,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError>
    where
        Self: std::marker::Sized,
    {
        match method_name.ident.as_str() {
            "set" => Ok(MethodProps::new(
                TypeDef::Unknown,
                AsnToken::Set.into(),
                vec![TypeDef::Asn],
            )
            .consume_value()),
            _ => Err(format!(
                "Unknown method: '{}' for type Asn",
                method_name.ident
            )
            .into()),
        }
    }

    fn into_type(
        self,
        type_def: &TypeDef,
    ) -> Result<TypeValue, CompileError> {
        match type_def {
            TypeDef::Asn => {
                Ok(TypeValue::Builtin(BuiltinTypeValue::Asn(self)))
            }
            _ => {
                Err(format!("Cannot convert type Asn to type {:?}", type_def)
                    .into())
            }
        }
    }

    fn exec_value_method<'a>(
        &'a self,
        method_token: usize,
        args: &'a [StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        match method_token.into() {
            AsnToken::Set => {
                if let TypeValue::Builtin(BuiltinTypeValue::Asn(asn)) =
                    args[0].as_ref()
                {
                    Ok(TypeValue::from(Asn::new(asn.0)))
                } else {
                    Err(VmError::AnonymousArgumentNotFound)
                }
            }
        }
    }

    fn exec_consume_value_method(
        self,
        _method_token: usize,
        _args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }

    fn exec_type_method<'a>(
        _method_token: usize,
        _args: &[StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }
}

impl PartialEq for Asn {
    fn eq(&self, other: &Self) -> bool {
        trace!("EQ Asn");
        if let Ok(TypeValue::Builtin(BuiltinTypeValue::Asn(Asn(o)))) = other.into_type(&TypeDef::Asn) {
            o == self.0
        } else {
            false
        }
    }
}

// impl std::hash::Hash for Asn {
//     fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
//         self.value.0.hash(state);
//     }
// }

impl From<Asn> for TypeValue {
    fn from(value: Asn) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::Asn(value))
    }
}

impl From<u32> for Asn {
    fn from(value: u32) -> Self {
        Asn(value.into())
    }
}

impl Display for Asn {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug)]
pub enum AsnToken {
    Set,
}

impl From<usize> for AsnToken {
    fn from(val: usize) -> Self {
        match val {
            0 => AsnToken::Set,
            _ => panic!("Unknown token value: {} for Asn", val),
        }
    }
}

impl From<AsnToken> for usize {
    fn from(val: AsnToken) -> Self {
        val as usize
    }
}

// ----------- AsPath type --------------------------------------------------

type RoutecoreHop = routecore::bgp::aspath::Hop<Vec<u8>>;

#[derive(Debug, Eq, PartialEq, Clone, Default, Hash, Serialize)]
pub struct AsPath(pub(crate) routecore::bgp::aspath::HopPath);

impl AsPath {
    pub fn new(
        as_path: Vec<routecore::asn::Asn>,
    ) -> Result<Self, LongSegmentError> {
        let path = routecore::bgp::aspath::HopPath::try_from(as_path)
            .map_err(|_| LongSegmentError)?;
        Ok(AsPath(path))
    }

    pub fn from_vec_u32(as_path: Vec<u32>) -> Result<Self, LongSegmentError> {
        let as_path = as_path
            .into_iter()
            .map(routecore::asn::Asn::from_u32)
            .collect();
        AsPath::new(as_path)
    }

    pub fn into_hops(self) -> Vec<Hop> {
        self.0.into_iter().map(Hop).collect::<Vec<_>>()
    }

    fn into_routecore_hops(
        self,
    ) -> Vec<routecore::bgp::aspath::Hop<Vec<u8>>> {
        self.0.into_iter().collect::<Vec<_>>()
    }

    pub fn contains(&self, hop: &Hop) -> bool {
        self.0.contains(&hop.0)
    }
}

impl RotoType for AsPath {
    fn get_props_for_method(
        _ty: TypeDef,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError>
    where
        Self: std::marker::Sized,
    {
        match method_name.ident.as_str() {
            "origin" => Ok(MethodProps::new(
                TypeDef::Asn,
                AsPathToken::Origin.into(),
                vec![],
            )),
            "contains" => Ok(MethodProps::new(
                TypeDef::Boolean,
                AsPathToken::Contains.into(),
                vec![TypeDef::Asn],
            )),
            "len" => Ok(MethodProps::new(
                TypeDef::U8,
                AsPathToken::Len.into(),
                vec![],
            )),
            _ => Err(format!(
                "Unknown method '{}' for type AsPath",
                method_name.ident
            )
            .into()),
        }
    }

    fn into_type(
        self,
        type_def: &TypeDef,
    ) -> Result<TypeValue, CompileError> {
        match type_def {
            TypeDef::AsPath => {
                Ok(TypeValue::Builtin(BuiltinTypeValue::AsPath(self)))
            }
            _ => Err(format!(
                "Cannot convert type AsPath to type {:?}",
                type_def
            )
            .into()),
        }
    }

    fn exec_value_method<'a>(
        &'a self,
        method: usize,
        args: &'a [StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        match method.into() {
            AsPathToken::Origin => match self.0.origin().cloned() {
                Some(origin_asn) => Ok(
                    TypeValue::Builtin(BuiltinTypeValue::Asn(Asn(origin_asn
                        .try_into_asn()
                        .unwrap())))
                ),
                None => Err(VmError::InvalidPayload),
            },
            AsPathToken::Contains => {
                if let TypeValue::Builtin(BuiltinTypeValue::Asn(Asn(
                    search_asn,
                ))) = args[0].as_ref()
                {
                    let contains =
                        self.contains(&Hop(RoutecoreHop::from(*search_asn)));
                    Ok(TypeValue::Builtin(BuiltinTypeValue::Boolean(Boolean(
                        contains,
                    ))))
                } else {
                    Ok(TypeValue::Unknown)
                }
            },
            AsPathToken::Len => {
                let len = self.0.hop_count();
                Ok(TypeValue::Builtin(BuiltinTypeValue::U8(U8(len as u8))))
            }
        }
    }

    fn exec_consume_value_method(
        self,
        _method_token: usize,
        _args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }

    fn exec_type_method<'a>(
        _method_token: usize,
        _args: &[StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }
}

impl VectorValue for crate::types::builtin::AsPath {
    type ReadItem = Hop;
    type WriteItem = Asn;

    fn prepend_vec(
        &mut self,
        vector: Vec<Self::WriteItem>,
    ) -> Result<(), LongSegmentError> {
        let mut as_path = vector
            .iter()
            .map(|a| routecore::bgp::aspath::Hop::from(a.0))
            .collect::<Vec<_>>();
        as_path.extend_from_slice(
            std::mem::take(self).into_routecore_hops().as_slice(),
        );

        self.0 = as_path.into();

        Ok(())
    }

    fn append_vec(
        &mut self,
        vector: Vec<Self::WriteItem>,
    ) -> Result<(), LongSegmentError> {
        let mut as_path = std::mem::take(self).into_routecore_hops();
        as_path.extend_from_slice(
            vector
                .iter()
                .map(|a| a.0.into())
                .collect::<Vec<_>>()
                .as_slice(),
        );

        self.0 = as_path.into();

        Ok(())
    }

    // Naieve insert that will try to append to the segment that is already
    // in place at the specified position. Fancier, more conditional ways are
    // available to the roto user, but those methods are implemented directly
    // on builtin::AsPath.
    fn insert_vec(
        &mut self,
        pos: u8,
        vector: Vec<Self::WriteItem>,
    ) -> Result<(), LongSegmentError> {
        let as_path = std::mem::take(self).into_routecore_hops();
        let mut left_path = as_path[..pos as usize].to_vec();
        left_path.extend_from_slice(
            vector
                .into_iter()
                .map(|a| a.0.into())
                .collect::<Vec<_>>()
                .as_slice(),
        );
        left_path.extend_from_slice(&self.0[pos as usize..]);

        self.0 = as_path.into();

        Ok(())
    }

    fn vec_len(&self) -> Option<usize> {
        Some(self.0.hop_count())
    }

    fn vec_is_empty(&self) -> bool {
        self.0.hop_count() == 0
    }

    fn into_vec(self) -> Vec<Self::ReadItem> {
        self.into_hops()
    }
}

impl From<AsPath> for TypeValue {
    fn from(val: AsPath) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::AsPath(val))
    }
}

impl From<routecore::bgp::aspath::HopPath> for AsPath {
    fn from(value: routecore::bgp::aspath::HopPath) -> Self {
        AsPath(value)
    }
}

impl Display for AsPath {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[repr(u8)]
#[derive(Debug)]
pub(crate) enum AsPathToken {
    Origin = 1,
    Contains = 2,
    Len = 3,
}

impl From<usize> for AsPathToken {
    fn from(value: usize) -> Self {
        match value {
            1 => AsPathToken::Origin,
            2 => AsPathToken::Contains,
            3 => AsPathToken::Len,
            _ => panic!("Unknown AsPathToken value: {}", value),
        }
    }
}

impl From<AsPathToken> for usize {
    fn from(val: AsPathToken) -> Self {
        val as usize
    }
}

impl From<Vec<Hop>> for AsPath {
    fn from(value: Vec<Hop>) -> Self {
        AsPath(value.into_iter().map(|h| h.0).collect::<Vec<_>>().into())
    }
}

//------------ Hop type -----------------------------------------------------

// A read-only type that contains an ASN or a more complex segment of a AS
// PATH, e.g. an AS_SET.

#[derive(Debug, Clone, Eq, PartialEq, Hash, Serialize)]
pub struct Hop(pub(crate) routecore::bgp::aspath::Hop<Vec<u8>>);

impl RotoType for Hop {
    fn get_props_for_method(
        _ty: TypeDef,
        _method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError>
    where
        Self: std::marker::Sized,
    {
        todo!()
    }

    fn into_type(
        self,
        _type_value: &TypeDef,
    ) -> Result<TypeValue, CompileError>
    where
        Self: std::marker::Sized,
    {
        todo!()
    }

    fn exec_value_method<'a>(
        &'a self,
        _method_token: usize,
        _args: &'a [StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }

    fn exec_consume_value_method(
        self,
        _method_token: usize,
        _args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }

    fn exec_type_method<'a>(
        _method_token: usize,
        _args: &[StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }
}

impl Display for Hop {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<Asn> for Hop {
    fn from(value: Asn) -> Self {
        Hop(value.0.into())
    }
}

//------------ OriginType type ----------------------------------------------

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash, Serialize)]
pub struct OriginType(pub(crate) routecore::bgp::types::OriginType);

impl OriginType {
    pub fn into_inner(self) -> routecore::bgp::types::OriginType {
        self.0
    }
}

impl RotoType for OriginType {
    fn get_props_for_method(
        _ty: TypeDef,
        _method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError>
    where
        Self: std::marker::Sized,
    {
        todo!()
    }

    fn into_type(
        self,
        _type_value: &TypeDef,
    ) -> Result<TypeValue, CompileError>
    where
        Self: std::marker::Sized,
    {
        todo!()
    }

    fn exec_value_method<'a>(
        &'a self,
        _method_token: usize,
        _args: &'a [StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }

    fn exec_consume_value_method(
        self,
        _method_token: usize,
        _args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }

    fn exec_type_method<'a>(
        _method_token: usize,
        _args: &[StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }
}

impl From<OriginType> for TypeValue {
    fn from(value: OriginType) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::OriginType(value))
    }
}

impl From<routecore::bgp::types::OriginType> for TypeValue {
    fn from(value: routecore::bgp::types::OriginType) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::OriginType(OriginType(value)))
    }
}

impl From<routecore::bgp::types::OriginType> for BuiltinTypeValue {
    fn from(value: routecore::bgp::types::OriginType) -> Self {
        BuiltinTypeValue::OriginType(OriginType(value))
    }
}

impl Display for OriginType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

//------------ NextHop type -------------------------------------------------

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash, Serialize)]
pub struct NextHop(pub(crate) routecore::bgp::types::NextHop);

impl RotoType for NextHop {
    fn get_props_for_method(
        _ty: TypeDef,
        _method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError>
    where
        Self: std::marker::Sized,
    {
        todo!()
    }

    fn into_type(
        self,
        _type_value: &TypeDef,
    ) -> Result<TypeValue, CompileError>
    where
        Self: std::marker::Sized,
    {
        todo!()
    }

    fn exec_value_method<'a>(
        &'a self,
        _method_token: usize,
        _args: &'a [StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }

    fn exec_consume_value_method(
        self,
        _method_token: usize,
        _args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }

    fn exec_type_method<'a>(
        _method_token: usize,
        _args: &[StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }
}

impl From<NextHop> for TypeValue {
    fn from(value: NextHop) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::NextHop(value))
    }
}

impl From<routecore::bgp::types::NextHop> for TypeValue {
    fn from(value: routecore::bgp::types::NextHop) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::NextHop(NextHop(value)))
    }
}

impl From<routecore::bgp::types::NextHop> for BuiltinTypeValue {
    fn from(value: routecore::bgp::types::NextHop) -> Self {
        BuiltinTypeValue::NextHop(NextHop(value))
    }
}

impl Display for NextHop {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

//------------ Multi Exit Discriminator type --------------------------------

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash, Serialize)]
pub struct MultiExitDisc(pub(crate) routecore::bgp::types::MultiExitDisc);

impl RotoType for MultiExitDisc {
    fn get_props_for_method(
        _ty: TypeDef,
        _method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError>
    where
        Self: std::marker::Sized,
    {
        todo!()
    }

    fn into_type(
        self,
        _type_value: &TypeDef,
    ) -> Result<TypeValue, CompileError>
    where
        Self: std::marker::Sized,
    {
        todo!()
    }

    fn exec_value_method<'a>(
        &'a self,
        _method_token: usize,
        _args: &'a [StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }

    fn exec_consume_value_method(
        self,
        _method_token: usize,
        _args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }

    fn exec_type_method<'a>(
        _method_token: usize,
        _args: &[StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }
}

impl From<MultiExitDisc> for TypeValue {
    fn from(value: MultiExitDisc) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::MultiExitDisc(value))
    }
}

impl From<routecore::bgp::types::MultiExitDisc> for TypeValue {
    fn from(value: routecore::bgp::types::MultiExitDisc) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::MultiExitDisc(MultiExitDisc(
            value,
        )))
    }
}

impl From<routecore::bgp::types::MultiExitDisc> for BuiltinTypeValue {
    fn from(value: routecore::bgp::types::MultiExitDisc) -> Self {
        BuiltinTypeValue::MultiExitDisc(MultiExitDisc(value))
    }
}

impl Display for MultiExitDisc {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

//------------ Unknown type -------------------------------------------------

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Unknown;

impl From<Unknown> for TypeValue {
    fn from(_value: Unknown) -> Self {
        TypeValue::Unknown
    }
}

impl RotoType for Unknown {
    fn get_props_for_method(
        _ty: TypeDef,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError>
    where
        Self: std::marker::Sized,
    {
        match method_name.ident.as_str() {
            "is_unknown" => Ok(MethodProps::new(
                TypeDef::Boolean,
                UnknownToken::IsUnknown.into(),
                vec![],
            )
            .consume_value()),
            _ => Err(format!(
                "Unknown method: '{}' for type Unknown",
                method_name.ident
            )
            .into()),
        }
    }

    fn into_type(
        self,
        _type_value: &TypeDef,
    ) -> Result<TypeValue, CompileError>
    where
        Self: std::marker::Sized,
    {
        todo!()
    }

    fn exec_value_method<'a>(
        &'a self,
        _method_token: usize,
        _args: &'a [StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }

    fn exec_consume_value_method(
        self,
        _method_token: usize,
        _args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }

    fn exec_type_method<'a>(
        _method_token: usize,
        _args: &[StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }
}

#[derive(Debug)]
pub(crate) enum UnknownToken {
    IsUnknown,
}

impl From<usize> for UnknownToken {
    fn from(val: usize) -> Self {
        match val {
            0 => UnknownToken::IsUnknown,
            _ => panic!("Unknown token value: {}", val),
        }
    }
}

impl From<UnknownToken> for usize {
    fn from(val: UnknownToken) -> Self {
        match val {
            UnknownToken::IsUnknown => 0,
        }
    }
}


//------------ Local Preference type ----------------------------------------


#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Serialize)]
pub struct LocalPref(pub(crate) routecore::bgp::types::LocalPref);

impl RotoType for LocalPref {
    fn get_props_for_method(
        _ty: TypeDef,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError>
    where
        Self: std::marker::Sized,
    {
        match method_name.ident.as_str() {
            "set" => Ok(MethodProps::new(
                TypeDef::Unknown,
                LocalPrefToken::Set.into(),
                vec![TypeDef::IntegerLiteral],
            )
            .consume_value()),
            _ => Err(format!(
                "Unknown method: '{}' for type LocalPref",
                method_name.ident
            )
            .into()),
        }
    }

    fn into_type(
        self,
        _type_value: &TypeDef,
    ) -> Result<TypeValue, CompileError>
    where
        Self: std::marker::Sized,
    {
        todo!()
    }

    fn exec_value_method<'a>(
        &'a self,
        method_token: usize,
        _args: &'a [StackValue],
        res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        match method_token.into() {
            LocalPrefToken::Set => {
                Ok(TypeValue::Builtin(BuiltinTypeValue::LocalPref(*self)))
            }
            _ => Err(VmError::InvalidMethodCall),
        }
    }

    fn exec_consume_value_method(
        self,
        _method_token: usize,
        _args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }

    fn exec_type_method<'a>(
        _method_token: usize,
        _args: &[StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }
}

impl From<LocalPref> for TypeValue {
    fn from(value: LocalPref) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::LocalPref(value))
    }
}

impl From<routecore::bgp::types::LocalPref> for TypeValue {
    fn from(value: routecore::bgp::types::LocalPref) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::LocalPref(LocalPref(value)))
    }
}

impl From<routecore::bgp::types::LocalPref> for BuiltinTypeValue {
    fn from(value: routecore::bgp::types::LocalPref) -> Self {
        BuiltinTypeValue::LocalPref(LocalPref(value))
    }
}

impl Display for LocalPref {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug)]
pub enum LocalPrefToken {
    Set,
}

impl From<usize> for LocalPrefToken {
    fn from(val: usize) -> Self {
        match val {
            0 => LocalPrefToken::Set,
            _ => panic!("Unknown token value: {}", val),
        }
    }
}

impl From<LocalPrefToken> for usize {
    fn from(val: LocalPrefToken) -> Self {
        match val {
            LocalPrefToken::Set => 0,
        }
    }
}

//------------ Aggregator type ----------------------------------------------

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Serialize)]
pub struct AtomicAggregator(
    pub(crate) routecore::bgp::message::update::Aggregator,
);

impl RotoType for AtomicAggregator {
    fn get_props_for_method(
        _ty: TypeDef,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError>
    where
        Self: std::marker::Sized,
    {
        match method_name.ident.as_str() {
            "set" => Ok(MethodProps::new(
                TypeDef::Unknown,
                AtomicAggregatorToken::Set.into(),
                vec![TypeDef::Asn, TypeDef::IpAddress],
            )
            .consume_value()),
            _ => Err(format!(
                "Unknown method: '{}' for type Atomic Aggregator",
                method_name.ident
            )
            .into()),
        }
    }

    fn into_type(
        self,
        _type_value: &TypeDef,
    ) -> Result<TypeValue, CompileError>
    where
        Self: std::marker::Sized,
    {
        todo!()
    }

    fn exec_value_method<'a>(
        &'a self,
        _method_token: usize,
        _args: &'a [StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }

    fn exec_consume_value_method(
        self,
        _method_token: usize,
        _args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }

    fn exec_type_method<'a>(
        _method_token: usize,
        _args: &[StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }
}

impl From<AtomicAggregator> for TypeValue {
    fn from(value: AtomicAggregator) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::AtomicAggregator(value))
    }
}

impl From<AtomicAggregator> for BuiltinTypeValue {
    fn from(value: AtomicAggregator) -> Self {
        BuiltinTypeValue::AtomicAggregator(value)
    }
}

impl From<routecore::bgp::message::update::Aggregator> for TypeValue {
    fn from(value: routecore::bgp::message::update::Aggregator) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::AtomicAggregator(
            AtomicAggregator(value),
        ))
    }
}

impl From<routecore::bgp::message::update::Aggregator> for BuiltinTypeValue {
    fn from(value: routecore::bgp::message::update::Aggregator) -> Self {
        BuiltinTypeValue::AtomicAggregator(AtomicAggregator(value))
    }
}

impl std::fmt::Display for AtomicAggregator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug)]
pub enum AtomicAggregatorToken {
    Set,
}

impl From<usize> for AtomicAggregatorToken {
    fn from(val: usize) -> Self {
        match val {
            0 => AtomicAggregatorToken::Set,
            _ => panic!("Unknown token value: {}", val),
        }
    }
}

impl From<AtomicAggregatorToken> for usize {
    fn from(val: AtomicAggregatorToken) -> Self {
        match val {
            AtomicAggregatorToken::Set => 0,
        }
    }
}

//------------ RouteStatus type ---------------------------------------------

// Status is piece of metadata that writes some (hopefully) relevant state of
// per-peer BGP session into every route. The goal is to be able to enable
// the logic in `rib-units` to decide whether routes should be send to its
// output and to be able output this information to API clients, without
// having to go back to the units that keep the per-peer session state.
#[derive(Debug, Eq, PartialEq, Copy, Clone, Default, Hash, Serialize)]
pub enum RouteStatus {
    // Between start and EOR on a BGP peer-session
    InConvergence,
    // After EOR for a BGP peer-session, either `Graceful Restart` or EOR
    UpToDate,
    // After hold-timer expiry
    Stale,
    // After the request for a Route Refresh to a peer and the reception of a
    // new route
    StartOfRouteRefresh,
    // After the reception of a withdrawal
    Withdrawn,
    // Status not relevant, e.g. a RIB that holds archived routes.
    #[default]
    Empty,
}

impl std::fmt::Display for RouteStatus {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RouteStatus::InConvergence => write!(f, "in convergence"),
            RouteStatus::UpToDate => write!(f, "up to date"),
            RouteStatus::Stale => write!(f, "stale"),
            RouteStatus::StartOfRouteRefresh => {
                write!(f, "start of route refresh")
            }
            RouteStatus::Withdrawn => write!(f, "withdrawn"),
            RouteStatus::Empty => write!(f, "empty"),
        }
    }
}

impl From<TypeValue> for RouteStatus {
    fn from(value: TypeValue) -> Self {
        if let TypeValue::Builtin(BuiltinTypeValue::RouteStatus(value)) =
            value
        {
            value
        } else {
            panic!("invalid something");
        }
    }
}
