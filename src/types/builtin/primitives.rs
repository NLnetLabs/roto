
use std::fmt::{Display, Formatter};

use routecore::asn::LongSegmentError;

use crate::ast::ShortString;
use crate::compile::CompileError;
use crate::traits::{MethodProps, RotoType, TokenConvert};
use crate::vm::VmError;

use super::super::typedef::TypeDef;
use super::super::typevalue::TypeValue;
use super::builtin_type_value::BuiltinTypeValue;

// ----------- A simple u32 type --------------------------------------------

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub struct U32(pub(crate) Option<u32>);

impl U32 {
    pub fn new(val: u32) -> Self {
        U32(Some(val))
    }
}

impl From<U32> for TypeValue {
    fn from(val: U32) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::U32(val))
    }
}

impl RotoType for U32 {
    fn get_props_for_method(
        self,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError>
    where
        Self: std::marker::Sized,
    {
        match method_name.ident.as_str() {
            "set" => Ok(MethodProps::new(
                TypeValue::Unknown,
                U32Token::Set.into(),
                vec![TypeDef::IntegerLiteral],
            ).consume_value()),
            _ => Err(format!(
                "Unknown method: '{}' for type U32",
                method_name.ident
            )
            .into()),
        }
    }

    fn exec_value_method<'a>(
        &'a self,
        _method_token: usize,
        _args: &[&TypeValue],
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue + 'a>, VmError> {
        todo!()
    }

    fn exec_consume_value_method(
        self,
        _method_token: usize,
        _args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue>, VmError> {
        todo!()
    }

    fn exec_type_method<'a>(
        _method_token: usize,
        _args: &[&'a TypeValue],
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue + 'a>, VmError> {
        todo!()
    }

    fn into_type(
        self,
        type_def: &TypeDef,
    ) -> Result<TypeValue, CompileError> {
        match type_def {
            TypeDef::U32 => {
                Ok(TypeValue::Builtin(BuiltinTypeValue::U32(self)))
            }
            _ => {
                Err(format!("Cannot convert type U32 to type {:?}", type_def)
                    .into())
            }
        }
    }
}

impl Display for U32 {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let v = match self.0 {
            Some(v) => v.to_string(),
            None => "None".to_string(),
        };
        write!(f, "{}", v)
    }
}

#[derive(Debug)]
pub enum U32Token {
    Set,
}

impl TokenConvert for U32Token {}

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

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub struct U8(pub(crate) Option<u8>);

impl U8 {
    pub fn new(val: u8) -> Self {
        U8(Some(val))
    }
}

impl RotoType for U8 {
    fn get_props_for_method(
        self,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError>
    where
        Self: std::marker::Sized,
    {
        match method_name.ident.as_str() {
            "set" => Ok(MethodProps::new(
                TypeValue::Unknown,
                U8Token::Set.into(),
                vec![TypeDef::IntegerLiteral],
            ).consume_value()),
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
        _args: &[&TypeValue],
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue + 'a>, VmError> {
        todo!()
    }

    fn exec_consume_value_method(
        self,
        _method_token: usize,
        _args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue>, VmError> {
        todo!()
    }

    fn exec_type_method<'a>(
        _method_token: usize,
        _args: &[&'a TypeValue],
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue + 'a>, VmError> {
        todo!()
    }

    fn into_type(
        self,
        type_def: &TypeDef,
    ) -> Result<TypeValue, CompileError> {
        match type_def {
            // Self
            TypeDef::U8 => Ok(TypeValue::Builtin(BuiltinTypeValue::U8(self))),
            TypeDef::U32 => match self.0 {
                Some(value) => Ok(TypeValue::Builtin(BuiltinTypeValue::U32(
                    U32(Some(value as u32)),
                ))),
                None => Err("Cannot convert None to U32".into()),
            },
            TypeDef::PrefixLength => match self.0 {
                Some(value) => match value {
                    0..=128 => Ok(TypeValue::Builtin(
                        BuiltinTypeValue::PrefixLength(PrefixLength(Some(
                            value,
                        ))),
                    )),
                    _ => Err(format!(
                        "Prefix length must be between 0 and 128, not {}",
                        value
                    )
                    .into()),
                },
                None => Ok(TypeValue::Builtin(
                    BuiltinTypeValue::PrefixLength(PrefixLength(None)),
                )),
            },
            TypeDef::IntegerLiteral => match self.0 {
                Some(value) => match value {
                    0..=128 => Ok(TypeValue::Builtin(
                        BuiltinTypeValue::PrefixLength(PrefixLength(Some(
                            value,
                        ))),
                    )),
                    _ => Err(format!(
                        "Prefix length must be between 0 and 128, not {}",
                        value
                    )
                    .into()),
                },
                None => Ok(TypeValue::Builtin(
                    BuiltinTypeValue::PrefixLength(PrefixLength(None)),
                )),
            },
            _ => {
                Err(format!("Cannot convert type U8 to type {:?}", type_def)
                    .into())
            }
        }
    }
}

impl Display for U8 {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let v = match self.0 {
            Some(v) => v.to_string(),
            None => "None".to_string(),
        };
        write!(f, "{}", v)
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

impl TokenConvert for U8Token {}

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

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub struct Boolean(pub(crate) Option<bool>);
impl Boolean {
    pub fn new(val: bool) -> Self {
        Boolean(Some(val))
    }

    pub fn is_false(&self) -> Result<bool, VmError> {
        if let Boolean(Some(bool_val)) = self {
            Ok(!*bool_val)
        } else {
            Err(VmError::InvalidValueType)
        }
    }
}

impl RotoType for Boolean {
    fn get_props_for_method(
        self,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError>
    where
        Self: std::marker::Sized,
    {
        match method_name.ident.as_str() {
            "set" => Ok(MethodProps::new(
                TypeValue::Unknown,
                BooleanToken::Set.into(),
                vec![TypeDef::Boolean],
            ).consume_value()),
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
        _args: &[&TypeValue],
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue + 'a>, VmError> {
        todo!()
    }

    fn exec_consume_value_method(
        self,
        _method_token: usize,
        _args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue>, VmError> {
        todo!()
    }

    fn exec_type_method<'a>(
        _method_token: usize,
        _args: &[&'a TypeValue],
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue + 'a>, VmError> {
        todo!()
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

impl Display for Boolean {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let v = match self.0 {
            Some(v) => v.to_string(),
            None => "None".to_string(),
        };
        write!(f, "{}", v)
    }
}

#[derive(Debug)]
pub enum BooleanToken {
    Set,
}

impl TokenConvert for BooleanToken {}

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

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct StringLiteral(pub(crate) Option<ShortString>);
impl StringLiteral {
    pub fn new(val: ShortString) -> Self {
        StringLiteral(Some(val))
    }
}

impl RotoType for StringLiteral {
    fn get_props_for_method(
        self,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError> {
        match method_name.ident.as_str() {
            "cmp" => Ok(MethodProps::new(
                TypeValue::from(&TypeDef::IntegerLiteral),
                StringLiteralToken::Cmp.into(),
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
        _args: &[&TypeValue],
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue + 'a>, VmError> {
        todo!()
    }

     fn exec_consume_value_method(
        self,
        _method_token: usize,
        _args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue>, VmError> {
        todo!()
    }

    fn exec_type_method<'a>(
        _method_token: usize,
        _args: &[&'a TypeValue],
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue + 'a>, VmError> {
        todo!()
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
        let v = match &self.0 {
            Some(v) => v.to_string(),
            None => "None".to_string(),
        };
        write!(f, "{}", v)
    }
}

#[derive(Debug)]
pub enum StringLiteralToken {
    Cmp,
}

impl TokenConvert for StringLiteralToken {}

impl From<usize> for StringLiteralToken {
    fn from(val: usize) -> Self {
        match val {
            0 => StringLiteralToken::Cmp,
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

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub struct IntegerLiteral(pub(crate) Option<i64>);
impl IntegerLiteral {
    pub fn new(val: i64) -> Self {
        IntegerLiteral(Some(val))
    }
}

impl RotoType for IntegerLiteral {
    fn get_props_for_method(
        self,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError> {
        match method_name.ident.as_str() {
            "cmp" => Ok(MethodProps::new(
                TypeValue::from(&TypeDef::IntegerLiteral),
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
                Some(value) => match value {
                    0..=128 => Ok(TypeValue::Builtin(
                        BuiltinTypeValue::PrefixLength(PrefixLength(Some(
                            value as u8,
                        ))),
                    )),
                    _ => Err(format!(
                        "Prefix length must be between 0 and 128, not {}",
                        value
                    )
                    .into()),
                },
                None => Ok(TypeValue::Builtin(
                    BuiltinTypeValue::PrefixLength(PrefixLength(None)),
                )),
            },
            _ => Err(format!(
                "Cannot convert type IntegerLiteral to type {:?}",
                type_def
            )
            .into()),
        }
    }

    fn exec_value_method<'a>(
        &'a self,
        _method_token: usize,
        _args: &[&TypeValue],
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue + 'a>, VmError> {
        todo!()
    }

     fn exec_consume_value_method(
        self,
        _method_token: usize,
        _args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue>, VmError> {
        todo!()
    }

    fn exec_type_method<'a>(
        _method_token: usize,
        _args: &[&'a TypeValue],
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue + 'a>, VmError> {
        todo!()
    }
}

impl From<IntegerLiteral> for TypeValue {
    fn from(val: IntegerLiteral) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::IntegerLiteral(val))
    }
}

impl Display for IntegerLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let v = match self.0 {
            Some(v) => v.to_string(),
            None => "None".to_string(),
        };
        write!(f, "{}", v)
    }
}

#[derive(Debug)]
pub(crate) enum IntegerLiteralToken {
    Cmp,
}

impl TokenConvert for IntegerLiteralToken {}

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

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub struct HexLiteral(pub(crate) Option<u64>);
impl HexLiteral {
    pub fn new(val: u64) -> Self {
        HexLiteral(Some(val))
    }
}

impl RotoType for HexLiteral {
    fn get_props_for_method(
        self,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError> {
        match method_name.ident.as_str() {
            "cmp" => Ok(MethodProps::new(
                TypeValue::from(&TypeDef::IntegerLiteral),
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
                let c = Community::new();
                Ok(TypeValue::Builtin(BuiltinTypeValue::Community(c)))
            }
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
        _args: &[&TypeValue],
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue + 'a>, VmError> {
        todo!()
    }

     fn exec_consume_value_method(
        self,
        _method_token: usize,
        _args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue>, VmError> {
        todo!()
    }

    fn exec_type_method<'a>(
        _method_token: usize,
        _args: &[&'a TypeValue],
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue + 'a>, VmError> {
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
        let v = match self.0 {
            Some(v) => format!("0x{:x}", v),
            None => "None".to_string(),
        };
        write!(f, "{}", v)
    }
}

#[derive(Debug)]
pub(crate) enum HexLiteralToken {
    Cmp,
}

impl TokenConvert for HexLiteralToken {}

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

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub struct Prefix(pub(crate) Option<routecore::addr::Prefix>);

impl Prefix {
    pub fn new(prefix: routecore::addr::Prefix) -> Self {
        Self(Some(prefix))
    }

    pub fn empty() -> Self {
        Self(None)
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
        self,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError>
    where
        Self: std::marker::Sized,
    {
        match method_name.ident.as_str() {
            "from" => Ok(MethodProps::new(
                TypeValue::from(&TypeDef::Prefix),
                PrefixToken::From.into(),
                vec![TypeDef::IpAddress, TypeDef::PrefixLength],
            )),
            "address" => Ok(MethodProps::new(
                TypeValue::from(&TypeDef::IpAddress),
                PrefixToken::Address.into(),
                vec![],
            )),
            "len" => Ok(MethodProps::new(
                TypeValue::from(&TypeDef::PrefixLength),
                PrefixToken::Len.into(),
                vec![],
            )),
            "matches" => Ok(MethodProps::new(
                TypeValue::from(&TypeDef::Boolean),
                PrefixToken::Matches.into(),
                vec![TypeDef::Prefix],
            )),
            "exists" => Ok(MethodProps::new(
                TypeValue::Builtin(BuiltinTypeValue::Boolean(Boolean(None))),
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
        _args: &[&'a TypeValue],
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue + 'a>, VmError> {
        match method_token.into() {
            PrefixToken::Address => {
                let prefix = self.0.ok_or(VmError::InvalidConversion)?;
                Ok(Box::new(move || {
                    TypeValue::Builtin(BuiltinTypeValue::IpAddress(
                        IpAddress(Some(prefix.addr())),
                    ))
                }))
            }
            PrefixToken::Len => {
                if let Prefix(
                    Some(pfx),
                ) = self
                {
                    Ok(Box::new(move || {
                        TypeValue::Builtin(BuiltinTypeValue::PrefixLength(
                            PrefixLength(Some(pfx.len())),
                        ))
                    }))
                } else {
                    Err(VmError::ArgumentNotFound)
                }
            }
            PrefixToken::From => unimplemented!(),
            PrefixToken::Exists => {
                if self.0.is_some() {
                    Ok(Box::new(move || true.into()))
                } else {
                    Ok(Box::new(move || false.into()))
                }
            },
            PrefixToken::Matches => todo!(),
        }
    }

     fn exec_consume_value_method(
        self,
        _method_token: usize,
        _args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue>, VmError> {
        todo!()
    }

    fn exec_type_method<'a>(
        method_token: usize,
        args: &[&'a TypeValue],
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue + 'a>, VmError> {
        match method_token.into() {
            PrefixToken::From => {
                if let TypeValue::Builtin(BuiltinTypeValue::IpAddress(ip)) =
                    args[0]
                {
                    let len: PrefixLength = args[1].try_into().map_err(|_e| VmError::InvalidConversion)?;
                    let ip = ip.0.ok_or("Cannot convert empty IP address").map_err(|_e| VmError::InvalidConversion)?;
                    Ok(Box::new(move || {
                        TypeValue::Builtin(BuiltinTypeValue::Prefix(
                            Prefix::new(
                                routecore::addr::Prefix::new(ip, len.into())
                                    .map_err(|e| {
                                        format!("Invalid prefix: {}", e)
                                    })
                                    .unwrap(),
                            ),
                        ))
                    }))
                } else {
                    Err(VmError::ArgumentNotFound)
                }
            }
            PrefixToken::Exists => unimplemented!(),
            PrefixToken::Address => unimplemented!(),
            PrefixToken::Len => unimplemented!(),
            PrefixToken::Matches => unimplemented!(),
            // _ => {
            //     Err(VmError::InvalidMethodCall)
            // }
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
        Prefix(Some(val))
    }
}

impl Display for Prefix {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let Some(prefix) = &self.0 {
            write!(f, "{}", prefix)
        } else {
            write!(f, "empty")
        }
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

impl TokenConvert for PrefixToken {}

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

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub struct PrefixLength(pub(crate) Option<u8>);

impl PrefixLength {
    pub fn new(val: u8) -> Self {
        PrefixLength(Some(val))
    }
}

impl RotoType for PrefixLength {
    fn get_props_for_method(
        self,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError>
    where
        Self: std::marker::Sized,
    {
        match method_name.ident.as_str() {
            "from" => Ok(MethodProps::new(
                TypeValue::from(&TypeDef::PrefixLength),
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
        _args: &[&TypeValue],
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue + 'a>, VmError> {
        todo!()
    }

     fn exec_consume_value_method(
        self,
        _method_token: usize,
        _args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue>, VmError> {
        todo!()
    }

    fn exec_type_method<'a>(
        _method_token: usize,
        _args: &[&'a TypeValue],
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue + 'a>, VmError> {
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
        if let Some(len) = &self.0 {
            write!(f, "/{}", len)
        } else {
            write!(f, "None")
        }
    }
}

#[derive(Debug)]
pub(crate) enum PrefixLengthToken {
    From,
}

impl TokenConvert for PrefixLengthToken {}

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
        val.0.unwrap()
    }
}

// ----------- Community ----------------------------------------------------


#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub struct Community(pub(crate) Option<routecore::bgp::communities::Community>);

impl Community {
    pub fn new() -> Self {
        Self(None)
    }
}

impl RotoType for Community {
    fn get_props_for_method(
        self,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError>
    where
        Self: std::marker::Sized,
    {
        match method_name.ident.as_str() {
            "from" => Ok(MethodProps::new(
                TypeValue::from(&TypeDef::Community),
                CommunityToken::From.into(),
                vec![TypeDef::U32],
            )),
            "standard" => Ok(MethodProps::new(
                TypeValue::from(&TypeDef::Community),
                CommunityToken::Standard.into(),
                vec![TypeDef::U32],
            )),
            "extended" => Ok(MethodProps::new(
                TypeValue::from(&TypeDef::Community),
                CommunityToken::Extended.into(),
                vec![TypeDef::U32],
            )),
            "large" => Ok(MethodProps::new(
                TypeValue::from(&TypeDef::Community),
                CommunityToken::Large.into(),
                vec![TypeDef::U32],
            )),
            "as" => Ok(MethodProps::new(
                TypeValue::from(&TypeDef::U32),
                CommunityToken::As.into(),
                vec![],
            )),
            "value" => Ok(MethodProps::new(
                TypeValue::from(&TypeDef::U32),
                CommunityToken::Value.into(),
                vec![],
            )),
            "exists" => Ok(MethodProps::new(
                TypeValue::from(&TypeDef::Boolean),
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
        _args: &[&TypeValue],
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue + 'a>, VmError> {
        todo!()
    }

     fn exec_consume_value_method(
        self,
        _method_token: usize,
        _args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue>, VmError> {
        todo!()
    }

    fn exec_type_method<'a>(
        _method_token: usize,
        _args: &[&'a TypeValue],
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue + 'a>, VmError> {
        todo!()
    }
}

impl From<Community> for TypeValue {
    fn from(val: Community) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::Community(val))
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

impl TokenConvert for CommunityToken {}

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

//------------ Communities --------------------------------------------------
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Communities(
    pub(crate) Option<Vec<Community>>,
);

//------------ MatchType ----------------------------------------------------

#[derive(Debug, Eq, PartialEq)]
pub enum MatchType {
    ExactMatch,
    LongestMatch,
    EmptyMatch,
}

// ----------- IpAddress type -----------------------------------------------

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub struct IpAddress(pub(crate) Option<std::net::IpAddr>);

impl IpAddress {
    pub fn new(addr: std::net::IpAddr) -> Self {
        IpAddress(Some(addr))
    }
}

impl RotoType for IpAddress {
    fn get_props_for_method(
        self,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError>
    where
        Self: std::marker::Sized,
    {
        match method_name.ident.as_str() {
            "from" => Ok(MethodProps::new(
                TypeValue::from(&TypeDef::IpAddress),
                IpAddressToken::From.into(),
                vec![TypeDef::String],
            )),
            "matches" => Ok(MethodProps::new(
                TypeValue::from(&TypeDef::Boolean),
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
        _args: &[&TypeValue],
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue + 'a>, VmError> {
        todo!();
    }

     fn exec_consume_value_method(
        self,
        _method_token: usize,
        _args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue>, VmError> {
        todo!()
    }

    fn exec_type_method<'a>(
        _method_token: usize,
        _args: &[&'a TypeValue],
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue + 'a>, VmError> {
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
        if let Some(addr) = self.0 {
            write!(f, "{}", addr)
        } else {
            write!(f, "None")
        }
    }
}

#[derive(Debug)]
pub(crate) enum IpAddressToken {
    From,
    Matches,
}

impl TokenConvert for IpAddressToken {}

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

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub struct Asn(pub(crate) Option<routecore::asn::Asn>);

impl Asn {
    pub fn new(asn: routecore::asn::Asn) -> Self {
        Asn(Some(asn))
    }

    pub fn from_u32(asn: u32) -> Self {
        Asn(Some(routecore::asn::Asn::from(asn)))
    }

    pub fn get_asn(&self) -> routecore::asn::Asn {
        self.0.unwrap()
    }
}

impl RotoType for Asn {
    fn get_props_for_method(
        self,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError>
    where
        Self: std::marker::Sized,
    {
        match method_name.ident.as_str() {
            "set" => Ok(MethodProps::new(
                TypeValue::Unknown,
                AsnToken::Set.into(),
                vec![TypeDef::Asn],
            ).consume_value()),
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
        args: &'a [&'a TypeValue],
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue + 'a>, VmError> {
        match method_token.into() {
            AsnToken::Set => {
                if let TypeValue::Builtin(BuiltinTypeValue::Asn(asn)) =
                    args[0]
                {
                    Ok(Box::new(move || {
                        TypeValue::from(Asn::new(asn.0.unwrap()))
                    }))
                } else {
                    Err(VmError::ArgumentNotFound)
                }
            }
        }
    }

     fn exec_consume_value_method(
        self,
        _method_token: usize,
        _args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue>, VmError> {
        todo!()
    }

    fn exec_type_method<'a>(
        _method_token: usize,
        _args: &[&'a TypeValue],
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue + 'a>, VmError> {
        todo!()
    }
}

impl From<Asn> for TypeValue {
    fn from(val: Asn) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::Asn(val))
    }
}

impl Display for Asn {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let Some(asn) = self.0 {
            write!(f, "{}", asn)
        } else {
            write!(f, "None")
        }
    }
}

#[derive(Debug)]
pub enum AsnToken {
    Set,
}

impl TokenConvert for AsnToken {}

impl From<usize> for AsnToken {
    fn from(val: usize) -> Self {
        match val {
            0 => AsnToken::Set,
            _ => panic!("Unknown token value: {}", val),
        }
    }
}

impl From<AsnToken> for usize {
    fn from(val: AsnToken) -> Self {
        val as usize
    }
}

// ----------- AsPath type --------------------------------------------------

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct AsPath(
    pub(crate) Option<routecore::asn::AsPath<Vec<routecore::asn::Asn>>>,
);

impl AsPath {
    pub fn new(
        as_path: Vec<routecore::asn::Asn>,
    ) -> Result<Self, LongSegmentError> {
        let mut new_as_path = routecore::asn::AsPathBuilder::new();
        for asn in as_path {
            new_as_path.push(asn)?;
        }
        let new_as_path = new_as_path.finalize();
        Ok(AsPath(Some(new_as_path)))
    }

    pub fn from_vec_u32(as_path: Vec<u32>) -> Result<Self, LongSegmentError> {
        let as_path = as_path
            .into_iter()
            .map(routecore::asn::Asn::from_u32)
            .collect();
        AsPath::new(as_path)
    }

    pub fn contains(&self, asn: routecore::asn::Asn) -> bool {
        if let Some(as_path) = &self.0 {
            as_path.iter().any(|a| a.elements().contains(&asn))
        } else {
            false
        }
    }

    fn inner_from_typevalue(
        type_value: TypeValue,
    ) -> Result<routecore::asn::AsPath<Vec<routecore::asn::Asn>>, CompileError>
    where
        Self: std::marker::Sized,
    {
        match type_value {
            TypeValue::Builtin(BuiltinTypeValue::AsPath(as_path)) => {
                if let Some(as_path) = as_path.0 {
                    Ok(as_path)
                } else {
                    Err("Invalid AsPath".into())
                }
            }
            _ => Err("Not an AsPath type".into()),
        }
    }
}

impl RotoType for AsPath {
    fn get_props_for_method(
        self,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError>
    where
        Self: std::marker::Sized,
    {
        match method_name.ident.as_str() {
            "origin" => Ok(MethodProps::new(
                TypeValue::Builtin(BuiltinTypeValue::Asn(Asn(None))),
                AsPathToken::Origin.into(),
                vec![],
            )),
            "contains" => Ok(MethodProps::new(
                TypeValue::Builtin(BuiltinTypeValue::Boolean(Boolean(None))),
                AsPathToken::Contains.into(),
                vec![TypeDef::Asn],
            )),
            "len" => Ok(MethodProps::new(
                TypeValue::Builtin(BuiltinTypeValue::U8(U8(None))),
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
        args: &'a [&'a TypeValue],
        _res_type: TypeDef,
    ) -> Result<Box<(dyn FnOnce() -> TypeValue + 'a)>, VmError> {
        match method.into() {
            AsPathToken::Origin => {
                if let Some(rc_as_path) = &self.0 {
                    Ok(Box::new(move || {
                        let origin: routecore::asn::Asn =
                            rc_as_path.iter().next().unwrap().elements()[0];

                        TypeValue::Builtin(BuiltinTypeValue::Asn(Asn(Some(
                            origin,
                        ))))
                    }))
                } else {
                    Ok(Box::new(move || {
                        TypeValue::Builtin(BuiltinTypeValue::Asn(Asn(None)))
                    }))
                }
            }
            AsPathToken::Contains => {
                if let Some(rc_as_path) = &self.0 {
                    {
                        Ok(Box::new(move || {
                            if let TypeValue::Builtin(
                                BuiltinTypeValue::Asn(Asn(search_asn)),
                            ) = args[0]
                            {
                                let contains =
                                    rc_as_path.contains(search_asn.unwrap());
                                TypeValue::Builtin(BuiltinTypeValue::Boolean(
                                    Boolean(Some(contains)),
                                ))
                            } else {
                                TypeValue::Unknown
                            }
                        }))
                    }
                } else {
                    Ok(Box::new(|| {
                        TypeValue::Unknown
                    }))
                }
            }
            AsPathToken::Len => {
                if let Some(rc_as_path) = &self.0 {
                    Ok(Box::new(|| {
                        let len = rc_as_path.iter().count();
                        TypeValue::Builtin(BuiltinTypeValue::U8(U8(Some(
                            len as u8,
                        ))))
                    }))
                } else {
                    Ok(Box::new(|| {
                        TypeValue::Builtin(BuiltinTypeValue::U8(U8(None)))
                    }))
                }
            }
        }
    }

     fn exec_consume_value_method(
        self,
        _method_token: usize,
        _args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue>, VmError> {
        todo!()
    }

    fn exec_type_method<'a>(
        _method_token: usize,
        _args: &[&'a TypeValue],
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue + 'a>, VmError> {
        todo!()
    }
}

impl From<AsPath> for TypeValue {
    fn from(val: AsPath) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::AsPath(val))
    }
}

impl Display for AsPath {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let Some(rc_as_path) = &self.0 {
            write!(f, "{}", rc_as_path)
        } else {
            write!(f, "None")
        }
    }
}

#[repr(u8)]
#[derive(Debug)]
pub(crate) enum AsPathToken {
    Origin = 1,
    Contains = 2,
    Len = 3,
}

impl TokenConvert for AsPathToken {}

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

//------------ OriginType type ----------------------------------------------

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct OriginType(
    pub(crate) Option<routecore::bgp::types::OriginType>,
);

impl RotoType for OriginType {
    fn get_props_for_method(
        self,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError>
    where
        Self: std::marker::Sized {
        todo!()
    }

    fn into_type(
        self,
        type_value: &TypeDef,
    ) -> Result<TypeValue, CompileError>
    where
        Self: std::marker::Sized {
        todo!()
    }

    fn exec_value_method<'a>(
        &'a self,
        method_token: usize,
        args: &'a [&'a TypeValue],
        res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue + 'a>, VmError> {
        todo!()
    }

    fn exec_consume_value_method(
        self,
        method_token: usize,
        args: Vec<TypeValue>,
        res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue>, VmError> {
        todo!()
    }

    fn exec_type_method<'a>(
        method_token: usize,
        args: &[&'a TypeValue],
        res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue + 'a>, VmError> {
        todo!()
    }
}

impl From<OriginType> for TypeValue {
    fn from(value: OriginType) -> Self {
        value.0.map(|t| TypeValue::Builtin(BuiltinTypeValue::OriginType(t.into()))).unwrap_or(TypeValue::Unknown)
    }
}

impl From<routecore::bgp::types::OriginType> for OriginType {
    fn from(value: routecore::bgp::types::OriginType) -> Self {
        Self(Some(value))
    }
}

impl Display for OriginType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let Some(or) = &self.0 {
            write!(f, "{}", or)
        } else {
            write!(f, "None")
        }
    }
}

//------------ NextHop type -------------------------------------------------

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct NextHop(
    pub(crate) Option<routecore::bgp::types::NextHop>,
);

//------------ Multi Exit Discriminator type --------------------------------

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct MultiExitDisc(
    pub(crate) Option<routecore::bgp::types::MultiExitDisc>,
);

//------------ Local Preference type ----------------------------------------

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct LocalPref(
    pub(crate) Option<routecore::bgp::types::LocalPref>,
);

//------------ Aggregator type ----------------------------------------------

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Aggregator(
    pub(crate) Option<routecore::bgp::message::update::Aggregator>,
);
