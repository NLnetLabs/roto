use inetnum::asn::Asn;
use string_interner::{backend::StringBackend, StringInterner};

use crate::{
    runtime::ty::{Reflect, TypeDescription, TypeRegistry},
    typechecker::{
        info::TypeInfo,
        types::{type_to_string, Primitive, Type},
    },
};
use std::{any::TypeId, fmt::Display, mem::MaybeUninit};

#[derive(Debug)]
pub enum FunctionRetrievalError {
    DoesNotExist { name: String, existing: Vec<String> },
    IncorrectNumberOfArguments { expected: usize, got: usize },
    TypeMismatch(String, TypeMismatch),
}

#[derive(Debug)]
pub struct TypeMismatch {
    rust_ty: String,
    roto_ty: String,
}

impl Display for FunctionRetrievalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FunctionRetrievalError::DoesNotExist { name, existing } => {
                writeln!(f, "The function `{name}` does not exist.")?;
                writeln!(f, "Hint: the following functions are defined:")?;
                for n in existing {
                    writeln!(f, " - {n}")?;
                }
                Ok(())
            }
            FunctionRetrievalError::IncorrectNumberOfArguments {
                expected,
                got,
            } => {
                writeln!(f, "The numer of arguments do not match")?;
                writeln!(f, "The Roto function has {expected} arguments, but the Rust function has {got}.")
            }
            FunctionRetrievalError::TypeMismatch(
                ctx,
                TypeMismatch { rust_ty, roto_ty },
            ) => {
                writeln!(
                    f,
                    "The types for {ctx} of the function do not match"
                )?;
                writeln!(f, "Expected `{roto_ty}` got `{rust_ty}`.")
            }
        }
    }
}

pub fn check_roto_type_reflect<T: Reflect>(
    registry: &mut TypeRegistry,
    type_info: &mut TypeInfo,
    identifiers: &StringInterner<StringBackend>,
    roto_ty: &Type,
) -> Result<(), TypeMismatch> {
    let rust_ty = registry.resolve::<T>().type_id;
    check_roto_type(registry, type_info, identifiers, rust_ty, roto_ty)
}

#[allow(non_snake_case)]
fn check_roto_type(
    registry: &TypeRegistry,
    type_info: &mut TypeInfo,
    identifiers: &StringInterner<StringBackend>,
    rust_ty: TypeId,
    roto_ty: &Type,
) -> Result<(), TypeMismatch> {
    // Convert this to consts when TypeId::of is const on stable
    let BOOL: TypeId = TypeId::of::<bool>();
    let U8: TypeId = TypeId::of::<u8>();
    let U16: TypeId = TypeId::of::<u16>();
    let U32: TypeId = TypeId::of::<u32>();
    let U64: TypeId = TypeId::of::<u64>();
    let I8: TypeId = TypeId::of::<i8>();
    let I16: TypeId = TypeId::of::<i16>();
    let I32: TypeId = TypeId::of::<i32>();
    let I64: TypeId = TypeId::of::<i64>();
    let UNIT: TypeId = TypeId::of::<()>();
    let ASN: TypeId = TypeId::of::<Asn>();

    let Some(rust_ty) = registry.get(rust_ty) else {
        return Err(TypeMismatch {
            rust_ty: "unknown".into(),
            roto_ty: type_to_string(identifiers, roto_ty),
        });
    };

    let error_message = TypeMismatch {
        rust_ty: rust_ty.rust_name.to_string(),
        roto_ty: type_to_string(identifiers, roto_ty),
    };

    let mut roto_ty = type_info.resolve(roto_ty);

    if let Type::IntVar(_) = roto_ty {
        roto_ty = Type::Primitive(Primitive::I32);
    }

    match rust_ty.description {
        TypeDescription::Leaf => {
            let expected_roto = match rust_ty.type_id {
                x if x == BOOL => Type::Primitive(Primitive::Bool),
                x if x == U8 => Type::Primitive(Primitive::U8),
                x if x == U16 => Type::Primitive(Primitive::U16),
                x if x == U32 => Type::Primitive(Primitive::U32),
                x if x == U64 => Type::Primitive(Primitive::U64),
                x if x == I8 => Type::Primitive(Primitive::I8),
                x if x == I16 => Type::Primitive(Primitive::I16),
                x if x == I32 => Type::Primitive(Primitive::I32),
                x if x == I64 => Type::Primitive(Primitive::I64),
                x if x == UNIT => Type::Primitive(Primitive::Unit),
                x if x == ASN => Type::Primitive(Primitive::Asn),
                _ => panic!(),
            };
            if expected_roto == roto_ty {
                Ok(())
            } else {
                Err(error_message)
            }
        }
        TypeDescription::ConstPtr(_) => todo!(),
        TypeDescription::MutPtr(_) => Ok(()), // TODO: actually check this
        TypeDescription::Verdict(rust_accept, rust_reject) => {
            let Type::Verdict(roto_accept, roto_reject) = &roto_ty else {
                return Err(error_message);
            };
            check_roto_type(
                registry,
                type_info,
                identifiers,
                rust_accept,
                roto_accept,
            )?;
            check_roto_type(
                registry,
                type_info,
                identifiers,
                rust_reject,
                roto_reject,
            )?;
            Ok(())
        }
        // We don't do options and results, we should hint towards verdict
        // when using them.
        TypeDescription::Option(_) | TypeDescription::Result(_, _) => {
            Err(error_message)
        }
    }
}

pub fn return_type_by_ref(registry: &TypeRegistry, rust_ty: TypeId) -> bool {
    let Some(rust_ty) = registry.get(rust_ty) else {
        return false;
    };

    #[allow(clippy::match_like_matches_macro)]
    match rust_ty.description {
        TypeDescription::Verdict(_, _) => true,
        _ => false,
    }
}

pub trait RotoParams {
    fn check(
        registry: &mut TypeRegistry,
        type_info: &mut TypeInfo,
        identifiers: &StringInterner<StringBackend>,
        ty: &[Type],
    ) -> Result<(), FunctionRetrievalError>;

    unsafe fn invoke<R: Reflect>(
        func_ptr: *const u8,
        params: Self,
        return_by_ref: bool,
    ) -> R;
}

macro_rules! unit {
    ($t:tt) => {
        ()
    };
}

macro_rules! params {
    ($($t:ident),*) => {
        #[allow(non_snake_case)]
        #[allow(unused_variables)]
        #[allow(unused_mut)]
        impl<$($t,)*> RotoParams for ($($t,)*)
        where
            $($t: Reflect,)*
        {
            fn check(
                registry: &mut TypeRegistry,
                type_info: &mut TypeInfo,
                identifiers: &StringInterner<StringBackend>,
                ty: &[Type]
            ) -> Result<(), FunctionRetrievalError> {
                let [$($t),*] = ty else {
                    let x: &[()] = &[$(unit!($t)),*];
                    return Err(FunctionRetrievalError::IncorrectNumberOfArguments {
                        expected: ty.len(),
                        got: x.len(),
                    });
                };

                // Little hack to return a bool even with no parameters
                let mut i = 0;
                $(
                    i += 1;
                    check_roto_type_reflect::<$t>(registry, type_info, identifiers, $t)
                        .map_err(|e| FunctionRetrievalError::TypeMismatch(format!("argument {i}"), e))?;
                )*
                Ok(())
            }

            unsafe fn invoke<R: Reflect>(func_ptr: *const u8, ($($t,)*): Self, return_by_ref: bool) -> R {
                if return_by_ref {
                    let func_ptr = unsafe {
                        std::mem::transmute::<*const u8, fn(*mut R, $($t),*) -> ()>(func_ptr)
                    };
                    let mut ret = MaybeUninit::<R>::uninit();
                    func_ptr(ret.as_mut_ptr(), $($t),*);
                    unsafe { ret.assume_init() }
                } else {
                    let func_ptr = unsafe {
                        std::mem::transmute::<*const u8, fn($($t),*) -> R>(func_ptr)
                    };
                    func_ptr($($t),*)
                }
            }
        }
    };
}

params!();
params!(A1);
params!(A1, A2);
params!(A1, A2, A3);
params!(A1, A2, A3, A4);
params!(A1, A2, A3, A4, A5);
params!(A1, A2, A3, A4, A5, A6);
params!(A1, A2, A3, A4, A5, A6, A7);
