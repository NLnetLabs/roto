use inetnum::{addr::Prefix, asn::Asn};

use crate::{
    runtime::ty::{
        Reflect, TypeDescription, TypeRegistry, GLOBAL_TYPE_REGISTRY,
    },
    typechecker::{
        info::TypeInfo,
        scope::{ResolvedName, ScopeRef},
        scoped_display::TypeDisplay,
        types::{Type, TypeDefinition},
    },
    IrValue, Memory,
};
use std::{
    any::TypeId, fmt::Display, mem::MaybeUninit, net::IpAddr, ops::Deref,
    sync::Arc,
};

#[derive(Debug)]
pub enum FunctionRetrievalError {
    DoesNotExist { name: String, existing: Vec<String> },
    IncorrectNumberOfArguments { expected: usize, got: usize },
    TypeMismatch(String, TypeMismatch),
}

#[derive(Debug)]
pub struct TypeMismatch {
    pub rust_ty: String,
    pub roto_ty: String,
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
                writeln!(f, "The number of arguments do not match")?;
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
    type_info: &mut TypeInfo,
    roto_ty: &Type,
) -> Result<(), TypeMismatch> {
    let mut registry = GLOBAL_TYPE_REGISTRY.lock().unwrap();
    let rust_ty = registry.resolve::<T>().type_id;
    check_roto_type(&registry, type_info, rust_ty, roto_ty)
}

#[allow(non_snake_case)]
fn check_roto_type(
    registry: &TypeRegistry,
    type_info: &mut TypeInfo,
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
    let F32: TypeId = TypeId::of::<f32>();
    let F64: TypeId = TypeId::of::<f64>();
    let UNIT: TypeId = TypeId::of::<()>();
    let ASN: TypeId = TypeId::of::<Asn>();
    let IPADDR: TypeId = TypeId::of::<IpAddr>();
    let PREFIX: TypeId = TypeId::of::<Prefix>();
    let STRING: TypeId = TypeId::of::<Arc<str>>();

    let Some(rust_ty) = registry.get(rust_ty) else {
        return Err(TypeMismatch {
            rust_ty: "unknown".into(),
            roto_ty: roto_ty.display(type_info).to_string(),
        });
    };

    let error_message = TypeMismatch {
        rust_ty: rust_ty.rust_name.to_string(),
        roto_ty: roto_ty.display(type_info).to_string(),
    };

    let mut roto_ty = type_info.resolve(roto_ty);

    if let Type::IntVar(_) = roto_ty {
        roto_ty = Type::named("i32", Vec::new());
    }

    if let Type::FloatVar(_) = roto_ty {
        roto_ty = Type::named("f64", Vec::new());
    }

    match rust_ty.description {
        TypeDescription::Leaf => {
            let expected_name = match rust_ty.type_id {
                x if x == BOOL => "bool",
                x if x == U8 => "u8",
                x if x == U16 => "u16",
                x if x == U32 => "u32",
                x if x == U64 => "u64",
                x if x == I8 => "i8",
                x if x == I16 => "i16",
                x if x == I32 => "i32",
                x if x == I64 => "i64",
                x if x == F32 => "f32",
                x if x == F64 => "f64",
                x if x == UNIT => "Unit",
                x if x == ASN => "Asn",
                x if x == IPADDR => "IpAddr",
                x if x == PREFIX => "Prefix",
                x if x == STRING => "String",
                _ => panic!(),
            };
            let expected_roto = Type::named(expected_name, Vec::new());
            if expected_roto == roto_ty {
                Ok(())
            } else {
                Err(error_message)
            }
        }
        TypeDescription::Val(ty) => {
            let Type::Name(type_name) = roto_ty else {
                return Err(error_message);
            };

            let TypeDefinition::Runtime(_, id) =
                type_info.resolve_type_name(&type_name)
            else {
                return Err(error_message);
            };

            if ty != id {
                return Err(error_message);
            }

            Ok(())
        }
        TypeDescription::Verdict(rust_accept, rust_reject) => {
            let Type::Name(type_name) = &roto_ty else {
                return Err(error_message);
            };

            if type_name.name
                != (ResolvedName {
                    scope: ScopeRef::GLOBAL,
                    ident: "Verdict".into(),
                })
            {
                return Err(error_message);
            }

            let [roto_accept, roto_reject] = &type_name.arguments[..] else {
                return Err(error_message);
            };

            check_roto_type(registry, type_info, rust_accept, roto_accept)?;
            check_roto_type(registry, type_info, rust_reject, roto_reject)?;
            Ok(())
        }
        TypeDescription::Option(rust_ty) => {
            let Type::Name(type_name) = &roto_ty else {
                return Err(error_message);
            };

            if type_name.name
                != (ResolvedName {
                    scope: ScopeRef::GLOBAL,
                    ident: "Optional".into(),
                })
            {
                return Err(error_message);
            }

            let [roto_ty] = &type_name.arguments[..] else {
                return Err(error_message);
            };
            check_roto_type(registry, type_info, rust_ty, roto_ty)
        }
    }
}

/// Parameters of a Roto function
///
/// This trait allows for checking the types against Roto types and converting
/// the values into values appropriate for Roto.
///
/// The `invoke` method can (unsafely) invoke a pointer as if it were a function
/// with these parameters.
///
/// This trait is implemented on function pointers with several numbers of parameters.
pub trait RotoFunc {
    /// Argument types of this function
    type Args;

    /// Return type of this function
    type Return: Reflect;

    /// Type of a Roto function with this type using a return pointer
    type RotoWithReturnPointer;

    /// Type of a Roto function with this type returning directly
    type RotoWithoutReturnPointer;

    /// The type of a Rust function wrapping a function of this type
    type RustWrapper;

    /// Check whether these parameters match a parameter list from Roto.
    fn check_args(
        type_info: &mut TypeInfo,
        ty: &[Type],
    ) -> Result<(), FunctionRetrievalError>;

    /// Call a function pointer as if it were a function with these parameters.
    ///
    /// This is _extremely_ unsafe, do not pass this arbitrary pointers and
    /// always call `RotoParams::check` before calling this function. Don't
    /// forget to also check the return type.
    ///
    /// A [`TypedFunc`](super::TypedFunc) is a safe abstraction around this
    /// function.
    unsafe fn invoke<Ctx: 'static>(
        ctx: &mut Ctx,
        args: Self::Args,
        func_ptr: *const u8,
        return_by_ref: bool,
    ) -> Self::Return;

    fn ptr(w: &Self::RustWrapper) -> *const u8;
    fn parameter_types(type_registry: &mut TypeRegistry) -> Vec<TypeId>;
    fn return_type(type_registry: &mut TypeRegistry) -> TypeId;

    fn ir_function(f: &Self::RustWrapper) -> RustIrFunction;
}

#[allow(clippy::type_complexity)]
#[derive(Clone)]
pub struct RustIrFunction(Arc<dyn Fn(&mut Memory, Vec<IrValue>)>);

impl Deref for RustIrFunction {
    type Target = Arc<dyn Fn(&mut Memory, Vec<IrValue>)>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

/// Little helper macro to create a unit
macro_rules! unit {
    ($t:tt) => {
        ()
    };
}

/// Implement the [`RotoParams`] trait for a tuple with some type parameters.
macro_rules! func {
    (fn($($a:ident),*) -> $r:ident) => {
        #[allow(non_snake_case)]
        #[allow(unused_variables)]
        #[allow(unused_mut)]
        impl<$($a,)* $r> RotoFunc for fn($($a,)*) -> $r
        where $($a: Reflect,)* $r: Reflect {
            type Args = ($($a,)*);
            type Return = $r;

            type RotoWithReturnPointer = extern "C" fn(*mut $r::Transformed, *mut (), $($a::AsParam),*) -> ();
            type RotoWithoutReturnPointer = extern "C" fn(*mut (), $($a::AsParam,)*) -> $r::Transformed;
            type RustWrapper = extern "C" fn (*mut $r::Transformed, $($a::AsParam),*) -> ();

            fn check_args(
                type_info: &mut TypeInfo,
                ty: &[Type]
            ) -> Result<(), FunctionRetrievalError> {
                let [$($a),*] = ty else {
                    let x: &[()] = &[$(unit!($a)),*];
                    return Err(FunctionRetrievalError::IncorrectNumberOfArguments {
                        expected: ty.len(),
                        got: x.len(),
                    });
                };

                let mut i = 0;
                $(
                    i += 1;
                    check_roto_type_reflect::<$a>(type_info, $a)
                        .map_err(|e| FunctionRetrievalError::TypeMismatch(format!("argument {i}"), e))?;
                )*
                Ok(())
            }

            fn ptr(w: &Self::RustWrapper) -> *const u8 {
                (*w) as *const u8
            }

            unsafe fn invoke<Ctx: 'static>(ctx: &mut Ctx, args: Self::Args, func_ptr: *const u8, return_by_ref: bool) -> R {
                let ($($a,)*) = args;
                let mut transformed = ($(<$a as Reflect>::transform($a),)*);
                let ($($a,)*) = &mut transformed;
                let ($($a,)*) = ($(<$a as Reflect>::as_param($a),)*);

                // We forget values that we pass into Roto. The script is responsible
                // for cleaning them op. Forgetting copy types does nothing, but that's
                // fine.
                #[allow(forgetting_copy_types)]
                std::mem::forget(transformed);

                if return_by_ref {
                    let func_ptr = unsafe {
                        std::mem::transmute::<*const u8, Self::RotoWithReturnPointer>(func_ptr)
                    };
                    let mut ret = MaybeUninit::<<$r as Reflect>::Transformed>::uninit();
                    func_ptr(ret.as_mut_ptr(), ctx as *mut Ctx as *mut (), $($a),*);
                    let transformed_ret = unsafe { ret.assume_init() };
                    let ret: Self::Return = Self::Return::untransform(transformed_ret);
                    ret
                } else {
                    let func_ptr = unsafe {
                        std::mem::transmute::<*const u8, Self::RotoWithoutReturnPointer>(func_ptr)
                    };
                    let ret = func_ptr(ctx as *mut Ctx as *mut (), $($a),*);
                    <R as Reflect>::untransform(ret)
                }
            }

            fn parameter_types(type_registry: &mut TypeRegistry) -> Vec<TypeId> {
                vec![$($a::resolve(type_registry).type_id,)*]
            }

            fn return_type(type_registry: &mut TypeRegistry) -> TypeId {
                $r::resolve(type_registry).type_id
            }

            fn ir_function(f: &Self::RustWrapper) -> RustIrFunction {
                let f = *f;
                // We reuse the type names as variable names, so they are
                // uppercase, but that's the easiest way to do this.
                #[allow(non_snake_case)]
                let f = move |mem: &mut Memory, args: Vec<IrValue>| {
                    let [$r, $($a),*]: &[IrValue] = &args else {
                        panic!("Number of arguments is not correct")
                    };

                    let &IrValue::Pointer($r) = $r else {
                        panic!("Out pointer is not a pointer")
                    };
                    let $r = mem.get($r);

                    $(
                        let Ok($a) = <$a as Reflect>::from_ir_value(mem, $a.clone()) else {
                            panic!("Type of argument is not correct: {}", $a)
                        };
                    )*
                    let mut uninit_ret = MaybeUninit::<<$r as Reflect>::Transformed>::uninit();
                    f($r as *mut <$r as Reflect>::Transformed, $($a),*);
                };
                RustIrFunction(Arc::new(f))
            }
        }
    };
}

func!(fn() -> R);
func!(fn(A1) -> R);
func!(fn(A1, A2) -> R);
func!(fn(A1, A2, A3) -> R);
func!(fn(A1, A2, A3, A4) -> R);
func!(fn(A1, A2, A3, A4, A5) -> R);
func!(fn(A1, A2, A3, A4, A5, A6) -> R);
func!(fn(A1, A2, A3, A4, A5, A6, A7) -> R);
