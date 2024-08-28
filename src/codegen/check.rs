use crate::{
    runtime::ty::{Reflect, TypeDescription, TypeRegistry},
    typechecker::{
        info::TypeInfo,
        types::{Primitive, Type},
    },
};
use std::{any::TypeId, mem::MaybeUninit};

// /// A type that is compatible with Roto
// ///
// /// Such a type needs to have a corresponding roto representation.
// /// It also needs to have a size and be convertible into a slice(?).
// ///
// /// We need to do several things with this:
// ///  - Do runtime type checking before handing out a roto function.
// ///  - Do runtime type checking
// pub trait RotoType {
//     const RETURN_BY_REF: bool;

//     fn check(ty: Type) -> bool;
// }

// impl RotoType for bool {
//     const RETURN_BY_REF: bool = false;

//     fn check(ty: Type) -> bool {
//         ty == Type::Primitive(Primitive::Bool)
//     }
// }

// impl RotoType for i8 {
//     const RETURN_BY_REF: bool = false;

//     fn check(ty: Type) -> bool {
//         ty == Type::Primitive(Primitive::I8)
//     }
// }

// impl RotoType for u8 {
//     const RETURN_BY_REF: bool = false;

//     fn check(ty: Type) -> bool {
//         ty == Type::Primitive(Primitive::U8)
//     }
// }

// impl RotoType for i16 {
//     const RETURN_BY_REF: bool = false;

//     fn check(ty: Type) -> bool {
//         ty == Type::Primitive(Primitive::I16)
//     }
// }

// impl RotoType for u16 {
//     const RETURN_BY_REF: bool = false;

//     fn check(ty: Type) -> bool {
//         ty == Type::Primitive(Primitive::U16)
//     }
// }

// impl RotoType for i32 {
//     const RETURN_BY_REF: bool = false;

//     fn check(ty: Type) -> bool {
//         ty == Type::Primitive(Primitive::I32)
//     }
// }

// impl RotoType for u32 {
//     const RETURN_BY_REF: bool = false;

//     fn check(ty: Type) -> bool {
//         ty == Type::Primitive(Primitive::U32)
//     }
// }

// impl RotoType for () {
//     const RETURN_BY_REF: bool = false;

//     fn check(ty: Type) -> bool {
//         ty == Type::Primitive(Primitive::Unit)
//     }
// }

// impl<T> RotoType for *mut T {
//     const RETURN_BY_REF: bool = false;

//     fn check(ty: Type) -> bool {
//         ty == Some(IrType::Pointer) || ty == Some(IrType::ExtPointer)
//     }
// }

// impl<A, R> RotoType for Verdict<A, R> {
//     const RETURN_BY_REF: bool = true;

//     fn check(ty: Type) -> bool {
//         ty == Some(IrType::Pointer) || ty == Some(Type::ExtPointer)
//     }
// }

pub fn check_roto_type_reflect<T: Reflect>(
    registry: &mut TypeRegistry,
    type_info: &mut TypeInfo,
    roto_ty: &Type,
) -> bool {
    let rust_ty = registry.resolve::<T>().type_id;
    check_roto_type(registry, type_info, rust_ty, roto_ty)
}

#[allow(non_snake_case)]
fn check_roto_type(
    registry: &TypeRegistry,
    type_info: &mut TypeInfo,
    rust_ty: TypeId,
    roto_ty: &Type,
) -> bool {
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

    let Some(rust_ty) = registry.get(rust_ty) else {
        return false;
    };

    let roto_ty = type_info.resolve(roto_ty);

    match rust_ty.description {
        TypeDescription::Leaf => match rust_ty.type_id {
            x if x == BOOL => roto_ty == Type::Primitive(Primitive::Bool),
            x if x == U8 => roto_ty == Type::Primitive(Primitive::U8),
            x if x == U16 => roto_ty == Type::Primitive(Primitive::U16),
            x if x == U32 => roto_ty == Type::Primitive(Primitive::U32),
            x if x == U64 => roto_ty == Type::Primitive(Primitive::U64),
            x if x == I8 => roto_ty == Type::Primitive(Primitive::I8),
            x if x == I16 => roto_ty == Type::Primitive(Primitive::I16),
            x if x == I32 => roto_ty == Type::Primitive(Primitive::I32),
            x if x == I64 => roto_ty == Type::Primitive(Primitive::I64),
            x if x == UNIT => roto_ty == Type::Primitive(Primitive::Unit),
            _ => panic!(),
        },
        TypeDescription::ConstPtr(_) => todo!(),
        TypeDescription::MutPtr(_) => true, // TODO: actually check this
        TypeDescription::Verdict(rust_accept, rust_reject) => {
            let Type::Verdict(roto_accept, roto_reject) = &roto_ty else {
                return false;
            };
            check_roto_type(registry, type_info, rust_accept, roto_accept)
                && check_roto_type(registry, type_info, rust_reject, roto_reject)
        }
        // We don't do options and results, we should hint towards verdict
        // when using them.
        TypeDescription::Option(_) => false,
        TypeDescription::Result(_, _) => false,
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
        ty: &[Type],
    ) -> bool;

    unsafe fn invoke<R: Reflect>(
        func_ptr: *const u8,
        params: Self,
        return_by_ref: bool,
    ) -> R;
}

macro_rules! params {
    ($($t:ident),*) => {
        #[allow(non_snake_case)]
        #[allow(unused_variables)]
        impl<$($t,)*> RotoParams for ($($t,)*)
        where
            $($t: Reflect,)*
        {
            fn check(registry: &mut TypeRegistry, type_info: &mut TypeInfo, ty: &[Type]) -> bool {
                let [$($t),*] = ty else {
                    return false;
                };
                // Little hack to return a bool even with no parameters
                true $(&& check_roto_type_reflect::<$t>(registry, type_info, $t))*
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
