use std::any::TypeId;

use crate::Runtime;

#[derive(Debug)]
pub enum Param {
    Val(TypeId),
    ConstPtr(TypeId),
    MutPtr(TypeId),
}

#[derive(Debug)]
pub struct ResolvedFunctionDescription {
    pub parameter_types: Vec<Param>,
    pub return_type: Option<Param>,
    pub pointer: *const u8,
}

pub struct FunctionDescription {
    parameter_types: Vec<(TypeId, &'static str)>,
    return_type: (TypeId, &'static str),
    pointer: *const u8,
}

impl FunctionDescription {
    pub fn resolve(
        self,
        rt: &Runtime,
    ) -> Option<ResolvedFunctionDescription> {
        let parameter_types = self
            .parameter_types
            .iter()
            .map(|ty| rt.find_type(ty.0))
            .collect::<Option<Vec<_>>>()?;

        let return_type =
            if self.return_type.0 == std::any::TypeId::of::<()>() {
                None
            } else {
                Some(rt.find_type(self.return_type.0)?)
            };

        Some(ResolvedFunctionDescription {
            parameter_types,
            return_type,
            pointer: self.pointer,
        })
    }
}

pub trait Func<A, R> {
    fn parameter_types() -> Vec<(TypeId, &'static str)>;
    fn return_type() -> (TypeId, &'static str);

    fn to_function_description(&self) -> FunctionDescription {
        FunctionDescription {
            parameter_types: Self::parameter_types(),
            return_type: Self::return_type(),
            pointer: &self as *const _ as *const u8,
        }
    }
}

macro_rules! func_impl {
    ($($arg:ident),*) => {
        impl<$($arg: 'static,)* Ret: 'static> Func<($($arg,)*), Ret> for extern "C" fn($($arg),*) -> Ret {
            fn parameter_types() -> Vec<(TypeId, &'static str)> {
                vec![$((
                    std::any::TypeId::of::<$arg>(),
                    std::any::type_name::<$arg>(),
                )),*]
            }

            fn return_type() -> (TypeId, &'static str) {
                (
                    std::any::TypeId::of::<Ret>(),
                    std::any::type_name::<Ret>(),
                )
            }
        }
    };
}

func_impl!();
func_impl!(A);
func_impl!(A, B);
func_impl!(A, B, C);
func_impl!(A, B, C, D);
func_impl!(A, B, C, D, E);
func_impl!(A, B, C, D, E, F);
func_impl!(A, B, C, D, E, F, G);
func_impl!(A, B, C, D, E, F, G, H);
func_impl!(A, B, C, D, E, F, G, H, I);
func_impl!(A, B, C, D, E, F, G, H, I, J);
func_impl!(A, B, C, D, E, F, G, H, I, J, K);
func_impl!(A, B, C, D, E, F, G, H, I, J, K, L);
