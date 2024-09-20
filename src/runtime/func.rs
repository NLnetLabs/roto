use std::{any::TypeId, sync::Arc};

use crate::{lower::value::ReturnValue, IrValue, Runtime};

use super::ty::{Reflect, TypeRegistry};

#[derive(Clone)]
pub struct FunctionDescription {
    parameter_types: Vec<TypeId>,
    return_type: TypeId,
    pointer: *const u8,
    wrapped: Arc<dyn Fn(Vec<IrValue>) -> Option<IrValue>>,
}

// SAFETY: FunctionDescription is only not Send and Sync because of the function
// pointer, but that's fine because those are static. The only constructors for
// FunctionDescription are in this module, so we know that it's only instantiated
// with these pointers that are ok to send and sync.
unsafe impl Send for FunctionDescription {}
unsafe impl Sync for FunctionDescription {}

impl FunctionDescription {
    pub fn parameter_types(&self) -> &[TypeId] {
        &self.parameter_types
    }

    pub fn return_type(&self) -> TypeId {
        self.return_type
    }

    pub fn pointer(&self) -> *const u8 {
        self.pointer
    }

    pub fn wrapped(&self) -> Arc<dyn Fn(Vec<IrValue>) -> Option<IrValue>> {
        self.wrapped.clone()
    }
}

impl PartialEq for FunctionDescription {
    fn eq(&self, other: &Self) -> bool {
        self.parameter_types == other.parameter_types
            && self.return_type == other.return_type
            && self.pointer == other.pointer
    }
}

impl Eq for FunctionDescription {}

impl std::fmt::Debug for FunctionDescription {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("FunctionDescription")
            .field("parameter_types", &self.parameter_types)
            .field("return_type", &self.return_type)
            .field("pointer", &self.pointer)
            .field("wrapped", &"<function>")
            .finish()
    }
}

pub trait Func<A, R>: Sized {
    fn parameter_types(reg: &mut TypeRegistry) -> Vec<TypeId>;
    fn return_type(reg: &mut TypeRegistry) -> TypeId;
    fn ptr(&self) -> *const u8;
    fn wrapped(self) -> Arc<dyn Fn(Vec<IrValue>) -> Option<IrValue>>;

    fn to_function_description(
        self,
        rt: &mut Runtime,
    ) -> Result<FunctionDescription, String> {
        let parameter_types = Self::parameter_types(&mut rt.type_registry);
        let return_type = Self::return_type(&mut rt.type_registry);
        let pointer = self.ptr();
        let wrapped = self.wrapped();

        Ok(FunctionDescription {
            parameter_types,
            return_type,
            pointer,
            wrapped,
        })
    }
}

macro_rules! func_impl {
    ($($arg:ident),*) => {
        impl<$($arg,)* Ret> Func<($($arg,)*), Ret> for extern "C" fn($($arg),*) -> Ret
        where
            $(
                $arg: Reflect + for<'a> TryFrom<&'a IrValue> + 'static,
            )*
            Ret: Reflect + Into<ReturnValue> + 'static,
        {
            #[allow(unused_variables)]
            fn parameter_types(reg: &mut TypeRegistry) -> Vec<TypeId> {
                vec![$(reg.resolve::<$arg>().type_id),*]
            }

            fn return_type(reg: &mut TypeRegistry) -> TypeId {
                reg.resolve::<Ret>().type_id
            }

            fn ptr(&self) -> *const u8 {
                // Make sure to deref before casting to get the function
                // pointer and not the pointer to the function pointer.
                (*self) as *const u8
            }

            fn wrapped(self) -> Arc<dyn Fn(Vec<IrValue>) -> Option<IrValue>> {
                // We reuse the type names as variable names, so they are
                // uppercase, but that's the easiest way to do this.
                #[allow(non_snake_case)]
                let f = move |args: Vec<IrValue>| {
                    let [$($arg),*]: &[IrValue] = &args else {
                        panic!("Number of arguments is not correct")
                    };
                    $(
                        let Ok($arg) = $arg.try_into() else {
                            panic!("Type of argument is not correct")
                        };
                    )*
                    let ret: ReturnValue = self($($arg),*).into();
                    ret.0
                };
                Arc::new(f)
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
