use std::any::{Any, TypeId};
use std::mem::MaybeUninit;
use std::ops::Deref;
use std::sync::Arc;

use crate::Value;
use crate::lir::{IrValue, Memory};

/// A type that indicates that a parameter is an out ptr.
#[repr(transparent)]
pub struct OutPtr<T: Value> {
    pub ptr: *mut T::Transformed,
}

pub struct WithOutPtr;
pub struct WithoutOutPtr;

#[derive(Clone)]
pub struct FunctionDescription {
    parameter_types: Vec<TypeId>,
    return_type: TypeId,
    pointer: Arc<Box<dyn Any>>,
    trampoline: *const u8,
    ir_function: RustIrFunction,
}

// SAFETY: FunctionDescription is only not Send and Sync because of the function
// pointer, but that's fine because those are static. The only constructors for
// FunctionDescription are in this module, so we know that it's only instantiated
// with these pointers that are ok to send and sync.
unsafe impl Send for FunctionDescription {}
unsafe impl Sync for FunctionDescription {}

/// Trait implemented by functions that can be registered in a `runtime`.
#[diagnostic::on_unimplemented(
    note = "All arguments and the return type of this function implement `Value`.",
    note = "You might have forgotten to wrap one of the arguments in `Val<T>`."
)]
pub trait RegisterableFn<A, R, MaybeOutPtr>: Send + 'static {
    /// The type of a Rust function wrapping a function of this type
    type RustWrapper;

    /// The trampoline function calling this function when called from Roto.
    const TRAMPOLINE: Self::RustWrapper;

    /// The pointer to this function.
    fn ptr(self) -> Arc<Box<dyn Any>>;

    /// The `TypeId`s of the parameter types of this function.
    fn parameter_types(&self) -> Vec<TypeId>;

    /// The `TypeId` of the return type of this function.
    fn return_type() -> TypeId;

    /// The IR function that wraps this function that will be called by the LIR evaluator.
    fn ir_function(&self) -> RustIrFunction;
}

impl FunctionDescription {
    pub fn of<A, R, O, F: RegisterableFn<A, R, O>>(func: F) -> Self {
        let parameter_types = func.parameter_types();
        let return_type = F::return_type();
        let trampoline_ptr = &F::TRAMPOLINE as *const _ as *const *const u8;
        let trampoline = unsafe { *trampoline_ptr };
        let ir_function = func.ir_function();
        let pointer = func.ptr();

        Self {
            parameter_types,
            return_type,
            pointer,
            trampoline,
            ir_function,
        }
    }

    pub fn parameter_types(&self) -> &[TypeId] {
        &self.parameter_types
    }

    pub fn return_type(&self) -> TypeId {
        self.return_type
    }

    pub fn pointer(&self) -> Arc<Box<dyn Any>> {
        self.pointer.clone()
    }

    pub fn ir_function(&self) -> RustIrFunction {
        self.ir_function.clone()
    }

    pub fn trampoline(&self) -> *const u8 {
        self.trampoline
    }
}

impl PartialEq for FunctionDescription {
    fn eq(&self, other: &Self) -> bool {
        self.parameter_types == other.parameter_types
            && self.return_type == other.return_type
            && Arc::ptr_eq(&self.pointer, &other.pointer)
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

#[allow(clippy::type_complexity)]
#[derive(Clone)]
pub struct RustIrFunction(Arc<dyn Fn(&mut Memory, Vec<IrValue>)>);

impl Deref for RustIrFunction {
    type Target = Arc<dyn Fn(&mut Memory, Vec<IrValue>)>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

macro_rules! registerable_fn {
    (fn($($a:ident),*) -> $r:ident) => {
        #[allow(non_snake_case)]
        #[allow(unused_variables)]
        #[allow(unused_mut)]
        impl<$($a,)* $r, F> RegisterableFn<($($a,)*), $r, WithoutOutPtr> for F
        where
            $($a: Value,)*
            $r: Value,
            F: Fn($($a,)*) -> $r + Send + 'static,
        {
            type RustWrapper = extern "C" fn (*const Self, *mut $r::Transformed, $($a::AsParam),*) -> ();

            const TRAMPOLINE: Self::RustWrapper = {
                extern "C" fn foo<$($a: Value,)* $r: Value>(x: *const impl Fn($($a,)*) -> $r, out: *mut $r::Transformed, $($a: $a::AsParam),*) -> () {
                    let res = (unsafe { &*x })(
                        $(<$a as Value>::untransform(<$a as Value>::to_value($a)),)*
                    );
                    let res_transformed  = <$r as Value>::transform(res);
                    unsafe { std::ptr::write(out, res_transformed) };
                }
                foo
            };

            fn ptr(self) -> Arc<Box<dyn Any>> {
                Arc::new(Box::new(self))
            }

            fn parameter_types(&self) -> Vec<TypeId> {
                vec![$($a::resolve().type_id,)*]
            }

            fn return_type() -> TypeId {
                $r::resolve().type_id
            }

            fn ir_function(&self) -> RustIrFunction {
                let f = self as *const _;
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
                        let Ok($a) = <$a as Value>::from_ir_value(mem, $a.clone()) else {
                            panic!("Type of argument is not correct: {}", $a)
                        };
                    )*
                    let mut uninit_ret = MaybeUninit::<<$r as Value>::Transformed>::uninit();
                    Self::TRAMPOLINE(
                        f,
                        $r as *mut <$r as Value>::Transformed,
                        $($a),*
                    );
                };
                RustIrFunction(Arc::new(f))
            }
        }
    }
}

registerable_fn!(fn() -> R);
registerable_fn!(fn(A1) -> R);
registerable_fn!(fn(A1, A2) -> R);
registerable_fn!(fn(A1, A2, A3) -> R);
registerable_fn!(fn(A1, A2, A3, A4) -> R);
registerable_fn!(fn(A1, A2, A3, A4, A5) -> R);
registerable_fn!(fn(A1, A2, A3, A4, A5, A6) -> R);
registerable_fn!(fn(A1, A2, A3, A4, A5, A6, A7) -> R);

macro_rules! registerable_fn_out_ptr {
    (fn($($a:ident),*) -> $r:ident) => {
        #[allow(non_snake_case)]
        #[allow(unused_variables)]
        #[allow(unused_mut)]
        impl<$($a,)* $r, F> RegisterableFn<($($a,)*), $r, WithOutPtr> for F
        where
            $($a: Value,)*
            $r: Value,
            F: Fn(OutPtr<$r>, $($a,)*) + Send + 'static,
        {
            type RustWrapper = extern "C" fn (*const Self, *mut $r::Transformed, $($a::AsParam),*) -> ();

            const TRAMPOLINE: Self::RustWrapper = {
                extern "C" fn foo<$($a: Value,)* $r: Value>(x: *const impl Fn(OutPtr<$r>, $($a,)*), out: *mut $r::Transformed, $($a: $a::AsParam),*) -> () {
                    (unsafe { &*x })(
                        OutPtr { ptr: out },
                        $(<$a as Value>::untransform(<$a as Value>::to_value($a)),)*
                    );
                }
                foo
            };

            fn ptr(self) -> Arc<Box<dyn Any>> {
                Arc::new(Box::new(self))
            }

            fn parameter_types(&self) -> Vec<TypeId> {
                vec![$($a::resolve().type_id,)*]
            }

            fn return_type() -> TypeId {
                $r::resolve().type_id
            }

            fn ir_function(&self) -> RustIrFunction {
                let f = self as *const _;
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
                        let Ok($a) = <$a as Value>::from_ir_value(mem, $a.clone()) else {
                            panic!("Type of argument is not correct: {}", $a)
                        };
                    )*
                    let mut uninit_ret = MaybeUninit::<<$r as Value>::Transformed>::uninit();
                    Self::TRAMPOLINE(
                        f,
                        $r as *mut <$r as Value>::Transformed,
                        $($a),*
                    );
                };
                RustIrFunction(Arc::new(f))
            }
        }
    }
}

registerable_fn_out_ptr!(fn() -> R);
registerable_fn_out_ptr!(fn(A1) -> R);
registerable_fn_out_ptr!(fn(A1, A2) -> R);
registerable_fn_out_ptr!(fn(A1, A2, A3) -> R);
registerable_fn_out_ptr!(fn(A1, A2, A3, A4) -> R);
registerable_fn_out_ptr!(fn(A1, A2, A3, A4, A5) -> R);
registerable_fn_out_ptr!(fn(A1, A2, A3, A4, A5, A6) -> R);
registerable_fn_out_ptr!(fn(A1, A2, A3, A4, A5, A6, A7) -> R);
