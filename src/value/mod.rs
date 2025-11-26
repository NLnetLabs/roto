//! Type information on Rust types
//!
//! The [`TypeRegistry`] holds information on Rust types that we have seen,
//! so we can match them to Roto types and use the names in error messages.
//!
//! The registry should initially hold all registered types and primitives.
//! On demand, we add more complex types via the [`Reflect`] trait, which
//! is implemented for types that have a Roto equivalent. This is necessary
//! for mapping a complex Rust type to Roto types.

use std::{
    any::{TypeId, type_name},
    collections::HashMap,
    net::IpAddr,
    sync::{Arc, LazyLock, Mutex},
};

use inetnum::{addr::Prefix, asn::Asn};
use sealed::sealed;

use crate::{
    lir::{IrValue, Memory},
    runtime::layout::Layout,
};

use option::RotoOption;
use val::Val;
use verdict::Verdict;

pub mod option;
pub mod val;
pub mod verdict;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TypeDescription {
    /// Some type that we don't know how to decompose
    Leaf,

    /// `Option<T>`
    Option(TypeId),

    /// `Verdict<A, R>`
    Verdict(TypeId, TypeId),
    /// `Val<T>`
    Val(TypeId),
}

#[derive(Clone)]
pub struct Ty {
    /// The name of the type in Rust, mostly for diagnostic purposes
    pub rust_name: &'static str,

    /// The memory alignment of the type in bytes
    pub layout: Layout,

    /// The [`TypeId`] corresponding to this type
    pub type_id: TypeId,

    /// Description of the structure of the type
    pub description: TypeDescription,
}

impl Ty {
    fn new<T: 'static>(description: TypeDescription) -> Self {
        Self {
            rust_name: type_name::<T>(),
            layout: Layout::of::<T>(),
            type_id: TypeId::of::<T>(),
            description,
        }
    }
}

static GLOBAL_TYPE_REGISTRY: LazyLock<Mutex<TypeRegistry>> =
    LazyLock::new(|| Mutex::new(TypeRegistry::default()));

/// A map from [`TypeId`] to a [`Ty`], which is a description of the type
#[derive(Clone, Default)]
pub struct TypeRegistry {
    map: HashMap<TypeId, &'static Ty>,
}

impl TypeRegistry {
    pub fn store<T: 'static>(description: TypeDescription) -> Ty {
        let ty = Ty::new::<T>(description);
        GLOBAL_TYPE_REGISTRY
            .lock()
            .unwrap()
            .map
            .entry(ty.type_id)
            .or_insert_with(|| {
                // Leaking is fine because we only store each type once in the
                // global TypeRegistry.
                Box::leak(Box::new(ty))
            })
            .clone()
    }

    pub fn get(id: TypeId) -> Option<&'static Ty> {
        let registry = GLOBAL_TYPE_REGISTRY.lock().unwrap();
        registry.map.get(&id).map(|v| &**v)
    }

    /// Register a type implementing [`Reflect`]
    pub fn resolve<T: Reflect>() -> Ty {
        T::resolve()
    }
}

/// A type that can be passed to Roto.
///
/// This trait is _sealed_, meaning that it cannot be implemented by downstream
/// crates, instead these types can be wrapped in [`Val`].
///
/// The `Reflect::Transformed` type represents the type that this type will be
/// converted into before being passed to Roto. For example, `Option` will be
/// converted into a type with the same variants, but a fixed layout.
///
/// The `Reflect::AsParam` then specifies how this value is passed to a Roto
/// function. Most primitives are simply passed by value, but many other types
/// are passed by `*mut Reflect::Transformed`.
#[sealed]
pub trait Reflect: Sized + 'static {
    /// Intermediate type that can be used to convert a type to a Roto type
    type Transformed: Clone;

    /// The type that this type should be converted into when passed to Roto
    type AsParam: Param<Self::Transformed>;

    /// Transform this value into a value that Roto understands
    fn transform(self) -> Self::Transformed;

    /// Transform this a Roto value back into this type
    fn untransform(transformed: Self::Transformed) -> Self;

    fn as_param(transformed: &mut Self::Transformed) -> Self::AsParam {
        Self::AsParam::as_param(transformed)
    }

    fn to_value(param: Self::AsParam) -> Self::Transformed {
        Self::AsParam::to_value(param)
    }

    /// Attempt to convert an IR value into `Self`
    fn from_ir_value(
        mem: &mut Memory,
        value: IrValue,
    ) -> Result<Self::AsParam, IrValueDoesNotMatchType> {
        Self::AsParam::from_ir_value(mem, value)
    }

    /// Put information about this type into the global type registry
    ///
    /// The information is also returned for direct use.
    fn resolve() -> Ty;

    fn name() -> &'static str {
        std::any::type_name::<Self>()
    }
}

pub struct IrValueDoesNotMatchType;

pub trait Param<T>: Sized {
    fn as_param(value: &mut T) -> Self;

    fn to_value(self) -> T;

    fn from_ir_value(
        mem: &mut Memory,
        value: IrValue,
    ) -> Result<Self, IrValueDoesNotMatchType>;
}

impl<T> Param<T> for *mut T {
    fn as_param(transformed: &mut T) -> Self {
        transformed as *mut T
    }

    fn to_value(self) -> T {
        unsafe { std::ptr::read(self) }
    }

    fn from_ir_value(
        mem: &mut Memory,
        value: IrValue,
    ) -> Result<Self, IrValueDoesNotMatchType> {
        let IrValue::Pointer(p) = value else {
            return Err(IrValueDoesNotMatchType);
        };
        Ok(mem.read_slice(p, std::mem::size_of::<T>()).as_ptr() as *mut T)
    }
}

#[sealed]
impl<A: Reflect, R: Reflect> Reflect for Verdict<A, R>
where
    A::Transformed: Clone,
    R::Transformed: Clone,
{
    type Transformed = Verdict<A::Transformed, R::Transformed>;
    type AsParam = *mut Self::Transformed;

    fn transform(self) -> Self::Transformed {
        match self {
            Self::Accept(a) => Verdict::Accept(a.transform()),
            Self::Reject(r) => Verdict::Reject(r.transform()),
        }
    }

    fn untransform(transformed: Self::Transformed) -> Self {
        match transformed {
            Verdict::Accept(a) => Self::Accept(A::untransform(a)),
            Verdict::Reject(r) => Self::Reject(R::untransform(r)),
        }
    }

    fn resolve() -> Ty {
        let t = A::resolve().type_id;
        let e = R::resolve().type_id;

        let desc = TypeDescription::Verdict(t, e);
        TypeRegistry::store::<Self>(desc)
    }
}

#[sealed]
impl<T: Reflect> Reflect for Option<T> {
    type Transformed = RotoOption<T::Transformed>;
    type AsParam = *mut Self::Transformed;

    fn transform(self) -> Self::Transformed {
        match self {
            Some(t) => RotoOption::Some(t.transform()),
            None => RotoOption::None,
        }
    }

    fn untransform(transformed: Self::Transformed) -> Self {
        match transformed {
            RotoOption::Some(t) => Some(T::untransform(t)),
            RotoOption::None => None,
        }
    }

    fn resolve() -> Ty {
        let t = T::resolve().type_id;

        let desc = TypeDescription::Option(t);
        TypeRegistry::store::<Self>(desc)
    }
}

impl<T> Param<Val<T>> for *mut T {
    fn as_param(value: &mut Val<T>) -> Self {
        &mut value.0 as *mut _
    }

    fn to_value(self) -> Val<T> {
        Val(unsafe { std::ptr::read(self) })
    }

    fn from_ir_value(
        mem: &mut Memory,
        value: IrValue,
    ) -> Result<Self, IrValueDoesNotMatchType> {
        let IrValue::Pointer(p) = value else {
            return Err(IrValueDoesNotMatchType);
        };
        Ok(mem.read_slice(p, std::mem::size_of::<T>()).as_ptr() as *mut T)
    }
}

#[sealed]
impl<T: 'static + Clone> Reflect for Val<T> {
    type Transformed = Self;
    type AsParam = *mut T;

    fn transform(self) -> Self::Transformed {
        self
    }

    fn untransform(transformed: Self::Transformed) -> Self {
        transformed
    }

    fn resolve() -> Ty {
        let t = TypeId::of::<T>();

        let desc = TypeDescription::Val(t);
        TypeRegistry::store::<Self>(desc)
    }

    fn name() -> &'static str {
        std::any::type_name::<T>()
    }
}

#[sealed]
impl Reflect for IpAddr {
    type Transformed = Self;
    type AsParam = *mut Self;

    fn transform(self) -> Self::Transformed {
        self
    }

    fn untransform(transformed: Self::Transformed) -> Self {
        transformed
    }

    fn resolve() -> Ty {
        TypeRegistry::store::<Self>(TypeDescription::Leaf)
    }
}

#[sealed]
impl Reflect for Prefix {
    type Transformed = Self;
    type AsParam = *mut Self;

    fn transform(self) -> Self::Transformed {
        self
    }

    fn untransform(transformed: Self::Transformed) -> Self {
        transformed
    }

    fn resolve() -> Ty {
        TypeRegistry::store::<Self>(TypeDescription::Leaf)
    }
}

#[sealed]
impl Reflect for Arc<str> {
    type Transformed = Self;
    type AsParam = *mut Self;

    fn transform(self) -> Self::Transformed {
        self
    }

    fn untransform(transformed: Self::Transformed) -> Self {
        transformed
    }

    fn resolve() -> Ty {
        TypeRegistry::store::<Self>(TypeDescription::Leaf)
    }
}

impl TryFrom<&IrValue> for () {
    type Error = ();

    fn try_from(_: &IrValue) -> Result<Self, Self::Error> {
        Err(())
    }
}

impl Param<()> for () {
    fn as_param(_: &mut ()) -> Self {}

    fn to_value(self) {}

    fn from_ir_value(
        _mem: &mut Memory,
        _value: IrValue,
    ) -> Result<Self, IrValueDoesNotMatchType> {
        Ok(())
    }
}

#[sealed]
impl Reflect for () {
    type Transformed = Self;
    type AsParam = Self;

    fn transform(self) -> Self::Transformed {
        self
    }

    fn untransform(transformed: Self::Transformed) -> Self {
        transformed
    }

    fn resolve() -> Ty {
        TypeRegistry::store::<Self>(TypeDescription::Leaf)
    }
}

macro_rules! simple_reflect {
    ($t:ty, $ir:ident) => {
        impl From<$t> for IrValue {
            fn from(value: $t) -> Self {
                IrValue::$ir(value)
            }
        }

        impl TryFrom<&IrValue> for $t {
            type Error = ();

            fn try_from(value: &IrValue) -> Result<Self, Self::Error> {
                match value {
                    IrValue::$ir(x) => Ok(*x),
                    _ => Err(()),
                }
            }
        }

        impl Param<$t> for $t {
            fn as_param(value: &mut $t) -> Self {
                *value
            }

            fn to_value(self) -> $t {
                self
            }

            fn from_ir_value(
                _: &mut Memory,
                value: IrValue,
            ) -> Result<Self, IrValueDoesNotMatchType> {
                let IrValue::$ir(p) = value else {
                    return Err(IrValueDoesNotMatchType);
                };
                Ok(p)
            }
        }

        #[sealed]
        impl Reflect for $t {
            type Transformed = Self;
            type AsParam = Self;

            fn transform(self) -> Self::Transformed {
                self
            }

            fn untransform(transformed: Self::Transformed) -> Self {
                transformed
            }

            fn resolve() -> Ty {
                TypeRegistry::store::<Self>(TypeDescription::Leaf)
            }
        }
    };
}

simple_reflect!(bool, Bool);
simple_reflect!(u8, U8);
simple_reflect!(u16, U16);
simple_reflect!(u32, U32);
simple_reflect!(u64, U64);
simple_reflect!(i8, I8);
simple_reflect!(i16, I16);
simple_reflect!(i32, I32);
simple_reflect!(i64, I64);
simple_reflect!(f32, F32);
simple_reflect!(f64, F64);
simple_reflect!(char, Char);
simple_reflect!(Asn, Asn);
