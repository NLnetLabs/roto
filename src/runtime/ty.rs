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
    any::{type_name, TypeId},
    collections::HashMap,
    net::IpAddr,
    sync::{Arc, LazyLock, Mutex},
};

use inetnum::{addr::Prefix, asn::Asn};

use crate::{IrValue, Memory};

use super::{optional::Optional, val::Val, verdict::Verdict};

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
    pub alignment: usize,

    /// The size of the type in bytes
    pub size: usize,

    /// The [`TypeId`] corresponding to this type
    pub type_id: TypeId,

    /// Description of the structure of the type
    pub description: TypeDescription,
}

impl Ty {
    fn new<T: 'static>(description: TypeDescription) -> Self {
        Self {
            rust_name: type_name::<T>(),
            alignment: std::mem::align_of::<T>(),
            size: std::mem::size_of::<T>(),
            type_id: TypeId::of::<T>(),
            description,
        }
    }
}

pub static GLOBAL_TYPE_REGISTRY: LazyLock<Mutex<TypeRegistry>> =
    LazyLock::new(|| Mutex::new(TypeRegistry::default()));

/// A map from [`TypeId`] to a [`Ty`], which is a description of the type
#[derive(Default)]
pub struct TypeRegistry {
    map: HashMap<TypeId, Ty>,
}

impl TypeRegistry {
    pub fn store<T: 'static>(&mut self, description: TypeDescription) -> Ty {
        let ty = Ty::new::<T>(description);
        self.map.entry(ty.type_id).or_insert(ty).clone()
    }

    pub fn get(&self, id: TypeId) -> Option<&Ty> {
        self.map.get(&id)
    }

    /// Register a type implementing [`Reflect`]
    pub fn resolve<T: Reflect>(&mut self) -> Ty {
        T::resolve(self)
    }
}

/// A type that can register itself into a [`TypeRegistry`].
///
/// Via the [`TypeRegistry`], it is then possible to query for information
/// about this type. Reflection is recursive for types such as [`Verdict`],
/// [`Result`] and [`Option`].
///
/// Pointers are explicitly _not_ recursive, because they can be used to pass
/// pointers to types that have been registered to Roto and therefore don't
/// need to implement this trait.
///
/// Additionally, this trait specifies how a type should be passed to Roto, via
/// the `AsParam` associated type.
pub trait Reflect: Sized + 'static {
    /// Intermediate type that can be used to convert a type to a Roto type
    type Transformed;

    /// The type that this type should be converted into when passed to Roto
    type AsParam: Param<Self::Transformed>;

    fn transform(self) -> Self::Transformed;

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

    /// Put information about this type into the [`TypeRegistry`]
    ///
    /// The information is also returned for direct use.
    fn resolve(registry: &mut TypeRegistry) -> Ty;

    /// Turn this value into bytes
    ///
    /// This is used by the IR evaluator
    fn to_bytes(mut transformed: Self::Transformed) -> Vec<u8> {
        let ptr = &mut transformed as *mut Self::Transformed as *mut u8;
        std::mem::forget(transformed);
        let size = std::mem::size_of::<Self::Transformed>();
        unsafe { Vec::from_raw_parts(ptr, size, size) }
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

impl<T: Clone> Param<T> for *mut T {
    fn as_param(transformed: &mut T) -> Self {
        transformed as *mut T
    }

    fn to_value(self) -> T {
        unsafe { &*self }.clone()
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

    fn resolve(registry: &mut TypeRegistry) -> Ty {
        let t = A::resolve(registry).type_id;
        let e = R::resolve(registry).type_id;

        let desc = TypeDescription::Verdict(t, e);
        registry.store::<Self>(desc)
    }
}

impl<T: Reflect> Reflect for Option<T>
where
    T::Transformed: Clone,
{
    type Transformed = Optional<T::Transformed>;
    type AsParam = *mut Self::Transformed;

    fn transform(self) -> Self::Transformed {
        match self {
            Some(t) => Optional::Some(t.transform()),
            None => Optional::None,
        }
    }

    fn untransform(transformed: Self::Transformed) -> Self {
        match transformed {
            Optional::Some(t) => Some(T::untransform(t)),
            Optional::None => None,
        }
    }

    fn resolve(registry: &mut TypeRegistry) -> Ty {
        let t = T::resolve(registry).type_id;

        let desc = TypeDescription::Option(t);
        registry.store::<Self>(desc)
    }
}

impl<T: Clone> Param<Val<T>> for *mut T {
    fn as_param(value: &mut Val<T>) -> Self {
        &mut value.0 as *mut _
    }

    fn to_value(self) -> Val<T> {
        Val(unsafe { &*self }.clone())
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

impl<T: 'static + Clone> Reflect for Val<T> {
    type Transformed = Self;
    type AsParam = *mut T;

    fn transform(self) -> Self::Transformed {
        self
    }

    fn untransform(transformed: Self::Transformed) -> Self {
        transformed
    }

    fn resolve(registry: &mut TypeRegistry) -> Ty {
        let t = registry.store::<T>(TypeDescription::Leaf).type_id;

        let desc = TypeDescription::Val(t);
        registry.store::<Self>(desc)
    }
}

impl Reflect for IpAddr {
    type Transformed = Self;
    type AsParam = *mut Self;

    fn transform(self) -> Self::Transformed {
        self
    }

    fn untransform(transformed: Self::Transformed) -> Self {
        transformed
    }

    fn resolve(registry: &mut TypeRegistry) -> Ty {
        registry.store::<Self>(TypeDescription::Leaf)
    }
}

impl Reflect for Prefix {
    type Transformed = Self;
    type AsParam = *mut Self;

    fn transform(self) -> Self::Transformed {
        self
    }

    fn untransform(transformed: Self::Transformed) -> Self {
        transformed
    }

    fn resolve(registry: &mut TypeRegistry) -> Ty {
        registry.store::<Self>(TypeDescription::Leaf)
    }
}

impl Reflect for Arc<str> {
    type Transformed = Self;
    type AsParam = *mut Self;

    fn transform(self) -> Self::Transformed {
        self
    }

    fn untransform(transformed: Self::Transformed) -> Self {
        transformed
    }

    fn resolve(registry: &mut TypeRegistry) -> Ty {
        registry.store::<Self>(TypeDescription::Leaf)
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

impl Reflect for () {
    type Transformed = Self;
    type AsParam = Self;

    fn transform(self) -> Self::Transformed {
        self
    }

    fn untransform(transformed: Self::Transformed) -> Self {
        transformed
    }

    fn resolve(registry: &mut TypeRegistry) -> Ty {
        registry.store::<Self>(TypeDescription::Leaf)
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

        impl Reflect for $t {
            type Transformed = Self;
            type AsParam = Self;

            fn transform(self) -> Self::Transformed {
                self
            }

            fn untransform(transformed: Self::Transformed) -> Self {
                transformed
            }

            fn resolve(registry: &mut TypeRegistry) -> Ty {
                registry.store::<Self>(TypeDescription::Leaf)
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
simple_reflect!(Asn, Asn);
