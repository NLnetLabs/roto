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
};

use inetnum::asn::Asn;

use super::verdict::Verdict;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TypeDescription {
    /// Some type that we don't know how to decompose
    Leaf,

    /// `*const T`
    ConstPtr(TypeId),

    /// `*mut T`
    MutPtr(TypeId),

    /// `Option<T>`
    Option(TypeId),

    /// `Result<T, E>`
    Result(TypeId, TypeId),

    /// `Verdict<A, R>`
    Verdict(TypeId, TypeId),
}

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

/// A map from TypeId to a [`Ty`], which is a description of the type
#[derive(Default)]
pub struct TypeRegistry {
    map: HashMap<TypeId, Ty>,
}

impl TypeRegistry {
    pub fn store<T: 'static>(&mut self, description: TypeDescription) -> &Ty {
        let ty = Ty::new::<T>(description);
        self.map.entry(ty.type_id).or_insert(ty)
    }

    pub fn get(&self, id: TypeId) -> Option<&Ty> {
        self.map.get(&id)
    }

    pub fn resolve<T: Reflect>(&mut self) -> &Ty {
        T::resolve(self)
    }
}

pub trait Reflect: 'static {
    fn resolve(registry: &mut TypeRegistry) -> &Ty;
}

impl<A: Reflect, R: Reflect> Reflect for Verdict<A, R> {
    fn resolve(registry: &mut TypeRegistry) -> &Ty {
        let t = A::resolve(registry).type_id;
        let e = R::resolve(registry).type_id;

        let desc = TypeDescription::Verdict(t, e);
        registry.store::<Self>(desc)
    }
}

impl<T: Reflect, E: Reflect> Reflect for Result<T, E> {
    fn resolve(registry: &mut TypeRegistry) -> &Ty {
        let t = T::resolve(registry).type_id;
        let e = E::resolve(registry).type_id;

        let desc = TypeDescription::Result(t, e);
        registry.store::<Self>(desc)
    }
}

impl<T: Reflect> Reflect for Option<T> {
    fn resolve(registry: &mut TypeRegistry) -> &Ty {
        let t = T::resolve(registry).type_id;

        let desc = TypeDescription::Option(t);
        registry.store::<Self>(desc)
    }
}

impl<T: 'static> Reflect for *mut T {
    fn resolve(registry: &mut TypeRegistry) -> &Ty {
        let t = registry.store::<T>(TypeDescription::Leaf).type_id;

        let desc = TypeDescription::MutPtr(t);
        registry.store::<Self>(desc)
    }
}

impl<T: 'static> Reflect for *const T {
    fn resolve(registry: &mut TypeRegistry) -> &Ty {
        let t = registry.store::<T>(TypeDescription::Leaf).type_id;

        let desc = TypeDescription::ConstPtr(t);
        registry.store::<Self>(desc)
    }
}

macro_rules! simple_reflect {
    ($t:ty) => {
        impl Reflect for $t {
            fn resolve(registry: &mut TypeRegistry) -> &Ty {
                registry.store::<Self>(TypeDescription::Leaf)
            }
        }
    };
}

simple_reflect!(());
simple_reflect!(bool);
simple_reflect!(u8);
simple_reflect!(u16);
simple_reflect!(u32);
simple_reflect!(u64);
simple_reflect!(i8);
simple_reflect!(i16);
simple_reflect!(i32);
simple_reflect!(i64);
simple_reflect!(Asn);
