//! Runtime definition for Roto
//!
//! Roto is an embedded language, therefore, it must be hooked up to the
//! outside world to do anything useful besides pure computation. This
//! connection is provided by the [`Runtime`], which exposes types, methods
//! and functions to Roto.
//!
//! Roto can run in different [`Runtime`]s, depending on the situation.
//! Extending the default [`Runtime`] is the primary way to extend the
//! capabilities of Roto.
//!
//! > **Note about safety**: You should be careful with the functions
//! > provided to Roto. If the types of functions and methods in Roto do
//! > not match up with the types in Rust then the Roto script will probably
//! > be unsound. That being said, FFI is just unsafe in general.
//!
//! TODO: Figure out some macro to automatically derive the correct
//! signature.
//!
//! A runtime function needs a bit of information:
//!  - The number of arguments
//!  - The type of the arguments.
//!  - The return type
//!  - Whether or not a type should be copied is not just up to the type
//!    but also up to the method, making this whole thing more complicated.
//! We might need polymorphism over the number of arguments.
//! The IR needs typed variables to do this correctly.
pub mod func;

use std::{
    any::{Any, TypeId},
    mem,
    net::IpAddr,
};

use func::{Func, Param, ResolvedFunctionDescription};
use routecore::{
    addr::Prefix,
    bgp::{
        aspath::{AsPath, HopPath},
        communities::Community,
        path_attributes::{
            Aggregator, AtomicAggregate, MultiExitDisc, NextHop,
        },
        types::{LocalPref, OriginType},
    },
};

use crate::lower::value::IrType;

/// Provides the types and functions that Roto can access via FFI
///
/// Even some types that can be written as literals should be provided here.
/// The idea here is that Roto can be used with different representations
/// of these types in different applications. The type checker will yield an
/// error if a literal is provided for an undeclared type.
pub struct Runtime {
    pub types: Vec<RuntimeType>,
    pub functions: Vec<RuntimeFunction>,
}

#[derive(Debug)]
pub struct RuntimeType {
    /// The name the type can be referenced by from Roto
    pub name: String,

    /// The name of the type in Rust, mostly for diagnostic purposes
    pub rust_name: &'static str,

    /// The memory alignment of the type
    pub alignment: usize,

    /// The size of the type
    pub size: usize,

    /// The `TypeId` corresponding to this type
    pub type_id: TypeId,

    /// The `TypeId` of a `*const` to this type
    pub const_ptr_id: TypeId,

    /// The `TypeId` of a `*mut` to this type
    pub mut_ptr_id: TypeId,

    /// Whether this type is `Copy`
    pub is_copy_type: bool,
}

#[derive(Debug)]
pub enum FunctionKind {
    Free,
    Method(TypeId),
    StaticMethod(TypeId),
}

#[derive(Debug)]
pub struct RuntimeFunction {
    pub name: String,
    pub description: ResolvedFunctionDescription,
    pub kind: FunctionKind,
}

pub struct RuntimeMethod {
    pub name: &'static str,
    pub parameter_types: Vec<IrType>,
    pub return_type: Option<IrType>,
    pub func: *const u8,
}

impl Runtime {
    /// Register a type with a default name
    ///
    /// The default type name is based on [`std::any::type_name`]. The string
    /// returned from that consists of a path with possibly some generics.
    /// Neither full paths and generics make sense in Roto, so we just want
    /// the last part of the path just before any generics. So, we determine
    /// the name with the following procedure:
    ///
    ///  - Split at the first `<` (if any) and take the first part
    ///  - Then split at the last `::` and take the last part.
    ///
    /// If that doesn't work for the type you want, use
    /// [`Runtime::register_type_with_name] instead.
    pub fn register_type<T: Any>(&mut self) {
        let name = Self::extract_name::<T>();
        self.register_type_with_name::<T>(name)
    }

    /// Register a `Copy` type with a default name
    ///
    /// See [`Runtime::register_type`]
    pub fn register_copy_type<T: Any + Copy>(&mut self) {
        let name = Self::extract_name::<T>();
        self.register_copy_type_with_name::<T>(name)
    }

    pub fn register_copy_type_with_name<T: Any + Copy>(
        &mut self,
        name: &str,
    ) {
        self.register_type_with_name_internal::<T>(name, true)
    }

    /// Register a reference type with a given name
    ///
    /// This makes the type available for use in Roto. However, Roto will
    /// only store pointers to this type.
    pub fn register_type_with_name<T: Any>(&mut self, name: &str) {
        self.register_type_with_name_internal::<T>(name, false)
    }

    fn extract_name<T: Any>() -> &'static str {
        let mut name = std::any::type_name::<T>();
        if let Some((first, _)) = name.split_once('<') {
            name = first;
        }
        if let Some((_, second)) = name.rsplit_once("::") {
            name = second;
        }
        name
    }

    /// Register a type for use in Roto specifying whether the type is `Copy`
    ///
    /// The `Copy`-ness is not checked. Which is why this
    fn register_type_with_name_internal<T: Any>(
        &mut self,
        name: &str,
        is_copy_type: bool,
    ) {
        if let Some(ty) = self.types.iter().find(|ty| ty.name == name) {
            panic!(
                "Type with name {name} already registered.\n\
                Previously registered type: {}\n\
                Newly registered type: {}",
                ty.rust_name,
                std::any::type_name::<T>(),
            )
        }

        if let Some(ty) =
            self.types.iter().find(|ty| ty.type_id == TypeId::of::<T>())
        {
            panic!(
                "Type {} is already registered under a different name: {}`",
                std::any::type_name::<T>(),
                ty.name,
            )
        }

        // We do not allow registering reference types and such, at least
        // for now.
        assert!(!name.starts_with(['&', '*']));

        self.types.push(RuntimeType {
            name: name.into(),
            rust_name: std::any::type_name::<T>(),
            size: mem::size_of::<T>(),
            alignment: mem::align_of::<T>(),
            type_id: TypeId::of::<T>(),
            const_ptr_id: TypeId::of::<*const T>(),
            mut_ptr_id: TypeId::of::<*mut T>(),
            is_copy_type,
        })
    }

    pub fn register_function<A, R>(
        &mut self,
        name: impl Into<String>,
        f: impl Func<A, R>,
    ) {
        let desc = f.to_function_description();
        let resolved = desc.resolve(self).unwrap();
        self.functions.push(RuntimeFunction {
            name: name.into(),
            description: resolved,
            kind: FunctionKind::Free,
        })
    }

    pub fn register_method<T: Any, A, R>(
        &mut self,
        name: impl Into<String>,
        f: impl Func<A, R>,
    ) {
        let desc = f.to_function_description();
        let resolved = desc.resolve(self).unwrap();

        let Some(first) = resolved.parameter_types.first() else {
            panic!()
        };

        let type_id = match first {
            Param::Val(id) | Param::ConstPtr(id) | Param::MutPtr(id) => *id,
        };

        if type_id != std::any::TypeId::of::<T>() {
            panic!("Registering a method on a type that doesn't correspond.")
        }

        self.functions.push(RuntimeFunction {
            name: name.into(),
            description: resolved,
            kind: FunctionKind::Method(std::any::TypeId::of::<T>()),
        })
    }

    pub fn register_static_method<T: Any, A, R>(
        &mut self,
        name: impl Into<String>,
        f: impl Func<A, R>,
    ) {
        let desc = f.to_function_description();
        let resolved = desc.resolve(self).unwrap();
        self.functions.push(RuntimeFunction {
            name: name.into(),
            description: resolved,
            kind: FunctionKind::StaticMethod(std::any::TypeId::of::<T>()),
        })
    }

    pub fn get_type(&self, id: TypeId) -> &RuntimeType {
        let ty = self.types.iter().find(|ty| ty.type_id == id).unwrap();
        ty
    }
}

impl Runtime {
    /// A Runtime that is as empty as possible.
    ///
    /// This contains only type information for Roto primitives.
    fn empty() -> Self {
        let mut rt = Runtime {
            types: Default::default(),
            functions: Default::default(),
        };

        rt.register_type::<bool>();
        rt.register_type::<u8>();
        rt.register_type::<u16>();
        rt.register_type::<u32>();
        rt.register_type::<u64>();
        rt.register_type::<i8>();
        rt.register_type::<i16>();
        rt.register_type::<i32>();
        rt.register_type::<i64>();

        rt
    }

    fn find_type(&self, id: TypeId) -> Option<Param> {
        for ty in &self.types {
            if ty.type_id == id {
                return Some(Param::Val(ty.type_id));
            }
            if ty.const_ptr_id == id {
                return Some(Param::ConstPtr(ty.type_id));
            }
            if ty.mut_ptr_id == id {
                return Some(Param::MutPtr(ty.type_id));
            }
        }
        None
    }
}

impl Default for Runtime {
    fn default() -> Self {
        let mut rt = Self::empty();

        rt.register_type::<IpAddr>();
        rt.register_type::<OriginType>();
        rt.register_type::<NextHop>();
        rt.register_type::<MultiExitDisc>();
        rt.register_type::<LocalPref>();
        rt.register_type::<Aggregator>();
        rt.register_type::<AtomicAggregate>();
        rt.register_type::<Community>();
        rt.register_type::<Prefix>();
        rt.register_type::<HopPath>();
        rt.register_type::<AsPath<Vec<u8>>>();

        rt
    }
}

#[cfg(test)]
mod tests {
    use super::Runtime;

    #[test]
    fn default_runtime() {
        let rt = Runtime::default();

        let names: Vec<_> = rt.types.iter().map(|ty| &ty.name).collect();
        assert_eq!(
            names,
            &[
                "bool",
                "u8",
                "u16",
                "u32",
                "u64",
                "i8",
                "i16",
                "i32",
                "i64",
                "IpAddr",
                "OriginType",
                "NextHop",
                "MultiExitDisc",
                "LocalPref",
                "Aggregator",
                "AtomicAggregate",
                "Community",
                "Prefix",
                "HopPath",
                "AsPath"
            ]
        );
    }
}
