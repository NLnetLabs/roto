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
//!
//! We might need polymorphism over the number of arguments.
//! The IR needs typed variables to do this correctly.

pub mod func;
pub mod ty;
pub mod val;
pub mod verdict;

use std::{any::TypeId, net::IpAddr};

use func::{Func, FunctionDescription};
use inetnum::{addr::Prefix, asn::Asn};
use roto_macros::roto_method;
use ty::{Ty, TypeDescription, TypeRegistry};

/// Provides the types and functions that Roto can access via FFI
///
/// Even some types that can be written as literals should be provided here.
/// The idea here is that Roto can be used with different representations
/// of these types in different applications. The type checker will yield an
/// error if a literal is provided for an undeclared type.
pub struct Runtime {
    pub runtime_types: Vec<RuntimeType>,
    pub functions: Vec<RuntimeFunction>,
    pub type_registry: TypeRegistry,
}

#[derive(Debug)]
pub enum Movability {
    Copy,
    CloneDrop {
        clone: unsafe extern "C" fn(*const (), *mut ()),
        drop: unsafe extern "C" fn(*mut ()),
    },
}

unsafe extern "C" fn extern_clone<T: Clone>(from: *const (), to: *mut ()) {
    let from = from as *const T;
    let to = to as *mut T;

    let from = unsafe { &*from };

    // *to is uninitialized so we *must* use std::ptr::write instead of using
    // a pointer assignment.
    unsafe { std::ptr::write(to, from.clone()) };
}

unsafe extern "C" fn extern_drop<T>(x: *mut ()) {
    let x = x as *mut T;
    std::ptr::read(x);
}

#[derive(Debug)]
pub struct RuntimeType {
    /// The name the type can be referenced by from Roto
    name: String,

    /// [`TypeId`] of the registered type
    ///
    /// This can be used to index into the [`TypeRegistry`]
    type_id: TypeId,

    /// Whether this type is `Copy`
    movability: Movability,

    size: usize,

    alignment: usize,
}

impl RuntimeType {
    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn type_id(&self) -> TypeId {
        self.type_id
    }

    pub fn movability(&self) -> &Movability {
        &self.movability
    }

    pub fn size(&self) -> usize {
        self.size
    }

    pub fn alignment(&self) -> usize {
        self.alignment
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FunctionKind {
    Free,
    Method(TypeId),
    StaticMethod(TypeId),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RuntimeFunction {
    /// Name that the function can be referenced by
    pub name: String,

    /// Description of the signature of the function
    pub description: FunctionDescription,

    /// Whether it's a free function, method or a static method
    pub kind: FunctionKind,

    /// Unique identifier for this function
    pub id: usize,
}

impl Runtime {
    /// Register a type with a default name
    ///
    /// This type will be cloned and dropped many times, so make sure to have
    /// a cheap [`Clone`] and [`Drop`](std::ops::Drop) implementations, for example an
    /// [`Rc`](std::rc::Rc) or an [`Arc`](std::sync::Arc).
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
    /// [`Runtime::register_clone_type_with_name`] instead.
    pub fn register_clone_type<T: 'static + Clone>(
        &mut self,
    ) -> Result<(), String> {
        let name = Self::extract_name::<T>();
        self.register_clone_type_with_name::<T>(name)
    }

    /// Register a `Copy` type with a default name
    ///
    /// See [`Runtime::register_clone_type`]
    pub fn register_copy_type<T: Copy + 'static>(
        &mut self,
    ) -> Result<(), String> {
        let name = Self::extract_name::<T>();
        self.register_copy_type_with_name::<T>(name)
    }

    pub fn register_copy_type_with_name<T: Copy + 'static>(
        &mut self,
        name: &str,
    ) -> Result<(), String> {
        self.register_type_with_name_internal::<T>(name, Movability::Copy)
    }

    /// Register a reference type with a given name
    ///
    /// This makes the type available for use in Roto. However, Roto will
    /// only store pointers to this type.
    pub fn register_clone_type_with_name<T: 'static + Clone>(
        &mut self,
        name: &str,
    ) -> Result<(), String> {
        let movability = Movability::CloneDrop {
            clone: extern_clone::<T> as _,
            drop: extern_drop::<T> as _,
        };
        self.register_type_with_name_internal::<T>(name, movability)
    }

    fn extract_name<T: 'static>() -> &'static str {
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
    fn register_type_with_name_internal<T: 'static>(
        &mut self,
        name: &str,
        movability: Movability,
    ) -> Result<(), String> {
        if let Some(ty) = self.runtime_types.iter().find(|ty| ty.name == name)
        {
            let name = self.type_registry.get(ty.type_id).unwrap().rust_name;
            return Err(format!(
                "Type with name {name} already registered.\n\
                Previously registered type: {}\n\
                Newly registered type: {}",
                name,
                std::any::type_name::<T>(),
            ));
        }

        if let Some(ty) = self
            .runtime_types
            .iter()
            .find(|ty| ty.type_id == TypeId::of::<T>())
        {
            return Err(format!(
                "Type {} is already registered under a different name: {}`",
                std::any::type_name::<T>(),
                ty.name,
            ));
        }

        // We do not allow registering reference types and such, at least
        // for now.
        assert!(!name.starts_with(['&', '*']));

        self.type_registry.store::<T>(ty::TypeDescription::Leaf);
        self.runtime_types.push(RuntimeType {
            name: name.into(),
            type_id: TypeId::of::<T>(),
            movability,
            size: std::mem::size_of::<T>(),
            alignment: std::mem::align_of::<T>(),
        });
        Ok(())
    }

    pub fn register_function<A, R>(
        &mut self,
        name: impl Into<String>,
        f: impl Func<A, R>,
    ) -> Result<(), String> {
        let description = f.to_function_description(self)?;
        self.check_description(&description)?;

        let id = self.functions.len();
        self.functions.push(RuntimeFunction {
            name: name.into(),
            description,
            kind: FunctionKind::Free,
            id,
        });
        Ok(())
    }

    pub fn register_method<T: 'static, A, R>(
        &mut self,
        name: impl Into<String>,
        f: impl Func<A, R>,
    ) -> Result<(), String> {
        let description = f.to_function_description(self).unwrap();
        self.check_description(&description)?;

        let Some(second) = description.parameter_types().get(1) else {
            panic!()
        };

        // `to_function_description` already checks the validity of the types
        // so unwrap is ok.
        let ty = self.type_registry.get(*second).unwrap();

        let type_id = match ty.description {
            TypeDescription::Leaf => ty.type_id,
            TypeDescription::ConstPtr(id) | TypeDescription::MutPtr(id) => id,
            _ => {
                return Err(format!(
                    "Cannot register a method on {}",
                    ty.rust_name
                ))
            }
        };

        if type_id != std::any::TypeId::of::<T>() {
            return Err(
                "Registering a method on a type that doesn't correspond."
                    .to_string(),
            );
        }

        let id = self.functions.len();
        self.functions.push(RuntimeFunction {
            name: name.into(),
            description,
            kind: FunctionKind::Method(std::any::TypeId::of::<T>()),
            id,
        });

        Ok(())
    }

    pub fn register_static_method<T: 'static, A, R>(
        &mut self,
        name: impl Into<String>,
        f: impl Func<A, R>,
    ) -> Result<(), String> {
        let description = f.to_function_description(self).unwrap();
        self.check_description(&description)?;

        let id = self.functions.len();
        self.functions.push(RuntimeFunction {
            name: name.into(),
            description,
            kind: FunctionKind::StaticMethod(std::any::TypeId::of::<T>()),
            id,
        });
        Ok(())
    }

    pub fn get_runtime_type(&self, id: TypeId) -> Option<&RuntimeType> {
        let ty = self.type_registry.get(id)?;
        let id = match ty.description {
            TypeDescription::Leaf => id,
            TypeDescription::ConstPtr(id) => id,
            TypeDescription::MutPtr(id) => id,
            _ => panic!(),
        };
        self.runtime_types.iter().find(|ty| ty.type_id == id)
    }

    fn check_description(
        &self,
        description: &FunctionDescription,
    ) -> Result<(), String> {
        let check_type = |id: &TypeId| {
            self.get_runtime_type(*id).ok_or_else(|| {
                let ty = self.type_registry.get(*id).unwrap();
                format!(
                    "Registered a function using an unregistered type: `{}`",
                    ty.rust_name
                )
            })
        };

        for ty in description.parameter_types() {
            check_type(ty)?;
        }

        check_type(&description.return_type())?;
        if description.return_type() != TypeId::of::<()>() {
            return Err("Runtime function cannot have a return type".into());
        }
        Ok(())
    }
}

impl Runtime {
    /// A Runtime that is as empty as possible.
    ///
    /// This contains only type information for Roto primitives.
    pub fn basic() -> Result<Self, String> {
        let mut rt = Runtime {
            runtime_types: Default::default(),
            functions: Default::default(),
            type_registry: Default::default(),
        };

        rt.register_copy_type_with_name::<()>("Unit")?;
        rt.register_copy_type::<bool>()?;
        rt.register_copy_type::<u8>()?;
        rt.register_copy_type::<u16>()?;
        rt.register_copy_type::<u32>()?;
        rt.register_copy_type::<u64>()?;
        rt.register_copy_type::<i8>()?;
        rt.register_copy_type::<i16>()?;
        rt.register_copy_type::<i32>()?;
        rt.register_copy_type::<i64>()?;
        rt.register_copy_type::<Asn>()?;
        rt.register_copy_type::<IpAddr>()?;
        rt.register_copy_type::<Prefix>()?;

        extern "C" fn prefix_new(out: *mut Prefix, ip: *mut IpAddr, len: u8) {
            let ip = unsafe { *ip };

            let p = Prefix::new(ip, len).unwrap();
            let p = unsafe {
                std::mem::transmute::<
                    Prefix,
                    [u8; std::mem::size_of::<Prefix>()],
                >(p)
            };

            let out = out as *mut [u8; std::mem::size_of::<Prefix>()];
            unsafe {
                *out = p;
            }
        }

        rt.register_static_method::<Prefix, _, _>(
            "new",
            prefix_new as extern "C" fn(_, _, _) -> _,
        )
        .unwrap();

        #[roto_method(rt, IpAddr, eq)]
        fn ipaddr_eq(a: *const IpAddr, b: *const IpAddr) -> bool {
            let a = unsafe { *a };
            let b = unsafe { *b };
            a == b
        }

        #[roto_method(rt, IpAddr)]
        fn is_ipv4(ip: *const IpAddr) -> bool {
            let ip = unsafe { &*ip };
            ip.is_ipv4()
        }

        #[roto_method(rt, IpAddr)]
        fn is_ipv6(ip: *const IpAddr) -> bool {
            let ip = unsafe { &*ip };
            ip.is_ipv6()
        }

        #[roto_method(rt, IpAddr)]
        fn to_canonical(ip: *const IpAddr) -> IpAddr {
            let ip = unsafe { &*ip };
            ip.to_canonical()
        }

        Ok(rt)
    }

    // We might not use this, but let's keep it around for now (as of 27/8/2024)
    #[allow(unused)]
    fn find_type(&self, id: TypeId, name: &str) -> Result<&Ty, String> {
        match self.type_registry.get(id) {
            Some(t) => Ok(t),
            None => Err(format!("Type `{name}` has not been registered and cannot be inspected by Roto")),
        }
    }
}

#[cfg(test)]
pub mod tests {
    use super::Runtime;
    use roto_macros::{roto_function, roto_method};
    use routecore::bgp::{
        aspath::{AsPath, HopPath},
        communities::Community,
        types::{LocalPref, OriginType},
    };

    pub fn routecore_runtime() -> Result<Runtime, String> {
        let mut rt = Runtime::basic()?;

        rt.register_clone_type::<OriginType>()?;
        rt.register_clone_type::<LocalPref>()?;
        rt.register_clone_type::<Community>()?;
        rt.register_clone_type::<HopPath>()?;
        rt.register_clone_type::<AsPath<Vec<u8>>>()?;

        #[roto_function(rt)]
        fn pow(x: u32, y: u32) -> u32 {
            x.pow(y)
        }

        #[roto_method(rt, u32)]
        fn is_even(x: u32) -> bool {
            x % 2 == 0
        }

        Ok(rt)
    }

    #[test]
    fn default_runtime() {
        let rt = routecore_runtime().unwrap();

        let names: Vec<_> =
            rt.runtime_types.iter().map(|ty| &ty.name).collect();
        assert_eq!(
            names,
            &[
                "Unit",
                "bool",
                "u8",
                "u16",
                "u32",
                "u64",
                "i8",
                "i16",
                "i32",
                "i64",
                "Asn",
                "IpAddr",
                "Prefix",
                "OriginType",
                "LocalPref",
                "Community",
                "HopPath",
                "AsPath"
            ]
        );
    }
}
