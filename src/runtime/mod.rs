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

use std::{
    any::{type_name, TypeId},
    collections::HashMap,
    net::{IpAddr, Ipv4Addr, Ipv6Addr},
};

use func::{Func, FunctionDescription};
use inetnum::{addr::Prefix, asn::Asn};
use roto_macros::{roto_method, roto_static_method};
use ty::{Ty, TypeDescription, TypeRegistry};

use crate::ast::Identifier;

/// Provides the types and functions that Roto can access via FFI
///
/// Even some types that can be written as literals should be provided here.
/// The idea here is that Roto can be used with different representations
/// of these types in different applications. The type checker will yield an
/// error if a literal is provided for an undeclared type.
pub struct Runtime {
    pub runtime_types: Vec<RuntimeType>,
    pub functions: Vec<RuntimeFunction>,
    pub constants: HashMap<Identifier, RuntimeConstant>,
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

    /// Size of the type in bytes
    size: usize,

    /// Alignment of the type in bytes
    alignment: usize,

    /// Docstring of the type to display in documentation
    docstring: String,
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

    pub docstring: &'static str,

    pub argument_names: &'static [&'static str],
}

pub struct DocumentedFunc<F> {
    pub func: F,
    pub docstring: &'static str,
    pub argument_names: &'static [&'static str],
}

#[derive(Clone)]
pub struct RuntimeConstant {
    pub name: Identifier,
    pub ty: TypeId,
    pub bytes: Box<[u8]>,
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
        docstring: &str,
    ) -> Result<(), String> {
        let name = Self::extract_name::<T>();
        self.register_clone_type_with_name::<T>(name, docstring)
    }

    /// Register a `Copy` type with a default name
    ///
    /// See [`Runtime::register_clone_type`]
    pub fn register_copy_type<T: Copy + 'static>(
        &mut self,
        docstring: &str,
    ) -> Result<(), String> {
        let name = Self::extract_name::<T>();
        self.register_copy_type_with_name::<T>(name, docstring)
    }

    pub fn register_copy_type_with_name<T: Copy + 'static>(
        &mut self,
        name: &str,
        docstring: &str,
    ) -> Result<(), String> {
        self.register_type_with_name_internal::<T>(
            name,
            Movability::Copy,
            docstring,
        )
    }

    /// Register a reference type with a given name
    ///
    /// This makes the type available for use in Roto. However, Roto will
    /// only store pointers to this type.
    pub fn register_clone_type_with_name<T: 'static + Clone>(
        &mut self,
        name: &str,
        docstring: &str,
    ) -> Result<(), String> {
        let movability = Movability::CloneDrop {
            clone: extern_clone::<T> as _,
            drop: extern_drop::<T> as _,
        };
        self.register_type_with_name_internal::<T>(
            name, movability, docstring,
        )
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
        docstring: &str,
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
            docstring: String::from(docstring),
        });
        Ok(())
    }

    pub fn register_function<A, R>(
        &mut self,
        name: impl Into<String>,
        f: impl Func<A, R>,
    ) -> Result<(), String> {
        let docstring = f.docstring();
        let argument_names = f.argument_names();
        let description = f.to_function_description(self)?;
        self.check_description(&description)?;

        let id = self.functions.len();
        self.functions.push(RuntimeFunction {
            name: name.into(),
            description,
            kind: FunctionKind::Free,
            id,
            docstring,
            argument_names,
        });
        Ok(())
    }

    pub fn register_method<T: 'static, A, R>(
        &mut self,
        name: impl Into<String>,
        f: impl Func<A, R>,
    ) -> Result<(), String> {
        let docstring = f.docstring();
        let argument_names = f.argument_names();
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
            docstring,
            argument_names,
        });

        Ok(())
    }

    pub fn register_static_method<T: 'static, A, R>(
        &mut self,
        name: impl Into<String>,
        f: impl Func<A, R>,
    ) -> Result<(), String> {
        let docstring = f.docstring();
        let argument_names = f.argument_names();
        let description = f.to_function_description(self).unwrap();
        self.check_description(&description)?;

        let id = self.functions.len();
        self.functions.push(RuntimeFunction {
            name: name.into(),
            description,
            kind: FunctionKind::StaticMethod(std::any::TypeId::of::<T>()),
            id,
            docstring,
            argument_names,
        });
        Ok(())
    }

    pub fn register_constant<T: 'static>(
        &mut self,
        name: &str,
        x: T,
    ) -> Result<(), String> {
        let type_id = TypeId::of::<T>();
        self.find_type(type_id, type_name::<T>())?;
        let mut bytes: Vec<u8> = vec![0; size_of::<T>()];
        unsafe {
            std::ptr::copy_nonoverlapping(
                &x as *const T as *const _,
                bytes.as_mut_ptr(),
                bytes.len(),
            )
        };

        let symbol = Identifier::from(name);
        self.constants.insert(
            symbol,
            RuntimeConstant {
                name: symbol,
                ty: type_id,
                bytes: bytes.into_boxed_slice(),
            },
        );

        Ok(())
    }

    pub fn iter_constants(
        &self,
    ) -> impl Iterator<Item = (Identifier, TypeId)> + '_ {
        self.constants.values().map(|g| (g.name, g.ty))
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

    fn print_function(&self, f: &RuntimeFunction) {
        let print_ty = |ty: TypeId| {
            let ty = self.get_runtime_type(ty).unwrap();
            ty.name.as_ref()
        };

        let RuntimeFunction {
            name,
            description,
            kind,
            id: _,
            docstring,
            argument_names,
        } = f;
        let mut params = description
            .parameter_types()
            .iter()
            .map(|ty| print_ty(*ty))
            .collect::<Vec<_>>();
        let ret = params.remove(0);

        let mut argument_names = argument_names.iter();
        let mut params = params.iter();
        let receiver = match *kind {
            FunctionKind::Method(_) => {
                // Discard the name of the receiver from the arguments
                let _ = argument_names.next();
                format!("{}.", params.next().unwrap())
            }
            FunctionKind::StaticMethod(id) => {
                format!("{}.", print_ty(id))
            }
            FunctionKind::Free => "".into(),
        };

        let mut parameter_string = String::new();
        let mut first = true;
        for param in params {
            if !first {
                parameter_string.push_str(", ");
            }
            let name = argument_names.next().map_or("_", |v| v);
            parameter_string.push_str(name);
            parameter_string.push_str(": ");
            parameter_string.push_str(param);
            first = false;
        }

        let kind = match kind {
            FunctionKind::Free => "function",
            FunctionKind::Method(_) => "method",
            FunctionKind::StaticMethod(_) => "static_method",
        };
        println!(
            "````{{roto:{kind}}} {receiver}{name}({parameter_string}) -> {ret}"
        );
        for line in docstring.lines() {
            println!("{line}")
        }
        println!("````");
        println!();
    }

    pub fn print_documentation(&self) {
        println!("# Standard Library");
        println!();

        for f in &self.functions {
            if f.kind != FunctionKind::Free {
                continue;
            }
            self.print_function(f);
        }

        for RuntimeType {
            name,
            type_id,
            docstring,
            ..
        } in &self.runtime_types
        {
            println!("`````{{roto:type}} {name}");
            for line in docstring.lines() {
                println!("{line}")
            }
            println!();

            for f in &self.functions {
                let id = match f.kind {
                    FunctionKind::Free => continue,
                    FunctionKind::Method(id)
                    | FunctionKind::StaticMethod(id) => id,
                };
                if id != *type_id {
                    continue;
                }
                self.print_function(f);
            }

            println!("`````\n")
        }
    }
}

macro_rules! int_docs {
    ($t:ty) => {{
        #[allow(unused_comparisons)]
        let signed = if <$t>::MIN < 0 { "signed" } else { "unsigned" };
        let bits = <$t>::BITS;
        let min = <$t>::MIN;
        let max = <$t>::MAX;
        &format!("The {signed} {bits}-bit integer type\n\nThis type can represent integers from {min} up to (and including) {max}.")
    }};
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
            constants: Default::default(),
        };

        rt.register_copy_type_with_name::<()>(
            "Unit",
            "The unit type that has just one possible value. It can be used \
            when there is nothing meaningful to be returned.",
        )?;
        rt.register_copy_type::<bool>(
            "The boolean type\n\n\
            This type has two possible values: `true` and `false`. Several \
            boolean operations can be used with booleans, such as `&&` (\
            logical and), `||` (logical or) and `not`.",
        )?;
        rt.register_copy_type::<u8>(int_docs!(u8))?;
        rt.register_copy_type::<u16>(int_docs!(u16))?;
        rt.register_copy_type::<u32>(int_docs!(u32))?;
        rt.register_copy_type::<u64>(int_docs!(u64))?;
        rt.register_copy_type::<i8>(int_docs!(i8))?;
        rt.register_copy_type::<i16>(int_docs!(i16))?;
        rt.register_copy_type::<i32>(int_docs!(i32))?;
        rt.register_copy_type::<i64>(int_docs!(i64))?;
        rt.register_copy_type::<Asn>(
            "An ASN: an Autonomous System Number\n\
            \n\
            An AS number can contain a number of 32-bits and is therefore similar to a [`u32`](u32). \
            However, AS numbers cannot be manipulated with arithmetic operations. An AS number \
            is constructed with the `AS` prefix followed by a number.\n\
            \n\
            ```roto\n\
            AS0\n\
            AS1010\n\
            AS4294967295\n\
            ```\n\
            ")?;
        rt.register_copy_type::<IpAddr>(
            "An IP address\n\nCan be either IPv4 or IPv6.\n\
            \n\
            ```roto\n\
            # IPv4 examples\n\
            127.0.0.1\n\
            0.0.0.0\n\
            255.255.255.255\n\
            \n\
            # IPv6 examples\n\
            0:0:0:0:0:0:0:1\n\
            ::1\n\
            ::\n\
            ```\n\
            ",
        )?;
        rt.register_copy_type::<Prefix>(
            "An IP address prefix: the combination of an IP address and a prefix length\n\n\
            A prefix can be constructed with the `/` operator or with the \
            [`Prefix.new`](Prefix.new) function. This operator takes an [`IpAddr`](IpAddr) \
            and a [`u8`](u8) as operands.\n
            \n\
            ```roto\n\
            1.1.1.0 / 8\n\
            192.0.0.0.0 / 24\n\
            ```\n\
            ",
        )?;

        /// Construct a new prefix
        ///
        /// A prefix can also be constructed with the `/` operator.
        ///
        /// ```roto
        /// Prefix.new(192.169.0.0, 16)
        ///
        /// # or equivalently
        /// 192.169.0.0 / 16
        /// ```
        #[roto_static_method(rt, Prefix, new)]
        fn prefix_new(ip: *mut IpAddr, len: u8) -> Prefix {
            let ip = unsafe { *ip };

            Prefix::new(ip, len).unwrap()
        }

        /// Check whether two IP addresses are equal
        ///
        /// A more convenient but equivalent method for checking equality is via the `==` operator.
        ///
        /// An IPv4 address is never equal to an IPv6 address. IP addresses are considered equal if
        /// all their bits are equal.
        ///
        /// ```roto
        /// 192.0.0.0 == 192.0.0.0   # -> true
        /// ::0 == ::0               # -> true
        /// 192.0.0.0 == 192.0.0.1   # -> false
        /// 0.0.0.0 == 0::0          # -> false
        ///
        /// # or equivalently:
        /// 192.0.0.0.eq(192.0.0.0)  # -> true
        /// ```
        #[roto_method(rt, IpAddr, eq)]
        fn ipaddr_eq(a: *const IpAddr, b: *const IpAddr) -> bool {
            let a = unsafe { *a };
            let b = unsafe { *b };
            a == b
        }

        /// Returns true if this address is an IPv4 address, and false otherwise.
        ///
        /// ```roto
        /// 1.1.1.1.is_ipv4() # -> true
        /// ::.is_ipv4()      # -> false
        /// ```
        #[roto_method(rt, IpAddr)]
        fn is_ipv4(ip: *const IpAddr) -> bool {
            let ip = unsafe { &*ip };
            ip.is_ipv4()
        }

        /// Returns true if this address is an IPv6 address, and false otherwise.
        ///
        /// ```roto
        /// 1.1.1.1.is_ipv6() # -> false
        /// ::.is_ipv6()      # -> true
        /// ```
        #[roto_method(rt, IpAddr)]
        fn is_ipv6(ip: *const IpAddr) -> bool {
            let ip = unsafe { &*ip };
            ip.is_ipv6()
        }

        /// Converts this address to an IPv4 if it is an IPv4-mapped IPv6 address, otherwise it returns self as-is.
        #[roto_method(rt, IpAddr)]
        fn to_canonical(ip: *const IpAddr) -> IpAddr {
            let ip = unsafe { &*ip };
            ip.to_canonical()
        }

        rt.register_constant(
            "LOCALHOSTV4",
            IpAddr::from(Ipv4Addr::LOCALHOST),
        )
        .unwrap();

        rt.register_constant(
            "LOCALHOSTV6",
            IpAddr::from(Ipv6Addr::LOCALHOST),
        )
        .unwrap();

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
        communities::{Community, Wellknown},
        types::{LocalPref, OriginType},
    };

    pub fn routecore_runtime() -> Result<Runtime, String> {
        let mut rt = Runtime::basic()?;

        rt.register_clone_type::<OriginType>("TODO")?;
        rt.register_clone_type::<LocalPref>("TODO")?;
        rt.register_clone_type::<Community>("TODO")?;
        rt.register_clone_type::<HopPath>("TODO")?;
        rt.register_clone_type::<AsPath<Vec<u8>>>("TODO")?;

        #[roto_function(rt)]
        fn pow(x: u32, y: u32) -> u32 {
            x.pow(y)
        }

        #[roto_method(rt, u32)]
        fn is_even(x: u32) -> bool {
            x % 2 == 0
        }

        rt.register_constant(
            "BLACKHOLE",
            Community::from(Wellknown::Blackhole),
        )
        .unwrap();

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
